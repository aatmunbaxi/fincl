;;;; engines/monte-carlo.lisp --- parallel Monte Carlo and Longstaff-Schwartz
;;;;
;;;; The engine contains no model. It asks the process for a terminal sampler
;;;; (and uses it if one exists) or for a stepper, and otherwise only knows
;;;; about chunking, RNG streams, statistics and regression.

(in-package #:fincl)

(define-condition regression-failure (pricing-error)
  ((step :initarg :step :reader failure-step)
   (cause :initarg :cause :reader failure-cause))
  (:report (lambda (c s)
             (format s "LSM regression failed at step ~D: ~A"
                     (failure-step c) (failure-cause c)))))

;;; --------------------------------------------------------------------
;;; Parallel plumbing
;;; --------------------------------------------------------------------

(defmacro with-pricing-kernel ((&key workers) &body body)
  "Run BODY with an lparallel kernel, creating a temporary one if none is
active. A long-running session should make one kernel and keep it."
  (a:with-gensyms (w)
    `(if lp:*kernel*
         (progn ,@body)
         (let* ((,w (or ,workers (cpu-count)))
                (lp:*kernel* (lp:make-kernel ,w)))
           (unwind-protect (progn ,@body)
             (lp:end-kernel :wait t))))))

(defvar *cpu-count* nil
  "Memoized by CPU-COUNT on first use, so it describes the machine that runs
the code, not the one that built it.")

(defun cpu-count ()
  (or *cpu-count*
      (setf *cpu-count*
            #+sbcl (max 1 (or (ignore-errors
                               (parse-integer
                                (with-output-to-string (s)
                                  (sb-ext:run-program "nproc" '() :search t :output s))
                                :junk-allowed t))
                              1))
            #-sbcl 4)))

(defun chunk-ranges (n n-chunks)
  "Split [0, N) into contiguous (start . end) ranges."
  (let ((size (ceiling n (max 1 (min n n-chunks)))))
    (iter (for start from 0 below n by size)
          (collect (cons start (min n (+ start size)))))))

;;; --------------------------------------------------------------------
;;; Random numbers
;;; --------------------------------------------------------------------

(defun random-state-uniform-fn (generator)
  "A closure yielding the same uniforms as (RS:RANDOM-UNIT GENERATOR
'DOUBLE-FLOAT), with the generator's next-value function and word size
looked up once instead of dispatched on every draw. The bit assembly copies
RANDOM-STATE:RANDOM-BYTES exactly, so the stream is unchanged."
  (let ((next (rs::next-byte-fun generator))   ; internal: dispatches once
        (chunk (rs:bits-per-byte generator))
        (bits (float-digits 1d0)))             ; 53
    (declare (type function next))
    (etypecase chunk
      ;; Generators that output floats rather than bits.
      (symbol
       (lambda () (coerce (funcall next generator) 'double-float)))
      ((integer 53)
       (if (= chunk bits)
           (lambda () (scale-float (coerce (funcall next generator) 'double-float) -53))
           (lambda ()
             (scale-float (coerce (ldb (byte 53 0) (the integer (funcall next generator)))
                                  'double-float)
                          -53))))
      ((integer 1 52)
       (lambda ()
         (let ((random 0))
           ;; Upper bits first, then the lowermost word, spilling over
           ;; misaligned boundaries exactly as RANDOM-BYTES does.
           (iter (for i from (- bits chunk) above 0 by chunk)
                 (setf (ldb (byte chunk i) random) (funcall next generator)))
           (setf (ldb (byte chunk 0) random) (funcall next generator))
           (scale-float (coerce random 'double-float) -53)))))))

(defun make-uniform-fn (designator seed)
  "Return a closure of no arguments yielding uniforms in [0, 1).

  :mersenne-twister-64 etc. -- a random-state generator: reproducible across
                               implementations, roughly 5x the cost of the
                               host's own RNG per draw.
  (:pcg 17)                 -- generator type plus a seed offset.
  :native                   -- the implementation's RNG: fastest, but the
                               stream is not portable between Lisps."
  (ematch designator
    (:native
     (let ((state #+sbcl (sb-ext:seed-random-state seed)
                  #-sbcl (make-random-state t)))
       (lambda () (random 1d0 state))))
    ((guard type (keywordp type))
     (random-state-uniform-fn (rs:make-generator type seed)))
    ((list (guard type (keywordp type)) (guard offset (integerp offset)))
     (random-state-uniform-fn (rs:make-generator type (+ seed offset))))))

(defun make-stream-normal-filler (engine chunk-index)
  "A closure (lambda (vec)) filling VEC with normals from one generator
stream per work unit CHUNK-INDEX, seeded from the engine seed and the chunk
index. Draws continue across paths, so a path's normals depend on how paths
are split into chunks."
  (let ((u (make-uniform-fn (rng engine) (+ (* 1000003 (seed engine)) chunk-index))))
    (declare (type function u))
    (ecase (sampler engine)
      (:inverse-transform
       (lambda (vec)
         (declare (type (simple-array double-float (*)) vec))
         (dotimes (i (length vec) vec)
           (setf (aref vec i) (norm-quantile (max 1d-16 (the double-float (funcall u))))))))
      (:box-muller
       ;; Draws come in pairs; the second is kept for the next call, even
       ;; across paths.
       (let ((spare 0d0) (has-spare nil))
         (declare (double-float spare))
         (lambda (vec)
           (declare (type (simple-array double-float (*)) vec))
           (dotimes (i (length vec) vec)
             (setf (aref vec i)
                   (if has-spare
                       (progn (setf has-spare nil) spare)
                       (let* ((u1 (max 1d-16 (the double-float (funcall u))))
                              (u2 (the double-float (funcall u)))
                              (rad (sqrt (* -2d0 (log u1))))
                              (theta (* 2d0 pi u2)))
                         (declare (double-float u1 u2 rad theta))
                         (setf spare (* rad (sin theta))
                               has-spare t)
                         (* rad (cos theta))))))))))))

(defun make-counter-normal-filler (engine)
  "Normals for the :COUNTER rng: normal k of path i is a pure function of
(seed, i, k). Normals 2b and 2b + 1 of a path come from the Philox block b
(COUNTER-UNIFORMS), by Box-Muller or by inverse transform. Returns (values
fill start-path): (funcall start-path i) makes the following fills produce
path i's normals from the first."
  (let ((key (ldb (byte 64 0) (seed engine)))
        (path 0) (block 0) (has-spare nil)
        (spare (make-array 1 :element-type 'double-float)))
    (declare (type (unsigned-byte 64) key)
             (type (integer 0 #.most-positive-fixnum) path block)
             (type (simple-array double-float (1)) spare))
    (macrolet ((filler ((u1 u2) pair)
                 ;; PAIR computes (values first second) from one block.
                 `(lambda (vec)
                    (declare (type (simple-array double-float (*)) vec))
                    (dotimes (i (length vec) vec)
                      (setf (aref vec i)
                            (if has-spare
                                (progn (setf has-spare nil) (aref spare 0))
                                (multiple-value-bind (,u1 ,u2) (counter-uniforms key path block)
                                  (declare (double-float ,u1 ,u2))
                                  (incf block)
                                  (setf has-spare t)
                                  (multiple-value-bind (first second) ,pair
                                    (declare (double-float first second))
                                    (setf (aref spare 0) second)
                                    first))))))))
      (values (ecase (sampler engine)
                (:box-muller
                 (filler (u1 u2)
                         (let ((rad (sqrt (* -2d0 (log u1))))
                               (theta (* 2d0 pi u2)))
                           (declare (double-float rad theta))
                           (values (* rad (cos theta)) (* rad (sin theta))))))
                (:inverse-transform
                 (filler (u1 u2) (values (norm-quantile u1) (norm-quantile u2)))))
              (lambda (i)
                (setf path i block 0 has-spare nil))))))

;;; Randomized quasi-Monte Carlo. Base path i is point (mod i per) of
;;; randomization (floor i per), PER = ceiling(n / 16), each randomization
;;; an independently scrambled and shifted Sobol' sequence. A path's
;;; dimensions are its Brownian-bridge coordinates, terminal value first,
;;; factor by factor: dimension k F + f is bridge coordinate k of factor f,
;;; so the best-distributed dimensions decide each factor's terminal value.
;;; The bridge maps the point's normals to the same number of independent
;;; standard normals (each step's increment over its standard deviation),
;;; which the stepper consumes as usual.

(defconstant +rqmc-randomizations+ 16
  "Independent randomizations of the Sobol' points; the standard error is
that of their means.")

(defun points-per-randomization (n)
  (max 1 (ceiling n +rqmc-randomizations+)))

(defstruct (sobol-plan (:constructor %make-sobol-plan))
  "Everything a :SOBOL filler shares across chunks: per-randomization
direction tables and shifts, and the Brownian-bridge construction over the
grid. Read-only."
  (directions nil :type simple-vector :read-only t)
  (shifts nil :type simple-vector :read-only t)
  (per 1 :type fixnum :read-only t)
  (n-factors 1 :type fixnum :read-only t)
  ;; Bridge: W[n] = sqrt(t_n) z_0, then for k = 1 .. n - 1, W[mid_k] =
  ;; a_k W[left_k] + b_k W[right_k] + c_k z_k.
  (left nil :type (simple-array fixnum (*)) :read-only t)
  (mid nil :type (simple-array fixnum (*)) :read-only t)
  (right nil :type (simple-array fixnum (*)) :read-only t)
  (a nil :type double-vector :read-only t)
  (b nil :type double-vector :read-only t)
  (c nil :type double-vector :read-only t)
  (terminal-sd 0d0 :type double-float :read-only t)
  (step-sd nil :type double-vector :read-only t))

(defun make-sampling-plan (engine times n-factors n-base)
  "The SOBOL-PLAN for paths on TIMES with N-FACTORS normals per step and
N-BASE base paths, or NIL unless ENGINE's rng is :SOBOL."
  (when (eq (rng engine) :sobol)
    (let* ((n (1- (length times)))
           (key (ldb (byte 64 0) (seed engine)))
           (tables (iter (for r below +rqmc-randomizations+)
                         (collect (multiple-value-list
                                   (sobol-tables (* n n-factors) :key key :randomization r)))))
           (left '()) (mid '()) (right '()) (a '()) (b '()) (c '()))
      ;; Breadth-first bisection of [0, n], so coarse points come first.
      (iter (with queue = `((0 . ,n)))
            (while queue)
            (for (l . r) = (pop queue))
            (when (> (- r l) 1)
              (let* ((m (floor (+ l r) 2))
                     (tl (aref times l)) (tm (aref times m)) (tr (aref times r)))
                (push l left) (push m mid) (push r right)
                (push (/ (- tr tm) (- tr tl)) a)
                (push (/ (- tm tl) (- tr tl)) b)
                (push (sqrt (/ (* (- tm tl) (- tr tm)) (- tr tl))) c)
                (setf queue (append queue `((,l . ,m) (,m . ,r)))))))
      (flet ((fixnums (list) (coerce (reverse list) '(simple-array fixnum (*))))
             (doubles (list) (coerce (reverse list) 'double-vector)))
        (%make-sobol-plan :directions (map 'simple-vector #'first tables)
                          :shifts (map 'simple-vector #'second tables)
                          :per (points-per-randomization n-base)
                          :n-factors n-factors
                          :left (fixnums left) :mid (fixnums mid) :right (fixnums right)
                          :a (doubles a) :b (doubles b) :c (doubles c)
                          :terminal-sd (sqrt (aref times n))
                          :step-sd (map 'double-vector #'sqrt (interval-lengths times)))))))

(defun make-sobol-normal-filler (plan)
  "(values fill start-path) for the :SOBOL rng from PLAN. START-PATH computes
the path's point (from the previous one when it is the next point of the same
randomization) and its normals for every step; each FILL copies the next
step's N-FACTORS normals."
  (let* ((n-factors (sobol-plan-n-factors plan))
         (step-sd (sobol-plan-step-sd plan))
         (n (length step-sd))
         (per (sobol-plan-per plan))
         (point (make-array (* n n-factors) :element-type 'word64))
         (w (make-array (1+ n) :element-type 'double-float :initial-element 0d0))
         (normals (make-array (* n n-factors) :element-type 'double-float))
         (randomization -1) (index -1) (cursor 0))
    (declare (fixnum n n-factors per randomization index cursor)
             (type double-vector step-sd w normals))
    (values (lambda (vec)
              (declare (type (simple-array double-float (*)) vec))
              (replace vec normals :start2 cursor)
              (incf cursor n-factors)
              vec)
            (lambda (i)
              (declare (fixnum i))
              (multiple-value-bind (r p) (floor i per)
                (let ((directions (svref (sobol-plan-directions plan) r)))
                  (if (and (= r randomization) (= p (1+ index)))
                      (sobol-next! point directions index)
                      (sobol-point! point directions (svref (sobol-plan-shifts plan) r) p))
                  (setf randomization r index p cursor 0)))
              (let ((left (sobol-plan-left plan)) (mid (sobol-plan-mid plan))
                    (right (sobol-plan-right plan))
                    (a (sobol-plan-a plan)) (b (sobol-plan-b plan)) (c (sobol-plan-c plan)))
                (dotimes (f n-factors)
                  (flet ((z (k) (norm-quantile (word64-to-open-unit (aref point (+ (* k n-factors) f))))))
                    (setf (aref w n) (* (sobol-plan-terminal-sd plan) (z 0)))
                    (dotimes (k (length mid))
                      (setf (aref w (aref mid k))
                            (+ (* (aref a k) (aref w (aref left k)))
                               (* (aref b k) (aref w (aref right k)))
                               (* (aref c k) (z (1+ k))))))
                    (dotimes (j n)
                      (setf (aref normals (+ (* j n-factors) f))
                            (/ (- (aref w (1+ j)) (aref w j)) (aref step-sd j)))))))))))

(defun make-normal-filler (engine chunk-index &optional plan)
  "Normals for work unit CHUNK-INDEX, as (values fill start-path). FILL is a
closure (lambda (vec)) that fills VEC, a (SIMPLE-ARRAY DOUBLE-FLOAT (*)),
with standard normals; call (funcall start-path i) before the normals of path
I. The generator and ENGINE's SAMPLER are resolved here, once: FILL makes no
generic call and no sampler test per draw, and fills a vector rather than
returning one normal, so no double-float is boxed on the way out.

Under the :COUNTER rng a path's normals depend only on the seed and the path
index (START-PATH selects them), so results do not depend on the chunking;
so under :SOBOL, whose PLAN (MAKE-SAMPLING-PLAN) fixes the grid. Under a
stream rng the chunk has one stream and START-PATH does nothing."
  (case (rng engine)
    (:counter (make-counter-normal-filler engine))
    (:sobol (make-sobol-normal-filler plan))
    (t (values (make-stream-normal-filler engine chunk-index)
               (lambda (i) (declare (ignore i)))))))

(declaim (inline fill-normals negate-into))

(defun fill-normals (filler vec)
  (declare (type function filler) (type (simple-array double-float (*)) vec))
  (funcall filler vec))

(defun negate-into (source target)
  "Antithetic partner of SOURCE's draws."
  (declare (type (simple-array double-float (*)) source target))
  (dotimes (i (length source) target)
    (setf (aref target i) (- (aref source i)))))

;;; --------------------------------------------------------------------
;;; Statistics
;;; --------------------------------------------------------------------

(defun moments (samples)
  "Mean and sum of squared deviations of SAMPLES, as the degree-2
NU.STATISTICS accumulator that STS:POOL and SUMMARIZE take. Computes exactly
what (STS:CENTRAL-SAMPLE-MOMENTS SAMPLES :DEGREE 2) computes (the same
one-pass update, in the same order, so the same bits), but on typed
doubles: the library maps a generic ADD over every sample."
  (declare (type (simple-array double-float (*)) samples))
  (let ((m 0d0) (s2 0d0) (w 0))
    (declare (double-float m s2) (fixnum w))
    (iter (declare (declare-variables))
          (for (the double-float y) in-vector samples)
          (let* ((d (- y m))
                 (next (1+ w))
                 (d/w (/ d (float next 1d0))))
            (declare (double-float d d/w) (fixnum next))
            (incf m d/w)
            (incf s2 (* (* d/w (float w 1d0)) d))
            (setf w next)))
    ;; The constructor is internal to NU.STATISTICS; this is the only use.
    (sts::make-central-sample-moments :w w :m m :s2 s2 :s3 nil :s4 nil)))

(defun pair-means (samples)
  "Means of the antithetic pairs in SAMPLES, whose second half holds the
partners of the first: element i is (SAMPLES[i] + SAMPLES[i + n/2]) / 2."
  (declare (type (simple-array double-float (*)) samples))
  (let* ((half (floor (length samples) 2))
         (pairs (make-array half :element-type 'double-float)))
    (dotimes (i half pairs)
      (setf (aref pairs i) (* 0.5d0 (+ (aref samples i) (aref samples (+ i half))))))))

(defun summarize (accumulator &key (discount 1d0))
  "Mean and standard error from a (possibly pooled) accumulator."
  (let ((n (sts:tally accumulator)))
    (values (* discount (sts:mean accumulator))
            (* discount (sqrt (/ (sts:variance accumulator) n))))))

(defun summarize-samples (engine samples &key (discount 1d0))
  "Mean and standard error of the per-path SAMPLES (in base-path order).
Under :SOBOL the samples of each randomization are averaged first, and the
standard error is that of the randomization means."
  (declare (type double-vector samples))
  (if (eq (rng engine) :sobol)
      (let* ((n (length samples))
             (per (points-per-randomization n)))
        (summarize (moments (iter (for start from 0 below n by per)
                                  (for end = (min n (+ start per)))
                                  (collect-double (/ (iter (for i from start below end)
                                                           (sum (aref samples i)))
                                                     (- end start))
                                                  into means length (ceiling n per))
                                  (finally (return means))))
                   :discount discount))
      (summarize (moments samples) :discount discount)))

;;; --------------------------------------------------------------------
;;; Path generation
;;; --------------------------------------------------------------------

(defstruct (path-matrix (:constructor %make-path-matrix (n-paths n-times data)))
  "The observable along every simulated path: N-PATHS x N-TIMES, stored
column-major in DATA, so element (path i, time j) is at i + j N-PATHS and
each time slice is contiguous, which is what the LSM backward sweep reads."
  (n-paths 0 :type fixnum :read-only t)
  (n-times 0 :type fixnum :read-only t)
  (data nil :type (simple-array double-float (*)) :read-only t))

(defun make-path-matrix (n-paths n-times)
  (%make-path-matrix n-paths n-times
                     (make-array (* n-paths n-times) :element-type 'double-float)))

(defun total-paths (engine)
  "Paths the engine simulates: N-PATHS, rounded up to even under antithetics."
  (if (antithetic engine) (* 2 (ceiling (n-paths engine) 2)) (n-paths engine)))

(defun map-paths (engine market tau make-visitor &key times)
  "Step every path of ENGINE's process, resolved at TAU, over TIMES (default
the uniform grid on [0, TAU] of N-STEPS steps) and show each state to a
visitor, without storing paths. Work is split into N-CHUNKS chunks run in parallel. Under the
:COUNTER rng path i's normals depend only on the seed and i; under a stream
rng each chunk has its own stream.

For each chunk, (funcall MAKE-VISITOR chunk-index start end n-base) returns
(values visit finish). The chunk steps the base paths [START, END) and, under
antithetics, their partners [START + N-BASE, END + N-BASE). It calls
(visit path-index step state) at every grid index of every path; STATE is
reused, so copy anything kept. It calls (finish) when the chunk is done.
Returns the FINISH values in chunk order, whatever the worker count.

  (map-paths e m 1d0 (lambda (chunk start end n-base)
                       (values (lambda (i j state) ...) (lambda () result))))"
  (let* ((times (or times (uniform-time-grid tau (n-steps engine))))
         (n-steps (1- (length times)))
         (p (-> engine process (resolve-process market tau)))
         (anti (antithetic engine))
         (n-base (if anti (/ (total-paths engine) 2) (total-paths engine)))
         (stepper (make-stepper p market times)) ; dispatch once
         (n-factors (process-factors p))
         (plan (make-sampling-plan engine times n-factors n-base))
         (template (initial-state p market))
         (ranges (chunk-ranges n-base (n-chunks engine))))
    (declare (type function stepper make-visitor)
             (fixnum n-base n-steps n-factors))
    (flet ((run-chunk (range index)
             (multiple-value-bind (visit finish)
                 (funcall make-visitor index (car range) (cdr range) n-base)
               (declare (type function visit finish))
               (multiple-value-bind (g start-path) (make-normal-filler engine index plan)
                 (declare (type function start-path))
                 (let ((state (copy-seq template))
                       (mirror (copy-seq template))
                       (normals (make-array n-factors :element-type 'double-float))
                       (anti-normals (make-array n-factors :element-type 'double-float)))
                   (declare (type (simple-array double-float (*))
                                  state mirror normals anti-normals))
                   (iter (declare (declare-variables))
                         (for (the fixnum i) from (car range) below (cdr range))
                         (for (the fixnum partner) = (+ i n-base))
                         (funcall start-path i)
                         (replace state template)
                         (replace mirror template)
                         (funcall visit i 0 state)
                         (when anti (funcall visit partner 0 mirror))
                         (iter (declare (declare-variables))
                               (for (the fixnum j) from 1 to n-steps)
                               (fill-normals g normals)
                               (funcall stepper state normals (1- j))
                               (funcall visit i j state)
                               (when anti
                                 (negate-into normals anti-normals)
                                 (funcall stepper mirror anti-normals (1- j))
                                 (funcall visit partner j mirror))))
                   (funcall finish))))))
      (with-pricing-kernel ()
        (lp:pmap 'list #'run-chunk ranges (a:iota (length ranges)))))))

(defun simulate-state-paths (engine market tau components &key times)
  "One PATH-MATRIX per state component in COMPONENTS (indices into the state
vector), in the same order: (total paths) x (grid points), on TIMES (default
the uniform grid of N-STEPS steps), antithetic partners in the second half.
Works for any process: the engine only calls MAKE-STEPPER."
  (let* ((n-paths (total-paths engine))
         (n-times (if times (length times) (1+ (n-steps engine))))
         (matrices (iter (repeat (length components))
                         (collect (make-path-matrix n-paths n-times))))
         (datas (map 'simple-vector #'path-matrix-data matrices))
         (indices (coerce components '(simple-array fixnum (*)))))
    (declare (fixnum n-paths) (type (simple-array fixnum (*)) indices))
    (map-paths engine market tau
               (lambda (chunk start end n-base)
                 (declare (ignore chunk start end n-base))
                 (values (lambda (i j state)
                           (declare (fixnum i j)
                                    (type (simple-array double-float (*)) state))
                           (let ((cell (+ i (the fixnum (* j n-paths)))))
                             (dotimes (k (length indices))
                               (setf (aref (the (simple-array double-float (*)) (svref datas k))
                                           cell)
                                     (aref state (aref indices k))))))
                         (constantly nil)))
               :times times)
    matrices))

(defun simulate-paths (engine market tau)
  "The PATH-MATRIX of the process observable, (total paths) x (N-STEPS + 1),
antithetic partners in the second half."
  (first (simulate-state-paths engine market tau '(0))))

;;; --------------------------------------------------------------------
;;; European: use the terminal shortcut when the process offers one
;;; --------------------------------------------------------------------

(defun price-european-by-sampling (sampler engine market tau kernel)
  "Terminal-only Monte Carlo: no path is ever materialized. Sample i (the
mean of a pair under antithetics) is written to slot i of one vector, and
the statistics are taken over it in index order, so they do not depend on
the chunking."
  (let* ((anti (antithetic engine))
         (n (if anti (ceiling (n-paths engine) 2) (n-paths engine)))
         (n-factors (process-factors (process engine)))
         (samples (make-array n :element-type 'double-float))
         (plan (make-sampling-plan engine (coerce `(0d0 ,(float tau 1d0)) 'double-vector) n-factors n))
         (ranges (chunk-ranges n (n-chunks engine))))
    (declare (type function sampler kernel) (fixnum n n-factors))
    (flet ((run-chunk (range index)
             (multiple-value-bind (g start-path) (make-normal-filler engine index plan)
               (declare (type function start-path))
               (let ((normals (make-array n-factors :element-type 'double-float))
                     (anti-normals (make-array n-factors :element-type 'double-float)))
                 (iter (declare (declare-variables))
                       (for (the fixnum i) from (car range) below (cdr range))
                       (funcall start-path i)
                       (fill-normals g normals)
                       (let ((v (funcall kernel (funcall sampler normals))))
                         (declare (double-float v))
                         (when anti
                           (negate-into normals anti-normals)
                           (setf v (* 0.5d0 (+ v (funcall kernel
                                                          (funcall sampler anti-normals))))))
                         (setf (aref samples i) v)))))))
      (with-pricing-kernel ()
        (lp:pmapc #'run-chunk ranges (a:iota (length ranges))))
      (summarize-samples engine samples :discount (discount-factor market tau)))))

;;; Any payoff and path feature: a payoff that needs only the observable at
;;; expiry uses the process's terminal sampler when it has one; everything
;;; else steps full paths through the payoff's PATH-KERNEL.
(defmethod price-mc ((x european-exercise) (p payoff) (f path-feature)
                     (e monte-carlo-engine) market)
  (let* ((tau (time-to-expiry x market))
         (sampler (and (equal (payoff-requirements p f) '(:terminal))
                       (null (control-variates e))
                       (-> e process (resolve-process market tau) (terminal-sampler market tau)))))
    (if sampler
        (price-european-by-sampling sampler e market tau (payoff-kernel p))
        (price-by-path-kernel x p f e market))))

;;; A continuous barrier needs the process's bridge, unless it is turned off.
(defmethod price-mc :before ((x european-exercise) (p payoff) (f barrier)
                             (e monte-carlo-engine) market)
  (let ((tau (time-to-expiry x market)))
    (when (and (barrier-bridge e) (null (observation-dates f))
               (null (bridge-variance (-> e process (resolve-process market tau)) market
                                      (coerce `(0d0 ,(float tau 1d0)) 'double-vector))))
      (error 'model-domain-error
             :engine e
             :reason (format nil "a continuous barrier is crossed between grid points by the ~
Brownian bridge, which ~(~A~) does not have. Give the barrier :MONITORING dates, or ~
make the engine with :BARRIER-BRIDGE NIL to check it at grid points only."
                             (class-name (class-of (process e))))))))

(declaim (ftype function control-variate-price)) ; selection.lisp, after WITH-ENGINES

(defun resolve-control-variates (x p f e market)
  "ENGINE's control variates for the option (X P F), as (payoff path price)
triples: PRICE is the variate's value on MARKET. Each item of the engine's
CONTROL-VARIATES is :EUROPEAN (P at expiry, without F), :GEOMETRIC-ASIAN (P
on the geometric average of F's dates; F must be an Asian), or (payoff path
pricer), PRICER an engine designator or a function of (option market).
Built-in variates are priced with the engine's process by automatic
selection, Monte Carlo excluded, so they are exact for that model."
  (iter (for item in (control-variates e))
        (for (payoff path pricer)
             = (match item
                 (:european
                  (when (typep p 'path-payoff)
                    (signal-invalid-argument 'control-variates item 'list
                                             "a variate with a terminal payoff, not a path payoff"))
                  `(,p ,(make-instance 'path-independent) nil))
                 (:geometric-asian
                  (unless (typep f 'asian)
                    (signal-invalid-argument 'control-variates item 'list
                                             "an Asian option for :GEOMETRIC-ASIAN"))
                  `(,p ,(make-instance 'asian :average :geometric :dates (observation-dates f)
                                              :strike-type (strike-type f))
                       nil))
                 ((list payoff path pricer) `(,payoff ,path ,pricer))
                 (_ (signal-invalid-argument
                     'control-variates item '(or keyword list)
                     ":EUROPEAN, :GEOMETRIC-ASIAN or (payoff path-feature pricer)"))))
        (when (member :cashflows (payoff-requirements payoff path))
          (signal-invalid-argument 'control-variates item 'list
                                   "a variate paying only at expiry"))
        (collect `(,payoff ,path ,(control-variate-price x payoff path pricer e market)))))

(defun control-variate-samples (samples means)
  "SAMPLES[0] adjusted by the variates SAMPLES[1..] with known MEANS: y -
sum_c beta_c (x_c - mean_c), beta from the least-squares fit of y on 1 and
the centred variates, the rows taken in path order."
  (let* ((y (svref samples 0))
         (n (length y))
         (k (length means))
         (acc (make-qr-accumulator (1+ k)))
         (row (make-array (1+ k) :element-type 'double-float)))
    (declare (type double-vector y))
    (dotimes (i n)
      (setf (aref row 0) 1d0)
      (iter (for mean in means) (for c from 1)
            (setf (aref row c) (- (aref (the double-vector (svref samples c)) i) mean)))
      (qr-add-row! acc row (aref y i)))
    (let ((beta (qr-solve acc))
          (adjusted (make-array n :element-type 'double-float)))
      (declare (type double-vector beta))
      (dotimes (i n adjusted)
        (setf (aref adjusted i)
              (- (aref y i)
                 (iter (for mean in means) (for c from 1)
                       (sum (* (aref beta c)
                               (- (aref (the double-vector (svref samples c)) i) mean))))))))))

(defun price-by-path-kernel (x p f e market)
  "Step every path over the time grid of X, P and F's dates (MAKE-TIME-GRID,
at most the span over N-STEPS apart) and value it through P's PATH-KERNEL
under F, and through the kernels of E's control variates on the same paths.
A path stops when every kernel's STEP has returned NIL; its antithetic
partner goes on alone. Cash flows recorded with PAY are discounted to their
payment dates and the final amount to expiry. Sample i (the mean of a pair
under antithetics) is written to slot i and the statistics are taken in index
order, so the result does not depend on the chunking."
  (let* ((tau (time-to-expiry x market))
         (process (-> e process (resolve-process market tau)))
         (variates (resolve-control-variates x p f e market))
         (specs `((,p ,f) ,@(mapcar (lambda (v) (subseq v 0 2)) variates)))
         (grid (make-time-grid market
                               :dates (iter (for (payoff path) in specs)
                                            (appending (instrument-grid-dates x payoff path)))
                               :n-steps (n-steps e)))
         (times (time-grid-times grid))
         (n-steps (1- (length times)))
         (stepper (make-stepper process market times))
         (template (initial-state process market))
         (n-factors (process-factors process))
         (n-kernels (length specs))
         (sizes (map 'simple-vector (lambda (s) (path-accumulator-size (first s) (second s))) specs))
         (expiry-discount (discount-factor market tau))
         (discount-kernel (path-discount-kernel process market grid))
         ;; Cash flows are carried relative to expiry, so the final amount
         ;; alone is summed as before and discounted once.
         (payments (path-payment-dates p f))
         (pay-discounts (map 'double-vector (lambda (d) (/ (discount-factor market d) expiry-discount))
                             (if (eq payments :grid) times payments)))
         (anti (antithetic e))
         (n-base (if anti (/ (total-paths e) 2) (total-paths e)))
         (samples (map-into (make-array n-kernels)
                            (lambda () (make-array n-base :element-type 'double-float))))
         (plan (make-sampling-plan e times n-factors n-base))
         (ranges (chunk-ranges n-base (n-chunks e)))
         ;; Each kernel's (init step final), the target's first.
         (kernels (let ((*barrier-bridge* (barrier-bridge e)))
                    (map 'simple-vector
                         (lambda (s)
                           (coerce (multiple-value-list (path-kernel (first s) (second s) process grid))
                                   'simple-vector))
                         specs)))
         (no-pay (lambda (amount k) (declare (ignore amount k)) nil)))
    (declare (type function stepper) (fixnum n-steps n-factors n-base n-kernels)
             (type double-vector pay-discounts))
    (flet ((run-chunk (range index)
             (multiple-value-bind (g start-path) (make-normal-filler e index plan)
               (declare (type function start-path))
               (let* ((state (copy-seq template))
                      (mirror (copy-seq template))
                      (normals (make-array n-factors :element-type 'double-float))
                      (anti-normals (make-array n-factors :element-type 'double-float))
                      ;; Per side (0 the base path, 1 its partner) and kernel.
                      (accs (flet ((side () (map 'simple-vector
                                                 (lambda (size)
                                                   (make-array size :element-type 'double-float))
                                                 sizes)))
                              (vector (side) (side))))
                      (alive (make-array `(2 ,n-kernels) :element-type 'bit :initial-element 0))
                      (sums (make-array 2 :element-type 'double-float))
                      (j 0))
                 (declare (type (simple-array double-float (*)) state mirror normals anti-normals sums)
                          (type (simple-array bit (2 *)) alive)
                          (fixnum j))
                 (flet ((payer (slot current)
                          ;; PAY for the path whose running sum is SUMS[SLOT].
                          (if discount-kernel
                              (lambda (amount k)
                                (declare (ignore k) (double-float amount))
                                (incf (aref sums slot)
                                      (* amount (the double-float
                                                     (funcall (the function discount-kernel) current j)))))
                              (lambda (amount k)
                                (declare (double-float amount) (fixnum k))
                                (incf (aref sums slot) (* amount (aref pay-discounts k)))))))
                   (let ((pays (vector (payer 0 state) (payer 1 mirror))))
                     (labels ((step-side (side j current)
                                ;; Step every live kernel of SIDE at J; true if any lives.
                                (let ((any nil))
                                  (dotimes (k n-kernels any)
                                    (when (= 1 (aref alive side k))
                                      (if (funcall (the function (svref (svref kernels k) 1))
                                                   (svref (svref accs side) k) j current
                                                   (if (zerop k) (svref pays side) no-pay))
                                          (setf any t)
                                          (setf (aref alive side k) 0))))))
                              (start-side (side current)
                                (dotimes (k n-kernels)
                                  (funcall (the function (svref (svref kernels k) 0))
                                           (svref (svref accs side) k))
                                  (setf (aref alive side k) 1))
                                (step-side side 0 current))
                              (path-value (side k current)
                                (+ (if (zerop k) (aref sums side) 0d0)
                                   (if (= 1 (aref alive side k))
                                       (* (the double-float
                                               (funcall (the function (svref (svref kernels k) 2))
                                                        (svref (svref accs side) k) current))
                                          (if discount-kernel
                                              (the double-float
                                                   (funcall (the function discount-kernel) current n-steps))
                                              1d0))
                                       0d0))))
                       (iter (declare (declare-variables))
                             (for (the fixnum i) from (car range) below (cdr range))
                             (funcall start-path i)
                             (replace state template)
                             (replace mirror template)
                             (setf (aref sums 0) 0d0 (aref sums 1) 0d0 j 0)
                             (let ((base-alive (start-side 0 state))
                                   (partner-alive (and anti (start-side 1 mirror))))
                               (iter (while (or base-alive partner-alive))
                                     (setf j (1+ j))
                                     (while (<= j n-steps))
                                     (fill-normals g normals)
                                     (when base-alive
                                       (funcall stepper state normals (1- j))
                                       (setf base-alive (step-side 0 j state)))
                                     (when partner-alive
                                       (negate-into normals anti-normals)
                                       (funcall stepper mirror anti-normals (1- j))
                                       (setf partner-alive (step-side 1 j mirror))))
                               (dotimes (k n-kernels)
                                 (let ((v (path-value 0 k state)))
                                   (declare (double-float v))
                                   (setf (aref (the double-vector (svref samples k)) i)
                                         (if anti
                                             (* 0.5d0 (+ v (the double-float (path-value 1 k mirror))))
                                             v)))))))))))))
      (with-pricing-kernel ()
        (lp:pmapc #'run-chunk ranges (a:iota (length ranges))))
      (summarize-samples e (if variates
                               (control-variate-samples
                                samples (mapcar (lambda (v)
                                                  (if discount-kernel
                                                      (third v)
                                                      (/ (third v) expiry-discount)))
                                                variates))
                               (svref samples 0))
                         :discount (if discount-kernel 1d0 expiry-discount)))))

;;; --------------------------------------------------------------------
;;; American: Longstaff-Schwartz
;;; --------------------------------------------------------------------

;;; The LSM basis. Regressor values are scaled, raised to powers by
;;; repeated multiplication (so a single regressor gives exactly 1, x, x*x,
;;; ...), and combined into every monomial of total degree <= the basis
;;; degree, lowest degree first. :LAGUERRE replaces the powers of the first
;;; regressor (the spot) by the weighted Laguerre functions exp(-x/2) L_k(x).

(defun basis-exponents (n-regressors degree)
  "Exponent lists of all monomials in N-REGRESSORS variables with total degree
<= DEGREE, graded by total degree; within a degree, earlier variables'
exponents come first, highest first.

  (basis-exponents 2 2) => ((0 0) (1 0) (0 1) (2 0) (1 1) (0 2))"
  (labels ((summing-to (n total)
             (if (= n 1)
                 (list (list total))
                 (iter (for a from total downto 0)
                       (appending (mapcar (lambda (rest) (cons a rest))
                                          (summing-to (1- n) (- total a))))))))
    (iter (for total from 0 to degree)
          (appending (summing-to n-regressors total)))))

(defun make-basis-filler (kind exponents scales degree)
  "A closure (lambda (basis values)) that writes the basis functions of the
regressor VALUES into BASIS. KIND is :MONOMIAL or :LAGUERRE, EXPONENTS as
from BASIS-EXPONENTS, SCALES one divisor per regressor. The closure owns its
scratch tables: make one per thread."
  (let* ((m (length scales))
         (width (1+ degree))
         (tables (make-array (* m width) :element-type 'double-float))
         (n-basis (length exponents))
         (offsets (make-array (* n-basis m) :element-type 'fixnum)))
    (declare (type (simple-array double-float (*)) scales tables)
             (type (simple-array fixnum (*)) offsets)
             (fixnum m width n-basis))
    ;; Index into TABLES of each regressor's factor in each basis function.
    (iter (for exps in exponents) (for i from 0)
          (iter (for e in exps) (for r from 0)
                (setf (aref offsets (+ (* i m) r)) (+ (* r width) e))))
    (lambda (basis values)
      (declare (type (simple-array double-float (*)) basis values)
               (optimize (speed 3) (safety 0)))
      (dotimes (r m)
        (let ((x (/ (aref values r) (aref scales r)))
              (row (* r width)))
          (declare (double-float x) (fixnum row))
          (if (and (zerop r) (eq kind :laguerre))
              (let ((w (exp (* -0.5d0 x))) (l0 1d0) (l1 (- 1d0 x)))
                (declare (double-float w l0 l1))
                (setf (aref tables row) w)
                (when (> width 1) (setf (aref tables (1+ row)) (* w l1)))
                (iter (declare (declare-variables))
                      (for (the fixnum k) from 1 below (1- width))
                      (let ((l2 (/ (- (* (- (+ (* 2 k) 1) x) l1) (* k l0)) (1+ k))))
                        (declare (double-float l2))
                        (setf (aref tables (+ row k 1)) (* w l2)
                              l0 l1 l1 l2))))
              (let ((b 1d0))
                (declare (double-float b))
                (dotimes (k width)
                  (setf (aref tables (+ row k)) b
                        b (* b x)))))))
      (dotimes (i n-basis basis)
        (let ((product (aref tables (aref offsets (* i m)))))
          (declare (double-float product))
          (iter (declare (declare-variables))
                (for (the fixnum r) from 1 below m)
                (setf product (* product (aref tables (aref offsets (+ (* i m) r))))))
          (setf (aref basis i) product))))))

(defun regress-continuation (datas cashflows col-offset kernel make-filler n-basis df ranges)
  "Discount CASHFLOWS one step (in place) and regress them on the basis over
the in-the-money paths at COL-OFFSET. DATAS holds one path-matrix data vector
per regressor, the observable first. Each block folds its rows into its own
QR accumulator in parallel; the accumulators are merged in block order
(RANGES are REGRESSION-BLOCKS), so the fit does not depend on the worker
count or the chunking. Returns (values accumulator
in-the-money-count)."
  (declare (type simple-vector datas)
           (type (simple-array double-float (*)) cashflows)
           (fixnum col-offset n-basis)
           (double-float df)
           (type function make-filler)
           (type (function (double-float) double-float) kernel))
  (let ((observable (svref datas 0))
        (m (length datas)))
    (declare (type (simple-array double-float (*)) observable))
    (flet ((run-chunk (range)
             (let ((acc (make-qr-accumulator n-basis))
                   (filler (funcall make-filler))
                   (basis (make-array n-basis :element-type 'double-float))
                   (values (make-array m :element-type 'double-float)))
               (declare (type function filler))
               (iter (declare (declare-variables))
                     (for (the fixnum i) from (car range) below (cdr range))
                     (for (the double-float s) = (aref observable (+ col-offset i)))
                     (setf (aref cashflows i) (* df (aref cashflows i)))
                     (when (plusp (funcall kernel s))
                       (dotimes (r m)
                         (setf (aref values r)
                               (aref (the (simple-array double-float (*)) (svref datas r))
                                     (+ col-offset i))))
                       (funcall filler basis values)
                       (qr-add-row! acc basis (aref cashflows i))))
               acc)))
      (let ((total (make-qr-accumulator n-basis)))
        (dolist (acc (lp:pmap 'list #'run-chunk ranges))
          (qr-merge! total acc))
        (values total (qr-accumulator-rows total))))))

(defconstant +regression-block+ 4096
  "Paths per LSM regression block. Blocks are fixed by the path count alone,
and their QR accumulators are merged in block order, so the regression does
not depend on N-CHUNKS or on the worker count.")

(defun regression-blocks (n-paths)
  "Contiguous (start . end) blocks of +REGRESSION-BLOCK+ paths covering
[0, N-PATHS)."
  (iter (for start from 0 below n-paths by +regression-block+)
        (collect (cons start (min n-paths (+ start +regression-block+))))))

(defgeneric regression-scale (payoff market)
  (:documentation "The level LSM divides the observable by before building
its polynomial basis, so the basis is well conditioned: the strike for a
struck payoff, the spot otherwise.")
  (:method ((p payoff) market) (float (spot market) 1d0))
  (:method ((p strike-payoff) market)
    (declare (ignore market))
    (strike p)))

;;; Any payoff: LSM needs only the kernel (exercise value, and which paths
;;; are in the money) and a scale for the regression basis. Exercise rights
;;; come from EXERCISE-MASK on the time grid, so the same sweep prices
;;; American and Bermudan exercise (Bermudan dates snap to the nearest step).
(defmethod price-mc ((x american-exercise) (p payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (price-lsm x p e market))

(defmethod price-mc ((x bermudan-exercise) (p payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (price-lsm x p e market))

(defun grid-step-discounts (market times)
  "Discount factors between consecutive grid TIMES: DF(t_{j+1}) / DF(t_j)."
  (iter (with n = (1- (length times)))
        (for (t0 t1) in-intervals times)
        (collect-double (/ (discount-factor market t1) (discount-factor market t0))
                        into v length n)
        (finally (return v))))

(defun price-lsm (x p e market)
  "Longstaff-Schwartz: (values price standard-error) of payoff P with the
exercise rights of X, on the grid of X's dates at most the span over N-STEPS
apart (MAKE-TIME-GRID): every Bermudan date is a grid point. With
LSM-VALUATION :INDEPENDENT the exercise rule fitted on the engine's paths
values a second, independent path set (another seed), a low-biased estimate."
  (let* ((tau (time-to-expiry x market))
         (times (time-grid-times (make-time-grid market :dates (exercise-mandatory-dates x)
                                                        :n-steps (n-steps e))))
         (process (-> e process (resolve-process market tau)))
         (components (or (regressors e) (process-regressors process)))
         (mask (exercise-mask x market times))
         (fits (make-hash-table)))
    (unless (eql 0 (first components))
      (signal-invalid-argument 'regressors components 'list
                               "state indices starting with 0, the observable"))
    (lsm-sweep p e market tau times components mask fits t)
    (multiple-value-bind (mean se)
        (if (eq (lsm-valuation e) :independent)
            (lsm-sweep p (derive-engine e :seed (+ (* 7919 (seed e)) 104729)) market tau times
                       components mask fits nil)
            (lsm-sweep p e market tau times components mask fits :reuse))
      (values (if (= 1 (sbit mask 0))
                  (max mean (funcall (payoff-kernel p) (spot market)))
                  mean)
              se))))

(defun lsm-sweep (p e market tau times components mask fits fit)
  "The LSM backward sweep over E's paths on TIMES: (values mean se) of the
discounted cash flows, before exercise at time 0. FIT T regresses at each
exercise date and records (active-components . coefficients) in FITS by
step; FIT NIL applies the rule recorded in FITS. FIT :REUSE returns the
result of the last fitting sweep (in-sample valuation)."
  (when (eq fit :reuse)
    (return-from lsm-sweep (values-list (gethash :in-sample fits))))
  (let* ((n-steps (1- (length times)))
         (process (-> e process (resolve-process market tau)))
         (matrices (simulate-state-paths e market tau components :times times))
         (n-paths (path-matrix-n-paths (first matrices)))
         (datas (map 'simple-vector #'path-matrix-data matrices))
         (data (svref datas 0))
         (kernel (payoff-kernel p))
         ;; The spot by the payoff's scale; latent components by their
         ;; value at t = 0.
         (template (initial-state process market))
         (scales (coerce (cons (regression-scale p market)
                               (iter (for c in (rest components))
                                     (collect (let ((v (aref template c)))
                                                (if (zerop v) 1d0 (float v 1d0))))))
                         '(simple-array double-float (*))))
         (regressions (make-hash-table :test 'equal))
         (discounts (grid-step-discounts market times))
         (cashflows (make-array n-paths :element-type 'double-float))
         (ranges (regression-blocks n-paths)))
    (declare (type (simple-array double-float (*)) data cashflows discounts)
             (type simple-bit-vector mask)
             (type (function (double-float) double-float) kernel)
             (fixnum n-paths n-steps))
    (flet ((regression-setup (active)
             ;; (values datas n-basis make-filler) for the regressor
             ;; components ACTIVE, cached: one basis per set of regressors.
             (or (gethash active regressions)
                 (setf (gethash active regressions)
                       (let* ((positions (mapcar (lambda (c) (position c components)) active))
                              (exponents (basis-exponents (length active) (basis-degree e)))
                              (scales (map '(simple-array double-float (*))
                                           (lambda (i) (aref scales i)) positions)))
                         (list (map 'simple-vector (lambda (i) (svref datas i)) positions)
                               (length exponents)
                               (lambda () (make-basis-filler (basis e) exponents scales
                                                             (basis-degree e))))))))
           (active-components (col-offset)
             ;; The spot, and every latent component that varies across the
             ;; in-the-money paths at this date: a constant one (Heston with
             ;; xi = 0) would make the regression rank deficient.
             (cons 0 (iter (for c in (rest components))
                           (for column = (svref datas (position c components)))
                           (when (let ((lo most-positive-double-float) (hi most-negative-double-float))
                                   (declare (double-float lo hi))
                                   (dotimes (i n-paths)
                                     (when (plusp (funcall kernel (aref data (+ col-offset i))))
                                       (let ((v (aref (the (simple-array double-float (*)) column)
                                                      (+ col-offset i))))
                                         (setf lo (min lo v) hi (max hi v)))))
                                   (< lo hi))
                             (collect c)))))
           (exercise! (datas n-basis make-filler beta col-offset)
             ;; Exercise where intrinsic beats fitted continuation.
             (declare (type simple-vector datas) (fixnum n-basis col-offset)
                      (type function make-filler)
                      (type (simple-array double-float (*)) beta))
             (lp:pmapc
              (lambda (range)
                (let ((basis (make-array n-basis :element-type 'double-float))
                      (values (make-array (length datas) :element-type 'double-float))
                      (filler (funcall make-filler)))
                  (declare (type function filler))
                  (iter (declare (declare-variables))
                        (for (the fixnum i) from (car range) below (cdr range))
                        (for (the double-float s) = (aref data (+ col-offset i)))
                        (for (the double-float intrinsic) = (funcall kernel s))
                        (when (plusp intrinsic)
                          (dotimes (r (length datas))
                            (setf (aref values r)
                                  (aref (the (simple-array double-float (*)) (svref datas r))
                                        (+ col-offset i))))
                          (funcall filler basis values)
                          (let ((cont 0d0))
                            (declare (double-float cont))
                            (dotimes (n n-basis)
                              (incf cont (* (aref beta n) (aref basis n))))
                            (when (> intrinsic cont)
                              (setf (aref cashflows i) intrinsic)))))))
              ranges)))
      (with-pricing-kernel ()
        ;; Terminal cash flows.
        (let ((terminal (* n-steps n-paths)))
          (dotimes (i n-paths)
            (setf (aref cashflows i) (funcall kernel (aref data (+ terminal i))))))
        ;; Backward induction. Steps the mask excludes only discount.
        (iter (declare (declare-variables))
              (for step from (1- n-steps) downto 1)
              (for (the fixnum col-offset) = (* step n-paths))
              (for (the double-float df) = (aref discounts step))
              (cond
                ((zerop (sbit mask step))
                 (dotimes (i n-paths)
                   (setf (aref cashflows i) (* df (aref cashflows i)))))
                ((not fit)
                 ;; Apply the fitted rule, if there was one at this step.
                 (dotimes (i n-paths)
                   (setf (aref cashflows i) (* df (aref cashflows i))))
                 (destructuring-bind (&optional active . beta) (gethash step fits)
                   (when beta
                     (destructuring-bind (datas n-basis make-filler) (regression-setup active)
                       (exercise! datas n-basis make-filler beta col-offset)))))
                (t
                 (restart-case
                     (let ((active (active-components col-offset)))
                       (destructuring-bind (datas n-basis make-filler) (regression-setup active)
                         (declare (type simple-vector datas) (fixnum n-basis)
                                  (type function make-filler))
                         (multiple-value-bind (acc itm)
                             (regress-continuation datas cashflows col-offset kernel make-filler
                                                   n-basis df ranges)
                           (when (> itm (* 2 n-basis))
                             (let ((beta (handler-case (qr-solve acc)
                                           (rank-deficient (c)
                                             (error 'regression-failure :step step :cause c)))))
                               (declare (type (simple-array double-float (*)) beta))
                               (setf (gethash step fits) `(,active . ,beta))
                               (exercise! datas n-basis make-filler beta col-offset))))))
                   (skip-exercise-date ()
                     :report "Skip early exercise at this date and continue."
                     nil)))))
        ;; Discount the first step. Under antithetics a sample is the mean of
        ;; a pair (path i and its partner i + n/2), so the standard error is
        ;; that of the estimator.
        (let ((result (multiple-value-list
                       (summarize-samples e (if (antithetic e) (pair-means cashflows) cashflows)
                                          :discount (aref discounts 0)))))
          (when fit (setf (gethash :in-sample fits) result))
          (values-list result))))))

;;; Any payoff and process: the slow fallback, at 20,000 paths rather than
;;; the engine's 100,000. For an American under Heston that is about 0.6 s
;;; with a standard error near 0.7% of the price; (:fallback ...) in
;;; WITH-ENGINES changes it.
(defmethod engine-candidates append ((designator (eql :mc)) exercise payoff path
                                     (process stochastic-process))
  (declare (ignore exercise payoff path))
  '((90 (:mc :n-paths 20000))))
