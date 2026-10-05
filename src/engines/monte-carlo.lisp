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
    (loop for start from 0 below n by size
          collect (cons start (min n (+ start size))))))

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
           (loop for i downfrom (- bits chunk) above 0 by chunk
                 do (setf (ldb (byte chunk i) random) (funcall next generator)))
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

(defun make-normal-filler (engine chunk-index)
  "Normals for work unit CHUNK-INDEX, as (values fill start-path). FILL is a
closure (lambda (vec)) that fills VEC, a (SIMPLE-ARRAY DOUBLE-FLOAT (*)),
with standard normals; call (funcall start-path i) before the normals of path
I. The generator and ENGINE's SAMPLER are resolved here, once: FILL makes no
generic call and no sampler test per draw, and fills a vector rather than
returning one normal, so no double-float is boxed on the way out.

Under the :COUNTER rng a path's normals depend only on the seed and the path
index (START-PATH selects them), so results do not depend on the chunking.
Under a stream rng the chunk has one stream and START-PATH does nothing."
  (if (eq (rng engine) :counter)
      (make-counter-normal-filler engine)
      (values (make-stream-normal-filler engine chunk-index)
              (lambda (i) (declare (ignore i))))))

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
    (loop for y double-float across samples
          do (let* ((d (- y m))
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

(defun step-discounts (market tau n-steps)
  "Discount factors between consecutive grid times, from the market protocol
rather than a single flat rate: DF(t_{j+1}) / DF(t_j)."
  (let ((v (make-array n-steps :element-type 'double-float))
        (dt (/ tau n-steps)))
    (dotimes (j n-steps v)
      (setf (aref v j) (/ (discount-factor market (* (1+ j) dt))
                          (discount-factor market (* j dt)))))))

(defun total-paths (engine)
  "Paths the engine simulates: N-PATHS, rounded up to even under antithetics."
  (if (antithetic engine) (* 2 (ceiling (n-paths engine) 2)) (n-paths engine)))

(defun map-paths (engine market tau make-visitor)
  "Step every path of ENGINE's process, resolved at TAU, over a uniform grid
on [0, TAU] of N-STEPS steps and show each state to a visitor, without
storing paths. Work is split into N-CHUNKS chunks run in parallel. Under the
:COUNTER rng path i's normals depend only on the seed and i; under a stream
rng each chunk has its own stream.

For each chunk, (funcall MAKE-VISITOR chunk-index start end n-base) returns
(values visit finish). The chunk steps the base paths [START, END) and, under
antithetics, their partners [START + N-BASE, END + N-BASE). It calls
(visit path-index step state) at steps 0 .. N-STEPS of every path; STATE is
reused, so copy anything kept. It calls (finish) when the chunk is done.
Returns the FINISH values in chunk order, whatever the worker count.

  (map-paths e m 1d0 (lambda (chunk start end n-base)
                       (values (lambda (i j state) ...) (lambda () result))))"
  (let* ((n-steps (n-steps engine))
         (p (resolve-process (process engine) market tau))
         (anti (antithetic engine))
         (n-base (if anti (/ (total-paths engine) 2) (total-paths engine)))
         (stepper (make-stepper p market (uniform-time-grid tau n-steps))) ; dispatch once
         (n-factors (process-factors p))
         (template (initial-state p market))
         (ranges (chunk-ranges n-base (n-chunks engine))))
    (declare (type function stepper make-visitor)
             (fixnum n-base n-steps n-factors))
    (flet ((run-chunk (range index)
             (multiple-value-bind (visit finish)
                 (funcall make-visitor index (car range) (cdr range) n-base)
               (declare (type function visit finish))
               (multiple-value-bind (g start-path) (make-normal-filler engine index)
                 (declare (type function start-path))
                 (let ((state (copy-seq template))
                       (mirror (copy-seq template))
                       (normals (make-array n-factors :element-type 'double-float))
                       (anti-normals (make-array n-factors :element-type 'double-float)))
                   (declare (type (simple-array double-float (*))
                                  state mirror normals anti-normals))
                   (loop for i fixnum from (car range) below (cdr range)
                         for partner fixnum = (+ i n-base)
                         do (funcall start-path i)
                            (replace state template)
                            (replace mirror template)
                            (funcall visit i 0 state)
                            (when anti (funcall visit partner 0 mirror))
                            (loop for j fixnum from 1 to n-steps
                                  do (fill-normals g normals)
                                     (funcall stepper state normals (1- j))
                                     (funcall visit i j state)
                                     (when anti
                                       (negate-into normals anti-normals)
                                       (funcall stepper mirror anti-normals (1- j))
                                       (funcall visit partner j mirror))))
                   (funcall finish))))))
      (with-pricing-kernel ()
        (lp:pmap 'list #'run-chunk ranges (a:iota (length ranges)))))))

(defun simulate-state-paths (engine market tau components)
  "One PATH-MATRIX per state component in COMPONENTS (indices into the state
vector), in the same order: (total paths) x (N-STEPS + 1), antithetic
partners in the second half. Works for any process: the engine only calls
MAKE-STEPPER."
  (let* ((n-paths (total-paths engine))
         (matrices (loop repeat (length components)
                         collect (make-path-matrix n-paths (1+ (n-steps engine)))))
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
                         (constantly nil))))
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
         (ranges (chunk-ranges n (n-chunks engine))))
    (declare (type function sampler kernel) (fixnum n n-factors))
    (flet ((run-chunk (range index)
             (multiple-value-bind (g start-path) (make-normal-filler engine index)
               (declare (type function start-path))
               (let ((normals (make-array n-factors :element-type 'double-float))
                     (anti-normals (make-array n-factors :element-type 'double-float)))
                 (loop for i fixnum from (car range) below (cdr range)
                       do (funcall start-path i)
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
      (summarize (moments samples) :discount (discount-factor market tau)))))

;;; Any payoff: a European needs only the observable at expiry, and the
;;; payoff reaches the engine as its PAYOFF-KERNEL.
(defmethod price-mc ((x european-exercise) (p payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (let* ((tau (time-to-expiry x market))
         (kernel (payoff-kernel p))
         (sampler (terminal-sampler (resolve-process (process e) market tau) market tau)))
    (if sampler
        (price-european-by-sampling sampler e market tau kernel)
        ;; No exact terminal law: step the paths, keeping only the terminal
        ;; payoffs, one slot per base path, and take the statistics in index
        ;; order. Under antithetics a sample is the mean of a pair, as in the
        ;; terminal shortcut, so the standard error is that of the pair
        ;; estimator.
        (let* ((n-steps (n-steps e))
               (n-base (if (antithetic e) (/ (total-paths e) 2) (total-paths e)))
               (samples (make-array n-base :element-type 'double-float)))
          (declare (fixnum n-steps n-base) (type function kernel))
          (map-paths e market tau
                     (lambda (chunk start end n-base)
                       (declare (ignore chunk start end) (fixnum n-base))
                       ;; A base path reaches step N-STEPS before its partner.
                       (values (lambda (i j state)
                                 (declare (fixnum i j)
                                          (type (simple-array double-float (*)) state))
                                 (when (= j n-steps)
                                   (let ((v (funcall kernel (aref state 0))))
                                     (declare (double-float v))
                                     (if (< i n-base)
                                         (setf (aref samples i) v)
                                         (let ((k (- i n-base)))
                                           (setf (aref samples k)
                                                 (* 0.5d0 (+ (aref samples k) v))))))))
                               (constantly nil))))
          (summarize (moments samples) :discount (discount-factor market tau))))))

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
                 (loop for a from total downto 0
                       append (mapcar (lambda (rest) (cons a rest))
                                      (summing-to (1- n) (- total a)))))))
    (loop for total from 0 to degree
          append (summing-to n-regressors total))))

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
    (loop for exps in exponents for i from 0
          do (loop for e in exps for r from 0
                   do (setf (aref offsets (+ (* i m) r)) (+ (* r width) e))))
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
                (loop for k fixnum from 1 below (1- width)
                      do (let ((l2 (/ (- (* (- (+ (* 2 k) 1) x) l1) (* k l0)) (1+ k))))
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
          (loop for r fixnum from 1 below m
                do (setf product (* product (aref tables (aref offsets (+ (* i m) r))))))
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
               (loop for i fixnum from (car range) below (cdr range)
                     for s double-float = (aref observable (+ col-offset i))
                     do (setf (aref cashflows i) (* df (aref cashflows i)))
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
  (loop for start from 0 below n-paths by +regression-block+
        collect (cons start (min n-paths (+ start +regression-block+)))))

(defgeneric regression-scale (payoff market)
  (:documentation "The level LSM divides the observable by before building
its polynomial basis, so the basis is well conditioned: the strike for a
struck payoff, the spot otherwise.")
  (:method ((p payoff) market) (float (spot market) 1d0))
  (:method ((p strike-payoff) market)
    (declare (ignore market))
    (strike p)))

;;; Any payoff: LSM needs only the kernel (exercise value, and which paths
;;; are in the money) and a scale for the regression basis.
(defmethod price-mc ((x american-exercise) (p payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (let* ((tau (time-to-expiry x market))
         (n-steps (n-steps e))
         (process (resolve-process (process e) market tau))
         (components (or (regressors e) (process-regressors process)))
         (matrices (progn
                     (unless (eql 0 (first components))
                       (signal-invalid-argument 'regressors components 'list
                                                "state indices starting with 0, the observable"))
                     (simulate-state-paths e market tau components)))
         (n-paths (path-matrix-n-paths (first matrices)))
         (datas (map 'simple-vector #'path-matrix-data matrices))
         (data (svref datas 0))
         (kernel (payoff-kernel p))
         ;; The spot by the payoff's scale; latent components by their
         ;; value at t = 0.
         (template (initial-state process market))
         (scales (coerce (cons (regression-scale p market)
                               (loop for c in (rest components)
                                     collect (let ((v (aref template c)))
                                               (if (zerop v) 1d0 (float v 1d0)))))
                         '(simple-array double-float (*))))
         (exponents (basis-exponents (length components) (basis-degree e)))
         (n-basis (length exponents))
         (make-filler (lambda () (make-basis-filler (basis e) exponents scales
                                                    (basis-degree e))))
         (discounts (step-discounts market tau n-steps))
         (mask (exercise-mask x market (uniform-time-grid tau n-steps)))
         (cashflows (make-array n-paths :element-type 'double-float))
         (ranges (regression-blocks n-paths)))
    (declare (type (simple-array double-float (*)) data cashflows discounts)
             (type simple-bit-vector mask)
             (type (function (double-float) double-float) kernel)
             (fixnum n-paths n-steps n-basis))
    (with-pricing-kernel ()
      ;; Terminal cash flows.
      (let ((terminal (* n-steps n-paths)))
        (dotimes (i n-paths)
          (setf (aref cashflows i) (funcall kernel (aref data (+ terminal i))))))
      ;; Backward induction. Steps the mask excludes only discount.
      (loop for step from (1- n-steps) downto 1
            for col-offset fixnum = (* step n-paths)
            for df double-float = (aref discounts step)
            do (if (zerop (sbit mask step))
                   (dotimes (i n-paths)
                     (setf (aref cashflows i) (* df (aref cashflows i))))
                   (restart-case
                       (multiple-value-bind (acc itm)
                           (regress-continuation datas cashflows col-offset kernel make-filler
                                                 n-basis df ranges)
                         (when (> itm (* 2 n-basis))
                           (let ((beta (handler-case (qr-solve acc)
                                         (rank-deficient (c)
                                           (error 'regression-failure :step step :cause c)))))
                             (declare (type (simple-array double-float (*)) beta))
                             ;; Exercise where intrinsic beats fitted continuation.
                             (lp:pmapc
                              (lambda (range)
                                (let ((basis (make-array n-basis :element-type 'double-float))
                                      (values (make-array (length datas)
                                                          :element-type 'double-float))
                                      (filler (funcall make-filler)))
                                  (declare (type function filler))
                                  (loop for i fixnum from (car range) below (cdr range)
                                        for s double-float = (aref data (+ col-offset i))
                                        for intrinsic double-float = (funcall kernel s)
                                        when (plusp intrinsic)
                                          do (dotimes (r (length datas))
                                               (setf (aref values r)
                                                     (aref (the (simple-array double-float (*))
                                                                (svref datas r))
                                                           (+ col-offset i))))
                                             (funcall filler basis values)
                                             (let ((cont 0d0))
                                               (declare (double-float cont))
                                               (dotimes (n n-basis)
                                                 (incf cont (* (aref beta n) (aref basis n))))
                                               (when (> intrinsic cont)
                                                 (setf (aref cashflows i) intrinsic))))))
                              ranges))))
                     (skip-exercise-date ()
                       :report "Skip early exercise at this date and continue."
                       nil))))
      ;; Discount the first step, and exercise now if the mask allows it.
      ;; Under antithetics a sample is the mean of a pair (path i and its
      ;; partner i + n/2), so the standard error is that of the estimator.
      (multiple-value-bind (mean se)
          (summarize (moments (if (antithetic e) (pair-means cashflows) cashflows))
                     :discount (aref discounts 0))
        (values (if (= 1 (sbit mask 0))
                    (max mean (funcall kernel (spot market)))
                    mean)
                se)))))
