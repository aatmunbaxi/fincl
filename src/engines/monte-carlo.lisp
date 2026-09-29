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

(defun make-normal-filler (engine chunk-index)
  "A closure (lambda (vec)) that fills VEC, a (SIMPLE-ARRAY DOUBLE-FLOAT (*)),
with standard normals from an independent, deterministically seeded stream
for work unit CHUNK-INDEX. The chunk count is an engine setting, so the
stream layout, and hence the answer, does not change with the number of
worker threads.

The generator and ENGINE's SAMPLER are resolved here, once: the closure makes
no generic call and no sampler test per draw. It fills a vector rather than
returning one normal, so no double-float is boxed on the way out."
  (let ((u (make-uniform-fn (rng engine) (+ (* 1000003 (seed engine)) chunk-index))))
    (declare (type function u))
    (ecase (sampler engine)
      (:inverse-transform
       ;; Needed if the stream is a low-discrepancy sequence.
       (lambda (vec)
         (declare (type (simple-array double-float (*)) vec))
         (dotimes (i (length vec) vec)
           (setf (aref vec i) (norm-quantile (max 1d-16 (the double-float (funcall u))))))))
      (:box-muller
       ;; Draws come in pairs; the second is kept for the next call, even
       ;; across paths, exactly as before.
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
  "Step every path of ENGINE's process over [0, TAU] in N-STEPS steps and
show each state to a visitor, without storing paths. Work is split into
N-CHUNKS chunks, each with its own normal stream, run in parallel.

For each chunk, (funcall MAKE-VISITOR chunk-index start end n-base) returns
(values visit finish). The chunk steps the base paths [START, END) and, under
antithetics, their partners [START + N-BASE, END + N-BASE). It calls
(visit path-index step state) at steps 0 .. N-STEPS of every path; STATE is
reused, so copy anything kept. It calls (finish) when the chunk is done.
Returns the FINISH values in chunk order, whatever the worker count.

  (map-paths e m 1d0 (lambda (chunk start end n-base)
                       (values (lambda (i j state) ...) (lambda () result))))"
  (let* ((p (process engine))
         (n-steps (n-steps engine))
         (anti (antithetic engine))
         (n-base (if anti (/ (total-paths engine) 2) (total-paths engine)))
         (dt (/ tau n-steps))
         (stepper (make-stepper p market dt))   ; dispatch once, not per path
         (n-factors (process-factors p))
         (template (initial-state p market))
         (ranges (chunk-ranges n-base (n-chunks engine))))
    (declare (type function stepper make-visitor)
             (fixnum n-base n-steps n-factors))
    (flet ((run-chunk (range index)
             (multiple-value-bind (visit finish)
                 (funcall make-visitor index (car range) (cdr range) n-base)
               (declare (type function visit finish))
               (let ((g (make-normal-filler engine index))
                     (state (copy-seq template))
                     (mirror (copy-seq template))
                     (normals (make-array n-factors :element-type 'double-float))
                     (anti-normals (make-array n-factors :element-type 'double-float)))
                 (declare (type (simple-array double-float (*))
                                state mirror normals anti-normals))
                 (loop for i fixnum from (car range) below (cdr range)
                       for partner fixnum = (+ i n-base)
                       do (replace state template)
                          (replace mirror template)
                          (funcall visit i 0 state)
                          (when anti (funcall visit partner 0 mirror))
                          (loop for j fixnum from 1 to n-steps
                                do (fill-normals g normals)
                                   (funcall stepper state normals)
                                   (funcall visit i j state)
                                   (when anti
                                     (negate-into normals anti-normals)
                                     (funcall stepper mirror anti-normals)
                                     (funcall visit partner j mirror))))
                 (funcall finish)))))
      (with-pricing-kernel ()
        (lp:pmap 'list #'run-chunk ranges (a:iota (length ranges)))))))

(defun simulate-paths (engine market tau)
  "The PATH-MATRIX of the process observable, (total paths) x (N-STEPS + 1),
antithetic partners in the second half. Works for any process: the engine
only calls MAKE-STEPPER."
  (let* ((n-paths (total-paths engine))
         (paths (make-path-matrix n-paths (1+ (n-steps engine))))
         (data (path-matrix-data paths)))
    (declare (type (simple-array double-float (*)) data) (fixnum n-paths))
    (map-paths engine market tau
               (lambda (chunk start end n-base)
                 (declare (ignore chunk start end n-base))
                 (values (lambda (i j state)
                           (declare (fixnum i j)
                                    (type (simple-array double-float (*)) state))
                           (setf (aref data (+ i (the fixnum (* j n-paths)))) (aref state 0)))
                         (constantly nil))))
    paths))

;;; --------------------------------------------------------------------
;;; European: use the terminal shortcut when the process offers one
;;; --------------------------------------------------------------------

(defun price-european-by-sampling (sampler engine market tau kernel)
  "Terminal-only Monte Carlo: no path is ever materialized."
  (let* ((anti (antithetic engine))
         (n (if anti (ceiling (n-paths engine) 2) (n-paths engine)))
         (n-factors (process-factors (process engine)))
         (ranges (chunk-ranges n (n-chunks engine))))
    (declare (type function sampler kernel) (fixnum n n-factors))
    (flet ((run-chunk (range index)
             (let ((g (make-normal-filler engine index))
                   (normals (make-array n-factors :element-type 'double-float))
                   (anti-normals (make-array n-factors :element-type 'double-float))
                   (samples (make-array (- (cdr range) (car range))
                                        :element-type 'double-float)))
               (dotimes (i (length samples))
                 (fill-normals g normals)
                 (let ((v (funcall kernel (funcall sampler normals))))
                   (declare (double-float v))
                   (when anti
                     (negate-into normals anti-normals)
                     (setf v (* 0.5d0 (+ v (funcall kernel
                                                    (funcall sampler anti-normals))))))
                   (setf (aref samples i) v)))
               (moments samples))))
      (let ((accumulators
              (with-pricing-kernel ()
                (lp:pmap 'list #'run-chunk ranges (a:iota (length ranges))))))
        ;; POOL merges chunk accumulators exactly; no re-reduction of raw sums.
        (summarize (reduce #'sts:pool accumulators)
                   :discount (discount-factor market tau))))))

;;; Any payoff: a European needs only the observable at expiry, and the
;;; payoff reaches the engine as its PAYOFF-KERNEL.
(defmethod price-mc ((x european-exercise) (p payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (let* ((tau (time-to-expiry x market))
         (kernel (payoff-kernel p))
         (sampler (terminal-sampler (process e) market tau)))
    (if sampler
        (price-european-by-sampling sampler e market tau kernel)
        ;; No exact terminal law: step the paths, keeping only each chunk's
        ;; terminal payoffs, and pool the chunk accumulators.
        (let ((n-steps (n-steps e)))
          (declare (fixnum n-steps) (type function kernel))
          (summarize
           (reduce #'sts:pool
                   (map-paths e market tau
                              (lambda (chunk start end n-base)
                                (declare (ignore chunk) (fixnum start end n-base))
                                (let* ((width (- end start))
                                       (samples (make-array (if (antithetic e) (* 2 width) width)
                                                            :element-type 'double-float)))
                                  (declare (fixnum width))
                                  ;; Base paths first, then their partners.
                                  (values (lambda (i j state)
                                            (declare (fixnum i j)
                                                     (type (simple-array double-float (*)) state))
                                            (when (= j n-steps)
                                              (setf (aref samples (if (< i n-base)
                                                                      (- i start)
                                                                      (+ width (- i n-base start))))
                                                    (funcall kernel (aref state 0)))))
                                          (lambda () (moments samples)))))))
           :discount (discount-factor market tau))))))

;;; --------------------------------------------------------------------
;;; American: Longstaff-Schwartz
;;; --------------------------------------------------------------------

(declaim (inline fill-basis))
(defun fill-basis (basis s scale)
  "Monomials 1, x, x^2, ... of x = S/SCALE into BASIS."
  (declare (type (simple-array double-float (*)) basis) (double-float s scale))
  (let ((x (/ s scale)) (b 1d0))
    (declare (double-float x b))
    (dotimes (n (length basis) basis)
      (setf (aref basis n) b
            b (* b x)))))

(defun regress-continuation (data cashflows col-offset scale kernel n-basis df ranges)
  "Discount CASHFLOWS one step (in place) and regress them on the basis over
the in-the-money paths at COL-OFFSET. Each chunk folds its rows into its own
QR accumulator in parallel; the accumulators are merged in chunk order, so
the fit does not depend on the worker count. Returns (values accumulator
in-the-money-count)."
  (declare (type (simple-array double-float (*)) data cashflows)
           (fixnum col-offset n-basis)
           (double-float scale df)
           (type (function (double-float) double-float) kernel))
  (flet ((run-chunk (range)
           (let ((acc (make-qr-accumulator n-basis))
                 (basis (make-array n-basis :element-type 'double-float)))
             (loop for i fixnum from (car range) below (cdr range)
                   for s double-float = (aref data (+ col-offset i))
                   do (setf (aref cashflows i) (* df (aref cashflows i)))
                      (when (plusp (funcall kernel s))
                        (qr-add-row! acc (fill-basis basis s scale) (aref cashflows i))))
             acc)))
    (let ((total (make-qr-accumulator n-basis)))
      (dolist (acc (lp:pmap 'list #'run-chunk ranges))
        (qr-merge! total acc))
      (values total (qr-accumulator-rows total)))))

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
         (paths (simulate-paths e market tau))
         (n-paths (path-matrix-n-paths paths))
         (data (path-matrix-data paths))
         (kernel (payoff-kernel p))
         (scale (regression-scale p market))
         (n-basis (1+ (basis-degree e)))
         (discounts (step-discounts market tau n-steps))
         (mask (exercise-mask x market (uniform-time-grid tau n-steps)))
         (cashflows (make-array n-paths :element-type 'double-float))
         (ranges (chunk-ranges n-paths (n-chunks e))))
    (declare (type (simple-array double-float (*)) data cashflows discounts)
             (type simple-bit-vector mask)
             (type (function (double-float) double-float) kernel)
             (fixnum n-paths n-steps n-basis)
             (double-float scale))
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
                           (regress-continuation data cashflows col-offset scale kernel
                                                 n-basis df ranges)
                         (when (> itm (* 2 n-basis))
                           (let ((beta (handler-case (qr-solve acc)
                                         (rank-deficient (c)
                                           (error 'regression-failure :step step :cause c)))))
                             (declare (type (simple-array double-float (*)) beta))
                             ;; Exercise where intrinsic beats fitted continuation.
                             (lp:pmapc
                              (lambda (range)
                                (let ((basis (make-array n-basis :element-type 'double-float)))
                                  (loop for i fixnum from (car range) below (cdr range)
                                        for s double-float = (aref data (+ col-offset i))
                                        for intrinsic double-float = (funcall kernel s)
                                        when (plusp intrinsic)
                                          do (fill-basis basis s scale)
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
      (multiple-value-bind (mean se)
          (summarize (moments cashflows) :discount (aref discounts 0))
        (values (if (= 1 (sbit mask 0))
                    (max mean (funcall kernel (spot market)))
                    mean)
                se)))))
