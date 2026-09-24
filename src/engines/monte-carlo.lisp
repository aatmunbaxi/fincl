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

(defun cpu-count ()
  #+sbcl (max 1 (or (ignore-errors
                     (parse-integer
                      (with-output-to-string (s)
                        (sb-ext:run-program "nproc" '() :search t :output s))
                      :junk-allowed t))
                    1))
  #-sbcl 4)

(defun chunk-ranges (n n-chunks)
  "Split [0, N) into contiguous (start . end) ranges."
  (let ((size (ceiling n (max 1 (min n n-chunks)))))
    (loop for start from 0 below n by size
          collect (cons start (min n (+ start size))))))

;;; --------------------------------------------------------------------
;;; Random numbers
;;; --------------------------------------------------------------------

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
     (let ((g (rs:make-generator type seed)))
       (lambda () (rs:random-unit g 'double-float))))
    ((list (guard type (keywordp type)) (guard offset (integerp offset)))
     (let ((g (rs:make-generator type (+ seed offset))))
       (lambda () (rs:random-unit g 'double-float))))))

(defstruct (gaussian-source (:constructor %make-gaussian-source (uniform sampler)))
  (uniform nil :type function)
  sampler
  (spare 0d0 :type double-float)
  (has-spare nil))

(defun make-chunk-source (engine chunk-index)
  "An independent, deterministically seeded stream per work unit. The chunk
count is an engine setting, so the stream layout — and hence the answer —
does not change with the number of worker threads."
  (%make-gaussian-source
   (make-uniform-fn (rng engine) (+ (* 1000003 (seed engine)) chunk-index))
   (sampler engine)))

(declaim (inline next-gaussian))
(defun next-gaussian (g)
  (if (gaussian-source-has-spare g)
      (progn (setf (gaussian-source-has-spare g) nil)
             (gaussian-source-spare g))
      (let ((u (gaussian-source-uniform g)))
        (declare (type function u))
        (if (eq (gaussian-source-sampler g) :inverse-transform)
            ;; Needed if the stream is a low-discrepancy sequence.
            (norm-quantile (max 1d-16 (funcall u)))
            (let* ((u1 (max 1d-16 (funcall u)))
                   (u2 (funcall u))
                   (rad (sqrt (* -2d0 (log u1))))
                   (theta (* 2d0 pi u2)))
              (declare (double-float u1 u2 rad theta))
              (setf (gaussian-source-spare g) (* rad (sin theta))
                    (gaussian-source-has-spare g) t)
              (* rad (cos theta)))))))

(declaim (inline fill-normals negate-into))

(defun fill-normals (source vec)
  (declare (type (simple-array double-float (*)) vec))
  (dotimes (i (length vec) vec)
    (setf (aref vec i) (next-gaussian source))))

(defun negate-into (source target)
  "Antithetic partner of SOURCE's draws."
  (declare (type (simple-array double-float (*)) source target))
  (dotimes (i (length source) target)
    (setf (aref target i) (- (aref source i)))))

;;; --------------------------------------------------------------------
;;; Statistics
;;; --------------------------------------------------------------------

(defun moments (samples)
  (sts:central-sample-moments samples :degree 2))

(defun summarize (accumulator &key (discount 1d0))
  "Mean and standard error from a (possibly pooled) accumulator."
  (let ((n (sts:tally accumulator)))
    (values (* discount (sts:mean accumulator))
            (* discount (sqrt (/ (sts:variance accumulator) n))))))

;;; --------------------------------------------------------------------
;;; Path generation
;;; --------------------------------------------------------------------

(defun storage (tensor)
  "Raw storage of a magicl tensor, for tight loops. MAGICL::STORAGE is
internal, so the dependency is isolated here."
  (magicl::storage tensor))

(defun step-discounts (market tau n-steps)
  "Discount factors between consecutive grid times, from the market protocol
rather than a single flat rate: DF(t_{j+1}) / DF(t_j)."
  (let ((v (make-array n-steps :element-type 'double-float))
        (dt (/ tau n-steps)))
    (dotimes (j n-steps v)
      (setf (aref v j) (/ (discount-factor market (* (1+ j) dt))
                          (discount-factor market (* j dt)))))))

(defun simulate-paths (engine market tau)
  "N-PATHS x (N-STEPS+1) column-major magicl matrix of the process observable.
Column-major keeps each time slice contiguous, which is what the LSM backward
sweep reads. Works for any process: the engine only calls MAKE-STEPPER."
  (let* ((p (process engine))
         (n-steps (n-steps engine))
         (anti (antithetic engine))
         (n-paths (if anti (* 2 (ceiling (n-paths engine) 2)) (n-paths engine)))
         (paths (magicl:empty (list n-paths (1+ n-steps))
                              :type 'double-float :layout :column-major))
         (data (storage paths))
         (dt (/ tau n-steps))
         (stepper (make-stepper p market dt))   ; dispatch once, not per path
         (n-factors (process-factors p))
         (template (initial-state p market))
         (n-base (if anti (/ n-paths 2) n-paths))
         (ranges (chunk-ranges n-base (n-chunks engine))))
    (declare (type (simple-array double-float (*)) data)
             (type function stepper)
             (fixnum n-paths n-base n-steps n-factors))
    (flet ((run-chunk (range index)
             (let ((g (make-chunk-source engine index))
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
                        (setf (aref data i) (aref state 0))
                        (when anti (setf (aref data partner) (aref mirror 0)))
                        (loop for j fixnum from 1 to n-steps
                              for col fixnum = (* j n-paths)
                              do (fill-normals g normals)
                                 (funcall stepper state normals)
                                 (setf (aref data (+ i col)) (aref state 0))
                                 (when anti
                                   (negate-into normals anti-normals)
                                   (funcall stepper mirror anti-normals)
                                   (setf (aref data (+ partner col))
                                         (aref mirror 0))))))))
      (with-pricing-kernel ()
        (lp:pmapc #'run-chunk ranges (a:iota (length ranges)))))
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
             (let ((g (make-chunk-source engine index))
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

(defmethod price-mc ((x european-exercise) (p vanilla-payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (let* ((tau (expiry x))
         (kernel (payoff-kernel p))
         (sampler (terminal-sampler (process e) market tau)))
    (if sampler
        (price-european-by-sampling sampler e market tau kernel)
        ;; No exact terminal law: step the paths and read the last column.
        (let* ((paths (simulate-paths e market tau))
               (n-paths (magicl:nrows paths))
               (data (storage paths))
               (terminal (* (n-steps e) n-paths))
               (samples (make-array n-paths :element-type 'double-float)))
          (declare (type (simple-array double-float (*)) data samples))
          (dotimes (i n-paths)
            (setf (aref samples i) (funcall kernel (aref data (+ terminal i)))))
          (summarize (moments samples) :discount (discount-factor market tau))))))

;;; --------------------------------------------------------------------
;;; American: Longstaff-Schwartz
;;; --------------------------------------------------------------------

(defun accumulate-normal-equations (data cashflows col-offset strike kernel
                                    n-basis df ranges)
  "Discount CASHFLOWS one step (in place) and accumulate X'X and X'y over
in-the-money paths, in parallel."
  (declare (type (simple-array double-float (*)) data cashflows)
           (fixnum col-offset n-basis)
           (double-float strike df)
           (type (function (double-float) double-float) kernel))
  (flet ((run-chunk (range)
           (let ((xtx (make-array (* n-basis n-basis) :element-type 'double-float
                                                      :initial-element 0d0))
                 (xty (make-array n-basis :element-type 'double-float
                                          :initial-element 0d0))
                 (basis (make-array n-basis :element-type 'double-float))
                 (count 0))
             (declare (fixnum count))
             (loop for i fixnum from (car range) below (cdr range)
                   for s double-float = (aref data (+ col-offset i))
                   do (setf (aref cashflows i) (* df (aref cashflows i)))
                      (when (plusp (funcall kernel s))
                        (incf count)
                        ;; Basis 1, x, x^2, ... with x = S/K for conditioning.
                        (let ((x (/ s strike)) (b 1d0))
                          (declare (double-float x b))
                          (dotimes (n n-basis)
                            (setf (aref basis n) b
                                  b (* b x))))
                        (dotimes (r n-basis)
                          (incf (aref xty r) (* (aref basis r) (aref cashflows i)))
                          (dotimes (c n-basis)
                            (incf (aref xtx (+ (* r n-basis) c))
                                  (* (aref basis r) (aref basis c)))))))
             (list xtx xty count))))
    (let ((partials (lp:pmap 'list #'run-chunk ranges)))
      (values (reduce #'nu:e+ partials :key #'first)
              (reduce #'nu:e+ partials :key #'second)
              (reduce #'+ partials :key #'third)))))

(defun solve-normal-equations (xtx xty n-basis step)
  "Least-squares coefficients via magicl/LAPACK. QR (dgels) would be better
conditioned than normal equations at higher basis degrees."
  (handler-case
      (storage (magicl:linear-solve
                (magicl:from-array xtx (list n-basis n-basis) :layout :row-major)
                (magicl:from-array xty (list n-basis))))
    (error (c) (error 'regression-failure :step step :cause c))))

(defmethod price-mc ((x american-exercise) (p vanilla-payoff)
                     (f path-independent) (e monte-carlo-engine) market)
  (let* ((tau (expiry x))
         (n-steps (n-steps e))
         (paths (simulate-paths e market tau))
         (n-paths (magicl:nrows paths))
         (data (storage paths))
         (kernel (payoff-kernel p))
         (strike (strike p))
         (n-basis (1+ (basis-degree e)))
         (discounts (step-discounts market tau n-steps))
         (cashflows (make-array n-paths :element-type 'double-float))
         (ranges (chunk-ranges n-paths (n-chunks e))))
    (declare (type (simple-array double-float (*)) data cashflows discounts)
             (type (function (double-float) double-float) kernel)
             (fixnum n-paths n-steps n-basis)
             (double-float strike))
    (with-pricing-kernel ()
      ;; Terminal cash flows.
      (let ((terminal (* n-steps n-paths)))
        (dotimes (i n-paths)
          (setf (aref cashflows i) (funcall kernel (aref data (+ terminal i))))))
      ;; Backward induction over exercise dates.
      (loop for step from (1- n-steps) downto 1
            for col-offset fixnum = (* step n-paths)
            do (restart-case
                   (multiple-value-bind (xtx xty itm)
                       (accumulate-normal-equations data cashflows col-offset
                                                    strike kernel n-basis
                                                    (aref discounts step) ranges)
                     (when (> itm (* 2 n-basis))
                       (let ((beta (solve-normal-equations xtx xty n-basis step)))
                         (declare (type (simple-array double-float (*)) beta))
                         ;; Exercise where intrinsic beats fitted continuation.
                         (lp:pmapc
                          (lambda (range)
                            (loop for i fixnum from (car range) below (cdr range)
                                  for s double-float = (aref data (+ col-offset i))
                                  for intrinsic double-float = (funcall kernel s)
                                  when (plusp intrinsic)
                                    do (let ((xr (/ s strike)) (b 1d0) (cont 0d0))
                                         (declare (double-float xr b cont))
                                         (dotimes (n n-basis)
                                           (incf cont (* (aref beta n) b))
                                           (setf b (* b xr)))
                                         (when (> intrinsic cont)
                                           (setf (aref cashflows i) intrinsic)))))
                          ranges))))
                 (skip-exercise-date ()
                   :report "Skip early exercise at this date and continue."
                   nil)))
      ;; Discount the first step and allow immediate exercise.
      (multiple-value-bind (mean se)
          (summarize (moments cashflows) :discount (aref discounts 0))
        (values (max mean (funcall kernel (spot market))) se)))))
