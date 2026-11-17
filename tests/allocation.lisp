;;;; allocation.lisp --- bytes consed per call by the closures the engines run in their loops
;;;;
;;;; A closure called per path step, per Fourier term or per sample must not
;;;; allocate beyond what returning a boxed result costs. Each case calls one
;;;; closure 10,000 times on fixed inputs and compares the bytes consed with a
;;;; ceiling pinned at the value measured when the closure was last audited.
;;;; Every nonzero ceiling says what allocates. Lowering a ceiling is free;
;;;; raising one needs a reason in the commit.
;;;;
;;;; Bytes are counted by SBCL only; elsewhere the suite prints a skip line.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defparameter *allocation-calls* 10000
  "Calls of each closure in an allocation check.")

(defparameter *allocation-slack* 2048
  "Bytes of allowance over the whole run for what the measuring loop itself
may allocate, about 0.2 bytes per call.")

(defun bytes-per-call (thunk)
  "Bytes consed per call by THUNK, a function of no arguments, over
*ALLOCATION-CALLS* calls after one warm-up call."
  (declare (function thunk))
  (funcall thunk)
  (let ((before (bytes-consed)))
    (dotimes (i *allocation-calls*) (funcall thunk))
    (/ (- (bytes-consed) before) (float *allocation-calls* 1d0))))

(defun check-allocation (name ceiling reason thunk)
  "Check that THUNK conses at most CEILING bytes per call; REASON says what
allocates when CEILING is not 0."
  (let ((measured (bytes-per-call thunk)))
    (check-that (format nil "~A: ~,1F bytes per call (ceiling ~D~@[: ~A~])" name measured ceiling reason)
                (<= measured (+ ceiling (/ *allocation-slack* *allocation-calls*))))))

(defparameter *allocation-times*
  (coerce '(0d0 0.1d0 0.2d0 0.3d0 0.4d0 0.5d0 0.6d0 0.7d0 0.8d0 0.9d0 1d0)
          '(simple-array double-float (*)))
  "The ten-step grid the stepper cases run on.")

(defun check-stepper-allocation (name process ceiling reason &key (times *allocation-times*))
  "Allocation of the MAKE-STEPPER closure of PROCESS (resolved on the case's
market) advancing one step from the spot, on the grid TIMES."
  (let* ((market (market-view (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                                              :vol 0.2d0)))
         (process (fincl::resolve-process process market 1d0))
         (stepper (make-stepper process market times))
         (state (copy-seq (fincl::initial-state process market)))
         (normals (make-array 6 :element-type 'double-float :initial-element 0.3d0))
         (step 0))
    (declare (function stepper) (fixnum step))
    (check-allocation name ceiling reason
                      (lambda ()
                        (setf (aref state 0) 100d0)
                        (funcall stepper state normals (setf step (mod (1+ step) 10)))))))

(defun check-characteristic-function-allocation (name process ceiling reason)
  "Allocation of one evaluation of PROCESS's characteristic function to one year."
  (let* ((market (market-view (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                                              :vol 0.2d0)))
         (process (fincl::resolve-process process market 1d0))
         (cf (characteristic-function process market 1d0))
         (u 0d0))
    (declare (function cf) (double-float u))
    (check-allocation name ceiling reason (lambda () (funcall cf (incf u 0.37d0))))))

(defun check-sampler-allocation (name process ceiling reason)
  "Allocation of one draw from PROCESS's terminal sampler to one year."
  (let* ((market (market-view (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                                              :vol 0.2d0)))
         (sampler (terminal-sampler (fincl::resolve-process process market 1d0) market 1d0))
         (normals (make-array 3 :element-type 'double-float :initial-element 0.2d0)))
    (declare (function sampler))
    (check-allocation name ceiling reason (lambda () (funcall sampler normals)))))

(defparameter *boxed-result* "the boxed double it returns")
(defparameter *boxed-complex* "the boxed complex it returns")

(define-checks allocation-checks
  (format t "~&Allocation of the closures in the engines' inner loops~%")
  (when-sbcl-checks ("allocation ceilings" "bytes consed are counted on SBCL only")
    (format t "~&Path steppers~%")
    (check-stepper-allocation "GBM" (make-gbm) 0 nil)
    (check-stepper-allocation "CEV, log-Euler" (make-cev :beta 0.7d0) 0 nil)
    (check-stepper-allocation "CEV, absorbing" (make-cev :beta 0.7d0 :scheme :absorbing-euler) 0 nil)
    (check-stepper-allocation "Heston, quadratic-exponential" (make-heston) 0 nil)
    (check-stepper-allocation "Heston, full truncation" (make-heston :scheme :full-truncation) 0 nil)
    (check-stepper-allocation "Bates" (make-bates) 0 nil)
    (check-stepper-allocation "local vol" (make-local-vol) 0 nil)
    (let ((garch (make-garch)))
      ;; A discrete-time model steps only over whole model periods.
      (check-stepper-allocation "GARCH" garch 0 nil
                                :times (coerce (iter (for i to 10) (collect (* i (fincl::period garch))))
                                               '(simple-array double-float (*)))))
    (check-stepper-allocation "Merton" (make-merton-jump) 32 *boxed-result*)
    (check-stepper-allocation "Kou" (make-kou-jump) 32 *boxed-result*)
    (check-stepper-allocation "NIG" (make-nig) 96 "the inverse Gaussian draw and its normal CDF")
    (check-stepper-allocation "variance gamma" (make-variance-gamma) 2752
                              "REGULARIZED-GAMMA in the gamma quantile's Newton iteration")
    (format t "~&Characteristic functions~%")
    (check-characteristic-function-allocation "GBM" (make-gbm) 96 *boxed-complex*)
    (check-characteristic-function-allocation "Heston" (make-heston) 160 *boxed-complex*)
    (check-characteristic-function-allocation "Bates" (make-bates) 352
                                              "two boxed complex results, one per closure it composes")
    (check-characteristic-function-allocation "Merton" (make-merton-jump) 192 *boxed-complex*)
    (check-characteristic-function-allocation "variance gamma" (make-variance-gamma) 128 *boxed-complex*)
    (check-characteristic-function-allocation "CGMY" (make-cgmy) 608
                                              "COMPLEX LOG and EXPT on complex doubles")
    (format t "~&Terminal samplers, payoffs and path kernels~%")
    (check-sampler-allocation "GBM terminal sampler" (make-gbm) 32 *boxed-result*)
    (check-sampler-allocation "Merton terminal sampler" (make-merton-jump) 48
                                "the boxed double it returns and the normal CDF of the Poisson draw")
    (let ((kernel (payoff-kernel (option-payoff (make-option (call 100) (expires #D"2027-09-24"))))))
      (declare (function kernel))
      (check-allocation "vanilla payoff kernel" 32 *boxed-result* (lambda () (funcall kernel 105d0))))
    (let* ((market (market-view (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                                                :vol 0.2d0)))
           (grid (fincl::make-time-grid market :dates `(#D"2027-05-01" #D"2027-09-24")))
           (process (fincl::resolve-process (make-gbm) market 1d0))
           (state (make-array 1 :element-type 'double-float :initial-element 103d0))
           (acc (make-array 3 :element-type 'double-float :initial-element 0d0))
           (pay (lambda (amount k) (declare (ignore amount k)) nil)))
      (dolist (case `(("arithmetic Asian" ,(make-option (call 100)
                                                         (average :arithmetic :dates `(#D"2027-05-01" #D"2027-09-24"))
                                                         (expires #D"2027-09-24")))
                      ("geometric Asian" ,(make-option (call 100)
                                                        (average :geometric :dates `(#D"2027-05-01" #D"2027-09-24"))
                                                        (expires #D"2027-09-24")))
                      ("fixed lookback" ,(make-option (call 100) (lookback :fixed) (expires #D"2027-09-24")))
                      ("continuous barrier" ,(make-option (call 100) (barrier :down-and-out 90)
                                                          (expires #D"2027-09-24")))))
        (destructuring-bind (name option) case
          (multiple-value-bind (init step) (fincl::path-kernel (option-payoff option)
                                                                (fincl::option-path option) process grid)
            (declare (function init step))
            (funcall init acc)
            (check-allocation (format nil "~A step" name) 0 nil
                              (lambda () (funcall step acc 1 state pay)))))))
    (format t "~&Normal fillers~%")
    (let ((engine (make-engine :mc :process (make-gbm) :n-paths 1000)))
      (multiple-value-bind (fill start) (fincl::make-normal-filler engine 0)
        (declare (function fill start))
        (let ((vec (make-array 4 :element-type 'double-float)))
          (funcall start 0)
          (check-allocation "counter rng, Box-Muller" 0 nil (lambda () (funcall fill vec))))))))
