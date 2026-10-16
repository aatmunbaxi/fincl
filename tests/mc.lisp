;;;; mc.lisp --- the Monte Carlo engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun mc-checks ()
  "Monte Carlo under GBM against Black-Scholes and the LSM benchmark."
  (with-standard-fixtures
    (format t "~&Monte Carlo, GBM (benchmarks 10.4506 / 4.478)~%")
    ;; Within 4 standard errors (about 0.065 at 400k paths).
    (iter (for (label engine) in `(("terminal sampler" ,(mc :n-paths 400000))
                                   ("PCG stream" ,(mc :n-paths 400000 :rng :pcg))
                                   ("inverse transform"
                                    ,(mc :n-paths 400000 :sampler :inverse-transform))))
          (multiple-value-bind (v se) (price call engine mkt)
            (check (format nil "European, ~A" label) 10.4506 v (* 4 se))))
    (check "LSM American put" 4.478
           (price aput (mc :n-paths 100000 :n-steps 50) mkt2) 0.02)
    (check-that "thread-count invariance"
                (= (price aput (mc :n-paths 20000) mkt2)
                   (let ((lp:*kernel* (lp:make-kernel 1)))
                     (unwind-protect (price aput (mc :n-paths 20000) mkt2)
                       (lp:end-kernel :wait t)))))
    ;; Degree 8 makes the normal equations' condition number ~1e16; the QR
    ;; regression never forms them.
    (let ((bbsr (price aput (binomial 4000) mkt2)))
      (multiple-value-bind (v se) (price aput (mc :n-paths 100000 :n-steps 50 :basis-degree 8) mkt2)
        (check "LSM with a degree-8 basis = BBSR binomial" bbsr v (+ (* 3 se) 0.01))))
    (check-that "a stepped European is identical for 1 and 4 workers"
                (let ((stepped (mc :n-paths 20000 :n-steps 20
                                   :process (make-instance 'heston :xi 0.5d0))))
                  (= (price call stepped mkt)
                     (let ((lp:*kernel* (lp:make-kernel 1)))
                       (unwind-protect (price call stepped mkt)
                         (lp:end-kernel :wait t))))))))

;;; --------------------------------------------------------------------
;;; Counter-based random streams (task 09)
;;; --------------------------------------------------------------------

(defun with-workers (n thunk)
  (let ((lp:*kernel* (lp:make-kernel n)))
    (unwind-protect (funcall thunk)
      (lp:end-kernel :wait t))))

(defun terminal-column (matrix)
  "The last time slice of a PATH-MATRIX, one value per path."
  (let ((n (fincl::path-matrix-n-paths matrix)))
    (subseq (fincl::path-matrix-data matrix) (* n (1- (fincl::path-matrix-n-times matrix))))))

(defun seconds (thunk)
  (let ((t0 (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) t0) (float internal-time-units-per-second 1d0))))

(defun counter-stream-checks ()
  (format t "~&Counter-based random streams~%")
  (with-standard-fixtures
    (let ((heston (make-instance 'heston :xi 0.5d0 :rho -0.7d0)))
      ;; (label thunk-of-n-chunks): every engine path, terminal, stepped and LSM.
      (iter (for (label price) in
             `(("terminal GBM European"
                ,(lambda (k) (price call (mc :n-paths 20000 :n-chunks k) mkt)))
               ("stepped Heston European"
                ,(lambda (k) (price call (mc :n-paths 20000 :n-steps 20 :n-chunks k
                                             :process heston)
                                    mkt)))
               ("LSM American put"
                ,(lambda (k) (price aput (mc :n-paths 20000 :n-chunks k) mkt2)))))
            (let ((prices (iter (for k in '(1 7 64))
                                (appending (iter (for w in '(1 2 8))
                                                 (collect (with-workers w (lambda ()
                                                                            (funcall price k)))))))))
              (check-that (format nil "~A: = for n-chunks 1, 7, 64 x 1, 2, 8 workers (~,6F)"
                                  label (first prices))
                          (every (lambda (p) (= p (first prices))) prices)))))
    (flet ((terminals (n-paths antithetic)
             (terminal-column (fincl::simulate-paths (mc :n-paths n-paths :n-steps 4
                                                         :antithetic antithetic)
                                                     mkt 1d0))))
      (check-that "prefix: the first 1000 terminal values agree at 1000 and 5000 paths"
                  (equalp (terminals 1000 nil) (subseq (terminals 5000 nil) 0 1000)))
      (check-that "prefix under antithetics: base paths and partners agree at 1000 and 5000"
                  (let ((small (terminals 1000 t)) (large (terminals 5000 t)))
                    (and (equalp (subseq small 0 500) (subseq large 0 500))
                         (equalp (subseq small 500) (subseq large 2500 3000))))))
    ;; GBM over one step: log S_T is linear in the normal, so a path and its
    ;; partner have log-terminals symmetric about the same centre.
    (let* ((s (terminal-column (fincl::simulate-paths (mc :n-paths 2000 :n-steps 1) mkt 1d0)))
           (centres (iter (for i below 1000)
                          (collect (+ (log (aref s i)) (log (aref s (+ i 1000))))))))
      (check-that "antithetic partner i uses the negated normals of path i"
                  (< (- (reduce #'max centres) (reduce #'min centres)) 1d-12)))
    ;; 50 seeds: z = (MC - BS) / SE should be standard normal.
    (let* ((exact (price call (make-instance 'black-scholes-engine) mkt))
           (zs (iter (for seed from 1 to 50)
                     (collect (multiple-value-bind (v se) (price call (mc :n-paths 100000 :seed seed) mkt)
                                (/ (- v exact) se)))))
           (mean (/ (reduce #'+ zs) 50))
           (variance (/ (iter (for z in zs) (sum (expt (- z mean) 2))) 49)))
      (check-that (format nil "50 seeds: z-scores have mean ~,3F (within 0.4) and variance ~,3F ~
                               (in [0.6, 1.5])" mean variance)
                  (and (< (abs mean) 0.4d0) (<= 0.6d0 variance 1.5d0))))
    ;; Throughput against the per-chunk Mersenne Twister stream, stepped
    ;; paths so generation dominates.
    (let* ((process (make-instance 'heston :xi 0.5d0))
           (run (lambda (rng)
                  (seconds (lambda ()
                             (price call (mc :n-paths 100000 :n-steps 50 :process process :rng rng)
                                    mkt)))))
           (mt (progn (funcall run :mersenne-twister-64) (funcall run :mersenne-twister-64)))
           (counter (funcall run :counter)))
      (check-that (format nil "throughput: counter ~,2Fs vs Mersenne Twister ~,2Fs (ratio ~,2F >= 0.7)"
                          counter mt (/ mt counter))
                  (>= (/ mt counter) 0.7d0)))))

;;; --------------------------------------------------------------------
;;; State-aware LSM (task 08)
;;; --------------------------------------------------------------------

(defun lsm-basis-checks ()
  (format t "~&LSM bases~%")
  (check-equal "graded exponents, two regressors, degree 2"
               '((0 0) (1 0) (0 1) (2 0) (1 1) (0 2)) (fincl::basis-exponents 2 2))
  (check-equal "one regressor, degree 3: 1, x, x^2, x^3" '((0) (1) (2) (3))
               (fincl::basis-exponents 1 3))
  (check-equal "C(d+m, m) functions: 3 regressors, degree 3" 20
               (length (fincl::basis-exponents 3 3)))
  (let ((basis (make-array 4 :element-type 'double-float))
        (x 1.3d0))
    (funcall (fincl::make-basis-filler :monomial (fincl::basis-exponents 1 3)
                                       (make-array 1 :element-type 'double-float
                                                     :initial-element 2d0)
                                       3)
             basis (make-array 1 :element-type 'double-float :initial-element (* 2 x)))
    (check-that "one-regressor monomials are exactly 1, x, x*x, x*x*x (the old basis)"
                (equalp basis (vector 1d0 x (* x x) (* (* x x) x))))
    (funcall (fincl::make-basis-filler :laguerre (fincl::basis-exponents 1 3)
                                       (make-array 1 :element-type 'double-float
                                                     :initial-element 1d0)
                                       3)
             basis (make-array 1 :element-type 'double-float :initial-element x))
    (let ((w (exp (* -0.5d0 x))))
      (check-that "weighted Laguerre functions exp(-x/2) L_k(x), k = 0..3"
                  (every (lambda (expected actual) (< (abs (- expected actual)) 1d-15))
                         (list w (* w (- 1 x)) (* w (/ (+ (* x x) (* -4 x) 2) 2))
                               (* w (/ (+ (- (* x x x)) (* 9 x x) (* -18 x) 6) 6)))
                         basis)))))

(defun clarke-parrott-checks ()
  ;; K = 10, T = 0.25, r = 0.1, q = 0, kappa = 5, theta = 0.16, xi = 0.9,
  ;; rho = 0.1, v0 = 0.0625. Reference values: Ikonen and Toivanen's RK-IT
  ;; method, as tabulated in Haentjens and in 't Hout, "ADI schemes for
  ;; pricing American options under the Heston model" (arXiv:1309.0110),
  ;; Table 2. They agree with Zvan et al. and Thakoor et al. to 3e-4; Clarke
  ;; and Parrott's own 1999 values (1.1080, 0.5316, ...) are less accurate.
  (format t "~&LSM under Heston: Clarke-Parrott American put~%")
  (let ((heston (make-instance 'heston :v0 0.0625d0 :kappa 5d0 :theta 0.16d0
                                       :xi 0.9d0 :rho 0.1d0))
        ;; ACT/360 and 90 days: T = 0.25 exactly.
        (expiry (date+ *today* 90))
        (s-only-error 0d0) (s-v-error 0d0))
    (iter (for (spot reference) in '((8 2.0000d0) (9 1.1076d0) (10 0.5199d0)
                                     (11 0.2135d0) (12 0.0820d0)))
          (let ((m (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                   :spot spot :rate 0.1d0 :vol 0.25d0))
                (put (make-option (put 10) (american) (expires expiry))))
            (multiple-value-bind (v se)
                (price put (mc :n-paths 100000 :n-steps 100 :process heston) m)
              (incf s-v-error (abs (- v reference)))
              (check (format nil "S = ~D, regressors (S, v): ~,4F, reference ~,4F"
                             spot v reference)
                     reference v (+ (* 3 se) 0.01)))
            (let ((v (price put (mc :n-paths 100000 :n-steps 100 :process heston
                                    :regressors '(0))
                            m)))
              (incf s-only-error (abs (- v reference)))
              (format t "~&       S only: ~,4F (error ~,4F)~%" v (- v reference)))))
    (format t "~&       total |error|: (S, v) ~,4F, S only ~,4F~%" s-v-error s-only-error)
    (check-that "regressing on (S, v) is closer to the reference than on S alone"
                (< s-v-error s-only-error))))
