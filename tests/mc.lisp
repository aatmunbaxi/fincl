;;;; mc.lisp --- the Monte Carlo engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun mc-checks ()
  "Monte Carlo under GBM against Black-Scholes and the LSM benchmark."
  (with-standard-fixtures
    (format t "~&Monte Carlo, GBM (benchmarks 10.4506 / 4.478)~%")
    (check "European, terminal sampler" 10.4506
           (price call (mc :n-paths 400000) mkt) 0.03)
    (check "European, PCG stream" 10.4506
           (price call (mc :n-paths 400000 :rng :pcg) mkt) 0.03)
    (check "European, inverse transform" 10.4506
           (price call (mc :n-paths 400000 :sampler :inverse-transform) mkt)
           0.03)
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
