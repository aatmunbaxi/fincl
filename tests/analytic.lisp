;;;; analytic.lisp --- closed forms and approximations

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun analytic-checks ()
  "Black-Scholes and Barone-Adesi-Whaley against known values."
  (with-standard-fixtures
    (format t "~&Analytic~%")
    (check "BS call" 10.4506 (price call bs mkt) 1d-4)
    (check "BS put" 5.5735 (price put bs mkt) 1d-4)
    (check "put-call parity" (- 100 (* 100 (exp -0.05d0)))
           (- (price call bs mkt) (price put bs mkt)) 1d-12)
    (check "BAW American put" 4.46 (price aput baw mkt2) 0.02)))
