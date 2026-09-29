;;;; market.lisp --- the market protocol

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun market-checks ()
  "Protocol functions on a flat market."
  (with-standard-fixtures
    (format t "~&Market protocol~%")
    (check "market time to 2027-09-24" 1d0 (market-time mkt #D"2027-09-24") 0)
    (check "discount factor" (exp -0.05d0) (discount-factor mkt 1d0) 1d-14)
    (check "discount factor, date" (exp -0.05d0)
           (discount-factor mkt #D"2027-09-24") 1d-14)
    (check "forward" (* 100 (exp 0.05d0)) (forward mkt 1d0) 1d-12)
    (check "forward, date" (* 100 (exp 0.05d0))
           (forward mkt #D"2027-09-24") 1d-12)
    (check "black vol" 0.2d0 (black-vol mkt 100d0 1d0) 1d-15)
    (check "black variance, date" 0.04d0
           (black-variance mkt 100d0 #D"2027-09-24") 1d-15)))
