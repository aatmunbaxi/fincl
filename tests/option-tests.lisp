(in-package #:fincl-tests)


(defconstant +double-comparison+ (* 256 double-float-epsilon))

(fiasco:deftest bs-monte-carlo-option ()
  (let ((op (make-instance 'fincl:european-option :K 110 :time-to-expiration 1))
        (proc (make-instance 'fincl:gbm :sigma 0.2))
        (price (fincl:analytic-option-pricer op 100 proc)))
    (assert (< ( abs (- price (fincl:mc-option-pricer op 100 10000 2))) +double-comparison+)))
