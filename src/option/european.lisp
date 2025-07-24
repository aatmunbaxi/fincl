(in-package #:fincl)

(defclass european-option (option) ())



(defmethod analytic-option-pricer ((op european-option)
                                   spot
                                   process))


(defmethod binomial-option-pricer ((op european-option)
                                   spot trials up down)
  (let* ((n trials)
         (is (magicl:arange (1+ trials) :type 'double-float))
         (rfr (1+ *risk-free-rate*))
         (p (/ (- rfr down) (- up down))))


    (/ (magicl:sum (magicl:map! (lambda (j)
                                  (* (alexandria:binomial-coefficient
                                      trials
                                      (coerce (round j) 'unsigned-byte))
                                     (expt p j)
                                     (expt (- 1 p) (- n j))
                                     (max
                                      0
                                      (- (* (expt up j)
                                            (expt down (- n j)) spot)
                                         (strike op)))))
                                is))
       (expt rfr n))))
