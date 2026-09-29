;;;; errors.lisp --- conditions and restarts

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun error-checks ()
  "Unsupported combinations, refused time steps and the LSM restart."
  (with-standard-fixtures
    (format t "~&Errors and restarts~%")
    (handler-case
        (progn (price aput bs mkt2)
               (incf *failures*)
               (format t "~&  FAIL unsupported combination not signalled~%"))
      (unsupported-combination (c)
        (format t "~&  ok   unsupported: ~A~%" c)))
    ;; A discrete-time process refuses a time grid it cannot represent.
    (handler-case
        (progn (price call (mc :n-paths 1000 :n-steps 7
                               :process (make-instance 'garch))
                      mkt)
               (incf *failures*)
               (format t "~&  FAIL GARCH accepted a wrong time step~%"))
      (incompatible-time-step (c)
        (format t "~&  ok   ~A~%" c)))
    (handler-bind ((regression-failure
                     (lambda (c) (declare (ignore c))
                       (invoke-restart 'skip-exercise-date))))
      (price aput (mc :n-paths 10000) mkt2))
    (format t "~&  ok   regression-failure restart is available~%")))
