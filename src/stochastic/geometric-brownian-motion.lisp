(in-package #:fincl)

(defclass GBM (stochastic-process)
  ((sigma
    :accessor sigma
    :initform 0
    :initarg :sigma)))



(defmethod gen-paths ((process GBM)
                      init-value
                      time-length
                      num-paths num-steps &key  (threads nil))
  (with-threaded-path-gen num-paths threads
    (let* ((noise (magicl:map! #'fincl.utils:quantile
                               (magicl:from-list
                                (loop :repeat (* x num-steps)
                                      :with v := (random-state:make-generator 'quasi)
                                      :collect (coerce  (random-state:next-byte v) 'double-float))
                                `(,x ,num-steps)
                                :type 'double-float)))
           (dt (/ time-length num-steps))
           (incrs (magicl:.+
                   (* (+ (drift process)
                         *risk-free-rate*
                         (- 0 (* 0.5 (expt (sigma process) 2))))
                      dt)
                   (magicl:scale! (magicl:.* (sqrt dt) noise) (sigma process))))
           (log-returns (cumsum incrs))
           (S (magicl:const init-value `(,x ,num-steps) :type 'double-float)))
      (magicl:hstack `(,(magicl:slice S '(0 0) `(,x 1))
                       ,(magicl:.* (magicl:.exp log-returns) S))))))
