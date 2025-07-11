(defclass GBM (stochastic-process)
  ((sigma
    :accessor sigma
    :initform 0
    :initarg :sigma)))


(defun cumsum (arr &key (axis 1))
  (let ((a (magicl:column  arr 0))
        (n (cadr (magicl:shape arr))))
    (loop :with v := `(,a)
          :for i :from 1 :upto (- n 1)
          :do (push (magicl:.+ (magicl:column arr i) (car v)) v)
          :finally (return (magicl:hstack (reverse v))))))


(defun quantile (x &optional (sigma 1) (mu 0))
  (+ (* (sqrt 2) sigma (special-functions:inverse-erf (- (* 2 x) 1)) ) mu))

(defmethod gen-paths ((process GBM)
                      init-value
                      time-length
                      num-paths num-steps)
  (let* ((noise (magicl:map! #'quantile
                             (magicl:from-list
                              (loop :repeat (* num-paths num-steps)
                                    :with v := (random-state:make-generator 'quasi)
                                    :collect (coerce  (random-state:next-byte v) 'double-float))
                              `(,num-paths ,num-steps)
                              :type 'double-float)))
         (dt (/ time-length num-steps))
         (incrs (magicl:.+
                 (* dt
                    (+ (drift process) 0.039 (- 0 (* 0.5 (expt (sigma process) 2)))))
                 (magicl:.* (sigma process) (magicl:.* (sqrt dt) noise))))
         (log-returns (cumsum incrs))
         (S (magicl:const init-value `(,num-paths ,num-steps) :type 'double-float)))
    (magicl:hstack `(,(magicl:slice S '(0 0) `(,num-paths 1))
                     ,(magicl:.* (magicl:.exp log-returns) S)))))
