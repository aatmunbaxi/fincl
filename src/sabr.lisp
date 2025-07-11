(defclass SABR (stochastic-process)
  (
   (alpha
    :initarg :alpha
    :accessor alpha
    :initform (error "A value for alpha must be provided"))
   (beta
    :initarg :beta
    :accessor beta
    :initform (error "A value for beta must be provided"))
   (rho
    :initarg :rho
    :accessor rho
    :initform (error "A value for rho must be provided"))
   (long-run-variance
    :initarg :long-variance
    :accessor long-variance
    :initform 0.1)))

(defun quasi-normal-noise (num-rows num-cols &optional (seed t))
  (magicl:map! #'quantile
               (magicl:from-list
                (loop :repeat (* num-rows num-cols)
                      :with v := (random-state:make-generator :quasi seed)
                      :collect (coerce  (random-state:next-byte v) 'double-float))
                `(,num-rows ,num-cols)
                :type 'double-float)))

(defmethod gen-paths ((process SABR) init-value time-length num-paths num-steps)
  (loop :with W := (quasi-normal-noise num-paths num-steps)
        :with rho := (rho process)
        :with WW := (quasi-normal-noise num-paths num-steps)
        :with Wprime := (magicl:.+
                         (magicl:.* rho W)
                         (magicl:.* (sqrt (- 1 (expt rho 2))) WW))
        :with s0 := (magicl:const init-value `(,num-paths 1) :type 'double-float)
        :with steps := `(,s0)
        :with dt := (/ time-length num-steps)
        :with variance := (magicl:const (long-variance process) `(,num-paths 1))
        :for i :from 0 :upto  (- num-steps 1)
        :finally (return (magicl:hstack (reverse steps)))
        :do
           (magicl:.+ variance
                      (magicl:.*
                       (alpha process)
                       (magicl:.* (sqrt dt)
                                  (magicl:.*
                                   variance
                                   (magicl:column Wprime i)))) variance)
           (push (magicl:.+ (magicl:.* (1+ (* dt (drift process))) (car steps))
                            (magicl:.*
                             (magicl:.* variance
                                        (magicl:.^ (car steps)  (beta process)))
                             (magicl:.* (sqrt dt) (magicl:column W i)))) steps)))
