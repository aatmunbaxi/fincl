(defclass SABR (stochastic-process)
  ((nu
    :initarg :nu
    :accessor nu
    :initform (error "A value for nu must be provided"))
   (beta
    :initarg :beta
    :accessor beta
    :initform (error "A value for beta must be provided"))
   (rho
    :initarg :rho
    :accessor rho
    :initform (error "A value for rho must be provided"))))


(defmethod gen-paths ((process SABR) init-value time-length num-paths num-steps)
  (loop :with W := (magicl:random-normal `(,num-paths ,num-steps))
        :with rho := (rho process)
        :with WW := (magicl:random-normal `(,num-paths ,num-steps))
        :with Wprime := (magicl:.+
                         (magicl:.* rho W)
                         (magicl:.* (sqrt (- 1 (expt rho 2))) WW))
        :with s0 := (magicl:const init-value `(,num-paths 1) :type 'double-float)
        :with paths := `(,s0)
        :with dt := (/ time-length num-steps)
        :with variance := (magicl:const 0.2 `(,num-paths 1))
        :for i :from 0 :below num-steps
        :finally (return (magicl:hstack (reverse paths)))
        :do
           (magicl:.+ variance
                      (magicl:.*
                       (nu process)
                       (magicl:.* (sqrt dt)
                                  (magicl:.*
                                   variance
                                   (magicl:column Wprime i)))) variance)
           (push (magicl:.+ (magicl:.* (1+ (* dt (drift process))) (car paths))
                            (magicl:.*
                             (magicl:.* variance
                              (magicl:.^ (car paths) (beta process)))
                             (magicl:.* (sqrt dt) (magicl:column W i)))) paths)))
