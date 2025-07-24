(in-package #:fincl)

(defclass SABR (stochastic-process)
  ((alpha
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
  "Generates normally distributed random variables from Halton quasi-random
sequence."
  (magicl:map! #'fincl.utils:quantile
               (magicl:from-list
                (alexandria:shuffle
                 (loop :repeat (* num-rows num-cols)
                       :with v := (random-state:make-generator 'halton seed)
                       :collect (coerce  (random-state:next-byte v) 'double-float)))
                `(,num-rows ,num-cols)
                :type 'double-float)))

(defmethod gen-paths ((process SABR) init-value time-length num-paths num-steps
                      &key (threads nil))
  (declare (type fixnum num-paths num-steps))
  (declare (type stochastic-process process))
  (with-threaded-path-gen num-paths threads
    (loop :with W := (quasi-normal-noise x num-steps)
          :with rho := (rho process)
          :with WW := (quasi-normal-noise x num-steps)
          :with Wprime := (magicl:.+
                           (magicl:.* rho W)
                           (magicl:scale! WW  (sqrt (- 1 (expt rho 2)))))
          :with s0 := (magicl:const init-value `(,x 1) :type 'double-float)
          :with steps := `(,s0)
          :with dt := (/ time-length num-steps)
          :with variance := (magicl:const (long-variance process) `(,x 1))
          :for i :from 0 :below num-steps
          :finally (return (magicl:hstack (reverse steps)))
          :do
             (magicl:.+ variance
                        (magicl:scale!
                         (magicl:scale!
                          (magicl:.*
                           variance
                           (magicl:column Wprime i))
                          (sqrt dt))
                         (alpha process)) variance)
             (push (magicl:.+ (magicl:.* (car steps) (1+ (* dt  (drift process))))
                              (magicl:.*
                               (magicl:.* variance
                                          (magicl:.^ (car steps)  (beta process)))
                               (magicl:scale! (magicl:column W i) (sqrt dt)))) steps))))
