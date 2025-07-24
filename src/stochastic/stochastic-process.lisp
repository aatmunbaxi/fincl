(in-package #:fincl)

(defclass stochastic-process ()
  ((drift
    :accessor drift
    :initarg :drift
    :initform 0)) )


(defgeneric gen-paths (process
                       init-value
                       time-length
                       num-paths
                       num-steps &key threads)
  (:documentation "Generate paths with PROCESS using Euler-Maruyama discretization scheme with optional drift."))



(defmacro with-threaded-path-gen (num-paths threads generator-body)
  `(flet ((generator (x) ,generator-body))
     (if threads
         (let* ((num-per (floor (/ ,num-paths ,threads)))
                (remaining (mod ,num-paths ,threads))
                (paths (loop :with paths := nil
                             :repeat (if (equal remaining 0) ,threads (1- ,threads))
                             :finally (return (if (equal remaining 0) paths  (nconc `(,remaining) paths)))
                             :do (setf paths (nconc `(,num-per) paths)))))
           (progn
             (setf lparallel:*kernel* (lparallel:make-kernel ,threads))
             (prog1
                 (magicl:vstack (lparallel:pmapcar #'generator paths))
               (lparallel:end-kernel :wait nil))))
         (generator ,num-paths))))
