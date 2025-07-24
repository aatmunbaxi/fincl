(in-package #:fincl.utils)

(declaim (inline quantile))
(defun quantile (x &optional (sigma 1) (mu 0))
  "Quantile function (also known as point percent function). Inverse to the cumulative
distribution function of the normal distribution."
  (declare (type number x))
  (+ (* (sqrt 2) sigma (special-functions:inverse-erf (- (* 2 x) 1))) mu))


(declaim (inline cdf))
(defun cdf (x)
  "Cumulative distribution function of the standard normal distribution."
  (/ (special-functions:erfc
      (coerce (/ (- 0 x) (sqrt 2)) 'double-float))
     2))

(random-state:define-generator halton 'double-float (random-state:stateful-generator)
    ((index 0 :type (unsigned-byte))
     (n 0)
     (d 1)
     (base 2))
  (:reseed)
  (:next
   (let ((x (- d n)))
     (if (equal x 1)
         (progn (setf n 1)
                (setf d (* d base)))
         (let ((y (floor (/ d base))))
           (loop :while (<= x y)
                 :finally (setf n (- (* y (1+ base)) x))
                 :do
                    (setf y (floor (/ y base))))))
     (/ n d))))
