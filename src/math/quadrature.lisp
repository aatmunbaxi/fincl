;;;; math/quadrature.lisp --- Gauss-Legendre quadrature

(in-package #:fincl)

(defparameter *gauss-legendre-10*
  '((0.1488743389816312d0 . 0.2955242247147529d0)
    (0.4333953941292472d0 . 0.2692667193099963d0)
    (0.6794095682990244d0 . 0.2190863625159820d0)
    (0.8650633666889845d0 . 0.1494513491505806d0)
    (0.9739065285171717d0 . 0.0666713443086881d0))
  "Positive nodes and weights of the 10-point Gauss-Legendre rule on [-1, 1].")

(defun integrate-gauss-legendre (f a b &key (panels 16))
  "The integral of F over [A, B] by the 10-point Gauss-Legendre rule on each
of PANELS equal panels: exact for polynomials of degree 19 per panel, and to
rounding for smooth F. Kinks should fall on panel boundaries.

  (integrate-gauss-legendre #'exp 0d0 1d0) => 1.718281828459045d0"
  (let* ((a (float a 1d0)) (b (float b 1d0))
         (width (/ (- b a) panels))
         (sum 0d0))
    (declare (double-float a b width sum))
    (dotimes (i panels (* 0.5d0 width sum))
      (let ((mid (+ a (* (+ i 0.5d0) width))))
        (iter (for (x . w) in *gauss-legendre-10*)
              (for dx = (* 0.5d0 width x))
              (incf sum (* w (+ (funcall f (- mid dx)) (funcall f (+ mid dx))))))))))
