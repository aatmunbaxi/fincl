;;;; processes/cgmy.lisp --- the CGMY process
;;;;
;;;; P. Carr, H. Geman, D. B. Madan and M. Yor, "The fine structure of asset
;;;; returns", J. Business 75 (2002). Levy density C e^(-G|x|)/|x|^(1+Y) for
;;;; x < 0 and C e^(-M x)/x^(1+Y) for x > 0, so
;;;;
;;;;   psi(u) = C Gamma(-Y) ((M - i u)^Y - M^Y + (G + i u)^Y - G^Y),
;;;;
;;;; with the limits Y -> 0 (variance gamma's form) and Y -> 1 taken
;;;; explicitly. M > 1 keeps E[e^L] finite. Known here only through psi:
;;;; Fourier engines price it; Monte Carlo does not (SIMULATABLE-P is NIL).

(in-package #:fincl)

(define-process cgmy (exponential-levy stochastic-process) ()
  ((c 1d0 :reader cgmy-c :bounds ((0) nil) :doc "Overall activity.")
   (g 5d0 :reader cgmy-g :bounds ((0) nil) :doc "Exponential decay of the left tail.")
   (m 5d0 :reader cgmy-m :bounds ((1) nil) :doc "Exponential decay of the right tail; above 1.")
   (y 0.5d0 :reader cgmy-y :bounds (nil (2)) :doc "Fine structure: Y < 0 finite activity, 0 <= Y < 1
infinite activity with finite variation, 1 <= Y < 2 infinite variation."))
  (:documentation "The CGMY tempered stable process."))

(defmethod simulatable-p ((p cgmy)) nil)

(defmethod levy-exponent ((p cgmy) u)
  (let ((c (cgmy-c p)) (g (cgmy-g p)) (m (cgmy-m p)) (y (cgmy-y p)) (iu (* #c(0d0 1d0) u)))
    (cond ((zerop y)
           (* (- c) (+ (log (- 1d0 (/ iu m))) (log (+ 1d0 (/ iu g))))))
          ((= y 1d0)
           (* c (+ (* (- m iu) (log (- m iu))) (- (* m (log m)))
                   (* (+ g iu) (log (+ g iu))) (- (* g (log g))))))
          (t
           (* c (float (sf:gamma (- y)) 1d0)
              (+ (expt (- m iu) y) (- (expt m y)) (expt (+ g iu) y) (- (expt g y))))))))

(defmethod levy-cumulants ((p cgmy))
  (let ((c (cgmy-c p)) (g (cgmy-g p)) (m (cgmy-m p)) (y (cgmy-y p)))
    (flet ((cumulant (n)
             ;; C Gamma(n - Y) (M^(Y-n) + (-1)^n G^(Y-n)); for n = 1 at Y = 1,
             ;; the limit C (log G - log M).
             (if (and (= n 1) (= y 1d0))
                 (* c (- (log g) (log m)))
                 (* c (float (sf:gamma (- n y)) 1d0)
                    (+ (expt m (- y n)) (* (expt -1 n) (expt g (- y n))))))))
      (values (cumulant 1) (cumulant 2) (cumulant 4)))))
