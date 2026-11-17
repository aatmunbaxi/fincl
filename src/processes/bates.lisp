;;;; processes/bates.lisp --- Bates: Heston with Merton jumps
;;;;
;;;;   dS/S = (r - q - lambda k) dt + sqrt(v) dW1 + (e^Y - 1) dN,
;;;;   v as in Heston, Y ~ N(mu_J, sigma_J^2), N Poisson(lambda)
;;;;
;;;; D. S. Bates, "Jumps and stochastic volatility: exchange rate processes
;;;; implicit in Deutsche mark options", Review of Financial Studies 9 (1996).
;;;; The jumps are independent of the diffusion, so the characteristic
;;;; function is Heston's times exp(T (psi_J(u) - i u lambda k)), and a step
;;;; is a Heston step (by its SCHEME) followed by the compensated jumps,
;;;; sampled as in MERTON-JUMP from two more normals. lambda = 0 is Heston;
;;;; xi = 0 with v0 = theta is Merton.

(in-package #:fincl)

(define-process bates (heston) (:factors 4)
  ((intensity 0.5d0 :measure-dependent t :type non-negative-double :bounds (0 nil) :doc "Jumps per unit of market time, lambda.")
   (jump-mean -0.1d0 :measure-dependent t :doc "Mean of the log jump size, mu_J.")
   (jump-vol 0.15d0 :measure-dependent t :type volatility :bounds (0 nil) :doc "Standard deviation of the log jump size, sigma_J."))
  (:documentation "Bates's model: Heston stochastic volatility plus Merton
jumps."))

(defun bates-jump-exponent-function (p)
  "A closure (lambda (u)) -> psi_J(u) - i u lambda k, a (complex double-float)
for a double-float U: the compensated jump part's exponent per unit time of the
Bates process P. The parameters and k are read once."
  (let* ((mu (jump-mean p)) (s (jump-vol p)) (l (intensity p))
         (half-s2 (* 0.5d0 s s))
         (k (- (exp (+ mu half-s2)) 1d0)))
    (declare (double-float mu s l half-s2 k))
    (lambda (u)
      (declare (double-float u))
      (- (* l (- (exp (- (* #c(0d0 1d0) u mu) (* half-s2 u u))) 1d0))
         (* #c(0d0 1d0) u l k)))))

(defmethod characteristic-function ((p bates) market horizon)
  (let* ((heston (call-next-method))
         (jump (bates-jump-exponent-function (resolve-process p market horizon)))
         (horizon (float horizon 1d0)))
    (declare (type function heston jump) (double-float horizon))
    (lambda (u)
      (declare (double-float u))
      (* (the (complex double-float) (funcall heston u))
         (exp (* horizon (the (complex double-float) (funcall jump u))))))))

(defmethod make-stepper ((p bates) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((heston (call-next-method))
         (dt (interval-lengths times))
         (l (intensity p))
         (mu (jump-mean p))
         (sigma (jump-vol p))
         (counts (map '(simple-array double-float (*)) (lambda (h) (* l h)) dt))
         ;; -lambda k dt: the jumps' compensator.
         (compensator (map '(simple-array double-float (*))
                           (lambda (h)
                             (* (- l)
                                (- (exp (+ mu (* 0.5d0 (expt sigma 2)))) 1d0)
                                h))
                           dt)))
    (declare (type function heston)
             (type (simple-array double-float (*)) counts compensator)
             (double-float l mu sigma))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals) (fixnum j))
      (funcall heston state normals j)
      (let ((n (poisson-from-normal (aref normals 2) (aref counts j))))
        (setf (aref state 0)
              (* (aref state 0)
                 (exp (+ (aref compensator j)
                         (if (zerop n)
                             0d0
                             (+ (* n mu) (* sigma (sqrt (the (double-float 0d0) (float n 1d0))) (aref normals 3)))))))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

;;; Jumps: no Brownian bridge between grid points.
(defmethod bridge-variance ((p bates) market times)
  (declare (ignore market times))
  nil)
