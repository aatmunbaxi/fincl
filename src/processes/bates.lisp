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
  ((intensity 0.5d0 :bounds (0 nil) :doc "Jumps per unit of market time, lambda.")
   (jump-mean -0.1d0 :doc "Mean of the log jump size, mu_J.")
   (jump-vol 0.15d0 :bounds (0 nil) :doc "Standard deviation of the log jump size, sigma_J."))
  (:documentation "Bates's model: Heston stochastic volatility plus Merton
jumps."))

(defun bates-jump-exponent (p u)
  "psi_J(U) - i U lambda k: the compensated jump part's exponent per unit time."
  (let* ((mu (jump-mean p)) (s (jump-vol p)) (l (intensity p))
         (k (- (exp (+ mu (* 0.5d0 s s))) 1d0)))
    (- (* l (- (exp (- (* #c(0d0 1d0) u mu) (* 0.5d0 s s u u))) 1d0))
       (* #c(0d0 1d0) u l k))))

(defmethod characteristic-function ((p bates) market horizon)
  (let ((heston (call-next-method))
        (p (resolve-process p market horizon)))
    (declare (type function heston))
    (lambda (u)
      (declare (double-float u))
      (* (funcall heston u) (exp (* horizon (bates-jump-exponent p u)))))))

(defmethod make-stepper ((p bates) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((heston (call-next-method))
         (dt (interval-lengths times))
         (counts (map '(simple-array double-float (*)) (lambda (h) (* (intensity p) h)) dt))
         ;; -lambda k dt: the jumps' compensator.
         (compensator (map '(simple-array double-float (*))
                           (lambda (h)
                             (* (- (intensity p))
                                (- (exp (+ (jump-mean p) (* 0.5d0 (expt (jump-vol p) 2)))) 1d0)
                                h))
                           dt))
         (mu (jump-mean p))
         (sigma (jump-vol p)))
    (declare (type function heston)
             (type (simple-array double-float (*)) counts compensator)
             (double-float mu sigma))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals) (fixnum j))
      (funcall heston state normals j)
      (let ((n (poisson-from-normal (aref normals 2) (aref counts j))))
        (setf (aref state 0)
              (* (aref state 0)
                 (exp (+ (aref compensator j)
                         (if (zerop n)
                             0d0
                             (+ (* n mu) (* sigma (sqrt (float n 1d0)) (aref normals 3)))))))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

;;; Jumps: no Brownian bridge between grid points.
(defmethod bridge-variance ((p bates) market times)
  (declare (ignore market times))
  nil)
