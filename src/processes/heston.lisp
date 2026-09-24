;;;; processes/heston.lisp --- Heston stochastic volatility
;;;;
;;;;   dS = (r - q) S dt + sqrt(v) S dW1
;;;;   dv = kappa (theta - v) dt + xi sqrt(v) dW2,   corr(dW1, dW2) = rho
;;;;
;;;; Two state variables and two correlated factors: the first process here
;;;; that the single-factor protocol has to accommodate without special
;;;; cases. Discretized with the full-truncation Euler scheme of Lord et al.,
;;;; which is simple and well behaved when the Feller condition
;;;; (2 kappa theta > xi^2) is violated, as it usually is in practice.
;;;; Andersen's QE scheme converges faster and would be the upgrade.

(in-package #:fincl)

(defclass heston (stochastic-process)
  ((v0 :initarg :v0 :initform nil :reader v0
       :documentation "Initial variance; NIL uses the market vol squared.")
   (kappa :initarg :kappa :initform 2d0 :reader kappa :type double-float)
   (theta :initarg :theta :initform nil :reader theta
          :documentation "Long-run variance; NIL uses V0.")
   (xi :initarg :xi :initform 0.3d0 :reader xi :type double-float
       :documentation "Vol of vol.")
   (rho :initarg :rho :initform -0.7d0 :reader rho :type double-float)))

(defmethod process-factors ((p heston)) 2)
(defmethod process-state-size ((p heston)) 2)

(defun heston-v0 (process market)
  (or (v0 process) (expt (black-vol market (spot market) 1d0) 2)))

(defmethod initial-state ((p heston) market)
  (let ((state (make-array 2 :element-type 'double-float)))
    (setf (aref state 0) (spot market)          ; observable
          (aref state 1) (heston-v0 p market))  ; latent variance
    state))

(defmethod make-stepper ((p heston) market dt)
  (let* ((kappa (kappa p))
         (theta (or (theta p) (heston-v0 p market)))
         (xi (xi p))
         (rho (rho p))
         (rho-bar (sqrt (- 1d0 (* rho rho))))
         (mu (* (- (zero-rate market dt) (dividend-yield market dt)) dt))
         (sqrt-dt (sqrt dt)))
    (declare (double-float kappa theta xi rho rho-bar mu sqrt-dt))
    (lambda (state normals)
      (declare (type (simple-array double-float (*)) state normals)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (v (aref state 1))
             (v+ (max 0d0 v))                    ; full truncation
             (sqrt-v (sqrt v+))
             (z1 (aref normals 0))
             ;; Correlate the variance factor with the spot factor.
             (z2 (+ (* rho z1) (* rho-bar (aref normals 1)))))
        (declare (double-float s v v+ sqrt-v z1 z2))
        (setf (aref state 0)
              (* s (exp (+ (- mu (* 0.5d0 v+ dt))
                           (* sqrt-v sqrt-dt z1))))
              (aref state 1)
              (+ v
                 (* kappa (- theta v+) dt)
                 (* xi sqrt-v sqrt-dt z2)))))))
