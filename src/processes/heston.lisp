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

(defun heston-default-v0 (process market horizon)
  "The market's one-year Black variance at the spot. HORIZON is ignored:
the one-year read predates horizon-aware defaults (task 08 fixes it)."
  (declare (ignore process horizon))
  (expt (black-vol market (spot market) 1d0) 2))

(defun heston-default-theta (process market horizon)
  "V0, so that variance starts at its long-run level."
  (or (v0 process) (heston-default-v0 process market horizon)))

(define-process heston (stochastic-process) (:state-size 2 :factors 2)
  ((v0 nil :bounds (0 nil) :market-default heston-default-v0
           :doc "Initial variance. NIL uses the market vol squared.")
   (kappa 2d0 :bounds ((0) nil) :doc "Mean-reversion speed of the variance.")
   (theta nil :bounds (0 nil) :market-default heston-default-theta
              :doc "Long-run variance. NIL uses V0.")
   (xi 0.3d0 :bounds (0 nil) :doc "Vol of vol.")
   (rho -0.7d0 :bounds (-1 1) :doc "Correlation of the spot and variance shocks."))
  (:documentation "Heston stochastic volatility."))

(defun heston-v0 (process market)
  (or (v0 process) (heston-default-v0 process market 1d0)))

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

;;; --------------------------------------------------------------------
;;; Characteristic function (for Fourier engines)
;;; --------------------------------------------------------------------

(defun heston-parameters (p market horizon)
  "Return (values kappa theta xi rho v0 mu) with defaults resolved."
  (let ((v0 (heston-v0 p market)))
    (values (kappa p) (or (theta p) v0) (xi p) (rho p) v0
            (- (zero-rate market horizon) (dividend-yield market horizon)))))

(defun heston-integrated-variance (kappa theta v0 horizon)
  "Expected integrated variance over HORIZON: the whole variance when xi = 0."
  (+ (* theta horizon)
     (/ (* (- v0 theta) (- 1d0 (exp (- (* kappa horizon))))) kappa)))

(defmethod characteristic-function ((p heston) market horizon)
  (multiple-value-bind (kappa theta xi rho v0 mu) (heston-parameters p market horizon)
    (if (zerop xi)
        ;; Deterministic variance: a Gaussian log-return.
        (let* ((w (heston-integrated-variance kappa theta v0 horizon))
               (mean (- (* mu horizon) (* 0.5d0 w))))
          (lambda (u)
            (declare (double-float u))
            (exp (complex (* -0.5d0 w u u) (* mean u)))))
        ;; Albrecher et al.'s "little trap" form, which stays on the
        ;; principal branch of the logarithm for long maturities.
        (let ((xi2 (* xi xi)))
          (lambda (u)
            (declare (double-float u))
            (let* ((iu (complex 0d0 u))
                   (beta (- kappa (* rho xi iu)))
                   (d (sqrt (+ (* beta beta) (* xi2 (+ iu (* u u))))))
                   (g (/ (- beta d) (+ beta d)))
                   (e (exp (- (* d horizon))))
                   (c (+ (* mu iu horizon)
                         (* (/ (* kappa theta) xi2)
                            (- (* (- beta d) horizon)
                               (* 2d0 (log (/ (- 1d0 (* g e)) (- 1d0 g))))))))
                   (dd (* (/ (- beta d) xi2) (/ (- 1d0 e) (- 1d0 (* g e))))))
              (exp (+ c (* dd v0)))))))))

