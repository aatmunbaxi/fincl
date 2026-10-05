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
  "sigma_ATM(T)^2 = w(T)/T, from the market's ATM-forward Black variance to
HORIZON = T."
  (declare (ignore process))
  (/ (atm-forward-variance market horizon) horizon))

(defun heston-default-theta (process market horizon)
  "V0, so that variance starts at its long-run level."
  (or (v0 process) (heston-default-v0 process market horizon)))

(define-process heston (stochastic-process) (:state-size 2 :factors 2)
  ((v0 nil :bounds (0 nil) :market-default heston-default-v0
           :doc "Initial variance. NIL resolves to sigma_ATM(T)^2 = w(T)/T, T the
expiry, w the market's ATM-forward Black variance.")
   (kappa 2d0 :bounds ((0) nil) :doc "Mean-reversion speed of the variance.")
   (theta nil :bounds (0 nil) :market-default heston-default-theta
              :doc "Long-run variance. NIL resolves to V0.")
   (xi 0.3d0 :bounds (0 nil) :doc "Vol of vol.")
   (rho -0.7d0 :bounds (-1 1) :doc "Correlation of the spot and variance shocks."))
  (:documentation "Heston stochastic volatility."))

(defun heston-v0 (process market &optional (horizon 1d0))
  "V0, or its market default at HORIZON. Engines resolve the process at the
expiry first; the one-year fallback serves direct callers such as plots."
  (or (v0 process) (heston-default-v0 process market horizon)))

(defmethod process-regressors ((p heston)) '(0 1))   ; spot and variance

(defmethod initial-state ((p heston) market)
  (let ((state (make-array 2 :element-type 'double-float)))
    (setf (aref state 0) (spot market)          ; observable
          (aref state 1) (heston-v0 p market))  ; latent variance
    state))

(defmethod make-stepper ((p heston) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((p (resolve-process p market (aref times (1- (length times)))))
         (kappa (kappa p))
         (theta (theta p))
         (xi (xi p))
         (rho (rho p))
         (rho-bar (sqrt (- 1d0 (* rho rho))))
         (n (1- (length times)))
         (dt (interval-lengths times))
         (sqrt-dt (map '(simple-array double-float (*)) #'sqrt dt))
         (mu (make-array n :element-type 'double-float)))
    (declare (double-float kappa theta xi rho rho-bar)
             (type (simple-array double-float (*)) dt sqrt-dt mu))
    (dotimes (j n)
      (setf (aref mu j) (log-growth market (aref times j) (aref times (1+ j)))))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals)
               (fixnum j)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (v (aref state 1))
             (v+ (max 0d0 v))                    ; full truncation
             (sqrt-v (sqrt v+))
             (dt (aref dt j))
             (sqrt-dt (aref sqrt-dt j))
             (z1 (aref normals 0))
             ;; Correlate the variance factor with the spot factor.
             (z2 (+ (* rho z1) (* rho-bar (aref normals 1)))))
        (declare (double-float s v v+ sqrt-v dt sqrt-dt z1 z2))
        (setf (aref state 0)
              (* s (exp (+ (- (aref mu j) (* 0.5d0 v+ dt))
                           (* sqrt-v sqrt-dt z1))))
              (aref state 1)
              (+ v
                 (* kappa (- theta v+) dt)
                 (* xi sqrt-v sqrt-dt z2)))))))

;;; --------------------------------------------------------------------
;;; Characteristic function (for Fourier engines)
;;; --------------------------------------------------------------------

(defun heston-parameters (p market horizon)
  "Return (values kappa theta xi rho v0 growth), resolved at HORIZON, with
GROWTH = log(F(HORIZON)/S0)."
  (let ((p (resolve-process p market horizon)))
    (values (kappa p) (theta p) (xi p) (rho p) (v0 p)
            (log-growth market 0d0 horizon))))

(defun heston-integrated-variance (kappa theta v0 horizon)
  "Expected integrated variance over HORIZON: the whole variance when xi = 0."
  (+ (* theta horizon)
     (/ (* (- v0 theta) (- 1d0 (exp (- (* kappa horizon))))) kappa)))

(defmethod characteristic-function ((p heston) market horizon)
  (multiple-value-bind (kappa theta xi rho v0 growth) (heston-parameters p market horizon)
    (if (zerop xi)
        ;; Deterministic variance: a Gaussian log-return.
        (let* ((w (heston-integrated-variance kappa theta v0 horizon))
               (mean (- growth (* 0.5d0 w))))
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
                   (c (+ (* growth iu)
                         (* (/ (* kappa theta) xi2)
                            (- (* (- beta d) horizon)
                               (* 2d0 (log (/ (- 1d0 (* g e)) (- 1d0 g))))))))
                   (dd (* (/ (- beta d) xi2) (/ (- 1d0 e) (- 1d0 (* g e))))))
              (exp (+ c (* dd v0)))))))))

