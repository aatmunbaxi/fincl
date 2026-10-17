;;;; processes/garch.lisp --- Duan's NGARCH(1,1) option pricing model
;;;;
;;;;   log S_t = log S_{t-1} + (r - q) d - h_t/2 + sqrt(h_t) z_t
;;;;   h_{t+1}  = omega + beta h_t + alpha h_t (z_t - gamma)^2
;;;;
;;;; The interesting structural point: GARCH is a DISCRETE-time model. Its
;;;; time step is part of the model, not a numerical knob, so unlike the
;;;; diffusions it cannot be handed an arbitrary dt. MAKE-STEPPER signals
;;;; INCOMPATIBLE-TIME-STEP instead of silently returning a wrong number —
;;;; the kind of mismatch a "just pass a process" design invites, and the
;;;; reason the protocol lets a process refuse a time grid.
;;;;
;;;; gamma > 0 is the leverage parameter: negative shocks raise next period's
;;;; variance more than positive ones. alpha = beta = gamma = 0 collapses to
;;;; GBM with variance omega per period.

(in-package #:fincl)

(defun garch-target-variance (process market horizon)
  "Variance per model period implied by the market: sigma_ATM(T)^2 * period,
with sigma_ATM(T)^2 = w(T)/T from the ATM-forward Black variance to HORIZON
= T. A NIL HORIZON (an unresolved process used directly) reads one period."
  (let ((horizon (or horizon (period process))))
    (* (/ (atm-forward-variance market horizon) horizon)
       (period process))))

(defun garch-persistence (process)
  (+ (garch-beta process) (* (alpha process) (+ 1d0 (expt (gamma process) 2)))))

(defun garch-default-omega (process market horizon)
  "The omega whose stationary variance is GARCH-TARGET-VARIANCE at HORIZON."
  (let ((persistence (garch-persistence process)))
    ;; beta + alpha (1 + gamma^2)
    (unless (< persistence 1d0)
      (error 'non-stationary-process :process process :persistence persistence))
    (* (garch-target-variance process market horizon) (- 1d0 persistence))))

(defun garch-omega (process market &optional horizon)
  (or (omega process) (garch-default-omega process market horizon)))

(defun garch-default-h0 (process market horizon)
  "The stationary variance omega / (1 - persistence)."
  (/ (garch-omega process market horizon) (- 1d0 (garch-persistence process))))

(define-process garch (stochastic-process) (:state-size 2)
  ((period (/ 1d0 252d0) :parameter nil :bounds ((0) nil)
                         :doc "Length of one model period in years. Part of the
model, not a numerical setting, but not calibrated either.")
   (alpha 0.1d0 :bounds (0 nil) :doc "ARCH coefficient.")
   (beta 0.8d0 :reader garch-beta :bounds (0 nil) :doc "GARCH coefficient.")
   (gamma 0.5d0 :doc "Leverage: negative shocks raise variance more.")
   (omega nil :bounds ((0) nil) :market-default garch-default-omega
              :doc "Variance intercept. NIL resolves to the omega whose stationary
variance is sigma_ATM(T)^2 * period, T the expiry, sigma_ATM(T) the market's
ATM-forward Black vol to T.")
   (h0 nil :bounds ((0) nil) :market-default garch-default-h0
           :doc "Initial conditional variance. NIL resolves to the stationary level."))
  (:documentation "Duan's NGARCH(1,1)."))

(defun garch-h0 (process market)
  (or (h0 process) (garch-default-h0 process market nil)))

(defmethod process-regressors ((p garch)) '(0 1))    ; spot and conditional variance
(defmethod process-state-names ((p garch)) '(:spot :conditional-variance))

(defmethod initial-state ((p garch) market)
  (let ((state (make-array 2 :element-type 'double-float)))
    (setf (aref state 0) (spot market)
          (aref state 1) (garch-h0 p market))
    state))

(defmethod make-stepper ((p garch) market times)
  (declare (type (simple-array double-float (*)) times))
  (let ((dt (interval-lengths times)))
    ;; A discrete-time model: every interval must be one model period.
    (iter (for step in-vector dt)
          (unless (< (abs (- step (period p))) (* 1d-9 (max 1d0 (period p))))
            (error 'incompatible-time-step :process p :requested step
                                           :required (period p)))))
  (let* ((p (resolve-process p market (aref times (1- (length times)))))
         (alpha (alpha p))
         (beta (garch-beta p))
         (gamma (gamma p))
         (omega (omega p))
         (mu (interval-log-growths market times)))
    (declare (double-float alpha beta gamma omega)
             (type (simple-array double-float (*)) mu))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals)
               (fixnum j)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (h (aref state 1))
             (z (aref normals 0)))
        (declare (double-float s h z))
        (setf (aref state 0)
              (* s (exp (+ (aref mu j) (* -0.5d0 h) (* (sqrt h) z))))
              (aref state 1)
              (+ omega (* beta h) (* alpha h (expt (- z gamma) 2)))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))
