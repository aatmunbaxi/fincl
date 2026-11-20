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

(define-process garch (stochastic-process) (:state-names (:spot :conditional-variance))
  ((period (/ 1d0 252d0) :parameter nil :type positive-double :bounds ((0) nil)
                         :doc "Length of one model period in years. Part of the
model, not a numerical setting, but not calibrated either.")
   (alpha 0.1d0 :type non-negative-double :bounds (0 nil) :doc "ARCH coefficient.")
   (beta 0.8d0 :reader garch-beta :type non-negative-double :bounds (0 nil) :doc "GARCH coefficient.")
   (gamma 0.5d0 :measure-dependent t :doc "Leverage: negative shocks raise variance more.")
   (omega nil :type (or null positive-double) :bounds ((0) nil) :market-default garch-default-omega
              :doc "Variance intercept. NIL resolves to the omega whose stationary
variance is sigma_ATM(T)^2 * period, T the expiry, sigma_ATM(T) the market's
ATM-forward Black vol to T.")
   (h0 nil :type (or null non-negative-double) :bounds ((0) nil) :market-default garch-default-h0
           :doc "Initial conditional variance. NIL resolves to the stationary level."))
  (:documentation "Duan's NGARCH(1,1)."))

(defun garch-h0 (process market)
  (or (h0 process) (garch-default-h0 process market nil)))

(defmethod process-regressors ((p garch)) '(0 1))    ; spot and conditional variance

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
              (* s (exp (+ (aref mu j) (* -0.5d0 h) (* (sqrt (the (double-float 0d0) h)) z))))
              (aref state 1)
              (+ omega (* beta h) (* alpha h (expt (- z gamma) 2)))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

;;; --------------------------------------------------------------------
;;; Change of measure
;;; --------------------------------------------------------------------
;;;
;;; Duan's model (Mathematical Finance 5, 1995) under P:
;;;   log S_t = log S_{t-1} + (r - q) d - h_t/2 + lambda sqrt(h_t) + sqrt(h_t) e_t
;;;   h_{t+1} = omega + beta h_t + alpha h_t (e_t - (gamma - lambda))^2,
;;; e_t standard normal under P, the risk-neutral process being this one with
;;; z_t = e_t + lambda. The physical stepper is the risk-neutral stepper with
;;; gamma - lambda fed the P normal e_t, then lambda sqrt(h_t) added, exactly.

(defmethod state-coordinate ((p garch) name)
  (when (eq name :spot) :log))

(defmethod state-diffusion ((p garch) market name times)
  (declare (ignore market name times))
  (let ((period (period p)))
    (values (lambda (j time state)
              (declare (ignore j time) (type double-vector state))
              (sqrt (/ (max 0d0 (aref state 1)) period)))
            nil)))

(defmethod premium-form-problem ((p garch) name kind spec)
  (declare (ignore name))
  (unless (and (eq kind :price-of-risk) (realp spec))
    "on :spot a real price of risk lambda per period (Duan's model); or a physical :gamma through :parameters"))

(defmethod measure-change-dynamics ((q garch) premia market horizon)
  (declare (ignore market horizon))
  (multiple-value-bind (kind lambda*) (premium-on premia :spot)
    (declare (ignore kind))
    (let ((given (getf premia :parameters)))
      (if (null lambda*)
          (call-next-method)
          (progn
            (when (member 'gamma given)
              (refuse-measure-change q "a price of risk on :spot and a physical :gamma together: the price ~
of risk sets gamma^P = gamma - lambda"))
            (values (process-with-parameters q `(gamma ,(- (gamma q) lambda*) ,@given))
                    `(:increment ,(lambda (j state)
                                    (declare (ignore j) (type double-vector state))
                                    (* lambda* (sqrt (max 0d0 (aref state 1))))))))))))
