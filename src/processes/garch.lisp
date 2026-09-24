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

(defclass garch (stochastic-process)
  ((period :initarg :period :initform (/ 1d0 252d0) :reader period :type double-float
           :documentation "Length of one model period in years.")
   (alpha :initarg :alpha :initform 0.1d0 :reader alpha :type double-float)
   (beta :initarg :beta :initform 0.8d0 :reader garch-beta :type double-float)
   (gamma :initarg :gamma :initform 0.5d0 :reader gamma :type double-float)
   (omega :initarg :omega :initform nil :reader omega
          :documentation "NIL calibrates omega so the stationary variance
equals the market's Black variance per period.")
   (h0 :initarg :h0 :initform nil :reader h0
       :documentation "Initial conditional variance; NIL uses the stationary
level.")))

(defmethod process-state-size ((p garch)) 2)

(defun garch-target-variance (process market)
  "Variance per model period implied by the market vol."
  (* (expt (black-vol market (spot market) (period process)) 2)
     (period process)))

(defun garch-omega (process market)
  (or (omega process)
      (let ((persistence (+ (garch-beta process)
                            (* (alpha process)
                               (+ 1d0 (expt (gamma process) 2))))))
        (assert (< persistence 1d0) ()
                "Non-stationary GARCH: beta + alpha(1 + gamma^2) = ~,4F >= 1."
                persistence)
        (* (garch-target-variance process market) (- 1d0 persistence)))))

(defun garch-h0 (process market)
  (or (h0 process)
      (let ((persistence (+ (garch-beta process)
                            (* (alpha process)
                               (+ 1d0 (expt (gamma process) 2))))))
        (/ (garch-omega process market) (- 1d0 persistence)))))

(defmethod initial-state ((p garch) market)
  (let ((state (make-array 2 :element-type 'double-float)))
    (setf (aref state 0) (spot market)
          (aref state 1) (garch-h0 p market))
    state))

(defmethod make-stepper ((p garch) market dt)
  (unless (< (abs (- dt (period p))) (* 1d-9 (max 1d0 (period p))))
    (error 'incompatible-time-step :process p :requested dt :required (period p)))
  (let* ((alpha (alpha p))
         (beta (garch-beta p))
         (gamma (gamma p))
         (omega (garch-omega p market))
         (mu (* (- (zero-rate market dt) (dividend-yield market dt)) dt)))
    (declare (double-float alpha beta gamma omega mu))
    (lambda (state normals)
      (declare (type (simple-array double-float (*)) state normals)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (h (aref state 1))
             (z (aref normals 0)))
        (declare (double-float s h z))
        (setf (aref state 0)
              (* s (exp (+ mu (* -0.5d0 h) (* (sqrt h) z))))
              (aref state 1)
              (+ omega (* beta h) (* alpha h (expt (- z gamma) 2))))))))
