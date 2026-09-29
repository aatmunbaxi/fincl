;;;; processes/gbm.lisp --- geometric Brownian motion
;;;;
;;;;   dS = (r - q) S dt + sigma S dW
;;;;
;;;; Exactly samplable in log space, so both the stepper and the terminal
;;;; shortcut are free of discretization error.

(in-package #:fincl)

(defun gbm-default-vol (process market horizon)
  "The market's Black vol at the spot, to HORIZON."
  (declare (ignore process))
  (black-vol market (spot market) horizon))

(define-process gbm (stochastic-process) ()
  ((vol nil :reader process-vol :bounds ((0) nil) :market-default gbm-default-vol
            :doc "Volatility. NIL takes the market's Black vol."))
  (:documentation "Black-Scholes dynamics."))

(defun gbm-vol (process market time)
  (or (process-vol process) (gbm-default-vol process market time)))

(defmethod make-stepper ((p gbm) market dt)
  (let* ((vol (gbm-vol p market dt))
         (drift (* (- (zero-rate market dt) (dividend-yield market dt)
                      (* 0.5d0 vol vol))
                   dt))
         (diffusion (* vol (sqrt dt))))
    (declare (double-float drift diffusion))
    (lambda (state normals)
      (declare (type (simple-array double-float (*)) state normals)
               (optimize (speed 3) (safety 0)))
      (setf (aref state 0)
            (* (aref state 0) (exp (+ drift (* diffusion (aref normals 0)))))))))

(defmethod characteristic-function ((p gbm) market horizon)
  (let* ((vol (gbm-vol p market horizon))
         (variance (* vol vol horizon))
         (mean (- (* (- (zero-rate market horizon) (dividend-yield market horizon))
                     horizon)
                  (* 0.5d0 variance))))
    (declare (double-float variance mean))
    (lambda (u)
      (declare (double-float u))
      (exp (complex (* -0.5d0 variance u u) (* mean u))))))

(defmethod log-cumulants ((p gbm) market horizon)
  (let ((vol (gbm-vol p market horizon)))
    (values (* (- (zero-rate market horizon) (dividend-yield market horizon)
                  (* 0.5d0 vol vol))
               horizon)
            (* vol vol horizon)
            0d0)))

(defmethod terminal-sampler ((p gbm) market time)
  (let* ((vol (gbm-vol p market time))
         (s0 (spot market))
         (drift (* (- (zero-rate market time) (dividend-yield market time)
                      (* 0.5d0 vol vol))
                   time))
         (diffusion (* vol (sqrt time))))
    (declare (double-float s0 drift diffusion))
    (lambda (normals)
      (declare (type (simple-array double-float (*)) normals)
               (optimize (speed 3) (safety 0)))
      (* s0 (exp (+ drift (* diffusion (aref normals 0))))))))
