;;;; processes/gbm.lisp --- geometric Brownian motion
;;;;
;;;;   dS = (r - q) S dt + sigma S dW
;;;;
;;;; Exactly samplable in log space, so both the stepper and the terminal
;;;; shortcut are free of discretization error.

(in-package #:fincl)

(defclass gbm (stochastic-process)
  ((vol :initarg :vol :initform nil :reader process-vol
        :documentation "Override; NIL takes the market's Black vol."))
  (:documentation "Black-Scholes dynamics."))

(defun gbm-vol (process market time)
  (or (process-vol process) (black-vol market (spot market) time)))

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
