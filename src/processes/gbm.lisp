;;;; processes/gbm.lisp --- geometric Brownian motion
;;;;
;;;;   dS = (r - q) S dt + sigma S dW
;;;;
;;;; Exactly samplable in log space, so both the stepper and the terminal
;;;; shortcut are free of discretization error.

(in-package #:fincl)

(defun gbm-default-vol (process market horizon)
  "sqrt(w(T)/T), w the market's ATM-forward Black variance to HORIZON = T:
the constant vol with the market's total variance to T. Used where one
number is needed (PARAMETER-VECTOR, a CRR tree's step size); a GBM whose vol
is NIL follows the full term structure w(t)."
  (declare (ignore process))
  (sqrt (/ (atm-forward-variance market horizon) horizon)))

(define-process gbm (stochastic-process independent-increments) ()
  ((vol nil :reader process-vol :bounds ((0) nil) :market-default gbm-default-vol
            :doc "Constant volatility. NIL follows the market's ATM-forward Black
variance w(t): the variance of ln S over [t0, t1] is w(t1) - w(t0), so every
ATM European is repriced, whatever the vol term structure. RESOLVE-PROCESS
keeps it NIL."))
  (:documentation "Black-Scholes dynamics, with the market's rate, dividend
and (by default) vol term structures."))

;;; NIL vol is not "unresolved": it names the term-structure model, which
;;; one constant cannot represent.
(defmethod resolve-process ((p gbm) market horizon)
  (declare (ignore market horizon))
  p)

(defun gbm-variance (process market horizon)
  "Total variance of ln S over [0, HORIZON]."
  (let ((vol (process-vol process)))
    (if vol (* vol vol horizon) (atm-forward-variance market horizon))))

(defun gbm-forward-variance (process market t0 t1)
  "Variance of ln S over [T0, T1]. Signals CALENDAR-ARBITRAGE if the
market's total variance falls."
  (let ((vol (process-vol process)))
    (if vol
        (* vol vol (- t1 t0))
        (let ((v (- (atm-forward-variance market t1) (atm-forward-variance market t0))))
          (when (minusp v)
            (error 'calendar-arbitrage :t0 t0 :t1 t1 :variance v))
          v))))

(defun gbm-vol (process market time)
  "The constant vol with GBM's total variance to TIME."
  (or (process-vol process) (sqrt (/ (gbm-variance process market time) time))))

(defmethod make-stepper ((p gbm) market times)
  (declare (type (simple-array double-float (*)) times))
  (multiple-value-bind (drift diffusion)
      (iter (with n = (1- (length times)))
            (for (t0 t1) in-intervals times)
            (for variance = (gbm-forward-variance p market t0 t1))
            (collect-double (- (log-growth market t0 t1) (* 0.5d0 variance)) into drift length n)
            (collect-double (sqrt variance) into diffusion length n)
            (finally (return (values drift diffusion))))
    (declare (type (simple-array double-float (*)) drift diffusion))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals)
               (fixnum j)
               (optimize (speed 3) (safety 0)))
      (setf (aref state 0)
            (* (aref state 0) (exp (+ (aref drift j)
                                      (* (aref diffusion j) (aref normals 0)))))))))

(defmethod increment-characteristic-function ((p gbm) market t0 t1)
  (let* ((variance (gbm-forward-variance p market t0 t1))
         (mean (- (log-growth market t0 t1) (* 0.5d0 variance))))
    (declare (double-float variance mean))
    (lambda (u)
      (declare (double-float u))
      (exp (complex (* -0.5d0 variance u u) (* mean u))))))

(defmethod characteristic-function ((p gbm) market horizon)
  (increment-characteristic-function p market 0d0 horizon))

(defmethod log-cumulants ((p gbm) market horizon)
  (let ((variance (gbm-variance p market horizon)))
    (values (- (log-growth market 0d0 horizon) (* 0.5d0 variance))
            variance
            0d0)))

(defmethod terminal-sampler ((p gbm) market time)
  (let* ((variance (gbm-variance p market time))
         (s0 (spot market))
         (drift (- (log-growth market 0d0 time) (* 0.5d0 variance)))
         (diffusion (sqrt variance)))
    (declare (double-float s0 drift diffusion))
    (lambda (normals)
      (declare (type (simple-array double-float (*)) normals)
               (optimize (speed 3) (safety 0)))
      (* s0 (exp (+ drift (* diffusion (aref normals 0))))))))
