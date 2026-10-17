;;;; processes/levy.lisp --- exponential Levy processes
;;;;
;;;;   ln S(t) = ln S(0) + log(F(t)/F(0)) - w(t)/2 + W(w(t)) + L(t) + omega t
;;;;
;;;; L is a Levy process with characteristic exponent psi, E[exp(i u L(t))] =
;;;; exp(t psi(u)), and W(w(t)) an optional Brownian part with total variance
;;;; w(t). The drift omega = -psi(-i) = -log E[exp(L(1))] makes the discounted
;;;; spot a martingale, so a process only states psi (LEVY-EXPONENT) and, if it
;;;; has one, its Brownian variance (DIFFUSION-VARIANCE). The mixin derives the
;;;; characteristic function, the increment characteristic function that COS
;;;; early exercise needs, and the log-cumulants (from LEVY-CUMULANTS, when a
;;;; process gives them). Time is market time, so jump intensities and the
;;;; like are per unit of market time.

(in-package #:fincl)

(define-condition no-exponential-moment (invalid-input)
  ((process :initarg :process :reader failure-process)
   (reason :initarg :reason :reader failure-reason))
  (:report (lambda (c s)
             (format s "~A has E[exp(L(1))] infinite or undefined (~A), so its spot ~
cannot be made a martingale."
                     (failure-process c) (failure-reason c))))
  (:documentation "Parameters of an exponential Levy process whose jump part
has no finite exponential moment."))

(defclass exponential-levy (independent-increments) ()
  (:documentation "Mixin for exponential Levy processes, listed before
STOCHASTIC-PROCESS so its methods override the defaults. The process gives
LEVY-EXPONENT (and optionally DIFFUSION-VARIANCE and LEVY-CUMULANTS) and
receives CHARACTERISTIC-FUNCTION, INCREMENT-CHARACTERISTIC-FUNCTION and
LOG-CUMULANTS, with the martingale drift -psi(-i)."))

(defgeneric levy-exponent (process u)
  (:documentation "psi(U), with E[exp(i U L(1))] = exp(psi(U)), of the Levy
part of PROCESS (resolved; see RESOLVE-PROCESS), for a real or complex U.
psi(0) = 0, and psi(-i) is the log of the exponential moment."))

(defgeneric diffusion-variance (process market t0 t1)
  (:documentation "Variance over [T0, T1] of the Brownian part beside the
Levy part: 0 for pure-jump processes.")
  (:method ((p exponential-levy) market t0 t1)
    (declare (ignore market t0 t1))
    0d0))

(defgeneric levy-cumulants (process)
  (:documentation "(values k1 k2 k4), the cumulants of L(1), or NIL when the
process does not give them in closed form.")
  (:method ((p exponential-levy)) nil))

(defgeneric exponential-moment-problem (process)
  (:documentation "NIL if E[exp(L(1))] is finite for PROCESS's parameters,
else a string saying which condition fails. Checked at construction and
before use; a parameter left to the market (NIL) is checked once resolved.")
  (:method ((p exponential-levy)) nil))

(defmethod initialize-instance :after ((p exponential-levy) &key)
  ;; Look only at real parameters; anything else is reported by
  ;; PARAMETERIZED's checks.
  (let ((problem (and (every (lambda (slot)
                               (typep (slot-value p (c2mop:slot-definition-name slot))
                                      '(or null real keyword)))
                             (managed-slots (class-of p)))
                      (exponential-moment-problem p))))
    (when problem
      (error 'no-exponential-moment :process p :reason problem))))

(defun levy-drift (process)
  "omega = -psi(-i), the drift per unit time that makes exp(L(t) + omega t)
a martingale."
  (let ((problem (exponential-moment-problem process)))
    (when problem
      (error 'no-exponential-moment :process process :reason problem)))
  (- (realpart (levy-exponent process #c(0d0 -1d0)))))

(defmethod increment-characteristic-function ((p exponential-levy) market t0 t1)
  (let* ((dt (- t1 t0))
         (v (float (diffusion-variance p market t0 t1) 1d0))
         (mean (+ (log-growth market t0 t1) (* -0.5d0 v) (* (levy-drift p) dt))))
    (declare (double-float dt v mean))
    (lambda (u)
      (declare (double-float u))
      (exp (+ (complex (* -0.5d0 v u u) (* mean u))
              (* dt (levy-exponent p u)))))))

(defmethod characteristic-function ((p exponential-levy) market horizon)
  (increment-characteristic-function p market 0d0 horizon))

(defmethod log-cumulants ((p exponential-levy) market horizon)
  (multiple-value-bind (k1 k2 k4) (levy-cumulants p)
    (when k1
      (let ((v (float (diffusion-variance p market 0d0 horizon) 1d0)))
        (values (+ (log-growth market 0d0 horizon) (* -0.5d0 v)
                   (* (+ (levy-drift p) k1) horizon))
                (+ v (* k2 horizon))
                (* k4 horizon))))))

(defun levy-drifts (process market times)
  "Per-interval drift of ln S over the grid TIMES before the Levy increment
and the Brownian shock: log(F(t1)/F(t0)) - v/2 + omega dt, and the Brownian
standard deviations sqrt(v), as two typed vectors."
  (declare (type (simple-array double-float (*)) times))
  (iter (with n = (1- (length times)))
        (with omega = (levy-drift process))
        (for (t0 t1) in-intervals times)
        (for v = (float (diffusion-variance process market t0 t1) 1d0))
        (collect-double (+ (log-growth market t0 t1) (* -0.5d0 v) (* omega (- t1 t0)))
                        into drift length n)
        (collect-double (sqrt v) into diffusion length n)
        (finally (return (values drift diffusion)))))

(defgeneric log-increment-sampler (process market times)
  (:documentation "A closure (lambda (normals j)) -> ln(S(TIMES[j+1]) /
S(TIMES[j])), sampled exactly from PROCESS-FACTORS normals and free of
mutable state, or NIL when the process has no sampler. PROCESS is resolved.
MAKE-STEPPER and TERMINAL-SAMPLER are built on it.")
  (:method ((p exponential-levy) market times)
    (declare (ignore market times))
    nil))

(defmethod make-stepper ((p exponential-levy) market times)
  (let ((increment (log-increment-sampler p market times)))
    (declare (type function increment))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals) (fixnum j))
      (setf (aref state 0)
            (* (aref state 0) (exp (the double-float (funcall increment normals j)))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

(defmethod terminal-sampler ((p exponential-levy) market time)
  (let ((increment (log-increment-sampler
                    p market (make-array 2 :element-type 'double-float
                                           :initial-contents `(0d0 ,(float time 1d0)))))
        (s0 (float (spot market) 1d0)))
    (when increment
      (locally (declare (type function increment) (double-float s0))
        (lambda (normals)
          (* s0 (exp (the double-float (funcall increment normals 0)))))))))

;;; --------------------------------------------------------------------
;;; Jump diffusions: GBM plus compound Poisson jumps
;;; --------------------------------------------------------------------

(define-process jump-diffusion (exponential-levy stochastic-process) ()
  ((vol nil :reader process-vol :bounds ((0) nil) :market-default gbm-default-vol
            :doc "Volatility of the Brownian part. NIL follows the market's
ATM-forward Black variance w(t), as for GBM, so with no jumps every ATM
European is repriced. RESOLVE-PROCESS keeps it NIL."))
  (:constructor nil)
  (:documentation "Superclass of GBM plus independent compound Poisson jumps:
the Brownian part is GBM's, with the market's rate, dividend and (by
default) vol term structures."))

;;; As for GBM, a NIL vol names the market's vol term structure.
(defmethod resolve-process ((p jump-diffusion) market horizon)
  (declare (ignore market horizon))
  p)

(defmethod diffusion-variance ((p jump-diffusion) market t0 t1)
  (gbm-forward-variance p market t0 t1))
