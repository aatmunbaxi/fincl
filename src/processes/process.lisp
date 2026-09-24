;;;; processes/process.lisp --- the path-generation protocol
;;;;
;;;; A process supplies dynamics; a market supplies the data those dynamics
;;;; are calibrated to. Engines know neither: the Monte Carlo engine asks the
;;;; process for a stepper and then runs a loop.
;;;;
;;;; CONVENTION: element 0 of a state vector is the observable — the quantity
;;;; payoffs are evaluated on. Extra elements are latent (variance, local
;;;; vol level, conditional variance). This keeps the inner loop free of
;;;; per-step generic calls.

(in-package #:fincl)

(define-condition incompatible-time-step (pricing-error)
  ((process :initarg :process :reader failure-process)
   (requested :initarg :requested :reader requested-dt)
   (required :initarg :required :reader required-dt))
  (:report (lambda (c s)
             (format s "~A requires a time step of ~,8F, but the engine asked ~
for ~,8F. Set :n-steps to expiry/~,8F."
                     (class-name (class-of (failure-process c)))
                     (required-dt c) (requested-dt c) (required-dt c)))))

(defclass stochastic-process ()
  ()
  (:documentation "Dynamics of one or more state variables."))

(defgeneric process-factors (process)
  (:documentation "Independent standard normals consumed per time step.")
  (:method ((p stochastic-process)) 1))

(defgeneric process-state-size (process)
  (:documentation "Length of the state vector. Element 0 is the observable.")
  (:method ((p stochastic-process)) 1))

(defgeneric initial-state (process market)
  (:documentation "Fresh state vector at t = 0.")
  (:method ((p stochastic-process) market)
    (let ((state (make-array (process-state-size p) :element-type 'double-float
                                                    :initial-element 0d0)))
      (setf (aref state 0) (spot market))
      state)))

(defgeneric make-stepper (process market dt)
  (:documentation "Dispatch once on (process, market) and return a closure

    (lambda (state normals) ...)

that advances STATE in place by DT, consuming PROCESS-FACTORS normals. The
closure is called once per path per step, so everything that can be hoisted
— drift, discretization constants, market lookups — belongs in the method
body, not the closure body."))

(defgeneric terminal-sampler (process market time)
  (:documentation "If the process has an exactly samplable terminal
distribution, return a closure

    (lambda (normals) ...) -> observable at TIME

so a European payoff can skip path generation entirely. Return NIL when no
such shortcut exists; the engine then steps the full path.")
  (:method ((p stochastic-process) market time)
    (declare (ignore market time))
    nil))
