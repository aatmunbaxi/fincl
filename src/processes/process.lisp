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

(defclass stochastic-process (parameterized)
  ()
  (:documentation "Dynamics of one or more state variables. Define concrete
processes with DEFINE-PROCESS."))

(defgeneric process-factors (process)
  (:documentation "Independent standard normals consumed per time step.")
  (:method ((p stochastic-process)) 1))

(defgeneric process-state-size (process)
  (:documentation "Length of the state vector. Element 0 is the observable.")
  (:method ((p stochastic-process)) 1))

(defmacro define-process (name superclasses (&key state-size factors) slots
                          &rest class-options)
  "Define the process class NAME. SUPERCLASSES defaults to
(STOCHASTIC-PROCESS). STATE-SIZE and FACTORS, when given, define
PROCESS-STATE-SIZE and PROCESS-FACTORS (both default to 1). Each of SLOTS is

  (slot-name default &key bounds market-default (parameter t) doc reader type)

- DEFAULT is the initform, evaluated per instance. NIL, with a
  MARKET-DEFAULT, means \"take it from the market\".
- BOUNDS is (lo hi) in interval-designator form: a real is inclusive, (x)
  exclusive, NIL unbounded.
- MARKET-DEFAULT names a function (process market horizon) -> double-float.
- PARAMETER NIL declares a setting: validated, but not a model parameter
  (not in PROCESS-PARAMETERS or PARAMETER-VECTOR).
- READER defaults to SLOT-NAME, and the initarg is the keyword SLOT-NAME.
- TYPE defaults to DOUBLE-FLOAT, or (OR NULL DOUBLE-FLOAT) when DEFAULT is NIL.

Steppers, characteristic functions and samplers stay ordinary DEFMETHODs.

  (define-process heston (stochastic-process) (:state-size 2 :factors 2)
    ((v0 nil :market-default heston-default-v0 :bounds (0 nil) :doc \"Initial variance.\")
     (kappa 2d0 :bounds ((0) nil) :doc \"Mean-reversion speed.\"))
    (:documentation \"Heston stochastic volatility.\"))"
  (flet ((slot-form (spec)
           (destructuring-bind (slot default &key (bounds nil bounds-p)
                                               (market-default nil market-default-p)
                                               (parameter t) doc (reader slot) type)
               spec
             `(,slot :initarg ,(a:make-keyword slot)
                     :initform ,default
                     :reader ,reader
                     :type ,(or type (if (null default) '(or null double-float) 'double-float))
                     :parameter ,parameter
                     ,@(when bounds-p `(:bounds ,bounds))
                     ,@(when market-default-p `(:market-default ,market-default))
                     ,@(when doc `(:documentation ,doc))))))
    `(progn
       (defclass ,name ,(or superclasses '(stochastic-process))
         ,(mapcar #'slot-form slots)
         (:metaclass parameterized-class)
         ,@class-options)
       ,@(when state-size
           `((defmethod process-state-size ((p ,name)) ,state-size)))
       ,@(when factors
           `((defmethod process-factors ((p ,name)) ,factors)))
       ',name)))

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

(defgeneric characteristic-function (process market horizon)
  (:documentation "If the log-return X = ln(S(HORIZON)/S(0)) has a known
risk-neutral characteristic function, return a closure

    (lambda (u) ...) -> E[exp(i u X)]

taking a real U and returning a complex number. HORIZON is in market time.
Return NIL when no closed form exists; Fourier engines then report the
combination as unsupported.")
  (:method ((p stochastic-process) market horizon)
    (declare (ignore market horizon))
    nil))

(defgeneric log-cumulants (process market horizon)
  (:documentation "Return (values c1 c2 c4), the first, second and fourth
cumulants of ln(S(HORIZON)/S(0)), or NIL if no closed form is provided.
Fourier engines size their truncation range from these and, given NIL,
estimate them from CHARACTERISTIC-FUNCTION instead.")
  (:method ((p stochastic-process) market horizon)
    (declare (ignore market horizon))
    nil))

(defgeneric terminal-sampler (process market time)
  (:documentation "If the process has an exactly samplable terminal
distribution, return a closure

    (lambda (normals) ...) -> observable at TIME

so a European payoff can skip path generation entirely. Return NIL when no
such shortcut exists; the engine then steps the full path.")
  (:method ((p stochastic-process) market time)
    (declare (ignore market time))
    nil))
