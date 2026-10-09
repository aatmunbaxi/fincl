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
for ~,8F. Set :n-steps to expiry/~,8F, or invoke USE-PROCESS-PERIOD."
                     (class-name (class-of (failure-process c)))
                     (required-dt c) (requested-dt c) (required-dt c)))))

(define-condition non-stationary-process (pricing-error)
  ((process :initarg :process :reader failure-process)
   (persistence :initarg :persistence :reader failure-persistence))
  (:report (lambda (c s)
             (format s "~A has persistence ~,4F >= 1, so it has no stationary variance ~
to default from; lower its persistence below 1 or give the defaulted ~
parameters explicitly."
                     (class-name (class-of (failure-process c))) (failure-persistence c))))
  (:documentation "A process whose market default needs a stationary
distribution it does not have."))

(define-condition calendar-arbitrage (pricing-error)
  ((t0 :initarg :t0 :reader arbitrage-t0)
   (t1 :initarg :t1 :reader arbitrage-t1)
   (variance :initarg :variance :reader arbitrage-variance))
  (:report (lambda (c s)
             (format s "Total Black variance falls from t = ~,6F to t = ~,6F ~
(forward variance ~,3E): the market's vol term structure admits calendar ~
arbitrage."
                     (arbitrage-t0 c) (arbitrage-t1 c) (arbitrage-variance c)))))

;;; Market quantities every process needs per interval. Rates enter the
;;; dynamics only through the forward: log(F(t1)/F(t0)) is exact for any
;;; deterministic term structure, where (r - q) dt with rates read at dt
;;; from 0 was not.

(defun log-growth (market t0 t1)
  "log(F(T1)/F(T0)): the drift of ln S over [T0, T1] before the variance
correction, in market time."
  (log (/ (forward-factor market t1) (forward-factor market t0))))

(defun atm-forward-variance (market horizon)
  "Total Black variance to HORIZON at the ATM-forward strike, w(T); 0 at
HORIZON 0."
  (if (zerop horizon)
      0d0
      (float (black-variance market (forward market horizon) horizon) 1d0)))

(defun interval-lengths (times)
  "TIMES[j+1] - TIMES[j], as a (SIMPLE-ARRAY DOUBLE-FLOAT (*))."
  (declare (type (simple-array double-float (*)) times))
  (let ((dt (make-array (1- (length times)) :element-type 'double-float)))
    (dotimes (j (length dt) dt)
      (setf (aref dt j) (- (aref times (1+ j)) (aref times j))))))

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

(defgeneric process-regressors (process)
  (:documentation "Indices of the state components an LSM regression should
use to estimate continuation values: the observable, and any latent state
that the conditional distribution of the future depends on. Always includes
0 first.

  (process-regressors (make-instance 'heston)) => (0 1)")
  (:method ((p stochastic-process)) '(0)))

(defgeneric initial-state (process market)
  (:documentation "Fresh state vector at t = 0.")
  (:method ((p stochastic-process) market)
    (let ((state (make-array (process-state-size p) :element-type 'double-float
                                                    :initial-element 0d0)))
      (setf (aref state 0) (spot market))
      state)))

(defgeneric resolve-process (process market horizon)
  (:documentation "PROCESS with every parameter left NIL (a market default)
replaced by its value from MARKET for an instrument expiring at HORIZON
(market time), as a new process; PROCESS itself if nothing is NIL. Engines
call this once per price, before any other process function, so that every
step, sample and characteristic function sees the same parameters. The
defaults are documented on each process's slots.

  (resolve-process (make-instance 'heston) market 2d0)  ; v0 = theta = w(2)/2")
  (:method ((p stochastic-process) market horizon)
    (let ((missing (remove-if-not (lambda (slot)
                                    (and (null (slot-value p (c2mop:slot-definition-name slot)))
                                         (slot-default-function slot)))
                                  (managed-slots (class-of p)))))
      (if (null missing)
          p
          (let ((initargs '()))
            (dolist (slot (c2mop:class-slots (class-of p)))
              (let ((name (c2mop:slot-definition-name slot))
                    (initarg (first (c2mop:slot-definition-initargs slot))))
                (when (and initarg (slot-boundp p name))
                  (push initarg initargs)
                  (push (if (member slot missing)
                            (funcall (slot-default-function slot) p market horizon)
                            (slot-value p name))
                        initargs))))
            (apply #'make-instance (class-of p) (nreverse initargs)))))))

(defgeneric make-stepper (process market times)
  (:documentation "Dispatch once on (process, market) and return a closure

    (lambda (state normals j) ...)

that advances STATE in place from TIMES[j] to TIMES[j+1], consuming
PROCESS-FACTORS normals. TIMES is a (SIMPLE-ARRAY DOUBLE-FLOAT (*)) of grid
times in market time, starting at 0 and possibly unevenly spaced. The
closure is called once per path per step, so everything that can be hoisted
belongs in the method body: per-interval drifts, variances and step lengths
are computed there into typed vectors indexed by J. Rates enter through
LOG-GROWTH over each interval, so the discounted observable is a martingale
under any deterministic term structure. A process that is not resolved (see
RESOLVE-PROCESS) is resolved at the last grid time."))

(defgeneric simulatable-p (process)
  (:documentation "True if PROCESS can generate paths (MAKE-STEPPER). A
process known only through its characteristic function (CGMY) returns NIL,
and the Monte Carlo engine reports the combination as unsupported.")
  (:method ((p stochastic-process)) t))

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

(defclass independent-increments () ()
  (:documentation "Mixin for processes whose log-increments over [t0, t1]
do not depend on the state at t0 (Levy processes with deterministic time
dependence, GBM among them). Such processes provide
INCREMENT-CHARACTERISTIC-FUNCTION, and Fourier engines can then step
backwards through exercise dates."))

(defgeneric increment-characteristic-function (process market t0 t1)
  (:documentation "If PROCESS has independent increments, return a closure
(lambda (u)) -> E[exp(i u X)] for X = ln(S(T1)/S(T0)), T0 < T1 in market
time; otherwise NIL. For T0 = 0 it is CHARACTERISTIC-FUNCTION at T1.")
  (:method ((p stochastic-process) market t0 t1)
    (declare (ignore market t0 t1))
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
