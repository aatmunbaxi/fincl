;;;; processes/process.lisp --- the path-generation protocol
;;;;
;;;; A process supplies dynamics. A market supplies the data to which those dynamics are
;;;; calibrated. Engines know neither. The Monte Carlo engine asks the process for a stepper
;;;; and then runs a loop.
;;;;
;;;; CONVENTION: element 0 of a state vector is the observable, the quantity on which
;;;; payoffs are evaluated. Extra elements are latent (variance, local vol level,
;;;; conditional variance). Thus the inner loop has no generic calls for each step.

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
  (:documentation "A process whose market default needs a stationary distribution that the process does not
have."))

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
;;; dynamics only through the carry factor: log(C(t1)/C(t0)) is exact for
;;; any deterministic term structure, where (r - q) dt with rates read at dt
;;; from 0 was not. Without discrete dividends C is F/S, so the discounted
;;; observable is a martingale; discrete dividends are jumps the engines
;;; apply at their ex-dates (DIVIDEND-STEPPER).

(defun log-growth (market t0 t1)
  "log(C(T1)/C(T0)), C the CARRY-FACTOR: the drift of ln S over [T0, T1]
before the variance correction, in market time, between discrete dividends.
Without them it is log(F(T1)/F(T0))."
  (log (/ (carry-factor market t1) (carry-factor market t0))))

(defun atm-forward-variance (market horizon)
  "Total Black variance to HORIZON at the ATM-forward strike, w(T); 0 at
HORIZON 0."
  (if (zerop horizon)
      0d0
      (float (black-variance market (forward market horizon) horizon) 1d0)))

(defun interval-lengths (times)
  "TIMES[j+1] - TIMES[j], as a (SIMPLE-ARRAY DOUBLE-FLOAT (*))."
  (declare (type (simple-array double-float (*)) times))
  (iter (for (t0 t1) in-intervals times)
        (collect-double (- t1 t0) into dt length (1- (length times)))
        (finally (return dt))))

(defun interval-log-growths (market times)
  "LOG-GROWTH over each interval j of the grid TIMES, as a (SIMPLE-ARRAY
DOUBLE-FLOAT (*))."
  (declare (type (simple-array double-float (*)) times))
  (iter (for (t0 t1) in-intervals times)
        (collect-double (log-growth market t0 t1) into mu length (1- (length times)))
        (finally (return mu))))

(defclass stochastic-process (parameterized)
  ()
  (:documentation "The dynamics of one or more state variables. Use DEFINE-PROCESS to define concrete
processes."))

(defgeneric process-factors (process)
  (:documentation "The number of independent standard normals that one time step consumes.")
  (:method ((p stochastic-process)) 1))

(defgeneric process-state-size (process)
  (:documentation "The length of the state vector. Element 0 is the observable.")
  (:method ((p stochastic-process)) 1))

(define-condition constructor-collision (invalid-input)
  ((name :initarg :name :reader collision-name)
   (class :initarg :class :reader collision-class))
  (:report (lambda (c s)
             (format s "Defining the process ~S would make ~S its constructor, but ~S ~
already names ~:[a function not defined by ~S~;a symbol of the COMMON-LISP package~]. ~
Rename the process, or give it (:constructor nil)."
                     (collision-class c) (collision-name c) (collision-name c)
                     (eq (symbol-package (collision-name c)) (find-package '#:common-lisp))
                     'define-process)))
  (:documentation "A process constructor MAKE-<class> would replace a function that DEFINE-PROCESS did not
define for that class."))

(defun check-constructor-name (constructor class)
  "Signal CONSTRUCTOR-COLLISION unless CONSTRUCTOR is free, or is CLASS's
own constructor from an earlier DEFINE-PROCESS."
  (when (or (eq (symbol-package constructor) (find-package '#:common-lisp))
            (and (fboundp constructor)
                 (not (eq (get constructor 'process-constructor) class))))
    (error 'constructor-collision :name constructor :class class)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *process-initargs* (make-hash-table :test 'eq)
    "Process class name -> the keyword initargs DEFINE-PROCESS gave it and its
DEFINE-PROCESS superclasses, for constructor lambda lists.")

  (defun constructor-name (class)
    (intern (format nil "MAKE-~A" (symbol-name class)) (symbol-package class))))

(defmacro define-process (name superclasses (&key state-size factors state-names) slots
                          &rest class-options)
  "Define the process class NAME and its constructor MAKE-NAME. SUPERCLASSES
defaults to (STOCHASTIC-PROCESS). STATE-SIZE and FACTORS define PROCESS-STATE-SIZE
and PROCESS-FACTORS when they are given. Both default to 1. STATE-NAMES is a list
of distinct keywords, one for each state component. It defines PROCESS-STATE-NAMES
and gives the state size by its length. If STATE-SIZE is also given, the two must
agree. Each of SLOTS is

  (slot-name default &key bounds market-default (parameter t) measure-dependent
                          doc reader type)

- DEFAULT is the initform. It is evaluated for each instance. NIL with a
  MARKET-DEFAULT means \"take it from the market\".
- BOUNDS is (lo hi) in interval-designator form. A real is inclusive. (x) is
  exclusive. NIL is unbounded.
- MARKET-DEFAULT names a function (process market horizon) that returns a
  double-float.
- PARAMETER NIL declares a setting. The macro validates it, but it is not a model
  parameter. It is not in PROCESS-PARAMETERS or PARAMETER-VECTOR.
- MEASURE-DEPENDENT T marks a parameter whose physical value can differ from its
  risk-neutral value (:PARAMETERS of CHANGE-MEASURE). The default NIL keeps the
  parameter fixed under a change of measure.
- READER defaults to SLOT-NAME. The initarg is the keyword SLOT-NAME.
- TYPE defaults to DOUBLE-FLOAT, or to (OR NULL DOUBLE-FLOAT) when DEFAULT is NIL.
  A given TYPE (a domain type such as VOLATILITY, or (OR NULL VOLATILITY)) must
  contain every value that BOUNDS allows. The macro checks this when the form is
  expanded.

MAKE-NAME takes the initargs of NAME and of its DEFINE-PROCESS superclasses as
keywords. It passes only the keywords that are given, so omitted parameters keep
their defaults. The class option (:constructor nil) omits it (abstract classes).
The macro signals CONSTRUCTOR-COLLISION at load time if MAKE-NAME is already a
function that the macro did not define for NAME. Steppers, characteristic
functions and samplers stay ordinary DEFMETHODs.

  (define-process heston (stochastic-process) (:state-names (:spot :variance) :factors 2)
    ((v0 nil :market-default heston-default-v0 :bounds (0 nil) :doc \"Initial variance.\")
     (kappa 2d0 :bounds ((0) nil) :doc \"Mean-reversion speed.\"))
    (:documentation \"Heston stochastic volatility.\"))

  (make-heston :kappa 1.5d0)"
  (when state-names
    (unless (and (listp state-names) (every #'keywordp state-names)
                 (= (length state-names) (length (remove-duplicates state-names))))
      (signal-invalid-argument 'state-names state-names 'list "a list of distinct keywords"))
    (when (and state-size (/= state-size (length state-names)))
      (signal-invalid-argument 'state-size state-size `(eql ,(length state-names))
                               (format nil "~D, the number of :STATE-NAMES ~S"
                                       (length state-names) state-names))))
  (flet ((slot-form (spec)
           (destructuring-bind (slot default &key (bounds nil bounds-p)
                                               (market-default nil market-default-p)
                                               (parameter t) measure-dependent
                                               doc (reader slot) type)
               spec
             (when (and type bounds-p)
               (check-bounds-within-type name slot bounds type))
             `(,slot :initarg ,(a:make-keyword slot)
                     :initform ,default
                     :reader ,reader
                     :type ,(or type (if (null default) '(or null double-float) 'double-float))
                     :parameter ,parameter
                     ,@(when bounds-p `(:bounds ,bounds))
                     ,@(when market-default-p `(:market-default ,market-default))
                     ,@(when measure-dependent `(:measure-dependent t))
                     ,@(when doc `(:documentation ,doc))))))
    (let* ((constructor-option (assoc :constructor class-options))
           (class-options (remove :constructor class-options :key #'first))
           (constructor (if constructor-option (second constructor-option) (constructor-name name)))
           (parameters (remove-duplicates
                        `(,@(iter (for super in superclasses)
                                  (appending (gethash super *process-initargs*)))
                          ,@(mapcar #'first slots))
                        :from-end t)))
      (setf (gethash name *process-initargs*) parameters)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *process-initargs*) ',parameters))
         (defclass ,name ,(or superclasses '(stochastic-process))
           ,(mapcar #'slot-form slots)
           (:metaclass parameterized-class)
           ,@class-options)
         ,@(when (or state-size state-names)
             `((defmethod process-state-size ((p ,name))
                 ,(or state-size (length state-names)))))
         ,@(when state-names
             `((defmethod process-state-names ((p ,name)) ',state-names)))
         ,@(when factors
             `((defmethod process-factors ((p ,name)) ,factors)))
         ,@(when constructor
             `((check-constructor-name ',constructor ',name)
               (defun ,constructor (&rest initargs &key ,@parameters)
                 ,(format nil "A new ~(~A~) process: (make-instance '~(~A~) initarg...)." name name)
                 (declare (ignore ,@parameters))
                 (apply #'make-instance ',name initargs))
               (setf (get ',constructor 'process-constructor) ',name)))
         ',name))))

(defgeneric process-regressors (process)
  (:documentation "Return the indices of the state components that an LSM regression uses to estimate
continuation values. These are the observable, and any latent state on which the
conditional distribution of the future depends. The list always starts with 0.

  (process-regressors (make-instance 'heston)) => (0 1)")
  (:method ((p stochastic-process)) '(0)))

(defgeneric initial-state (process market)
  (:documentation "Return a new state vector at t = 0.")
  (:method ((p stochastic-process) market)
    (let ((state (make-array (process-state-size p) :element-type 'double-float
                                                    :initial-element 0d0)))
      (setf (aref state 0) (spot market))
      state)))

(defgeneric resolve-process (process market horizon)
  (:documentation "Return PROCESS with every parameter that is NIL (a market default) replaced by its value
from MARKET for an instrument that expires at HORIZON (market time). The result is a new
process. If no parameter is NIL, return PROCESS itself. Engines call this function once
for each price, before any other process function. Then every step, sample and
characteristic function sees the same parameters. The slots of each process document the
defaults.

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

that advances STATE in place from TIMES[j] to TIMES[j+1]. It consumes PROCESS-FACTORS
normals. TIMES is a (SIMPLE-ARRAY DOUBLE-FLOAT (*)) of grid times in market time. It
starts at 0 and can have uneven spacing. The closure runs once for each path and each
step. Thus the method body must compute everything that it can compute early: it computes
drifts, variances and step lengths for each interval into typed vectors that J indexes.
Rates enter through LOG-GROWTH over each interval, so the discounted observable is a
martingale under any deterministic term structure. The method does not handle discrete
dividends. An :AROUND method applies them to the closure that it returns
(DIVIDEND-STEPPER). If the process is not resolved (see RESOLVE-PROCESS), the function
resolves it at the last grid time."))

;;; --------------------------------------------------------------------
;;; Discrete dividends
;;; --------------------------------------------------------------------

(defgeneric dividend-model (process)
  (:documentation "How PROCESS treats discrete cash dividends: :SPOT, the
price drops by the dividend at its ex-date (floored at 0); or :ESCROWED,
the process drives S minus the value of the dividends still to go ex before
expiry. GBM takes either as its DIVIDEND-MODEL setting; every other process
is :SPOT. Proportional dividends scale the price at their ex-date in both.")
  (:method ((p stochastic-process)) :spot))

(defun apply-dividend-jumps (s jumps)
  "S after the (kind . amount) JUMPS in order: proportional ones scale it,
cash ones subtract (floored at 0)."
  (declare (double-float s))
  (dolist (jump jumps s)
    (setf s (if (eq (car jump) :proportional)
                (* s (- 1d0 (the double-float (cdr jump))))
                (max 0d0 (- s (the double-float (cdr jump))))))))

(defun interval-dividend-jumps (events times &key (kinds '(:cash :proportional)))
  "A simple-vector over the intervals of TIMES: for interval j, the (kind .
amount) of the EVENTS of KINDS going ex in (TIMES[j], TIMES[j+1]], or NIL."
  (declare (type (simple-array double-float (*)) times))
  (let ((jumps (make-array (1- (length times)) :initial-element nil)))
    (iter (for (t0 t1) in-intervals times)
          (for j from 0)
          (setf (svref jumps j)
                (iter (for e in (dividends-between events t0 t1))
                      (when (member (dividend-event-kind e) kinds)
                        (collect (cons (dividend-event-kind e) (dividend-event-amount e)))))))
    jumps))

(defun dividend-stepper (stepper process market times)
  "STEPPER, a MAKE-STEPPER closure over TIMES, with MARKET's discrete
dividends going ex in (0, last time] applied: under the :SPOT model each
interval's dividends are subtracted at its end (they fall on grid times when
the engine put their ex-dates on the grid); under :ESCROWED the process steps
S - R(t), R(t) the cash dividends still to go ex (ESCROWED-DIVIDENDS), and
proportional ones scale it. STEPPER itself when there are none."
  (declare (type (simple-array double-float (*)) times) (function stepper))
  (let* ((horizon (aref times (1- (length times))))
         (events (dividends-between (discrete-dividends market) 0d0 horizon)))
    (cond
      ((null events) stepper)
      ((eq (dividend-model process) :escrowed)
       (let ((jumps (interval-dividend-jumps events times :kinds '(:proportional)))
             (escrow (map 'double-vector (lambda (time) (escrowed-dividends market events time horizon))
                          times)))
         (declare (simple-vector jumps) (type double-vector escrow))
         (lambda (state normals j)
           (declare (type double-vector state) (fixnum j))
           (decf (aref state 0) (aref escrow j))
           (funcall stepper state normals j)
           (a:when-let ((js (svref jumps j)))
             (setf (aref state 0) (apply-dividend-jumps (aref state 0) js)))
           (incf (aref state 0) (aref escrow (1+ j)))
           nil)))
      (t
       (let ((jumps (interval-dividend-jumps events times)))
         (declare (simple-vector jumps))
         (lambda (state normals j)
           (declare (type double-vector state) (fixnum j))
           (funcall stepper state normals j)
           (a:when-let ((js (svref jumps j)))
             (setf (aref state 0) (apply-dividend-jumps (aref state 0) js)))
           nil))))))

(defun dividend-dates (market horizon &key cum)
  "The ex-dates of MARKET's discrete dividends going ex in (0, HORIZON], for
time grids; with CUM also the day before each (if after the valuation date),
the last on which an exercise still collects the dividend."
  (let ((today (valuation-date market)))
    (iter (for e in (dividends-between (discrete-dividends market) 0d0 horizon))
          (for ex = (dividend-event-date e))
          (collect ex)
          (when (and cum (date< today (date+ ex -1)))
            (collect (date+ ex -1))))))

(defvar *apply-discrete-dividends* t
  "When NIL, MAKE-STEPPER returns the primary stepper without discrete
dividends, for a wrapper that applies them itself.")

(defmethod make-stepper :around ((p stochastic-process) market times)
  (if *apply-discrete-dividends*
      (dividend-stepper (call-next-method) p market times)
      (call-next-method)))

(defgeneric simulatable-p (process)
  (:documentation "True if PROCESS can generate paths (MAKE-STEPPER). A process that is known only through its
characteristic function (CGMY) returns NIL. The Monte Carlo engine then reports the
combination as unsupported.")
  (:method ((p stochastic-process)) t))

(defgeneric characteristic-function (process market horizon)
  (:documentation "If the log-return X = ln(S(HORIZON)/S(0)) has a known risk-neutral characteristic
function, return a closure

    (lambda (u) ...) -> E[exp(i u X)]

that takes a real U and returns a complex number. HORIZON is in market time. If no closed
form exists, return NIL. Fourier engines then report the combination as unsupported.")
  (:method ((p stochastic-process) market horizon)
    (declare (ignore market horizon))
    nil))

(defclass independent-increments () ()
  (:documentation "A mixin for processes whose log-increments over [t0, t1] do not depend on the state at
t0 (Levy processes with deterministic time dependence, GBM among them). Such processes
provide INCREMENT-CHARACTERISTIC-FUNCTION. Fourier engines can then step backward through
exercise dates."))

(defgeneric increment-characteristic-function (process market t0 t1)
  (:documentation "If PROCESS has independent increments, return a closure (lambda (u)) -> E[exp(i u X)] for
X = ln(S(T1)/S(T0)), with T0 < T1 in market time. Otherwise return NIL. For T0 = 0 it is
CHARACTERISTIC-FUNCTION at T1.")
  (:method ((p stochastic-process) market t0 t1)
    (declare (ignore market t0 t1))
    nil))

(defgeneric log-cumulants (process market horizon)
  (:documentation "Return (values c1 c2 c4), the first, second and fourth cumulants of ln(S(HORIZON)/S(0)).
Return NIL if no closed form is available. Fourier engines size their truncation range
from these values. Given NIL, they estimate the cumulants from CHARACTERISTIC-FUNCTION.")
  (:method ((p stochastic-process) market horizon)
    (declare (ignore market horizon))
    nil))

(defgeneric terminal-sampler (process market time)
  (:documentation "If the process has a terminal distribution that can be sampled exactly, return a closure

    (lambda (normals) ...) -> observable at TIME

Then a European payoff can skip path generation. Return NIL when no such shortcut exists,
or when discrete dividends go ex by TIME. The engine then steps the full path.")
  (:method ((p stochastic-process) market time)
    (declare (ignore market time))
    nil))

(defmethod terminal-sampler :around ((p stochastic-process) market time)
  (unless (dividends-between (discrete-dividends market) 0d0 time)
    (call-next-method)))

;;; --------------------------------------------------------------------
;;; Named state components and pathwise discounting
;;; --------------------------------------------------------------------

(defgeneric process-state-names (process)
  (:documentation "Return one keyword for each element of the state vector of PROCESS, in order. Element 0,
the observable, is :SPOT.

  (process-state-names (make-heston)) => (:SPOT :VARIANCE)")
  (:method ((p stochastic-process))
    (if (= 1 (process-state-size p))
        '(:spot)
        (cons :spot (iter (for i from 1 below (process-state-size p))
                          (collect (a:make-keyword (format nil "STATE-~D" i))))))))

(defgeneric spot-loadings (process)
  (:documentation "Return the loadings of the Brownian motion that drives the observable of PROCESS on its
factors. The result is a DOUBLE-VECTOR of length PROCESS-FACTORS with unit norm: W = sum_k
loading_k Z_k in the continuous-time limit. To correlate the observables of two processes
at c correlates their factors i and j at c a_i b_j (the :CORRELATION of SIMULATE). The
default puts all of it on factor 0.

  (spot-loadings (make-heston :rho -0.6d0)) => #(-0.6d0 0.8d0)")
  (:method ((p stochastic-process))
    (let ((v (make-array (process-factors p) :element-type 'double-float :initial-element 0d0)))
      (setf (aref v 0) 1d0)
      v)))

;;; --------------------------------------------------------------------
;;; Market components driven by a process
;;; --------------------------------------------------------------------

(defgeneric process-targets (process)
  (:documentation "Return the kinds of market component that PROCESS can drive in SIMULATE, from (:spot :vol
:dividend :dividend-schedule :curve). Every equity process returns (:SPOT). The first kind
is the kind that a process that is given alone drives.

  (process-targets (make-gbm)) => (:SPOT)")
  (:method ((p stochastic-process)) '(:spot)))

(defgeneric structure-at-state (process target initial market date state &key)
  (:documentation "Return the market component TARGET (for example (:spot :spx) or (:curve :usd-ois)) on
DATE, on a path where the state vector of PROCESS is STATE. INITIAL is the component in the
initial market. MARKET is the market on DATE that is built so far (aged, with the
components of the legs before this one set). The keys are the options of the leg in
SIMULATE, for example :KNOTS in ((:curve :usd-ois) (make-hull-white) :knots tenors). The
method for a spot returns element 0 of STATE.

To let a process drive another kind of component, define a method on its class. It must
also list the kind in PROCESS-TARGETS.

  (structure-at-state (make-gbm) '(:spot :spx) 100d0 m d #(104.2d0)) => 104.2d0")
  (:method ((p stochastic-process) target initial market date state &key)
    (declare (ignore initial market date))
    (if (eq (first target) :spot)
        (aref state 0)
        (signal-invalid-argument 'target target 'list
                                 (format nil "a target ~(~A~) drives: ~{~S~^, ~}"
                                         (class-name (class-of p)) (process-targets p))))))

(defgeneric state-discount-bond (process state t0 t1)
  (:documentation "Return the price at T0 of a zero-coupon bond that pays 1 at T1, on a path where the state
of PROCESS at T0 is STATE. Return NIL if PROCESS has no closed form. Times are in the
market time of the leg.

  (state-discount-bond process state 1d0 5d0)   ; a 4-year bond, 1 year in")
  (:method ((p stochastic-process) state t0 t1)
    (declare (ignore state t0 t1))
    nil))

(defgeneric steppable-p (process)
  (:documentation "True if SIMULATE can step PROCESS (it has a stepper). The
default is SIMULATABLE-P; a process the Monte Carlo engine cannot price
options under but that can be stepped (a short-rate model) returns T.")
  (:method ((p stochastic-process)) (simulatable-p p)))

(declaim (ftype (function (t t) (values (or index null) &optional)) state-index))
(defun state-index (process designator)
  "Return the state-vector index of the component that DESIGNATOR names. DESIGNATOR is a
keyword from PROCESS-STATE-NAMES, or a list of only that keyword. Resolve names once,
outside inner loops.

  (state-index (make-heston) :variance) => 1"
  (let* ((names (process-state-names process))
         (name (if (consp designator) (first designator) designator))
         (index (position name names)))
    (unless (and index (or (atom designator) (null (rest designator))))
      (signal-invalid-argument 'designator designator 'keyword
                               (format nil "a state component of ~(~A~): ~{~S~^, ~}"
                                       (class-name (class-of process)) names)))
    index))

(defgeneric bridge-variance (process market times)
  (:documentation "For continuous monitoring between grid points, return a closure (lambda (state j acc
slot)). It stores in ACC[SLOT] the variance of the log-observable over [TIMES[j],
TIMES[j+1]], given STATE at TIMES[j]. This is the variance of the Brownian bridge between
the two grid values. The default returns NIL: the log-observable is not a diffusion
between grid points (jumps, discrete-time models).")
  (:method ((p stochastic-process) market times)
    (declare (ignore market times))
    nil))

(defgeneric path-discount-kernel (process market grid)
  (:documentation "NIL for deterministic rates: engines discount with the
market's discount factors. A process with stochastic rates returns
(lambda (state j)), the discount factor from 0 to the time of grid index J
along the path whose state is STATE; engines then discount every cash flow
with it. No process implements it yet.")
  (:method ((p stochastic-process) market grid)
    (declare (ignore market grid))
    nil))
