;;;; engines/engine.lisp --- engine classes and the pricing protocol

(in-package #:fincl)

(define-condition unsupported-combination (pricing-error)
  ((exercise :initarg :exercise :reader error-exercise)
   (payoff :initarg :payoff :reader error-payoff)
   (path :initarg :path :reader error-path)
   (engine :initarg :engine :reader error-engine)
   (process :initarg :process :initform nil :reader error-process)
   (market :initarg :market :reader error-market))
  (:report
   (lambda (c s)
     (flet ((name (x) (class-name (class-of x))))
       (format s "~A has no method for ~A / ~A / ~A~@[ under ~A~] in a ~A. ~
Use an engine that has one, e.g. through USE-ENGINE."
               (name (error-engine c)) (name (error-exercise c))
               (name (error-payoff c)) (name (error-path c))
               (and (error-process c) (name (error-process c)))
               (name (error-market c)))))))

(define-condition model-domain-error (pricing-error)
  ((engine :initarg :engine :reader error-engine)
   (reason :initarg :reason :reader error-reason))
  (:report
   (lambda (c s)
     (format s "~A does not apply here: ~A"
             (class-name (class-of (error-engine c))) (error-reason c))))
  (:documentation "The engine has a method for the instrument, but its approximation does not hold in
this market regime."))

(define-condition option-expired (pricing-error)
  ((option :initarg :option :reader error-option)
   (valuation-date :initarg :valuation-date :reader error-valuation-date))
  (:report
   (lambda (c s)
     (format s "Option expired on ~A, before the valuation date ~A."
             (expiry (option-exercise (error-option c)))
             (error-valuation-date c)))))

;;; --------------------------------------------------------------------
;;; Engines
;;; --------------------------------------------------------------------

(defclass engine-class (standard-class)
  ((designator :initarg :designator :initform nil
               :documentation "The keyword naming the class in MAKE-ENGINE, as a
one-element list (class option syntax).")
   (models :initarg :models :initform nil :reader engine-class-models
           :documentation "Process classes the engine models."))
  (:documentation "Metaclass of engines, accepting the class options
(:designator keyword) and (:models process-class ...). A subclass defined
without it is an ordinary engine with no designator of its own."))

(defmethod c2mop:validate-superclass ((c engine-class) (s standard-class)) t)
(defmethod c2mop:validate-superclass ((c standard-class) (s engine-class)) t)

(defvar *engine-designators* (make-hash-table :test 'eq)
  "Engine designator keyword -> engine class name.")

(defun engine-class-designator (class)
  (and (typep class 'engine-class) (first (slot-value class 'designator))))

(defmethod shared-initialize :after ((c engine-class) slot-names &key)
  (declare (ignore slot-names))
  (let ((key (engine-class-designator c)))
    (when key
      (let ((owner (gethash key *engine-designators*)))
        (when (and owner (not (eq owner (class-name c))))
          (signal-invalid-argument 'designator key 'keyword
                                   (format nil "a designator not already taken (~S names ~S)"
                                           key owner))))
      (setf (gethash key *engine-designators*) (class-name c)))))

(defclass engine () ()
  (:metaclass engine-class))

(defclass analytic-engine (engine) ()
  (:metaclass engine-class)
  (:documentation "Closed forms. The class of an analytic engine fixes its model. It has no process
slot, because a closed form exists only for the dynamics that it was derived under.
To add a closed form for Heston, add an engine class and a method, not a parameter."))

(defclass black-scholes-engine (analytic-engine)
  ((dividend-model :initarg :dividend-model :initform :spot :reader dividend-model
                   :type (member :spot :escrowed)
                   :documentation "The model of discrete cash dividends (see GBM's
DIVIDEND-MODEL): :ESCROWED is Black's formula on the dividend-adjusted
forward, exact; :SPOT is Bos and Vandermark's approximation. Set from the
process when automatic selection makes the engine for a GBM."))
  (:metaclass engine-class)
  (:designator :black-scholes)
  (:models gbm)
  (:documentation "Black's formula for European vanillas."))

(defclass barone-adesi-whaley-engine (analytic-engine) ()
  (:metaclass engine-class)
  (:designator :baw)
  (:models gbm)
  (:documentation "The quadratic approximation for American vanillas."))

(defclass roll-geske-whaley-engine (analytic-engine)
  ((dividend-model :initarg :dividend-model :initform :escrowed :reader dividend-model
                   :type (member :spot :escrowed)
                   :documentation "Must be :ESCROWED, the model the formula is
derived under; automatic selection sets it from the process, and :SPOT makes
the engine decline."))
  (:metaclass engine-class)
  (:designator :rgw)
  (:models gbm)
  (:documentation "The closed form of Roll, Geske and Whaley for an American call with one discrete cash
dividend, under the escrowed model and with no continuous dividend yield.

  (make-engine :rgw)"))

(defclass merton-series-engine (analytic-engine)
  ((process :initarg :process :initform (make-instance 'merton-jump) :reader process
            :type merton-jump
            :documentation "The Merton jump diffusion whose parameters the
series uses."))
  (:metaclass engine-class)
  (:designator :merton-series)
  (:models merton-jump)
  (:documentation "The series of Merton (1976) for European vanillas under a jump diffusion. It sums
Black prices for each number of jumps, weighted by the Poisson probabilities.

  (make-instance 'merton-series-engine :process (make-instance 'merton-jump :intensity 1))"))

(defclass cev-engine (analytic-engine)
  ((process :initarg :process :initform (make-instance 'cev :beta 0.5d0) :reader process
            :type cev
            :documentation "The CEV process, with beta <= 1."))
  (:metaclass engine-class)
  (:designator :cev)
  (:models cev)
  (:documentation "The noncentral chi-square formula of Schroder (1989) for European vanillas under
CEV, with absorption at zero and beta <= 1.

  (make-instance 'cev-engine :process (make-instance 'cev :beta 0.5d0))"))

(defclass monte-carlo-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "Path-generation backend. The engine is model
agnostic: everything model-specific comes from the process.")
   (n-paths :initarg :n-paths :initform 100000 :reader n-paths :type (integer 1))
   (n-steps :initarg :n-steps :initform 50 :reader n-steps :type (integer 1)
            :documentation "Time steps; also the exercise dates for LSM.")
   (seed :initarg :seed :initform 42 :reader seed)
   (rng :initarg :rng :initform :counter :reader rng
        :documentation ":COUNTER (the default): normal k of path i is a pure
function of (seed, i, k), from the Philox4x32-10 counter-based generator, so
prices do not depend on N-CHUNKS or the worker count, and path i is the same
whatever N-PATHS is. :SOBOL: randomized quasi-Monte Carlo, 16 scrambled and
shifted Sobol' sequences (Joe-Kuo directions, at most 1111 steps x factors)
through a Brownian bridge, normals by inverse transform whatever SAMPLER
says; the standard error is that of the 16 randomization means, and results
do not depend on N-CHUNKS or the worker count. Otherwise one stream per chunk: a random-state generator
designator (a keyword such as :MERSENNE-TWISTER-64, or (keyword
seed-offset)), or :NATIVE for the host RNG; prices then depend on N-CHUNKS.")
   (sampler :initarg :sampler :initform :box-muller :reader sampler
            :documentation ":box-muller or :inverse-transform.")
   (antithetic :initarg :antithetic :initform t :reader antithetic)
   (n-chunks :initarg :n-chunks :initform 64 :reader n-chunks
             :documentation "Work units, fixed independently of the thread
count so results do not depend on how many workers are running. Under the
:COUNTER rng they do not depend on N-CHUNKS either.")
   (basis-degree :initarg :basis-degree :initform 3 :reader basis-degree
                 :documentation "Total degree of the LSM regression basis.")
   (basis :initarg :basis :initform :monomial :reader basis
          :type (member :monomial :laguerre)
          :documentation "LSM basis functions of the regressors x_i: :MONOMIAL,
all monomials of total degree <= BASIS-DEGREE; or :LAGUERRE, the weighted
Laguerre functions exp(-x/2) L_a(x) of the scaled spot times monomials in the
other regressors, a + b <= BASIS-DEGREE. The spot is scaled by the payoff's
REGRESSION-SCALE (the strike), other components by their value at t = 0.")
   (control-variates :initarg :control-variates :initform nil :reader control-variates
                     :documentation "Variates simulated on the same paths, whose
known prices correct the estimate (regression coefficients fitted over all
paths): :EUROPEAN (the option's payoff at expiry, priced exactly for the
process), :GEOMETRIC-ASIAN (for an Asian, the payoff of the geometric
average), or (payoff path-feature pricer), PRICER an engine designator or a
function of (option market). European exercise only.")
   (barrier-bridge :initarg :barrier-bridge :initform t :reader barrier-bridge
                   :documentation "If true, a continuously monitored barrier is
crossed between grid points with the Brownian-bridge probability, which
needs a diffusion (BRIDGE-VARIANCE: GBM, CEV log-Euler, Heston). NIL checks
the barrier at grid points only, which overprices knock-outs; any process.")
   (lsm-valuation :initarg :lsm-valuation :initform :in-sample :reader lsm-valuation
                  :type (member :in-sample :independent)
                  :documentation "LSM's price: :IN-SAMPLE, on the paths the exercise rule
was fitted on, or :INDEPENDENT, the fitted rule applied to a fresh path set
(a different seed), which is biased low.")
   (regressors :initarg :regressors :initform nil :reader regressors
               :documentation "State components the LSM regression uses, as
indices into the state vector, starting with 0. NIL uses the process's
PROCESS-REGRESSORS: spot and variance for Heston and GARCH, spot otherwise."))
  (:metaclass engine-class)
  (:designator :mc)
  (:models stochastic-process))

(defclass binomial-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "The lattice's dynamics. Cox-Ross-Rubinstein trees
model GBM only.")
   (n-steps :initarg :n-steps :initform 2000 :reader n-steps :type (integer 1)
            :documentation "Time steps in the tree.")
   (smooth :initarg :smooth :initform t :reader smooth
           :documentation "If true, value the last step with the
Black-Scholes formula (Broadie and Detemple's BBS tree), which removes the
odd-even oscillation of the plain CRR tree.")
   (extrapolate :initarg :extrapolate :initform t :reader extrapolate
                :documentation "If true, return 2 v(N) - v(N/2), which
cancels the O(1/N) error of the tree. Reliable only with SMOOTH."))
  (:metaclass engine-class)
  (:designator :binomial)
  (:models gbm)
  (:documentation "The Cox-Ross-Rubinstein binomial tree.

  (make-instance 'binomial-engine :n-steps 5000)"))

(defclass cos-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "Supplies the characteristic function and
cumulants of the log-return.")
   (n-terms :initarg :n-terms :initform 256 :reader n-terms :type (integer 1)
            :documentation "Terms in the Fourier-cosine expansion.")
   (truncation :initarg :truncation :initform 12d0 :reader truncation :type (real (0))
               :documentation "L in the truncation range
[c1 - L sqrt(c2 + sqrt(c4)), c1 + L sqrt(c2 + sqrt(c4))].")
   (richardson-dates :initarg :richardson-dates :initform 16
                     :reader richardson-dates :type (integer 1)
                     :documentation "Exercise dates M of the coarsest
Bermudan in the American extrapolation, which uses M, 2M, 4M and 8M."))
  (:metaclass engine-class)
  (:designator :cos)
  (:models stochastic-process)
  (:documentation "The Fourier-cosine (COS) method of Fang and Oosterlee. A European option needs only
a characteristic function. A Bermudan or American option uses the backward recursion
of their 2009 paper. It needs a process whose log-increments are independent of the
state (the INDEPENDENT-INCREMENTS mixin, for example GBM).

  (make-instance 'cos-engine :process (make-instance 'heston) :n-terms 160)"))

(defclass fd-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "The dynamics: GBM, CEV and local vol on a log-spot
grid (PDE-COEFFICIENTS), Heston on a (log-spot, variance) grid by ADI.")
   (n-space :initarg :n-space :initform 200 :reader n-space :type (integer 1)
            :documentation "Log-spot grid points.")
   (n-variance :initarg :n-variance :initform 50 :reader n-variance :type (integer 1)
               :documentation "Variance grid points (Heston).")
   (n-time :initarg :n-time :initform 200 :reader n-time :type (integer 1)
           :documentation "Time steps to expiry; exercise and monitoring dates are
grid times, with gaps filled evenly.")
   (theta :initarg :theta :initform 1/2 :reader fd-theta :type (real 0 1)
          :documentation "The theta-scheme weight: 1/2 Crank-Nicolson, 1 implicit.
Heston's ADI uses Hundsdorfer-Verwer's 1/2 + sqrt(3)/6 instead.")
   (rannacher-steps :initarg :rannacher-steps :initform 2 :reader rannacher-steps
                    :type (integer 0)
                    :documentation "Fully implicit substeps replacing the step after
each payoff discontinuity (expiry, Bermudan exercise, barrier monitoring).
0 disables the smoothing.")
   (concentration :initarg :concentration :initform 2 :reader concentration
                  :documentation "Width of the sinh concentration around the strike,
in standard deviations of ln S_T; NIL for a uniform grid.")
   (width :initarg :width :initform 5 :reader grid-width :type (real (0))
          :documentation "Half-width of the log-spot grid, in standard deviations of
ln S_T around the spot."))
  (:metaclass engine-class)
  (:designator :fd)
  (:models stochastic-process)
  (:documentation "Finite differences in log-spot. It uses a theta-scheme with Rannacher start-up on a
sinh-concentrated grid, Ikonen-Toivanen splitting for American exercise, and
continuous barriers as boundary conditions. Heston uses Hundsdorfer-Verwer ADI.

  (make-engine :fd :process (make-local-vol) :n-space 400 :n-time 400)"))

;;; --------------------------------------------------------------------
;;; Protocol
;;; --------------------------------------------------------------------

(defgeneric price (instrument engine market)
  ;; The market first: see src/pricing.lisp.
  (:argument-precedence-order market instrument engine)
  (:documentation "Return (values npv standard-error engine). The second value is NIL for
deterministic engines. The third value is the engine that priced the instrument.
ENGINE can be an engine designator (src/engines/selection.lisp): a keyword such as
:binomial, a list (:cos :n-terms 512) as data, a process (priced with an engine that
automatic selection chooses), or :auto for GBM.

  (price opt (make-engine :binomial :n-steps 4000) m)
  (price opt (make-heston :xi 0.5d0) m)"))

(defgeneric engine-candidates (designator exercise payoff path process)
  (:method-combination append)
  (:documentation "Return ((priority designator) ...), a list that shows how automatic selection can
use the engine DESIGNATOR (a keyword) for EXERCISE x PAYOFF x PATH under PROCESS.
Selection tries the candidates in order of increasing priority across engines: 10
for a closed form, 20 for a Fourier or lattice method, 30 for an approximation, 90
or more for a slow fallback. Each engine file adds APPEND methods on (eql
its-designator) next to its pricing methods. The designator stops two engines with
the same specializers from replacing the method of each other.")
  (:method append (designator exercise payoff path process)
    (declare (ignore designator exercise payoff path process))
    '()))

(defgeneric validate-pricing-inputs (instrument engine market)
  (:documentation "Signal an error if ENGINE cannot price INSTRUMENT in MARKET. PRICE calls it once,
before it dispatches to the engine. The default method accepts everything.")
  (:method (instrument engine market)
    (declare (ignore instrument engine market))
    nil))

(defmethod validate-pricing-inputs ((o option) engine market)
  (validate-payoff (option-payoff o) market (expiry (option-exercise o)))
  (validate-path-feature (option-path o) market)
  (when (and (typep engine 'engine) (option-dividends o market))
    (check-discrete-dividends engine o market)))

(defvar *selecting-engine* nil
  "True while automatic selection tries candidates (PRICE-WITH-MODEL), so an
engine can decline where it would only approximate what a later candidate
prices exactly.")

(defun option-dividends (o market)
  "MARKET's discrete dividends going ex by O's expiry."
  (dividends-between (discrete-dividends market) 0d0 (time-to-expiry (option-exercise o) market)))

(defun refuse-dividends (engine &optional (detail "it does not model them") (advice t))
  "Signal MODEL-DOMAIN-ERROR for ENGINE: discrete dividends go ex before the
expiry, and DETAIL; with ADVICE, name the engines that model them."
  (error 'model-domain-error
         :engine engine
         :reason (format nil "discrete dividends go ex before the expiry, and ~A.~:[~; Price ~
with :black-scholes (European vanillas), :rgw (an American call, escrowed model), ~
:binomial, :fd or :mc.~]" detail advice)))

(defgeneric check-discrete-dividends (engine option market)
  (:documentation "Signal MODEL-DOMAIN-ERROR unless ENGINE models the
discrete dividends that go ex during OPTION's life in MARKET. PRICE calls it
only when there are some; the default refuses, so an engine prices with
discrete dividends only where it applies them.")
  (:method ((e engine) o market)
    (declare (ignore o market))
    (refuse-dividends e)))

(defun restart-applies-p (condition type)
  "For a restart :TEST: offer the restart for conditions of TYPE, and when no
condition is given."
  (or (null condition) (typep condition type)))

(defmethod price :around ((o option) engine market)
  ;; Cross-cutting checks, once, instead of in every engine method. The
  ;; expiry cases stay here rather than in VALIDATE-PRICING-INPUTS because
  ;; the at-expiry case returns a price.
  (restart-case
      (let ((today (valuation-date market))
            (expiry (expiry (option-exercise o))))
        (cond ((date< expiry today)
               (error 'option-expired :option o :valuation-date today))
              ((date= expiry today)
               ;; Nothing left to model: the option is worth its payoff.
               (values (payoff-value (option-payoff o) (spot market)) nil
                       (and (typep engine 'engine) engine)))
              (t
               (validate-pricing-inputs o engine market)
               ;; Engine methods return two values; designators already
               ;; return the engine they resolved to.
               (multiple-value-bind (npv se resolved) (call-next-method)
                 (values npv se (or resolved (and (typep engine 'engine) engine)))))))
    (use-value (value)
      :test (lambda (c) (restart-applies-p c '(or option-expired non-positive-volatility)))
      :report "Return a value as the price."
      :interactive (lambda ()
                     (format *query-io* "~&Price: ")
                     (list (eval (read *query-io*))))
      (values value nil))
    (use-engine (new-engine)
      :test (lambda (c) (restart-applies-p c 'unsupported-combination))
      :report "Price with another engine."
      :interactive (lambda ()
                     (format *query-io* "~&Engine (evaluated): ")
                     (list (eval (read *query-io*))))
      (price o new-engine market))))

;; Each engine family re-dispatches on the components it actually cares
;; about. The market is a specializer too, so a term-structure or stochastic
;; market can select different methods.
(defmethod price ((o option) (e analytic-engine) market)
  (price-analytic (option-exercise o) (option-payoff o) (option-path o) e market))

(defun whole-periods (tau period)
  "TAU / PERIOD rounded, if it is within 1e-6 of an integer; else NIL."
  (let* ((ratio (/ tau period))
         (n (round ratio)))
    (and (plusp n) (< (abs (- ratio n)) 1d-6) n)))

(defmethod price ((o option) (e monte-carlo-engine) market)
  ;; USE-PROCESS-PERIOD is offered only when the expiry is a whole number
  ;; of model periods; the handler records the condition for the restart.
  (unless (simulatable-p (process e))
    (error 'unsupported-combination
           :exercise (option-exercise o) :payoff (option-payoff o) :path (option-path o)
           :process (process e) :engine e :market market))
  (let ((tau (time-to-expiry (option-exercise o) market))
        (step-condition nil))
    (handler-bind ((incompatible-time-step (lambda (c) (setf step-condition c))))
      (restart-case
          (price-mc (option-exercise o) (option-payoff o) (option-path o) e market)
        (use-process-period ()
          :test (lambda (c)
                  (and (typep c 'incompatible-time-step)
                       (whole-periods tau (required-dt c))))
          :report "Re-price with one time step per model period."
          (price o (derive-engine e :n-steps (whole-periods tau (required-dt step-condition)))
                 market))))))

;; The COS engine also dispatches on the process: which contracts it can
;; price depends on what the process can tell it.
(defmethod price ((o option) (e cos-engine) market)
  (price-cos (option-exercise o) (option-payoff o) (option-path o)
             (process e) e market))

;; The PDE engine dispatches on the process too: its coefficients (or, for
;; Heston, its two-dimensional scheme) come from it.
(defmethod price ((o option) (e fd-engine) market)
  (price-fd (option-exercise o) (option-payoff o) (option-path o)
            (process e) e market))

(defmethod price ((o option) (e binomial-engine) market)
  (restart-case
      (price-binomial (option-exercise o) (option-payoff o) (option-path o)
                      (process e) e market)
    (increase-steps (n-steps)
      :test (lambda (c) (restart-applies-p c 'invalid-tree))
      :report "Re-price with more tree steps."
      :interactive (lambda ()
                     (format *query-io* "~&Steps: ")
                     (list (parse-integer (read-line *query-io*))))
      (price o (derive-engine e :n-steps n-steps) market))))

(defgeneric price-analytic (exercise payoff path engine market))
(defgeneric price-mc (exercise payoff path engine market))
(defgeneric price-cos (exercise payoff path process engine market))
(defgeneric price-binomial (exercise payoff path process engine market))
(defgeneric price-fd (exercise payoff path process engine market))

(macrolet ((fallback (name)
             `(defmethod ,name (exercise payoff path process (engine engine) market)
                (error 'unsupported-combination
                       :exercise exercise :payoff payoff :path path
                       :process process :engine engine :market market))))
  (fallback price-cos)
  (fallback price-binomial)
  (fallback price-fd))

;; Least-specific fallbacks turn a missing combination into a readable error
;; instead of NO-APPLICABLE-METHOD.
(macrolet ((fallback (name)
             `(defmethod ,name (exercise payoff path (engine engine) market)
                (error 'unsupported-combination
                       :exercise exercise :payoff payoff :path path
                       :engine engine :market market))))
  (fallback price-analytic)
  (fallback price-mc))

;;; --------------------------------------------------------------------
;;; Engine variants and recovery
;;; --------------------------------------------------------------------

(declaim (ftype (function (t &rest t) (values t &optional)) derive-engine))
(defun derive-engine (engine &rest initargs)
  "Make a new engine of the class of ENGINE. INITARGS replace the matching settings.
The new engine copies every other setting. ENGINE does not change: engines are
immutable, like markets.

  (derive-engine (make-instance 'binomial-engine :n-steps 500) :n-steps 4000)"
  (apply #'make-instance (class-of engine)
         `(,@initargs
           ,@(iter (for slot in (c2mop:class-slots (class-of engine)))
                   (for name = (c2mop:slot-definition-name slot))
                   (for initarg = (first (c2mop:slot-definition-initargs slot)))
                   (when (and initarg (slot-boundp engine name))
                     (appending `(,initarg ,(slot-value engine name))))))))

(declaim (ftype (function (t &optional t) (values t &optional)) use-engine))
(defun use-engine (engine &optional condition)
  "Invoke the USE-ENGINE restart for CONDITION, if it exists. Then the instrument is
priced with ENGINE. Otherwise return NIL. PRICE offers the restart for
UNSUPPORTED-COMBINATION."
  (let ((restart (find-restart 'use-engine condition)))
    (when restart (invoke-restart restart engine))))

(declaim (ftype (function (t &optional t) (values t &optional)) increase-steps))
(defun increase-steps (n-steps &optional condition)
  "Invoke the INCREASE-STEPS restart for CONDITION, if it exists. Then the engine
prices the instrument again with N-STEPS steps. Otherwise return NIL. The binomial
engine offers the restart for INVALID-TREE."
  (let ((restart (find-restart 'increase-steps condition)))
    (when restart (invoke-restart restart n-steps))))

(declaim (ftype (function (&optional t) (values t &optional)) use-process-period))
(defun use-process-period (&optional condition)
  "Invoke the USE-PROCESS-PERIOD restart for CONDITION, if it exists. Then the engine
prices the instrument again with one step for each model period. Otherwise return
NIL. The Monte Carlo engine offers the restart for INCOMPATIBLE-TIME-STEP when the
expiry is a whole number of periods."
  (let ((restart (find-restart 'use-process-period condition)))
    (when restart (invoke-restart restart))))

(defmacro with-pricing-policy (clauses &body body)
  "Run BODY with a recovery policy. Each clause is (condition-type (restart-function
argument...)). When a condition of that type is signalled, the macro calls
(restart-function argument... condition). It evaluates the arguments at that time.
The restart functions follow the protocol of CL:USE-VALUE. They invoke their
restart if the condition offers it. Otherwise they return NIL, and the condition
propagates.

  (with-pricing-policy ((unsupported-combination (use-engine *mc*))
                        (invalid-tree (increase-steps 4000))
                        (option-expired (use-value 0d0)))
    (mapcar (lambda (o) (price o *cos* market)) options))"
  `(handler-bind
       ,(iter (for (type (function . arguments)) in clauses)
              (collect (let ((c (gensym "CONDITION")))
                         `(,type (lambda (,c) (,function ,@arguments ,c))))))
     ,@body))
