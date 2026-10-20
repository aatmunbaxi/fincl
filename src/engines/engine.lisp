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
  (:documentation "The engine has a method for the instrument, but its
approximation does not hold in this market regime."))

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
  (:documentation "Closed forms. An analytic engine's model is fixed by its
class: there is no process slot, because a closed form exists only for the
dynamics it was derived under. Adding Heston's characteristic-function
solution means adding an engine class and a method, not a parameter."))

(defclass black-scholes-engine (analytic-engine) ()
  (:metaclass engine-class)
  (:designator :black-scholes)
  (:models gbm)
  (:documentation "Black's formula for European vanillas."))

(defclass barone-adesi-whaley-engine (analytic-engine) ()
  (:metaclass engine-class)
  (:designator :baw)
  (:models gbm)
  (:documentation "Quadratic approximation for American vanillas."))

(defclass merton-series-engine (analytic-engine)
  ((process :initarg :process :initform (make-instance 'merton-jump) :reader process
            :type merton-jump
            :documentation "The Merton jump diffusion whose parameters the
series uses."))
  (:metaclass engine-class)
  (:designator :merton-series)
  (:models merton-jump)
  (:documentation "Merton's (1976) series for European vanillas under a jump
diffusion: Black prices conditional on the number of jumps, weighted by
their Poisson probabilities.

  (make-instance 'merton-series-engine :process (make-instance 'merton-jump :intensity 1))"))

(defclass cev-engine (analytic-engine)
  ((process :initarg :process :initform (make-instance 'cev :beta 0.5d0) :reader process
            :type cev
            :documentation "The CEV process, with beta <= 1."))
  (:metaclass engine-class)
  (:designator :cev)
  (:models cev)
  (:documentation "Schroder's (1989) noncentral chi-square formula for
European vanillas under CEV with absorption at zero, beta <= 1.

  (make-instance 'cev-engine :process (make-instance 'cev :beta 0.5d0))"))

(defclass monte-carlo-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "Path-generation backend. The engine is model
agnostic: everything model-specific comes from the process.")
   (n-paths :initarg :n-paths :initform 100000 :reader n-paths)
   (n-steps :initarg :n-steps :initform 50 :reader n-steps
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
   (n-steps :initarg :n-steps :initform 2000 :reader n-steps
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
  (:documentation "Cox-Ross-Rubinstein binomial tree.

  (make-instance 'binomial-engine :n-steps 5000)"))

(defclass cos-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "Supplies the characteristic function and
cumulants of the log-return.")
   (n-terms :initarg :n-terms :initform 256 :reader n-terms
            :documentation "Terms in the Fourier-cosine expansion.")
   (truncation :initarg :truncation :initform 12d0 :reader truncation
               :documentation "L in the truncation range
[c1 - L sqrt(c2 + sqrt(c4)), c1 + L sqrt(c2 + sqrt(c4))].")
   (richardson-dates :initarg :richardson-dates :initform 16
                     :reader richardson-dates
                     :documentation "Exercise dates M of the coarsest
Bermudan in the American extrapolation, which uses M, 2M, 4M and 8M."))
  (:metaclass engine-class)
  (:designator :cos)
  (:models stochastic-process)
  (:documentation "Fang and Oosterlee's Fourier-cosine (COS) method.
Europeans need only a characteristic function; Bermudans and Americans use
the backward recursion of their 2009 paper, which needs a process whose
log-increments are independent of the state (the INDEPENDENT-INCREMENTS
mixin; GBM).

  (make-instance 'cos-engine :process (make-instance 'heston) :n-terms 160)"))

(defclass fd-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "The dynamics: GBM, CEV and local vol on a log-spot
grid (PDE-COEFFICIENTS), Heston on a (log-spot, variance) grid by ADI.")
   (n-space :initarg :n-space :initform 200 :reader n-space
            :documentation "Log-spot grid points.")
   (n-variance :initarg :n-variance :initform 50 :reader n-variance
               :documentation "Variance grid points (Heston).")
   (n-time :initarg :n-time :initform 200 :reader n-time
           :documentation "Time steps to expiry; exercise and monitoring dates are
grid times, with gaps filled evenly.")
   (theta :initarg :theta :initform 1/2 :reader fd-theta
          :documentation "The theta-scheme weight: 1/2 Crank-Nicolson, 1 implicit.
Heston's ADI uses Hundsdorfer-Verwer's 1/2 + sqrt(3)/6 instead.")
   (rannacher-steps :initarg :rannacher-steps :initform 2 :reader rannacher-steps
                    :documentation "Fully implicit substeps replacing the step after
each payoff discontinuity (expiry, Bermudan exercise, barrier monitoring).
0 disables the smoothing.")
   (concentration :initarg :concentration :initform 2 :reader concentration
                  :documentation "Width of the sinh concentration around the strike,
in standard deviations of ln S_T; NIL for a uniform grid.")
   (width :initarg :width :initform 5 :reader grid-width
          :documentation "Half-width of the log-spot grid, in standard deviations of
ln S_T around the spot."))
  (:metaclass engine-class)
  (:designator :fd)
  (:models stochastic-process)
  (:documentation "Finite differences in log-spot: a theta-scheme with
Rannacher start-up on a sinh-concentrated grid, Ikonen-Toivanen splitting
for American exercise, continuous barriers as boundary conditions; Heston by
Hundsdorfer-Verwer ADI.

  (make-engine :fd :process (make-local-vol) :n-space 400 :n-time 400)"))

;;; --------------------------------------------------------------------
;;; Protocol
;;; --------------------------------------------------------------------

(defgeneric price (instrument engine market)
  ;; The market first: see src/pricing.lisp.
  (:argument-precedence-order market instrument engine)
  (:documentation "Returns (values npv standard-error engine). The second
value is NIL for deterministic engines; the third is the engine that priced.
ENGINE may be an engine designator (src/engines/selection.lisp): a keyword
such as :binomial, a list (:cos :n-terms 512) as data, a process to price
under with an automatically chosen engine, or :auto for GBM.

  (price opt (make-engine :binomial :n-steps 4000) m)
  (price opt (make-heston :xi 0.5d0) m)"))

(defgeneric engine-candidates (designator exercise payoff path process)
  (:method-combination append)
  (:documentation "((priority designator) ...): how automatic selection may
use the engine DESIGNATOR (a keyword) for EXERCISE x PAYOFF x PATH under
PROCESS, tried in increasing priority across engines (10 a closed form, 20
a Fourier or lattice method, 30 an approximation, 90 or more a slow
fallback). Each engine file adds APPEND methods on (eql its-designator) next
to its pricing methods: the designator keeps two engines with the same
specializers from replacing each other's method.")
  (:method append (designator exercise payoff path process)
    (declare (ignore designator exercise payoff path process))
    '()))

(defgeneric validate-pricing-inputs (instrument engine market)
  (:documentation "Signal an error if INSTRUMENT cannot be priced by ENGINE
in MARKET. PRICE calls it once, before dispatching to the engine; the
default accepts everything.")
  (:method (instrument engine market)
    (declare (ignore instrument engine market))
    nil))

(defmethod validate-pricing-inputs ((o option) engine market)
  (validate-payoff (option-payoff o) market (expiry (option-exercise o)))
  (validate-path-feature (option-path o) market))

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

(defun derive-engine (engine &rest initargs)
  "A new engine of ENGINE's class with INITARGS replacing the matching
settings and every other setting copied. ENGINE is unchanged: engines are
immutable, like markets.

  (derive-engine (make-instance 'binomial-engine :n-steps 500) :n-steps 4000)"
  (apply #'make-instance (class-of engine)
         `(,@initargs
           ,@(iter (for slot in (c2mop:class-slots (class-of engine)))
                   (for name = (c2mop:slot-definition-name slot))
                   (for initarg = (first (c2mop:slot-definition-initargs slot)))
                   (when (and initarg (slot-boundp engine name))
                     (appending `(,initarg ,(slot-value engine name))))))))

(defun use-engine (engine &optional condition)
  "Invoke the USE-ENGINE restart for CONDITION, if there is one, so the
instrument is priced with ENGINE; otherwise return NIL. PRICE offers it for
UNSUPPORTED-COMBINATION."
  (let ((restart (find-restart 'use-engine condition)))
    (when restart (invoke-restart restart engine))))

(defun increase-steps (n-steps &optional condition)
  "Invoke the INCREASE-STEPS restart for CONDITION, if there is one, so the
instrument is re-priced with N-STEPS steps; otherwise return NIL. The
binomial engine offers it for INVALID-TREE."
  (let ((restart (find-restart 'increase-steps condition)))
    (when restart (invoke-restart restart n-steps))))

(defun use-process-period (&optional condition)
  "Invoke the USE-PROCESS-PERIOD restart for CONDITION, if there is one, so
the instrument is re-priced with one step per model period; otherwise return
NIL. The Monte Carlo engine offers it for INCOMPATIBLE-TIME-STEP when the
expiry is a whole number of periods."
  (let ((restart (find-restart 'use-process-period condition)))
    (when restart (invoke-restart restart))))

(defmacro with-pricing-policy (clauses &body body)
  "Run BODY with a recovery policy. Each clause is (condition-type
(restart-function argument...)): when a condition of that type is signalled,
(restart-function argument... condition) is called, with the arguments
evaluated then. Restart functions follow CL:USE-VALUE's protocol: they invoke
their restart if it is available for the condition and otherwise return NIL,
so the condition propagates.

  (with-pricing-policy ((unsupported-combination (use-engine *mc*))
                        (invalid-tree (increase-steps 4000))
                        (option-expired (use-value 0d0)))
    (mapcar (lambda (o) (price o *cos* market)) options))"
  `(handler-bind
       ,(iter (for (type (function . arguments)) in clauses)
              (collect (let ((c (gensym "CONDITION")))
                         `(,type (lambda (,c) (,function ,@arguments ,c))))))
     ,@body))
