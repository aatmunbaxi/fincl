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
       (format s "~A has no method for ~A / ~A / ~A~@[ under ~A~] in a ~A."
               (name (error-engine c)) (name (error-exercise c))
               (name (error-payoff c)) (name (error-path c))
               (and (error-process c) (name (error-process c)))
               (name (error-market c)))))))

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

(defclass engine () ())

(defclass analytic-engine (engine) ()
  (:documentation "Closed forms. An analytic engine's model is fixed by its
class: there is no process slot, because a closed form exists only for the
dynamics it was derived under. Adding Heston's characteristic-function
solution means adding an engine class and a method, not a parameter."))

(defclass black-scholes-engine (analytic-engine) ())

(defclass barone-adesi-whaley-engine (analytic-engine) ()
  (:documentation "Quadratic approximation for American vanillas."))

(defclass monte-carlo-engine (engine)
  ((process :initarg :process :initform (make-instance 'gbm) :reader process
            :documentation "Path-generation backend. The engine is model
agnostic: everything model-specific comes from the process.")
   (n-paths :initarg :n-paths :initform 100000 :reader n-paths)
   (n-steps :initarg :n-steps :initform 50 :reader n-steps
            :documentation "Time steps; also the exercise dates for LSM.")
   (seed :initarg :seed :initform 42 :reader seed)
   (rng :initarg :rng :initform :mersenne-twister-64 :reader rng
        :documentation "A random-state generator designator: a keyword,
(keyword seed-offset), or :native for the host RNG.")
   (sampler :initarg :sampler :initform :box-muller :reader sampler
            :documentation ":box-muller or :inverse-transform.")
   (antithetic :initarg :antithetic :initform t :reader antithetic)
   (n-chunks :initarg :n-chunks :initform 64 :reader n-chunks
             :documentation "Work units, fixed independently of the thread
count so results do not depend on how many workers are running.")
   (basis-degree :initarg :basis-degree :initform 3 :reader basis-degree
                 :documentation "Polynomial degree of the LSM regression.")))

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
  (:documentation "Fang and Oosterlee's Fourier-cosine (COS) method.
Europeans need only a characteristic function; Bermudans and Americans use
the backward recursion of their 2009 paper, which needs a process whose
log-increments are independent of the state (a Levy process).

  (make-instance 'cos-engine :process (make-instance 'heston) :n-terms 160)"))

;;; --------------------------------------------------------------------
;;; Protocol
;;; --------------------------------------------------------------------

(defgeneric price (instrument engine market)
  (:documentation "Returns (values npv standard-error). The second value is
NIL for deterministic engines."))

(defgeneric validate-pricing-inputs (instrument engine market)
  (:documentation "Signal an error if INSTRUMENT cannot be priced by ENGINE
in MARKET. PRICE calls it once, before dispatching to the engine; the
default accepts everything.")
  (:method (instrument engine market)
    (declare (ignore instrument engine market))
    nil))

(defmethod validate-pricing-inputs ((o option) engine market)
  (validate-payoff (option-payoff o) market (expiry (option-exercise o))))

(defmethod price :around ((o option) engine market)
  ;; Cross-cutting checks, once, instead of in every engine method. The
  ;; expiry cases stay here rather than in VALIDATE-PRICING-INPUTS because
  ;; the at-expiry case returns a price.
  (let ((today (valuation-date market))
        (expiry (expiry (option-exercise o))))
    (cond ((date< expiry today)
           (error 'option-expired :option o :valuation-date today))
          ((date= expiry today)
           ;; Nothing left to model: the option is worth its payoff.
           (values (payoff-value (option-payoff o) (spot market)) nil))
          (t
           (validate-pricing-inputs o engine market)
           (call-next-method)))))

;; Each engine family re-dispatches on the components it actually cares
;; about. The market is a specializer too, so a term-structure or stochastic
;; market can select different methods.
(defmethod price ((o option) (e analytic-engine) market)
  (price-analytic (option-exercise o) (option-payoff o) (option-path o) e market))

(defmethod price ((o option) (e monte-carlo-engine) market)
  (price-mc (option-exercise o) (option-payoff o) (option-path o) e market))

;; The COS engine also dispatches on the process: which contracts it can
;; price depends on what the process can tell it.
(defmethod price ((o option) (e cos-engine) market)
  (price-cos (option-exercise o) (option-payoff o) (option-path o)
             (process e) e market))

(defmethod price ((o option) (e binomial-engine) market)
  (price-binomial (option-exercise o) (option-payoff o) (option-path o)
                  (process e) e market))

(defgeneric price-analytic (exercise payoff path engine market))
(defgeneric price-mc (exercise payoff path engine market))
(defgeneric price-cos (exercise payoff path process engine market))
(defgeneric price-binomial (exercise payoff path process engine market))

(macrolet ((fallback (name)
             `(defmethod ,name (exercise payoff path process (engine engine) market)
                (error 'unsupported-combination
                       :exercise exercise :payoff payoff :path path
                       :process process :engine engine :market market))))
  (fallback price-cos)
  (fallback price-binomial))

;; Least-specific fallbacks turn a missing combination into a readable error
;; instead of NO-APPLICABLE-METHOD.
(macrolet ((fallback (name)
             `(defmethod ,name (exercise payoff path (engine engine) market)
                (error 'unsupported-combination
                       :exercise exercise :payoff payoff :path path
                       :engine engine :market market))))
  (fallback price-analytic)
  (fallback price-mc))
