;;;; engines/engine.lisp --- engine classes and the pricing protocol

(in-package #:fincl)

(define-condition unsupported-combination (pricing-error)
  ((exercise :initarg :exercise :reader error-exercise)
   (payoff :initarg :payoff :reader error-payoff)
   (path :initarg :path :reader error-path)
   (engine :initarg :engine :reader error-engine)
   (market :initarg :market :reader error-market))
  (:report
   (lambda (c s)
     (flet ((name (x) (class-name (class-of x))))
       (format s "~A has no method for ~A / ~A / ~A in a ~A."
               (name (error-engine c)) (name (error-exercise c))
               (name (error-payoff c)) (name (error-path c))
               (name (error-market c)))))))

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

;;; --------------------------------------------------------------------
;;; Protocol
;;; --------------------------------------------------------------------

(defgeneric price (instrument engine market)
  (:documentation "Returns (values npv standard-error). The second value is
NIL for deterministic engines."))

(defmethod price :around ((o option) engine market)
  ;; Cross-cutting validation, once, instead of in every engine method.
  (assert (plusp (expiry (option-exercise o))) () "Option has expired.")
  (assert (plusp (black-vol market (strike (option-payoff o))
                            (expiry (option-exercise o))))
          () "Volatility must be positive.")
  (call-next-method))

;; Each engine family re-dispatches on the components it actually cares
;; about. The market is a specializer too, so a term-structure or stochastic
;; market can select different methods.
(defmethod price ((o option) (e analytic-engine) market)
  (price-analytic (option-exercise o) (option-payoff o) (option-path o) e market))

(defmethod price ((o option) (e monte-carlo-engine) market)
  (price-mc (option-exercise o) (option-payoff o) (option-path o) e market))

(defgeneric price-analytic (exercise payoff path engine market))
(defgeneric price-mc (exercise payoff path engine market))

;; Least-specific fallbacks turn a missing combination into a readable error
;; instead of NO-APPLICABLE-METHOD.
(macrolet ((fallback (name)
             `(defmethod ,name (exercise payoff path (engine engine) market)
                (error 'unsupported-combination
                       :exercise exercise :payoff payoff :path path
                       :engine engine :market market))))
  (fallback price-analytic)
  (fallback price-mc))
