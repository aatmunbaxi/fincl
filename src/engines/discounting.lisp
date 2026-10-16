;;;; engines/discounting.lisp --- rates engines; discounting bonds, swaps, FRAs
;;;;
;;;; RATES-ENGINE is the root of every engine for interest-rate instruments.
;;;; They price on a composite market as a whole (see PRICE :AROUND in
;;;; pricing.lisp), so they read curves by id rather than through the
;;;; single-underlying protocol. The discounting engine values cash flows
;;;; that need no model: bonds, swaps and FRAs.

(in-package #:fincl)

(defclass rates-engine (engine) ()
  (:metaclass engine-class)
  (:documentation "An engine for interest-rate instruments."))

(defclass discounting-engine (rates-engine)
  ((discount-curve :initarg :discount-curve :initform nil :reader engine-discount-curve
                   :type (or null keyword)
                   :documentation "The id of the curve to discount on; NIL: the
curve of the instrument's currency."))
  (:metaclass engine-class)
  (:designator :discounting)
  (:documentation "Values an instrument's cash flows by discounting: a bond
(its flows after the valuation date), a swap (the sum of its signed legs) or
an FRA.

  (price swap :discounting market)
  (price bond (make-engine :discounting :discount-curve :usd-ois) market)"))

(defmethod price :around ((i rates-instrument) engine market)
  (check-input market composite-market "a composite market, which holds the curves and fixings")
  (multiple-value-bind (npv se resolved) (call-next-method)
    (values npv se (or resolved (and (typep engine 'engine) engine)))))

(defmethod price ((i rates-instrument) (designator symbol) market)
  (when (eq designator :auto)
    (signal-invalid-argument 'engine designator 'keyword
                             "an engine for interest-rate instruments, such as :DISCOUNTING"))
  (price i (ensure-engine designator) market))

(defmethod price ((i rates-instrument) (designator cons) market)
  (price i (ensure-engine designator) market))

(defmethod price ((i rates-instrument) (e engine) market)
  (error 'unsupported-combination :exercise nil :payoff i :path nil :engine e :market market))

(defmethod price ((b bond) (e discounting-engine) market)
  (values (bond-npv b market :discount-curve (engine-discount-curve e)) nil))

(defmethod price ((s swap) (e discounting-engine) market)
  (values (swap-npv s market :discount-curve (engine-discount-curve e)) nil))

(defmethod price ((f forward-rate-agreement) (e discounting-engine) market)
  (values (fra-npv f market :discount-curve (engine-discount-curve e)) nil))
