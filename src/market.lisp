;;;; market.lisp --- the market protocol
;;;;
;;;; Engines never read market slots. They go through these generic
;;;; functions, so a term-structure market can be dropped in later without
;;;; touching a single engine. The flat Black-Scholes market below is one
;;;; implementation of the protocol, not the definition of it.
;;;;
;;;; A market is an immutable snapshot: it is passed to every parallel
;;;; worker, so nothing may mutate it. Scenarios are new markets, not
;;;; edits to an existing one.

(in-package #:fincl)

(defclass market ()
  ()
  (:documentation "State of the world at a valuation instant."))

;;; --------------------------------------------------------------------
;;; The protocol
;;; --------------------------------------------------------------------

(defgeneric spot (market)
  (:documentation "Current price of the underlying."))

(defgeneric zero-rate (market time)
  (:documentation "Continuously compounded risk-free zero rate to TIME."))

(defgeneric dividend-yield (market time)
  (:documentation "Continuously compounded dividend yield to TIME."))

(defgeneric black-vol (market strike time)
  (:documentation "Black-Scholes volatility for STRIKE and TIME. A flat
market ignores both arguments; a surface interpolates."))

;;; Derived quantities. Defaults are expressed in terms of the protocol
;;; above, so a market only has to implement the three primitives; a curve
;;; market can still override these directly with its own interpolation.

(defgeneric discount-factor (market time)
  (:method ((m market) time)
    (exp (- (* (zero-rate m time) time)))))

(defgeneric forward-factor (market time)
  (:documentation "Growth factor of the underlying: exp((r - q) T).")
  (:method ((m market) time)
    (exp (* (- (zero-rate m time) (dividend-yield m time)) time))))

(defgeneric forward (market time)
  (:method ((m market) time)
    (* (spot m) (forward-factor m time))))

;;; --------------------------------------------------------------------
;;; Flat Black-Scholes market
;;; --------------------------------------------------------------------

(defclass black-scholes-market (market)
  ((spot :initarg :spot :reader %spot :type double-float
         :initform (a:required-argument :spot))
   (rate :initarg :rate :reader %rate :type double-float :initform 0d0)
   (dividend :initarg :dividend :reader %dividend :type double-float
             :initform 0d0)
   (vol :initarg :vol :reader %vol :type double-float
        :initform (a:required-argument :vol)))
  (:documentation "Constant rate, dividend yield and volatility."))

(defmethod spot ((m black-scholes-market)) (%spot m))
(defmethod zero-rate ((m black-scholes-market) time)
  (declare (ignore time)) (%rate m))
(defmethod dividend-yield ((m black-scholes-market) time)
  (declare (ignore time)) (%dividend m))
(defmethod black-vol ((m black-scholes-market) strike time)
  (declare (ignore strike time)) (%vol m))

(defun make-bs-market (&key spot rate (dividend 0d0) vol)
  "Inputs are coerced to double-float. Pass double-float or rational
literals: (float 0.05 1d0) is 0.05000000074505806d0, because 0.05 reads as a
single-float. Write 0.05d0 or 1/20, or set *read-default-float-format* to
'double-float in your session."
  (make-instance 'black-scholes-market
                 :spot (float spot 1d0) :rate (float rate 1d0)
                 :dividend (float dividend 1d0) :vol (float vol 1d0)))

(defmethod print-object ((m black-scholes-market) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "S=~,4F r=~,4F q=~,4F vol=~,4F"
            (%spot m) (%rate m) (%dividend m) (%vol m))))
