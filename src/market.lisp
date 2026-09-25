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
;;;;
;;;; Every protocol function taking a point in time accepts either a DATE or
;;;; a REAL. A real is a horizon in market time: the year fraction from the
;;;; valuation date under the market's day counter. Methods on DATE convert
;;;; with MARKET-TIME and delegate, so concrete markets implement only the
;;;; REAL methods.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defclass market ()
  ()
  (:documentation "State of the world at a valuation date."))

;;; --------------------------------------------------------------------
;;; Time
;;; --------------------------------------------------------------------

(defgeneric valuation-date (market)
  (:documentation "The date the market is observed on."))

(defgeneric market-day-counter (market)
  (:documentation "The day counter that turns dates into market time."))

(defun market-time (market date)
  "Year fraction from MARKET's valuation date to DATE under its day counter.

  (market-time m #D\"2027-09-24\") => 1.0d0   ; valued 2026-09-24, ACT/365F"
  (year-fraction (market-day-counter market) (valuation-date market) date))

;;; --------------------------------------------------------------------
;;; The protocol
;;; --------------------------------------------------------------------

(defgeneric spot (market)
  (:documentation "Current price of the underlying."))

(defgeneric zero-rate (market x)
  (:documentation "Continuously compounded risk-free zero rate to X, a date
or a horizon in market time."))

(defgeneric dividend-yield (market x)
  (:documentation "Continuously compounded dividend yield to X, a date or a
horizon in market time."))

(defgeneric black-vol (market strike x)
  (:documentation "Black-Scholes volatility for STRIKE to X, a date or a
horizon in market time. A flat market ignores STRIKE and X; a surface
interpolates."))

(defgeneric black-variance (market strike x)
  (:documentation "Total Black variance for STRIKE to X, a date or a
horizon in market time: vol^2 * horizon for a flat market."))

;;; Derived quantities. Defaults are expressed in terms of the protocol
;;; above, so a market only has to implement the primitives; a curve
;;; market can still override these directly with its own interpolation.

(defgeneric discount-factor (market x)
  (:method ((m market) (horizon real))
    (exp (- (* (zero-rate m horizon) horizon)))))

(defgeneric forward-factor (market x)
  (:documentation "Growth factor of the underlying: exp((r - q) horizon).")
  (:method ((m market) (horizon real))
    (exp (* (- (zero-rate m horizon) (dividend-yield m horizon)) horizon))))

(defgeneric forward (market x)
  (:method ((m market) (horizon real))
    (* (spot m) (forward-factor m horizon))))

;;; Dates: convert to market time and delegate to the REAL methods.

(defmethod zero-rate ((m market) (x date))
  (zero-rate m (market-time m x)))
(defmethod dividend-yield ((m market) (x date))
  (dividend-yield m (market-time m x)))
(defmethod black-vol ((m market) strike (x date))
  (black-vol m strike (market-time m x)))
(defmethod black-variance ((m market) strike (x date))
  (black-variance m strike (market-time m x)))
(defmethod discount-factor ((m market) (x date))
  (discount-factor m (market-time m x)))
(defmethod forward-factor ((m market) (x date))
  (forward-factor m (market-time m x)))
(defmethod forward ((m market) (x date))
  (forward m (market-time m x)))

;;; --------------------------------------------------------------------
;;; Flat Black-Scholes market
;;; --------------------------------------------------------------------

(defclass black-scholes-market (market)
  ((valuation-date :initarg :valuation-date :reader valuation-date :type date
                   :initform (a:required-argument :valuation-date))
   (day-counter :initarg :day-counter :reader market-day-counter
                :type day-counter :initform +actual-365-fixed+)
   (spot :initarg :spot :reader %spot :type double-float
         :initform (a:required-argument :spot))
   (rate :initarg :rate :reader %rate :type double-float :initform 0d0)
   (dividend :initarg :dividend :reader %dividend :type double-float
             :initform 0d0)
   (vol :initarg :vol :reader %vol :type double-float
        :initform (a:required-argument :vol)))
  (:documentation "Constant rate, dividend yield and volatility."))

(defmethod spot ((m black-scholes-market)) (%spot m))
(defmethod zero-rate ((m black-scholes-market) (horizon real))
  (declare (ignore horizon)) (%rate m))
(defmethod dividend-yield ((m black-scholes-market) (horizon real))
  (declare (ignore horizon)) (%dividend m))
(defmethod black-vol ((m black-scholes-market) strike (horizon real))
  (declare (ignore strike horizon)) (%vol m))
(defmethod black-variance ((m black-scholes-market) strike (horizon real))
  (declare (ignore strike))
  (* (%vol m) (%vol m) horizon))

(defun make-bs-market (&key valuation-date (day-counter +actual-365-fixed+)
                            spot rate (dividend 0d0) vol)
  "Return a BLACK-SCHOLES-MARKET observed on VALUATION-DATE. Numeric inputs
are coerced to double-float; pass double-float or rational literals, since
0.05 reads as a single-float and (float 0.05 1d0) is 0.05000000074505806d0.

  (make-bs-market :valuation-date #D\"2026-09-24\"
                  :spot 100 :rate 0.05d0 :vol 0.2d0)"
  (make-instance 'black-scholes-market
                 :valuation-date (or valuation-date
                                     (a:required-argument :valuation-date))
                 :day-counter day-counter
                 :spot (float spot 1d0) :rate (float rate 1d0)
                 :dividend (float dividend 1d0) :vol (float vol 1d0)))

(defgeneric derive-market (market &key &allow-other-keys)
  (:documentation "Return a new market equal to MARKET except for the given
keys. MARKET is not modified.

  (derive-market m :valuation-date #D\"2027-03-24\" :spot 95)"))

(defmethod derive-market ((m black-scholes-market)
                          &key (valuation-date (valuation-date m))
                               (day-counter (market-day-counter m))
                               (spot (%spot m)) (rate (%rate m))
                               (dividend (%dividend m)) (vol (%vol m)))
  (make-bs-market :valuation-date valuation-date :day-counter day-counter
                  :spot spot :rate rate :dividend dividend :vol vol))

(defmethod print-object ((m black-scholes-market) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A S=~,4F r=~,4F q=~,4F vol=~,4F"
            (valuation-date m) (%spot m) (%rate m) (%dividend m) (%vol m))))
