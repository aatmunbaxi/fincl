;;;; term-structures/protocol.lisp --- curves and vol structures
;;;;
;;;; A term structure answers one question as a function of a horizon: how
;;;; much a unit of currency is worth today (a yield curve), how much of the
;;;; underlying's value is paid away as dividends (a dividend curve), how much
;;;; total Black variance there is (a vol structure). Horizons are reals in the
;;;; market time of the market holding the structure, so structures know no
;;;; dates and no day counter.
;;;;
;;;; Each protocol has a primitive (a discount factor, a total variance) and
;;;; a derived quantity with a default built on it (a zero rate, a vol). A
;;;; flat structure overrides the derived quantity with its constant, so a
;;;; flat market computes exactly what a flat Black-Scholes market computed.

(in-package #:fincl)

(defclass term-structure () ()
  (:documentation "A market quantity as a function of the horizon."))

(defclass yield-curve (term-structure) ()
  (:documentation "Discount factors: CURVE-DISCOUNT, with CURVE-ZERO-RATE
derived."))

(defclass dividend-curve (term-structure) ()
  (:documentation "The fraction of the underlying's value not paid away as
dividends: CURVE-DIVIDEND-DISCOUNT, with CURVE-DIVIDEND-YIELD derived."))

(defclass black-vol-structure (term-structure) ()
  (:documentation "Total Black variance by strike and horizon:
STRUCTURE-BLACK-VARIANCE, with STRUCTURE-BLACK-VOL derived."))

(defgeneric curve-discount (curve horizon)
  (:documentation "Discount factor to HORIZON (market time).

  (curve-discount (make-instance 'flat-yield-curve :rate 0.05d0) 1d0) => 0.951229..."))

(defgeneric curve-zero-rate (curve horizon)
  (:documentation "Continuously compounded zero rate to HORIZON.")
  (:method ((c yield-curve) horizon)
    (if (zerop horizon)
        (curve-zero-rate c 1d-6)
        (/ (- (log (curve-discount c horizon))) horizon))))

(defgeneric curve-dividend-discount (curve horizon)
  (:documentation "exp(-integral of the dividend yield to HORIZON)."))

(defgeneric curve-dividend-yield (curve horizon)
  (:documentation "Continuously compounded average dividend yield to HORIZON.")
  (:method ((c dividend-curve) horizon)
    (if (zerop horizon)
        (curve-dividend-yield c 1d-6)
        (/ (- (log (curve-dividend-discount c horizon))) horizon))))

(defgeneric structure-black-variance (structure strike horizon)
  (:documentation "Total Black variance for STRIKE to HORIZON."))

(defgeneric structure-black-vol (structure strike horizon)
  (:documentation "Black volatility for STRIKE to HORIZON.")
  (:method ((s black-vol-structure) strike horizon)
    (if (zerop horizon)
        (structure-black-vol s strike 1d-6)
        (sqrt (/ (structure-black-variance s strike horizon) horizon)))))

;;; --------------------------------------------------------------------
;;; Shifts, for risk-factor bumps
;;; --------------------------------------------------------------------

(deftype shift ()
  "(:relative x) multiplies by 1 + x; (:absolute x) adds x."
  '(cons (member :relative :absolute) (cons real null)))

(defun apply-shift (value shift)
  "VALUE moved by SHIFT, a (:relative x) or (:absolute x) list.

  (apply-shift 100d0 '(:relative 1/100)) => 101.0d0"
  (check-input shift shift "(:relative x) or (:absolute x)")
  (destructuring-bind (kind amount) shift
    (ecase kind
      (:relative (* value (+ 1 (float amount 1d0))))
      (:absolute (+ value (float amount 1d0))))))

(defgeneric bump-structure (structure shift)
  (:documentation "A new structure with its level moved by SHIFT ((:relative
x) or (:absolute x)): rates for curves, the vol for vol structures.
STRUCTURE is unchanged.

  (bump-structure (make-instance 'flat-black-vol :vol 0.2d0) '(:absolute 0.01d0))
  => a flat 21% vol"))
