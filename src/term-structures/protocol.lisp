;;;; term-structures/protocol.lisp --- curves and vol structures
;;;;
;;;; A term structure answers one question as a function of a horizon: how
;;;; much a unit of currency is worth today (a yield curve), how much of the
;;;; underlying's value is paid away as dividends (a dividend curve), how much
;;;; total Black variance there is (a vol structure).
;;;;
;;;; Flat structures know no dates: their horizons are reals in the market
;;;; time of the market holding them. A DATED-CURVE (yield-curves.lisp) has a
;;;; reference date and a day counter of its own; it takes dates, or reals in
;;;; its own time, and a market view adapts it to market time
;;;; (CURVE-IN-MARKET).
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

(defgeneric curve-discount (curve x)
  (:documentation "Discount factor to X: a horizon (a real, in the curve's
time), or a date for a curve with a reference date.

  (curve-discount (make-instance 'flat-yield-curve :rate 0.05d0) 1d0) => 0.951229..."))

(defmethod curve-discount ((c yield-curve) (x date))
  (signal-invalid-argument 'x x 'real
                           (format nil "a horizon: ~A has no reference date to measure a date from"
                                   (class-name (class-of c)))))

(defparameter *rate-dt* 1d-4
  "The short time over which a zero rate at the reference date, or an
instantaneous forward, is measured (QuantLib's dt).")

(defgeneric curve-zero-rate (curve x &optional compounding frequency day-counter)
  (:documentation "The zero rate to X (a horizon or, for a dated curve, a
date) quoted with COMPOUNDING (default :CONTINUOUS) and FREQUENCY (default
:ANNUAL), time measured by DAY-COUNTER (default the curve's) from the
reference date to a date X. A double-float, as QuantLib's
YieldTermStructure::zeroRate.

  (curve-zero-rate curve #D\"2031-06-15\" :compounded :annual +actual-365-fixed+)"))

(defmethod curve-zero-rate ((c yield-curve) x &optional (compounding :continuous) (frequency :annual)
                                                  day-counter)
  (etypecase x
    (real
     (if (and (eq compounding :continuous) (null day-counter))
         (if (zerop x)
             (curve-zero-rate c 1d-6)
             (/ (- (log (curve-discount c x))) x))
         (let ((time (if (zerop x) *rate-dt* (float x 1d0))))
           (interest-rate-rate
            (implied-rate (/ (curve-discount c time)) (or day-counter (curve-day-counter c))
                          compounding frequency time)))))
    (date
     (let ((dc (or day-counter (curve-day-counter c)))
           (time (curve-time c x)))
       (interest-rate-rate
        (if (zerop time)
            (implied-rate (/ (curve-discount c *rate-dt*)) dc compounding frequency *rate-dt*)
            (implied-rate (/ (curve-discount c x)) dc compounding frequency
                          (curve-reference-date c) x)))))))

(defgeneric curve-forward-rate (curve x1 x2 &optional compounding frequency day-counter)
  (:documentation "The forward rate from X1 to X2 (horizons, or dates for a
dated curve) quoted with COMPOUNDING (default :CONTINUOUS) and FREQUENCY
(default :ANNUAL) over DAY-COUNTER's fraction between the dates (default the
curve's). Equal X1 and X2 give the instantaneous forward, measured over
*RATE-DT*. As QuantLib's YieldTermStructure::forwardRate."))

(defmethod curve-forward-rate ((c yield-curve) x1 x2 &optional (compounding :continuous)
                                                       (frequency :annual) day-counter)
  (let ((dc (or day-counter (curve-day-counter c))))
    (flet ((instantaneous (t1)
             (let* ((t1 (max (- t1 (/ *rate-dt* 2)) 0d0))
                    (t2 (+ t1 *rate-dt*)))
               (interest-rate-rate
                (implied-rate (/ (curve-discount c t1) (curve-discount c t2))
                              dc compounding frequency *rate-dt*)))))
      (etypecase x1
        (real
         (cond ((= x1 x2) (instantaneous (float x1 1d0)))
               ((< x2 x1) (signal-invalid-argument 'x2 x2 'real (format nil "a horizon after ~A" x1)))
               (t (interest-rate-rate
                   (implied-rate (/ (curve-discount c x1) (curve-discount c x2))
                                 dc compounding frequency (- x2 x1))))))
        (date
         (cond ((date= x1 x2) (instantaneous (curve-time c x1)))
               ((date< x2 x1) (signal-invalid-argument 'x2 x2 'date (format nil "a date after ~A" x1)))
               (t (interest-rate-rate
                   (implied-rate (/ (curve-discount c x1) (curve-discount c x2))
                                 dc compounding frequency x1 x2)))))))))

(defgeneric curve-day-counter (curve)
  (:documentation "The day counter that measures the curve's time: the
curve's own for a dated curve; ACT/365F, nominally, for an undated one.")
  (:method ((c term-structure)) +actual-365-fixed+))

(defgeneric curve-time (curve date)
  (:documentation "Year fraction from CURVE's reference date to DATE under
its day counter. Only dated curves have one.")
  (:method ((c term-structure) date)
    (signal-invalid-argument 'curve c 'dated-curve
                             (format nil "a curve with a reference date to measure ~A from" date))))

(defgeneric curve-reference-date (curve)
  (:documentation "The date a dated curve's discount factors are measured from."))

(defgeneric curve-dividend-discount (curve horizon)
  (:documentation "exp(-integral of the dividend yield to HORIZON)."))

(defgeneric curve-dividend-yield (curve horizon)
  (:documentation "Continuously compounded average dividend yield to HORIZON.")
  (:method ((c dividend-curve) horizon)
    (if (zerop horizon)
        (curve-dividend-yield c 1d-6)
        (/ (- (log (curve-dividend-discount c horizon))) horizon))))

(defgeneric structure-black-variance (structure strike horizon &optional forward)
  (:documentation "Total Black variance for STRIKE to HORIZON. FORWARD, the
forward to HORIZON, is what a surface in moneyness needs; a market passes
it."))

(defgeneric structure-black-vol (structure strike horizon &optional forward)
  (:documentation "Black volatility for STRIKE to HORIZON (FORWARD as for
STRUCTURE-BLACK-VARIANCE).")
  (:method ((s black-vol-structure) strike horizon &optional forward)
    (if (zerop horizon)
        (structure-black-vol s strike 1d-6 forward)
        (sqrt (/ (structure-black-variance s strike horizon forward) horizon)))))

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

(defgeneric bump-structure-quote (structure selector shift)
  (:documentation "A new structure with the quote of one input instrument,
chosen by SELECTOR (a helper's tenor or index), moved by SHIFT and the
structure rebuilt. Only bootstrapped curves have quotes.")
  (:method (structure selector shift)
    (declare (ignore selector shift))
    (signal-invalid-argument 'structure structure 'piecewise-yield-curve
                             "a bootstrapped curve, whose input quotes can move")))

(defgeneric bump-structure (structure shift)
  (:documentation "A new structure with its level moved by SHIFT ((:relative
x) or (:absolute x)): rates for curves, the vol for vol structures.
STRUCTURE is unchanged.

  (bump-structure (make-instance 'flat-black-vol :vol 0.2d0) '(:absolute 0.01d0))
  => a flat 21% vol"))
