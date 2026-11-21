;;;; term-structures/protocol.lisp --- curves and vol structures
;;;;
;;;; A term structure answers one question as a function of a horizon. Examples: how much a
;;;; unit of currency is worth today (a yield curve), how much of the value of the
;;;; underlying is paid away as dividends (a dividend curve), and how much total Black
;;;; variance there is (a vol structure).
;;;;
;;;; Flat structures know no dates. Their horizons are reals in the market time of the
;;;; market that holds them. A DATED-CURVE (yield-curves.lisp) has a reference date and a
;;;; day counter of its own. It takes dates, or reals in its own time. A market view adapts
;;;; it to market time (CURVE-IN-MARKET).
;;;;
;;;; Each protocol has a primitive (a discount factor, a total variance) and a derived
;;;; quantity with a default that is built on it (a zero rate, a vol). A flat structure
;;;; overrides the derived quantity with its constant. Thus a flat market computes exactly
;;;; what a flat Black-Scholes market computed.

(in-package #:fincl)

(defclass term-structure () ()
  (:documentation "A market quantity as a function of the horizon."))

(defclass yield-curve (term-structure) ()
  (:documentation "Discount factors: CURVE-DISCOUNT. CURVE-ZERO-RATE is derived from it."))

(defclass dividend-curve (term-structure) ()
  (:documentation "The fraction of the value of the underlying that is not paid away as dividends:
CURVE-DIVIDEND-DISCOUNT. CURVE-DIVIDEND-YIELD is derived from it."))

(defclass black-vol-structure (term-structure) ()
  (:documentation "Total Black variance by strike and horizon: STRUCTURE-BLACK-VARIANCE. STRUCTURE-BLACK-VOL is
derived from it."))

(defgeneric curve-discount (curve x)
  (:documentation "Return the discount factor to X: a horizon (a real, in the time of the curve), or a date for
a curve with a reference date.

  (curve-discount (make-flat-yield-curve :rate 0.05d0) 1d0) => 0.951229..."))

(defmethod curve-discount ((c yield-curve) (x date))
  (signal-invalid-argument 'x x 'real
                           (format nil "a horizon: ~A has no reference date to measure a date from"
                                   (class-name (class-of c)))))

(defparameter *rate-dt* 1d-4
  "The short time over which a zero rate at the reference date, or an
instantaneous forward, is measured.")

(defgeneric curve-zero-rate (curve x &optional compounding frequency day-counter)
  (:documentation "Return the zero rate to X (a horizon or, for a dated curve, a date) as a double-float. The
rate is quoted with COMPOUNDING (default :CONTINUOUS) and FREQUENCY (default :ANNUAL).
DAY-COUNTER measures time (default: the day counter of the curve) from the reference date to
a date X.

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
  (:documentation "Return the forward rate from X1 to X2 (horizons, or dates for a dated curve). The rate is
quoted with COMPOUNDING (default :CONTINUOUS) and FREQUENCY (default :ANNUAL) over the
fraction of DAY-COUNTER between the dates (default: the day counter of the curve). Equal X1
and X2 give the instantaneous forward, measured over *RATE-DT*."))

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
  (:documentation "Return the day counter that measures the time of the curve. For a dated curve, it is the day
counter of the curve. For an undated curve, it is ACT/365F, nominally.")
  (:method ((c term-structure)) +actual-365-fixed+))

(defgeneric curve-time (curve date)
  (:documentation "Return the year fraction from the reference date of CURVE to DATE under its day counter.
Only dated curves have a reference date.")
  (:method ((c term-structure) date)
    (signal-invalid-argument 'curve c 'dated-curve
                             (format nil "a curve with a reference date to measure ~A from" date))))

(defgeneric curve-reference-date (curve)
  (:documentation "Return the date from which the discount factors of a dated curve are measured."))

(defgeneric curve-dividend-discount (curve horizon)
  (:documentation "Return exp(-integral of the dividend yield to HORIZON)."))

(defgeneric curve-dividend-yield (curve horizon)
  (:documentation "Return the continuously compounded average dividend yield to HORIZON.")
  (:method ((c dividend-curve) horizon)
    (if (zerop horizon)
        (curve-dividend-yield c 1d-6)
        (/ (- (log (curve-dividend-discount c horizon))) horizon))))

(defgeneric structure-black-variance (structure strike horizon &optional forward)
  (:documentation "Return the total Black variance for STRIKE to HORIZON. FORWARD is the forward to HORIZON. A
surface in moneyness needs it. A market passes it."))

(defgeneric structure-black-vol (structure strike horizon &optional forward)
  (:documentation "Return the Black volatility for STRIKE to HORIZON (FORWARD is as for
STRUCTURE-BLACK-VARIANCE).")
  (:method ((s black-vol-structure) strike horizon &optional forward)
    (if (zerop horizon)
        (structure-black-vol s strike 1d-6 forward)
        (sqrt (/ (structure-black-variance s strike horizon forward) horizon)))))

;;; --------------------------------------------------------------------
;;; Shifts, for risk-factor bumps
;;; --------------------------------------------------------------------

(deftype risk-factor ()
  "A named risk factor: a keyword kind and its key, for example (:spot :spx),
(:curve :usd-ois) or (:quote :usd-ois #T\"5Y\"). The registry of market component
kinds decides which kinds are valid. The type checks only the shape."
  '(cons keyword list))

(deftype shift ()
  "(:relative x) multiplies by 1 + x. (:absolute x) adds x."
  '(cons (member :relative :absolute) (cons real null)))

(declaim (ftype (function (real t) (values real &optional)) apply-shift))
(defun apply-shift (value shift)
  "Return VALUE moved by SHIFT, a (:relative x) or (:absolute x) list.

  (apply-shift 100d0 '(:relative 1/100)) => 101.0d0"
  (check-input shift shift "(:relative x) or (:absolute x)")
  (destructuring-bind (kind amount) shift
    (ecase kind
      (:relative (* value (+ 1 (float amount 1d0))))
      (:absolute (+ value (float amount 1d0))))))

(defgeneric bump-structure-quote (structure selector shift)
  (:documentation "Return a new structure. The quote of one input instrument, chosen by SELECTOR (the tenor or
the index of a helper), moves by SHIFT. The function rebuilds the structure. Only bootstrapped
curves have quotes.")
  (:method (structure selector shift)
    (declare (ignore selector shift))
    (signal-invalid-argument 'structure structure 'piecewise-yield-curve
                             "a bootstrapped curve, whose input quotes can move")))

(define-condition no-bump-structure (invalid-argument) ()
  (:documentation "BUMP-STRUCTURE was called on an object with no method."))

(defgeneric bump-structure (structure shift)
  (:documentation "Return a new structure with its level moved by SHIFT ((:relative x) or (:absolute x)). The
level is the rates for curves and the vol for vol structures. STRUCTURE does not change. An
object with no method signals INVALID-ARGUMENT.

  (bump-structure (make-flat-black-vol :vol 0.2d0) '(:absolute 0.01d0))
  => a flat 21% vol")
  (:method (structure shift)
    (declare (ignore shift))
    (error 'no-bump-structure :place 'structure :datum structure :expected-type 'term-structure
                              :expected "a structure with a BUMP-STRUCTURE method")))
