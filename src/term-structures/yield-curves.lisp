;;;; term-structures/yield-curves.lisp --- dated yield curves
;;;;
;;;; A DATED-CURVE has a reference date and a day counter of its own, which
;;;; may differ from a market's. It answers dates, or reals in its own time
;;;; (the year fraction from its reference date). The interpolated curves
;;;; follow QuantLib's InterpolatedDiscountCurve, InterpolatedZeroCurve and
;;;; InterpolatedForwardCurve: times are the day counter's fractions from the
;;;; first date, which is the reference date, and beyond the last date each
;;;; extrapolates with a flat instantaneous forward.
;;;;
;;;; A market measures horizons in market time from its valuation date, so a
;;;; view takes a dated curve through CURVE-IN-MARKET: an adapter that maps a
;;;; market horizon to the curve's time and divides by the discount factor at
;;;; the valuation date, unless the curve already shares the market's
;;;; reference date and day counter.

(in-package #:fincl)

(defclass dated-curve (yield-curve)
  ((reference-date :initarg :reference-date :reader curve-reference-date :type date)
   (day-counter :initarg :day-counter :reader curve-day-counter :type day-counter
                :initform +actual-365-fixed+))
  (:documentation "A yield curve measured from REFERENCE-DATE under DAY-COUNTER."))

(defmethod curve-time ((c dated-curve) date)
  (year-fraction (curve-day-counter c) (curve-reference-date c) date))

(defmethod curve-discount ((c dated-curve) (x date))
  (curve-discount c (curve-time c x)))

(defun check-curve-time (curve time)
  (when (minusp time)
    (signal-invalid-argument 'time time 'real
                             (format nil "a time at or after ~A's reference date ~A"
                                     (class-name (class-of curve)) (curve-reference-date curve)))))

;;; --------------------------------------------------------------------
;;; Interpolated curves
;;; --------------------------------------------------------------------

(defclass interpolated-curve (dated-curve)
  ((dates :initarg :dates :reader curve-dates :type simple-vector)
   (times :reader curve-times :type double-vector)
   (knot-values :reader curve-values :type double-vector)
   (interpolation :initarg :interpolation :reader curve-interpolation :type keyword)
   (interpolator :reader curve-interpolator))
  (:documentation "A dated curve through knots: DATES (the first is the
reference date) and one value per date, interpolated in time."))

(defun knot-index (times time)
  "The index of TIME in TIMES if it is exactly a knot, else NIL."
  (declare (type double-vector times) (double-float time))
  (let ((lo 0) (hi (1- (length times))))
    (iter (while (<= lo hi))
          (let ((mid (floor (+ lo hi) 2)))
            (cond ((= (aref times mid) time) (return-from knot-index mid))
                  ((< (aref times mid) time) (setf lo (1+ mid)))
                  (t (setf hi (1- mid))))))
    nil))

(defun prepare-knots (curve dates values what)
  "Check DATES and VALUES and set CURVE's reference date, times and values."
  (check-input dates (and sequence (not null)) "a non-empty sequence of dates")
  (let ((dates (coerce dates 'simple-vector))
        (values (map 'double-vector (lambda (v) (float v 1d0)) values)))
    (unless (every #'datep dates)
      (signal-invalid-argument 'dates dates 'sequence "a sequence of dates"))
    (unless (= (length dates) (length values))
      (signal-invalid-argument what values 'sequence (format nil "one value per date (~D)" (length dates))))
    (iter (for (a b) on (coerce dates 'list)) (while b)
          (unless (date< a b)
            (signal-invalid-argument 'dates dates 'sequence "strictly increasing dates")))
    (setf (slot-value curve 'dates) dates
          (slot-value curve 'knot-values) values
          (slot-value curve 'reference-date) (svref dates 0)
          (slot-value curve 'times)
          (map 'double-vector (lambda (d) (year-fraction (curve-day-counter curve) (svref dates 0) d))
               dates))))

(defun curve-interpolate (curve time)
  "The interpolated value at TIME within the knots, exact at a knot."
  (let ((i (knot-index (curve-times curve) time)))
    (if i
        (aref (curve-values curve) i)
        (funcall (curve-interpolator curve) time))))

(defun last-time (curve) (a:last-elt (curve-times curve)))
(defun last-value (curve) (a:last-elt (curve-values curve)))

;;; --------------------------------------------------------------------
;;; Discount factors
;;; --------------------------------------------------------------------

(defclass interpolated-discount-curve (interpolated-curve) ()
  (:default-initargs :interpolation :log-linear)
  (:documentation "Discount factors at dates (the first 1, at the reference
date), interpolated by INTERPOLATION (default :LOG-LINEAR, which gives
piecewise-flat forwards).

  (make-instance 'interpolated-discount-curve :dates dates :discounts dfs
                                              :day-counter +actual-360+)"))

(defmethod initialize-instance :after ((c interpolated-discount-curve) &key dates discounts)
  (prepare-knots c dates discounts 'discounts)
  (unless (= (aref (curve-values c) 0) 1d0)
    (signal-invalid-argument 'discounts discounts 'sequence "discount factors starting at 1"))
  (unless (every #'plusp (curve-values c))
    (signal-invalid-argument 'discounts discounts 'sequence "positive discount factors"))
  (setf (slot-value c 'interpolator)
        (make-interpolator (curve-interpolation c) (curve-times c) (curve-values c))))

(defmethod curve-discount ((c interpolated-discount-curve) (x real))
  (let ((time (float x 1d0)))
    (check-curve-time c time)
    (if (<= time (last-time c))
        (curve-interpolate c time)
        ;; Flat instantaneous forward beyond the last date.
        (let* ((d-max (last-value c))
               (forward (-> c
                            curve-interpolator
                            (interpolator-derivative (last-time c))
                            (/ d-max)
                            -)))
          (* d-max (exp (- (* forward (- time (last-time c))))))))))

(defmethod bump-structure ((c interpolated-discount-curve) shift)
  ;; Zero rates at the knots move by SHIFT: e^(-z t) -> e^(-z' t).
  (make-instance 'interpolated-discount-curve
                 :dates (curve-dates c) :day-counter (curve-day-counter c)
                 :interpolation (curve-interpolation c)
                 :discounts (map 'list (lambda (time df)
                                         (if (zerop time)
                                             1d0
                                             (-<> df
                                                  log
                                                  -
                                                  (/ <> time)
                                                  (apply-shift <> shift)
                                                  (* <> time)
                                                  -
                                                  exp)))
                                 (curve-times c) (curve-values c))))

;;; --------------------------------------------------------------------
;;; Zero rates
;;; --------------------------------------------------------------------

(defclass interpolated-zero-curve (interpolated-curve) ()
  (:default-initargs :interpolation :linear)
  (:documentation "Zero rates at dates, interpolated by INTERPOLATION
(default :LINEAR). Rates quoted with another COMPOUNDING and FREQUENCY are
converted to continuous compounding at construction, the first over one
day, as QuantLib does.

  (make-instance 'interpolated-zero-curve :dates dates :zero-rates zs
                                          :compounding :compounded :frequency :annual)"))

(defmethod initialize-instance :after ((c interpolated-zero-curve)
                                       &key dates zero-rates (compounding :continuous)
                                            (frequency :annual))
  (prepare-knots c dates zero-rates 'zero-rates)
  (unless (eq compounding :continuous)
    (let ((values (curve-values c)) (dc (curve-day-counter c)))
      (dotimes (i (length values))
        (setf (aref values i)
              (interest-rate-rate
               (equivalent-rate (make-interest-rate (aref values i) :day-counter dc
                                                                    :compounding compounding
                                                                    :frequency frequency)
                                :continuous :no-frequency
                                (if (zerop i) (/ 1d0 365) (aref (curve-times c) i)) nil))))))
  (setf (slot-value c 'interpolator)
        (make-interpolator (curve-interpolation c) (curve-times c) (curve-values c))))

(defun interpolated-zero (c time)
  "The continuous zero rate at TIME, with flat-forward extrapolation."
  (if (<= time (last-time c))
      (curve-interpolate c time)
      (let* ((t-max (last-time c)) (z-max (last-value c))
             (forward (+ z-max (* t-max (interpolator-derivative (curve-interpolator c) t-max)))))
        (/ (+ (* z-max t-max) (* forward (- time t-max))) time))))

(defmethod curve-discount ((c interpolated-zero-curve) (x real))
  (let ((time (float x 1d0)))
    (check-curve-time c time)
    (if (zerop time) 1d0 (exp (- (* (interpolated-zero c time) time))))))

(defmethod bump-structure ((c interpolated-zero-curve) shift)
  (make-instance 'interpolated-zero-curve
                 :dates (curve-dates c) :day-counter (curve-day-counter c)
                 :interpolation (curve-interpolation c)
                 :zero-rates (map 'list (lambda (z) (apply-shift z shift)) (curve-values c))))

;;; --------------------------------------------------------------------
;;; Instantaneous forwards
;;; --------------------------------------------------------------------

(defclass interpolated-forward-curve (interpolated-curve)
  ((primitive :reader forward-primitive :type double-vector
              :documentation "Integral of the forward from 0 to each knot.")
   (integral :reader forward-integral-function
             :documentation "For :MONOTONE-CONVEX, the integral of the forward from 0."))
  (:default-initargs :interpolation :backward-flat)
  (:documentation "Instantaneous forward rates at dates. INTERPOLATION is
:BACKWARD-FLAT (default: the rate at a date holds over the period ending
there), :LINEAR, or :MONOTONE-CONVEX (Hagan-West, as QuantLib's
ConvexMonotone: the value at a date is the average forward over the period
ending there, the first is ignored). The discount factor is exp(-integral
of the forward); beyond the last date the forward is the last value.

  (make-instance 'interpolated-forward-curve :dates dates :forwards fs)"))

(defmethod initialize-instance :after ((c interpolated-forward-curve) &key dates forwards)
  (prepare-knots c dates forwards 'forwards)
  (check-input (slot-value c 'interpolation) (member :backward-flat :linear :monotone-convex)
               ":BACKWARD-FLAT, :LINEAR or :MONOTONE-CONVEX")
  (let* ((times (curve-times c)) (values (curve-values c))
         (primitive (make-array (length times) :element-type 'double-float :initial-element 0d0)))
    (iter (for i from 1 below (length times))
          (for dx = (- (aref times i) (aref times (1- i))))
          (setf (aref primitive i)
                (+ (aref primitive (1- i))
                   (* dx (ecase (curve-interpolation c)
                           ((:backward-flat :monotone-convex) (aref values i))
                           (:linear (* 0.5d0 (+ (aref values i) (aref values (1- i))))))))))
    (setf (slot-value c 'primitive) primitive)
    (ecase (curve-interpolation c)
      (:linear (setf (slot-value c 'interpolator) (make-interpolator :linear times values)))
      (:backward-flat (setf (slot-value c 'interpolator)
                            (lambda (x) (backward-flat-value times values x))))
      (:monotone-convex
       (multiple-value-bind (forward integral) (make-monotone-convex times values)
         (setf (slot-value c 'interpolator) forward
               (slot-value c 'integral) integral))))))

(defun backward-flat-value (times values x)
  (if (or (<= x (aref times 0)) (= (length times) 1))
      (aref values 0)
      (let ((i (knot-segment times x)))
        (if (= x (aref times i)) (aref values i) (aref values (1+ i))))))

(defun forward-integral (c time)
  "Integral of the forward from 0 to TIME, flat beyond the last date."
  (let* ((times (curve-times c)) (values (curve-values c)) (t-max (last-time c)))
    (cond
      ((> time t-max)
       (+ (if (eq (curve-interpolation c) :monotone-convex)
              (funcall (forward-integral-function c) t-max)
              (a:last-elt (forward-primitive c)))
          (* (last-value c) (- time t-max))))
      ((eq (curve-interpolation c) :monotone-convex)
       (funcall (forward-integral-function c) time))
      (t
        (let* ((i (knot-segment times time))
               (dx (- time (aref times i))))
          (+ (aref (forward-primitive c) i)
             (ecase (curve-interpolation c)
               (:backward-flat (* dx (aref values (1+ i))))
               (:linear (let* ((h (- (aref times (1+ i)) (aref times i)))
                               (slope (/ (- (aref values (1+ i)) (aref values i)) h)))
                          (* dx (+ (aref values i) (* 0.5d0 slope dx))))))))))))

(defmethod curve-discount ((c interpolated-forward-curve) (x real))
  (let ((time (float x 1d0)))
    (check-curve-time c time)
    ;; Through the zero rate, integral / t, as QuantLib does: the same bits.
    (if (zerop time) 1d0 (exp (- (* (/ (forward-integral c time) time) time))))))

(defmethod bump-structure ((c interpolated-forward-curve) shift)
  (make-instance 'interpolated-forward-curve
                 :dates (curve-dates c) :day-counter (curve-day-counter c)
                 :interpolation (curve-interpolation c)
                 :forwards (map 'list (lambda (f) (apply-shift f shift)) (curve-values c))))

(defmethod print-object ((c interpolated-curve) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A..~A, ~D knots, ~(~A~)" (curve-reference-date c) (a:last-elt (curve-dates c))
            (length (curve-dates c)) (curve-interpolation c))))

;;; --------------------------------------------------------------------
;;; Dated curves in a market's time
;;; --------------------------------------------------------------------

(defclass market-time-curve (yield-curve)
  ((curve :initarg :curve :reader adapted-curve)
   (valuation-date :initarg :valuation-date :reader adapted-valuation-date)
   (day-counter :initarg :day-counter :reader adapted-day-counter)
   (base :initarg :base :reader adapted-base
         :documentation "The curve's discount factor at the valuation date."))
  (:documentation "A dated curve seen from a market: horizons in market time
from the valuation date, discount factors relative to the valuation date."))

(defgeneric curve-in-market (curve market)
  (:documentation "CURVE as a function of MARKET's time: undated structures
and dated curves already measured from the valuation date under the market's
day counter are themselves; other dated curves are adapted.")
  (:method ((c term-structure) market)
    (declare (ignore market))
    c)
  (:method ((c dated-curve) market)
    (let ((today (valuation-date market)) (dc (market-day-counter market)))
      (if (and (date= (curve-reference-date c) today)
               (eq (class-of (curve-day-counter c)) (class-of dc))
               (equalp (curve-day-counter c) dc))
          c
          (make-instance 'market-time-curve :curve c :valuation-date today :day-counter dc
                                            :base (curve-discount c today))))))

(defun market-horizon-days (day-counter valuation-date horizon)
  "(values n fraction): the market HORIZON (a year fraction under
DAY-COUNTER from VALUATION-DATE) as the whole days N after the valuation date
plus a FRACTION of the next day, linear between the year fractions of
consecutive days."
  (flet ((yf (n) (year-fraction day-counter valuation-date (date+ valuation-date n))))
    (let ((n (max 0 (floor (* horizon 365.25d0)))))
      (iter (while (and (plusp n) (> (yf n) horizon))) (decf n))
      (iter (while (<= (yf (1+ n)) horizon)) (incf n))
      (let ((lo (yf n)) (hi (yf (1+ n))))
        (values n (if (= hi lo) 0d0 (/ (- horizon lo) (- hi lo))))))))

(defmethod curve-discount ((c market-time-curve) (horizon real))
  (let* ((today (adapted-valuation-date c))
         (curve (adapted-curve c)))
    (if (zerop horizon)
        1d0
        (multiple-value-bind (n fraction) (market-horizon-days (adapted-day-counter c) today horizon)
          (let* ((t0 (curve-time curve (date+ today n)))
                 (time (if (zerop fraction)
                           t0
                           (+ t0 (* fraction (- (curve-time curve (date+ today (1+ n))) t0))))))
            (/ (curve-discount curve time) (adapted-base c)))))))

(defmethod curve-discount ((c market-time-curve) (x date))
  (/ (curve-discount (adapted-curve c) x) (adapted-base c)))
