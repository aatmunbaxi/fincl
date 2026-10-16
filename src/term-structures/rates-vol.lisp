;;;; term-structures/rates-vol.lisp --- volatilities of interest rates
;;;;
;;;; A rates vol structure gives the total variance of a rate (a caplet's
;;;; fixing, a swap rate) from a reference date to an option's expiry date,
;;;; under its own day counter. The engine reading it decides what kind of
;;;; vol it is: lognormal (possibly shifted) for BLACK-RATES-ENGINE, normal
;;;; for BACHELIER-RATES-ENGINE. Only expiry matters: no smile, no swap
;;;; tenor (SABR and cubes are later work).

(in-package #:fincl)

(defclass rates-vol-structure (term-structure)
  ((day-counter :initarg :day-counter :initform +actual-365-fixed+ :reader rates-vol-day-counter
                :documentation "Measures the time from the reference date to an expiry."))
  (:documentation "Volatility of a rate as a function of the option's
expiry: RATES-VOL-VARIANCE."))

(defgeneric rates-vol-variance (structure reference-date date)
  (:documentation "Total variance (vol squared times time) of the rate from
REFERENCE-DATE to the expiry DATE.

  (rates-vol-variance (make-instance 'flat-rates-vol :vol 0.2d0) today #D\"2027-09-24\")"))

(defun rates-vol-time (structure reference-date date)
  (year-fraction (rates-vol-day-counter structure) reference-date date))

(defclass flat-rates-vol (rates-vol-structure)
  ((vol :initarg :vol :reader rates-vol :type double-float))
  (:documentation "One volatility for every expiry (QuantLib's constant
optionlet and swaption volatilities).

  (make-instance 'flat-rates-vol :vol 0.25d0)"))

(defmethod initialize-instance :after ((s flat-rates-vol) &key)
  (check-input (slot-value s 'vol) (real 0) "a non-negative volatility")
  (setf (slot-value s 'vol) (float (slot-value s 'vol) 1d0)))

(defmethod rates-vol-variance ((s flat-rates-vol) reference-date date)
  (let ((vol (rates-vol s)))
    (* vol vol (rates-vol-time s reference-date date))))

(defclass expiry-rates-vol (rates-vol-structure)
  ((dates :initarg :dates :reader rates-vol-dates)
   (vols :initarg :vols :reader rates-vol-vols))
  (:documentation "Volatilities by expiry date: total variance is linear in
time between the knots and from zero at the reference date, and the vol is
flat beyond the last date (QuantLib's CapletVarianceCurve).

  (make-instance 'expiry-rates-vol :dates (list #D\"2027-03-24\" #D\"2027-09-24\")
                                   :vols '(0.32d0 0.28d0))"))

(defmethod initialize-instance :after ((s expiry-rates-vol) &key)
  (let ((dates (slot-value s 'dates)) (vols (slot-value s 'vols)))
    (check-input dates (and list (not null)) "a non-empty list of expiry dates")
    (unless (and (= (length dates) (length vols)) (every #'realp vols) (every (lambda (v) (>= v 0)) vols))
      (signal-invalid-argument 'vols vols 'list "one non-negative volatility per date"))
    (setf (slot-value s 'vols) (mapcar (lambda (v) (float v 1d0)) vols))))

(defmethod rates-vol-variance ((s expiry-rates-vol) reference-date date)
  (let* ((times (mapcar (lambda (d) (rates-vol-time s reference-date d)) (rates-vol-dates s)))
         (variances (mapcar (lambda (time vol) (* time vol vol)) times (rates-vol-vols s)))
         (time (rates-vol-time s reference-date date)))
    (unless (every #'< (cons 0d0 times) times)
      (signal-invalid-argument 'dates (rates-vol-dates s) 'list
                               (format nil "increasing expiry dates after ~A" reference-date)))
    (if (<= time 0d0)
        0d0
        (let* ((variance
                 (if (<= time (a:lastcar times))
                     ;; QuantLib's LinearInterpolation: the segment starting
                     ;; at or before TIME, the slope taken first.
                     (iter (for (t0 t1 . more) on (cons 0d0 times))
                           (for (v0 v1) on (cons 0d0 variances))
                           (when (or (< time t1) (null more))
                             (return (max 0d0 (+ v0 (* (- time t0) (/ (- v1 v0) (- t1 t0))))))))
                     (/ (* (a:lastcar variances) time) (a:lastcar times))))
               ;; Read back through the vol, as QuantLib does.
               (vol (sqrt (/ variance time))))
          (* vol vol time)))))

(defun ensure-rates-vol (volatility)
  "VOLATILITY as a rates vol structure: a structure, or a real for a flat one."
  (etypecase volatility
    (rates-vol-structure volatility)
    (real (make-instance 'flat-rates-vol :vol volatility))))
