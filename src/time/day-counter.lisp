;;;; time/day-counter.lisp --- day-count conventions
;;;;
;;;; Definitions follow QuantLib; tests/fixtures/day-counters.sexp, generated
;;;; from QuantLib, is the reference. Both generics are antisymmetric: when
;;;; END precedes START the result is minus the result for (END, START).

(in-package #:fincl)

(defclass day-counter () ()
  (:documentation "A convention for counting days and year fractions."))

(defmethod make-load-form ((dc day-counter) &optional environment)
  (make-load-form-saving-slots dc :environment environment))

(defgeneric day-count (day-counter start end)
  (:documentation "Days from START to END under DAY-COUNTER, as an integer.

  (day-count +actual-360+ #D\"2027-01-01\" #D\"2027-07-01\") => 181")
  (:method ((dc day-counter) start end)
    (date- end start)))

(defgeneric year-fraction (day-counter start end &key ref-start ref-end)
  (:documentation "Years from START to END under DAY-COUNTER, as a
double-float. REF-START and REF-END are accepted for conventions that need
a reference period; none of the conventions here use them.

  (year-fraction +actual-365-fixed+ #D\"2027-01-01\" #D\"2028-01-01\") => 1.0d0"))

(defmethod day-count :around ((dc day-counter) start end)
  (if (date< end start)
      (- (call-next-method dc end start))
      (call-next-method)))

(defmethod year-fraction :around ((dc day-counter) start end &key ref-start ref-end)
  (if (date< end start)
      (- (call-next-method dc end start :ref-start ref-start :ref-end ref-end))
      (call-next-method)))

;;; --------------------------------------------------------------------
;;; ACT/360, ACT/365F
;;; --------------------------------------------------------------------

(defclass actual-360 (day-counter) ()
  (:documentation "ACT/360: actual days / 360."))

(defmethod year-fraction ((dc actual-360) start end &key ref-start ref-end)
  (declare (ignore ref-start ref-end))
  (/ (float (date- end start) 1d0) 360d0))

(defclass actual-365-fixed (day-counter) ()
  (:documentation "ACT/365F: actual days / 365."))

(defmethod year-fraction ((dc actual-365-fixed) start end &key ref-start ref-end)
  (declare (ignore ref-start ref-end))
  (/ (float (date- end start) 1d0) 365d0))

;;; --------------------------------------------------------------------
;;; 30/360
;;; --------------------------------------------------------------------

(defclass thirty-360 (day-counter)
  ((variant :initarg :variant :reader thirty-360-variant
            :initform (a:required-argument :variant)
            :type (member :bond-basis :european)))
  (:documentation "30/360. :BOND-BASIS is the US bond basis (QuantLib
Thirty360::BondBasis); :EUROPEAN is 30E/360 (Thirty360::European).

  (make-instance 'thirty-360 :variant :european)"))

(defmethod day-count ((dc thirty-360) start end)
  (multiple-value-bind (y1 m1 d1) (civil-from-days (date-serial start))
    (multiple-value-bind (y2 m2 d2) (civil-from-days (date-serial end))
      (ecase (thirty-360-variant dc)
        (:bond-basis
         (when (= d1 31) (setf d1 30))
         (when (and (= d2 31) (= d1 30)) (setf d2 30)))
        (:european
         (when (= d1 31) (setf d1 30))
         (when (= d2 31) (setf d2 30))))
      (+ (* 360 (- y2 y1)) (* 30 (- m2 m1)) (- d2 d1)))))

(defmethod year-fraction ((dc thirty-360) start end &key ref-start ref-end)
  (declare (ignore ref-start ref-end))
  (/ (float (day-count dc start end) 1d0) 360d0))

;;; --------------------------------------------------------------------
;;; ACT/ACT ISDA
;;; --------------------------------------------------------------------

(defclass actual-actual-isda (day-counter) ()
  (:documentation "ACT/ACT ISDA: days in each calendar year divided by
that year's length (365 or 366)."))

(defmethod year-fraction ((dc actual-actual-isda) start end &key ref-start ref-end)
  (declare (ignore ref-start ref-end))
  (if (date= start end)
      0d0
      ;; Same operation order as QuantLib, so results match bit for bit.
      (let* ((y1 (date-year start))
             (y2 (date-year end))
             (dib1 (if (leap-year-p y1) 366d0 365d0))
             (dib2 (if (leap-year-p y2) 366d0 365d0))
             (sum (float (- y2 y1 1) 1d0)))
        (incf sum (/ (float (date- (make-date (1+ y1) 1 1) start) 1d0) dib1))
        (incf sum (/ (float (date- end (make-date y2 1 1)) 1d0) dib2))
        sum)))

;;; --------------------------------------------------------------------
;;; BUS/252
;;; --------------------------------------------------------------------

(defclass business-252 (day-counter)
  ((calendar :initarg :calendar :reader day-counter-calendar
             :initform (a:required-argument :calendar)))
  (:documentation "BUS/252: business days of CALENDAR in [start, end) / 252.

  (make-instance 'business-252 :calendar (make-instance 'weekends-only))"))

(defmethod day-count ((dc business-252) start end)
  (business-days-between (day-counter-calendar dc) start end))

(defmethod year-fraction ((dc business-252) start end &key ref-start ref-end)
  (declare (ignore ref-start ref-end))
  (/ (float (day-count dc start end) 1d0) 252d0))

;;; --------------------------------------------------------------------
;;; Shared instances
;;; --------------------------------------------------------------------

;;; DEFVAR rather than DEFCONSTANT: a constant's value form is evaluated at
;;; compile time, before the class can be instantiated, and a recompiled
;;; instance would not be EQL to the old one. Treat these as constants.
(defvar +actual-360+ (make-instance 'actual-360))
(defvar +actual-365-fixed+ (make-instance 'actual-365-fixed))
(defvar +actual-actual-isda+ (make-instance 'actual-actual-isda))

;;; --------------------------------------------------------------------
;;; ACT/ACT ICMA
;;; --------------------------------------------------------------------

(defun lround (x)
  "C's lround: X rounded to the nearest integer, halves away from zero (CL's
ROUND rounds halves to even)."
  (if (minusp x) (- (floor (+ (- x) 1/2))) (floor (+ x 1/2))))

(defclass actual-actual-icma (day-counter)
  ((schedule :initarg :schedule :initform nil :reader day-counter-schedule))
  (:documentation "ACT/ACT ICMA (ISMA), as QuantLib's ActualActual(ISMA). With a
SCHEDULE, a period is split along the schedule's coupon dates, stubs
completed by notional coupon dates, and each piece counts its days over its
reference period's days times the coupons per year. Without one, the
reference period is REF-START to REF-END of YEAR-FRACTION (QuantLib's
original ISMA algorithm, which also splits long stubs).

  (make-instance 'actual-actual-icma :schedule (make-schedule ...))"))

(defun icma-quasi-coupon-dates (schedule)
  "The schedule's dates with each stub's end replaced by the notional coupon
date one tenor away, and a further notional date for a long stub (QuantLib's
getListOfPeriodDatesIncludingQuasiPayments, indexing quirk included)."
  (let* ((dates (coerce (schedule-dates schedule) 'list))
         (size (length dates))
         (regular (schedule-regular-p schedule))
         (calendar (schedule-calendar schedule))
         (tenor (schedule-tenor schedule))
         (back (make-tenor (- (tenor-n tenor)) (tenor-unit tenor)))
         (convention (schedule-convention schedule))
         (eom (schedule-end-of-month schedule))
         (new (copy-list dates)))
    (flet ((move (date tenor) (advance calendar date tenor :convention convention :end-of-month eom)))
      (when (zerop (sbit regular 0))
        (let ((notional (move (second dates) back)))
          (setf (first new) notional)
          (when (date< (first dates) notional)
            (push (move notional back) new))))
      (when (zerop (sbit regular (1- (length regular))))
        (let ((notional (move (nth (- size 2) dates) tenor)))
          ;; QuantLib writes position size - 1 of the list it has possibly
          ;; just extended at the front; so does this.
          (setf (nth (1- size) new) notional)
          (when (date< notional (a:lastcar dates))
            (setf new `(,@new ,(move notional tenor)))))))
    new))

(defun icma-reference-fraction (d1 d2 d3 d4)
  "Days from D1 to D2 over the reference period D3-D4's days times the
coupons per year that period implies (one, over a year from D1, below 16
days)."
  (let ((reference (float (date- d4 d3) 1d0)) (per-year 0))
    (if (< reference 16)
        (setf per-year 1
              reference (float (date- (add-tenor d1 (make-tenor 1 :years)) d1) 1d0))
        (setf per-year (-<> (* 12 reference)
                            (/ <> 365d0)
                            lround
                            (float 1d0)
                            (/ 12d0 <>)
                            lround)))
    ;; A reference period over two years implies no coupons a year, where
    ;; QuantLib divides by zero.
    (when (zerop per-year)
      (signal-invalid-argument '(- d4 d3) (date- d4 d3) '(integer 0 730)
                               "a reference period of at most two years for ICMA"))
    (/ (float (date- d2 d1) 1d0) (* reference per-year))))

(defmethod year-fraction ((dc actual-actual-icma) start end &key ref-start ref-end)
  (cond ((date= start end) 0d0)
        ((day-counter-schedule dc)
         (let* ((coupons (icma-quasi-coupon-dates (day-counter-schedule dc)))
                (first (iter (for d in coupons) (finding d minimizing (date-serial d))))
                (last (iter (for d in coupons) (finding d maximizing (date-serial d)))))
           (unless (and (not (date< start first)) (not (date< last end)))
             (signal-invalid-argument 'end end 'date
                                      (format nil "dates within the schedule's coupon dates ~A to ~A"
                                              first last)))
           (iter (for (period-start period-end) on coupons)
                 (while period-end)
                 (when (and (date< start period-end) (date< period-start end))
                   (sum (icma-reference-fraction (if (date< start period-start) period-start start)
                                                 (if (date< period-end end) period-end end)
                                                 period-start period-end))))))
        (t (icma-reference-year-fraction start end (or ref-start start) (or ref-end end)))))

(defun icma-reference-year-fraction (d1 d2 ref-start ref-end)
  "QuantLib's original ISMA algorithm (Old_ISMA_Impl) for D1 < D2."
  (cond
    ((date= d1 d2) 0d0)
    (t
     (unless (and (date< ref-start ref-end) (date< d1 ref-end))
       (signal-invalid-argument 'ref-end ref-end 'date
                                "a reference period ending after both its start and the first date"))
     (let ((months (lround (/ (* 12 (float (date- ref-end ref-start) 1d0)) 365))))
       (when (zerop months)
         (setf ref-start d1
               ref-end (add-tenor d1 (make-tenor 1 :years))
               months 12))
       (let ((period (/ (float months 1d0) 12d0)))
         (flet ((plus-months (date k) (add-tenor date (make-tenor k :months))))
           (if (not (date< ref-end d2))
               (if (not (date< d1 ref-start))
                   (/ (* period (float (date- d2 d1) 1d0)) (float (date- ref-end ref-start) 1d0))
                   ;; Long first period: split at the notional coupon.
                   (let ((previous (plus-months ref-start (- months))))
                     (if (date< ref-start d2)
                         (+ (icma-reference-year-fraction d1 ref-start previous ref-start)
                            (icma-reference-year-fraction ref-start d2 ref-start ref-end))
                         (icma-reference-year-fraction d1 d2 previous ref-start))))
               (progn
                 (unless (not (date< d1 ref-start))
                   (signal-invalid-argument 'ref-start ref-start 'date
                                            "a reference period starting on or before the first date"))
                 (let ((sum (icma-reference-year-fraction d1 ref-end ref-start ref-end))
                       (i 0) (new-start nil) (new-end nil))
                   (iter (setf new-start (plus-months ref-end (* months i))
                               new-end (plus-months ref-end (* months (1+ i))))
                         (until (date< d2 new-end))
                         (incf sum period) (incf i))
                   (+ sum (icma-reference-year-fraction new-start d2 new-start new-end)))))))))))
