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
