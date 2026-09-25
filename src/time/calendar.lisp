;;;; time/calendar.lisp --- the business-day calendar protocol
;;;;
;;;; A calendar answers BUSINESS-DAY-P; everything else is derived from it.
;;;; Only calendars with no country holidays are provided: NULL-CALENDAR,
;;;; WEEKENDS-ONLY, and JOINT-CALENDAR for combining others.

(in-package #:fincl)

(defclass calendar () ()
  (:documentation "Decides which dates are business days."))

(defmethod make-load-form ((c calendar) &optional environment)
  (make-load-form-saving-slots c :environment environment))

(defgeneric business-day-p (calendar date)
  (:documentation "True if DATE is a business day in CALENDAR."))

(defgeneric weekend-p (calendar date)
  (:documentation "True if DATE falls on CALENDAR's weekend.")
  (:method ((c calendar) date)
    (>= (day-of-week date) 6)))

(defun holiday-p (calendar date)
  "True if DATE is not a business day in CALENDAR, weekends included."
  (not (business-day-p calendar date)))

;;; --------------------------------------------------------------------
;;; Calendars
;;; --------------------------------------------------------------------

(defclass null-calendar (calendar) ()
  (:documentation "Every date is a business day."))

(defmethod business-day-p ((c null-calendar) date)
  (declare (ignore date))
  t)

(defmethod weekend-p ((c null-calendar) date)
  (declare (ignore date))
  nil)

(defclass weekends-only (calendar) ()
  (:documentation "Saturdays and Sundays are holidays; nothing else is."))

(defmethod business-day-p ((c weekends-only) date)
  (not (weekend-p c date)))

(defclass joint-calendar (calendar)
  ((calendars :initarg :calendars :reader joint-calendars
              :initform (a:required-argument :calendars))
   (rule :initarg :rule :reader joint-rule :initform :join-holidays
         :type (member :join-holidays :join-business-days)))
  (:documentation "Combines CALENDARS. Under :JOIN-HOLIDAYS a date is a
business day only if it is one in every calendar; under
:JOIN-BUSINESS-DAYS, if it is one in any calendar.

  (make-instance 'joint-calendar :calendars (list a b) :rule :join-holidays)"))

(defmethod business-day-p ((c joint-calendar) date)
  (let ((test (lambda (cal) (business-day-p cal date))))
    (ecase (joint-rule c)
      (:join-holidays (every test (joint-calendars c)))
      (:join-business-days (some test (joint-calendars c))))))

(defmethod weekend-p ((c joint-calendar) date)
  (let ((test (lambda (cal) (weekend-p cal date))))
    (ecase (joint-rule c)
      (:join-holidays (some test (joint-calendars c)))
      (:join-business-days (every test (joint-calendars c))))))

;;; --------------------------------------------------------------------
;;; Derived operations
;;; --------------------------------------------------------------------

(defun adjust (calendar date &optional (convention :following))
  "Move DATE onto a business day of CALENDAR. CONVENTION is :UNADJUSTED,
:FOLLOWING, :MODIFIED-FOLLOWING, :PRECEDING or :MODIFIED-PRECEDING. The
modified conventions reverse direction rather than leave DATE's month.

  (adjust (make-instance 'weekends-only) #D\"2027-07-31\" :modified-following)
  => #D\"2027-07-30\""
  (flet ((roll (d step)
           (loop until (business-day-p calendar d)
                 do (setf d (date+ d step)))
           d))
    (ecase convention
      (:unadjusted date)
      (:following (roll date 1))
      (:preceding (roll date -1))
      (:modified-following
       (let ((d (roll date 1)))
         (if (= (date-month d) (date-month date)) d (roll date -1))))
      (:modified-preceding
       (let ((d (roll date -1)))
         (if (= (date-month d) (date-month date)) d (roll date 1)))))))

(defun business-days-between (calendar from to &key (include-first t) include-last)
  "Business days of CALENDAR between FROM and TO, by default counting
[FROM, TO). Negative when TO precedes FROM, as in QuantLib.

  (business-days-between (make-instance 'weekends-only)
                         #D\"2027-06-14\" #D\"2027-06-21\") => 5"
  (let* ((lo (if (date< to from) to from))
         (hi (if (date< to from) from to))
         (count (if (and include-last (business-day-p calendar hi)) 1 0)))
    (loop for d = (if include-first lo (date+ lo 1)) then (date+ d 1)
          while (date< d hi)
          when (business-day-p calendar d) do (incf count))
    (if (date< to from) (- count) count)))
