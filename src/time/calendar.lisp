;;;; time/calendar.lisp --- the business-day calendar protocol
;;;;
;;;; A calendar answers BUSINESS-DAY-P. All other functions derive from it. This file
;;;; provides only calendars with no country holidays: NULL-CALENDAR, WEEKENDS-ONLY, and
;;;; JOINT-CALENDAR, which combines other calendars.

(in-package #:fincl)

(defclass calendar () ()
  (:documentation "A calendar decides which dates are business days."))

(defmethod make-load-form ((c calendar) &optional environment)
  (make-load-form-saving-slots c :environment environment))

(defgeneric business-day-p (calendar date)
  (:documentation "True if DATE is a business day in CALENDAR."))

(defgeneric weekend-p (calendar date)
  (:documentation "True if DATE falls on the weekend of CALENDAR.")
  (:method ((c calendar) date)
    (>= (day-of-week date) 6)))

(declaim (ftype (function (calendar date) (values boolean &optional)) holiday-p))
(defun holiday-p (calendar date)
  "True if DATE is not a business day in CALENDAR. Weekends count as holidays."
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
  (:documentation "Saturdays and Sundays are holidays. Nothing else is."))

(defmethod business-day-p ((c weekends-only) date)
  (not (weekend-p c date)))

(defclass joint-calendar (calendar)
  ((calendars :initarg :calendars :reader joint-calendars
              :initform (a:required-argument :calendars))
   (rule :initarg :rule :reader joint-rule :initform :join-holidays
         :type (member :join-holidays :join-business-days)))
  (:documentation "A combination of CALENDARS. Under :JOIN-HOLIDAYS a date is a business day only if it is one in
every calendar. Under :JOIN-BUSINESS-DAYS a date is a business day if it is one in any
calendar.

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

(declaim (ftype (function (calendar date &optional symbol) (values date &optional)) adjust))
(defun adjust (calendar date &optional (convention :following))
  "Move DATE onto a business day of CALENDAR. CONVENTION is :UNADJUSTED, :FOLLOWING,
:MODIFIED-FOLLOWING, :PRECEDING or :MODIFIED-PRECEDING. The modified conventions reverse
direction. They do not leave the month of DATE.

  (adjust (make-instance 'weekends-only) #D\"2027-07-31\" :modified-following)
  => #D\"2027-07-30\""
  (flet ((roll (d step)
           (iter (until (business-day-p calendar d))
                 (setf d (date+ d step)))
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

(defgeneric count-business-days (calendar from to)
  (:documentation "Return the business days of CALENDAR in [FROM, TO), with FROM <= TO. The default tests each day.
Holiday calendars use prefix counts for each year.")
  (:method ((calendar calendar) from to)
    (iter (for d in-dates from below to)
          (counting (business-day-p calendar d)))))

(declaim (ftype (function (calendar date date &key (:include-first t) (:include-last t))
                    (values integer &optional))
          business-days-between))
(defun business-days-between (calendar from to &key (include-first t) include-last)
  "Return the business days of CALENDAR between FROM and TO. By default the count includes FROM and
excludes TO ([FROM, TO)). The result is negative when TO precedes FROM.

  (business-days-between (make-instance 'weekends-only)
                         #D\"2027-06-14\" #D\"2027-06-21\") => 5"
  (let* ((lo (if (date< to from) to from))
         (hi (if (date< to from) from to))
         (count (+ (count-business-days calendar lo hi)
                   (if (and (not include-first) (date< lo hi) (business-day-p calendar lo)) -1 0)
                   (if (and include-last (business-day-p calendar hi)) 1 0))))
    (if (date< to from) (- count) count)))

(declaim (ftype (function (calendar date) (values date &optional)) calendar-end-of-month))
(defun calendar-end-of-month (calendar date)
  "Return the last business day of the month of DATE in CALENDAR."
  (multiple-value-bind (y m) (civil-from-days (date-serial date))
    (adjust calendar (make-date y m (days-in-month y m)) :preceding)))

(declaim (ftype (function (calendar date) (values boolean &optional)) calendar-end-of-month-p))
(defun calendar-end-of-month-p (calendar date)
  "True if DATE is on or after the last business day of its month."
  (not (date< date (calendar-end-of-month calendar date))))

(declaim (ftype (function (calendar date tenor &key (:convention symbol) (:end-of-month t))
                    (values date &optional))
          advance))
(defun advance (calendar date tenor &key (convention :following) end-of-month)
  "Return DATE moved by TENOR in CALENDAR. Days count business days. Weeks add calendar days, then
adjust by CONVENTION. Months and years add calendar months (the day is limited to the length of
the month), then adjust. With END-OF-MONTH, a DATE at the end of its month moves to the end of
the target month. The end of the month is the last calendar day under :UNADJUSTED, and the last
business day otherwise.

  (advance +target+ #D\"2027-06-30\" #T\"2D\") => #D\"2027-07-02\""
  (let ((n (tenor-n tenor)))
    (cond ((zerop n) (adjust calendar date convention))
          ((eq (tenor-unit tenor) :days)
           (let ((step (if (plusp n) 1 -1)) (d date))
             (iter (repeat (abs n))
                   (setf d (date+ d step))
                   (iter (until (business-day-p calendar d)) (setf d (date+ d step))))
             d))
          ((eq (tenor-unit tenor) :weeks)
           (adjust calendar (date+ date (* 7 n)) convention))
          (t
           (let ((target (add-tenor date tenor)))
             (cond ((and end-of-month (eq convention :unadjusted)
                         (= (date-day date) (days-in-month (date-year date) (date-month date))))
                    (make-date (date-year target) (date-month target)
                               (days-in-month (date-year target) (date-month target))))
                   ((and end-of-month (not (eq convention :unadjusted))
                         (calendar-end-of-month-p calendar date))
                    (calendar-end-of-month calendar target))
                   (t (adjust calendar target convention))))))))
