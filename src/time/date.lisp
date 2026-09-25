;;;; time/date.lisp --- calendar dates
;;;;
;;;; A date is a serial day number: days since 1970-01-01 in the proleptic
;;;; Gregorian calendar. Conversion to and from year/month/day uses Howard
;;;; Hinnant's days_from_civil / civil_from_days, which are exact for every
;;;; date, negative serials included.

(in-package #:fincl)

(define-condition invalid-date (error)
  ((description :initarg :description :reader invalid-date-description))
  (:report (lambda (c s) (write-string (invalid-date-description c) s))))

(defclass date ()
  ((serial :initarg :serial :reader date-serial :type fixnum))
  (:documentation "A calendar date. Immutable.

  (make-date 2027 6 15) => #D\"2027-06-15\""))

(declaim (inline %date))
(defun %date (serial)
  (make-instance 'date :serial serial))

(defmethod make-load-form ((d date) &optional environment)
  (declare (ignore environment))
  `(%date ,(date-serial d)))

;;; --------------------------------------------------------------------
;;; Civil conversions (Hinnant)
;;; --------------------------------------------------------------------

(defun days-from-civil (year month day)
  "Serial day number of YEAR-MONTH-DAY. Unvalidated: out-of-range MONTH or
DAY still yields a number, which MAKE-DATE rejects by round-tripping."
  (declare (integer year month day))
  (let* ((y (if (<= month 2) (1- year) year))
         (era (floor y 400))
         (yoe (- y (* era 400)))
         (doy (+ (floor (+ (* 153 (if (> month 2) (- month 3) (+ month 9))) 2) 5)
                 (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (+ (* era 146097) doe -719468)))

(defun civil-from-days (serial)
  "Return (values year month day) for SERIAL."
  (declare (integer serial))
  (let* ((z (+ serial 719468))
         (era (floor z 146097))
         (doe (- z (* era 146097)))
         (yoe (floor (- (+ (- doe (floor doe 1460)) (floor doe 36524))
                        (floor doe 146096))
                     365))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (1+ (- doy (floor (+ (* 153 mp) 2) 5))))
         (month (if (< mp 10) (+ mp 3) (- mp 9)))
         (year (+ yoe (* era 400))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defun leap-year-p (year)
  "True if YEAR is a Gregorian leap year.

  (leap-year-p 2000) => T, (leap-year-p 2100) => NIL"
  (and (zerop (mod year 4))
       (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun days-in-month (year month)
  "Number of days in MONTH of YEAR.

  (days-in-month 2028 2) => 29"
  (if (= month 2)
      (if (leap-year-p year) 29 28)
      (aref #(31 0 31 30 31 30 31 31 30 31 30 31) (1- month))))

;;; --------------------------------------------------------------------
;;; Construction
;;; --------------------------------------------------------------------

(defun make-date (year month day)
  "Return the date YEAR-MONTH-DAY. Signals INVALID-DATE if it does not exist.

  (make-date 2027 6 15) => #D\"2027-06-15\""
  (check-type year integer)
  (check-type month integer)
  (check-type day integer)
  (let ((serial (days-from-civil year month day)))
    (multiple-value-bind (y m d) (civil-from-days serial)
      (unless (and (= y year) (= m month) (= d day))
        (error 'invalid-date
               :description (format nil "~D-~2,'0D-~2,'0D is not a valid date."
                                    year month day)))
      (%date serial))))

(defun parse-date (string)
  "Parse a strict YYYY-MM-DD string. Signals INVALID-DATE otherwise.

  (parse-date \"2027-06-15\") => #D\"2027-06-15\""
  (flet ((digits-at-p (&rest positions)
           (every (lambda (i) (char<= #\0 (char string i) #\9)) positions)))
    (unless (and (stringp string)
                 (= (length string) 10)
                 (char= (char string 4) #\-)
                 (char= (char string 7) #\-)
                 (digits-at-p 0 1 2 3 5 6 8 9))
      (error 'invalid-date
             :description (format nil "~S is not a date in YYYY-MM-DD form." string)))
    (make-date (parse-integer string :start 0 :end 4)
               (parse-integer string :start 5 :end 7)
               (parse-integer string :start 8 :end 10))))

(defmethod print-object ((d date) stream)
  (multiple-value-bind (y m dd) (civil-from-days (date-serial d))
    (format stream "#D\"~4,'0D-~2,'0D-~2,'0D\"" y m dd)))

;;; --------------------------------------------------------------------
;;; Accessors and arithmetic
;;; --------------------------------------------------------------------

(defun date-year (date) (nth-value 0 (civil-from-days (date-serial date))))
(defun date-month (date) (nth-value 1 (civil-from-days (date-serial date))))
(defun date-day (date) (nth-value 2 (civil-from-days (date-serial date))))

(defun day-of-week (date)
  "ISO day of the week: 1 = Monday ... 7 = Sunday.

  (day-of-week #D\"2027-06-15\") => 2"
  ;; 1970-01-01 was a Thursday.
  (1+ (mod (+ (date-serial date) 3) 7)))

(declaim (inline date+ date- date< date<= date=))

(defun date+ (date days)
  "The date DAYS calendar days after DATE (before, if DAYS is negative).

  (date+ #D\"2027-06-15\" 10) => #D\"2027-06-25\""
  (%date (+ (date-serial date) days)))

(defun date- (a b)
  "Calendar days from B to A, as an integer.

  (date- #D\"2027-06-25\" #D\"2027-06-15\") => 10"
  (- (date-serial a) (date-serial b)))

(defun date< (a b) (< (date-serial a) (date-serial b)))
(defun date<= (a b) (<= (date-serial a) (date-serial b)))
(defun date= (a b) (= (date-serial a) (date-serial b)))
