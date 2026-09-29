;;;; time/tenor.lisp --- periods such as 3M or 10D

(in-package #:fincl)

(define-condition invalid-tenor (error)
  ((description :initarg :description :reader invalid-tenor-description))
  (:report (lambda (c s) (write-string (invalid-tenor-description c) s))))

(deftype tenor-unit () '(member :days :weeks :months :years))

(defstruct (tenor (:constructor %tenor (n unit))
                  (:copier nil)
                  (:predicate tenorp))
  "A length of time in whole days, weeks, months or years. Immutable; two
tenors are EQUALP when they have the same N and UNIT.

  (make-tenor 3 :months) => #T\"3M\""
  (n 0 :type integer :read-only t)
  (unit :days :type tenor-unit :read-only t))

(defun make-tenor (n unit)
  "Return a tenor of N UNITs. UNIT is :DAYS, :WEEKS, :MONTHS or :YEARS.

  (make-tenor 10 :days) => #T\"10D\""
  (check-type n integer)
  (check-type unit tenor-unit)
  (%tenor n unit))

(defmethod make-load-form ((p tenor) &optional environment)
  (declare (ignore environment))
  `(make-tenor ,(tenor-n p) ,(tenor-unit p)))

(defun unit-letter (unit)
  (ecase unit (:days #\D) (:weeks #\W) (:months #\M) (:years #\Y)))

(defmethod print-object ((p tenor) stream)
  (format stream "#T\"~D~C\"" (tenor-n p) (unit-letter (tenor-unit p))))

(defun parse-tenor (string)
  "Parse an optionally signed integer followed by D, W, M or Y (either
case). Signals INVALID-TENOR otherwise.

  (parse-tenor \"3M\") => #T\"3M\""
  (let* ((len (if (stringp string) (length string) 0))
         (start (if (and (> len 0) (char= (char string 0) #\-)) 1 0))
         (unit (and (> len (1+ start))
                    (case (char-upcase (char string (1- len)))
                      (#\D :days) (#\W :weeks) (#\M :months) (#\Y :years)))))
    (unless (and unit
                 (loop for i from start below (1- len)
                       always (char<= #\0 (char string i) #\9)))
      (error 'invalid-tenor
             :description (format nil "~S is not a tenor such as \"10D\", \"2W\", \"3M\" or \"1Y\"."
                                  string)))
    (make-tenor (parse-integer string :end (1- len)) unit)))

(defun add-months (date months end-of-month)
  (multiple-value-bind (y m d) (civil-from-days (date-serial date))
    (multiple-value-bind (dy m0) (floor (+ (1- m) months) 12)
      (let* ((ny (+ y dy))
             (nm (1+ m0))
             (last (days-in-month ny nm)))
        (make-date ny nm (if (and end-of-month (= d (days-in-month y m)))
                             last
                             (min d last)))))))

(defun add-tenor (date tenor &key end-of-month)
  "The date TENOR after DATE, with no business-day adjustment. Months and
years clamp the day to the target month's length. With END-OF-MONTH true
and DATE on the last day of its month, the result is the last day of the
target month.

  (add-tenor #D\"2027-01-31\" #T\"1M\") => #D\"2027-02-28\"
  (add-tenor #D\"2027-02-28\" #T\"1M\" :end-of-month t) => #D\"2027-03-31\""
  (let ((n (tenor-n tenor)))
    (ecase (tenor-unit tenor)
      (:days (date+ date n))
      (:weeks (date+ date (* 7 n)))
      (:months (add-months date n end-of-month))
      (:years (add-months date (* 12 n) end-of-month)))))
