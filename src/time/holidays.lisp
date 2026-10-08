;;;; time/holidays.lisp --- holiday calendars from rules
;;;;
;;;; A holiday calendar is declared as data:
;;;;
;;;;   (define-calendar target (weekends-only)
;;;;     "TARGET, the euro area payment system."
;;;;     (:fixed 1 1)
;;;;     (:easter -2 :from 2000)
;;;;     ...)
;;;;
;;;; DEFINE-CALENDAR makes a class, a singleton (+TARGET+) and the class's
;;;; holiday rules. BUSINESS-DAY-P then reads a per-year bit vector, built
;;;; once per year from the weekend and the rules, and COUNT-BUSINESS-DAYS
;;;; reads per-year prefix counts, so counting over a century costs O(years).
;;;;
;;;; Calendars are shared across lparallel workers. A year is built under the
;;;; calendar's lock and installed into a simple vector by a single store
;;;; after a write barrier; reads take no lock.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Dates the rules need
;;; --------------------------------------------------------------------

(defun easter (year)
  "Easter Sunday of YEAR in the Gregorian calendar, by the anonymous
Gregorian computus (Meeus/Jones/Butcher).

  (easter 2027) => #D\"2027-03-28\""
  (let* ((a (mod year 19))
         (b (floor year 100)) (c (mod year 100))
         (d (floor b 4)) (e (mod b 4))
         (f (floor (+ b 8) 25))
         (g (floor (+ (- b f) 1) 3))
         (h (mod (+ (* 19 a) (- b d g) 15) 30))
         (i (floor c 4)) (k (mod c 4))
         (l (mod (- (+ 32 (* 2 e) (* 2 i)) h k) 7))
         (m (floor (+ a (* 11 h) (* 22 l)) 451))
         (month (floor (+ h l (* -7 m) 114) 31))
         (day (1+ (mod (+ h l (* -7 m) 114) 31))))
    (make-date year month day)))

(defparameter *weekdays* '(:monday 1 :tuesday 2 :wednesday 3 :thursday 4 :friday 5
                           :saturday 6 :sunday 7)
  "Weekday keywords and their ISO numbers (DAY-OF-WEEK).")

(defun weekday-number (weekday)
  (or (getf *weekdays* weekday)
      (signal-invalid-argument 'weekday weekday '(member :monday :tuesday :wednesday :thursday
                                                  :friday :saturday :sunday)
                               "a weekday keyword such as :MONDAY")))

(defun nth-weekday (n weekday month year)
  "The N-th WEEKDAY of MONTH in YEAR; negative N counts from the month's
end (-1 is the last). NIL if there is no such day.

  (nth-weekday 3 :monday 1 2027) => #D\"2027-01-18\"
  (nth-weekday -1 :monday 5 2027) => #D\"2027-05-31\""
  (let ((target (weekday-number weekday)))
    (if (plusp n)
        (let* ((first (make-date year month 1))
               (offset (mod (- target (day-of-week first)) 7))
               (day (+ 1 offset (* 7 (1- n)))))
          (and (<= day (days-in-month year month)) (make-date year month day)))
        (let* ((last-day (days-in-month year month))
               (last (make-date year month last-day))
               (offset (mod (- (day-of-week last) target) 7))
               (day (- last-day offset (* 7 (1- (- n))))))
          (and (>= day 1) (make-date year month day))))))

;;; --------------------------------------------------------------------
;;; Observance
;;; --------------------------------------------------------------------

(defvar *observances* (make-hash-table :test 'eq)
  "Observance keyword -> function from a holiday's date to the date it is
observed on, or NIL if it is not observed.")

(defmacro define-observance (name (date) &body body)
  "Define the observance rule NAME: BODY maps the holiday's DATE to the date
it is observed on, or NIL if it is not observed that year.

  (define-observance :sunday-to-monday (date)
    (if (= (day-of-week date) 7) (date+ date 1) date))"
  `(progn (setf (gethash ,name *observances*) (lambda (,date) ,@body))
          ,name))

(define-observance :none (date) date)

(define-observance :next-monday (date)
  ;; Saturday and Sunday move to the Monday after.
  (case (day-of-week date) (6 (date+ date 2)) (7 (date+ date 1)) (t date)))

(define-observance :nearest-weekday (date)
  ;; Saturday to the Friday before, Sunday to the Monday after.
  (case (day-of-week date) (6 (date+ date -1)) (7 (date+ date 1)) (t date)))

(define-observance :sunday-to-monday (date)
  (if (= (day-of-week date) 7) (date+ date 1) date))

(defun observe (keyword date)
  (funcall (or (gethash keyword *observances*)
               (signal-invalid-argument 'observe keyword 'keyword
                                        (format nil "an observance: ~{~S~^, ~}"
                                                (a:hash-table-keys *observances*))))
           date))

;;; --------------------------------------------------------------------
;;; Rules
;;; --------------------------------------------------------------------

(defstruct (holiday-rule (:constructor %make-holiday-rule))
  (kind nil :read-only t)            ; :fixed :nth-weekday :easter :dates :when
  (arguments '() :read-only t)
  (observe :none :read-only t)
  (from nil :read-only t)
  (to nil :read-only t)
  (except '() :read-only t))         ; years the rule does not apply

(defun make-holiday-rule (form)
  "Parse one rule FORM of DEFINE-CALENDAR (the :WHEN function already
evaluated)."
  (flet ((options (rest)
           (destructuring-bind (&key (observe :none) from to except) rest
             (values observe from to except))))
    (destructuring-bind (kind &rest rest) form
      (multiple-value-bind (arguments options)
          (ecase kind
            (:fixed (values (subseq rest 0 2) (nthcdr 2 rest)))
            (:nth-weekday (values (subseq rest 0 3) (nthcdr 3 rest)))
            (:easter (values (subseq rest 0 1) (nthcdr 1 rest)))
            (:dates (values (mapcar (lambda (d) (if (stringp d) (parse-date d) d)) rest) '()))
            (:when (values (subseq rest 0 1) (nthcdr 1 rest))))
        (multiple-value-bind (observe from to except) (options options)
          (%make-holiday-rule :kind kind :arguments arguments :observe observe
                              :from from :to to :except except))))))

(defun rule-applies-p (rule year)
  (and (or (null (holiday-rule-from rule)) (>= year (holiday-rule-from rule)))
       (or (null (holiday-rule-to rule)) (<= year (holiday-rule-to rule)))
       (not (member year (holiday-rule-except rule)))))

(defun rule-dates (rule year)
  "The dates RULE makes holidays because of YEAR's occurrence, after
observance (which can move a date into a neighbouring year)."
  (when (rule-applies-p rule year)
    (let ((arguments (holiday-rule-arguments rule)))
      (flet ((observed (date)
               (and date (let ((d (observe (holiday-rule-observe rule) date))) (and d (list d))))))
        (ecase (holiday-rule-kind rule)
          (:fixed (destructuring-bind (month day) arguments
                    (observed (make-date year month day))))
          (:nth-weekday (destructuring-bind (n weekday month) arguments
                          (observed (nth-weekday n weekday month year))))
          (:easter (observed (date+ (easter year) (first arguments))))
          (:dates (remove year arguments :key #'date-year :test-not #'=))
          (:when (let ((predicate (first arguments)))
                   (loop for serial from (date-serial (make-date year 1 1))
                           below (date-serial (make-date (1+ year) 1 1))
                         for date = (%date serial)
                         when (multiple-value-bind (y m d) (civil-from-days serial)
                                (funcall predicate y m d))
                           collect date))))))))

;;; --------------------------------------------------------------------
;;; Holiday calendars and their cache
;;; --------------------------------------------------------------------

(defconstant +first-cached-year+ 1900)
(defconstant +last-cached-year+ 2199)

(defclass holiday-calendar (calendar)
  ((years :initform (make-array (1+ (- +last-cached-year+ +first-cached-year+))
                                :initial-element nil)
          :reader calendar-years
          :documentation "Year index -> CALENDAR-YEAR, filled on first use.")
   (lock :initform (bt:make-lock "holiday calendar") :reader calendar-lock))
  (:documentation "A calendar whose holidays come from rules (DEFINE-CALENDAR),
with a per-year cache of business days."))

(defmethod make-load-form ((c holiday-calendar) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-name (class-of c))))

(defgeneric calendar-holidays (calendar year)
  (:documentation "The dates of YEAR that CALENDAR's rules make holidays
(weekends aside). A calendar defined on another holiday calendar adds its
rules to those it inherits.")
  (:method ((c holiday-calendar) year)
    (declare (ignore year))
    '()))

(defstruct (calendar-year (:constructor %make-calendar-year (closed counts)))
  "One year of a holiday calendar: CLOSED[i] is 1 when day i of the year
(0 = January 1st) is not a business day; COUNTS[i] is the number of
business days among days 0 .. i-1."
  (closed nil :type simple-bit-vector :read-only t)
  (counts nil :type (simple-array (unsigned-byte 16) (*)) :read-only t))

(defun build-calendar-year (calendar year)
  (let* ((start (date-serial (make-date year 1 1)))
         (days (- (date-serial (make-date (1+ year) 1 1)) start))
         (closed (make-array days :element-type 'bit :initial-element 0))
         (counts (make-array (1+ days) :element-type '(unsigned-byte 16))))
    (dotimes (i days)
      (when (weekend-p calendar (%date (+ start i)))
        (setf (sbit closed i) 1)))
    ;; Rules for the neighbouring years too: observance can cross January 1st.
    (dolist (y (list (1- year) year (1+ year)))
      (dolist (date (calendar-holidays calendar y))
        (let ((i (- (date-serial date) start)))
          (when (< -1 i days) (setf (sbit closed i) 1)))))
    (dotimes (i days)
      (setf (aref counts (1+ i)) (+ (aref counts i) (- 1 (sbit closed i)))))
    (%make-calendar-year closed counts)))

(defun calendar-year (calendar year)
  "CALENDAR's CALENDAR-YEAR for YEAR: cached for 1900-2199, built under the
calendar's lock on first use; outside that range built afresh."
  (if (<= +first-cached-year+ year +last-cached-year+)
      (let ((years (calendar-years calendar)) (index (- year +first-cached-year+)))
        (or (svref years index)
            (bt:with-lock-held ((calendar-lock calendar))
              (or (svref years index)
                  (let ((entry (build-calendar-year calendar year)))
                    ;; Every write that builds ENTRY lands before the store
                    ;; that publishes it.
                    #+sbcl (sb-thread:barrier (:write))
                    (setf (svref years index) entry))))))
      (build-calendar-year calendar year)))

(defun clear-calendar-cache (calendar)
  "Forget CALENDAR's cached years (after its rules are redefined)."
  (bt:with-lock-held ((calendar-lock calendar))
    (fill (calendar-years calendar) nil))
  calendar)

(defmethod business-day-p ((c holiday-calendar) date)
  (multiple-value-bind (year) (date-year date)
    (zerop (sbit (calendar-year-closed (calendar-year c year))
                 (- (date-serial date) (date-serial (make-date year 1 1)))))))

(defmethod count-business-days ((c holiday-calendar) from to)
  ;; [FROM, TO) with FROM <= TO, from the per-year prefix counts.
  (let ((y1 (date-year from)) (y2 (date-year to)))
    (flet ((index (date year) (- (date-serial date) (date-serial (make-date year 1 1))))
           (counts (year) (calendar-year-counts (calendar-year c year))))
      (if (= y1 y2)
          (- (aref (counts y1) (index to y1)) (aref (counts y1) (index from y1)))
          (+ (let ((counts (counts y1)))
               (- (aref counts (1- (length counts))) (aref counts (index from y1))))
             (loop for y from (1+ y1) below y2
                   sum (let ((counts (counts y))) (aref counts (1- (length counts)))))
             (aref (counts y2) (index to y2)))))))

;;; --------------------------------------------------------------------
;;; DEFINE-CALENDAR
;;; --------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *holiday-calendar-classes* '(holiday-calendar)
    "Names of classes made by DEFINE-CALENDAR, known at macroexpansion."))

(defmacro define-calendar (name (superclass) &body body)
  "Define the holiday calendar NAME: a class on SUPERCLASS (WEEKENDS-ONLY,
or another holiday calendar, whose rules NAME inherits), the singleton
+NAME+, and the holiday rules in BODY (after an optional docstring). Each
rule is one of

  (:fixed month day)              a date every year
  (:nth-weekday n weekday month)  e.g. 3 :monday 1; negative N counts from
                                  the month's end (-1 :monday 5: last Monday
                                  of May)
  (:easter offset)                days from Easter Sunday (-2 Good Friday,
                                  1 Easter Monday)
  (:dates \"YYYY-MM-DD\" ...)     one-off closures
  (:when predicate)               PREDICATE, evaluated, of (year month day)

and :FIXED, :NTH-WEEKDAY, :EASTER and :WHEN take the options :OBSERVE
keyword (:none, :next-monday, :nearest-weekday, :sunday-to-monday, or one
from DEFINE-OBSERVANCE), :FROM year, :TO year and :EXCEPT (years...). The
weekend comes from SUPERCLASS.

  (define-calendar my-exchange (weekends-only)
    \"An exchange closed on New Year's Day and Good Friday.\"
    (:fixed 1 1 :observe :sunday-to-monday)
    (:easter -2))"
  (let* ((documentation (and (stringp (first body)) (first body)))
         (rules (if documentation (rest body) body))
         (constant (intern (format nil "+~A+" (symbol-name name))))
         (rule-forms (mapcar (lambda (rule)
                               (if (eq (first rule) :when)
                                   `(list :when ,(second rule) ,@(cddr rule))
                                   `',rule))
                             rules))
         (supers (if (member superclass *holiday-calendar-classes*)
                     `(,superclass)
                     `(holiday-calendar ,superclass))))
    (pushnew name *holiday-calendar-classes*)
    (a:with-gensyms (calendar year own)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (pushnew ',name *holiday-calendar-classes*))
         (defclass ,name ,supers ()
           ,@(when documentation `((:documentation ,documentation))))
         (let ((,own (mapcar #'make-holiday-rule (list ,@rule-forms))))
           (defmethod calendar-holidays ((,calendar ,name) ,year)
             `(,@(loop for rule in ,own append (rule-dates rule ,year))
               ,@(call-next-method))))
         (defvar ,constant (make-instance ',name)
           ,(format nil "The ~(~A~) calendar." name))
         ;; Redefinition: drop years cached under the old rules.
         (clear-calendar-cache ,constant)
         ',name))))
