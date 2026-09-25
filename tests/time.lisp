;;;; time.lisp --- dates, tenors, calendars and day counters

(in-package #:fincl/tests)

(defun d (string) (parse-date string))

;;; --------------------------------------------------------------------
;;; Dates
;;; --------------------------------------------------------------------

(defun oracle-serial (y m d)
  "Days since 1970-01-01 from CL's own calendar, independent of fincl."
  (- (floor (encode-universal-time 0 0 0 d m y 0) 86400) 25567))

(defun oracle-day-of-week (y m d)
  (1+ (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 d m y 0) 0))))

(defun check-date-round-trips ()
  (let* ((first (date-serial (make-date 1900 1 1)))
         (last (date-serial (make-date 2100 12 31)))
         (epoch (make-date 1970 1 1))
         (bad '()))
    (loop for serial from first to last
          for date = (date+ epoch serial)
          for y = (date-year date) for m = (date-month date) for dd = (date-day date)
          unless (and (= (date-serial (make-date y m dd)) serial)
                      (= (oracle-serial y m dd) serial)
                      (= (oracle-day-of-week y m dd) (day-of-week date))
                      (date= (parse-date (format nil "~4,'0D-~2,'0D-~2,'0D" y m dd))
                             date))
            do (push date bad))
    (check-equal "days from 1900-01-01 to 2100-12-31" 73414 (1+ (- last first)))
    (check-that (format nil "date round trips, 1900-01-01..2100-12-31~@[ (bad: ~A)~]"
                        (and bad (subseq (reverse bad) 0 (min 5 (length bad)))))
                (null bad))))

(defun date-checks ()
  (format t "~&Dates~%")
  (check-date-round-trips)
  (check-equal "epoch serial" 0 (date-serial (make-date 1970 1 1)))
  (check-equal "print form" "#D\"2027-06-15\"" (prin1-to-string (d "2027-06-15")))
  (check-equal "day of week" 2 (day-of-week (d "2027-06-15")))
  (check-equal "date+" (d "2027-07-05") (date+ (d "2027-06-15") 20) :test #'date=)
  (check-equal "date-" -20 (date- (d "2027-06-15") (d "2027-07-05")))
  (check-that "date< / date<= / date="
              (and (date< (d "2027-06-15") (d "2027-06-16"))
                   (not (date< (d "2027-06-16") (d "2027-06-16")))
                   (date<= (d "2027-06-16") (d "2027-06-16"))
                   (date= (d "2027-06-16") (make-date 2027 6 16))))
  (check-that "2000-02-29 exists" (make-date 2000 2 29))
  (dolist (ymd '((2027 2 30) (2023 2 29) (1900 2 29) (2100 2 29) (2027 4 31)
                 (2027 13 1) (2027 0 10) (2027 6 0) (2027 6 32)))
    (check-error (format nil "make-date ~{~D~^-~}" ymd) (apply #'make-date ymd)
                 invalid-date))
  (dolist (s '("2027-6-15" "2027/06/15" " 2027-06-15" "20270615" "2027-06-15x"
               "2027-02-30" "" "abcd-ef-gh"))
    (check-error (format nil "parse-date ~S" s) (parse-date s) invalid-date)))

;;; --------------------------------------------------------------------
;;; Tenors
;;; --------------------------------------------------------------------

(defun tenor-checks ()
  (format t "~&Tenors~%")
  (loop for (string n unit) in '(("10D" 10 :days) ("2W" 2 :weeks) ("3M" 3 :months)
                                 ("1Y" 1 :years) ("3m" 3 :months) ("-1M" -1 :months))
        for p = (parse-tenor string)
        do (check-equal (format nil "parse-tenor ~S" string)
                        (list n unit) (list (tenor-n p) (tenor-unit p))))
  (check-equal "print form" "#T\"3M\"" (prin1-to-string (parse-tenor "3M")))
  (dolist (s '("3X" "M" "" "3" "3MM" " 3M" "3 M" "+3M"))
    (check-error (format nil "parse-tenor ~S" s) (parse-tenor s) invalid-tenor))
  (loop for (start tenor eom expected)
          in '(("2027-06-15" "10D" nil "2027-06-25")
               ("2027-06-15" "2W" nil "2027-06-29")
               ("2027-01-31" "1M" nil "2027-02-28")
               ("2027-01-31" "1M" t "2027-02-28")
               ("2027-02-28" "1M" nil "2027-03-28")
               ("2027-02-28" "1M" t "2027-03-31")
               ("2024-02-28" "1M" t "2024-03-28")
               ("2027-04-30" "1M" nil "2027-05-30")
               ("2027-04-30" "1M" t "2027-05-31")
               ("2027-03-31" "-1M" nil "2027-02-28")
               ("2027-12-31" "2M" nil "2028-02-29")
               ("2024-02-29" "1Y" nil "2025-02-28")
               ("2024-02-29" "1Y" t "2025-02-28")
               ("2023-02-28" "1Y" nil "2024-02-28")
               ("2023-02-28" "1Y" t "2024-02-29"))
        do (check-equal (format nil "~A + ~A~:[~; EOM~]" start tenor eom)
                        (d expected)
                        (add-tenor (d start) (parse-tenor tenor) :end-of-month eom)
                        :test #'date=)))

;;; --------------------------------------------------------------------
;;; Calendars
;;; --------------------------------------------------------------------

(defclass holiday-list-calendar (calendar)
  ((holidays :initarg :holidays :reader holidays))
  (:documentation "Weekends plus an explicit list of holidays."))

(defmethod business-day-p ((c holiday-list-calendar) date)
  (not (or (weekend-p c date)
           (member date (holidays c) :test #'date=))))

(defun calendar-checks ()
  (format t "~&Calendars~%")
  (let* ((weekends (make-instance 'weekends-only))
         (null-cal (make-instance 'null-calendar))
         (tuesday (d "2027-06-15"))
         (listed (make-instance 'holiday-list-calendar :holidays (list tuesday)))
         (join-holidays (make-instance 'joint-calendar
                                       :calendars (list weekends listed)))
         (join-business (make-instance 'joint-calendar
                                       :calendars (list null-cal weekends)
                                       :rule :join-business-days))
         (join-business-2 (make-instance 'joint-calendar
                                         :calendars (list weekends listed)
                                         :rule :join-business-days))
         (saturday (d "2027-06-19")))
    (check-that "weekends-only: Saturday is a holiday, Tuesday is not"
                (and (holiday-p weekends saturday) (business-day-p weekends tuesday)))
    (check-that "null calendar: Saturday is a business day"
                (and (business-day-p null-cal saturday) (not (weekend-p null-cal saturday))))
    (check-that "join holidays: listed holiday and weekend are holidays"
                (and (holiday-p join-holidays tuesday)
                     (holiday-p join-holidays saturday)
                     (business-day-p join-holidays (d "2027-06-16"))))
    (check-that "join business days: business in any calendar"
                (and (business-day-p join-business saturday)
                     (business-day-p join-business-2 tuesday)
                     (holiday-p join-business-2 saturday)))
    (check-that "joint weekend-p: any under join-holidays, all under join-business-days"
                (and (weekend-p join-holidays saturday)
                     (not (weekend-p join-business saturday))
                     (weekend-p join-business-2 saturday)))
    (check-equal "business days [Mon, next Mon), weekends only"
                 5 (business-days-between weekends (d "2027-06-14") (d "2027-06-21")))
    (check-equal "business days [Mon, next Mon), joint"
                 4 (business-days-between join-holidays (d "2027-06-14") (d "2027-06-21")))
    (check-equal "business days reversed, joint"
                 -4 (business-days-between join-holidays (d "2027-06-21") (d "2027-06-14")))
    (check-equal "business days include-last"
                 5 (business-days-between join-holidays (d "2027-06-14") (d "2027-06-21")
                                          :include-last t))
    (loop for (date convention expected)
            in '(("2027-06-15" :following "2027-06-16")
                 ("2027-06-15" :preceding "2027-06-14")
                 ("2027-06-15" :unadjusted "2027-06-15")
                 ("2027-06-19" :following "2027-06-21")
                 ("2027-07-31" :following "2027-08-02")
                 ("2027-07-31" :modified-following "2027-07-30")
                 ("2027-05-01" :preceding "2027-04-30")
                 ("2027-05-01" :modified-preceding "2027-05-03"))
          do (check-equal (format nil "adjust ~A ~(~A~)" date convention)
                          (d expected) (adjust join-holidays (d date) convention)
                          :test #'date=))))

;;; --------------------------------------------------------------------
;;; Day counters
;;; --------------------------------------------------------------------

(defun fixture-day-counter (name)
  (cond ((string= name "actual-360") +actual-360+)
        ((string= name "actual-365-fixed") +actual-365-fixed+)
        ((string= name "thirty-360-bond-basis")
         (make-instance 'thirty-360 :variant :bond-basis))
        ((string= name "thirty-360-european")
         (make-instance 'thirty-360 :variant :european))
        ((string= name "actual-actual-isda") +actual-actual-isda+)
        ((string= name "business-252-weekends-only")
         (make-instance 'business-252 :calendar (make-instance 'weekends-only)))
        (t (error "Unknown fixture convention ~S." name))))

(defun fixture-path ()
  (asdf:system-relative-pathname "fincl" "tests/fixtures/day-counters.sexp"))

(defun read-fixtures ()
  (with-open-file (s (fixture-path))
    (let ((*read-eval* nil)
          (*package* (find-package '#:fincl/tests)))
      (loop for form = (read s nil s)
            until (eq form s)
            when (stringp (first form)) collect form))))

(defun day-counter-fixture-checks ()
  (unless (probe-file (fixture-path))
    (incf *failures*)
    (format t "~&  FAIL ~A is missing; run tests/fixtures/generate_day_counters.py~%"
            (fixture-path))
    (return-from day-counter-fixture-checks))
  (let ((results (make-hash-table :test #'equal)))
    ;; name -> (count . mismatches)
    (loop for (name start end days yf) in (read-fixtures)
          for dc = (fixture-day-counter name)
          for a = (d start) for b = (d end)
          for entry = (or (gethash name results)
                          (setf (gethash name results) (list 0)))
          do (incf (car entry))
             (let ((got-days (day-count dc a b))
                   (got-yf (year-fraction dc a b)))
               (unless (and (eql got-days days)
                            (typep got-yf 'double-float)
                            (<= (abs (- got-yf yf)) 1d-15))
                 (push (list start end days got-days yf got-yf) (cdr entry)))))
    (loop for name being the hash-keys of results using (hash-value entry)
          for (count . bad) = entry
          do (check-that (format nil "~A: ~D QuantLib fixtures~@[, mismatches ~S~]"
                                 name count
                                 (and bad (subseq bad 0 (min 3 (length bad)))))
                         (null bad)))))

(defun day-counter-checks ()
  (format t "~&Day counters~%")
  (day-counter-fixture-checks)
  (let ((pairs (mapcar (lambda (p) (mapcar #'d p))
                       '(("2027-01-31" "2027-03-31") ("2024-02-29" "2029-08-31")
                         ("2023-12-31" "2024-12-31") ("2027-06-15" "2027-06-19")))))
    (dolist (name '("actual-360" "actual-365-fixed" "thirty-360-bond-basis"
                    "thirty-360-european" "actual-actual-isda"
                    "business-252-weekends-only"))
      (let ((dc (fixture-day-counter name)))
        (check-that (format nil "~A antisymmetric" name)
                    (loop for (a b) in pairs
                          always (and (= (day-count dc a b) (- (day-count dc b a)))
                                      (= (year-fraction dc a b)
                                         (- (year-fraction dc b a)))))))))
  (check-equal "ACT/365F one year" 1d0
               (year-fraction +actual-365-fixed+ (d "2027-01-01") (d "2028-01-01"))))

(defun time-checks ()
  (date-checks)
  (tenor-checks)
  (calendar-checks)
  (day-counter-checks))

(defun run-time-tests ()
  "Run only the time-layer checks. Returns T when all pass."
  (let ((*failures* 0))
    (time-checks)
    (format t "~&~[All time checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
