;;;; calendars.lisp --- holiday rules, the cache, and QuantLib's calendars

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun calendar-fixture ()
  (with-open-file (s (asdf:system-relative-pathname "fincl" "tests/fixtures/calendars.sexp"))
    (loop for entry = (read s nil s) until (eq entry s) collect entry)))

;;; A calendar defined here, outside fincl, exercising every rule form.
(define-observance :saturday-to-friday-only (date)
  (if (= (day-of-week date) 6) (date+ date -1) date))

(define-calendar toy-holiday-calendar (weekends-only)
  "Every kind of rule, for the tests."
  (:fixed 1 1 :observe :next-monday)
  (:fixed 7 4 :observe :saturday-to-friday-only)
  (:nth-weekday -2 :friday 6)                  ; second-to-last Friday of June
  (:nth-weekday 1 :wednesday 3 :from 2030)
  (:easter 1 :except (2027))
  (:dates "2028-02-29")
  (:when (lambda (y m d) (declare (ignore y)) (and (= m 10) (= d 13))) :to 2028))

(defun naive-business-days (calendar from to)
  (loop for d = from then (date+ d 1) while (date< d to) count (business-day-p calendar d)))

(defun calendar-checks ()
  (format t "~&Holiday calendars~%")
  (let ((fixture (calendar-fixture)))
    (check-that "Easter matches QuantLib's table for 1901-2199, and 1900 is April 15th"
                (and (date= (easter 1900) #D"1900-04-15")
                     (loop for (kind year date) in fixture
                           always (or (not (eq kind :easter))
                                      (date= (easter year) (parse-date date))))))
    (check-that "nth-weekday counts from the month's end for negative n"
                (and (date= (nth-weekday -1 :monday 5 2027) #D"2027-05-31")
                     (date= (nth-weekday -2 :friday 6 2027) #D"2027-06-18")
                     (date= (nth-weekday 5 :monday 5 2027) #D"2027-05-31")
                     (null (nth-weekday 5 :tuesday 2 2027))))
    (flet ((observed (keyword date) (fincl::observe keyword date)))
      (check-that "observance: :next-monday moves Saturday and Sunday to Monday"
                  (and (date= (observed :next-monday #D"2028-01-01") #D"2028-01-03")
                       (date= (observed :next-monday #D"2023-01-01") #D"2023-01-02")
                       (date= (observed :next-monday #D"2027-01-01") #D"2027-01-01")))
      (check-that "observance: :nearest-weekday moves Saturday to Friday, Sunday to Monday"
                  (and (date= (observed :nearest-weekday #D"2026-07-04") #D"2026-07-03")
                       (date= (observed :nearest-weekday #D"2027-07-04") #D"2027-07-05")))
      (check-that "observance: :sunday-to-monday leaves Saturday alone"
                  (and (date= (observed :sunday-to-monday #D"2023-01-01") #D"2023-01-02")
                       (date= (observed :sunday-to-monday #D"2028-01-01") #D"2028-01-01")))
      (check-that "observance: one from DEFINE-OBSERVANCE"
                  (date= (observed :saturday-to-friday-only #D"2026-07-04") #D"2026-07-03")))
    (flet ((holiday-p* (date) (not (business-day-p +toy-holiday-calendar+ date))))
      (check-that "a calendar defined outside fincl applies every kind of rule"
                  (and (holiday-p* #D"2028-01-03")       ; New Year on Saturday -> Monday
                       (not (holiday-p* #D"2027-12-31"))
                       (holiday-p* #D"2026-07-03")       ; July 4th on Saturday -> Friday
                       (holiday-p* #D"2027-06-18")       ; second-to-last Friday of June
                       (not (holiday-p* #D"2029-03-07")) (holiday-p* #D"2030-03-06") ; :from
                       (holiday-p* #D"2028-04-17") (not (holiday-p* #D"2027-03-29")) ; :except
                       (holiday-p* #D"2028-02-29")
                       (holiday-p* #D"2027-10-13") (not (holiday-p* #D"2031-10-13"))))) ; :to
    (let ((random (sb-ext:seed-random-state 3)) (calendars (list +nyse+ +target+ +toy-holiday-calendar+)))
      (check-that "count-business-days = the day-by-day count on 1000 random spans"
                  (loop repeat 1000
                        for calendar = (nth (random 3 random) calendars)
                        for from = (date+ #D"1950-01-01" (random 40000 random))
                        for to = (date+ from (random 9000 random))
                        always (= (count-business-days calendar from to)
                                  (naive-business-days calendar from to))))
      ;; June 7th-14th 2027 has no holiday; June 18th is Juneteenth observed.
      (check-that "business-days-between keeps its endpoint conventions"
                  (and (= 5 (business-days-between +nyse+ #D"2027-06-07" #D"2027-06-14"))
                       (= -5 (business-days-between +nyse+ #D"2027-06-14" #D"2027-06-07"))
                       (= 5 (business-days-between +nyse+ #D"2027-06-07" #D"2027-06-14"
                                                   :include-first nil :include-last t))
                       (= 4 (business-days-between +nyse+ #D"2027-06-14" #D"2027-06-21")))))
    (business-days-between +nyse+ #D"1990-01-01" #D"2090-01-01") ; warm the cache
    (let* ((start (get-internal-real-time))
           (count (dotimes (i 1000 (business-days-between +nyse+ #D"1990-01-01" #D"2090-01-01"))
                    (business-days-between +nyse+ #D"1990-01-01" #D"2090-01-01")))
           (micros (/ (* 1d6 (- (get-internal-real-time) start)) internal-time-units-per-second 1000)))
      (check-that (format nil "100 years of NYSE business days (~D) in ~,1F us (< 1 ms)" count micros)
                  (< micros 1000d0)))
    ;; Two fresh calendars, cold caches: one filled from 8 threads at once.
    (let* ((days (loop for serial from (date-serial #D"1990-01-01") below (date-serial #D"2061-01-01")
                       collect (fincl::%date serial)))
           (single (let ((c (make-instance 'nyse))) (mapcar (lambda (d) (business-day-p c d)) days)))
           (shared (make-instance 'nyse))
           (threads (loop for k below 8
                          collect (let ((order (if (evenp k) days (reverse days))))
                                    (bt:make-thread
                                     (lambda () (mapcar (lambda (d) (cons d (business-day-p shared d)))
                                                        order)))))))
      (check-that "8 threads on a cold cache see what one thread sees"
                  (every (lambda (thread)
                           (let ((answers (bt:join-thread thread)))
                             (every (lambda (pair) (eq (cdr pair)
                                                       (nth (- (date-serial (car pair))
                                                               (date-serial #D"1990-01-01"))
                                                            single)))
                                    answers)))
                         threads)))
    (loop for (kind name . dates) in fixture
          when (eq kind :calendar)
            do (let* ((calendar (symbol-value (find-symbol (format nil "+~:@(~A~)+" name) '#:fincl)))
                      (expected (mapcar #'parse-date dates))
                      (ours (loop for serial from (date-serial #D"1990-01-01")
                                    to (date-serial #D"2060-12-31")
                                  for d = (fincl::%date serial)
                                  unless (or (weekend-p calendar d) (business-day-p calendar d))
                                    collect d))
                      (missing (set-difference expected ours :test #'date=))
                      (extra (set-difference ours expected :test #'date=)))
                 (check-that (format nil "~A matches QuantLib 1990-2060 (~D holidays~@[; missing ~A~]~@[; extra ~A~])"
                                     name (length expected) missing extra)
                             (and (null missing) (null extra)))))))

;;; --------------------------------------------------------------------
;;; Schedules and ACT/ACT ICMA
;;; --------------------------------------------------------------------

(defun schedule-fixture ()
  (with-open-file (s (asdf:system-relative-pathname "fincl" "tests/fixtures/schedules.sexp"))
    (let ((*read-default-float-format* 'double-float))
      (loop for entry = (read s nil s) until (eq entry s) collect entry))))

(defun schedule-checks ()
  (format t "~&Schedules and ACT/ACT ICMA~%")
  (let ((cases 0) (bad-dates '()) (bad-regular 0) (icma-count 0) (worst-icma 0d0)
        (bad-refusals 0) (worst-reference 0d0))
    (dolist (case (schedule-fixture))
      (destructuring-bind (&key calendar effective termination tenor convention
                                termination-convention rule end-of-month first-date
                                next-to-last-date dates regular icma icma-reference)
          case
        (let* ((calendar (symbol-value (find-symbol (format nil "+~:@(~A~)+" calendar) '#:fincl)))
               (schedule (make-schedule (parse-date effective) (parse-date termination)
                                        (parse-tenor tenor)
                                        :calendar calendar :convention convention
                                        :termination-convention termination-convention
                                        :rule rule :end-of-month end-of-month
                                        :first-date (and first-date (parse-date first-date))
                                        :next-to-last-date (and next-to-last-date
                                                                (parse-date next-to-last-date))))
               (ours (schedule-dates schedule))
               (icma-counter (make-instance 'actual-actual-icma :schedule schedule))
               (reference-counter (make-instance 'actual-actual-icma)))
          (incf cases)
          (unless (and (= (length ours) (length dates)) (every #'date= ours (mapcar #'parse-date dates)))
            (push case bad-dates))
          (unless (equal (coerce (schedule-regular-p schedule) 'list)
                         (mapcar (lambda (r) (if r 1 0)) regular))
            (incf bad-regular))
          (if icma
              (loop for (a b) on ours while b
                    for expected in icma
                    do (incf icma-count)
                       (setf worst-icma (max worst-icma (abs (- (year-fraction icma-counter a b) expected)))))
              ;; QuantLib's result is not finite: fincl refuses.
              (unless (handler-case (progn (year-fraction icma-counter (first ours) (second ours)) nil)
                        (invalid-input () t))
                (incf bad-refusals)))
          (loop for (a b) on ours while b
                for expected in icma-reference
                do (setf worst-reference
                         (max worst-reference
                              (abs (- (year-fraction reference-counter a b :ref-start a :ref-end b)
                                      expected))))))))
    (check-that (format nil "~D schedules: dates match QuantLib exactly~@[ (mismatches: ~S)~]"
                        cases (and bad-dates (subseq bad-dates 0 (min 3 (length bad-dates)))))
                (null bad-dates))
    (check-that "... and so do the regular-period flags" (zerop bad-regular))
    (check-that (format nil "ICMA over the schedules: ~D periods within 1e-15 (worst ~,1E)"
                        icma-count worst-icma)
                (<= worst-icma 1d-15))
    (check-that "ICMA refuses where QuantLib's result is not finite (zero-coupon periods)"
                (zerop bad-refusals))
    (check-that (format nil "ICMA with each period as its own reference within 1e-15 (worst ~,1E)"
                        worst-reference)
                (<= worst-reference 1d-15)))
  (let ((s (make-schedule #D"2024-03-15" #D"2029-01-31" #T"6M" :calendar +target+)))
    (check-that "unadjusted dates adjust to the dates"
                (every (lambda (u d) (date= (adjust +target+ u :modified-following) d))
                       (schedule-unadjusted-dates s) (schedule-dates s)))
    (check-that "a short front stub is flagged irregular"
                (and (zerop (sbit (schedule-regular-p s) 0))
                     (every #'plusp (subseq (schedule-regular-p s) 1)))))
  (check-error "termination before effective"
               (make-schedule #D"2029-01-31" #D"2024-01-31" #T"6M") invalid-input)
  (check-error "a first date with the :zero rule"
               (make-schedule #D"2024-01-31" #D"2029-01-31" #T"6M" :rule :zero
                                                                  :first-date #D"2024-06-30")
               invalid-input)
  (check-equal "advance counts business days" #D"2027-07-02"
               (advance +target+ #D"2027-06-30" #T"2D") :test #'date=)
  (check-equal "advance with end of month keeps the last business day" #D"2027-02-26"
               (advance +nyse+ #D"2027-01-29" #T"1M" :end-of-month t) :test #'date=))
