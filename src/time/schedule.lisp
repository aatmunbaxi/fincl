;;;; time/schedule.lisp --- coupon and exercise schedules
;;;;
;;;; MAKE-SCHEDULE follows QuantLib's Schedule constructor (ql/time/
;;;; schedule.cpp, 1.43) step for step: generation backward or forward from
;;;; a seed in whole multiples of the tenor, skipping dates that adjust onto
;;;; their neighbour, stubs at the ends, end-of-month alignment, then the
;;;; business-day adjustment and the two safety checks that merge a period
;;;; collapsed by adjustment. tests/fixtures/schedules.sexp, generated from
;;;; QuantLib, is the reference.

(in-package #:fincl)

(defstruct (schedule (:constructor %make-schedule))
  "Dates of a schedule, immutable. SCHEDULE-DATES are adjusted;
SCHEDULE-UNADJUSTED-DATES the same dates before adjustment;
SCHEDULE-REGULAR-P has one bit per period, 0 for a stub. The other slots are
the inputs ICMA day counting needs (END-OF-MONTH as applied: only for month
and year tenors)."
  (dates '() :type list :read-only t)
  (unadjusted-dates '() :type list :read-only t)
  (regular-p #* :type simple-bit-vector :read-only t)
  (calendar nil :read-only t)
  (tenor nil :read-only t)
  (convention :modified-following :read-only t)
  (termination-convention :modified-following :read-only t)
  (rule :backward :read-only t)
  (end-of-month nil :read-only t))

(defmethod print-object ((s schedule) stream)
  (print-unreadable-object (s stream :type t)
    (let ((dates (schedule-dates s)))
      (format stream "~A to ~A, ~D periods" (first dates) (a:lastcar dates) (1- (length dates))))))

(defun next-twentieth (date)
  "The 20th of DATE's month, or of the next month if that is before DATE."
  (let ((twentieth (make-date (date-year date) (date-month date) 20)))
    (if (date< twentieth date) (add-tenor twentieth (make-tenor 1 :months)) twentieth)))

(defun make-schedule (effective termination tenor
                      &key (calendar (make-instance 'null-calendar))
                           (convention :modified-following)
                           (termination-convention convention)
                           (rule :backward) end-of-month first-date next-to-last-date)
  "The schedule from EFFECTIVE to TERMINATION every TENOR, as QuantLib's
Schedule. RULE is :BACKWARD (dates counted back from TERMINATION, any stub at
the front), :FORWARD (from EFFECTIVE, stub at the back), :ZERO (the two
dates only; also any zero TENOR) or :TWENTIETH (forward on the 20th of the
month). Dates are adjusted onto CALENDAR's business days by CONVENTION, the
last one by TERMINATION-CONVENTION. END-OF-MONTH, for month and year tenors,
keeps a schedule seeded on a month end on month ends. FIRST-DATE and
NEXT-TO-LAST-DATE give explicit stubs (backward and forward rules only).

  (make-schedule #D\"2027-01-31\" #D\"2032-01-31\" #T\"6M\" :calendar +target+
                 :end-of-month t)"
  (check-input rule (member :backward :forward :zero :twentieth)
               ":BACKWARD, :FORWARD, :ZERO or :TWENTIETH")
  (unless (date< effective termination)
    (signal-invalid-argument 'termination termination 'date
                             (format nil "a date after the effective date ~A" effective)))
  (let* ((n (tenor-n tenor))
         (unit (tenor-unit tenor))
         (eom (and end-of-month (member unit '(:months :years)) (plusp n)))
         (first (and first-date (not (date= first-date effective)) first-date))
         (next-to-last (and next-to-last-date (not (date= next-to-last-date termination))
                            next-to-last-date))
         (rule (if (zerop n) :zero rule))
         (dates (make-array 0 :adjustable t :fill-pointer t))
         (regular (make-array 0 :adjustable t :fill-pointer t))
         (seed nil))
    (when (minusp n)
      (signal-invalid-argument 'tenor tenor 'tenor "a non-negative tenor"))
    (flet ((seed-step (date k) (add-tenor date (make-tenor (* k n) unit) :end-of-month eom))
           (adj (date c) (adjust calendar date c))
           (last-date () (aref dates (1- (length dates))))
           (push-date (date regular-p)
             (vector-push-extend date dates)
             (vector-push-extend regular-p regular)))
      (when first
        (unless (and (member rule '(:backward :forward))
                     (date< effective first) (not (date< termination first)))
          (signal-invalid-argument 'first-date first-date 'date
                                   (format nil "a date in (~A, ~A] under :BACKWARD or :FORWARD"
                                           effective termination))))
      (when next-to-last
        (unless (and (member rule '(:backward :forward))
                     (not (date< next-to-last effective)) (date< next-to-last termination))
          (signal-invalid-argument 'next-to-last-date next-to-last-date 'date
                                   (format nil "a date in [~A, ~A) under :BACKWARD or :FORWARD"
                                           effective termination))))
      (when (and (eq rule :twentieth) eom)
        (signal-invalid-argument 'end-of-month end-of-month 'null
                                 "NIL: end of month is incompatible with :TWENTIETH"))
      (ecase rule
        (:zero
         (vector-push-extend effective dates)
         (push-date termination t))
        (:backward
         (vector-push-extend termination dates)
         (setf seed termination)
         (when next-to-last
           (push-date next-to-last (date= (seed-step seed -1) next-to-last))
           (setf seed next-to-last))
         (let ((exit (or first effective)))
           (loop for periods from 1
                 for date = (seed-step seed (- periods))
                 do (when (date< date exit)
                      (when (and first (not (date= (adj (last-date) convention)
                                                   (adj first convention))))
                        (let ((previous (last-date)))
                          (push-date first (date= (seed-step previous -1) first))))
                      (return))
                    (unless (date= (adj (last-date) convention) (adj date convention))
                      (push-date date t))))
         (unless (date= (adj (last-date) convention) (adj effective convention))
           (let ((previous (last-date)))
             (push-date effective (date= (seed-step previous -1) effective))))
         (setf dates (reverse dates) regular (reverse regular)))
        ((:forward :twentieth)
         (vector-push-extend effective dates)
         (setf seed effective)
         (cond (first
                (push-date first (date= (seed-step seed 1) first))
                (setf seed first))
               ((eq rule :twentieth)
                (let ((twentieth (next-twentieth effective)))
                  (unless (date= twentieth effective)
                    (push-date twentieth nil)
                    (setf seed twentieth)))))
         (let ((exit (or next-to-last termination)))
           (loop for periods from 1
                 for date = (seed-step seed periods)
                 do (when (date< exit date)
                      (when (and next-to-last (not (date= (adj (last-date) convention)
                                                          (adj next-to-last convention))))
                        (let ((previous (last-date)))
                          (push-date next-to-last (date= (seed-step previous 1) next-to-last))))
                      (return))
                    (unless (date= (adj (last-date) convention) (adj date convention))
                      (push-date date t))))
         (unless (date= (adj (last-date) termination-convention)
                        (adj termination termination-convention))
           (if (eq rule :twentieth)
               (push-date (next-twentieth termination) t)
               (push-date termination nil)))))
      ;; Adjustment: the ends by their conventions, the rest by CONVENTION,
      ;; moved to month ends when the seed is one.
      (let* ((dates (coerce dates 'simple-vector))
             (unadjusted (copy-seq dates))
             (regular (coerce regular 'simple-vector))
             (count (length dates)))
        (unless (eq convention :unadjusted)
          (setf (svref dates 0) (adj (svref dates 0) convention)))
        (unless (eq termination-convention :unadjusted)
          (setf (svref dates (1- count)) (adj (svref dates (1- count)) termination-convention)))
        (let ((month-ends (and eom seed (calendar-end-of-month-p calendar seed))))
          (loop for i from 1 below (1- count)
                do (setf (svref dates i)
                         (adj (if month-ends
                                  (let ((d (svref dates i)))
                                    (make-date (date-year d) (date-month d)
                                               (days-in-month (date-year d) (date-month d))))
                                  (svref dates i))
                              convention))))
        ;; Safety checks: a next-to-last date on or after the last, or a
        ;; second date on or before the first, after adjustment.
        (let ((dates (coerce dates 'list)) (unadjusted (coerce unadjusted 'list))
              (regular (coerce regular 'list)))
          (let ((n (length dates)))
            (when (and (>= n 2) (not (date< (nth (- n 2) dates) (nth (1- n) dates))))
              (when (>= (length regular) 2)
                (setf (nth (- (length regular) 2) regular)
                      (date= (nth (- n 2) dates) (nth (1- n) dates))))
              (setf (nth (- n 2) dates) (nth (1- n) dates)
                    (nth (- n 2) unadjusted) (nth (1- n) unadjusted)
                    dates (butlast dates) unadjusted (butlast unadjusted)
                    regular (butlast regular))))
          (when (and (>= (length dates) 2) (not (date< (first dates) (second dates))))
            (when (>= (length regular) 2)
              (setf (second regular) (date= (second dates) (first dates))))
            (setf (second dates) (first dates)
                  (second unadjusted) (first unadjusted)
                  dates (rest dates) unadjusted (rest unadjusted)
                  regular (rest regular)))
          (unless (> (length dates) 1)
            (signal-invalid-argument 'effective effective 'date
                                     "dates that leave at least one period after adjustment"))
          (%make-schedule :dates dates :unadjusted-dates unadjusted
                          :regular-p (map 'simple-bit-vector (lambda (r) (if r 1 0)) regular)
                          :calendar calendar
                          :tenor (if (eq rule :zero) (make-tenor 0 :years) tenor)
                          :convention convention :termination-convention termination-convention
                          :rule rule :end-of-month eom))))))
