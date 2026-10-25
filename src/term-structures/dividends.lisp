;;;; term-structures/dividends.lisp --- discrete dividend schedules
;;;;
;;;; A continuous dividend yield (a DIVIDEND-CURVE) suits an index. A single
;;;; stock pays known amounts on known dates: a DIVIDEND-SCHEDULE lists them,
;;;; and a market holds one per underlying next to its dividend curve. The
;;;; market view turns the schedule into DIVIDEND-EVENTs in market time
;;;; (market.lisp), which FORWARD and the engines read.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defstruct (dividend (:constructor %make-dividend (ex-date amount kind pay-date)))
  "One dividend: KIND :CASH pays AMOUNT (currency) on PAY-DATE to holders on
EX-DATE; KIND :PROPORTIONAL pays the fraction AMOUNT of the price."
  (ex-date nil :type date :read-only t)
  (amount 0d0 :type double-float :read-only t)
  (kind :cash :type (member :cash :proportional) :read-only t)
  (pay-date nil :type date :read-only t))

(defclass dividend-schedule (term-structure)
  ((dividends :initarg :dividends :reader schedule-dividends :type list
              :documentation "DIVIDEND structures, by ex-date."))
  (:documentation "Known discrete dividends of one underlying. Made by
MAKE-DIVIDEND-SCHEDULE."))

(defun as-date (x what)
  (let ((date (if (stringp x) (parse-date x) x)))
    (check-input date date (format nil "a date or a \"YYYY-MM-DD\" string for ~A" what))
    date))

(defun make-dividend-schedule (entries)
  "A DIVIDEND-SCHEDULE from ENTRIES, a list of

  (ex-date amount &key (kind :cash) pay-date)

EX-DATE and PAY-DATE are dates or \"YYYY-MM-DD\" strings; PAY-DATE (default
EX-DATE) may not precede EX-DATE. A :CASH AMOUNT is a non-negative sum of
money, a :PROPORTIONAL one a fraction in [0, 1) of the price.

  (make-dividend-schedule '((#D\"2027-02-10\" 0.82d0 :pay-date #D\"2027-03-01\")
                            (#D\"2027-08-10\" 0.85d0)))"
  (check-input entries list "a list of (ex-date amount &key kind pay-date)")
  (make-instance
   'dividend-schedule
   :dividends
   (stable-sort
    (iter (for entry in entries)
          (collect
              (destructuring-bind (ex-date amount &key (kind :cash) pay-date) entry
                (let ((ex-date (as-date ex-date "an ex-date"))
                      (pay-date (as-date (or pay-date ex-date) "a pay date")))
                  (check-input kind (member :cash :proportional) ":CASH or :PROPORTIONAL")
                  (if (eq kind :cash)
                      (check-input amount (real 0) "a non-negative cash amount")
                      (check-input amount (real 0 (1)) "a proportional amount in [0, 1)"))
                  (when (date< pay-date ex-date)
                    (signal-invalid-argument 'pay-date pay-date 'date
                                             (format nil "a pay date on or after the ex-date ~A" ex-date)))
                  (%make-dividend ex-date (float amount 1d0) kind pay-date)))))
    #'date< :key #'dividend-ex-date)))

(defun schedule-entries (schedule)
  "The entries of SCHEDULE as MAKE-DIVIDEND-SCHEDULE takes them."
  (iter (for d in (schedule-dividends schedule))
        (collect `(,(dividend-ex-date d) ,(dividend-amount d)
                   ,@(unless (eq (dividend-kind d) :cash) `(:kind ,(dividend-kind d)))
                   ,@(unless (date= (dividend-pay-date d) (dividend-ex-date d))
                       `(:pay-date ,(dividend-pay-date d)))))))

(defun shift-dividend (d shift)
  (%make-dividend (dividend-ex-date d) (max 0d0 (apply-shift (dividend-amount d) shift))
                  (dividend-kind d) (dividend-pay-date d)))

;;; A bump moves every amount; BUMP-DIVIDEND moves the one on an ex-date.
(defmethod bump-structure ((s dividend-schedule) shift)
  (make-instance 'dividend-schedule
                 :dividends (mapcar (lambda (d) (shift-dividend d shift)) (schedule-dividends s))))

(defun bump-dividend (schedule ex-date shift)
  "A copy of SCHEDULE with the amounts of the dividends going ex on EX-DATE
moved by SHIFT."
  (let ((found nil))
    (prog1 (make-instance 'dividend-schedule
                          :dividends (iter (for d in (schedule-dividends schedule))
                                           (collect (if (date= (dividend-ex-date d) ex-date)
                                                        (progn (setf found t) (shift-dividend d shift))
                                                        d))))
      (unless found
        (signal-invalid-argument 'ex-date ex-date 'date
                                 (format nil "an ex-date of the schedule: ~{~A~^, ~}"
                                         (mapcar #'dividend-ex-date (schedule-dividends schedule))))))))

(defmethod print-object ((s dividend-schedule) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~{~A~^ ~}"
            (iter (for d in (schedule-dividends s))
                  (collect (format nil "~A:~,4F~:[%~;~]" (dividend-ex-date d)
                                   (if (eq (dividend-kind d) :cash)
                                       (dividend-amount d)
                                       (* 100 (dividend-amount d)))
                                   (eq (dividend-kind d) :cash)))))))

;;; --------------------------------------------------------------------
;;; Events in market time
;;; --------------------------------------------------------------------

(defstruct (dividend-event (:constructor make-dividend-event (date time kind amount)))
  "A dividend as engines see it: its ex-DATE and TIME (market time), KIND,
and AMOUNT: for :CASH the drop in the price at the ex-date (the amount
discounted from its pay date to the ex-date), for :PROPORTIONAL the fraction."
  (date nil :type date :read-only t)
  (time 0d0 :type double-float :read-only t)
  (kind :cash :type (member :cash :proportional) :read-only t)
  (amount 0d0 :type double-float :read-only t))
