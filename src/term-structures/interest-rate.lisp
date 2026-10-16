;;;; term-structures/interest-rate.lisp --- quoted interest rates
;;;;
;;;; A rate is a number plus the convention that turns it into growth: a day
;;;; counter (time between dates), a compounding rule and, for compounded
;;;; rules, a frequency. The arithmetic follows QuantLib's InterestRate
;;;; exactly (tests/fixtures/rates.sexp is generated from it).

(in-package #:fincl)

(deftype compounding ()
  "How a rate grows: :SIMPLE 1 + r t; :COMPOUNDED (1 + r/f)^(f t);
:CONTINUOUS e^(r t); :SIMPLE-THEN-COMPOUNDED simple up to t = 1/f, then
compounded; :COMPOUNDED-THEN-SIMPLE the reverse."
  '(member :simple :compounded :continuous :simple-then-compounded :compounded-then-simple))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *frequencies*
  '((:no-frequency . -1) (:once . 0) (:annual . 1) (:semiannual . 2)
    (:every-fourth-month . 3) (:quarterly . 4) (:bimonthly . 6) (:monthly . 12)
    (:every-fourth-week . 13) (:biweekly . 26) (:weekly . 52) (:daily . 365))
  "Frequency keywords and the number of periods per year (QuantLib's values)."))

(deftype frequency ()
  "Periods per year, named: :ANNUAL, :SEMIANNUAL, :QUARTERLY, :MONTHLY,
:WEEKLY, :DAILY and the others of *FREQUENCIES*."
  `(member ,@(mapcar #'car *frequencies*)))

(defun frequency-periods (frequency)
  "Periods per year of FREQUENCY: (frequency-periods :quarterly) => 4."
  (check-input frequency frequency "a frequency keyword (:annual, :semiannual, ...)")
  (cdr (assoc frequency *frequencies*)))

(defun periods-frequency (n)
  "The frequency keyword with N periods a year."
  (or (car (rassoc n *frequencies*))
      (signal-invalid-argument 'n n 'integer "a number of periods with a frequency keyword")))

(defun compounded-p (compounding)
  (member compounding '(:compounded :simple-then-compounded :compounded-then-simple)))

(defstruct (interest-rate (:constructor %make-interest-rate (rate day-counter compounding frequency)))
  "RATE quoted under DAY-COUNTER, COMPOUNDING and FREQUENCY. Read-only."
  (rate 0d0 :type double-float :read-only t)
  (day-counter nil :type day-counter :read-only t)
  (compounding :continuous :type compounding :read-only t)
  (frequency :annual :type frequency :read-only t))

(defun make-interest-rate (rate &key (day-counter +actual-365-fixed+) (compounding :continuous)
                                     (frequency :annual))
  "A quoted rate. Compounded rules need a frequency of at least one period
a year.

  (make-interest-rate 0.05d0 :day-counter +actual-360+ :compounding :compounded
                             :frequency :semiannual)"
  (check-input rate real "a real rate")
  (check-input day-counter day-counter "a day counter")
  (check-input compounding compounding
               ":SIMPLE, :COMPOUNDED, :CONTINUOUS, :SIMPLE-THEN-COMPOUNDED or :COMPOUNDED-THEN-SIMPLE")
  (check-input frequency frequency "a frequency keyword")
  (when (and (compounded-p compounding) (member frequency '(:once :no-frequency)))
    (signal-invalid-argument 'frequency frequency 'frequency
                             "a frequency of at least one period a year for a compounded rate"))
  (%make-interest-rate (float rate 1d0) day-counter compounding frequency))

(defmethod print-object ((r interest-rate) stream)
  (print-unreadable-object (r stream :type t)
    (format stream "~,6F% ~(~A~)~:[~*~; ~(~A~)~] ~(~A~)"
            (* 100 (interest-rate-rate r)) (interest-rate-compounding r)
            (compounded-p (interest-rate-compounding r)) (interest-rate-frequency r)
            (class-name (class-of (interest-rate-day-counter r))))))

(defun rate-time (day-counter time-or-start end ref-start ref-end)
  "A year fraction: TIME-OR-START itself when END is NIL, else DAY-COUNTER's
fraction from TIME-OR-START to END."
  (if end
      (progn
        (when (date< end time-or-start)
          (signal-invalid-argument 'end end 'date
                                   (format nil "a date on or after ~A" time-or-start)))
        (year-fraction day-counter time-or-start end :ref-start ref-start :ref-end ref-end))
      (float time-or-start 1d0)))

(defun compound-factor (rate time-or-start &optional end ref-start ref-end)
  "The growth of one unit at RATE (an INTEREST-RATE) over TIME-OR-START years,
or over the dates TIME-OR-START to END under the rate's day counter (with
optional reference period dates).

  (compound-factor (make-interest-rate 0.05d0) 1d0) => 1.0512710963760241d0"
  (check-input rate interest-rate "an INTEREST-RATE")
  (let ((r (interest-rate-rate rate))
        (time (rate-time (interest-rate-day-counter rate) time-or-start end ref-start ref-end)))
    (when (minusp time)
      (signal-invalid-argument 'time time 'real "a non-negative time"))
    (flet ((simple () (+ 1d0 (* r time)))
           (compounded ()
             (let ((f (float (frequency-periods (interest-rate-frequency rate)) 1d0)))
               (expt (+ 1d0 (/ r f)) (* f time)))))
      (ecase (interest-rate-compounding rate)
        (:simple (simple))
        (:compounded (compounded))
        (:continuous (exp (* r time)))
        (:simple-then-compounded
         (if (<= time (/ 1d0 (frequency-periods (interest-rate-frequency rate)))) (simple) (compounded)))
        (:compounded-then-simple
         (if (> time (/ 1d0 (frequency-periods (interest-rate-frequency rate)))) (simple) (compounded)))))))

(defun discount-factor-of (rate time-or-start &optional end ref-start ref-end)
  "1 / COMPOUND-FACTOR: the value now of one unit paid after the time or
between the dates."
  (/ 1d0 (compound-factor rate time-or-start end ref-start ref-end)))

(defun implied-rate (compound day-counter compounding frequency time-or-start
                     &optional end ref-start ref-end)
  "The INTEREST-RATE under DAY-COUNTER, COMPOUNDING and FREQUENCY whose
compound factor over the time (or the dates TIME-OR-START to END) is
COMPOUND. A factor of exactly 1 is the zero rate.

  (implied-rate 1.05d0 +actual-365-fixed+ :simple :annual 1d0) => 5% simple"
  (check-input compound (real (0)) "a positive compound factor")
  (let ((compound (float compound 1d0))
        (time (rate-time day-counter time-or-start end ref-start ref-end)))
    (flet ((compounded ()
             (let ((f (float (frequency-periods frequency) 1d0)))
               (* (- (expt compound (/ 1d0 (* f time))) 1d0) f)))
           (simple () (/ (- compound 1d0) time)))
      (make-interest-rate
       (cond ((= compound 1d0)
              (when (minusp time) (signal-invalid-argument 'time time 'real "a non-negative time"))
              0d0)
             (t
              (unless (plusp time) (signal-invalid-argument 'time time 'real "a positive time"))
              (ecase compounding
                (:simple (simple))
                (:compounded (compounded))
                (:continuous (/ (log compound) time))
                (:simple-then-compounded
                 (if (<= time (/ 1d0 (frequency-periods frequency))) (simple) (compounded)))
                (:compounded-then-simple
                 (if (> time (/ 1d0 (frequency-periods frequency))) (simple) (compounded))))))
       :day-counter day-counter :compounding compounding :frequency frequency))))

(defun equivalent-rate (rate compounding frequency start end
                        &key (day-counter (interest-rate-day-counter rate)) ref-start ref-end)
  "RATE re-quoted with COMPOUNDING and FREQUENCY: over START years when END is
NIL, or between the dates START and END, measuring time with RATE's day
counter for the growth and DAY-COUNTER (default the same) for the result.

  (equivalent-rate r :continuous :no-frequency 2d0 nil)
  (equivalent-rate r :compounded :annual #D\"2027-01-15\" #D\"2028-01-15\"
                   :day-counter +actual-360+)"
  (check-input rate interest-rate "an INTEREST-RATE")
  (if end
      (implied-rate (compound-factor rate start end ref-start ref-end)
                    day-counter compounding frequency start end ref-start ref-end)
      (implied-rate (compound-factor rate start) day-counter compounding frequency start)))
