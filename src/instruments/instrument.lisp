;;;; instruments/instrument.lisp --- shared across asset classes
;;;;
;;;; Everything here is meant to be reused by bonds, swaps and anything else
;;;; added later. Option-specific components live in options.lisp.

(in-package #:fincl)

(defclass instrument ()
  ()
  (:documentation "A contract. Carries terms only: no market data, no model.
Instruments are immutable and reusable across scenarios."))

;;; --------------------------------------------------------------------
;;; Exercise (shared: callable bonds and Bermudan swaptions need this too)
;;; --------------------------------------------------------------------

(defclass exercise ()
  ((expiry :initarg :expiry :reader expiry :type date
           :initform (a:required-argument :expiry)
           :documentation "Last date on which the option can be exercised.")))

(defun check-expiry (expiry)
  (typecase expiry
    (date expiry)
    (tenor
     (error "Expiry ~S is a tenor and needs a start date to resolve it: pass ~
:FROM, e.g. :from #D\"2026-09-24\" or :from (valuation-date market)."
            expiry))
    (t
     (error "Expiry ~S is not a date. Expiries are dates, e.g. #D\"2027-09-24\" ~
or (make-date 2027 9 24), or tenors such as #T\"1Y\" with :FROM; the market ~
converts dates to year fractions with its day counter."
            expiry))))

(defun resolve-expiry (expiry &key from calendar (convention :following)
                                   end-of-month)
  "Return EXPIRY as a date. A date is returned as is. A tenor is added to the
date FROM (with END-OF-MONTH as in ADD-TENOR) and, if CALENDAR is given,
moved onto a business day with CONVENTION (see ADJUST).

  (resolve-expiry #T\"1Y\" :from #D\"2026-09-26\"
                  :calendar (make-instance 'weekends-only))
  => #D\"2027-09-27\""
  (if (and (typep expiry 'tenor) from)
      (let ((date (add-tenor (check-expiry from) expiry :end-of-month end-of-month)))
        (if calendar (adjust calendar date convention) date))
      (check-expiry expiry)))

(defmethod initialize-instance :after ((x exercise) &key)
  (check-expiry (expiry x)))

(defclass european-exercise (exercise) ()
  (:documentation "Exercise on the expiry date only."))

(defclass american-exercise (exercise) ()
  (:documentation "Exercise on any date up to and including the expiry."))

(defclass bermudan-exercise (exercise)
  ((expiry :initform nil)
   (dates :initarg :dates :reader exercise-dates
          :initform (a:required-argument :dates)))
  (:documentation "Exercise on any of DATES. EXPIRY is the last of them.

  (make-instance 'bermudan-exercise
                 :dates (list #D\"2027-03-24\" #D\"2027-09-24\"))"))

(defmethod initialize-instance :around ((x bermudan-exercise) &rest initargs
                                        &key dates &allow-other-keys)
  (unless (and dates (listp dates))
    (error "A Bermudan exercise needs a non-empty list of dates, not ~S." dates))
  (mapc #'check-expiry dates)
  (let ((sorted (sort (copy-list dates) #'date<)))
    (apply #'call-next-method x :dates sorted :expiry (a:lastcar sorted) initargs)))

(defgeneric exercise-allowed-p (exercise date)
  (:documentation "Can the holder exercise on DATE? One generic function is
the entire difference between European, American and Bermudan in a lattice
or LSM engine.")
  (:method ((x european-exercise) date) (date= date (expiry x)))
  (:method ((x american-exercise) date) (date<= date (expiry x)))
  (:method ((x bermudan-exercise) date)
    (and (member date (exercise-dates x) :test #'date=) t)))

(defun time-to-expiry (exercise market)
  "Year fraction from MARKET's valuation date to EXERCISE's expiry under the
market's day counter."
  (market-time market (expiry exercise)))

;;; --------------------------------------------------------------------
;;; Path dependence
;;; --------------------------------------------------------------------

(defclass path-feature () ())
(defclass path-independent (path-feature) ())
;; Barrier, Asian and lookback subclasses go here; engines dispatch on them.

;;; --------------------------------------------------------------------
;;; Payoff protocol
;;; --------------------------------------------------------------------

(defclass payoff ()
  ((kind :initarg :kind :reader kind :type (member :call :put)
         :initform (a:required-argument :kind))))

(defgeneric payoff-value (payoff x)
  (:documentation "Dispatching payoff. Convenient; too slow to call per path."))

(defgeneric payoff-kernel (payoff)
  (:documentation "Dispatch once, return a typed (double-float ->
double-float) closure. Engines call this before entering their loops."))
