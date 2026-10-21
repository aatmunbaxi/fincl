;;;; instruments/indexes.lisp --- interest-rate indexes and their fixings
;;;;
;;;; An index is terms only: a tenor, a fixing calendar, fixing days, a day
;;;; counter, a business-day convention and the id of the curve that
;;;; forecasts it. Historical fixings are market data and live in the
;;;; composite market (MAKE-MARKET's :fixings); a fixing before the valuation
;;;; date that the market lacks signals MISSING-FIXING, with a USE-VALUE
;;;; restart. The logic follows QuantLib's IborIndex and OvernightIndex.

(in-package #:fincl)

(define-condition missing-fixing (pricing-error)
  ((index :initarg :index :reader error-index)
   (date :initarg :date :reader error-date))
  (:report (lambda (c s)
             (format s "The market has no ~A fixing for ~A, a date before its valuation ~
date. Add it to the market's :fixings, or supply a rate with USE-VALUE."
                     (index-name (error-index c)) (error-date c))))
  (:documentation "A past fixing an instrument needs is not in the market.
USE-VALUE supplies the rate."))

(defclass interest-rate-index ()
  ((name :initarg :name :reader index-name :type keyword
         :documentation "The index id, under which the market holds its fixings.")
   (family :initarg :family :reader index-family :type string)
   (tenor :initarg :tenor :reader index-tenor :type tenor)
   (fixing-calendar :initarg :fixing-calendar :reader index-fixing-calendar :type calendar)
   (fixing-days :initarg :fixing-days :reader index-fixing-days :type (integer 0))
   (day-counter :initarg :day-counter :reader index-day-counter :type day-counter)
   (convention :initarg :convention :reader index-convention :initform :modified-following)
   (end-of-month :initarg :end-of-month :reader index-end-of-month :initform nil)
   (currency :initarg :currency :reader index-currency :type keyword)
   (forecast-curve :initarg :forecast-curve :reader index-forecast-curve :type keyword
                   :documentation "Id of the market curve that forecasts the index."))
  (:documentation "A published interest rate: terms only."))

(defclass ibor-index (interest-rate-index) ()
  (:documentation "A term rate fixed FIXING-DAYS before its value date for a
deposit of TENOR (EURIBOR, Term SOFR).

  (make-instance 'ibor-index :name :euribor-6m :family \"Euribor\" :tenor #T\"6M\"
                             :fixing-calendar +target+ :fixing-days 2 :day-counter +actual-360+
                             :convention :modified-following :end-of-month t :currency :eur
                             :forecast-curve :euribor-6m)"))

(defclass overnight-index (ibor-index) ()
  (:default-initargs :tenor (make-tenor 1 :days) :convention :following)
  (:documentation "An overnight rate (SOFR, ESTR, SONIA): an index whose
deposit runs to the next business day."))

(defmethod print-object ((i interest-rate-index) stream)
  (print-unreadable-object (i stream :type t)
    (format stream "~S ~A forecast by ~S" (index-name i) (index-tenor i) (index-forecast-curve i))))

(defun euribor (tenor &key (forecast-curve nil forecast-curve-p))
  "EURIBOR for TENOR: TARGET, two fixing days, ACT/360, modified following
and end of month for monthly tenors (following otherwise). Forecast by the
curve FORECAST-CURVE, default the index's own id (:euribor-6m for 6M).

  (euribor #T\"6M\" :forecast-curve :eur-6m)"
  (let* ((monthly (member (tenor-unit tenor) '(:months :years)))
         (name (a:make-keyword (format nil "EURIBOR-~D~C" (tenor-n tenor) (unit-letter (tenor-unit tenor))))))
    (make-instance 'ibor-index :name name :family "Euribor" :tenor tenor
                               :fixing-calendar +target+ :fixing-days 2 :day-counter +actual-360+
                               :convention (if monthly :modified-following :following)
                               :end-of-month (and monthly t) :currency :eur
                               :forecast-curve (if forecast-curve-p forecast-curve name))))

(defun sofr (&key (forecast-curve :sofr))
  "SOFR: fixed with no lag on the SOFR calendar, ACT/360, in USD."
  (make-instance 'overnight-index :name :sofr :family "SOFR" :fixing-calendar +united-states-sofr+
                                  :fixing-days 0 :day-counter +actual-360+ :currency :usd
                                  :forecast-curve forecast-curve))

(defun estr (&key (forecast-curve :estr))
  "ESTR: fixed with no lag on TARGET, ACT/360, in EUR."
  (make-instance 'overnight-index :name :estr :family "ESTR" :fixing-calendar +target+
                                  :fixing-days 0 :day-counter +actual-360+ :currency :eur
                                  :forecast-curve forecast-curve))

(defun sonia (&key (forecast-curve :sonia))
  "SONIA: fixed with no lag on the London Stock Exchange calendar, ACT/365F,
in GBP."
  (make-instance 'overnight-index :name :sonia :family "SONIA"
                                  :fixing-calendar +united-kingdom-exchange+ :fixing-days 0
                                  :day-counter +actual-365-fixed+ :currency :gbp
                                  :forecast-curve forecast-curve))

;;; --------------------------------------------------------------------
;;; Dates
;;; --------------------------------------------------------------------

(defun index-value-date (index fixing-date)
  "The start of the deposit fixed on FIXING-DATE."
  (advance (index-fixing-calendar index) fixing-date (make-tenor (index-fixing-days index) :days)))

(defun index-fixing-date (index value-date)
  "The fixing date of the deposit starting on VALUE-DATE."
  (advance (index-fixing-calendar index) value-date (make-tenor (- (index-fixing-days index)) :days)))

(defun index-maturity-date (index value-date)
  "The end of the deposit starting on VALUE-DATE."
  (advance (index-fixing-calendar index) value-date (index-tenor index)
           :convention (index-convention index) :end-of-month (index-end-of-month index)))

;;; --------------------------------------------------------------------
;;; Fixings and forecasts
;;; --------------------------------------------------------------------

(defun market-discount-curve (market curve)
  "CURVE, a yield curve or the id of one in MARKET, as a function of
MARKET's time: resolve it once to take many discount factors."
  (curve-in-market (if (keywordp curve) (find-yield-curve market curve) curve) market))

(defun market-curve-discount (market curve date)
  "Discount factor from MARKET's valuation date to DATE on CURVE, a yield
curve or the id of one in MARKET."
  (curve-discount (market-discount-curve market curve) (market-time market date)))

(defun index-forecast (index market start end &optional span)
  "The simple forward rate of INDEX's forecast curve in MARKET from START to
END over SPAN (default the index day counter's fraction):
(D(start)/D(end) - 1) / span."
  (let ((span (or span (year-fraction (index-day-counter index) start end)))
        (curve (market-discount-curve market (index-forecast-curve index))))
    (unless (plusp span)
      (signal-invalid-argument 'end end 'date (format nil "a date after ~A" start)))
    (/ (- (/ (curve-discount curve (market-time market start))
             (curve-discount curve (market-time market end)))
          1d0)
       span)))

(defun past-fixing (index market date)
  "INDEX's fixing on DATE from MARKET's fixings, or NIL."
  (let ((table (gethash (index-name index) (market-fixings market))))
    (and table (values (gethash date table)))))

(defun index-fixing (index market fixing-date)
  "INDEX's rate fixed on FIXING-DATE: a market fixing before the valuation
date (MISSING-FIXING if absent, with USE-VALUE), the forecast after it, and
on the valuation date the fixing if the market has it, else the forecast.

  (index-fixing (euribor #T\"6M\") market #D\"2027-03-10\")"
  (check-input market composite-market "a composite market, which holds fixings and curves")
  (unless (business-day-p (index-fixing-calendar index) fixing-date)
    (signal-invalid-argument 'fixing-date fixing-date 'date
                             (format nil "a business day of ~A's fixing calendar" (index-name index))))
  (let ((today (valuation-date market)))
    (flet ((forecast ()
             (let ((start (index-value-date index fixing-date)))
               (index-forecast index market start (index-maturity-date index start)))))
      (cond ((date< today fixing-date) (forecast))
            ((date= today fixing-date) (or (past-fixing index market fixing-date) (forecast)))
            (t (or (past-fixing index market fixing-date)
                   (restart-case (error 'missing-fixing :index index :date fixing-date)
                     (use-value (rate)
                       :report "Use a rate for the missing fixing."
                       :interactive (lambda ()
                                      (format *query-io* "~&Rate: ")
                                      (list (eval (read *query-io*))))
                       (float rate 1d0)))))))))
