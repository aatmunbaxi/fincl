;;;; instruments/cashflows.lisp --- cash flows, coupons and legs
;;;;
;;;; A leg is a list of cash flows. Fixed coupons know their amount; floating
;;;; coupons read fixings and forecast curves from a market. Leg builders
;;;; follow QuantLib's FixedRateLeg, IborLeg and OvernightLeg period by
;;;; period (reference periods for stubs, payment lags, par Ibor coupons,
;;;; daily compounded overnight coupons), and tests/fixtures/legs.sexp is
;;;; generated from them.

(in-package #:fincl)

(defclass cashflow ()
  ((payment-date :initarg :payment-date :reader payment-date :type date)
   (currency :initarg :currency :reader cashflow-currency :type keyword))
  (:documentation "An amount paid on PAYMENT-DATE in CURRENCY."))

(defgeneric cashflow-amount (cashflow market)
  (:documentation "The amount CASHFLOW pays, a double-float. MARKET supplies
fixings and forecast curves to floating coupons; fixed flows ignore it."))

(defclass simple-cashflow (cashflow)
  ((amount :initarg :amount :reader simple-amount :type double-float))
  (:documentation "A known amount: a notional exchange or a fee."))

(defmethod cashflow-amount ((c simple-cashflow) market)
  (declare (ignore market))
  (simple-amount c))

(defclass coupon (cashflow)
  ((nominal :initarg :nominal :reader coupon-nominal :type double-float)
   (accrual-start :initarg :accrual-start :reader accrual-start :type date)
   (accrual-end :initarg :accrual-end :reader accrual-end :type date)
   (ref-start :initarg :ref-start :reader coupon-ref-start :type date)
   (ref-end :initarg :ref-end :reader coupon-ref-end :type date)
   (day-counter :initarg :day-counter :reader coupon-day-counter :type day-counter))
  (:documentation "Interest on NOMINAL accrued from ACCRUAL-START to
ACCRUAL-END under DAY-COUNTER, with REF-START and REF-END the reference
period for day counters that need one."))

(defun accrual-period (coupon &optional (end (accrual-end coupon)))
  "The year fraction of COUPON's accrual, up to END."
  (year-fraction (coupon-day-counter coupon) (accrual-start coupon) end
                 :ref-start (coupon-ref-start coupon) :ref-end (coupon-ref-end coupon)))

(defgeneric coupon-rate (coupon market)
  (:documentation "The rate COUPON accrues at: fixed, or fixed or forecast
from MARKET."))

(defmethod cashflow-amount ((c coupon) market)
  (* (coupon-nominal c) (coupon-rate c market) (accrual-period c)))

(defgeneric accrued-amount-to (coupon date market)
  (:documentation "Interest COUPON has accrued by DATE: 0 on or before its
accrual start and after its payment date.")
  (:method ((c coupon) date market)
    (if (or (date<= date (accrual-start c)) (date< (payment-date c) date))
        0d0
        (* (coupon-nominal c) (coupon-rate c market)
           (accrual-period c (if (date< date (accrual-end c)) date (accrual-end c)))))))

;;; --------------------------------------------------------------------
;;; Fixed coupons
;;; --------------------------------------------------------------------

(defclass fixed-rate-coupon (coupon)
  ((rate :initarg :rate :reader fixed-coupon-rate :type interest-rate))
  (:documentation "Interest at a fixed INTEREST-RATE: nominal times the
rate's compound factor over the accrual period, less one."))

(defmethod coupon-rate ((c fixed-rate-coupon) market)
  (declare (ignore market))
  (interest-rate-rate (fixed-coupon-rate c)))

(defmethod cashflow-amount ((c fixed-rate-coupon) market)
  (declare (ignore market))
  (* (coupon-nominal c)
     (- (compound-factor (fixed-coupon-rate c) (accrual-start c) (accrual-end c)
                         (coupon-ref-start c) (coupon-ref-end c))
        1d0)))

(defmethod accrued-amount-to ((c fixed-rate-coupon) date market)
  (declare (ignore market))
  (if (or (date<= date (accrual-start c)) (date< (payment-date c) date))
      0d0
      (* (coupon-nominal c)
         (- (compound-factor (fixed-coupon-rate c) (accrual-start c)
                             (if (date< date (accrual-end c)) date (accrual-end c))
                             (coupon-ref-start c) (coupon-ref-end c))
            1d0))))

;;; --------------------------------------------------------------------
;;; Floating coupons
;;; --------------------------------------------------------------------

(defclass floating-rate-coupon (coupon)
  ((index :initarg :index :reader coupon-index :type interest-rate-index)
   (gearing :initarg :gearing :reader coupon-gearing :type double-float :initform 1d0)
   (spread :initarg :spread :reader coupon-spread :type double-float :initform 0d0))
  (:documentation "Interest at GEARING times an index rate plus SPREAD."))

(defclass ibor-coupon (floating-rate-coupon)
  ((fixing-days :initarg :fixing-days :reader coupon-fixing-days :type (integer 0))
   (fixing-date :reader coupon-fixing-date :type date)
   (fixing-value-date :reader fixing-value-date :type date)
   (fixing-end-date :reader fixing-end-date :type date
                    :documentation "End of the forecast period: the next coupon's
fixing value date (QuantLib's par coupon), at least a day after the value
date."))
  (:documentation "A coupon on an Ibor index fixed FIXING-DAYS before its
accrual start. The rate is forecast over the coupon's own accrual (from this
fixing's value date to the next fixing's), QuantLib's default \"par\" Ibor
coupon, rather than over the index tenor."))

(defmethod initialize-instance :after ((c ibor-coupon) &key)
  (let* ((index (coupon-index c))
         (calendar (index-fixing-calendar index))
         (fixing (advance calendar (accrual-start c) (make-tenor (- (coupon-fixing-days c)) :days)
                          :convention :preceding))
         (value (index-value-date index fixing))
         (next-fixing (advance calendar (accrual-end c) (make-tenor (- (coupon-fixing-days c)) :days)))
         (end (index-value-date index next-fixing)))
    (setf (slot-value c 'fixing-date) fixing
          (slot-value c 'fixing-value-date) value
          (slot-value c 'fixing-end-date) (if (date< value end) end (date+ value 1)))))

(defmethod coupon-rate ((c ibor-coupon) market)
  (let* ((index (coupon-index c))
         (fixing-date (coupon-fixing-date c))
         (today (valuation-date market))
         (fixing (if (or (date< fixing-date today)
                         (and (date= fixing-date today) (past-fixing index market fixing-date)))
                     (index-fixing index market fixing-date)
                     (index-forecast index market (fixing-value-date c) (fixing-end-date c)))))
    (+ (* (coupon-gearing c) fixing) (coupon-spread c))))

(defclass overnight-indexed-coupon (floating-rate-coupon)
  ((value-dates :reader overnight-value-dates :type simple-vector)
   (fixing-dates :reader overnight-fixing-dates :type simple-vector)
   (interest-dates :reader overnight-interest-dates :type simple-vector)
   (spans :reader overnight-spans :type double-vector))
  (:documentation "A coupon on an overnight index compounded daily in
arrears over its accrual period: (prod (1 + r_i d_i) - 1) / tau, d_i the
index day counter's fraction of each business day, then gearing and spread.
Forecast days telescope to D(start)/D(end) on the forecast curve. Lookback,
lockout and observation shift are not modelled."))

(defun business-day-list (calendar from to)
  "The business days of CALENDAR from FROM to TO, both included."
  (iter (for d in-business-days calendar from from to to) (collect d)))

(defmethod initialize-instance :after ((c overnight-indexed-coupon) &key)
  (let* ((index (coupon-index c))
         (calendar (index-fixing-calendar index))
         (start (accrual-start c)) (end (accrual-end c))
         (values (coerce (business-day-list calendar (adjust calendar start :preceding)
                                            (adjust calendar end :following))
                         'simple-vector))
         (n (1- (length values)))
         (interest (copy-seq values)))
    (unless (plusp n)
      (signal-invalid-argument 'accrual-end end 'date
                               (format nil "a date at least one business day after ~A" start)))
    (setf (svref interest 0) start
          (svref interest n) end)
    (setf (slot-value c 'value-dates) values
          (slot-value c 'interest-dates) interest
          (slot-value c 'fixing-dates)
          (if (zerop (index-fixing-days index))
              (subseq values 0 n)
              (map 'simple-vector (lambda (d) (index-fixing-date index d)) (subseq values 0 n)))
          (slot-value c 'spans)
          (let ((spans (make-array n :element-type 'double-float)))
            (dotimes (i n spans)
              (setf (aref spans i) (year-fraction (index-day-counter index) (svref interest i)
                                                  (svref interest (1+ i)))))))))

(defmethod coupon-rate ((c overnight-indexed-coupon) market)
  ;; QuantLib's CompoundingOvernightIndexedCouponPricer, without lookback.
  (let* ((index (coupon-index c))
         (today (valuation-date market))
         (fixings (overnight-fixing-dates c))
         (values (overnight-value-dates c))
         (interest (overnight-interest-dates c))
         (spans (overnight-spans c))
         (n (length fixings))
         (compound 1d0)
         (i 0))
    ;; Fixed days: before today, and today if the market has the fixing.
    (iter (while (and (< i n) (date< (svref fixings i) today)))
          (setf compound (* compound (+ 1d0 (* (index-fixing index market (svref fixings i))
                                               (aref spans i)))))
          (incf i))
    (when (and (< i n) (date= (svref fixings i) today))
      (let ((fixing (past-fixing index market today)))
        (when fixing
          (setf compound (* compound (+ 1d0 (* fixing (aref spans i)))))
          (incf i))))
    ;; Forecast days telescope; a first day starting on a holiday, and a
    ;; last value date after the accrual end, are compounded one by one.
    (when (< i n)
      (let* ((curve (index-forecast-curve index))
             (start (if (and (zerop i) (date< (svref values 0) (svref interest 0))) 1 i))
             (end (- n (if (date<= (svref values n) (accrual-end c)) 0 1))))
        (flet ((one-day (k)
                 (setf compound (* compound (+ 1d0 (* (index-fixing index market (svref fixings k))
                                                      (aref spans k)))))))
          (if (< start end)
              (progn
                (iter (while (< i start)) (one-day i) (incf i))
                (setf compound (* compound (/ (market-curve-discount market curve (svref values start))
                                              (market-curve-discount market curve (svref values end)))))
                (setf i end))
              nil)
          (iter (while (< i n)) (one-day i) (incf i)))))
    (let ((tau (year-fraction (index-day-counter index) (svref interest 0) (svref interest n))))
      (+ (* (coupon-gearing c) (/ (- compound 1d0) tau)) (coupon-spread c)))))

;;; --------------------------------------------------------------------
;;; Legs
;;; --------------------------------------------------------------------

(defun notional-and-currency (notional currency)
  "(values amount currency) from a real NOTIONAL and CURRENCY, or from MONEY."
  (if (money-p notional)
      (progn
        (when (and currency (not (eq currency (money-currency notional))))
          (error 'currency-mismatch :currencies `(,(money-currency notional) ,currency)
                                    :operation "leg notional"))
        (values (money-amount notional) (money-currency notional)))
      (progn
        (check-input notional real "a real notional or an amount of MONEY")
        (unless currency
          (signal-invalid-argument 'currency currency 'currency
                                   "a currency (or give the notional as MONEY)"))
        (check-input currency currency "a currency registered with DEFINE-CURRENCY")
        (values (float notional 1d0) currency))))

(defun schedule-periods (schedule)
  "The schedule's periods as (start end regular-p) lists."
  (let ((dates (schedule-dates schedule)) (regular (schedule-regular-p schedule)))
    (iter (for (start end) on dates) (while end)
          (for i from 0)
          (collect `(,start ,end ,(= 1 (sbit regular i)))))))

(defun payment-date-for (calendar end lag convention)
  (advance calendar end (make-tenor lag :days) :convention convention))

(defun fixed-leg (schedule notional rate day-counter
                  &key currency (payment-convention :following) (payment-lag 0) payment-calendar
                       (compounding :simple) (frequency :annual))
  "Fixed-rate coupons on SCHEDULE's periods: NOTIONAL (a real, with CURRENCY,
or MONEY) at RATE (a real quoted with DAY-COUNTER, COMPOUNDING and FREQUENCY,
or an INTEREST-RATE). Coupons pay PAYMENT-LAG business days after each
period end, adjusted by PAYMENT-CONVENTION on PAYMENT-CALENDAR (default the
schedule's). A short or long first or last period accrues against a notional
regular period, as QuantLib's FixedRateLeg.

  (fixed-leg schedule (make-money 1d6 :usd) 0.04d0 (make-instance 'thirty-360 :variant :bond-basis))"
  (check-input schedule schedule "a SCHEDULE")
  (multiple-value-bind (nominal currency) (notional-and-currency notional currency)
    (let* ((rate (if (interest-rate-p rate)
                     rate
                     (make-interest-rate rate :day-counter day-counter :compounding compounding
                                              :frequency frequency)))
           (calendar (schedule-calendar schedule))
           (payment-calendar (or payment-calendar calendar))
           (tenor (schedule-tenor schedule))
           (periods (schedule-periods schedule))
           (n (length periods)))
      (iter (for (start end regular) in periods)
            (for i from 0)
            (collect (multiple-value-bind (ref-start ref-end)
                         (cond ((and (zerop i) (not regular) tenor)
                                (values (advance calendar end (make-tenor (- (tenor-n tenor)) (tenor-unit tenor))
                                                 :convention (schedule-convention schedule)
                                                 :end-of-month (schedule-end-of-month schedule))
                                        end))
                               ((and (= i (1- n)) (> n 1) (not regular) tenor)
                                (values start (advance calendar start tenor
                                                       :convention (schedule-convention schedule)
                                                       :end-of-month (schedule-end-of-month schedule))))
                               (t (values start end)))
                       (make-instance 'fixed-rate-coupon
                                      :payment-date (payment-date-for payment-calendar end payment-lag
                                                                      payment-convention)
                                      :currency currency :nominal nominal
                                      :accrual-start start :accrual-end end
                                      :ref-start ref-start :ref-end ref-end
                                      :day-counter (interest-rate-day-counter rate) :rate rate)))))))

(defun floating-leg (schedule notional index
                     &key currency (spread 0) (gearing 1) day-counter
                          (payment-convention :following) (payment-lag 0) payment-calendar
                          fixing-days)
  "Floating coupons on SCHEDULE's periods paying GEARING times INDEX plus
SPREAD on NOTIONAL (a real, or MONEY; the currency defaults to the index's):
IBOR-COUPONs for an Ibor index, fixed FIXING-DAYS (default the index's)
before each period; OVERNIGHT-INDEXED-COUPONs for an overnight index.
DAY-COUNTER defaults to the index's. Payment dates as in FIXED-LEG.

  (floating-leg schedule 1d6 (euribor #T\"6M\") :spread 0.001d0)"
  (check-input schedule schedule "a SCHEDULE")
  (check-input index ibor-index "an Ibor or overnight index")
  (multiple-value-bind (nominal currency)
      (notional-and-currency notional (or currency (and (not (money-p notional)) (index-currency index))))
    (let* ((calendar (schedule-calendar schedule))
           (payment-calendar (or payment-calendar calendar))
           (tenor (schedule-tenor schedule))
           (day-counter (or day-counter (index-day-counter index)))
           (overnight (typep index 'overnight-index))
           (periods (schedule-periods schedule))
           (n (length periods)))
      (iter (for (start end regular) in periods)
            (for i from 0)
            (collect (let ((ref-start start) (ref-end end)
                           ;; OvernightLeg adjusts notional reference dates by the
                           ;; payment convention, IborLeg by the schedule's.
                           (convention (if overnight payment-convention (schedule-convention schedule))))
                       (when (and (zerop i) (not regular) tenor)
                         (setf ref-start (adjust calendar (add-tenor end (make-tenor (- (tenor-n tenor))
                                                                                     (tenor-unit tenor)))
                                                 convention)))
                       (when (and (= i (1- n)) (not regular) tenor)
                         (setf ref-end (adjust calendar (add-tenor start tenor) convention)))
                       (apply #'make-instance
                              (if overnight 'overnight-indexed-coupon 'ibor-coupon)
                              :payment-date (payment-date-for payment-calendar end payment-lag
                                                              payment-convention)
                              :currency currency :nominal nominal
                              :accrual-start start :accrual-end end
                              :ref-start ref-start :ref-end ref-end :day-counter day-counter
                              :index index :gearing (float gearing 1d0) :spread (float spread 1d0)
                              (unless overnight
                                `(:fixing-days ,(or fixing-days (index-fixing-days index)))))))))))

;;; --------------------------------------------------------------------
;;; Valuing legs
;;; --------------------------------------------------------------------

(defun leg-currency (leg)
  (let ((currencies (->> leg (mapcar #'cashflow-currency) remove-duplicates)))
    (when (rest currencies)
      (error 'currency-mismatch :currencies currencies :operation "valuing a leg"))
    (first currencies)))

(defun leg-discount-curve (leg market discount-curve)
  (or discount-curve (discount-curve-id market (leg-currency leg))))

(defun live-cashflows (leg market)
  "Cash flows paying on or after MARKET's valuation date."
  (remove-if (lambda (cf) (date< (payment-date cf) (valuation-date market))) leg))

(defun leg-npv (leg market &key discount-curve)
  "The value on MARKET's valuation date of the cash flows of LEG paying on or
after it, as MONEY. DISCOUNT-CURVE is a curve or a curve id; by default the
curve the market discounts the leg's currency on.

  (leg-npv (fixed-leg schedule (make-money 1d6 :eur) 0.03d0 +actual-360+) market)"
  (check-input market composite-market "a composite market")
  (let ((curve (leg-discount-curve leg market discount-curve)))
    (make-money (iter (for cf in (live-cashflows leg market))
                      (sum (* (cashflow-amount cf market)
                              (market-curve-discount market curve (payment-date cf)))))
                (leg-currency leg))))

(defun leg-bps (leg market &key discount-curve)
  "The value of one basis point on the notional of LEG's live coupons, as
MONEY: 1e-4 times the sum of nominal times accrual period times discount
factor (QuantLib's CashFlows::bps)."
  (check-input market composite-market "a composite market")
  (let ((curve (leg-discount-curve leg market discount-curve)))
    (make-money (* 1d-4
                   (iter (for cf in (live-cashflows leg market))
                         (when (typep cf 'coupon)
                           (sum (* (coupon-nominal cf) (accrual-period cf)
                                   (market-curve-discount market curve (payment-date cf)))))))
                (leg-currency leg))))

(defun accrued-amount (leg date &optional market)
  "Interest accrued by DATE on the coupons of LEG paying on its next payment
date after DATE, as MONEY (QuantLib's CashFlows::accruedAmount). Floating
coupons need MARKET for their rates.

  (accrued-amount leg #D\"2027-01-15\")"
  (let* ((next (iter (for cf in leg)
                     (when (date< date (payment-date cf))
                       (iter:minimize (date-serial (payment-date cf))))))
         (paying (and next (remove-if-not (lambda (cf)
                                            (and (typep cf 'coupon)
                                                 (= (date-serial (payment-date cf)) next)))
                                          leg))))
    (when (and (null market) (some (lambda (cf) (typep cf 'floating-rate-coupon)) paying))
      (signal-invalid-argument 'market market 'composite-market
                               "a market, for the floating coupons' rates"))
    (make-money (iter (for cf in paying) (sum (accrued-amount-to cf date market)))
                (leg-currency leg))))
