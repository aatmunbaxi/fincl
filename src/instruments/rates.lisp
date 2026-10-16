;;;; instruments/rates.lisp --- bonds, swaps and forward rate agreements
;;;;
;;;; Interest-rate instruments are terms only, like options. They are priced
;;;; on a composite market's curves (no underlying, so no market view): a
;;;; floating coupon is forecast on its index's curve, and every cash flow is
;;;; discounted on the curve of its currency unless a curve is named. The
;;;; bond analytics follow QuantLib's BondFunctions and CashFlows, the swaps
;;;; its VanillaSwap, OvernightIndexedSwap and DiscountingSwapEngine, and the
;;;; FRA its ForwardRateAgreement (tests/fixtures/bonds-swaps.sexp).

(in-package #:fincl)

(defclass rates-instrument (instrument) ()
  (:metaclass component-class)
  (:documentation "An interest-rate instrument. It is priced on a composite
market as a whole: its curves are named by currency and index, not reached
through an underlying."))

(defun rates-discount-curve (market currency discount-curve)
  "DISCOUNT-CURVE (a curve or a curve id), or the curve MARKET discounts
CURRENCY on."
  (check-input market composite-market "a composite market, which holds the curves")
  (or discount-curve (discount-curve-id market currency)))

(defun occurred-p (cashflow date)
  "Whether CASHFLOW has been paid by DATE: paying on or before it (QuantLib's
hasOccurred with reference-date flows excluded)."
  (date<= (payment-date cashflow) date))

;;; --------------------------------------------------------------------
;;; Bonds
;;; --------------------------------------------------------------------

(defclass bond (rates-instrument)
  ((face :initarg :face :reader bond-face :type double-float)
   (settlement-days :initarg :settlement-days :reader bond-settlement-days :type (integer 0))
   (calendar :initarg :calendar :reader bond-calendar
             :documentation "The settlement and payment calendar.")
   (issue-date :initarg :issue-date :initform nil :reader bond-issue-date
               :documentation "The first settlement date, or NIL.")
   (cashflows :initarg :cashflows :reader bond-cashflows
              :documentation "The coupons, then the redemption, by payment date."))
  (:metaclass component-class)
  (:documentation "A bond: cash flows on a face amount, settling
SETTLEMENT-DAYS business days after a trade (not before the issue date).
Prices are per 100 of face, as quoted."))

(defclass fixed-rate-bond (bond)
  ((schedule :initarg :schedule :reader bond-schedule)
   (coupon :initarg :coupon :reader bond-coupon :type interest-rate)
   (payment-convention :initarg :payment-convention :reader bond-payment-convention)
   (redemption :initarg :redemption :reader bond-redemption :type double-float
               :documentation "Paid at maturity, per 100 of face."))
  (:metaclass component-class)
  (:documentation "A bond paying a fixed coupon on each period of its
schedule and REDEMPTION per 100 of face with the last coupon. Build with
MAKE-FIXED-RATE-BOND."))

(defun make-fixed-rate-bond (schedule coupon day-counter
                             &key (face 100) currency (settlement-days 2)
                                  (payment-convention :following) (redemption 100) issue-date
                                  payment-calendar)
  "A fixed-rate bond (QuantLib's FixedRateBond): COUPON, a simple annual rate
under DAY-COUNTER (or an INTEREST-RATE), on FACE (a real, with CURRENCY, or
MONEY) over SCHEDULE's periods, paid on dates adjusted by PAYMENT-CONVENTION
on PAYMENT-CALENDAR (default the schedule's, also the settlement calendar).
REDEMPTION per 100 of face is paid with the last coupon.

  (make-fixed-rate-bond (make-schedule #D\"2024-05-15\" #D\"2034-05-15\" #T\"6M\"
                                       :calendar +united-states-government-bond+
                                       :convention :unadjusted)
                        0.04375d0 (make-instance 'actual-actual-icma :schedule s)
                        :currency :usd :settlement-days 1)"
  (check-input schedule schedule "a SCHEDULE")
  (check-input settlement-days (integer 0) "a whole number of business days")
  (check-input redemption real "a real redemption per 100 of face")
  (when issue-date (check-input issue-date date "the issue date, or NIL"))
  (multiple-value-bind (nominal currency) (notional-and-currency face currency)
    (let* ((calendar (or payment-calendar (schedule-calendar schedule)))
           (coupons (fixed-leg schedule (make-money nominal currency) coupon day-counter
                               :payment-convention payment-convention :payment-calendar calendar))
           (redemption (float redemption 1d0))
           (maturity (payment-date (a:lastcar coupons))))
      (make-instance 'fixed-rate-bond
                     :currency currency :face nominal :settlement-days settlement-days
                     :calendar calendar :issue-date issue-date :schedule schedule
                     :coupon (fixed-coupon-rate (first coupons))
                     :payment-convention payment-convention :redemption redemption
                     :cashflows `(,@coupons
                                  ,(make-instance 'simple-cashflow
                                                  :payment-date maturity :currency currency
                                                  :amount (/ (* nominal redemption) 100d0)))))))

(defun bond-maturity-date (bond)
  "The date of BOND's redemption."
  (payment-date (a:lastcar (bond-cashflows bond))))

(defun bond-settlement-date (bond date)
  "The date a trade of BOND on DATE settles: SETTLEMENT-DAYS business days
later, but not before the issue date."
  (let ((settlement (advance (bond-calendar bond) date (make-tenor (bond-settlement-days bond) :days)))
        (issue (bond-issue-date bond)))
    (if (and issue (date< settlement issue)) issue settlement)))

(defun bond-notional (bond date)
  "BOND's outstanding face on DATE: the face before the redemption date, 0
from it on."
  (if (date< date (bond-maturity-date bond)) (bond-face bond) 0d0))

(defun resolve-settlement (bond settlement valuation-date)
  "SETTLEMENT, or the settlement date of a trade on VALUATION-DATE, checked
to fall before BOND's maturity."
  (let ((date (cond (settlement (check-input settlement date "a settlement date") settlement)
                    (valuation-date (check-input valuation-date date "a valuation date")
                                    (bond-settlement-date bond valuation-date))
                    (t (signal-invalid-argument 'settlement settlement 'date
                                                "a :SETTLEMENT date or a :VALUATION-DATE to settle from")))))
    (when (zerop (bond-notional bond date))
      (signal-invalid-argument 'settlement date 'date
                               (format nil "a settlement date before the bond's maturity, ~A"
                                       (bond-maturity-date bond))))
    date))

(defun bond-live-cashflows (bond settlement)
  (remove-if (lambda (cf) (occurred-p cf settlement)) (bond-cashflows bond)))

(defun per-100 (bond amount settlement)
  "AMOUNT as a price per 100 of BOND's notional at SETTLEMENT."
  (/ (* amount 100d0) (bond-notional bond settlement)))

(defun bond-accrued-amount (bond &key settlement valuation-date)
  "Interest accrued at SETTLEMENT (default: the settlement of a trade on
VALUATION-DATE) per 100 of face."
  (let ((settlement (resolve-settlement bond settlement valuation-date)))
    (per-100 bond (money-amount (accrued-amount (bond-cashflows bond) settlement)) settlement)))

(defun bond-market-settlement (bond market settlement)
  (check-input market composite-market "a composite market, which holds the curves")
  (resolve-settlement bond settlement (valuation-date market)))

(defun bond-dirty-price (bond market &key discount-curve settlement)
  "Value at SETTLEMENT (default: of a trade on MARKET's valuation date) of
BOND's later cash flows, per 100 of face, discounted on DISCOUNT-CURVE (a
curve or curve id; default the curve of BOND's currency).

  (bond-dirty-price bond market)"
  (let* ((settlement (bond-market-settlement bond market settlement))
         (curve (rates-discount-curve market (instrument-currency bond) discount-curve)))
    (per-100 bond
             (/ (iter (for cf in (bond-live-cashflows bond settlement))
                      (sum (* (cashflow-amount cf market)
                              (market-curve-discount market curve (payment-date cf)))))
                (market-curve-discount market curve settlement))
             settlement)))

(defun bond-clean-price (bond market &key discount-curve settlement)
  "BOND-DIRTY-PRICE less the accrued interest."
  (let ((settlement (bond-market-settlement bond market settlement)))
    (- (bond-dirty-price bond market :discount-curve discount-curve :settlement settlement)
       (bond-accrued-amount bond :settlement settlement))))

(defun bond-bps (bond market &key discount-curve settlement)
  "Value of one basis point of coupon at SETTLEMENT, per 100 of face: 1e-4
times the sum of each later coupon's nominal, accrual period and discount
factor (QuantLib's BondFunctions::bps on a curve)."
  (let* ((settlement (bond-market-settlement bond market settlement))
         (curve (rates-discount-curve market (instrument-currency bond) discount-curve)))
    (per-100 bond
             (/ (* 1d-4 (iter (for cf in (bond-live-cashflows bond settlement))
                              (when (typep cf 'coupon)
                                (sum (* (coupon-nominal cf) (accrual-period cf)
                                        (market-curve-discount market curve (payment-date cf)))))))
                (market-curve-discount market curve settlement))
             settlement)))

;;; Yield analytics. A yield discounts each cash flow over its own period,
;;; measured from the previous flow under the yield's day counter with the
;;; coupon's reference period (QuantLib's getStepwiseDiscountTime).

(defun stepwise-time (cashflow day-counter npv-date last-date)
  "Years from LAST-DATE to CASHFLOW's payment under DAY-COUNTER, as QuantLib
measures them when discounting at a yield."
  (let ((date (payment-date cashflow)))
    (if (typep cashflow 'coupon)
        (let ((ref-start (coupon-ref-start cashflow))
              (ref-end (coupon-ref-end cashflow))
              (start (accrual-start cashflow)))
          (if (date= last-date start)
              (year-fraction day-counter last-date date :ref-start ref-start :ref-end ref-end)
              (- (year-fraction day-counter start date :ref-start ref-start :ref-end ref-end)
                 (year-fraction day-counter start last-date :ref-start ref-start :ref-end ref-end))))
        (year-fraction day-counter last-date date
                       :ref-start (if (date= last-date npv-date)
                                      (add-tenor date (make-tenor -1 :years))
                                      last-date)
                       :ref-end date))))

(defun map-yield-flows (function bond rate settlement)
  "Call FUNCTION with each live cash flow's amount, the stepwise time of its
period and the cumulative time to it, under RATE's day counter."
  (let ((dc (interest-rate-day-counter rate))
        (last settlement)
        (time 0d0))
    (dolist (cf (bond-live-cashflows bond settlement))
      (let ((step (stepwise-time cf dc settlement last)))
        (incf time step)
        (funcall function (cashflow-amount cf nil) step time)
        (setf last (payment-date cf))))))

(defun yield-npv (bond rate settlement)
  "Value at SETTLEMENT of BOND's later flows discounted at RATE, period by
period (QuantLib's CashFlows::npv at a yield)."
  (let ((npv 0d0) (discount 1d0))
    (map-yield-flows (lambda (amount step time)
                       (declare (ignore time))
                       (setf discount (* discount (discount-factor-of rate step)))
                       (incf npv (* amount discount)))
                     bond rate settlement)
    npv))

(defun bond-rate (yield day-counter compounding frequency)
  (if (interest-rate-p yield)
      yield
      (make-interest-rate yield :day-counter day-counter :compounding compounding
                                :frequency frequency)))

(defun bond-price-from-yield (bond yield day-counter compounding frequency
                              &key settlement valuation-date (clean t))
  "BOND's clean price (dirty if CLEAN is NIL) at YIELD, a rate under
DAY-COUNTER, COMPOUNDING and FREQUENCY (or an INTEREST-RATE, the three then
ignored). The settlement date is SETTLEMENT, or that of a trade on
VALUATION-DATE.

  (bond-price-from-yield bond 0.033d0 dc :compounded :semiannual :valuation-date today)"
  (let* ((settlement (resolve-settlement bond settlement valuation-date))
         (dirty (per-100 bond (yield-npv bond (bond-rate yield day-counter compounding frequency)
                                         settlement)
                         settlement)))
    (if clean (- dirty (bond-accrued-amount bond :settlement settlement)) dirty)))

(defun bond-yield (bond clean-price day-counter compounding frequency
                   &key settlement valuation-date (tolerance 1d-15))
  "The yield at which BOND's clean price is CLEAN-PRICE, as a rate under
DAY-COUNTER, COMPOUNDING and FREQUENCY, solved by Brent's method to
TOLERANCE. The settlement date is SETTLEMENT, or that of a trade on
VALUATION-DATE.

  (bond-yield bond 101.25d0 dc :compounded :semiannual :valuation-date today)"
  (check-input clean-price real "a real clean price")
  (let* ((settlement (resolve-settlement bond settlement valuation-date))
         (target (/ (+ clean-price (bond-accrued-amount bond :settlement settlement))
                    (/ 100d0 (bond-notional bond settlement)))))
    (values (solve-brent (lambda (y)
                           (- (yield-npv bond (bond-rate y day-counter compounding frequency) settlement)
                              target))
                         -0.2d0 1d0 :expand-hi t :tolerance tolerance))))

(defun bond-duration (bond yield day-counter compounding frequency
                      &key (type :modified) settlement valuation-date)
  "BOND's duration at YIELD (a rate under DAY-COUNTER, COMPOUNDING and
FREQUENCY, or an INTEREST-RATE): TYPE :MODIFIED (-P'/P), :MACAULAY
(compounded yields only) or :SIMPLE (the present-value-weighted time).

  (bond-duration bond 0.033d0 dc :compounded :semiannual :type :macaulay :valuation-date today)"
  (check-input type (member :modified :macaulay :simple) ":MODIFIED, :MACAULAY or :SIMPLE")
  (let* ((settlement (resolve-settlement bond settlement valuation-date))
         (rate (bond-rate yield day-counter compounding frequency))
         (r (interest-rate-rate rate))
         (compounding (interest-rate-compounding rate))
         (n (and (compounded-p compounding) (frequency-periods (interest-rate-frequency rate))))
         (p 0d0) (dp 0d0))
    (when (and (eq type :macaulay) (not (eq compounding :compounded)))
      (signal-invalid-argument 'yield rate 'interest-rate "a compounded yield for a Macaulay duration"))
    (map-yield-flows
     (lambda (c step time)
       (declare (ignore step))
       (let ((b (discount-factor-of rate time)))
         (incf p (* c b))
         (if (eq type :simple)
             (incf dp (* time c b))
             (flet ((simple () (decf dp (* c b b time)))
                    (compounded () (decf dp (/ (* c time b) (+ 1 (/ r n))))))
               (ecase compounding
                 (:simple (simple))
                 (:compounded (compounded))
                 (:continuous (decf dp (* c b time)))
                 (:simple-then-compounded (if (<= time (/ 1d0 n)) (simple) (compounded)))
                 (:compounded-then-simple (if (> time (/ 1d0 n)) (simple) (compounded))))))))
     bond rate settlement)
    (cond ((zerop p) 0d0)
          ((eq type :simple) (/ dp p))
          ((eq type :modified) (/ (- dp) p))
          (t (* (+ 1d0 (/ r n)) (/ (- dp) p))))))

(defun bond-convexity (bond yield day-counter compounding frequency &key settlement valuation-date)
  "BOND's convexity P''/P at YIELD (a rate under DAY-COUNTER, COMPOUNDING
and FREQUENCY, or an INTEREST-RATE)."
  (let* ((settlement (resolve-settlement bond settlement valuation-date))
         (rate (bond-rate yield day-counter compounding frequency))
         (r (interest-rate-rate rate))
         (compounding (interest-rate-compounding rate))
         (n (and (compounded-p compounding) (frequency-periods (interest-rate-frequency rate))))
         (p 0d0) (d2p 0d0))
    (map-yield-flows
     (lambda (c step time)
       (declare (ignore step))
       (let ((b (discount-factor-of rate time)))
         (incf p (* c b))
         (flet ((simple () (incf d2p (* c 2d0 b b b time time)))
                (compounded () (incf d2p (/ (* c b time (+ (* n time) 1))
                                            (* n (+ 1 (/ r n)) (+ 1 (/ r n)))))))
           (ecase compounding
             (:simple (simple))
             (:compounded (compounded))
             (:continuous (incf d2p (* c b time time)))
             (:simple-then-compounded (if (<= time (/ 1d0 n)) (simple) (compounded)))
             (:compounded-then-simple (if (> time (/ 1d0 n)) (simple) (compounded)))))))
     bond rate settlement)
    (if (zerop p) 0d0 (/ d2p p))))

;;; Z-spread: the spread over the curve's zero rates (quoted with the given
;;; compounding and frequency) that reprices the bond (QuantLib's
;;; ZeroSpreadedTermStructure).

(defun spreaded-discount (market curve spread compounding frequency date)
  "Discount factor to DATE on MARKET's CURVE with SPREAD added to its zero
rate quoted with COMPOUNDING and FREQUENCY, both in market time."
  (let ((time (market-time market date))
        (dc (market-day-counter market))
        (curve (curve-in-market (if (keywordp curve) (find-yield-curve market curve) curve) market)))
    (if (zerop time)
        1d0
        (let* ((zero (interest-rate-rate
                      (implied-rate (/ 1d0 (curve-discount curve time)) dc compounding frequency time)))
               (spreaded (make-interest-rate (+ zero spread) :day-counter dc
                                                             :compounding compounding
                                                             :frequency frequency))
               (continuous (interest-rate-rate
                            (equivalent-rate spreaded :continuous :no-frequency time nil))))
          (exp (- (* continuous time)))))))

(defun z-spread-npv (bond market curve spread compounding frequency settlement)
  (/ (iter (for cf in (bond-live-cashflows bond settlement))
           (sum (* (cashflow-amount cf market)
                   (spreaded-discount market curve spread compounding frequency (payment-date cf)))))
     (spreaded-discount market curve spread compounding frequency settlement)))

(defun bond-price-from-z-spread (bond spread market compounding frequency
                                 &key discount-curve settlement (clean t))
  "BOND's clean price (dirty if CLEAN is NIL) with SPREAD over the discount
curve's zero rates, quoted with COMPOUNDING and FREQUENCY."
  (let* ((settlement (bond-market-settlement bond market settlement))
         (curve (rates-discount-curve market (instrument-currency bond) discount-curve))
         (dirty (per-100 bond (z-spread-npv bond market curve spread compounding frequency settlement)
                         settlement)))
    (if clean (- dirty (bond-accrued-amount bond :settlement settlement)) dirty)))

(defun z-spread (bond clean-price market compounding frequency
                 &key discount-curve settlement (tolerance 1d-15))
  "The spread over the discount curve's zero rates (quoted with COMPOUNDING
and FREQUENCY) at which BOND's clean price is CLEAN-PRICE.

  (z-spread bond 101.25d0 market :compounded :semiannual)"
  (check-input clean-price real "a real clean price")
  (let* ((settlement (bond-market-settlement bond market settlement))
         (curve (rates-discount-curve market (instrument-currency bond) discount-curve))
         (target (/ (+ clean-price (bond-accrued-amount bond :settlement settlement))
                    (/ 100d0 (bond-notional bond settlement)))))
    (values (solve-brent (lambda (s)
                           (- (z-spread-npv bond market curve s compounding frequency settlement)
                              target))
                         -0.2d0 0.5d0 :tolerance tolerance))))

(defun bond-npv (bond market &key discount-curve)
  "Value on MARKET's valuation date of BOND's cash flows paying after it."
  (let ((curve (rates-discount-curve market (instrument-currency bond) discount-curve))
        (today (valuation-date market)))
    (iter (for cf in (bond-cashflows bond))
          (unless (occurred-p cf today)
            (sum (* (cashflow-amount cf market) (market-curve-discount market curve (payment-date cf))))))))

;;; --------------------------------------------------------------------
;;; Swaps
;;; --------------------------------------------------------------------

(deftype swap-type ()
  "Which side of the fixed leg: a :PAYER swap pays it, a :RECEIVER receives it."
  '(member :payer :receiver))

(defclass swap (rates-instrument)
  ((legs :initarg :legs :reader swap-legs :documentation "The legs, each a list of cash flows.")
   (signs :initarg :signs :reader swap-signs
          :documentation "+1d0 for a leg received, -1d0 for a leg paid."))
  (:metaclass component-class)
  (:documentation "An exchange of legs. Its value is the sum of the received
legs' values less the paid legs'."))

(defclass fixed-vs-floating-swap (swap)
  ((type :initarg :type :reader swap-type :type swap-type)
   (nominal :initarg :nominal :reader swap-nominal :type double-float)
   (fixed-rate :initarg :fixed-rate :reader swap-fixed-rate :type double-float)
   (spread :initarg :spread :reader swap-spread :type double-float)
   (index :initarg :index :reader swap-index)
   (fixed-schedule :initarg :fixed-schedule :reader swap-fixed-schedule))
  (:metaclass component-class)
  (:documentation "A fixed leg (leg 0) against a floating leg (leg 1) on the
same nominal. A payer swap pays the fixed leg."))

(defclass vanilla-swap (fixed-vs-floating-swap) ()
  (:metaclass component-class)
  (:documentation "Fixed against Ibor coupons (QuantLib's VanillaSwap). Build
with MAKE-VANILLA-SWAP."))

(defclass ois-swap (fixed-vs-floating-swap) ()
  (:metaclass component-class)
  (:documentation "Fixed against daily compounded overnight coupons on one
schedule (QuantLib's OvernightIndexedSwap). Build with MAKE-OIS-SWAP."))

(defun swap-signs-for (type)
  (check-input type swap-type ":PAYER (pays the fixed leg) or :RECEIVER")
  (if (eq type :payer) '(-1d0 1d0) '(1d0 -1d0)))

(defun make-vanilla-swap (type nominal fixed-schedule fixed-rate fixed-day-counter
                          float-schedule index
                          &key (spread 0) floating-day-counter payment-convention)
  "A vanilla swap (QuantLib's VanillaSwap): fixed coupons at FIXED-RATE under
FIXED-DAY-COUNTER on FIXED-SCHEDULE against par Ibor coupons on INDEX plus
SPREAD on FLOAT-SCHEDULE (FLOATING-DAY-COUNTER, default the index's), both
on NOMINAL in the index's currency. TYPE is :PAYER or :RECEIVER of the fixed
leg. Payment dates are adjusted by PAYMENT-CONVENTION, default the floating
schedule's.

  (make-vanilla-swap :payer 1d6 fixed 0.024d0 (make-instance 'thirty-360 :variant :bond-basis)
                     floating (euribor #T\"6M\"))"
  (check-input fixed-rate real "a real fixed rate")
  (check-input spread real "a real spread")
  (check-input index ibor-index "an Ibor index")
  (let* ((signs (swap-signs-for type))
         (convention (or payment-convention (schedule-convention float-schedule)))
         (currency (index-currency index))
         (fixed (fixed-leg fixed-schedule nominal fixed-rate fixed-day-counter
                           :currency currency :payment-convention convention))
         (floating (floating-leg float-schedule nominal index :spread spread
                                                             :day-counter floating-day-counter
                                                             :payment-convention convention)))
    (make-instance 'vanilla-swap :type type :nominal (float nominal 1d0)
                                 :fixed-rate (float fixed-rate 1d0) :spread (float spread 1d0)
                                 :index index :currency currency :fixed-schedule fixed-schedule
                                 :legs `(,fixed ,floating) :signs signs)))

(defun make-ois-swap (type nominal schedule fixed-rate fixed-day-counter index
                      &key (spread 0) (payment-lag 0) (payment-convention :following) payment-calendar)
  "An overnight indexed swap (QuantLib's OvernightIndexedSwap): fixed
coupons at FIXED-RATE under FIXED-DAY-COUNTER against daily compounded
coupons on the overnight INDEX plus SPREAD, both on SCHEDULE and NOMINAL,
paid PAYMENT-LAG business days after each period on PAYMENT-CALENDAR
(default the schedule's), adjusted by PAYMENT-CONVENTION.

  (make-ois-swap :payer 1d6 schedule 0.033d0 +actual-360+ (sofr) :payment-lag 2)"
  (check-input fixed-rate real "a real fixed rate")
  (check-input spread real "a real spread")
  (check-input index overnight-index "an overnight index")
  (let* ((signs (swap-signs-for type))
         (currency (index-currency index))
         (fixed (fixed-leg schedule nominal fixed-rate fixed-day-counter
                           :currency currency :payment-lag payment-lag
                           :payment-convention payment-convention :payment-calendar payment-calendar))
         (floating (floating-leg schedule nominal index :spread spread :payment-lag payment-lag
                                                       :payment-convention payment-convention
                                                       :payment-calendar payment-calendar)))
    (make-instance 'ois-swap :type type :nominal (float nominal 1d0)
                             :fixed-rate (float fixed-rate 1d0) :spread (float spread 1d0)
                             :index index :currency currency :fixed-schedule schedule
                             :legs `(,fixed ,floating) :signs signs)))

(defun swap-leg-npv (swap i market &key discount-curve)
  "Signed value of SWAP's leg I on MARKET: positive for a leg received."
  (* (nth i (swap-signs swap))
     (money-amount (leg-npv (nth i (swap-legs swap)) market
                            :discount-curve (rates-discount-curve market (instrument-currency swap)
                                                                  discount-curve)))))

(defun swap-leg-bps (swap i market &key discount-curve)
  "Signed value of one basis point on the coupons of SWAP's leg I."
  (* (nth i (swap-signs swap))
     (money-amount (leg-bps (nth i (swap-legs swap)) market
                            :discount-curve (rates-discount-curve market (instrument-currency swap)
                                                                  discount-curve)))))

(defun swap-npv (swap market &key discount-curve)
  "SWAP's value on MARKET: the sum of its signed leg values."
  (iter (for i below (length (swap-legs swap)))
        (sum (swap-leg-npv swap i market :discount-curve discount-curve))))

(defun fair-rate (swap market &key discount-curve)
  "The fixed rate at which SWAP is worth zero.

  (fair-rate swap market)"
  (- (swap-fixed-rate swap)
     (/ (swap-npv swap market :discount-curve discount-curve)
        (/ (swap-leg-bps swap 0 market :discount-curve discount-curve) 1d-4))))

(defun fair-spread (swap market &key discount-curve)
  "The floating spread at which SWAP is worth zero."
  (- (swap-spread swap)
     (/ (swap-npv swap market :discount-curve discount-curve)
        (/ (swap-leg-bps swap 1 market :discount-curve discount-curve) 1d-4))))

;;; --------------------------------------------------------------------
;;; Forward rate agreements
;;; --------------------------------------------------------------------

(defclass forward-rate-agreement (rates-instrument)
  ((index :initarg :index :reader fra-index)
   (value-date :initarg :value-date :reader fra-value-date :type date)
   (maturity-date :initarg :maturity-date :reader fra-maturity-date :type date)
   (position :initarg :position :reader fra-position :type (member :long :short))
   (strike :initarg :strike :reader fra-strike :type double-float)
   (notional :initarg :notional :reader fra-notional :type double-float)
   (indexed :initarg :indexed :reader fra-indexed-p
            :documentation "T: the rate is the index fixing; NIL: the forward from
the value date to the maturity date."))
  (:metaclass component-class)
  (:documentation "A forward rate agreement (QuantLib's
ForwardRateAgreement): on the value date the long side receives
notional (F - K) T / (1 + F T), F the rate for the period and T its length
under the index's day counter. Build with MAKE-FORWARD-RATE-AGREEMENT."))

(defun make-forward-rate-agreement (index value-date position strike notional &key maturity-date)
  "An FRA on the Ibor INDEX from VALUE-DATE, POSITION :LONG (receives the
rate) or :SHORT, at the simple STRIKE on NOTIONAL. Without MATURITY-DATE the
period is the index tenor and the rate the index fixing; with it, the
period ends there (adjusted) and the rate is the forward over it.

  (make-forward-rate-agreement (euribor #T\"6M\") #D\"2026-12-29\" :long 0.02d0 1d6)"
  (check-input index ibor-index "an Ibor index")
  (check-input value-date date "the value date")
  (check-input position (member :long :short) ":LONG or :SHORT")
  (check-input strike real "a real strike rate")
  (check-input notional (real (0)) "a positive notional")
  (let ((maturity (adjust (index-fixing-calendar index)
                          (or maturity-date (index-maturity-date index value-date))
                          (index-convention index))))
    (unless (date< value-date maturity)
      (signal-invalid-argument 'maturity-date maturity 'date (format nil "a date after ~A" value-date)))
    (make-instance 'forward-rate-agreement
                   :index index :value-date value-date :maturity-date maturity :position position
                   :strike (float strike 1d0) :notional (float notional 1d0)
                   :indexed (null maturity-date) :currency (index-currency index))))

(defun fra-forward-rate (fra market)
  "The rate FRA settles on: the index fixing, or the forecast forward."
  (let ((index (fra-index fra)))
    (if (fra-indexed-p fra)
        (index-fixing index market (index-fixing-date index (fra-value-date fra)))
        (index-forecast index market (fra-value-date fra) (fra-maturity-date fra)))))

(defun fra-amount (fra market)
  "What FRA pays on its value date."
  (let ((f (fra-forward-rate fra market))
        (k (fra-strike fra))
        (tau (year-fraction (index-day-counter (fra-index fra)) (fra-value-date fra)
                            (fra-maturity-date fra))))
    (/ (* (fra-notional fra) (if (eq (fra-position fra) :long) 1 -1) (- f k) tau)
       (+ 1d0 (* f tau)))))

(defun fra-npv (fra market &key discount-curve)
  "FRA-AMOUNT discounted from the value date."
  (* (fra-amount fra market)
     (market-curve-discount market (rates-discount-curve market (instrument-currency fra) discount-curve)
                            (fra-value-date fra))))

;;; --------------------------------------------------------------------
;;; Caps, floors and collars
;;; --------------------------------------------------------------------

(defclass cap-floor (rates-instrument)
  ((type :initarg :type :reader cap-floor-type :type (member :cap :floor :collar))
   (leg :initarg :leg :reader cap-floor-leg :documentation "The Ibor coupons capped or floored.")
   (cap-rates :initarg :cap-rates :reader cap-rates
              :documentation "One cap rate per coupon, or NIL for a floor.")
   (floor-rates :initarg :floor-rates :reader floor-rates
                :documentation "One floor rate per coupon, or NIL for a cap."))
  (:metaclass component-class)
  (:documentation "A strip of caplets or floorlets on a leg of Ibor coupons
(QuantLib's CapFloor). A caplet pays max(rate - cap, 0), a floorlet
max(floor - rate, 0), on the coupon's nominal and accrual; a collar is long
the cap and short the floor. Build with MAKE-CAP-FLOOR."))

(defun make-cap-floor (type leg &key cap-rate floor-rate)
  "Caplets (TYPE :CAP), floorlets (:FLOOR) or both (:COLLAR, long the cap)
on LEG, a list of Ibor coupons (FLOATING-LEG). CAP-RATE and FLOOR-RATE are a
real, or a list with one rate per coupon.

  (make-cap-floor :cap (floating-leg schedule 1d6 (euribor #T\"6M\")) :cap-rate 0.025d0)"
  (check-input type (member :cap :floor :collar) ":CAP, :FLOOR or :COLLAR")
  (check-input leg (and list (not null)) "a non-empty leg of Ibor coupons")
  (unless (every (lambda (c) (typep c 'ibor-coupon)) leg)
    (signal-invalid-argument 'leg leg 'list "a leg of Ibor coupons (FLOATING-LEG on an Ibor index)"))
  (flet ((rates (rate which needed)
           (cond ((not needed) nil)
                 ((realp rate) (make-list (length leg) :initial-element (float rate 1d0)))
                 ((and (listp rate) (= (length rate) (length leg)) (every #'realp rate))
                  (mapcar (lambda (r) (float r 1d0)) rate))
                 (t (signal-invalid-argument which rate 'real
                                             (format nil "a real rate, or a list of ~D (one per coupon)"
                                                     (length leg)))))))
    (make-instance 'cap-floor :type type :leg leg
                              :cap-rates (rates cap-rate 'cap-rate (member type '(:cap :collar)))
                              :floor-rates (rates floor-rate 'floor-rate (member type '(:floor :collar)))
                              :currency (cashflow-currency (first leg)))))

(defun caplet-forward (coupon market)
  "The rate COUPON's optionlet is struck on: its fixing net of spread and
gearing (QuantLib's adjustedFixing)."
  (/ (- (coupon-rate coupon market) (coupon-spread coupon)) (coupon-gearing coupon)))

;;; --------------------------------------------------------------------
;;; Swaptions
;;; --------------------------------------------------------------------

(defclass swaption (rates-instrument)
  ((swap :initarg :swap :reader swaption-swap)
   (exercise :initarg :exercise :reader swaption-exercise)
   (settlement :initarg :settlement :reader swaption-settlement
               :type (member :physical :cash-annuity)))
  (:metaclass component-class)
  (:documentation "The right to enter SWAP (a fixed-vs-floating swap; a
payer swaption enters a payer swap) on an exercise date: European, or
Bermudan for lattice engines. SETTLEMENT :PHYSICAL enters the swap;
:CASH-ANNUITY pays its value with the fixed leg discounted at the swap rate
(QuantLib's par yield curve method). Build with MAKE-SWAPTION."))

(defun make-swaption (swap exercise &key (settlement :physical))
  "A swaption on SWAP. EXERCISE is an exercise object, or a date for a
European swaption.

  (make-swaption swap #D\"2027-09-24\")
  (make-swaption swap (make-instance 'bermudan-exercise :dates dates))"
  (check-input swap fixed-vs-floating-swap "a fixed-vs-floating swap (MAKE-VANILLA-SWAP, MAKE-OIS-SWAP)")
  (check-input settlement (member :physical :cash-annuity) ":PHYSICAL or :CASH-ANNUITY")
  (let ((exercise (if (typep exercise 'date)
                      (make-instance 'european-exercise :expiry exercise)
                      exercise)))
    (check-input exercise (or european-exercise bermudan-exercise)
                 "a European or Bermudan exercise, or an exercise date")
    (make-instance 'swaption :swap swap :exercise exercise :settlement settlement
                             :currency (instrument-currency swap))))

;;; --------------------------------------------------------------------
;;; Callable bonds
;;; --------------------------------------------------------------------

(defclass callable-fixed-rate-bond (fixed-rate-bond)
  ((call-schedule :initarg :call-schedule :reader bond-call-schedule
                  :documentation "Entries (date clean-price type), TYPE :CALL (the
issuer may redeem at the price) or :PUT (the holder may), by date."))
  (:metaclass component-class)
  (:documentation "A fixed-rate bond with call or put dates: on each, the
bond may be redeemed at a clean price per 100 of face plus accrued interest
(QuantLib's CallableFixedRateBond). Build with
MAKE-CALLABLE-FIXED-RATE-BOND; price with a lattice engine."))

(defun make-callable-fixed-rate-bond (schedule coupon day-counter call-schedule &rest keys
                                      &key &allow-other-keys)
  "A fixed-rate bond as MAKE-FIXED-RATE-BOND (same keys) callable or
puttable on CALL-SCHEDULE, a list of (date clean-price) or (date clean-price
type), TYPE :CALL (default) or :PUT, prices per 100 of face.

  (make-callable-fixed-rate-bond schedule 0.03d0 dc `((,#D\"2029-09-28\" 100) (,#D\"2030-09-30\" 100))
                                 :currency :eur)"
  (check-input call-schedule (and list (not null)) "a non-empty list of (date clean-price [type])")
  (let ((bond (apply #'make-fixed-rate-bond schedule coupon day-counter keys))
        (calls (mapcar (lambda (entry)
                         (destructuring-bind (date price &optional (type :call)) entry
                           (check-input date date "a call date")
                           (check-input price real "a clean price per 100 of face")
                           (check-input type (member :call :put) ":CALL or :PUT")
                           `(,date ,(float price 1d0) ,type)))
                       call-schedule)))
    (change-class bond 'callable-fixed-rate-bond
                  :call-schedule (sort calls #'date< :key #'first))))
