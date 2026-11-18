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

(declaim (ftype (function (t) (values date &optional)) bond-maturity-date))
(defun bond-maturity-date (bond)
  "The date of BOND's redemption."
  (payment-date (a:lastcar (bond-cashflows bond))))

(declaim (ftype (function (t t) (values date &optional)) bond-settlement-date))
(defun bond-settlement-date (bond date)
  "The date a trade of BOND on DATE settles: SETTLEMENT-DAYS business days
later, but not before the issue date."
  (let ((settlement (advance (bond-calendar bond) date (make-tenor (bond-settlement-days bond) :days)))
        (issue (bond-issue-date bond)))
    (if (and issue (date< settlement issue)) issue settlement)))

(declaim (ftype (function (t t) (values double-float &optional)) bond-notional))
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

(declaim (ftype (function (t &key (:settlement t) (:valuation-date t))
                    (values double-float &optional))
          bond-accrued-amount))
(defun bond-accrued-amount (bond &key settlement valuation-date)
  "Interest accrued at SETTLEMENT (default: the settlement of a trade on
VALUATION-DATE) per 100 of face."
  (let ((settlement (resolve-settlement bond settlement valuation-date)))
    (per-100 bond (money-amount (accrued-amount (bond-cashflows bond) settlement)) settlement)))

(defun bond-market-settlement (bond market settlement)
  (check-input market composite-market "a composite market, which holds the curves")
  (resolve-settlement bond settlement (valuation-date market)))

(declaim (ftype (function (t t &key (:discount-curve t) (:settlement t))
                    (values double-float &optional))
          bond-dirty-price))
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

(declaim (ftype (function (t t &key (:discount-curve t) (:settlement t))
                    (values double-float &optional))
          bond-clean-price))
(defun bond-clean-price (bond market &key discount-curve settlement)
  "BOND-DIRTY-PRICE less the accrued interest."
  (let ((settlement (bond-market-settlement bond market settlement)))
    (- (bond-dirty-price bond market :discount-curve discount-curve :settlement settlement)
       (bond-accrued-amount bond :settlement settlement))))

(declaim (ftype (function (t t &key (:discount-curve t) (:settlement t))
                    (values double-float &optional))
          bond-bps))
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

(declaim (ftype (function (t t t t t &key (:settlement t) (:valuation-date t) (:clean t))
                    (values double-float &optional))
          bond-price-from-yield))
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

(declaim (ftype (function (t t t t t &key (:settlement t) (:valuation-date t) (:tolerance real))
                    (values double-float &optional))
          bond-yield))
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

(declaim (ftype (function (t t t t t &key (:type t) (:settlement t) (:valuation-date t))
                    (values double-float &optional))
          bond-duration))
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

(declaim (ftype (function (t t t t t &key (:settlement t) (:valuation-date t))
                    (values double-float &optional))
          bond-convexity))
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

(declaim (ftype (function (t t t t t &key (:discount-curve t) (:settlement t) (:clean t))
                    (values double-float &optional))
          bond-price-from-z-spread))
(defun bond-price-from-z-spread (bond spread market compounding frequency
                                 &key discount-curve settlement (clean t))
  "BOND's clean price (dirty if CLEAN is NIL) with SPREAD over the discount
curve's zero rates, quoted with COMPOUNDING and FREQUENCY."
  (let* ((settlement (bond-market-settlement bond market settlement))
         (curve (rates-discount-curve market (instrument-currency bond) discount-curve))
         (dirty (per-100 bond (z-spread-npv bond market curve spread compounding frequency settlement)
                         settlement)))
    (if clean (- dirty (bond-accrued-amount bond :settlement settlement)) dirty)))

(declaim (ftype (function (t t t t t &key (:discount-curve t) (:settlement t) (:tolerance real))
                    (values double-float &optional))
          z-spread))
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

(declaim (ftype (function (t t &key (:discount-curve t)) (values double-float &optional)) bond-npv))
(defun bond-npv (bond market &key discount-curve)
  "Value on MARKET's valuation date of BOND's cash flows paying after it."
  (let ((curve (market-discount-curve market (rates-discount-curve market (instrument-currency bond) discount-curve)))
        (today (valuation-date market)))
    ;; MARKET-CURVE-DISCOUNT's operations, with the curve resolved once.
    (iter (for cf in (bond-cashflows bond))
          (unless (occurred-p cf today)
            (sum (* (cashflow-amount cf market) (curve-discount curve (market-time market (payment-date cf)))))))))

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
   (fixed-rate :initarg :fixed-rate :reader swap-fixed-rate :type rate)
   (spread :initarg :spread :reader swap-spread :type rate)
   (index :initarg :index :reader swap-index)
   (fixed-schedule :initarg :fixed-schedule :reader swap-fixed-schedule)
   (floating-schedule :initarg :floating-schedule :reader swap-floating-schedule
                      :documentation "The floating leg's schedule; the fixed leg's for an OIS.")
   (fixed-day-counter :initarg :fixed-day-counter :reader swap-fixed-day-counter)
   (floating-day-counter :initarg :floating-day-counter :initform nil
                         :reader swap-floating-day-counter
                         :documentation "As given, or NIL for the index's.")
   (payment-convention :initarg :payment-convention :reader swap-payment-convention)
   (payment-lag :initarg :payment-lag :initform 0 :reader swap-payment-lag)
   (payment-calendar :initarg :payment-calendar :initform nil :reader swap-payment-calendar
                     :documentation "As given, or NIL for the schedule's."))
  (:metaclass component-class)
  (:documentation "A fixed leg (leg 0) against a floating leg (leg 1) on the
same nominal. A payer swap pays the fixed leg."))

(defclass vanilla-swap (fixed-vs-floating-swap) ()
  (:metaclass component-class)
  (:documentation "Fixed against Ibor coupons (QuantLib's VanillaSwap). Build
with MAKE-SWAP and an Ibor index (or MAKE-VANILLA-SWAP)."))

(defclass ois-swap (fixed-vs-floating-swap) ()
  (:metaclass component-class)
  (:documentation "Fixed against daily compounded overnight coupons on one
schedule (QuantLib's OvernightIndexedSwap). Build with MAKE-SWAP and an
overnight index (or MAKE-OIS-SWAP)."))

(defun swap-signs-for (type)
  (check-input type swap-type ":PAYER (pays the fixed leg) or :RECEIVER")
  (if (eq type :payer) '(-1d0 1d0) '(1d0 -1d0)))

(declaim (ftype (function (t (integer 0) t &key (:discount-curve t))
                    (values double-float &optional))
          swap-leg-npv))
(defun swap-leg-npv (swap i market &key discount-curve)
  "Signed value of SWAP's leg I on MARKET: positive for a leg received."
  (* (nth i (swap-signs swap))
     (money-amount (leg-npv (nth i (swap-legs swap)) market
                            :discount-curve (rates-discount-curve market (instrument-currency swap)
                                                                  discount-curve)))))

(declaim (ftype (function (t (integer 0) t &key (:discount-curve t))
                    (values double-float &optional))
          swap-leg-bps))
(defun swap-leg-bps (swap i market &key discount-curve)
  "Signed value of one basis point on the coupons of SWAP's leg I."
  (* (nth i (swap-signs swap))
     (money-amount (leg-bps (nth i (swap-legs swap)) market
                            :discount-curve (rates-discount-curve market (instrument-currency swap)
                                                                  discount-curve)))))

(declaim (ftype (function (t t &key (:discount-curve t)) (values double-float &optional)) swap-npv))
(defun swap-npv (swap market &key discount-curve)
  "SWAP's value on MARKET: the sum of its signed leg values."
  (iter (for i below (length (swap-legs swap)))
        (sum (swap-leg-npv swap i market :discount-curve discount-curve))))

(declaim (ftype (function (t t &key (:discount-curve t))
                    (values double-float &optional))
          fair-rate))
(defun fair-rate (swap market &key discount-curve)
  "The fixed rate at which SWAP is worth zero.

  (fair-rate swap market)"
  (- (swap-fixed-rate swap)
     (/ (swap-npv swap market :discount-curve discount-curve)
        (/ (swap-leg-bps swap 0 market :discount-curve discount-curve) 1d-4))))

(declaim (ftype (function (t t &key (:discount-curve t))
                    (values double-float &optional))
          fair-spread))
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
   (strike :initarg :strike :reader fra-strike :type rate)
   (notional :initarg :notional :reader fra-notional :type double-float)
   (indexed :initarg :indexed :reader fra-indexed-p
            :documentation "T: the rate is the index fixing; NIL: the forward from
the value date to the maturity date."))
  (:metaclass component-class)
  (:documentation "A forward rate agreement (QuantLib's
ForwardRateAgreement): on the value date the long side receives
notional (F - K) T / (1 + F T), F the rate for the period and T its length
under the index's day counter. Build with MAKE-FORWARD-RATE-AGREEMENT."))

(declaim (ftype (function (t t) (values double-float &optional)) fra-forward-rate))
(defun fra-forward-rate (fra market)
  "The rate FRA settles on: the index fixing, or the forecast forward."
  (let ((index (fra-index fra)))
    (if (fra-indexed-p fra)
        (index-fixing index market (index-fixing-date index (fra-value-date fra)))
        (index-forecast index market (fra-value-date fra) (fra-maturity-date fra)))))

(declaim (ftype (function (t t) (values double-float &optional)) fra-amount))
(defun fra-amount (fra market)
  "What FRA pays on its value date."
  (let ((f (fra-forward-rate fra market))
        (k (fra-strike fra))
        (tau (year-fraction (index-day-counter (fra-index fra)) (fra-value-date fra)
                            (fra-maturity-date fra))))
    (/ (* (fra-notional fra) (if (eq (fra-position fra) :long) 1 -1) (- f k) tau)
       (+ 1d0 (* f tau)))))

(declaim (ftype (function (t t &key (:discount-curve t)) (values double-float &optional)) fra-npv))
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

;;; --------------------------------------------------------------------
;;; Fixing dates (task 32)
;;; --------------------------------------------------------------------

(defgeneric cashflow-fixing-dates (cashflow)
  (:documentation "The dates CASHFLOW's rate fixes on, a list.")
  (:method ((c cashflow)) '())
  (:method ((c ibor-coupon)) `(,(coupon-fixing-date c)))
  (:method ((c overnight-indexed-coupon)) (coerce (overnight-fixing-dates c) 'list)))

(defgeneric instrument-fixing-dates (instrument)
  (:documentation "Every date INSTRUMENT's floating rates fix on, unsorted.")
  (:method ((i rates-instrument)) '())
  (:method ((s swap)) (iter (for leg in (swap-legs s)) (appending (mapcan #'cashflow-fixing-dates leg))))
  (:method ((s swaption)) (instrument-fixing-dates (swaption-swap s)))
  (:method ((c cap-floor)) (mapcan #'cashflow-fixing-dates (cap-floor-leg c)))
  (:method ((f forward-rate-agreement))
    (when (fra-indexed-p f) `(,(index-fixing-date (fra-index f) (fra-value-date f))))))

(declaim (ftype (function (t &optional t) (values list &optional)) fixing-dates))
(defun fixing-dates (instrument &optional market)
  "The dates the floating rates of the rates INSTRUMENT fix on, ascending
and distinct; with MARKET, only those after its valuation date. A SIMULATE
run that prices INSTRUMENT along its paths needs them among its dates, with
the index in :FIX.

  (fixing-dates swap m)  ; => (#D\"2027-03-24\" #D\"2027-09-23\" ...)"
  (check-input instrument rates-instrument "a rates instrument")
  (let ((dates (remove-duplicates (sort (copy-list (instrument-fixing-dates instrument)) #'date<)
                                  :test #'date=)))
    (if market
        (remove-if-not (lambda (d) (date< (valuation-date market) d)) dates)
        dates)))
