;;;; rates.lisp --- curves, bonds, swaps, caps and swaptions
;;;;
;;;; The curves of tests/fixtures/bootstrap.sexp, valued on 2026-09-24: SOFR
;;;; and ESTR from 21 OIS quotes, EURIBOR 6M from deposits, FRAs and swaps
;;;; discounted on ESTR. The C++ side builds them with fixture_curves.hpp.
;;;; Bootstrap rows time the bootstrap itself; the others price on curves
;;;; built in the setup.
;;;;
;;;;   benchmark                   QuantLib                                    fincl
;;;;   bootstrap-ois-log-linear    PiecewiseYieldCurve<Discount, LogLinear>    bootstrap-curve :log-linear
;;;;   bootstrap-ois-monotone      PiecewiseYieldCurve<ForwardRate, ConvexMonotone>  :monotone-convex
;;;;   bootstrap-euribor-on-estr   PiecewiseYieldCurve, ESTR discounting       bootstrap-curve :discount-curve
;;;;   bond-clean-price            BondFunctions::cleanPrice (10Y Treasury)    bond-clean-price
;;;;   bond-yield                  BondFunctions::yield (Brent, 1e-14)         bond-yield
;;;;   swap-npv-fair-rate          VanillaSwap + DiscountingSwapEngine         :discounting, fair-rate
;;;;   cap-black                   Cap + BlackCapFloorEngine (5Y semiannual)   :black-rates
;;;;   swaption-black              Swaption + BlackSwaptionEngine (1Y x 5Y)    :black-rates
;;;;   swaption-hw-jamshidian      JamshidianSwaptionEngine (1Y x 9Y)          :jamshidian
;;;;   swaption-bermudan-hw-tree   TreeSwaptionEngine, 100 steps               :hull-white-tree, 100 steps

(in-package #:fincl/benchmarks)
(named-readtables:in-readtable fincl:syntax)

(defparameter *ois-tenors*
  '("1W" "2W" "1M" "2M" "3M" "4M" "6M" "9M" "1Y" "18M" "2Y" "3Y" "4Y" "5Y" "7Y" "10Y" "12Y"
    "15Y" "20Y" "25Y" "30Y"))

(defparameter *sofr-quotes*
  '(3.85d0 3.84d0 3.82d0 3.79d0 3.76d0 3.72d0 3.66d0 3.57d0 3.50d0 3.38d0 3.30d0 3.24d0 3.25d0
    3.28d0 3.36d0 3.48d0 3.55d0 3.62d0 3.68d0 3.66d0 3.62d0)
  "In percent.")

(defparameter *estr-quotes*
  '(1.93d0 1.93d0 1.92d0 1.91d0 1.90d0 1.89d0 1.88d0 1.88d0 1.89d0 1.93d0 1.98d0 2.07d0 2.15d0
    2.23d0 2.37d0 2.53d0 2.61d0 2.68d0 2.70d0 2.63d0 2.55d0)
  "In percent.")

(defparameter *euribor-quotes*
  '((:deposit "6M" 0.0210d0) (:fra "1" 0.0208d0) (:fra "2" 0.0206d0) (:fra "3" 0.0205d0)
    (:fra "6" 0.0204d0) (:fra "9" 0.0206d0) (:fra "12" 0.0210d0) (:swap "2Y" 0.0215d0)
    (:swap "3Y" 0.0222d0) (:swap "4Y" 0.0230d0) (:swap "5Y" 0.0237d0) (:swap "6Y" 0.0243d0)
    (:swap "7Y" 0.0249d0) (:swap "8Y" 0.0254d0) (:swap "9Y" 0.0258d0) (:swap "10Y" 0.0262d0)
    (:swap "12Y" 0.0269d0) (:swap "15Y" 0.0276d0) (:swap "20Y" 0.0279d0) (:swap "25Y" 0.0274d0)
    (:swap "30Y" 0.0268d0)))

(defparameter *curve-end* #D"2056-09-24"
  "Thirty years out: every curve's value is its discount factor here.")

(defun quote-sum (quotes) (reduce #'+ quotes))

(defun ois-helpers (quotes index payment-lag)
  (mapcar (lambda (tenor q)
            (ois-helper (/ q 100) (parse-tenor tenor) index :valuation-date *today*
                                                            :payment-lag payment-lag))
          *ois-tenors* quotes))

(defun sofr-helpers () (ois-helpers *sofr-quotes* (sofr) 2))
(defun estr-helpers () (ois-helpers *estr-quotes* (estr) 1))

(defun euribor-helpers ()
  (iter (for (kind tenor quote) in *euribor-quotes*)
        (collect (ecase kind
                   (:deposit (deposit-helper quote (euribor #T"6M") :valuation-date *today*))
                   (:fra (fra-helper quote (parse-integer tenor) (euribor #T"6M") :valuation-date *today*))
                   (:swap (swap-helper quote (parse-tenor tenor) (euribor #T"6M") :valuation-date *today*
                                                                                 :fixed-convention :unadjusted))))))

(defun curve-result (curve helpers)
  "A bootstrap's value (the discount factor 30 years out) and details (the
discount factors at the pillars)."
  (values (curve-discount curve *curve-end*) nil
          `(:pillar-discounts ,(mapcar (lambda (h) (curve-discount curve (helper-pillar-date h))) helpers))))

(defun rates-market (&key (eur-discount :estr))
  "SOFR, ESTR and EURIBOR 6M (on ESTR), log-linear; USD discounted on SOFR,
EUR on EUR-DISCOUNT."
  (let* ((sofr (bootstrap-curve (sofr-helpers)))
         (estr (bootstrap-curve (estr-helpers)))
         (euribor (bootstrap-curve (euribor-helpers) :discount-curve estr)))
    (make-market :valuation-date *today*
                 :curves `((:sofr :curve ,sofr) (:estr :curve ,estr) (:euribor-6m :curve ,euribor))
                 :discounting `((:usd . :sofr) (:eur . ,eur-discount)))))

(defun euro-swap (type nominal rate start end &key (spread 0))
  (make-vanilla-swap type nominal (make-schedule start end #T"1Y" :calendar +target+)
                     rate (make-instance 'thirty-360 :variant :bond-basis)
                     (make-schedule start end #T"6M" :calendar +target+) (euribor #T"6M")
                     :spread spread :floating-day-counter +actual-360+))

(defun treasury ()
  "The 10Y Treasury: 4.375% semiannual, ACT/ACT (ICMA), T+1."
  (let ((schedule (make-schedule #D"2024-05-15" #D"2034-05-15" #T"6M"
                                 :calendar +united-states-government-bond+ :convention :unadjusted
                                 :termination-convention :unadjusted :rule :backward)))
    (values (make-fixed-rate-bond schedule 0.04375d0 (make-instance 'actual-actual-icma :schedule schedule)
                                  :face 100 :currency :usd :settlement-days 1 :payment-convention :following)
            (make-instance 'actual-actual-icma :schedule schedule))))

(defparameter *hull-white-resets*
  (list #D"2027-09-28" #D"2028-09-28" #D"2029-09-28" #D"2030-09-30" #D"2031-09-29"))

(define-benchmark "bootstrap-ois-log-linear"
    (:group :rates :title "SOFR curve from 21 OIS, log-linear discounts" :engine "bootstrap-curve"
     :inputs `(:helpers 21 :quote-sum ,(quote-sum *sofr-quotes*))
     :tolerance 1d-10)
    ((helpers (sofr-helpers)))
  (curve-result (bootstrap-curve helpers :interpolation :log-linear) helpers))

(define-benchmark "bootstrap-ois-monotone"
    (:group :rates :title "SOFR curve from 21 OIS, monotone-convex forwards" :engine "bootstrap-curve"
     :inputs `(:helpers 21 :quote-sum ,(quote-sum *sofr-quotes*))
     :tolerance 1d-10
     :notes "monotone convex is not local: both iterate whole-curve passes until
no node moves by 1e-12")
    ((helpers (sofr-helpers)))
  (curve-result (bootstrap-curve helpers :interpolation :monotone-convex) helpers))

(define-benchmark "bootstrap-euribor-on-estr"
    (:group :rates :title "EURIBOR 6M curve, 21 helpers, ESTR discounting" :engine "bootstrap-curve"
     :inputs `(:helpers 21 :quote-sum ,(quote-sum (mapcar #'third *euribor-quotes*)))
     :tolerance 1d-10
     :notes "the ESTR curve is built in the setup")
    ((estr (bootstrap-curve (estr-helpers))) (helpers (euribor-helpers)))
  (curve-result (bootstrap-curve helpers :interpolation :log-linear :discount-curve estr) helpers))

(define-benchmark "bond-clean-price"
    (:group :rates :title "10Y Treasury clean price on the SOFR curve" :engine "bond-clean-price"
     :inputs '(:coupon 0.04375d0 :maturity "2034-05-15" :settlement-days 1))
    ((m (rates-market)) (bond (treasury)))
  (bond-clean-price bond m))

(define-benchmark "bond-yield"
    (:group :rates :title "10Y Treasury yield at a clean price of 99.5" :engine "bond-yield"
     :inputs '(:coupon 0.04375d0 :maturity "2034-05-15" :settlement-days 1 :clean-price 99.5d0)
     :tolerance 1d-12
     :notes "Brent on both sides, QuantLib to an accuracy of 1e-14, fincl to a
tolerance of 1e-14")
    ((bond (treasury)) (dc (nth-value 1 (treasury))))
  (bond-yield bond 99.5d0 dc :compounded :semiannual :valuation-date *today* :tolerance 1d-14))

(define-benchmark "swap-npv-fair-rate"
    (:group :rates :title "10Y EUR receiver swap, NPV and fair rate" :engine "discounting"
     :inputs '(:nominal 5d6 :fixed-rate 0.026d0 :spread 0.001d0 :start "2026-09-28" :end "2036-09-29")
     :tolerance 5d-4
     :notes "tolerance 1e-10 per unit nominal, the fixtures'; the value is the NPV on 5,000,000; QuantLib computes the fair rate in
the same calculation, fincl in a second pass")
    ((m (rates-market))
     (swap (euro-swap :receiver 5d6 0.026d0 #D"2026-09-28" #D"2036-09-29" :spread 0.001d0))
     (e (make-engine :discounting)))
  (values (price swap e m) nil `(:fair-rate ,(fair-rate swap m))))

(define-benchmark "cap-black"
    (:group :rates :title "5Y cap at 2.5% on EURIBOR 6M, Black 25%" :engine "black-rates"
     :inputs '(:nominal 1d6 :cap-rate 0.025d0 :vol 0.25d0 :start "2026-09-28" :end "2031-09-29")
     :tolerance 1d-4 :notes "tolerance 1e-10 per unit nominal, the fixtures'")
    ((m (rates-market))
     (cap (make-cap-floor :cap (floating-leg (make-schedule #D"2026-09-28" #D"2031-09-29" #T"6M"
                                                            :calendar +target+)
                                             1d6 (euribor #T"6M"))
                          :cap-rate 0.025d0))
     (e (make-engine :black-rates :volatility 0.25d0)))
  (priced cap e m))

(define-benchmark "swaption-black"
    (:group :rates :title "1Y into 5Y payer swaption at 2.5%, Black 20%" :engine "black-rates"
     :inputs '(:nominal 1d6 :fixed-rate 0.025d0 :vol 0.2d0 :exercise "2027-09-24")
     :tolerance 1d-4 :notes "tolerance 1e-10 per unit nominal, the fixtures'")
    ((m (rates-market))
     (swaption (make-swaption (euro-swap :payer 1d6 0.025d0 #D"2027-09-28" #D"2032-09-28") #D"2027-09-24"))
     (e (make-engine :black-rates :volatility 0.2d0)))
  (priced swaption e m))

(define-benchmark "swaption-hw-jamshidian"
    (:group :rates :title "1Y into 9Y payer swaption at 2.7%, Hull-White" :engine "jamshidian"
     :inputs '(:nominal 1d6 :fixed-rate 0.027d0 :a 0.05d0 :sigma 0.01d0 :exercise "2027-09-28")
     :tolerance 1d-6
     :notes "Hull-White fitted to the EURIBOR 6M curve, which also discounts")
    ((m (rates-market :eur-discount :euribor-6m))
     (swaption (make-swaption (euro-swap :payer 1d6 0.027d0 (first *hull-white-resets*) #D"2036-09-28")
                              (first *hull-white-resets*)))
     (e (make-engine :jamshidian :process (make-hull-white :a 0.05d0 :sigma 0.01d0))))
  (priced swaption e m))

(define-benchmark "swaption-bermudan-hw-tree"
    (:group :rates :title "Bermudan payer swaption at 2.7%, annual from 1Y to 10Y, Hull-White tree"
     :engine "hull-white-tree"
     :inputs '(:nominal 1d6 :fixed-rate 0.027d0 :a 0.05d0 :sigma 0.01d0 :exercise-dates 5)
     :work '(:time-steps 100)
     :tolerance 1d-4
     :notes "fincl reproduces QuantLib's trinomial tree and time grid; tolerance 1e-10
per unit nominal")
    ((m (rates-market :eur-discount :euribor-6m))
     (swaption (make-swaption (euro-swap :payer 1d6 0.027d0 (first *hull-white-resets*) #D"2036-09-28")
                              (make-instance 'bermudan-exercise :dates *hull-white-resets*)))
     (e (make-engine :hull-white-tree :process (make-hull-white :a 0.05d0 :sigma 0.01d0) :time-steps 100)))
  (priced swaption e m))
