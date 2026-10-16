;;;; engines/rates-analytic.lisp --- Black and Bachelier caps and swaptions
;;;;
;;;; A caplet is an option on its coupon's fixing, struck at the cap rate and
;;;; paid on the discounted accrual; a European swaption is an option on the
;;;; forward swap rate, paid on the annuity. Both engines read a rates vol
;;;; structure at the option's expiry: BLACK-RATES-ENGINE as a (shifted)
;;;; lognormal vol, BACHELIER-RATES-ENGINE as a normal vol. They follow
;;;; QuantLib's BlackCapFloorEngine, BachelierCapFloorEngine and
;;;; BlackStyleSwaptionEngine (tests/fixtures/caps-swaptions.sexp).

(in-package #:fincl)

(defclass volatility-rates-engine (rates-engine)
  ((volatility :initarg :volatility :reader engine-volatility
               :initform (a:required-argument :volatility)
               :documentation "A rates vol structure, or a real for a flat vol.")
   (discount-curve :initarg :discount-curve :initform nil :reader engine-discount-curve
                   :type (or null keyword)
                   :documentation "The id of the curve to discount on; NIL: the
curve of the instrument's currency."))
  (:metaclass engine-class)
  (:documentation "A closed-form rates engine reading a volatility at each
option's expiry."))

(defmethod initialize-instance :after ((e volatility-rates-engine) &key)
  (setf (slot-value e 'volatility) (ensure-rates-vol (slot-value e 'volatility))))

(defclass black-rates-engine (volatility-rates-engine)
  ((displacement :initarg :displacement :initform 0d0 :reader engine-displacement
                 :documentation "Added to rate and strike: the vol is lognormal in
rate + displacement."))
  (:metaclass engine-class)
  (:designator :black-rates)
  (:documentation "Black's formula (shifted lognormal) for caps, floors,
collars and European swaptions.

  (price cap (make-engine :black-rates :volatility 0.25d0) market)
  (price swaption (make-engine :black-rates :volatility 0.12d0 :displacement 0.02d0) market)"))

(defmethod initialize-instance :after ((e black-rates-engine) &key)
  (check-input (slot-value e 'displacement) (real 0) "a non-negative displacement")
  (setf (slot-value e 'displacement) (float (slot-value e 'displacement) 1d0)))

(defclass bachelier-rates-engine (volatility-rates-engine) ()
  (:metaclass engine-class)
  (:designator :bachelier-rates)
  (:documentation "Bachelier's formula (normal vols) for caps, floors,
collars and European swaptions.

  (price swaption (make-engine :bachelier-rates :volatility 0.008d0) market)"))

(defgeneric rates-option-value (engine phi strike forward std-dev annuity)
  (:documentation "The value of a call (PHI 1) or put (PHI -1) on a rate with
FORWARD at STRIKE, total standard deviation STD-DEV, paid on ANNUITY.")
  (:method ((e black-rates-engine) phi strike forward std-dev annuity)
    ;; QuantLib's blackFormula, operation for operation.
    (if (zerop std-dev)
        (* (max (* (- forward strike) phi) 0d0) annuity)
        (let ((f (+ forward (engine-displacement e)))
              (k (+ strike (engine-displacement e))))
          (if (zerop k)
              (if (plusp phi) (* f annuity) 0d0)
              (let* ((d1 (+ (/ (log (/ f k)) std-dev) (* 0.5d0 std-dev)))
                     (d2 (- d1 std-dev)))
                (* annuity phi (- (* f (norm-cdf (* phi d1))) (* k (norm-cdf (* phi d2))))))))))
  (:method ((e bachelier-rates-engine) phi strike forward std-dev annuity)
    (if (zerop std-dev)
        (* annuity (max (* (- forward strike) phi) 0d0))
        (bachelier-formula phi forward strike annuity (* std-dev std-dev)))))

(defun rates-std-dev (engine market date)
  "The standard deviation of the rate to the expiry DATE: 0 on or before the
valuation date."
  (let ((today (valuation-date market)))
    (if (date< today date)
        (sqrt (rates-vol-variance (engine-volatility engine) today date))
        0d0)))

;;; --------------------------------------------------------------------
;;; Caps and floors
;;; --------------------------------------------------------------------

(defun optionlet-values (cap engine market)
  "The value of each of CAP's optionlets, a list; 0 for those already paid."
  (let ((curve (rates-discount-curve market (instrument-currency cap) (engine-discount-curve engine)))
        (today (valuation-date market))
        (type (cap-floor-type cap)))
    (iter (for coupon in (cap-floor-leg cap))
          (for i from 0)
          (collect
              (if (date< today (payment-date coupon))
                  (let* ((gearing (coupon-gearing coupon))
                         (spread (coupon-spread coupon))
                         (annuity (* (market-curve-discount market curve (payment-date coupon))
                                     (* (coupon-nominal coupon) gearing (accrual-period coupon))))
                         (forward (caplet-forward coupon market))
                         (std-dev (rates-std-dev engine market (coupon-fixing-date coupon))))
                    (flet ((optionlet (phi rate)
                             (rates-option-value engine phi (/ (- rate spread) gearing) forward
                                                 std-dev annuity)))
                      (ecase type
                        (:cap (optionlet 1 (nth i (cap-rates cap))))
                        (:floor (optionlet -1 (nth i (floor-rates cap))))
                        (:collar (- (optionlet 1 (nth i (cap-rates cap)))
                                    (optionlet -1 (nth i (floor-rates cap))))))))
                  0d0)))))

(defun cap-floor-optionlet-prices (cap engine market)
  "The value of each optionlet of CAP under ENGINE (a Black or Bachelier
rates engine) on MARKET, as a list.

  (cap-floor-optionlet-prices cap (make-engine :black-rates :volatility 0.25d0) market)"
  (check-input cap cap-floor "a cap, floor or collar")
  (check-input engine volatility-rates-engine "a Black or Bachelier rates engine")
  (check-input market composite-market "a composite market, which holds the curves")
  (optionlet-values cap engine market))

(defmethod price ((c cap-floor) (e volatility-rates-engine) market)
  (values (iter (for v in (optionlet-values c e market)) (sum v)) nil))

;;; --------------------------------------------------------------------
;;; European swaptions
;;; --------------------------------------------------------------------

(defun tenor-frequency (tenor)
  "The frequency keyword of coupons every TENOR: 1Y is :ANNUAL, 6M :SEMIANNUAL."
  (let ((months (ecase (tenor-unit tenor)
                  (:years (* 12 (tenor-n tenor)))
                  (:months (tenor-n tenor))
                  ((:weeks :days) nil))))
    (or (and months (plusp months) (zerop (mod 12 months)) (periods-frequency (/ 12 months)))
        (signal-invalid-argument 'tenor tenor 'tenor "a tenor dividing a year into whole months"))))

(defun swaption-annuity (swaption swap market curve fixed-bps atm)
  "What one unit of swap rate is worth at exercise: the fixed leg's BPS per
basis point, physically settled; for cash settlement the fixed leg discounted
at ATM, compounded at the fixed frequency from the swap's start, times the
discount factor to the start."
  (ecase (swaption-settlement swaption)
    (:physical (/ (abs fixed-bps) 1d-4))
    (:cash-annuity
     (let* ((fixed (first (swap-legs swap)))
            (start (accrual-start (first fixed)))
            (rate (make-interest-rate atm :day-counter (coupon-day-counter (first fixed))
                                          :compounding :compounded
                                          :frequency (tenor-frequency
                                                      (schedule-tenor (swap-fixed-schedule swap)))))
            (cash-bps (* 1d-4 (iter (for coupon in fixed)
                                    (unless (occurred-p coupon start)
                                      (sum (* (coupon-nominal coupon) (accrual-period coupon)
                                              (discount-factor-of rate start (payment-date coupon)))))))))
       (* (abs (/ cash-bps 1d-4)) (market-curve-discount market curve start))))))

(defmethod price ((s swaption) (e volatility-rates-engine) market)
  (let* ((swap (swaption-swap s))
         (exercise (swaption-exercise s))
         (date (expiry exercise))
         (fixed (first (swap-legs swap))))
    (unless (typep exercise 'european-exercise)
      (error 'unsupported-combination :exercise exercise :payoff s :path nil :engine e :market market))
    (unless (date<= date (accrual-start (first fixed)))
      (signal-invalid-argument 'swaption s 'swaption
                               (format nil "a swaption whose swap starts on or after its exercise ~
date ~A (the swap starts ~A)" date (accrual-start (first fixed)))))
    (let* ((curve (rates-discount-curve market (instrument-currency s) (engine-discount-curve e)))
           (fixed-bps (swap-leg-bps swap 0 market :discount-curve curve))
           (strike (swap-fixed-rate swap))
           (atm (fair-rate swap market :discount-curve curve))
           (spread (swap-spread swap)))
      ;; Vols are quoted for swaps without a floating spread: move it to the fixed side.
      (unless (zerop spread)
        (let ((correction (* spread (abs (/ (swap-leg-bps swap 1 market :discount-curve curve)
                                            fixed-bps)))))
          (decf strike correction)
          (decf atm correction)))
      (values (rates-option-value e (if (eq (swap-type swap) :payer) 1 -1) strike atm
                                  (rates-std-dev e market date)
                                  (swaption-annuity s swap market curve fixed-bps atm))
              nil))))
