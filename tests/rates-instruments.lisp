;;;; rates-instruments.lisp --- bonds, swaps, FRAs, caps, swaptions and
;;;; short-rate models (task 16), against QuantLib-generated fixtures

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun bootstrap-fixture-curves ()
  "The log-linear SOFR, ESTR and EURIBOR 6M (on ESTR) curves of
bootstrap.sexp, bootstrapped by fincl: ((:sofr . curve) (:estr . curve)
(:euribor-6m . curve))."
  (let ((today #D"2026-09-24") (built '()))
    (dolist (spec (rest (assoc :curves (read-fixture "bootstrap.sexp"))))
      (destructuring-bind (&key name interpolation discount-curve helpers &allow-other-keys) spec
        (when (eq interpolation :log-linear)
          (push (cons name (bootstrap-curve (mapcar (lambda (row) (fixture-helper row today)) helpers)
                                            :interpolation :log-linear
                                            :discount-curve (cdr (assoc discount-curve built))))
                built))))
    built))

(defvar *rates-market* nil)

(defun rates-market ()
  "A composite market on 2026-09-24 holding the bootstrap fixture's curves:
USD discounted on :SOFR, EUR on :ESTR, EURIBOR 6M forecast on :EURIBOR-6M."
  (or *rates-market*
      (setf *rates-market*
            (let ((curves (bootstrap-fixture-curves)))
              (make-market :valuation-date #D"2026-09-24"
                           :curves (mapcar (lambda (c) `(,(car c) :curve ,(cdr c))) curves)
                           :discounting '((:usd . :sofr) (:eur . :estr)))))))

(defun fixture-schedule (spec)
  (destructuring-bind (&key effective termination tenor calendar convention termination-convention
                            rule end-of-month first-date)
      spec
    (make-schedule (fixture-date effective) (fixture-date termination) (parse-tenor tenor)
                   :calendar (fixture-calendar calendar) :convention convention
                   :termination-convention termination-convention :rule rule
                   :end-of-month end-of-month
                   :first-date (and first-date (fixture-date first-date)))))

(defun bond-day-counter (name schedule)
  (if (eq name :actual-actual-icma)
      (make-instance 'actual-actual-icma :schedule schedule)
      (rates-day-counter name)))

(defun fixture-bond (spec)
  (destructuring-bind (&key schedule coupon day-counter face settlement-days payment-convention
                            redemption issue-date currency &allow-other-keys)
      spec
    (let ((s (fixture-schedule schedule)))
      (make-fixed-rate-bond s coupon (bond-day-counter day-counter s)
                            :face face :currency currency :settlement-days settlement-days
                            :payment-convention payment-convention :redemption redemption
                            :issue-date (and issue-date (fixture-date issue-date))))))

(defmacro collect-worst ((&rest names) &body body)
  "Bind each of NAMES to a list collector and return, after BODY, a plist of
the worst |expected - actual| per name. In BODY, (record name expected actual)."
  `(let ,(mapcar (lambda (n) `(,n '())) names)
     (flet ((record (name expected actual)
              (ecase name
                ,@(mapcar (lambda (n) `(,n (push (cons expected actual) ,n))) names))))
       ,@body
       (list ,@(mapcan (lambda (n) `(',n (worst ,n))) names)))))

(defun bond-checks ()
  (format t "~&Bonds: QuantLib's FixedRateBond and BondFunctions, 10 bonds~%")
  (let* ((m (rates-market))
         (fixture (getf (first (read-fixture "bonds-swaps.sexp")) :bonds))
         (dates-ok t)
         (worst
           (collect-worst (accrued npv clean dirty bps yield yield-price durations convexity
                                   z-spread z-price cashflows)
             (dolist (spec fixture)
               (let* ((bond (fixture-bond spec))
                      (settlement (fixture-date (getf spec :settlement)))
                      (dc (bond-day-counter (getf spec :yield-day-counter) (bond-schedule bond)))
                      (freq (getf spec :yield-frequency))
                      (y (getf spec :yield)))
                 (unless (and (date= settlement (bond-settlement-date bond (valuation-date m)))
                              (= (getf spec :notional) (bond-notional bond settlement))
                              (= (length (getf spec :cashflows)) (length (bond-cashflows bond)))
                              (every (lambda (row cf) (date= (fixture-date (first row)) (payment-date cf)))
                                     (getf spec :cashflows) (bond-cashflows bond)))
                   (setf dates-ok nil)
                   (format t "~&  ~A: settlement, notional or cash-flow dates differ~%" (getf spec :name)))
                 (iter (for (nil amount) in (getf spec :cashflows))
                       (for cf in (bond-cashflows bond))
                       (record 'cashflows amount (cashflow-amount cf m)))
                 (record 'accrued (getf spec :accrued) (bond-accrued-amount bond :valuation-date #D"2026-09-24"))
                 ;; Per 100 of face, like the prices.
                 (record 'npv (/ (* 100 (getf spec :npv)) (getf spec :face))
                         (/ (* 100 (price bond :discounting m)) (bond-face bond)))
                 (record 'clean (getf spec :clean) (bond-clean-price bond m))
                 (record 'dirty (getf spec :dirty) (bond-dirty-price bond m))
                 (record 'bps (getf spec :bps) (bond-bps bond m))
                 (record 'yield y (bond-yield bond (getf spec :price) dc :compounded freq
                                              :settlement settlement))
                 (record 'yield-price (getf spec :yield-clean)
                         (bond-price-from-yield bond y dc :compounded freq :settlement settlement))
                 (record 'yield-price (getf spec :yield-dirty)
                         (bond-price-from-yield bond y dc :compounded freq :settlement settlement
                                                                           :clean nil))
                 (dolist (type '(:modified :macaulay :simple))
                   (record 'durations (getf spec type)
                           (bond-duration bond y dc :compounded freq :type type :settlement settlement)))
                 (record 'convexity (getf spec :convexity)
                         (bond-convexity bond y dc :compounded freq :settlement settlement))
                 (record 'z-spread (getf spec :z-spread)
                         (z-spread bond (getf spec :price) m :compounded freq))
                 (record 'z-price (getf spec :z-clean)
                         (bond-price-from-z-spread bond (getf spec :z-spread) m :compounded freq)))))))
    (check-that "settlement dates, notionals and cash-flow dates match QuantLib" dates-ok)
    (iter (for (name value) on worst by #'cddr)
          (check-that (format nil "~(~A~) within 1e-10 of QuantLib, per 100 of face (worst ~,1E)"
                              name value)
                      (<= value 1d-10)))))

(defun fixture-swap (spec)
  (destructuring-bind (&key kind type nominal fixed-rate spread start end payment-lag &allow-other-keys)
      spec
    (let ((start (fixture-date start)) (end (fixture-date end)))
      (ecase kind
        (:vanilla
         (make-vanilla-swap type nominal
                            (make-schedule start end #T"1Y" :calendar +target+)
                            fixed-rate (make-instance 'thirty-360 :variant :bond-basis)
                            (make-schedule start end #T"6M" :calendar +target+)
                            (euribor #T"6M")
                            :spread spread :floating-day-counter +actual-360+))
        (:ois
         (make-ois-swap type nominal (make-schedule start end #T"1Y" :calendar +united-states-sofr+)
                        fixed-rate +actual-360+ (sofr) :spread spread :payment-lag payment-lag))))))

(defun swap-checks ()
  (format t "~&Swaps: QuantLib's VanillaSwap and OvernightIndexedSwap, 7 swaps~%")
  (let* ((m (rates-market))
         (worst
           (collect-worst (npv fair-rate fair-spread leg-npv leg-bps)
             (dolist (spec (getf (first (read-fixture "bonds-swaps.sexp")) :swaps))
               (let ((swap (fixture-swap spec))
                     (scale (getf spec :nominal)))
                 (record 'npv (/ (getf spec :npv) scale) (/ (price swap :discounting m) scale))
                 (record 'fair-rate (getf spec :fair-rate) (fair-rate swap m))
                 (record 'fair-spread (getf spec :fair-spread) (fair-spread swap m))
                 (record 'leg-npv (/ (getf spec :fixed-npv) scale) (/ (swap-leg-npv swap 0 m) scale))
                 (record 'leg-npv (/ (getf spec :floating-npv) scale) (/ (swap-leg-npv swap 1 m) scale))
                 (record 'leg-bps (/ (getf spec :fixed-bps) scale) (/ (swap-leg-bps swap 0 m) scale))
                 (record 'leg-bps (/ (getf spec :floating-bps) scale) (/ (swap-leg-bps swap 1 m) scale)))))))
    (iter (for (name value) on worst by #'cddr)
          (check-that (format nil "~(~A~) within 1e-10 of QuantLib~:[~;, per unit nominal~] (worst ~,1E)"
                              name (member name '(npv leg-npv leg-bps)) value)
                      (<= value 1d-10)))))

(defun fra-checks ()
  (format t "~&FRAs: QuantLib's ForwardRateAgreement, 4 FRAs~%")
  (let* ((m (rates-market))
         (worst
           (collect-worst (forward-rate npv)
             (dolist (spec (getf (first (read-fixture "bonds-swaps.sexp")) :fras))
               (destructuring-bind (&key value-date maturity type strike notional forward-rate npv
                                    &allow-other-keys)
                   spec
                 (let ((fra (make-forward-rate-agreement
                             (euribor #T"6M") (fixture-date value-date) type strike notional
                             :maturity-date (and maturity (fixture-date maturity)))))
                   (record 'forward-rate forward-rate (fra-forward-rate fra m))
                   (record 'npv (/ npv notional) (/ (price fra :discounting m) notional))))))))
    (iter (for (name value) on worst by #'cddr)
          (check-that (format nil "~(~A~) within 1e-12 of QuantLib, per unit notional (worst ~,1E)"
                              name value)
                      (<= value 1d-12)))))

;;; --------------------------------------------------------------------
;;; Phase 2: caps, floors, European swaptions
;;; --------------------------------------------------------------------

(defun caps-fixture () (first (read-fixture "caps-swaptions.sexp")))

(defun fixture-rates-engine (name)
  (ecase name
    (:black (make-engine :black-rates :volatility 0.25d0))
    (:shifted-black (make-engine :black-rates :volatility 0.15d0 :displacement 0.01d0))
    (:normal (make-engine :bachelier-rates :volatility 0.0085d0))
    (:black-per-expiry
     (let ((vols (getf (caps-fixture) :per-expiry-vols)))
       (make-engine :black-rates
                    :volatility (make-instance 'expiry-rates-vol
                                               :dates (mapcar (lambda (row) (fixture-date (first row))) vols)
                                               :vols (mapcar #'second vols)))))))

(defun fixture-cap-leg (spec)
  (destructuring-bind (&key nominal start end &allow-other-keys) spec
    (floating-leg (make-schedule (fixture-date start) (fixture-date end) #T"6M" :calendar +target+)
                  nominal (euribor #T"6M"))))

(defun fixture-cap (spec)
  (destructuring-bind (&key type cap-rate floor-rate &allow-other-keys) spec
    (make-cap-floor type (fixture-cap-leg spec) :cap-rate cap-rate :floor-rate floor-rate)))

(defun cap-checks ()
  (format t "~&Caps and floors: QuantLib's Black and Bachelier cap/floor engines, 5 x 4 vols~%")
  (let* ((m (rates-market))
         (worst
           (collect-worst (total optionlets)
             (dolist (spec (getf (caps-fixture) :caps))
               (let ((cap (fixture-cap spec)) (scale (getf spec :nominal)))
                 (iter (for (vol total optionlets) in (getf spec :prices))
                       (for engine = (fixture-rates-engine vol))
                       (record 'total (/ total scale) (/ (price cap engine m) scale))
                       (iter (for expected in optionlets)
                             (for actual in (cap-floor-optionlet-prices cap engine m))
                             (record 'optionlets (/ expected scale) (/ actual scale)))))))))
    (iter (for (name value) on worst by #'cddr)
          (check-that (format nil "~(~A~) within 1e-10 of QuantLib per unit nominal (worst ~,1E)" name value)
                      (<= value 1d-10))))
  ;; Cap - floor = a payer swap paying the strike on the floating schedule.
  (let* ((m (rates-market))
         (start #D"2026-09-28") (end #D"2031-09-29") (k 0.022d0)
         (schedule (make-schedule start end #T"6M" :calendar +target+))
         (leg (floating-leg schedule 1d6 (euribor #T"6M")))
         (swap (make-vanilla-swap :payer 1d6 schedule k +actual-360+ schedule (euribor #T"6M"))))
    (dolist (vol '(:black :normal :black-per-expiry))
      (let ((engine (fixture-rates-engine vol)))
        (check (format nil "cap - floor = payer swap, ~(~A~), per unit nominal" vol)
               (/ (price swap :discounting m) 1d6)
               (/ (- (price (make-cap-floor :cap leg :cap-rate k) engine m)
                     (price (make-cap-floor :floor leg :floor-rate k) engine m))
                  1d6)
               1d-10))))
  ;; Implied vols recover the vol that priced each caplet.
  (let* ((m (rates-market))
         (spec (first (getf (caps-fixture) :caps)))
         (cap (fixture-cap spec))
         (today (valuation-date m))
         (black-worst 0d0) (normal-worst 0d0))
    (iter (for coupon in (cap-floor-leg cap))
          (for black in (cap-floor-optionlet-prices cap (fixture-rates-engine :shifted-black) m))
          (for normal in (cap-floor-optionlet-prices cap (fixture-rates-engine :normal) m))
          (for tau = (year-fraction +actual-365-fixed+ today (fincl::coupon-fixing-date coupon)))
          (for annuity = (* (fincl::market-curve-discount m :estr (payment-date coupon))
                            (coupon-nominal coupon) (accrual-period coupon)))
          (for forward = (fincl::caplet-forward coupon m))
          ;; Options worth less than 1e-6 of their annuity carry no vol information.
          (when (and (> tau 0.4d0) (> black (* 1d-6 annuity)))
            (setf black-worst
                  (max black-worst
                       (abs (- 0.15d0 (implied-black-vol 1d0 black (+ forward 0.01d0) (+ 0.025d0 0.01d0)
                                                         annuity tau))))
                  normal-worst
                  (max normal-worst
                       (abs (- 0.0085d0 (implied-normal-vol 1d0 normal forward 0.025d0 annuity tau)))))))
    (check-that (format nil "implied shifted-Black vols recover 15% from caplet prices (worst ~,1E)"
                        black-worst)
                (< black-worst 1d-10))
    (check-that (format nil "implied normal vols recover 85bp from caplet prices (worst ~,1E)" normal-worst)
                (< normal-worst 1d-12))))

(defun fixture-swaption-swap (spec)
  (destructuring-bind (&key type fixed-rate spread nominal start end &allow-other-keys) spec
    (let ((start (fixture-date start)) (end (fixture-date end)))
      (make-vanilla-swap type nominal (make-schedule start end #T"1Y" :calendar +target+)
                         fixed-rate (make-instance 'thirty-360 :variant :bond-basis)
                         (make-schedule start end #T"6M" :calendar +target+) (euribor #T"6M")
                         :spread spread :floating-day-counter +actual-360+))))

(defun swaption-checks ()
  (format t "~&European swaptions: QuantLib's Black and Bachelier swaption engines, 4 x 2 x 3~%")
  (let ((m (rates-market))
        (engines `((:black . ,(make-engine :black-rates :volatility 0.20d0))
                   (:shifted-black . ,(make-engine :black-rates :volatility 0.12d0 :displacement 0.02d0))
                   (:normal . ,(make-engine :bachelier-rates :volatility 0.0080d0))))
        (worst 0d0))
    (dolist (spec (getf (caps-fixture) :swaptions))
      (let ((swap (fixture-swaption-swap spec)))
        (iter (for (settlement vol expected) in (getf spec :prices))
              (for swaption = (make-swaption swap (fixture-date (getf spec :exercise))
                                             :settlement settlement))
              (setf worst (max worst (/ (abs (- expected (price swaption (cdr (assoc vol engines)) m)))
                                        (getf spec :nominal)))))))
    (check-that (format nil "physical and cash-settled prices within 1e-10 of QuantLib per unit nominal ~
(worst ~,1E)" worst)
                (<= worst 1d-10))
    (check-error "a Black engine refuses a Bermudan swaption"
                 (price (make-swaption (fixture-swaption-swap (first (getf (caps-fixture) :swaptions)))
                                       (make-instance 'bermudan-exercise
                                                      :dates (list #D"2027-09-24" #D"2028-09-25")))
                        (make-engine :black-rates :volatility 0.2d0) m)
                 unsupported-combination)))

;;; --------------------------------------------------------------------
;;; Phase 3: Hull-White
;;; --------------------------------------------------------------------

(defun hull-white-fixture () (getf (first (read-fixture "hull-white.sexp")) :models))

(defvar *hull-white-market* nil)

(defun hull-white-market ()
  "The rates market discounting EUR on the EURIBOR 6M curve: Hull-White
prices on one curve."
  (or *hull-white-market*
      (setf *hull-white-market*
            (make-market :valuation-date #D"2026-09-24"
                         :curves (mapcar (lambda (c) `(,(car c) :curve ,(cdr c))) (bootstrap-fixture-curves))
                         :discounting '((:usd . :sofr) (:eur . :euribor-6m))))))

(defparameter *hull-white-resets*
  (list #D"2027-09-28" #D"2028-09-28" #D"2029-09-28" #D"2030-09-30" #D"2031-09-29"))

(defun hull-white-swap (type rate start &optional (end #D"2036-09-28") (nominal 1d6))
  (make-vanilla-swap type nominal (make-schedule start end #T"1Y" :calendar +target+)
                     rate (make-instance 'thirty-360 :variant :bond-basis)
                     (make-schedule start end #T"6M" :calendar +target+) (euribor #T"6M")
                     :floating-day-counter +actual-360+))

(defun relative-error (expected actual)
  (abs (/ (- actual expected) expected)))

(defun hull-white-analytic-checks ()
  (format t "~&Hull-White: discount bonds and bond options against QuantLib's HullWhite~%")
  (let* ((m (hull-white-market))
         (curve (make-rate-curve m :euribor-6m))
         (worst-curve 0d0) (worst-discount 0d0) (worst-bond 0d0) (worst-option 0d0)
         (worst-tiny-a 0d0))
    (dolist (spec (hull-white-fixture))
      (let ((hw (make-hull-white :a (getf spec :a) :sigma (getf spec :sigma))))
        (iter (for (time expected) in (getf spec :discounts))
              (for model = (hull-white-discount hw curve time))
              (setf worst-discount (max worst-discount (relative-error expected model))
                    worst-curve (max worst-curve
                                     (relative-error (fincl::rate-curve-discount curve time) model))))
        (iter (for (t0 t1 r expected) in (getf spec :bonds))
              (setf worst-bond (max worst-bond (relative-error expected
                                                               (hull-white-discount-bond hw curve t0 t1 r)))))
        (iter (for (type k maturity start end expected) in (getf spec :bond-options))
              (for error = (abs (- expected (hull-white-bond-option hw curve (if (eq type :call) 1 -1)
                                                                    k maturity start end))))
              (if (< (getf spec :a) 1d-8)
                  (setf worst-tiny-a (max worst-tiny-a error))
                  (setf worst-option (max worst-option error))))))
    (check-that (format nil "P(0, T) reprices the curve's discount factors (worst relative ~,1E)" worst-curve)
                (< worst-curve 1d-12))
    (check-that (format nil "P(0, T) as QuantLib's (worst relative ~,1E)" worst-discount)
                (< worst-discount 1d-12))
    ;; A(t, T) carries the instantaneous forward, a difference quotient over
    ;; 1e-4 years: curve differences of 1e-16 grow to ~1e-12 by t = 5.
    (check-that (format nil "P(t, T | r) as QuantLib's (worst relative ~,1E)" worst-bond)
                (< worst-bond 1d-11))
    (check-that (format nil "zero-coupon bond options as QuantLib's (worst ~,1E)" worst-option)
                (< worst-option 1d-12))
    ;; For a below sqrt(eps) QuantLib's option vol differs from
    ;; sigma (T - t) sqrt(maturity) by ~2e-8 relative; the cause is open.
    (check-that (format nil "... and with a = 1e-10, to 1e-9 (worst ~,1E)" worst-tiny-a)
                (< worst-tiny-a 1d-9))))

(defun hull-white-swaption-checks ()
  (format t "~&Hull-White swaptions: Jamshidian and the tree against QuantLib~%")
  (let ((m (hull-white-market))
        (jamshidian 0d0) (tree 0d0) (bermudan 0d0) (callable 0d0) (fine 0d0) (fitted 0d0))
    (dolist (spec (hull-white-fixture))
      (let ((hw (make-hull-white :a (getf spec :a) :sigma (getf spec :sigma))))
        (iter (for (reset type expected-analytic expected-tree) in (getf spec :europeans))
              (for swaption = (make-swaption (hull-white-swap type 0.027d0 (fixture-date reset))
                                             (fixture-date reset)))
              (for analytic = (price swaption (make-engine :jamshidian :process hw) m))
              (setf jamshidian (max jamshidian (relative-error expected-analytic analytic))
                    tree (max tree (relative-error expected-tree
                                                   (price swaption (make-engine :hull-white-tree :process hw
                                                                                                 :time-steps 200)
                                                          m)))))
        ;; The tree converges on Jamshidian, erratically as the strike moves
        ;; against the nodes (QuantLib's tree, which this one reproduces,
        ;; does the same): 3-7e-4 at 200-800 steps a year on a flat curve. A
        ;; 1Y into 2Y swaption keeps the tree small.
        (let* ((flat (make-market :valuation-date *today* :curves '((:eur-flat :rate 0.025d0))
                                  :discounting '((:eur . :eur-flat))))
               (reset (first *hull-white-resets*))
               (years (fincl::market-time flat #D"2029-09-28")))
          (dolist (type '(:payer :receiver))
            (let ((swaption (make-swaption (hull-white-swap type 0.027d0 reset #D"2029-09-28") reset)))
              (setf fine (max fine (relative-error
                                    (price swaption (make-engine :jamshidian :process hw) flat)
                                    (price swaption (make-engine :hull-white-tree :process hw
                                                                                  :time-steps (round (* 200 years)))
                                           flat)))))))
        (iter (for (type rate steps expected) in (getf spec :bermudans))
              (for swaption = (make-swaption (hull-white-swap type rate (first *hull-white-resets*))
                                             (make-instance 'bermudan-exercise :dates *hull-white-resets*)))
              (setf bermudan (max bermudan (relative-error
                                            expected
                                            (price swaption (make-engine :hull-white-tree :process hw
                                                                                          :time-steps steps)
                                                   m)))))
        (let* ((schedule (make-schedule #D"2026-09-28" #D"2036-09-28" #T"1Y" :calendar +target+
                                        :convention :unadjusted))
               (bond (make-callable-fixed-rate-bond
                      schedule 0.03d0 (make-instance 'thirty-360 :variant :bond-basis)
                      (mapcar (lambda (d) `(,d 100)) (subseq (schedule-dates schedule) 3 10))
                      :currency :eur)))
          (iter (for (steps npv nil) in (getf spec :callable))
                (setf callable (max callable (relative-error
                                              npv (price bond (make-engine :hull-white-tree :process hw
                                                                                            :time-steps steps)
                                                         m))))))
        ;; The fitted tree reprices the curve at every grid time.
        (let* ((curve (make-rate-curve m :euribor-6m))
               (grid (fincl::mandatory-time-grid '(1d0 5.5d0 10d0) 120))
               (lattice (fincl::make-hull-white-lattice hw curve grid)))
          (iter (for i from 0 below (length grid))
                (setf fitted (max fitted (relative-error
                                          (fincl::rate-curve-discount curve (aref grid i))
                                          (aref (fincl::lattice-state-price-sums lattice) i))))))))
    (check-that (format nil "Jamshidian European swaptions as QuantLib's (worst relative ~,1E)" jamshidian)
                (< jamshidian 1d-10))
    (check-that (format nil "tree European swaptions as QuantLib's on the same grid (worst relative ~,1E)" tree)
                (< tree 1d-4))
    (check-that (format nil "tree within 1e-3 of Jamshidian at 200 steps a year, flat curve (worst relative ~,1E)"
                        fine)
                (< fine 1d-3))
    (check-that (format nil "Bermudan swaptions as QuantLib's TreeSwaptionEngine (worst relative ~,1E)" bermudan)
                (< bermudan 1d-4))
    (check-that (format nil "callable bond as QuantLib's TreeCallableFixedRateBondEngine (worst relative ~,1E)"
                        callable)
                (< callable 1d-4))
    (check-that (format nil "the fitted tree reprices the curve (worst relative ~,1E)" fitted)
                (< fitted 1d-8))))

(defun hull-white-invariant-checks ()
  (format t "~&Hull-White invariants and calibration~%")
  (let* ((m (hull-white-market))
         (hw (make-hull-white :a 0.05d0 :sigma 0.01d0))
         (engine (make-engine :hull-white-tree :process hw :time-steps 400)))
    (dolist (type '(:payer :receiver))
      (dolist (rate '(0.022d0 0.027d0 0.032d0))
        (let ((bermudan (price (make-swaption (hull-white-swap type rate (first *hull-white-resets*))
                                              (make-instance 'bermudan-exercise :dates *hull-white-resets*))
                               engine m))
              (europeans (mapcar (lambda (reset)
                                   (price (make-swaption (hull-white-swap type rate reset) reset) engine m))
                                 *hull-white-resets*)))
          (check-that (format nil "~(~A~) at ~,2F%: Bermudan ~,2F >= every co-terminal European (max ~,2F)"
                              type (* 100 rate) bermudan (reduce #'max europeans))
                      (>= bermudan (reduce #'max europeans)))))))
  ;; Calibration recovers the parameters that generated the vols.
  (let* ((m (hull-white-market))
         (truth (make-hull-white :a 0.08d0 :sigma 0.009d0))
         (swaptions (mapcar (lambda (reset) (make-swaption (hull-white-swap :payer 0.027d0 reset) reset))
                            *hull-white-resets*))
         (quotes (mapcar (lambda (s)
                           (let ((target (price s (make-engine :jamshidian :process truth) m)))
                             `(,s ,(solve-brent (lambda (vol)
                                                  (- (price s (make-engine :black-rates :volatility vol) m)
                                                     target))
                                                0.01d0 2d0 :tolerance 1d-15))))
                         swaptions)))
    (multiple-value-bind (fitted errors) (calibrate-hull-white (make-hull-white :a 0.2d0 :sigma 0.02d0)
                                                               m quotes)
      (check-that (format nil "calibration to 5 co-terminal swaptions recovers a = 0.08, sigma = 0.009 ~
(~,10F, ~,10F; worst price error ~,1E)"
                          (hull-white-a fitted) (process-parameter fitted :sigma)
                          (reduce #'max (map 'list #'abs errors)))
                  (and (< (abs (- (hull-white-a fitted) 0.08d0)) 1d-6)
                       (< (abs (- (process-parameter fitted :sigma) 0.009d0)) 1d-6))))
    (let ((fitted (calibrate-hull-white (make-hull-white :a 0.08d0 :sigma 0.02d0) m quotes :fix-a t)))
      (check-that "with a fixed, sigma alone is recovered to 1e-6"
                  (and (= (hull-white-a fitted) 0.08d0)
                       (< (abs (- (process-parameter fitted :sigma) 0.009d0)) 1d-6))))))

(defun rates-instrument-checks ()
  (bond-checks)
  (swap-checks)
  (fra-checks)
  (cap-checks)
  (swaption-checks)
  (hull-white-analytic-checks)
  (hull-white-swaption-checks)
  (hull-white-invariant-checks))

(defun run-rates-instrument-tests ()
  "Run only the interest-rate instrument checks (task 16). Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (rates-instrument-checks))
    (format t "~&~[All rates instrument checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
