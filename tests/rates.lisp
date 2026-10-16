;;;; rates.lisp --- currencies, interest rates, yield curves, cash flows,
;;;; bootstrapping (task 15), against QuantLib-generated fixtures

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun rates-day-counter (name)
  (ecase name
    (:actual-360 +actual-360+)
    (:actual-365-fixed +actual-365-fixed+)
    (:thirty-360-bond-basis (make-instance 'thirty-360 :variant :bond-basis))
    (:thirty-360-european (make-instance 'thirty-360 :variant :european))
    (:actual-actual-isda +actual-actual-isda+)))

(defun fixture-date (string) (parse-date string))

(defun worst (pairs)
  "The largest |expected - actual| over (expected . actual) PAIRS."
  (reduce #'max pairs :key (lambda (p) (abs (- (car p) (cdr p)))) :initial-value 0d0))

;;; --------------------------------------------------------------------
;;; Phase 1: currencies, interest rates, curves
;;; --------------------------------------------------------------------

(defun currency-checks ()
  (format t "~&Currencies and money~%")
  (check-that "registered currencies are of type CURRENCY"
              (and (typep :usd 'currency) (typep :eur 'currency) (not (typep :xyz 'currency))))
  (check "money+ in one currency" 3.5d0
         (money-amount (money+ (make-money 1 :usd) (make-money 2.5d0 :usd))) 0)
  (check-error "money+ across currencies" (money+ (make-money 1 :usd) (make-money 1 :eur))
               currency-mismatch)
  (check-error "money- across currencies" (money- (make-money 1 :usd) (make-money 1 :gbp))
               currency-mismatch)
  (check-that "CURRENCY-MISMATCH is an INVALID-INPUT"
              (subtypep 'currency-mismatch 'invalid-input))
  (check-error "an unregistered currency" (make-money 1 :xyz) invalid-argument)
  (check-that "money prints with the currency's digits"
              (search "1234.50 USD" (princ-to-string (make-money 1234.5d0 :usd)))))

(defun interest-rate-checks ()
  (format t "~&Interest rates: QuantLib InterestRate, 200 random cases~%")
  (let ((cases (rest (assoc :interest-rates (read-fixture "rates.sexp"))))
        (compound '()) (discount '()) (compound-time '()) (implied '()) (equivalent '()))
    (dolist (c cases)
      (destructuring-bind (&key rate day-counter compounding frequency start end time
                                ((:compound cf)) ((:discount df)) ((:compound-time cft))
                                result-day-counter result-compounding result-frequency
                                ((:implied imp)) ((:equivalent eqv)))
          c
        (let* ((d1 (fixture-date start)) (d2 (fixture-date end))
               (dc (rates-day-counter day-counter))
               (dc2 (rates-day-counter result-day-counter))
               (r (make-interest-rate rate :day-counter dc :compounding compounding
                                           :frequency frequency)))
          (push (cons cf (compound-factor r d1 d2)) compound)
          (push (cons df (discount-factor-of r d1 d2)) discount)
          (push (cons cft (compound-factor r time)) compound-time)
          (push (cons imp (interest-rate-rate (implied-rate cf dc2 result-compounding
                                                            result-frequency d1 d2)))
                implied)
          (push (cons eqv (interest-rate-rate (equivalent-rate r result-compounding result-frequency
                                                               d1 d2 :day-counter dc2)))
                equivalent))))
    (flet ((report (name pairs tolerance)
             (let ((w (worst pairs)))
               (check-that (format nil "~A matches QuantLib to ~,0E (worst ~,1E)" name tolerance w)
                           (<= w tolerance)))))
      ;; Compound factors reach e^4.5 over 30 years: 1e-14 relative, so
      ;; absolute 1e-13 on them.
      (report "compound factor between dates" compound 1d-13)
      (report "discount factor between dates" discount 1d-14)
      (report "compound factor over a time" compound-time 1d-13)
      (report "implied rate" implied 1d-14)
      (report "equivalent rate" equivalent 1d-14)))
  (check-error "a compounded rate needs a frequency"
               (make-interest-rate 0.05d0 :compounding :compounded :frequency :once)
               invalid-argument))

(defun fixture-curve (spec)
  (destructuring-bind (&key kind interpolation day-counter dates values compounding frequency
                       &allow-other-keys)
      spec
    (let ((dates (mapcar #'fixture-date dates))
          (dc (rates-day-counter day-counter)))
      (ecase kind
        (:discount (make-instance 'interpolated-discount-curve :dates dates :discounts values
                                                               :day-counter dc
                                                               :interpolation interpolation))
        (:zero (make-instance 'interpolated-zero-curve :dates dates :zero-rates values
                                                       :day-counter dc :interpolation interpolation
                                                       :compounding compounding
                                                       :frequency frequency))
        (:forward (make-instance 'interpolated-forward-curve :dates dates :forwards values
                                                             :day-counter dc
                                                             :interpolation interpolation))))))

(defun yield-curve-checks ()
  (format t "~&Yield curves: QuantLib's interpolated discount, zero and forward curves~%")
  (dolist (spec (rest (assoc :curves (read-fixture "rates.sexp"))))
    (let* ((curve (fixture-curve spec))
           (name (format nil "~(~A~) curve (~(~A~), ~(~A~)~@[, ~(~A~) input~])"
                         (getf spec :kind) (getf spec :interpolation) (getf spec :day-counter)
                         (and (getf spec :compounding) (not (eq (getf spec :compounding) :continuous))
                              (getf spec :compounding))))
           (discounts '()) (zeros '()) (forwards '()))
      (dolist (q (getf spec :queries))
        (ecase (first q)
          (:discount (destructuring-bind (d v) (rest q)
                       (push (cons v (curve-discount curve (fixture-date d))) discounts)))
          (:zero (destructuring-bind (d dc comp freq v) (rest q)
                   (push (cons v (curve-zero-rate curve (fixture-date d) comp freq
                                                  (rates-day-counter dc)))
                         zeros)))
          (:forward (destructuring-bind (d1 d2 dc comp freq v) (rest q)
                      (push (cons v (curve-forward-rate curve (fixture-date d1) (fixture-date d2)
                                                        comp freq (rates-day-counter dc)))
                            forwards)))))
      (check-that (format nil "~A: discount factors match QuantLib to 1e-12 (worst ~,1E)"
                          name (worst discounts))
                  (<= (worst discounts) 1d-12))
      (check-that (format nil "~A: zero rates match QuantLib to 1e-12 (worst ~,1E)" name (worst zeros))
                  (<= (worst zeros) 1d-12))
      (check-that (format nil "~A: forward rates match QuantLib to 1e-12 (worst ~,1E)"
                          name (worst forwards))
                  (<= (worst forwards) 1d-12))
      ;; The inputs come back exactly at the knots: discount factors, or the
      ;; stored continuous zero rates and forwards.
      (unless (eq (getf spec :compounding) :compounded)
        (let ((dates (mapcar #'fixture-date (getf spec :dates))) (values (getf spec :values)))
          (check-that (format nil "~A: returns its inputs at the knots exactly" name)
                      (if (eq (getf spec :kind) :discount)
                          (every (lambda (d v) (= v (curve-discount curve d))) dates values)
                          (every #'= values (coerce (fincl::curve-values curve) 'list))))))))
  (let* ((dates (list #D"2026-09-24" #D"2027-09-24" #D"2031-09-24"))
         (curve (make-instance 'interpolated-discount-curve :dates dates :discounts '(1 0.96d0 0.8d0))))
    (check-error "a date before the reference date" (curve-discount curve #D"2026-09-01")
                 invalid-argument)
    (check-error "discount factors not starting at 1"
                 (make-instance 'interpolated-discount-curve :dates dates :discounts '(0.99d0 0.96d0 0.8d0))
                 invalid-argument)
    (check-error "a date on a flat curve" (curve-discount (make-instance 'flat-yield-curve :rate 0.03d0)
                                                          #D"2027-01-01")
                 invalid-argument)))

(defun curve-market-checks ()
  (format t "~&Dated curves in markets~%")
  (let* ((today #D"2026-09-24")
         (dates (list today #D"2027-03-24" #D"2027-09-24" #D"2029-09-24" #D"2036-09-24"))
         (dfs '(1 0.98d0 0.955d0 0.88d0 0.68d0))
         (act365 (make-instance 'interpolated-discount-curve :dates dates :discounts dfs))
         (act360 (make-instance 'interpolated-discount-curve :dates dates :discounts dfs
                                                             :day-counter +actual-360+))
         (market (lambda (curve &optional (day today))
                   (make-market :valuation-date day
                                :underlyings '((:spx :spot 100 :currency :usd :vol 0.2d0))
                                :curves `((:usd :curve ,curve)))))
         (d #D"2028-06-15"))
    (check "a curve sharing the market's conventions: DF at a date"
           (curve-discount act365 d) (discount-factor (funcall market act365) d) 0)
    (check "an ACT/360 curve in an ACT/365F market: DF at a date"
           (curve-discount act360 d) (discount-factor (funcall market act360) d) 1d-15)
    (let ((aged (derive-market (funcall market act360) :valuation-date #D"2027-01-15")))
      (check "an aged market discounts from its own valuation date"
             (/ (curve-discount act360 d) (curve-discount act360 #D"2027-01-15"))
             (discount-factor aged d) 1d-15))
    (let* ((m (funcall market act360))
           (call (make-option (call 100) (expires d)))
           (black (fincl::black-formula 1d0 (forward m d) 100d0 (curve-discount act360 d)
                                 (* 0.04d0 (market-time m d)))))
      (check "Black-Scholes prices off the curve" black
             (price call (make-instance 'black-scholes-engine) m) 1d-12))
    (let* ((m (funcall market act360))
           (bumped (bump m '(:curve :usd) '(:absolute 1/10000)))
           (tm (curve-time act360 #D"2029-09-24")))
      (check "a 1bp curve bump moves knot zero rates by 1bp"
             (+ (/ (- (log 0.88d0)) tm) 1d-4)
             (curve-zero-rate (fincl::find-yield-curve bumped :usd) #D"2029-09-24")
             1d-14))))

;;; --------------------------------------------------------------------
;;; Phase 2: indexes, fixings, cash flows
;;; --------------------------------------------------------------------

(defun fixture-calendar (name)
  (ecase name
    (:target +target+)
    (:united-states-government-bond +united-states-government-bond+)
    (:united-states-settlement +united-states-settlement+)
    (:united-states-sofr +united-states-sofr+)
    (:united-kingdom-settlement +united-kingdom-settlement+)
    (:united-kingdom-exchange +united-kingdom-exchange+)))

(defun fixture-index (name)
  (ecase name
    (:euribor-6m (euribor #T"6M"))
    (:euribor-3m (euribor #T"3M"))
    (:estr (estr))
    (:sofr (sofr))
    (:sonia (sonia))))

(defun legs-market (fixture &key (drop-fixing nil))
  "The market of tests/fixtures/legs.sexp, valued on 2026-09-24. DROP-FIXING,
(index-id \"date\"), leaves one fixing out."
  (make-market
   :valuation-date #D"2026-09-24"
   :curves (iter (for (id . plist) in (rest (assoc :curves fixture)))
                 (collect `(,id :curve ,(make-instance 'interpolated-discount-curve
                                                       :dates (mapcar #'fixture-date (getf plist :dates))
                                                       :discounts (getf plist :discounts)))))
   :discounting '((:eur . :estr) (:usd . :sofr) (:gbp . :sonia))
   :fixings (iter (for (id . pairs) in (rest (assoc :fixings fixture)))
                  (collect `(,id ,@(remove-if (lambda (pair)
                                                (and drop-fixing (eq id (first drop-fixing))
                                                     (string= (first pair) (second drop-fixing))))
                                              pairs))))))

(defun fixture-leg (spec)
  (destructuring-bind (&key kind effective termination tenor calendar convention
                            termination-convention rule end-of-month first-date day-counter index
                            rate gearing payment-lag extra currency &allow-other-keys)
      spec
    (let ((schedule (make-schedule (fixture-date effective) (fixture-date termination)
                                   (parse-tenor tenor)
                                   :calendar (fixture-calendar calendar) :convention convention
                                   :termination-convention termination-convention :rule rule
                                   :end-of-month end-of-month
                                   :first-date (and first-date (fixture-date first-date)))))
      (ecase kind
        (:fixed (fixed-leg schedule (make-money 100 currency) rate (rates-day-counter day-counter)
                           :payment-lag payment-lag
                           :compounding (if (string= extra "compounded") :compounded :simple)))
        ((:ibor :overnight)
         (apply #'floating-leg schedule 100 (fixture-index index) :spread rate :gearing gearing
                :payment-lag payment-lag
                (when (string= extra "0") '(:fixing-days 0))))))))

(defun leg-checks ()
  (format t "~&Cash flows: QuantLib's FixedRateLeg, IborLeg and OvernightLeg, 20 legs~%")
  (let* ((fixture (read-fixture "legs.sexp"))
         (market (legs-market fixture))
         (specs (rest (assoc :legs fixture)))
         (dates-ok t) (amounts '()) (npvs '()) (bps '()) (accrued '())
         (overnight-flat '()) (overnight-past '()))
    (dolist (spec specs)
      (let ((leg (fixture-leg spec))
            (expected (getf spec :cashflows)))
        (unless (= (length leg) (length expected))
          (setf dates-ok nil)
          (format t "~&  ~A leg from ~A: ~D cash flows, QuantLib has ~D~%"
                  (getf spec :kind) (getf spec :effective) (length leg) (length expected)))
        (iter (for cf in leg)
              (for (pay start end ref-start ref-end rate accrual amount) in expected)
              (unless (and (date= (payment-date cf) (fixture-date pay))
                           (date= (accrual-start cf) (fixture-date start))
                           (date= (accrual-end cf) (fixture-date end))
                           (date= (fincl::coupon-ref-start cf) (fixture-date ref-start))
                           (date= (fincl::coupon-ref-end cf) (fixture-date ref-end)))
                (setf dates-ok nil)
                (format t "~&  ~A ~A: dates ~A ~A ~A ~A ~A differ~%" (getf spec :kind)
                        (getf spec :effective) pay start end ref-start ref-end))
              (let ((pair (list (cons amount (cashflow-amount cf market))
                                (cons rate (coupon-rate cf market))
                                (cons accrual (accrual-period cf)))))
                (setf amounts (append pair amounts))
                (when (eq (getf spec :kind) :overnight)
                  (if (date< (accrual-start cf) #D"2026-09-24")
                      (push (cons rate (coupon-rate cf market)) overnight-past)
                      (push (cons rate (coupon-rate cf market)) overnight-flat)))))
        (push (cons (getf spec :npv) (money-amount (leg-npv leg market))) npvs)
        (push (cons (getf spec :bps) (money-amount (leg-bps leg market))) bps)
        (when (getf spec :accrued-date)
          (push (cons (getf spec :accrued)
                      (money-amount (accrued-amount leg (fixture-date (getf spec :accrued-date)) market)))
                accrued))))
    (check-that "payment, accrual and reference dates match QuantLib" dates-ok)
    (flet ((report (name pairs tolerance)
             (check-that (format nil "~A match QuantLib to ~,0E (worst ~,1E)" name tolerance (worst pairs))
                         (<= (worst pairs) tolerance))))
      (report "coupon amounts, rates and accrual periods (notional 100)" amounts 1d-10)
      (report "overnight coupon rates, forecast only" overnight-flat 1d-12)
      (report "overnight coupon rates, with past fixings" overnight-past 1d-10)
      (report "leg NPVs" npvs 1d-10)
      (report "leg BPS" bps 1d-12)
      (report "accrued amounts (fixed and Ibor legs)" accrued 1d-10))
    ;; A missing past fixing: signalled, and USE-VALUE supplies it.
    (let* ((spec (find-if (lambda (s) (and (eq (getf s :kind) :ibor)
                                           (string= (getf s :effective) "2025-12-15")))
                          specs))
           (leg (fixture-leg spec))
           (coupon (find-if (lambda (cf) (date< (fincl::coupon-fixing-date cf) #D"2026-09-24")
                                         (date< #D"2026-09-24" (payment-date cf)))
                            leg))
           (fixing-date (fincl::coupon-fixing-date coupon))
           (thin (legs-market fixture :drop-fixing `(:euribor-6m ,(format nil "~4,'0D-~2,'0D-~2,'0D"
                                                                           (date-year fixing-date)
                                                                           (date-month fixing-date)
                                                                           (date-day fixing-date))))))
      (check-error "a missing past fixing" (cashflow-amount coupon thin) missing-fixing)
      (check "USE-VALUE supplies the missing fixing"
             (+ 0.03d0 0.001d0)
             (handler-bind ((missing-fixing (lambda (c) (use-value 0.03d0 c))))
               (coupon-rate coupon thin))
             1d-15))
    (check-error "a leg in two currencies"
                 (leg-npv (append (fixed-leg (make-schedule #D"2027-01-15" #D"2028-01-15" #T"6M")
                                             (make-money 1 :usd) 0.01d0 +actual-360+)
                                  (fixed-leg (make-schedule #D"2027-01-15" #D"2028-01-15" #T"6M")
                                             (make-money 1 :eur) 0.01d0 +actual-360+))
                          market)
                 currency-mismatch)))

;;; --------------------------------------------------------------------
;;; Phase 3: bootstrapping
;;; --------------------------------------------------------------------

(defun fixture-helper (row today)
  (destructuring-bind (kind tenor quote &rest rest) row
    (declare (ignore rest))
    (ecase kind
      (:ois (ois-helper quote (parse-tenor tenor)
                        (if (< quote 0.03d0) (estr) (sofr))
                        :valuation-date today :payment-lag (if (< quote 0.03d0) 1 2)))
      (:deposit (deposit-helper quote (euribor #T"6M") :valuation-date today))
      (:fra (fra-helper quote (parse-integer tenor) (euribor #T"6M") :valuation-date today))
      (:swap (swap-helper quote (parse-tenor tenor) (euribor #T"6M") :valuation-date today
                          :fixed-convention :unadjusted)))))

(defun bootstrap-checks ()
  (format t "~&Bootstrapping: QuantLib's PiecewiseYieldCurve, SOFR, ESTR and EURIBOR 6M~%")
  (let ((today #D"2026-09-24")
        (built '()))
    (dolist (spec (rest (assoc :curves (read-fixture "bootstrap.sexp"))))
      (destructuring-bind (&key name interpolation discount-curve helpers discounts) spec
        (let* ((hs (mapcar (lambda (row) (fixture-helper row today)) helpers))
               (discount (and discount-curve
                              (cdr (assoc (list discount-curve interpolation) built :test #'equal))))
               (start (get-internal-real-time))
               (curve (bootstrap-curve hs :interpolation interpolation :discount-curve discount))
               (ms (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second))
               (label (format nil "~(~A~) ~(~A~)~@[ on ~(~A~)~]" name interpolation discount-curve)))
          (push (cons (list name interpolation) curve) built)
          (check-that (format nil "~A: pillar dates match QuantLib" label)
                      (every (lambda (h row) (date= (helper-pillar-date h) (fixture-date (fourth row))))
                             hs helpers))
          (let ((errors (iter (for h in hs)
                              (collect (abs (- (helper-implied-quote h curve discount) (helper-quote h)))))))
            (check-that (format nil "~A: every helper reprices to 1e-12 (worst ~,1E)" label
                                (reduce #'max errors))
                        (< (reduce #'max errors) 1d-12)))
          (let ((w (worst (iter (for (d v) in discounts)
                                (collect (cons v (curve-discount curve (fixture-date d))))))))
            (check-that (format nil "~A: 50 discount factors match QuantLib to 1e-10 (worst ~,1E, ~D ms)"
                                label w ms)
                        (<= w 1d-10))))))
    ;; A 1bp move of the 5Y SOFR quote on the log-linear curve leaves every
    ;; discount factor up to the 4Y pillar exactly as it was.
    (let* ((curve (cdr (assoc '(:sofr :log-linear) built :test #'equal)))
           (market (make-market :valuation-date today :curves `((:sofr :curve ,curve))))
           (bumped (find-yield-curve-for-test (bump market `(:quote :sofr ,#T"5Y") '(:absolute 1/10000))
                                              :sofr))
           (pillar-4y (helper-pillar-date (find #T"4Y" (curve-helpers curve) :key #'helper-tenor
                                                                             :test #'equalp)))
           (pillar-5y (helper-pillar-date (find #T"5Y" (curve-helpers curve) :key #'helper-tenor
                                                                             :test #'equalp)))
           (before (iter (for d first (date+ today 1) then (date+ d 17))
                         (while (date<= d pillar-4y)) (collect d))))
      (check-that "a 5Y quote bump leaves the curve before the 4Y pillar unchanged (log-linear)"
                  (every (lambda (d) (= (curve-discount curve d) (curve-discount bumped d))) before))
      (check-that "a 5Y quote bump lowers the 5Y discount factor"
                  (< (curve-discount bumped pillar-5y) (curve-discount curve pillar-5y)))
      (check "the bumped 5Y helper reprices its new quote"
             (+ (helper-quote (find #T"5Y" (curve-helpers curve) :key #'helper-tenor :test #'equalp))
                1d-4)
             (let ((h (find #T"5Y" (curve-helpers bumped) :key #'helper-tenor :test #'equalp)))
               (helper-implied-quote h bumped))
             1d-12))))

(defun find-yield-curve-for-test (market id)
  (fincl::find-yield-curve market id))

(defun rates-checks ()
  (currency-checks)
  (interest-rate-checks)
  (yield-curve-checks)
  (curve-market-checks)
  (leg-checks)
  (bootstrap-checks))

(defun run-rates-tests ()
  "Run only the checks of this file. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (rates-checks))
    (format t "~&~[All rates checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
