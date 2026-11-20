;;;; simulation-validation.lisp --- simulation studies with known answers (task 32, Phase 4)
;;;;
;;;; Each scenario is a study written with the public API (SIMULATE and its
;;;; clauses, PRICE, GREEKS), checked against a result from the literature or
;;;; an independent fincl engine. RUN-SIMULATION-VALIDATION-TESTS prints each
;;;; estimate, its reference and SE in one table.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defvar *validation-rows* '()
  "(scenario estimate reference se) rows for the report, newest first.")

(defun validation-row (scenario estimate reference se)
  (push `(,scenario ,estimate ,reference ,se) *validation-rows*))

(defun print-validation-table ()
  (format t "~&~%Simulation validation: estimate, reference and standard error~%")
  (format t "~&  ~58A ~12@A ~12@A ~10@A~%" "scenario" "estimate" "reference" "SE")
  (dolist (row (reverse *validation-rows*))
    (destructuring-bind (scenario estimate reference se) row
      (format t "~&  ~58A ~12,4F ~12@A ~10@A~%" scenario estimate
              (if reference (format nil "~,4F" reference) "-")
              (if se (format nil "~,4F" se) "-")))))

(defun validation-market (&key (rate 0d0) (vol 0.2d0))
  (make-market :valuation-date *today*
               :underlyings `((:spx :spot 100 :currency :usd :vol ,vol))
               :curves `((:usd :rate ,rate))))

(defun hedge-dates (n days)
  "N dates DAYS / N days apart, the last DAYS after *TODAY*."
  (iter (for i from 1 to n) (collect (date+ *today* (* i (/ days n))))))

(defun summary (result name)
  "(values mean se sd) of output NAME."
  (multiple-value-bind (mean se) (simulation-summary result (simulation-values result name))
    (values mean se (sample-sd (simulation-values result name)))))

(defun hedge-pnl-run (market call dates delta-market &key (n-paths 4000) world)
  "A short CALL sold at its price on MARKET, delta-hedged on DATES with
Black-Scholes deltas on (funcall DELTA-MARKET date-market); the final P&L
at expiry. r = 0, so cash earns nothing."
  (let ((expiry (fincl::expiry (option-exercise call))))
    (simulate ((or world (make-gbm)) market dates :n-paths n-paths)
      (:accumulate (shares 0d0) (cash (price call :black-scholes market)))
      (:state s :spot)
      (:start (market)
        (setf shares (getf (greeks call :black-scholes (funcall delta-market market) :which '(:delta)) :delta))
        (decf cash (* shares s)))
      (:every (date market)
        (when (date< date expiry)
          (let ((delta (getf (greeks call :black-scholes (funcall delta-market market) :which '(:delta)) :delta)))
            (decf cash (* (- delta shares) s))
            (setf shares delta))))
      (:final (:pnl (+ cash (* shares s) (- (max 0d0 (- s 100d0)))))))))

;;; --------------------------------------------------------------------
;;; V1: hedging at implied against hedging at realized
;;; --------------------------------------------------------------------

(defun gamma-cash-integral (sigma-i sigma-r s0 strike maturity)
  "The integral over [0, T] of E[Gamma_I(S_t, t) S_t^2] for S a driftless GBM at
SIGMA-R: Gamma_I S^2 = S phi(d1) / (sigma_I sqrt(tau)). With t = T - u^2
the 1/sqrt(tau) singularity cancels."
  (flet ((inner (time)
           (let ((tau (- maturity time)))
             (fincl::integrate-gauss-legendre
              (lambda (z)
                (let* ((s (* s0 (exp (+ (* -0.5d0 sigma-r sigma-r time) (* sigma-r (sqrt time) z)))))
                       (d1 (/ (+ (log (/ s strike)) (* 0.5d0 sigma-i sigma-i tau)) (* sigma-i (sqrt tau)))))
                  (* (norm-pdf z) s (norm-pdf d1) (/ 1d0 sigma-i))))
              -10d0 10d0 :panels 40))))
    ;; dt / sqrt(tau) = 2 du.
    (fincl::integrate-gauss-legendre (lambda (u) (* 2d0 (inner (- maturity (* u u))))) 0d0 (sqrt maturity) :panels 20)))

(defun v1-checks ()
  (format t "~&V1: hedging at implied against hedging at realized (Ahmad-Wilmott)~%")
  (let* ((m (validation-market))
         (days 364)
         (expiry (date+ *today* days))
         (call (make-option (call 100) (expires expiry) (underlying :spx)))
         (world (make-gbm :vol 0.15d0))
         (at-realized (lambda (market) (bump market '(:vol :spx) '(:absolute -5/100))))
         (reference (- (price call :black-scholes m) (price call :black-scholes (bump m '(:vol :spx) '(:absolute -5/100)))))
         (realized (mapcar (lambda (n) (hedge-pnl-run m call (hedge-dates n days) at-realized :world world)) '(13 52))))
    (multiple-value-bind (mean se sd52) (summary (second realized) :pnl)
      (validation-row "V1 hedge at realized (N=52): mean P&L vs V(si) - V(sr)" mean reference se)
      (check-that (format nil "hedging at realized: mean P&L ~,4F = V(sigma_I) - V(sigma_R) ~,4F within 3 SE (~,4F)" mean reference se)
                  (<= (abs (- mean reference)) (* 3 se)))
      (let ((ratio (/ sd52 (nth-value 2 (summary (first realized) :pnl)))))
        (validation-row "V1 hedge at realized: sd(N=52) / sd(N=13)" ratio 0.5d0 nil)
        (check-that (format nil "... its standard deviation falls by ~,3F, in [0.4, 0.6], when N quadruples" ratio)
                    (<= 0.4d0 ratio 0.6d0))))
    (let* ((maturity (market-time m expiry))
           (reference (* 0.5d0 (- (* 0.2d0 0.2d0) (* 0.15d0 0.15d0))
                         (gamma-cash-integral 0.2d0 0.15d0 100d0 100d0 maturity)))
           (implied (mapcar (lambda (n) (hedge-pnl-run m call (hedge-dates n days) #'identity :world world)) '(13 52 182))))
      (multiple-value-bind (mean13 se13 sd13) (summary (first implied) :pnl)
        (declare (ignore se13))
        (multiple-value-bind (mean52 se52 sd52) (summary (second implied) :pnl)
          (let ((bias (abs (- mean52 mean13))))
            (validation-row "V1 hedge at implied (N=52): mean P&L vs 1/2 (si^2-sr^2) int E[G S^2]" mean52 reference se52)
            (check-that (format nil "hedging at implied: mean P&L ~,4F = the gamma integral ~,4F within 3 SE (~,4F) plus the ~
discrete-hedging bias (~,4F at N=13 -> 52)" mean52 reference se52 bias)
                        (<= (abs (- mean52 reference)) (+ (* 3 se52) bias)))
            (format t "~&       bias against the integral: ~,4F at N=13, ~,4F at N=52~%"
                    (- mean13 reference) (- mean52 reference))
            ;; Var(N) = Var(infinity) + c / N: extrapolate from N = 52 and
            ;; 182; a sample variance's SE is Var sqrt(2 / (n - 1)).
            (let* ((sd182 (nth-value 2 (summary (third implied) :pnl)))
                   (v52 (* sd52 sd52)) (v182 (* sd182 sd182))
                   (limit (/ (- (* 182 v182) (* 52 v52)) 130d0))
                   (limit-se (/ (sqrt (+ (expt (* 182 v182 (sqrt (/ 2d0 3999))) 2)
                                         (expt (* 52 v52 (sqrt (/ 2d0 3999))) 2)))
                                130d0)))
              (validation-row "V1 hedge at implied: P&L variance extrapolated to N = infinity" limit nil limit-se)
              (check-that (format nil "... and its standard deviation does not tend to 0: ~,4F at N=13, ~,4F at 52, ~,4F at 182; ~
the variance extrapolated to continuous hedging, ~,4F, is above 3 SE (~,4F)" sd13 sd52 sd182 limit limit-se)
                          (> limit (* 3 limit-se))))))))))

;;; --------------------------------------------------------------------
;;; V2: discrete hedging error
;;; --------------------------------------------------------------------

(defun v2-checks ()
  (format t "~&V2: discrete hedging error (Boyle-Emanuel; Derman-Kamal)~%")
  (let* ((m (validation-market))
         (days 832)
         (expiry (date+ *today* days))
         (call (make-option (call 100) (expires expiry) (underlying :spx)))
         (ns '(13 52 208 832))
         (runs (mapcar (lambda (n) (hedge-pnl-run m call (hedge-dates n days) #'identity)) ns))
         (sds (mapcar (lambda (r) (nth-value 2 (summary r :pnl))) runs))
         (vega (getf (greeks call :black-scholes m :which '(:vega)) :vega))
         (derman-kamal (* (sqrt (/ pi 4)) 0.2d0 vega (/ 1d0 (sqrt 832d0)))))
    (check-that "the mean P&L is 0 within 3 SE at N = 13, 52, 208 and 832"
                (iter (for r in runs) (for n in ns)
                      (multiple-value-bind (mean se) (summary r :pnl)
                        (validation-row (format nil "V2 BS hedge, model right (N=~D): mean P&L" n) mean 0d0 se)
                        (always (<= (abs mean) (* 3 se))))))
    (check-that (format nil "the standard deviation halves per quadrupling: ratios ~{~,3F~^, ~}, each in [0.45, 0.55]"
                        (mapcar #'/ (rest sds) sds))
                (every (lambda (x) (<= 0.45d0 x 0.55d0)) (mapcar #'/ (rest sds) sds)))
    (validation-row "V2 sd at N=832 vs Derman-Kamal sqrt(pi/4) sigma vega / sqrt(N)" (a:lastcar sds) derman-kamal nil)
    (check-that (format nil "at N = 832 the standard deviation ~,4F is within 15% of Derman-Kamal's ~,4F" (a:lastcar sds) derman-kamal)
                (< (abs (1- (/ (a:lastcar sds) derman-kamal))) 0.15d0))
    (let* ((heston (make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0 :xi 0.5d0 :rho -0.7d0))
           (r (simulate (heston m (hedge-dates 832 days) :n-paths 2000)
                (:state v :variance)
                (:state s :spot)
                (:derive (:vol :spx) (market)
                  (rescale-vol-structure (market-component market '(:vol :spx)) (heston-atm-variance heston v)))
                (:accumulate (shares 0d0) (cash 0d0))
                (:start (market)
                  (setf shares (getf (greeks call :black-scholes market :which '(:delta)) :delta))
                  (setf cash (- (price call :black-scholes market) (* shares s))))
                (:every (date market)
                  (when (date< date expiry)
                    (let ((delta (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
                      (decf cash (* (- delta shares) s))
                      (setf shares delta))))
                (:final (:pnl (+ cash (* shares s) (- (max 0d0 (- s 100d0))))))))
           (sd (nth-value 2 (summary r :pnl))))
      (validation-row "V2 Heston world, BS on the derived surface (N=832): sd / GBM sd" (/ sd (a:lastcar sds)) nil nil)
      (check-that (format nil "under Heston (example 3's world and surface) the standard deviation at N = 832, ~,4F, is above GBM's (ratio ~,2F)"
                          sd (/ sd (a:lastcar sds)))
                  (> sd (a:lastcar sds))))))

;;; --------------------------------------------------------------------
;;; V3: exercise rules for an American put
;;; --------------------------------------------------------------------

(defun bermudan-put (dates)
  (make-option (put 100) (bermudan :dates dates) (underlying :spx)))

(defun tree-for (market dates &optional (per-day 2))
  "A binomial engine with PER-DAY steps a day to the last of DATES, so every
date is a step."
  (make-engine :binomial :n-steps (* per-day (date- (a:lastcar dates) (valuation-date market)))))

(defun v3-checks ()
  (format t "~&V3: exercise rules for an American put (binomial Bermudan)~%")
  (let* ((m (validation-market :rate 0.05d0))
         (dates (hedge-dates 26 182))
         (expiry (a:lastcar dates))
         (europe (make-option (put 100) (expires expiry) (underlying :spx)))
         (reference (price (bermudan-put dates) (tree-for m dates) m))
         (tree-error (abs (- (price (bermudan-put dates) (tree-for m dates 4) m) reference)))
         (r (simulate ((make-gbm) m dates :n-paths 8000 :antithetic t)
              (:state s :spot)
              (:hold (optimal nil) (itm nil))
              (:accumulate (optimal-value 0d0) (itm-value 0d0) (never-value 0d0))
              (:every (date market)
                (let ((intrinsic (max 0d0 (- 100d0 s)))
                      (later (remove-if-not (lambda (d) (date< date d)) dates)))
                  (when (and (not optimal) (plusp intrinsic)
                             (or (null later)
                                 (>= intrinsic (if (rest later)
                                                   (price (bermudan-put later) (tree-for market later) market)
                                                   (price europe :black-scholes market)))))
                    (setf optimal t optimal-value (* (path-discount) intrinsic)))
                  (when (and (not itm) (or (<= s 80d0) (and (null later) (plusp intrinsic))))
                    (setf itm t itm-value (* (path-discount) intrinsic)))
                  (when (null later)
                    (setf never-value (* (path-discount) intrinsic)))))
              (:final (:optimal optimal-value) (:itm itm-value) (:never never-value)))))
    (multiple-value-bind (mean se) (summary r :optimal)
      (validation-row "V3 continuation-value rule vs binomial Bermudan" mean reference se)
      (check-that (format nil "exercising at the continuation value: mean ~,4F = the Bermudan ~,4F within 3 SE (~,4F) plus the tree's error (~,4F)"
                          mean reference se tree-error)
                  (<= (abs (- mean reference)) (+ (* 3 se) tree-error))))
    (dolist (rule '(:itm :never))
      (multiple-value-bind (mean se) (summary r rule)
        (validation-row (format nil "V3 rule ~(~A~): value lost against the Bermudan" (if (eq rule :itm) "20% in the money" "never early"))
                        (- reference mean) nil se)
        (check-that (format nil "rule ~(~A~): ~,4F, below the Bermudan by ~,4F, more than 3 SE (~,4F)"
                            rule mean (- reference mean) se)
                    (> (- reference mean) (* 3 se)))))))

;;; --------------------------------------------------------------------
;;; V4: an issuer's call rule under Hull-White
;;; --------------------------------------------------------------------

(defun v4-bond (call-dates)
  (make-callable-fixed-rate-bond (make-schedule :effective *today* :termination (years-after *today* 6) :tenor #T"1Y" :calendar +target+)
                                 0.04d0 +actual-365-fixed+
                                 (mapcar (lambda (d) `(,d 100)) call-dates)
                                 :currency :eur :settlement-days 0))

(defun issuer-rule-run (bond hw m &key (n-paths 5000))
  "The bondholder's discounted cash flows when the issuer calls on a call date
as soon as the remaining bond's value on the date's market exceeds the call
price (the dates are coupon dates, so nothing is accrued)."
  (let* ((flows (fincl::bond-cashflows bond))
         (dates (remove-duplicates (mapcar #'fincl::payment-date flows) :test #'date=))
         (calls (mapcar #'first (fincl::bond-call-schedule bond))))
    (simulate ((((:curve :eur) hw :knots (monthly-knots 84))) m dates :n-paths n-paths)
      (:accumulate (received 0d0))
      (:every (date market)
        (dolist (cf flows)
          (when (date= date (fincl::payment-date cf))
            (incf received (* (path-discount) (fincl::cashflow-amount cf market)))))
        (when (and (member date calls :test #'date=)
                   (> (fincl::bond-npv bond market) 100d0))
          (incf received (* (path-discount) 100d0))
          (stop)))
      (:final (:value received)))))

(defun v4-checks ()
  (format t "~&V4: an issuer's call rule under Hull-White (the trinomial tree)~%")
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.03d0 :sigma 0.01d0))
         (coupon-dates (mapcar #'fincl::payment-date (fincl::bond-cashflows (v4-bond (list (years-after *today* 3))))))
         (one (v4-bond (list (third coupon-dates))))
         (three (v4-bond (subseq coupon-dates 1 4))))
    (flet ((tree (bond steps) (price bond (make-engine :hull-white-tree :process hw :time-steps steps) m)))
      (let* ((reference (tree one 400))
             (tree-error (abs (- (tree one 800) reference))))
        (multiple-value-bind (mean se) (summary (issuer-rule-run one hw m) :value)
          (validation-row "V4 one call date, call when the bond is worth more" mean reference se)
          (check-that (format nil "one call date: the holder's mean discounted cash flows ~,4F = the tree's ~,4F within 3 SE (~,4F) plus the tree's error (~,4F)"
                              mean reference se tree-error)
                      (<= (abs (- mean reference)) (+ (* 3 se) tree-error)))))
      (let ((reference (tree three 400)))
        (multiple-value-bind (mean se) (summary (issuer-rule-run three hw m) :value)
          (validation-row "V4 three call dates, the one-date rule: value left to the holder" (- mean reference) nil se)
          ;; Calling whenever the straight bond exceeds the price calls too
          ;; often (the bond still callable later is worth less), so the
          ;; holder gets at least the tree's value.
          (check-that (format nil "three call dates: the one-date rule leaves the holder ~,4F above the tree's ~,4F (at or above, within 3 SE ~,4F)"
                              (- mean reference) reference se)
                      (>= mean (- reference (* 3 se)))))))))

;;; --------------------------------------------------------------------
;;; V5: immunizing a liability under one factor
;;; --------------------------------------------------------------------

(defun immunization-run (hw m n &key (days 576) (weight :factor) (n-paths 4000))
  "Owe a 7-year zero-coupon bond; hold 2- and 10-year ones matching its value
and its sensitivity to the short rate (WEIGHT :FACTOR, B(t, T) P) or its
Macaulay duration (:MACAULAY, (T - t) P), rebalanced self-financing on N dates
over DAYS. The output is the discounted residual at the last date."
  (let* ((curve (make-rate-curve m :eur))
         (a (hull-white-a hw))
         (maturities (mapcar (lambda (y) (market-time m (years-after *today* y))) '(2 7 10)))
         (dates (hedge-dates n days)))
    (flet ((bonds (time rate)
             (mapcar (lambda (maturity) (hull-white-discount-bond hw curve time maturity rate)) maturities))
           (sensitivity (time maturity)
             (if (eq weight :factor)
                 (/ (- 1 (exp (- (* a (- maturity time))))) a)
                 (- maturity time))))
      (simulate ((((:curve :eur) hw)) m dates :n-paths n-paths)
        (:state r :short-rate)
        (:accumulate (n2 0d0) (n10 0d0) (value 0d0))
        (:hold (rebalance nil))
        (:start
          (setf rebalance
                (lambda (time rate value)
                  (destructuring-bind (p2 p7 p10) (bonds time rate)
                    (destructuring-bind (b2 b7 b10) (mapcar (lambda (mt) (sensitivity time mt)) maturities)
                      ;; n2 p2 + n10 p10 = value; n2 b2 p2 + n10 b10 p10 = b7 p7.
                      (let* ((det (* p2 p10 (- b10 b2)))
                             (x2 (/ (- (* value b10 p10) (* b7 p7 p10)) det))
                             (x10 (/ (- (* b7 p7 p2) (* value b2 p2)) det)))
                        (values x2 x10))))))
          (setf value (second (bonds 0d0 r)))
          (multiple-value-setq (n2 n10) (funcall rebalance 0d0 r value)))
        (:every (date)
          (let ((time (market-time m date)))
            (destructuring-bind (p2 p7 p10) (bonds time r)
              (setf value (+ (* n2 p2) (* n10 p10)))
              (if (date< date (a:lastcar dates))
                  (multiple-value-setq (n2 n10) (funcall rebalance time r value))
                  (setf value (* (path-discount) (- value p7)))))))
        (:final (:residual value))))))

(defun v5-checks ()
  (format t "~&V5: immunizing a liability under one factor~%")
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.1d0 :sigma 0.01d0))
         (coarse (immunization-run hw m 6))
         (fine (immunization-run hw m 24)))
    (multiple-value-bind (mean se sd24) (summary fine :residual)
      (validation-row "V5 factor-delta hedge (N=24): mean discounted residual" mean 0d0 se)
      (check-that (format nil "factor-delta hedge: the mean discounted residual ~,2E is 0 within 3 SE (~,2E)" mean se)
                  (<= (abs mean) (* 3 se)))
      (let ((ratio (/ sd24 (nth-value 2 (summary coarse :residual)))))
        (validation-row "V5 factor-delta hedge: sd(N=24) / sd(N=6)" ratio 0.5d0 nil)
        (check-that (format nil "... its standard deviation falls by ~,3F, in [0.4, 0.6], when N quadruples" ratio)
                    (<= 0.4d0 ratio 0.6d0))))
    (let ((sd6 (nth-value 2 (summary (immunization-run hw m 6 :weight :macaulay) :residual)))
          (sd24 (nth-value 2 (summary (immunization-run hw m 24 :weight :macaulay) :residual))))
      (validation-row "V5 Macaulay hedge: sd(N=24) / sd(N=6)" (/ sd24 sd6) nil nil)
      (format t "~&  info Macaulay-duration hedge: residual sd ~,2E at N=6, ~,2E at N=24 (ratio ~,3F)~%"
              sd6 sd24 (/ sd24 sd6)))))

;;; --------------------------------------------------------------------
;;; V6: exposure of a swaption's underlying
;;; --------------------------------------------------------------------

(defun v6-checks ()
  (format t "~&V6: exposure of a swaption's underlying (Jamshidian)~%")
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.03d0 :sigma 0.008d0))
         (swap (euribor-swap :payer 0.033d0 #D"2028-09-28" #D"2033-09-28"))
         (expiry (first (fixing-dates swap)))
         (r (simulate ((((:curve :eur) hw :knots (monthly-knots))) m (list expiry) :n-paths 8000)
              (:portfolio book (make-portfolio "swap" (make-position swap)) :record (:value) :engine :discounting)
              (:final (:payer (* (path-discount) (max 0d0 (price book :discounting market))))
                      (:receiver (* (path-discount) (max 0d0 (- (price book :discounting market)))))
                      (:swap (* (path-discount) (price book :discounting market)))))))
    (iter (for name in '(:payer :receiver))
          (for jamshidian = (price (make-swaption (on-swap (if (eq name :payer) swap (euribor-swap :receiver 0.033d0 #D"2028-09-28" #D"2033-09-28")))
                                                  (expires expiry))
                                   (make-engine :jamshidian :process hw) m))
          (multiple-value-bind (mean se) (summary r name)
            (validation-row (format nil "V6 ~(~A~) swaption by exposure vs Jamshidian" name) mean jamshidian se)
            (check-that (format nil "~(~A~): mean discounted positive exposure at expiry ~,1F = Jamshidian ~,1F within 3 SE (~,1F)"
                                name mean jamshidian se)
                        (<= (abs (- mean jamshidian)) (* 3 se)))))
    (multiple-value-bind (mean se)
        (simulation-summary r (map 'fincl:double-vector #'- (simulation-values r :payer) (simulation-values r :receiver)))
      (let ((forward (swap-npv swap m)))
        (validation-row "V6 payer - receiver vs the forward swap (parity)" mean forward se)
        (check-that (format nil "payer - receiver ~,1F = the forward swap's value ~,1F within 3 SE (~,1F)" mean forward se)
                    (<= (abs (- mean forward)) (* 3 se)))))))

(define-checks simulation-validation-checks
  (let ((*validation-rows* '()))
    (v1-checks)
    (v2-checks)
    (v3-checks)
    (v4-checks)
    (v5-checks)
    (v6-checks)
    (print-validation-table)))

(defun run-simulation-validation-tests ()
  "Run only the simulation validation studies (task 32, Phase 4). Returns T
when all pass."
  (with-pricing-kernel ()
    (run-suite-list '(simulation-validation-checks))))
