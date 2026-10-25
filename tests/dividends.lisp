;;;; dividends.lisp --- discrete dividends (task 21)
;;;;
;;;; The reference is tests/fixtures/dividends.sexp, from QuantLib (made by
;;;; generate_dividends.cpp): Europeans by CashDividendEuropeanEngine
;;;; (Healy's method for the spot model, and the escrowed model), Americans
;;;; by FdBlackScholesVanillaEngine on a 2000 x 2000 grid.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defparameter *dividend-today* #D"2026-10-26"
  "The valuation date of the QuantLib dividend fixtures.")

(defun dividend-fixtures ()
  "The entries of dividends.sexp as plists."
  (iter (for (name kind exercise model spot strike rate yield vol valuation expiry dividends npv)
             in (first (read-fixture "dividends.sexp")))
        (collect `(:name ,name :kind ,kind :exercise ,exercise :model ,model :spot ,spot :strike ,strike
                   :rate ,rate :yield ,yield :vol ,vol :valuation ,(parse-date valuation)
                   :expiry ,(parse-date expiry) :dividends ,dividends :npv ,npv))))

(defun fixture-market (fx)
  (make-bs-market :valuation-date (getf fx :valuation) :spot (getf fx :spot) :rate (getf fx :rate)
                  :dividend (getf fx :yield) :vol (getf fx :vol)
                  :dividend-schedule (getf fx :dividends)))

(defun fixture-option (fx)
  (make-option-from-clauses `((,(getf fx :kind) ,(getf fx :strike)) (,(getf fx :exercise))
                              (:expires ,(getf fx :expiry)))))

(defun fixture-label (fx)
  (format nil "~A ~(~A ~A ~A~) K=~D" (getf fx :name) (getf fx :model) (getf fx :exercise)
          (getf fx :kind) (round (getf fx :strike))))

(defun gbm-with (model) (make-gbm :dividend-model model))

;;; --------------------------------------------------------------------
;;; Phase 1: market data and forwards
;;; --------------------------------------------------------------------

(defun dividend-market-checks ()
  (format t "~&Discrete dividends: schedules and forwards~%")
  (let* ((today *dividend-today*)
         (schedule '(("2027-01-15" 1.5d0 :pay-date "2027-02-01") ("2027-07-15" 2d0)))
         (rates-only (make-bs-market :valuation-date today :spot 100 :rate 0.05d0 :vol 0.2d0
                                     :dividend-schedule schedule))
         (with-yield (make-bs-market :valuation-date today :spot 100 :rate 0.05d0 :dividend 0.02d0
                                     :vol 0.2d0 :dividend-schedule schedule))
         (ex-times (mapcar (lambda (d) (market-time rates-only (parse-date d)))
                           '("2027-01-15" "2027-07-15")))
         (pay-times (mapcar (lambda (d) (market-time rates-only (parse-date d)))
                            '("2027-02-01" "2027-07-15")))
         (amounts '(1.5d0 2d0)))
    (flet ((expected (m q horizon)
             ;; F(T) = (S Q(T) - sum_{ex_i <= T} D_i P(pay_i) Q(T)/Q(ex_i)) / P(T)
             (flet ((p (x) (discount-factor m x)) (big-q (x) (exp (- (* q x)))))
               (/ (- (* 100 (big-q horizon))
                     (iter (for ex in ex-times) (for pay in pay-times) (for d in amounts)
                           (when (<= ex horizon)
                             (sum (/ (* d (p pay) (big-q horizon)) (big-q ex))))))
                  (p horizon)))))
      (dolist (horizon '(0.1d0 0.5d0 1d0 2d0))
        (dolist (case `(("rates only" ,rates-only 0d0) ("with a yield" ,with-yield 0.02d0)))
          (destructuring-bind (label m q) case
            (let ((e (expected m q horizon)))
              (check-that (format nil "forward to ~,1F, ~A, matches the formula to 1e-14" horizon label)
                          (< (abs (/ (- (forward m horizon) e) e)) 1d-14)))))))
    (let ((m (make-bs-market :valuation-date today :spot 100 :rate 0.05d0 :vol 0.2d0
                             :dividend-schedule '(("2027-01-15" 0.02d0 :kind :proportional)
                                                  ("2027-07-15" 0.03d0 :kind :proportional)))))
      (check-that "proportional dividends scale the forward by prod (1 - delta)"
                  (< (abs (- (forward m 1d0) (* 100 (exp 0.05d0) 0.98d0 0.97d0))) 1d-12)))
    ;; Dividends gone ex before today, or after the expiry, change nothing.
    (let* ((plain (make-bs-market :valuation-date today :spot 100 :rate 0.04d0 :dividend 0.01d0 :vol 0.25d0))
           (outside (make-bs-market :valuation-date today :spot 100 :rate 0.04d0 :dividend 0.01d0 :vol 0.25d0
                                    :dividend-schedule '(("2026-10-01" 2) ("2028-01-15" 2))))
           (expiry #D"2027-10-26")
           (options `(("European call" ,(make-option (call 100) (expires expiry)))
                      ("American put" ,(make-option (put 100) (american) (expires expiry)))))
           (engines `(:black-scholes :binomial :fd :cos :baw
                      ,(make-engine :mc :n-paths 20000)
                      ,(make-engine :mc :n-paths 20000 :process (gbm-with :escrowed)))))
      (check-that "a dividend gone ex before the valuation date is not an event"
                  (= 1 (length (discrete-dividends outside))))
      (dolist (case options)
        (destructuring-bind (label o) case
          (dolist (engine engines)
            (let ((a (handler-case (multiple-value-list (price o engine plain)) (fincl-error () :error)))
                  (b (handler-case (multiple-value-list (price o engine outside)) (fincl-error () :error))))
              (unless (eq a :error)
                (check-that (format nil "~A by ~A: dividends outside its life give the same price (=)"
                                    label (fincl::engine-designator-key engine))
                            (equal (subseq a 0 2) (subseq b 0 2)))))))))
    ;; Bumps.
    (let* ((m (make-market :valuation-date today
                           :underlyings '((:x :spot 100 :vol 0.2d0
                                           :dividend-schedule (("2027-01-15" 1.5d0) ("2027-07-15" 2d0))))
                           :curves '((:c :rate 0.05d0))))
           (bumped (bump m `(:dividend-schedule :x ,#D"2027-07-15") '(:absolute 1/2)))
           (all (bump m '(:dividend-schedule :x) '(:relative 1/10)))
           (view (market-view m)) (bumped-view (market-view bumped)))
      (check-that "bumping the July dividend leaves forwards before its ex-date unchanged (=)"
                  (every (lambda (h) (= (forward view h) (forward bumped-view h))) '(0.1d0 0.3d0 0.7d0)))
      (check "bumping the July dividend by 0.5 lowers the 1Y forward by 0.5 P(ex)/P(1Y)"
             (* 0.5d0 (/ (discount-factor view (market-time view #D"2027-07-15")) (discount-factor view 1d0)))
             (- (forward view 1d0) (forward bumped-view 1d0)) 1d-12)
      (check "a relative bump moves every amount"
             (* 0.1d0 (- (forward (market-view m) 0d0) (forward (market-view m) 1d0)
                         (- (forward (market-view m) 0d0) (* 100 (exp 0.05d0)))))
             (- (forward (market-view m) 1d0) (forward (market-view all) 1d0)) 1d-10)
      (check-error "bumping a date with no dividend" (bump m `(:dividend-schedule :x ,#D"2027-03-01") '(:absolute 1))
                   invalid-argument))
    (check-error "a negative cash dividend" (make-dividend-schedule '(("2027-01-15" -1))) invalid-argument)
    (check-error "a proportional dividend of 100%" (make-dividend-schedule '(("2027-01-15" 1 :kind :proportional)))
                 invalid-argument)
    (check-error "a pay date before the ex-date"
                 (make-dividend-schedule '(("2027-01-15" 1 :pay-date "2027-01-01"))) invalid-argument)))

;;; --------------------------------------------------------------------
;;; Phase 2: Europeans and the tree
;;; --------------------------------------------------------------------

(defun bivariate-normal-checks ()
  (format t "~&Bivariate normal CDF~%")
  (flet ((oracle (a b rho)
           (let ((s (sqrt (- 1 (* rho rho)))))
             (fincl::integrate-gauss-legendre (lambda (x) (* (norm-pdf x) (norm-cdf (/ (- b (* rho x)) s))))
                                       -9d0 a :panels 400))))
    (let ((worst 0d0))
      (dolist (a '(-3d0 -1.5d0 -0.3d0 0d0 0.4d0 1.2d0 2.5d0))
        (dolist (b '(-2.7d0 -0.8d0 0d0 0.6d0 1.9d0))
          (dolist (rho '(-0.999d0 -0.95d0 -0.9d0 -0.6d0 -0.2d0 0.1d0 0.5d0 0.8d0 0.93d0 0.99d0 0.9999d0))
            (setf worst (max worst (abs (- (bivariate-norm-cdf a b rho) (oracle a b rho))))))))
      (check-that (format nil "M(a, b; rho) against quadrature over 385 points to 1e-14 (~,1E)" worst)
                  (< worst 1d-14)))
    (check "M(0, 0; rho) = 1/4 + asin(rho)/(2 pi)" (+ 0.25d0 (/ (asin 0.5d0) (* 2 pi)))
           (bivariate-norm-cdf 0 0 0.5d0) 1d-15)
    (check "rho = 1: Phi(min(a, b))" (norm-cdf -0.2d0) (bivariate-norm-cdf 0.3d0 -0.2d0 1) 1d-15)
    (check "rho = -1: Phi(a) - Phi(-b)" (- (norm-cdf 0.3d0) (norm-cdf -0.2d0))
           (bivariate-norm-cdf 0.3d0 0.2d0 -1) 1d-15)))

(defun dividend-european-checks ()
  (format t "~&Discrete dividends: Europeans~%")
  (let ((fixtures (remove :american (dividend-fixtures) :key (lambda (fx) (getf fx :exercise))))
        (worst 0d0))
    (dolist (fx fixtures)
      (let* ((m (fixture-market fx)) (o (fixture-option fx))
             (model (getf fx :model))
             (v (price o (make-engine :black-scholes :dividend-model model) m)))
        (if (eq model :escrowed)
            (check (format nil "~A: Black on the adjusted forward vs QuantLib" (fixture-label fx))
                   (getf fx :npv) v 1d-12)
            (setf worst (max worst (abs (- v (getf fx :npv))))))))
    (check-that (format nil "Bos-Vandermark against QuantLib's spot model (Healy), worst ~,4F < 0.01" worst)
                (< worst 0.01d0)))
  ;; The escrowed European is Black-Scholes on the spot less the dividends.
  (let* ((today *dividend-today*)
         (m (make-bs-market :valuation-date today :spot 100 :rate 0.05d0 :vol 0.3d0
                            :dividend-schedule '(("2027-01-25" 3) ("2027-07-26" 3))))
         (pv (+ (* 3 (discount-factor m (market-time m #D"2027-01-25")))
                (* 3 (discount-factor m (market-time m #D"2027-07-26")))))
         (adjusted (make-bs-market :valuation-date today :spot (- 100 pv) :rate 0.05d0 :vol 0.3d0))
         (call (make-option (call 100) (expires #D"2027-10-26")))
         (escrowed (make-engine :black-scholes :dividend-model :escrowed)))
    (check "escrowed European = Black-Scholes on S - PV(D)" (price call :black-scholes adjusted)
           (price call escrowed m) 1d-12)
    (multiple-value-bind (v se) (price call (make-engine :mc :n-paths 400000 :process (gbm-with :escrowed)) m)
      (check-that (format nil "escrowed Monte Carlo within 3 SE of the closed form (~,4F +- ~,4F)" v se)
                  (< (abs (- v (price call escrowed m))) (* 3 se))))
    (let ((fd (price call (make-engine :fd :n-space 400 :n-time 400) m)))
      (multiple-value-bind (v se) (price call (make-engine :mc :n-paths 500000) m)
        (check-that (format nil "spot-model Monte Carlo (500k paths) within 3 SE of FD (~,4F +- ~,4F vs ~,4F)"
                            v se fd)
                    (< (abs (- v fd)) (* 3 se)))))
    ;; Under the spot model with a control variate the European variate is
    ;; priced by FD, not by Bos-Vandermark.
    (multiple-value-bind (v se) (price call (make-engine :mc :n-paths 100000 :control-variates '(:european)) m)
      (check "European control variate under dividends: the FD price" (price call (make-engine :fd) m) v
             (* 3 (max se 1d-6)))))
  ;; Proportional dividends are a scaled spot.
  (let* ((today *dividend-today*)
         (m (make-bs-market :valuation-date today :spot 100 :rate 0.05d0 :vol 0.3d0
                            :dividend-schedule '(("2027-01-25" 0.02d0 :kind :proportional)
                                                 ("2027-07-26" 0.03d0 :kind :proportional))))
         (scaled (make-bs-market :valuation-date today :spot (* 100 0.98d0 0.97d0) :rate 0.05d0 :vol 0.3d0)))
    (dolist (kind '(:call :put))
      (let ((o (make-option-from-clauses `((,kind 100) (:expires ,#D"2027-10-26")))))
        (check (format nil "proportional dividends, European ~(~A~): Black-Scholes on S prod (1 - delta)" kind)
               (price o :black-scholes scaled) (price o :black-scholes m) 1d-12)
        (check (format nil "proportional dividends, European ~(~A~): FD" kind)
               (price o :black-scholes scaled) (price o (make-engine :fd :n-space 400 :n-time 400) m) 1d-4)))))

(defun dividend-binomial-checks ()
  (format t "~&Discrete dividends: binomial tree~%")
  ;; Vellekoop and Nieuwenhuis (spot model) and the escrowed tree against
  ;; QuantLib's fine-grid FD.
  (dolist (fx (remove :european (dividend-fixtures) :key (lambda (fx) (getf fx :exercise))))
    (check (format nil "~A: tree vs QuantLib" (fixture-label fx)) (getf fx :npv)
           (price (fixture-option fx) (make-engine :binomial :process (gbm-with (getf fx :model)))
                  (fixture-market fx))
           2d-3)))

;;; --------------------------------------------------------------------
;;; Phase 3: finite differences and Roll-Geske-Whaley
;;; --------------------------------------------------------------------

(defun dividend-fd-checks ()
  (format t "~&Discrete dividends: finite differences~%")
  (dolist (fx (remove :european (dividend-fixtures) :key (lambda (fx) (getf fx :exercise))))
    (let* ((m (fixture-market fx)) (o (fixture-option fx))
           (process (gbm-with (getf fx :model)))
           (fd (price o (make-engine :fd :n-space 400 :n-time 400 :process process) m)))
      (check (format nil "~A: FD vs QuantLib" (fixture-label fx)) (getf fx :npv) fd 1d-3)
      ;; Vellekoop-Nieuwenhuis is the spot model's tree.
      (when (eq (getf fx :model) :spot)
        (check (format nil "~A: FD vs the Vellekoop-Nieuwenhuis tree" (fixture-label fx))
               (price o (make-engine :binomial :n-steps 4000 :process process) m) fd 1d-3))))
  ;; With r > 0 and no yield a call is exercised only just before an
  ;; ex-date: the American is the Bermudan exercisable on the ex-date (on
  ;; the price before the dividend) and at expiry.
  (let* ((m (make-bs-market :valuation-date *dividend-today* :spot 100 :rate 0.05d0 :vol 0.2d0
                            :dividend-schedule '(("2027-05-26" 4))))
         (engine (make-engine :fd :n-space 400 :n-time 400))
         (american (price (make-option (call 100) (american) (expires #D"2027-10-26")) engine m))
         (bermudan (price (make-option (call 100) (bermudan :dates (list #D"2027-05-26" #D"2027-10-26"))) engine m))
         (european (price (make-option (call 100) (expires #D"2027-10-26")) engine m)))
    (check-that (format nil "the call's early-exercise premium is real (~,4F over the European)"
                        (- american european))
                (> (- american european) 0.1d0))
    (check "exercise only just before the ex-date: American call = Bermudan (ex-date, expiry)"
           bermudan american 2d-4))
  ;; Roll-Geske-Whaley: Haug's example in years, QuantLib in days, and FD.
  (let ((m (make-bs-market :valuation-date *dividend-today* :spot 80 :rate 0.06d0 :vol 0.3d0
                           :dividend-schedule `((,(date+ *dividend-today* 91) 4)))))
    (check "Roll-Geske-Whaley, Haug's example (t1 = 0.25, T = 1/3): 4.3860"
           4.3860d0 (fincl::roll-geske-whaley m 82 (/ 1d0 3) 0.25d0 4 0.09d0) 5d-5)
    (let ((o (make-option (call 82) (american) (expires (date+ *dividend-today* 122)))))
      (check "Roll-Geske-Whaley vs QuantLib's escrowed FD" 4.384947d0 (price o :rgw m) 1d-5)
      (check "Roll-Geske-Whaley vs escrowed FD at 800 x 800"
             (price o (make-engine :fd :n-space 800 :n-time 800 :process (gbm-with :escrowed)) m)
             (price o :rgw m) 1d-3)
      (check-that "automatic selection takes Roll-Geske-Whaley for an escrowed GBM"
                  (typep (nth-value 2 (price o (gbm-with :escrowed) m)) 'roll-geske-whaley-engine))
      (check-error "Roll-Geske-Whaley declines the spot model" (price o (make-engine :rgw :dividend-model :spot) m)
                   model-domain-error)
      (check-error "Roll-Geske-Whaley declines a put" (price (make-option (put 82) (american) (expires (date+ *dividend-today* 122))) :rgw m)
                   model-domain-error)))
  ;; Other dynamics: the dividend steps are the process's own.
  (let* ((m (make-bs-market :valuation-date *dividend-today* :spot 100 :rate 0.05d0 :vol 0.25d0
                            :dividend-schedule '(("2027-03-26" 3))))
         (put (make-option (put 100) (american) (expires #D"2027-10-26")))
         (gbm (price put (make-engine :fd :n-space 400 :n-time 400) m)))
    (check "Heston FD with xi near 0 = GBM FD, American put with a cash dividend"
           gbm (price put (make-engine :fd :n-space 200 :n-variance 30 :n-time 200
                                           :process (make-heston :v0 0.0625d0 :theta 0.0625d0 :xi 1d-4 :kappa 1))
                      m)
           2d-3)
    (check "local vol FD on a flat surface = GBM FD, American put with a cash dividend"
           gbm (price put (make-engine :fd :n-space 400 :n-time 400 :process (make-local-vol)) m) 1d-3)
    (multiple-value-bind (v se) (price put (make-engine :mc :n-paths 100000 :n-steps 50) m)
      (check-that (format nil "LSM American put with a cash dividend within 3 SE + 0.02 of FD (~,4F +- ~,4F vs ~,4F)"
                          v se gbm)
                  (< (abs (- v gbm)) (+ (* 3 se) 0.02d0))))
    (let ((european (make-option (call 100) (expires #D"2027-10-26")))
          (heston (make-heston :xi 0.4d0 :rho -0.6d0)))
      (multiple-value-bind (v se) (price european (make-engine :mc :n-paths 200000 :process heston) m)
        (let ((fd (price european (make-engine :fd :n-space 200 :n-variance 50 :n-time 200 :process heston) m)))
          (check-that (format nil "Heston European with a cash dividend: Monte Carlo within 3 SE of FD (~,4F +- ~,4F vs ~,4F)"
                              v se fd)
                      (< (abs (- v fd)) (* 3 se))))))))

(defun dividend-engine-checks ()
  (format t "~&Discrete dividends: engines that do not model them, Greeks, implied vol~%")
  (let* ((m (make-bs-market :valuation-date *dividend-today* :spot 100 :rate 0.05d0 :vol 0.25d0
                            :dividend-schedule '(("2027-03-26" 3))))
         (call (make-option (call 100) (expires #D"2027-10-26")))
         (put (make-option (put 100) (american) (expires #D"2027-10-26"))))
    (check-error "COS declines discrete dividends" (price call :cos m) model-domain-error)
    (check-error "BAW declines discrete dividends" (price put :baw m) model-domain-error)
    (check-error "Black-Scholes declines a barrier with discrete dividends"
                 (price (make-option (call 100) (barrier :down-and-out 80) (expires #D"2027-10-26")) :black-scholes m)
                 model-domain-error)
    (check-error "Monte Carlo declines a continuous barrier's bridge across a dividend"
                 (price (make-option (call 100) (barrier :down-and-out 80) (expires #D"2027-10-26"))
                        (make-engine :mc :n-paths 1000) m)
                 model-domain-error)
    (check-error "pathwise Greeks decline discrete dividends"
                 (greeks call (make-engine :mc :n-paths 1000) m :method :pathwise :which '(:delta))
                 model-domain-error)
    (check-that "automatic selection: FD for the spot-model European (Bos-Vandermark is an approximation), the tree for the American"
                (and (typep (nth-value 2 (price call :auto m)) 'fd-engine)
                     (typep (nth-value 2 (price put :auto m)) 'binomial-engine)))
    (check-that "automatic selection: Black-Scholes for the escrowed European, exact"
                (typep (nth-value 2 (price call (gbm-with :escrowed) m)) 'black-scholes-engine))
    (let ((bumped (greeks call :black-scholes m :which '(:delta :gamma)))
          (grid (greeks call (make-engine :fd :n-space 400 :n-time 400) m :which '(:delta :gamma))))
      (check "European delta with a dividend: Bos-Vandermark bumps vs the FD grid"
             (getf grid :delta) (getf bumped :delta) 2d-3)
      (check "European gamma with a dividend: Bos-Vandermark bumps vs the FD grid"
             (getf grid :gamma) (getf bumped :gamma) 2d-4))
    (let ((escrowed (make-engine :black-scholes :dividend-model :escrowed)))
      (check "escrowed European delta: bumps vs the escrowed FD grid"
             (getf (greeks call (make-engine :fd :n-space 400 :n-time 400 :process (gbm-with :escrowed)) m
                           :which '(:delta))
                   :delta)
             (getf (greeks call escrowed m :which '(:delta)) :delta) 1d-3))
    (check "implied vol of a spot-model price under its own engine round-trips"
           0.25d0 (implied-volatility call (price call :black-scholes m) :black-scholes m) 1d-8)))

(defun dividend-checks ()
  (dividend-market-checks)
  (dividend-european-checks)
  (dividend-binomial-checks)
  (dividend-fd-checks)
  (dividend-engine-checks))

(defun run-dividend-tests ()
  "Run only the discrete-dividend checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (bivariate-normal-checks)
      (dividend-checks))
    (format t "~&~[All dividend checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
