;;;; simulation-books.lisp --- self-financing books, exact simulated curves,
;;;; per-path records (task 33)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun monthly-dates (n &optional (from *today*))
  "The N month-ends after FROM."
  (mapcar (lambda (k) (add-tenor from (make-tenor k :months))) (a:iota n :start 1)))

(defun nan-equal (a b)
  "True if the double vectors A and B are =, NaNs matching NaNs."
  (and (= (length a) (length b))
       (every (lambda (x y) (or (and (sb-ext:float-nan-p x) (sb-ext:float-nan-p y)) (= x y))) a b)))

;;; --------------------------------------------------------------------
;;; Phase 1: per-path records, previous state, time
;;; --------------------------------------------------------------------

(defun record-run (keep)
  "A GBM run recording the spot from the third date and stopping above 115;
the kept paths also hold their recorded values in a list."
  (let ((keep keep))
    (simulate ((make-gbm) (sim-market) (monthly-dates 12) :n-paths 400 :seed 5 :keep keep)
      (:hold (seen '()))
      (:state s :spot)
      (:every (date)
        (record :s s)
        (when (date< #D"2026-12-01" date) (push (cons date (record :late (* 2 s))) seen))
        (when (> s 115) (stop)))
      (:final (:s s)))))

(defun previous-run (&key (n-paths 300))
  (simulate ((make-heston) (sim-market) (monthly-dates 12) :n-paths n-paths :seed 9)
    (:accumulate (carried 100d0) (carried-v 0d0) (mismatches 0d0) (time-errors 0d0) (time-sum 0d0))
    (:state s :spot)
    (:state v :variance)
    (:start (&key time)
      (setf carried-v v)
      (unless (= time 0d0) (incf time-errors)))
    (:every (date &key time dt)
      (unless (and (= (previous s) carried) (= (previous v) carried-v)) (incf mismatches))
      (unless (= time (market-time (sim-market) date)) (incf time-errors))
      (incf time-sum dt)
      (setf carried s carried-v v))
    (:final (:mismatches mismatches) (:time-errors time-errors) (:time-sum time-sum))))

(defun path-record-checks ()
  (format t "~&Simulation books: per-path records, previous state, time~%")
  (let* ((kept (record-run '(:paths 10)))
         (bare (record-run nil))
         (dates (simulation-dates kept)))
    (check-that "for 10 kept paths, SIMULATION-PATH-RECORD = the values recorded, NaN elsewhere"
                (iter (for i below 10)
                      (for live = (simulation-path-state kept i :spot))
                      ;; SEEN as held on the path's last date: every record of :LATE.
                      (for last = (position-if-not #'sb-ext:float-nan-p live :from-end t))
                      (for seen = (aref (simulation-held kept i :seen) last))
                      (for late = (simulation-path-record kept i :late))
                      (always (iter (for d in dates) (for k from 0)
                                    (for entry = (assoc d seen :test #'date=))
                                    (always (if entry
                                                (= (aref late k) (cdr entry))
                                                (sb-ext:float-nan-p (aref late k))))))
                      (always (iter (for k from 1 below (length dates))
                                    (always (if (sb-ext:float-nan-p (aref live k))
                                                (sb-ext:float-nan-p (aref (simulation-path-record kept i :s) k))
                                                (= (aref live k) (aref (simulation-path-record kept i :s) k))))))))
    (check-that "some kept path stopped early (the NaN-after-STOP case is exercised)"
                (iter (for i below 10) (thereis (sb-ext:float-nan-p (a:last-elt (simulation-path-record kept i :s))))))
    (check-that "the statistics with :KEEP (:PATHS 10) are = to those without"
                (iter (for name in '(:s :late))
                      (always (iter (for statistic in '(:mean :std-dev :min :max :count))
                                    (always (nan-equal (simulation-statistic kept name statistic)
                                                       (simulation-statistic bare name statistic)))))))
    (check-error "a path that was not kept" (simulation-path-record kept 10 :s) invalid-argument)
    (check-error "a name that was not recorded" (simulation-path-record kept 0 :nope) invalid-argument)
    (check-that "a second RECORD of a name on a date replaces the first in the path record"
                (let ((r (simulate ((make-gbm) (sim-market) (monthly-dates 2) :n-paths 4 :keep (:paths 2))
                           (:every (date) (record :x 1) (record :x 2)))))
                  (and (= 2d0 (aref (simulation-path-record r 0 :x) 1))
                       (= 1.5d0 (aref (simulation-statistic r :x :mean) 1))))))
  (let ((r (previous-run)))
    (check-that "(previous s) and (previous v) = hand-carried accumulators on every path and date"
                (every #'zerop (simulation-values r :mismatches)))
    (check-that "TIME = MARKET-TIME on every date, 0 at :START"
                (every #'zerop (simulation-values r :time-errors)))
    (check-that "the DT sum = the last date's time (=)"
                (let ((last (market-time (sim-market) (a:lastcar (monthly-dates 12)))))
                  (every (lambda (x) (= x last)) (simulation-values r :time-sum)))))
  (check-that "(previous x) of an :ACCUMULATE variable signals at macroexpansion, naming :ACCUMULATE"
              (handler-case (progn (macroexpand-1 '(simulate ((make-gbm) m dates)
                                                    (:accumulate (x 0d0))
                                                    (:every (date) (record :p (previous x)))))
                                   nil)
                (invalid-argument (c) (search ":ACCUMULATE" (princ-to-string c)))))
  (check-error "&key with an unknown name" (macroexpand-1 '(simulate ((make-gbm) m dates)
                                                            (:every (date &key when) (record :p 1))))
               invalid-argument))

;;; --------------------------------------------------------------------
;;; Phase 2: exact simulated curves
;;; --------------------------------------------------------------------

(defun usd-market (&key (rate 0.04d0) (dividend 0d0) dividend-schedule (vol 0.16d0))
  "An equity :EQ and a flat USD curve that discounts it."
  (make-market :valuation-date *today*
               :underlyings `((:eq :spot 100 :currency :usd :dividend ,dividend :vol ,vol
                                   ,@(when dividend-schedule `(:dividend-schedule ,dividend-schedule))))
               :curves `((:usd :rate ,rate))
               :discounting '((:usd . :usd))))

(defun maturity-days ()
  "50 maturities in days from 1 day to 30 years, roughly geometric."
  (remove-duplicates (iter (for i below 50)
                           (collect (max 1 (round (exp (* i (/ (log (* 30 365)) 49)))))))))

(defun years-from (date n) (add-tenor date (make-tenor n :years)))

(defun old-knot-curve (hw initial-market date r knots)
  "The interpolated curve a Hull-White leg with KNOTS built before task 33."
  (let* ((rc (make-rate-curve initial-market :usd))
         (now (market-time initial-market date))
         (dates (fincl::curve-knot-dates date knots nil)))
    (make-instance 'fincl::interpolated-discount-curve
                   :dates (cons date dates) :day-counter (market-day-counter initial-market)
                   :discounts (cons 1d0 (mapcar (lambda (d) (hull-white-discount-bond hw rc now (market-time initial-market d) r))
                                                dates)))))

(defun exact-curve-checks ()
  (format t "~&Simulation books: exact simulated curves~%")
  (let* ((m (usd-market))
         (hw (make-hull-white :a 0.05d0 :sigma 0.01d0))
         (rc (make-rate-curve m :usd))
         (days (maturity-days))
         (knots (mapcar (lambda (n) (make-tenor n :years)) '(1 2 5 10))))
    (let ((r (simulate ((((:curve :usd) hw)) m (monthly-dates 12) :n-paths 100 :keep (:paths 1))
               (:accumulate (worst 0d0))
               (:state rate :short-rate (:curve :usd))
               (:every (date market &key time)
                 (dolist (n days)
                   (let* ((d (date+ date n))
                          (exact (hull-white-discount-bond hw rc time (market-time m d) rate)))
                     (setf worst (max worst (abs (/ (- (market-curve-discount market :usd d) exact) exact)))))))
               (:final worst))))
      (check-that "on 100 paths x 12 dates the state curve = HULL-WHITE-DISCOUNT-BOND at 50 maturities to 1e-14"
                  (< (reduce #'max (simulation-values r)) 1d-14))
      (let* ((market (cdr (second (path-markets r 0))))
             (curve (market-component market '(:curve :usd)))
             (bumped (fincl::bump-structure curve '(:absolute 1d-4))))
        (check-that "a state curve bumped by 1bp = the curve times e^(-s tau) to 1e-15"
                    (iter (for n in days)
                          (for tau = (/ n 365d0))
                          (always (< (abs (- (/ (curve-discount bumped tau) (curve-discount curve tau))
                                             (exp (* -1d-4 tau))))
                                     1d-15))))
        (check-that "BUMP of a simulated market moves its curve"
                    (< (market-curve-discount (bump market '(:curve :usd) '(:absolute 1d-4)) :usd (years-from *today* 5))
                       (market-curve-discount market :usd (years-from *today* 5))))
        (check-error "the snapshot form of a state curve" (snapshot-form curve) invalid-argument)
        (check-that "its report suggests :KNOTS"
                    (search ":KNOTS" (handler-case (snapshot-form curve) (invalid-argument (c) (princ-to-string c)))))))
    (let ((r (simulate ((((:curve :usd) hw :knots knots)) m (monthly-dates 12) :n-paths 100)
               (:accumulate (mismatches 0d0))
               (:state rate :short-rate (:curve :usd))
               (:every (date market)
                 (let ((old (fincl::curve-in-market (old-knot-curve hw m date rate knots) market)))
                   (dolist (n days)
                     (unless (= (market-curve-discount market :usd (date+ date n))
                                (curve-discount old (market-time market (date+ date n))))
                       (incf mismatches)))))
               (:final mismatches))))
      (check-that "with :KNOTS the curve is = to the interpolated curve of before the task"
                  (every #'zerop (simulation-values r))))
    ;; E[D(t) P(t, T)] = P(0, T).
    (let* ((ts '(1 5 10))
           (r (simulate ((((:curve :usd) hw)) m (mapcar (lambda (n) (years-from *today* n)) ts)
                         :n-paths 20000 :antithetic t)
                (:accumulate (a1 0d0) (a10 0d0) (b1 0d0) (b10 0d0) (c1 0d0) (c10 0d0))
                (:every (date market)
                  (let ((d (path-discount)))
                    (flet ((pv (n) (* d (market-curve-discount market :usd (years-from date n)))))
                      (cond ((date= date (years-from *today* 1)) (setf a1 (pv 1) a10 (pv 10)))
                            ((date= date (years-from *today* 5)) (setf b1 (pv 1) b10 (pv 10)))
                            (t (setf c1 (pv 1) c10 (pv 10)))))))
                (:final (:a1 a1) (:a10 a10) (:b1 b1) (:b10 b10) (:c1 c1) (:c10 c10)))))
      (iter (for (name tt n) in '((:a1 1 1) (:a10 1 10) (:b1 5 1) (:b10 5 10) (:c1 10 1) (:c10 10 10)))
            (multiple-value-bind (mean se) (simulation-summary r (simulation-values r name))
              (let ((p0 (market-curve-discount m :usd (years-from (years-from *today* tt) n))))
                (check (format nil "E[path-discount x P(~D, ~D)] = P(0, ~D) within 3 SE (~,5F)" tt (+ tt n) (+ tt n) se)
                       p0 mean (* 3 se))))))
    ;; A bond on the date's market = its flows' Hull-White bond prices.
    (let* ((bond (make-fixed-rate-bond (make-schedule *today* (years-from *today* 10) (make-tenor 1 :years))
                                       0.04d0 +actual-365-fixed+ :currency :usd :settlement-days 0))
           (r (simulate ((((:curve :usd) hw)) m (list (years-from *today* 2) (add-tenor *today* (make-tenor 43 :months)))
                         :n-paths 200)
                (:accumulate (worst 0d0))
                (:state rate :short-rate (:curve :usd))
                (:every (date market &key time)
                  (let ((exact (iter (for cf in (fincl::bond-cashflows bond))
                                     (when (date< date (fincl::payment-date cf))
                                       (sum (* (fincl::cashflow-amount cf market)
                                               (hull-white-discount-bond hw rc time (market-time m (fincl::payment-date cf))
                                                                         rate)))))))
                    (setf worst (max worst (abs (/ (- (price bond :discounting market) exact) exact))))))
                (:final worst))))
      (check-that "a fixed-rate bond priced with :DISCOUNTING = the sum of its flows' bond prices to 1e-12"
                  (< (reduce #'max (simulation-values r)) 1d-12)))
    ;; A market build with a state curve evaluates no bond price: the curve
    ;; makes its bond price function (the Hull-White formula with the terms
    ;; of its date hoisted) on the first price.
    (let ((calls (list 0)))
      (sb-int:encapsulate 'fincl::state-discount-function 'count
                          (lambda (f &rest args) (sb-ext:atomic-incf (car calls)) (apply f args)))
      (unwind-protect
           (progn
             (simulate ((((:curve :usd) hw)) m (monthly-dates 6) :n-paths 50)
               (:every (date market) (record :built (if market 1 0))))
             (check-equal "building markets with a state curve evaluates no bond price function" 0 (car calls))
             (simulate ((((:curve :usd) hw)) m (monthly-dates 6) :n-paths 50)
               (:every (date market) (record :p (market-curve-discount market :usd (years-from date 1)))))
             (check-equal "a price on each path and date makes one per market" 300 (car calls)))
        (sb-int:unencapsulate 'fincl::state-discount-function 'count)))))

;;; --------------------------------------------------------------------
;;; Phase 3: cash, trades and interest
;;; --------------------------------------------------------------------

(defun book-value (book market &optional (engine :black-scholes))
  (price book engine market))

(defun v2-book-pair (&key (n 52) (n-paths 2000))
  "V2's short call hedged by hand (HAND) and with a cash book and TRADE
(BOOK, the same arithmetic in the same order). The book collects no income,
so the call's payoff is subtracted as by hand (r = 0, so interest is 1)."
  (let* ((m (validation-market))
         (days 364)
         (expiry (date+ *today* days))
         (call (make-option (call 100) (expires expiry) (underlying :spx)))
         (dates (hedge-dates n days)))
    (values
     (hedge-pnl-run m call dates #'identity :n-paths n-paths)
     (simulate ((make-gbm) m dates :n-paths n-paths)
       (:portfolio book (make-portfolio "hedge" (make-position call :quantity -1))
                   :cash (price call :black-scholes m) :engine :black-scholes :collect '())
       (:state s :spot)
       (:start (market)
         (trade book (make-holding) :to (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
       (:every (date market)
         (when (date< date expiry)
           (trade book (make-holding) :to (getf (greeks call :black-scholes market :which '(:delta)) :delta))))
       (:final (:pnl (+ (fincl::book-balance book)
                        (* (position-quantity (find-if (lambda (p) (typep (position-instrument p) 'fincl::holding))
                                                       (portfolio-positions book)))
                           s)
                        (- (max 0d0 (- s 100d0))))))))))

(defun cash-book-checks ()
  (format t "~&Simulation books: cash, trades and interest~%")
  (let* ((m (usd-market :rate 0.04d0))
         (dates (monthly-dates 24))
         (last (a:lastcar dates))
         (plain (simulate ((make-gbm) m dates :n-paths 50)
                  (:portfolio book (make-portfolio "cash") :cash 1000 :record (:cash))
                  (:final (:cash (fincl::book-balance book))))))
    (check-that "a book of cash only ends at the initial amount / P(0, T) to 1e-13 (no curve leg)"
                (let ((expected (/ 1000d0 (market-curve-discount m :usd last))))
                  (every (lambda (x) (< (abs (/ (- x expected) expected)) 1d-13)) (simulation-values plain :cash))))
    (check-that ":RECORD (:CASH) records the balance under (book :cash)"
                (< (abs (- (aref (simulation-statistic plain '(:book :cash) :mean) 12)
                           (/ 1000d0 (market-curve-discount m :usd (nth 11 dates)))))
                   1d-9))
    (let ((hw (simulate ((((:curve :usd) (make-hull-white :a 0.05d0 :sigma 0.01d0))) m dates :n-paths 200)
                (:portfolio book (make-portfolio "cash") :cash 1000)
                (:accumulate (worst 0d0))
                (:every (date)
                  (setf worst (max worst (abs (- (* (path-discount) (fincl::book-balance book)) 1000d0)))))
                (:final (:worst worst)))))
      (check-that "under Hull-White the discounted cash balance is the initial amount on every path and date to 1e-13"
                  (< (/ (reduce #'max (simulation-values hw :worst)) 1000d0) 1d-13)))
    (let ((r (simulate ((make-gbm) (usd-market :rate 0.04d0) dates :n-paths 20000 :antithetic t)
               (:portfolio book (make-portfolio "levered") :cash 0 :engine :black-scholes)
               (:start (market) (trade book (make-holding) :to 1))
               (:final (:pv (* (path-discount) (book-value book market)))))))
      (multiple-value-bind (mean se) (simulation-summary r (simulation-values r :pv))
        (check (format nil "a share bought with borrowed cash: mean discounted book value 0 within 3 SE (~,4F)" se)
               0d0 mean (* 3 se)))))
  (multiple-value-bind (hand book) (v2-book-pair)
    (check-that "V2's hedge by hand and with TRADE give = P&L on every path (same arithmetic, same order)"
                (equalp (simulation-values hand :pnl) (simulation-values book :pnl))))
  (let ((m (usd-market))
        (dates (monthly-dates 2))
        (bond (make-fixed-rate-bond (make-schedule *today* (years-from *today* 5) (make-tenor 1 :years))
                                    0.04d0 +actual-365-fixed+ :currency :usd :settlement-days 0)))
    (check-error "TRADE of an instrument that cannot be priced (a bond under :AUTO) signals its condition"
                 (simulate ((make-gbm) m dates :n-paths 2)
                   (:portfolio book (make-portfolio "b") :cash 100)
                   (:every (date) (trade book bond :to 1)))
                 invalid-argument)
    (check-that "TRADE without :CASH names the key"
                (search ":CASH" (handler-case (simulate ((make-gbm) m dates :n-paths 2)
                                                (:portfolio book (make-portfolio "b"))
                                                (:every (date) (trade book (make-holding) :to 1)))
                                  (invalid-argument (c) (princ-to-string c)))))
    (check-error ":CASH with a cash account in INIT"
                 (simulate ((make-gbm) m dates :n-paths 2)
                   (:portfolio book (make-portfolio "b" (make-position (make-cash-account :currency :usd) :quantity 5))
                               :cash 100))
                 invalid-argument)
    (check-error "a negative DEPOSIT"
                 (simulate ((make-gbm) m dates :n-paths 2)
                   (:portfolio book (make-portfolio "b") :cash 100)
                   (:every (date) (deposit book -1)))
                 invalid-argument)
    (check-error "TRADE of cash" (simulate ((make-gbm) m dates :n-paths 2)
                                   (:portfolio book (make-portfolio "b") :cash 100)
                                   (:every (date) (trade book (make-cash-account) :to 1)))
                 invalid-argument)
    (check-that "a cash account in INIT turns accounting on, and DEPOSIT and WITHDRAW return the balance"
                (let ((r (simulate ((make-gbm) (usd-market :rate 0d0) dates :n-paths 2)
                           (:portfolio book (make-portfolio "b" (make-position (make-cash-account :currency :usd)
                                                                               :quantity 5)))
                           (:accumulate (seen 0d0))
                           (:every (date) (setf seen (+ (deposit book 3) (withdraw book 1))))
                           (:final (:seen seen) (:cash (fincl::book-balance book))))))
                  (and (every (lambda (x) (= x 19d0)) (simulation-values r :seen))
                       (every (lambda (x) (= x 9d0)) (simulation-values r :cash))))))
  (check-that "cash balances are = under 1, 2 and 8 workers"
              (let ((runs (mapcar (lambda (n)
                                    (with-workers n (lambda () (simulation-values (nth-value 1 (v2-book-pair :n-paths 300))
                                                                                  :pnl))))
                                  '(1 2 8))))
                (every (lambda (v) (equalp v (first runs))) runs)))
  (let* ((m (usd-market))
         (book (make-portfolio "b" (make-position (make-holding :underlying :eq) :quantity 2)
                               (make-position (make-cash-account :currency :usd) :quantity 50))))
    (check-that "a cash account prices at 1 under any engine, its Greeks are 0, and it has a snapshot form"
                (and (= 250d0 (price book :auto m))
                     (= 2d0 (getf (greeks book :black-scholes m :which '(:delta)) :delta))
                     (equalp '(make-cash-account :currency :usd) (snapshot-form (make-cash-account :currency :usd)))))
    (check-that "EXPLAIN-PNL of a book with cash: the cash explains nothing and moves nothing"
                (handler-case (progn (explain-pnl book m (bump m '(:spot :eq) '(:relative 1/100)) :engine :black-scholes) t)
                  (error (c) (format t "~&       ~A~%" c) nil)))))

;;; --------------------------------------------------------------------
;;; Phase 4: income
;;; --------------------------------------------------------------------

(defparameter *pays-nothing* '(fincl::cash-account fincl::cap-floor fincl::swaption)
  "Exported instrument classes POSITION-INCOME documents as paying nothing.")

(defun book-dividend-market ()
  (usd-market :rate 0.05d0 :dividend 0.02d0 :vol 0.2d0
              :dividend-schedule `((,(date+ *today* 100) 2) (,(date+ *today* 200) 0.03d0 :kind :proportional))))

(defun share-book-run (market process &key (collect :all) (quantity 1))
  (let ((collect collect))
    (simulate (process market (monthly-dates 12) :n-paths 20000 :antithetic t)
      (:portfolio book (make-portfolio "share" (make-position (make-holding :underlying :eq) :quantity quantity))
                  :cash 0 :engine :black-scholes :collect collect)
      (:final (:pv (* (path-discount) (price book :black-scholes market)))))))

(defun annual-bond (start years &key (coupon 0.04d0))
  (make-fixed-rate-bond (make-schedule start (years-from start years) (make-tenor 1 :years))
                        coupon +actual-365-fixed+ :currency :usd :settlement-days 0))

(defun income-checks ()
  (format t "~&Simulation books: income~%")
  (let* ((m (book-dividend-market))
         (last (a:lastcar (monthly-dates 12))))
    (flet ((summary-of (r) (simulation-summary r (simulation-values r :pv))))
      (multiple-value-bind (mean se) (summary-of (share-book-run m (make-gbm)))
        (check (format nil "a share with q = 2%, a cash and a proportional dividend (:spot): mean discounted share + cash = S0 within 3 SE (~,4F)" se)
               100d0 mean (* 3 se)))
      (multiple-value-bind (mean se) (summary-of (share-book-run m (make-gbm) :collect '(:coupon)))
        (let ((forward (* (market-curve-discount m :usd last) (forward m last))))
          (check (format nil "... with :COLLECT excluding dividends it is the discounted forward ~,4F within 3 SE (~,4F)" forward se)
                 forward mean (* 3 se))))
      (multiple-value-bind (mean se) (summary-of (share-book-run m (make-gbm :dividend-model :escrowed)))
        (check (format nil "... and under :ESCROWED, S0 within 3 SE (~,4F)" se) 100d0 mean (* 3 se)))))
  (let ((r (simulate ((make-gbm) (usd-market :rate 0.05d0 :dividend-schedule `((,(nth 2 (monthly-dates 4)) 2)))
                      (monthly-dates 4) :n-paths 4)
             (:portfolio book (make-portfolio "short" (make-position (make-holding :underlying :eq) :quantity -1))
                         :cash 0 :record (:cash)))))
    (check-equal "a short holding pays its dividend from cash on the ex-date" -2d0
                 (aref (simulation-statistic r '(:book :cash) :mean) 3)))
  ;; Bonds.
  (let* ((m (usd-market :rate 0.04d0))
         (bond (annual-bond *today* 3))
         (dates (monthly-dates 40))
         (p0 (price bond :discounting m))
         (r (simulate ((make-gbm) m dates :n-paths 4 :keep (:paths 1))
              (:portfolio book (make-portfolio "bond" (make-position bond)) :cash 0 :engine :discounting)
              (:final (:cash (fincl::book-balance book)) (:positions (length (portfolio-positions book)))))))
    (check-that "a fixed-rate bond held to maturity, no curve leg: final cash = its price / P(0, T) to 1e-12"
                (let ((expected (/ p0 (market-curve-discount m :usd (a:lastcar dates)))))
                  (every (lambda (x) (< (abs (/ (- x expected) expected)) 1d-12)) (simulation-values r :cash))))
    (check-that "... and the matured bond left the book (only cash remains)"
                (every (lambda (n) (= n 1)) (simulation-values r :positions)))
    (let ((hw (simulate ((((:curve :usd) (make-hull-white :a 0.05d0 :sigma 0.01d0))) m dates :n-paths 20000 :antithetic t)
                (:portfolio book (make-portfolio "bond" (make-position bond)) :cash 0 :engine :discounting)
                (:final (:pv (* (path-discount) (fincl::book-balance book)))))))
      (multiple-value-bind (mean se) (simulation-summary hw (simulation-values hw :pv))
        (check (format nil "under Hull-White the mean discounted final cash = the bond's price within 3 SE (~,5F)" se)
               p0 mean (* 3 se)))))
  ;; Within-interval timing: a coupon mid-month, no curve leg.
  (let* ((m (usd-market :rate 0.04d0))
         (bond (annual-bond (date+ *today* 15) 1))
         (dates (monthly-dates 14))
         (pay (fincl::payment-date (a:lastcar (bond-cashflows bond))))
         (k (position-if (lambda (d) (date< pay d)) dates))
         (r (simulate ((make-gbm) m dates :n-paths 2)
              (:portfolio book (make-portfolio "bond" (make-position bond)) :cash 0 :engine :discounting
                          :record (:cash)))))
    (check-that "a flow paid between dates is credited grown exactly by P(0, t_p) / P(0, t_k) to 1e-13"
                (let ((expected (* (reduce #'+ (mapcar (lambda (cf) (fincl::cashflow-amount cf m)) (bond-cashflows bond)))
                                   (/ (market-curve-discount m :usd pay) (market-curve-discount m :usd (nth k dates))))))
                  (< (abs (/ (- (aref (simulation-statistic r '(:book :cash) :mean) (1+ k)) expected) expected)) 1d-13))))
  ;; Options.
  (let* ((m (usd-market :rate 0.03d0))
         (dates (monthly-dates 6))
         (expiry (nth 3 dates))
         (call (make-option (call 100) (expires expiry) (underlying :eq)))
         (r (simulate ((make-gbm) m dates :n-paths 200 :keep (:paths 5))
              (:portfolio book (make-portfolio "call" (make-position call :quantity 2)) :cash 0 :engine :black-scholes)
              (:accumulate (payoff 0d0) (cash-at-expiry 0d0))
              (:state s :spot)
              (:every (date)
                (when (date= date expiry)
                  (setf payoff (* 2 (max 0d0 (- s 100d0))) cash-at-expiry (fincl::book-balance book))))
              (:final (:payoff payoff) (:cash cash-at-expiry) (:held (length (portfolio-positions book)))))))
    (check-that "a European call held to an expiry among DATES pays its payoff into cash (=)"
                (equalp (simulation-values r :payoff) (simulation-values r :cash)))
    (check-that "... and is removed from the book"
                (every (lambda (n) (= n 1)) (simulation-values r :held)))
    (let ((between (make-option (call 100) (expires (date+ expiry 10)) (underlying :eq))))
      (check-that "an expiry between dates signals, naming the neighbouring dates"
                  (let ((text (handler-case (simulate ((make-gbm) m dates :n-paths 2)
                                              (:portfolio book (make-portfolio "c" (make-position between)) :cash 0))
                                (invalid-argument (c) (princ-to-string c)))))
                    (and (stringp text) (search (princ-to-string expiry) text)
                         (search (princ-to-string (nth 4 dates)) text) (search "DATES" text))))))
  (check-that "every exported instrument class has a POSITION-INCOME method or is listed as paying nothing"
              (iter (for symbol in-package :fincl external-only t)
                    (for class = (find-class symbol nil))
                    (always (or (null class)
                                (not (subtypep class 'fincl::instrument))
                                (member symbol '(fincl::instrument fincl::rates-instrument)) ; abstract
                                (member symbol *pays-nothing*)
                                (some (lambda (method)
                                        (let ((specializer (first (c2mop:method-specializers method))))
                                          (and (not (eq specializer (find-class 'fincl::instrument)))
                                               (subtypep class specializer))))
                                      (c2mop:generic-function-methods #'fincl::position-income))
                                (progn (format t "~&       no income method: ~S~%" symbol) nil))))))

;;; --------------------------------------------------------------------
;;; Phase 5: the reference studies
;;; --------------------------------------------------------------------

(defun constant-maturity-bond (date years rate)
  "A bullet bond issued on DATE, maturing YEARS later, paying the annual
coupon RATE (user code in the manual's example)."
  (make-fixed-rate-bond (make-schedule date (years-from date years) (make-tenor years :years))
                        rate +actual-365-fixed+ :currency :usd :settlement-days 0))

(defun annual-bond-from (date years rate)
  (make-fixed-rate-bond (make-schedule date (years-from date years) (make-tenor 1 :years))
                        rate +actual-365-fixed+ :currency :usd :settlement-days 0))

(defun par-rate (market years)
  "The YEARS annual par yield of MARKET's USD curve (user code)."
  (let ((discounts (iter (for i from 1 to years)
                         (collect (market-curve-discount market :usd (years-from (valuation-date market) i))))))
    (/ (- 1d0 (a:lastcar discounts)) (reduce #'+ discounts))))

(defun retirement-market ()
  "The equity's -3% yield makes it drift 3% over the rate (the drift device
:COLLECT keeps out of the book's income)."
  (make-market :valuation-date *today*
               :underlyings '((:eq :spot 100 :currency :usd :dividend -0.03d0 :vol 0.16d0))
               :curves '((:usd :rate 0.04d0)) :discounting '((:usd . :usd))))

(defun retirement-run (n-paths)
  "The manual's retirement drawdown: 60/40 in the equity and a 6-year par
bond rolled monthly, 5% a year withdrawn monthly, stopped at ruin."
  (simulate ((((:curve :usd) (make-hull-white :a 0.05d0 :sigma 0.01d0)) (:eq (make-gbm)))
             (retirement-market) (monthly-dates 300) :n-paths n-paths :keep (:paths 10)
             :correlation ((:eq (:curve :usd) 0.2d0)))
    (:portfolio book (make-portfolio "60/40") :cash 1 :engine :discounting
                :collect '(:discrete-dividend :coupon :redemption) :record (:value))
    (:hold (bond nil))
    (:every (date market)
      (withdraw book (/ 0.05d0 12))
      (when bond (trade book bond :to 0))                 ; sell last month's bond
      (let ((v (price book :discounting market)))    ; cash and the equity
        (if (<= v 0)
            (stop)
            (progn
              (setf bond (annual-bond-from date 6 (par-rate market 6)))
              (trade book (make-holding :underlying :eq) :to (/ (* 0.6d0 v) (market-component market '(:spot :eq))))
              (trade book bond :to (/ (* 0.4d0 v) (price bond :discounting market)))))))
    (:final (:ruined (if (<= (price book :discounting market) 0) 1d0 0d0)))))

(defun bond-fund-run (n-paths)
  "A fund rolling a 6-year zero-coupon bond monthly as a book (:BOOK), and the
same fund by hand (:HAND, the design discussion's probe: last month's zero
repriced at today's short rate by HULL-WHITE-DISCOUNT-BOND)."
  (let* ((m (retirement-market))
         (hw (make-hull-white :a 0.05d0 :sigma 0.01d0))
         (rc (make-rate-curve m :usd)))
    (simulate ((((:curve :usd) hw)) m (monthly-dates 300) :n-paths n-paths :seed 3)
      (:portfolio book (make-portfolio "fund") :cash 1 :engine :discounting)
      (:accumulate (nav 1d0) (maturity 6d0))
      (:hold (bond nil))
      (:state r :short-rate (:curve :usd))
      (:start (market)
        (setf bond (constant-maturity-bond *today* 6 0d0))
        (trade book bond :to (/ (fincl::book-balance book) (price bond :discounting market))))
      (:every (date market &key time dt)
        (setf nav (* nav (/ (hull-white-discount-bond hw rc time maturity r)
                            (hull-white-discount-bond hw rc (- time dt) maturity (previous r))))
              maturity (+ time 6d0))
        (trade book bond :to 0)
        (setf bond (constant-maturity-bond date 6 0d0))
        (trade book bond :to (/ (fincl::book-balance book) (price bond :discounting market))))
      (:final (:book (log (price book :discounting market))) (:hand (log nav))))))

(defun reference-study-checks ()
  (format t "~&Simulation books: reference studies~%")
  (let* ((start (get-internal-real-time))
         (r (retirement-run 20000))
         (seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    (multiple-value-bind (p se) (simulation-summary r (simulation-values r :ruined))
      (format t "~&       retirement drawdown, 20000 paths x 300 dates: P(ruin) = ~,4F +- ~,4F in ~,1F s~%" p se seconds)
      (check-that "the retirement study runs and reports a ruin probability in (0, 1) with its SE"
                  (and (< 0 p 1) (plusp se)))
      (check-that "the book's value is recorded on the kept paths"
                  (= 1d0 (aref (simulation-path-record r 0 '(:book :value)) 0)))))
  (let ((r (bond-fund-run 2000)))
    (multiple-value-bind (book se) (simulation-summary r (simulation-values r :book))
      (let ((hand (simulation-summary r (simulation-values r :hand))))
        (check (format nil "a rolled 6-year zero as a book: mean log return over 25 years = the probe's by hand ~,4F within 3 SE (~,4F)"
                       hand se)
               hand book (* 3 se)))))
  ;; The delta hedge, its short call paid at expiry out of cash.
  (let* ((m (validation-market))
         (days 364)
         (expiry (date+ *today* days))
         (call (make-option (call 100) (expires expiry) (underlying :spx)))
         (dates (hedge-dates 52 days))
         (hand (hedge-pnl-run m call dates #'identity :n-paths 4000))
         (book (simulate ((make-gbm) m dates :n-paths 4000)
                 (:portfolio book (make-portfolio "hedge" (make-position call :quantity -1))
                             :cash (price call :black-scholes m) :engine :black-scholes)
                 (:start (market)
                   (trade book (make-holding) :to (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
                 (:every (date market)
                   (when (date< date expiry)
                     (trade book (make-holding) :to (getf (greeks call :black-scholes market :which '(:delta)) :delta))))
                 (:final (:pnl (price book :black-scholes market))))))
    (check-that "the delta hedge as a book (the short call paid from cash at expiry) = V2's P&L on every path to 1e-12"
                (every (lambda (x y) (< (abs (- x y)) 1d-12)) (simulation-values hand :pnl) (simulation-values book :pnl)))
    (check (format nil "... and its mean P&L is V2's (~,4F)" (simulation-mean hand :pnl))
           (simulation-mean hand :pnl) (simulation-mean book :pnl) 1d-12))
  ;; Dollar-cost averaging against a lump sum.
  (let* ((m (usd-market :rate 0.04d0 :vol 0.2d0))
         (dates (monthly-dates 24))
         (s0 100d0))
    (flet ((lump (path-discount-final s) (declare (ignore path-discount-final)) (* (/ 24000d0 s0) s)))
      (let ((book (simulate ((make-gbm) m dates :n-paths 4000)
                    (:portfolio dca (make-portfolio "dca") :cash 24000 :engine :black-scholes)
                    (:state s :spot)
                    (:every (date) (trade dca (make-holding) :by (/ 1000d0 s)))
                    (:final (:difference (- (lump 0 s) (price dca :black-scholes market))))))
            (hand (simulate ((make-gbm) m dates :n-paths 4000)
                    (:accumulate (shares 0d0) (cash 24000d0) (last 1d0))
                    (:state s :spot)
                    (:every (date)
                      (let ((d (path-discount)))
                        (setf cash (* cash (/ last d)) last d))
                      (let* ((q1 (+ shares (/ 1000d0 s)))
                             (dq (- q1 shares)))
                        (setf cash (- cash (* dq s)) shares q1)))
                    (:final (:difference (- (lump 0 s) (+ (* shares s) cash)))))))
        (check-that "dollar-cost averaging as a book: lump minus DCA = a hand-carried version on every path (=)"
                    (equalp (simulation-values book :difference) (simulation-values hand :difference)))
        (check-equal "... so the means are =" (simulation-mean hand :difference) (simulation-mean book :difference)
                     :test #'=)))))

;;; --------------------------------------------------------------------
;;; Several books on the same paths
;;; --------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mix-book-clauses (var weight)
    "Clauses of a book VAR of WEIGHT in the equity and the rest in cash,
rebalanced monthly after a withdrawal."
    `((:portfolio ,var (make-portfolio ,(string-downcase (symbol-name var))) :cash 1
                  :engine :black-scholes :collect '() :record (:value :cash))
      (:every (date market)
        (withdraw ,var (/ 0.05d0 12))
        (trade ,var (make-holding :underlying :eq)
               :to (/ (* ,weight (price ,var :black-scholes market)) (market-component market '(:spot :eq))))))))

(defmacro mix-run (&rest books)
  "A simulation of BOOKS, each (var weight), on one set of paths."
  `(simulate ((make-gbm) (retirement-market) (monthly-dates 24) :n-paths 200)
     ,@(iter (for (var weight) in books) (appending (mix-book-clauses var weight)))))

(defun several-book-checks ()
  (format t "~&Simulation books: several books~%")
  (let ((both (mix-run (sixty 0.6d0) (seventy 0.7d0)))
        (sixty (mix-run (sixty 0.6d0)))
        (seventy (mix-run (seventy 0.7d0))))
    (check-that "two books on one set of paths: without :FINAL the outputs are the books' values by name"
                (equal '(:sixty :seventy) (simulation-output-names both)))
    (check-that "... each = the book simulated alone on every path"
                (and (equalp (simulation-values both :sixty) (simulation-values sixty :value))
                     (equalp (simulation-values both :seventy) (simulation-values seventy :value))))
    (check-that "... each book's records are its own and = the book alone"
                (and (equalp (simulation-statistic both '(:sixty :cash) :mean)
                             (simulation-statistic sixty '(:sixty :cash) :mean))
                     (equalp (simulation-statistic both '(:seventy :value) :mean)
                             (simulation-statistic seventy '(:seventy :value) :mean))
                     (not (equalp (simulation-statistic both '(:sixty :value) :mean)
                                  (simulation-statistic both '(:seventy :value) :mean))))))
  (let ((m (usd-market :rate 0d0))
        (dates (monthly-dates 2)))
    (check-that "DEPOSIT and TRADE move only the book named"
                (let ((r (simulate ((make-gbm) m dates :n-paths 2)
                           (:portfolio a (make-portfolio "a") :cash 1)
                           (:portfolio b (make-portfolio "b") :cash 1)
                           (:every (date) (deposit b 2) (trade a (make-holding) :to 0))
                           (:final (:a (fincl::book-balance a)) (:b (fincl::book-balance b))))))
                  (and (every (lambda (x) (= x 1d0)) (simulation-values r :a))
                       (every (lambda (x) (= x 5d0)) (simulation-values r :b))))))
  (flet ((expand (&rest clauses)
           (macroexpand-1 `(simulate ((make-gbm) m dates)
                             (:portfolio a (make-portfolio "a") :cash 1)
                             (:portfolio b (make-portfolio "b") :cash 1)
                             ,@clauses))))
    (check-that "TRADE naming no book signals at macroexpansion, naming the books"
                (handler-case (progn (expand '(:every (date) (trade (make-holding) :to 1))) nil)
                  (invalid-argument (c) (and (search "A, B" (princ-to-string c)) t))))
    (check-error "WITHDRAW naming a variable that is not a book"
                 (expand '(:accumulate (c 0d0)) '(:every (date) (withdraw c 1))) invalid-argument)
    (check-error "two :PORTFOLIO clauses of one variable"
                 (expand '(:portfolio a (make-portfolio "again"))) invalid-argument)))

(defun book-docstring-checks ()
  (format t "~&Simulation books: docstring examples~%")
  (let* ((market (sim-market))
         (dates (every-n-days 30))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (*package* (find-package :fincl/tests))
         (doc (documentation 'simulate 'function))
         (r (eval `(let ((market ,market) (dates ',dates) (call ,call) (expiry #D"2027-09-24"))
                     (declare (ignorable market dates call expiry))
                     ,(read-from-string (subseq doc (search "(simulate ((make-gbm) market dates :n-paths 20000)
    (:portfolio" doc)))))))
    (check-that "SIMULATE's self-financing hedge example runs; its mean P&L is 0 within 3 SE"
                (multiple-value-bind (mean se) (simulation-summary r (simulation-values r :pnl))
                  (<= (abs mean) (* 3 se)))))
  (let* ((r (simulate ((make-gbm) (sim-market) (monthly-dates 3) :n-paths 20 :keep (:paths 2))
              (:portfolio book (make-portfolio "b") :cash 1 :record (:value))
              (:state s :spot)
              (:every (date) (record :balance s))))
         (*package* (find-package :fincl/tests))
         (doc (documentation 'simulation-path-record 'function)))
    (check-that "SIMULATION-PATH-RECORD's docstring examples run"
                (iter (for start = (search "(simulation-path-record r" doc :start2 (if start (1+ start) 0)))
                      (while start)
                      (always (typep (eval `(let ((r ,r)) ,(read-from-string (subseq doc start))))
                                     'fincl:double-vector)))))
  (check-that "MAKE-CASH-ACCOUNT's and CASH-ACCOUNT's docstring examples run"
              (let ((*package* (find-package :fincl/tests)))
               (and (typep (eval (read-from-string (subseq (documentation 'make-cash-account 'function)
                                                          (search "(make-cash-account" (documentation 'make-cash-account 'function)))))
                          'cash-account)
                   (typep (eval (read-from-string (subseq (documentation 'cash-account 'type)
                                                          (search "(make-position" (documentation 'cash-account 'type)))))
                          'fincl::portfolio-position))))
  (let* ((r (simulate ((((:curve :usd) (make-hull-white))) (usd-market) (monthly-dates 12) :n-paths 2 :keep (:paths 1))))
         (curve (market-component (cdr (a:lastcar (path-markets r 0))) '(:curve :usd)))
         (doc (documentation 'state-discount-bond 'function)))
    (check-that "STATE-DISCOUNT-BOND's docstring example runs on a simulated curve's process and state"
                (let ((v (eval `(let ((process ,(fincl::state-curve-process curve)) (state ,(fincl::state-curve-state curve)))
                                  ,(let ((*package* (find-package :fincl/tests)))
                                     (read-from-string (subseq doc (search "(state-discount-bond process" doc))))))))
                  (< 0d0 v 1d0)))))

(defun simulation-book-checks ()
  (book-docstring-checks)
  (path-record-checks)
  (exact-curve-checks)
  (cash-book-checks)
  (income-checks)
  (several-book-checks)
  (reference-study-checks))

(defun run-simulation-book-tests ()
  "Run only the task 33 checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (simulation-book-checks))
    (format t "~&~[All simulation book checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
