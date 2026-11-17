;;;; simulation.lisp --- market path simulation (task 29)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun sim-market (&key (rate 0.05d0) (dividend-schedule nil))
  (make-bs-market :valuation-date *today* :spot 100 :rate rate :vol 0.2d0
                  :dividend-schedule dividend-schedule))

(defvar sim-special-market nil)

(defun sim-quarterly-dates ()
  (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24"))

(defun every-n-days (n &optional (end #D"2027-09-24"))
  "Dates N days apart from *TODAY*, ending on END."
  (iter (for d initially (date+ *today* n) then (date+ d n))
        (while (date< d end))
        (collect d into dates)
        (finally (return `(,@dates ,end)))))

(defun sample-sd (values)
  (let* ((n (length values)) (mean (/ (reduce #'+ values) n)))
    (sqrt (/ (reduce #'+ (map 'list (lambda (x) (expt (- x mean) 2)) values)) (1- n)))))

;;; --------------------------------------------------------------------
;;; Phase 1: the core
;;; --------------------------------------------------------------------

(defun run-asian-pair (market n-paths &key (antithetic t) (n-steps 50))
  "Arithmetic and geometric Asian call payoffs at 100 as two outputs, written
as the Asian path kernel computes them."
  (simulate ((make-gbm) market (sim-quarterly-dates) :n-paths n-paths :antithetic antithetic :n-steps n-steps)
    (:accumulate (sum 0d0) (sum-log 0d0))
    (:state s :spot)
    (:every (date) (incf sum s) (incf sum-log (log s)))
    (:final (:arithmetic (max 0d0 (* 1d0 (- (/ sum 4d0) 100d0))))
            (:geometric (max 0d0 (* 1d0 (- (exp (/ sum-log 4d0)) 100d0)))))))

(defun simulation-core-checks ()
  (format t "~&Simulation: the core~%")
  (let* ((m (sim-market))
         (r (run-asian-pair m 20000))
         (df (discount-factor m #D"2027-09-24"))
         (engine (make-engine :mc :n-paths 20000 :antithetic t :n-steps 50)))
    (flet ((asian (kind) (make-option (call 100) (average kind :dates (sim-quarterly-dates))
                                      (expires #D"2027-09-24"))))
      (check-that "an arithmetic Asian output's mean = the engine's price (=)"
                  (= (* df (simulation-mean r :arithmetic)) (price (asian :arithmetic) engine m)))
      (check-that "a geometric Asian output of the same run = its engine price (=)"
                  (= (* df (simulation-mean r :geometric)) (price (asian :geometric) engine m))))
    (let ((single (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 20000 :antithetic t :n-steps 50)
                    (:accumulate (sum 0d0))
                    (:state s :spot)
                    (:every (date) (incf sum s))
                    (:final (:arithmetic (max 0d0 (* 1d0 (- (/ sum 4d0) 100d0))))))))
      (check-that "a single-output run gives = per-path values to the same output of a two-output run"
                  (equalp (simulation-values single) (simulation-values r :arithmetic))))
    (let* ((dates (sim-quarterly-dates))
           (on-dates (list (second dates) (fourth dates)))
           (before (copy-list on-dates))
           (on (simulate ((make-gbm) m dates :n-paths 500 :seed 3)
                 (:state s :spot)
                 (:on on-dates (i) (record :i i))))
           (every (simulate ((make-gbm) m dates :n-paths 500 :seed 3)
                    (:state s :spot)
                    (:every (date)
                      (let ((i (position date on-dates :test #'date=)))
                        (when i (record :i i)))))))
      (check-that ":ON leaves the caller's date list alone (SORT must not relink it)"
                  (equal on-dates before))
      (check-that ":ON runs on its dates only, I being the position in its list"
                  (flet ((means (r) (substitute-if 0d0 #'fincl::nan-p (simulation-statistic r :i :mean))))
                    (equalp (means on) (means every)))))
    (check-that "outputs are = under 1, 2 and 8 workers"
                (let ((runs (mapcar (lambda (n) (with-workers n (lambda () (simulation-values (run-asian-pair m 2000) :geometric))))
                                    '(1 2 8))))
                  (every (lambda (v) (equalp v (first runs))) runs)))
    (check-that "path i's outputs do not change when n-paths doubles"
                (let ((small (simulation-values (run-asian-pair m 1000 :antithetic nil) :arithmetic))
                      (large (simulation-values (run-asian-pair m 2000 :antithetic nil) :arithmetic)))
                  (equalp small (subseq large 0 1000))))
    (let* ((paired (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 20000 :antithetic t)
                     (:state s :spot)
                     (:final (:k100 (max 0d0 (- s 100d0))) (:k101 (max 0d0 (- s 101d0))))))
           (a (simulation-values paired :k100)) (b (simulation-values paired :k101))
           (difference (map 'double-vector #'- a b)))
      (multiple-value-bind (mean se) (simulation-summary paired difference)
        (multiple-value-bind (m2 se2) (fincl::summarize-samples
                                       (fincl::simulation-plan-engine (fincl::simulation-result-plan paired))
                                       (fincl::pair-means difference))
          (check-that "simulation-summary of a difference = summarize-samples of the antithetic pair means (=)"
                      (and (= mean m2) (= se se2))))
        (check-that (format nil "paired SE of a difference of correlated outputs (~,4F) is below the SEs in quadrature (~,4F)"
                            se (sqrt (+ (expt (simulation-standard-error paired :k100) 2)
                                        (expt (simulation-standard-error paired :k101) 2))))
                    (< se (sqrt (+ (expt (simulation-standard-error paired :k100) 2)
                                   (expt (simulation-standard-error paired :k101) 2))))))))
  ;; Heston moments and forward.
  (let* ((m (sim-market))
         (dates (sim-quarterly-dates))
         (r (simulate ((make-heston :v0 0.04d0 :theta 0.06d0 :kappa 2d0 :xi 0.3d0 :rho -0.5d0) m dates
                       :n-paths 40000)
              (:state v :variance)
              (:state s :spot)
              (:every (date) (record :v v) (record :s s)))))
    (flet ((within-3-se (name expected)
             (let ((mean (simulation-statistic r name :mean))
                   (sd (simulation-statistic r name :std-dev))
                   (count (simulation-statistic r name :count)))
               (iter (for e in expected) (for k from 1)
                     (always (<= (abs (- (aref mean k) e)) (* 3 (/ (aref sd k) (sqrt (aref count k))))))))))
      (check-that "Heston: the recorded mean variance matches theta + (v0 - theta) e^(-kappa t) within 3 SE"
                  (within-3-se :v (mapcar (lambda (d) (+ 0.06d0 (* -0.02d0 (exp (* -2 (market-time m d)))))) dates)))
      (check-that "Heston: the recorded mean spot matches the forward within 3 SE"
                  (within-3-se :s (mapcar (lambda (d) (forward m d)) dates)))))
  ;; Cash dividends.
  (let* ((m (sim-market :dividend-schedule '(("2027-01-15" 3d0))))
         (dates (list #D"2027-01-14" #D"2027-01-16" #D"2027-09-24"))
         (r (simulate ((make-gbm) m dates :n-paths 40000)
              (:state s :spot)
              (:every (date) (record :s s)))))
    (check-that "a cash dividend: the mean spot matches the forward before and after the ex-date within 3 SE"
                (iter (for d in dates) (for k from 1)
                      (always (<= (abs (- (aref (simulation-statistic r :s :mean) k) (forward m d)))
                                  (* 3 (/ (aref (simulation-statistic r :s :std-dev) k)
                                          (sqrt (aref (simulation-statistic r :s :count) k)))))))))
  ;; Errors, :hold, stop.
  (let ((m (sim-market)) (started 0))
    (check-that "a wrong :state name signals before any path runs and lists the valid names"
                (handler-case (progn (simulate ((make-heston) m (sim-quarterly-dates) :n-paths 10)
                                       (:state q :varience)
                                       (:start (incf started))
                                       (:final q))
                                     nil)
                  (invalid-argument (c) (and (zerop started) (search ":VARIANCE" (princ-to-string c))))))
    (check-error "a CGMY process is refused" (simulate ((make-cgmy) m (sim-quarterly-dates)) (:final 1)) invalid-argument)
    (check-error "a date on or before the valuation date is refused"
                 (simulate ((make-gbm) m (list *today*)) (:final 1)) invalid-argument)
    (check-that ":hold is reset on every path and never shared, under 8 workers"
                (with-workers 8 (lambda () (let ((r (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 2000 :antithetic t)
                             (:hold (seen '()))
                             (:every (date) (push date seen))
                             (:final (:count (length seen))))))
                    (every (lambda (n) (= n 4)) (simulation-values r :count))))))
    (check-that "after (stop) every output is still computed"
                (let ((r (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 100)
                           (:accumulate (dates 0d0))
                           (:every (date) (incf dates) (when (= dates 2) (stop)))
                           (:final (:dates dates) (:one 1)))))
                  (and (every (lambda (x) (= x 2)) (simulation-values r :dates))
                       (every (lambda (x) (= x 1)) (simulation-values r :one)))))
    (check-error "a repeated output name signals at macroexpansion"
                 (macroexpand '(simulate ((make-gbm) m d) (:final (:a 1) (:a 2)))) invalid-argument)
    (check-error "a non-keyword output name signals at macroexpansion"
                 (macroexpand '(simulate ((make-gbm) m d) (:final (a 1) (:b 2)))) invalid-argument)
    (check-that "a non-real output signals, naming the output and the path"
                (handler-case (progn (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 4) (:final (:bad "x") (:ok 1))) nil)
                  (invalid-argument (c) (let ((report (princ-to-string c)))
                                          (and (search ":BAD" report) (search "path 0" report))))))
    (check-that "an unknown output name lists the output names"
                (handler-case (progn (simulation-values (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 4)
                                                          (:final (:a 1) (:b 2)))
                                                        :c)
                                     nil)
                  (invalid-argument (c) (search ":A, :B" (princ-to-string c)))))))

;;; --------------------------------------------------------------------
;;; Phase 2: markets and collection
;;; --------------------------------------------------------------------

(defun delta-hedge-run (market call dates &key (n-paths 4000) keep)
  (eval `(simulate ((make-gbm) ,market ',dates :n-paths ,n-paths :keep ,keep)
           (:accumulate (shares 0d0) (cash 0d0) (premium 0d0))
           (:state s :spot)
           (:start (market) (setf premium (price ,call :black-scholes market)) (incf cash premium))
           (:every (date market)
             (let ((delta (if (date< date (fincl::expiry (option-exercise ,call)))
                              (getf (greeks ,call :black-scholes market :which '(:delta)) :delta)
                              shares)))
               (decf cash (* (- delta shares) s))
               (setf shares delta)))
           (:final (:hedged (+ cash (* shares s) (- (payoff-value (option-payoff ,call) s))))
                   (:unhedged (- premium (payoff-value (option-payoff ,call) s)))))))

(defun simulation-market-checks ()
  (format t "~&Simulation: markets and collection~%")
  (let* ((m (make-market :valuation-date *today*
                         :underlyings '((:spx :spot 100 :currency :usd :vol 0.2d0 :dividend 0.01d0
                                         :dividend-schedule (("2027-06-15" 1d0))))
                         :curves '((:usd :rate 0.03d0))))
         (call (make-option (call 100) (expires #D"2027-09-24") (underlying :spx)))
         (checks '()))
    (simulate ((make-gbm) m (list #D"2027-03-24") :n-paths 3)
      (:state s :spot)
      (:every (date market)
        (push (list (valuation-date market) (spot (market-view market :underlying :spx)) s
                    (eq (gethash :usd (fincl::market-yield-curves market)) (gethash :usd (fincl::market-yield-curves m)))
                    (eq (gethash :spx (fincl::market-vol-structures market)) (gethash :spx (fincl::market-vol-structures m)))
                    (eq (gethash :spx (fincl::market-dividend-schedules market)) (gethash :spx (fincl::market-dividend-schedules m)))
                    (price call :black-scholes market)
                    (price call :black-scholes
                           (make-market :valuation-date #D"2027-03-24"
                                        :underlyings `((:spx :spot ,s :currency :usd :vol 0.2d0 :dividend 0.01d0
                                                        :dividend-schedule (("2027-06-15" 1d0))))
                                        :curves '((:usd :rate 0.03d0)))))
              checks)))
    (check-that "a market at a state has the date, the simulated spot, and eq curves, vols and dividends"
                (every (lambda (c) (destructuring-bind (date spot s curve vol dividends) (subseq c 0 6)
                                     (and (date= date #D"2027-03-24") (= spot s) curve vol dividends)))
                       checks))
    (check-that "a European on it = Black-Scholes at that spot and remaining time to 1e-12"
                (every (lambda (c) (< (abs (- (seventh c) (eighth c))) 1d-12)) checks)))
  (let ((built 0))
    (simulate ((make-gbm) (sim-market) (sim-quarterly-dates) :n-paths 50
               :market-from-state (lambda (&rest args) (incf built) (apply #'market-at-state args)))
      (:state s :spot)
      (:every (date) (record :s s)))
    (check-equal "a body that never mentions a market builds none" 0 built))
  (let* ((m (sim-market :rate 0d0))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (coarse (delta-hedge-run m call (every-n-days 14)))
         (fine (delta-hedge-run m call (every-n-days 7)))
         (ratio (/ (sample-sd (simulation-values fine :hedged)) (sample-sd (simulation-values coarse :hedged)))))
    (multiple-value-bind (mean se) (simulation-summary fine (simulation-values fine :hedged))
      (check-that (format nil "delta hedge at the market's vol: mean P&L ~,4F is 0 within 3 SE (~,4F)" mean se)
                  (<= (abs mean) (* 3 se))))
    (check-that (format nil "halving the rebalancing interval scales the hedge error's sd by ~,3F, in [0.6, 0.8]" ratio)
                (<= 0.6 ratio 0.8)))
  ;; Stored paths and their markets.
  (let* ((m (sim-market))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (r (eval `(simulate ((make-gbm) ,m ',(sim-quarterly-dates) :n-paths 500 :keep (:paths 5))
                     (:hold (prices '()))
                     (:every (date market) (push (price ,call :black-scholes market) prices))
                     (:final (:n (length prices)))))))
    (check-equal "(:paths 5) stores exactly 5 paths"
                 5 (count-if #'identity (fincl::simulation-store-kept-states (fincl::simulation-result-store r))))
    (check-that "path-markets reproduces the prices computed during the run (=)"
                (iter (for path below 5)
                      (always (equal (reverse (aref (simulation-held r path :prices) 4))
                                     (mapcar (lambda (dm) (price call :black-scholes (cdr dm)))
                                             (rest (path-markets r path)))))))
    (check-error "an unstored path is refused" (path-markets r 7) invalid-argument))
  ;; :keep evaluates k, so a variable works.
  (let* ((m (sim-market))
         (n-show 3)
         (r (simulate ((make-gbm) m (list #D"2027-09-24") :n-paths 50 :keep (:quantiles (:paths n-show)))
              (:final (:x 1d0)))))
    (check-equal "(:paths n-show) with a variable stores n-show paths"
                 3 (count-if #'identity (fincl::simulation-store-kept-states (fincl::simulation-result-store r)))))
  ;; A clause may name its market with a global (special) variable.
  (let* ((m (sim-market))
         (r (simulate ((make-gbm) m (list #D"2027-09-24") :n-paths 20)
              (:accumulate (x 0d0))
              (:every (date sim-special-market) (setf x (spot sim-special-market)))
              (:final (:x x)))))
    (check-that "a special variable names the date's market in a clause"
                (every (lambda (x) (> x 0d0)) (simulation-values r :x))))
  ;; Accessors for plotting.
  (let* ((m (sim-market))
         (r (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 200 :keep (:quantiles (:paths 4)))
              (:state s :spot)
              (:every (date) (record :s s))
              (:final (:s s)))))
    (check-equal "simulation-recorded-names" '(:s) (simulation-recorded-names r))
    (check-equal "simulation-kept-paths" 4 (simulation-kept-paths r))
    (check-that "simulation-quantiles-p: recorded name yes, other no"
                (and (simulation-quantiles-p r :s) (not (simulation-quantiles-p r :other))))
    (let ((path (simulation-path-state r 2 :spot)))
      (check-equal "simulation-path-state has a value per simulation date"
                   (length (simulation-dates r)) (length path))
      (check-that "its first value is the spot and the last is the output"
                  (and (= (aref path 0) 100d0)
                       (= (aref path (1- (length path))) (aref (simulation-values r :s) 2)))))
    (check-error "simulation-path-state of an unstored path" (simulation-path-state r 9 :spot) invalid-argument)
    (check-equal "no kept paths: 0"
                 0 (simulation-kept-paths (simulate ((make-gbm) m (sim-quarterly-dates) :n-paths 10)
                                            (:final (:x 1d0))))))
  ;; Quantiles.
  (let* ((m (sim-market))
         (r (simulate ((make-gbm) m (list #D"2027-09-24") :n-paths 40000 :keep :quantiles)
              (:state s :spot)
              (:every (date) (record :s s))
              (:final (:s s))))
         (sigma 0.2d0) (tau (market-time m #D"2027-09-24")))
    (flet ((exact (p) (* (forward m #D"2027-09-24")
                         (exp (+ (* -0.5d0 sigma sigma tau) (* sigma (sqrt tau) (norm-quantile p))))))
           (tolerance (p q)
             ;; 3 SE of a sample quantile: sqrt(p (1 - p) / n) / density.
             (* 3 (/ (sqrt (/ (* p (- 1 p)) 40000))
                     (/ (norm-pdf (norm-quantile p)) (* q sigma (sqrt tau)))))))
      (check-that ":keep :quantiles: recorded quantiles at expiry match the GBM quantiles within 3 SE"
                  (every (lambda (p) (let ((q (exact p)))
                                       (<= (abs (- (aref (simulation-statistic r :s :quantile p) 1) q))
                                           (tolerance p q))))
                         '(0.05d0 0.5d0 0.95d0)))
      (check-that "simulation-quantile of the terminal-spot output matches them too"
                  (every (lambda (p) (let ((q (exact p)))
                                       (<= (abs (- (simulation-quantile r :s p) q)) (tolerance p q))))
                         '(0.05d0 0.5d0 0.95d0))))))

;;; --------------------------------------------------------------------
;;; Phase 3: several underlyings
;;; --------------------------------------------------------------------

(defun three-underlying-market ()
  (make-market :valuation-date *today*
               :underlyings '((:aapl :spot 100 :currency :usd :vol 0.3d0 :dividend 0.01d0)
                              (:msft :spot 50 :currency :usd :vol 0.2d0)
                              (:ibm :spot 10 :currency :usd :vol 0.25d0))
               :curves '((:usd :rate 0.03d0))))

(defun simulation-multi-checks ()
  (format t "~&Simulation: several underlyings~%")
  (let* ((m (three-underlying-market))
         (n 40000)
         (pairwise (simulate (((:aapl (make-gbm)) (:msft (make-gbm))) m (list #D"2026-09-25") :n-paths n
                              :correlation '((:aapl :msft 0.6d0)))
                     (:state a :spot :aapl) (:state b :spot :msft)
                     (:final (:ra (log (/ a 100d0))) (:rb (log (/ b 50d0))))))
         (matrix (simulate (((:aapl (make-gbm)) (:msft (make-gbm))) m (list #D"2026-09-25") :n-paths n
                            :correlation #2A((1 0.6d0) (0.6d0 1)))
                   (:state a :spot :aapl) (:state b :spot :msft)
                   (:final (:ra (log (/ a 100d0))) (:rb (log (/ b 50d0))))))
         (x (simulation-values pairwise :ra)) (y (simulation-values pairwise :rb))
         (mx (/ (reduce #'+ x) n)) (my (/ (reduce #'+ y) n))
         (rho (/ (reduce #'+ (map 'list (lambda (u v) (* (- u mx) (- v my))) x y))
                 (sqrt (* (reduce #'+ (map 'list (lambda (u) (expt (- u mx) 2)) x))
                          (reduce #'+ (map 'list (lambda (v) (expt (- v my) 2)) y)))))))
    (check-that (format nil "one-step log-return correlation ~,4F matches 0.6 within 3 SE" rho)
                (<= (abs (- rho 0.6d0)) (* 3 (/ (- 1 (* 0.36d0)) (sqrt n)))))
    (check-that "the full-matrix form gives = paths to the pairwise form"
                (and (equalp x (simulation-values matrix :ra)) (equalp y (simulation-values matrix :rb)))))
  (let* ((m (three-underlying-market))
         (heston (make-heston :v0 0.09d0 :theta 0.09d0 :rho -0.6d0))
         (n 40000)
         (r (simulate (((:aapl heston) (:msft (make-gbm))) m (list #D"2026-09-25") :n-paths n
                       :correlation '((:aapl :msft 0.6)))
              (:state a :spot :aapl) (:state b :spot :msft)
              (:final (:ra (log (/ a 100d0))) (:rb (log (/ b 50d0))))))
         (x (simulation-values r :ra)) (y (simulation-values r :rb))
         (mx (/ (reduce #'+ x) n)) (my (/ (reduce #'+ y) n))
         (rho (/ (reduce #'+ (map 'list (lambda (u v) (* (- u mx) (- v my))) x y))
                 (sqrt (* (reduce #'+ (map 'list (lambda (u) (expt (- u mx) 2)) x))
                          (reduce #'+ (map 'list (lambda (v) (expt (- v my) 2)) y)))))))
    (check-that (format nil "Heston (QE) with GBM: the spot correlation ~,4F matches 0.6 within 3 SE (spot loadings)" rho)
                (<= (abs (- rho 0.6d0)) (* 3 (/ (- 1 0.36d0) (sqrt n))))))
  (let* ((m (three-underlying-market))
         (dates (sim-quarterly-dates))
         (r (simulate (((:aapl (make-heston)) (:msft (make-gbm))) m dates :n-paths 40000
                       :correlation '((:aapl :msft 0.8)))
              (:state a :spot :aapl) (:state b :spot :msft)
              (:every (date) (record :a a) (record :b b)))))
    (check-that "each marginal is unchanged by the correlation: mean spots match their forwards within 3 SE"
                (iter (for name in '(:a :b)) (for u in '(:aapl :msft))
                      (always (iter (for d in dates) (for k from 1)
                                    (always (<= (abs (- (aref (simulation-statistic r name :mean) k)
                                                        (forward (market-view m :underlying u) d)))
                                                (* 3 (/ (aref (simulation-statistic r name :std-dev) k)
                                                        (sqrt (aref (simulation-statistic r name :count) k)))))))))))
  (let ((m (three-underlying-market)))
    (check-error "a non-positive-definite correlation matrix is refused"
                 (simulate (((:aapl (make-gbm)) (:msft (make-gbm))) m (list #D"2026-09-25")
                            :correlation #2A((1 2) (2 1)))
                   (:final 1))
                 invalid-argument)
    (check-that "an underlying the market lacks signals missing-market-data listing the market's"
                (handler-case (progn (simulate (((:aapl (make-gbm)) (:goog (make-gbm))) m (list #D"2026-09-25"))
                                       (:final 1))
                                     nil)
                  (missing-market-data (c) (search ":IBM" (princ-to-string c)))))
    (let ((spots '()))
      (count-calls (copies 'fincl::copy-composite-market)
        (simulate (((:aapl (make-gbm)) (:msft (make-gbm))) m (list #D"2026-12-24") :n-paths 1)
          (:state a :spot :aapl) (:state b :spot :msft)
          (:every (date market)
            (setf (car copies) 0)
            (push (list (spot (market-view market :underlying :aapl)) a
                        (spot (market-view market :underlying :msft)) b
                        (spot (market-view market :underlying :ibm)) (car copies))
                  spots))))
      (check-that "market-at-state sets both spots in one table copy and keeps an unsimulated spot"
                  (every (lambda (s) (destructuring-bind (a a* b b* ibm n) s
                                       (and (= a a*) (= b b*) (= ibm 10d0) (= n 1))))
                         spots)))
    (check-equal "simulation-state-names on pairs" '((:spot :aapl) (:variance :aapl) (:spot :msft))
                 (simulation-state-names `((:aapl ,(make-heston)) (:msft ,(make-gbm)))))))

;;; --------------------------------------------------------------------
;;; Phase 4: the docstring examples
;;; --------------------------------------------------------------------

(defun simulation-docstring-checks ()
  (format t "~&Simulation: docstring examples~%")
  (let* ((market (sim-market))
         (dates (every-n-days 30))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (*package* (find-package :fincl/tests))
         (r (eval `(let ((market ,market) (dates ',dates) (call ,call) (expiry #D"2027-09-24"))
                     (declare (ignorable market dates call expiry))
                     ,(let ((doc (documentation 'simulate 'function)))
                        (read-from-string (subseq doc (search "(simulate ((make-gbm)" doc))))))))
    (check-that "SIMULATE's docstring example runs and returns :hedged and :unhedged outputs"
                (and (typep r 'simulation-result)
                     (equal '(:hedged :unhedged) (simulation-output-names r))))
    (check-that "... and records the :balance by date"
                (and (equal '(:balance) (simulation-recorded-names r))
                     (= (length (simulation-dates r)) (length (simulation-statistic r :balance :mean)))))
    (check-that "... and the hedge reduces the P&L's spread"
                (< (sample-sd (simulation-values r :hedged)) (sample-sd (simulation-values r :unhedged)))))
  (check-that "MAP-SIMULATED-PATHS's docstring example runs: 1000 terminal spots"
              (let* ((doc (documentation 'map-simulated-paths 'function))
                     (form (let ((*package* (find-package :fincl/tests))
                                 (*readtable* (named-readtables:find-readtable 'fincl:syntax)))
                             (read-from-string (subseq doc (search "(map-simulated-paths (lambda" doc)))))
                     (v (eval `(let ((m ,(sim-market))) (declare (ignorable m)) ,form))))
                (and (= 1000 (length v)) (every #'plusp v)))))

;;; --------------------------------------------------------------------
;;; Phase 5: portfolios
;;; --------------------------------------------------------------------

(defun simulation-portfolio-checks ()
  (format t "~&Simulation: portfolios~%")
  (let* ((m (sim-market :rate 0d0))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (dates (every-n-days 30))
         (hedge (make-holding))
         (accumulated
           (simulate ((make-gbm) m dates :n-paths 300)
             (:accumulate (shares 0d0))
             (:state s :spot)
             (:every (date market)
               (when (date< date #D"2027-09-24")
                 (setf shares (getf (greeks call :black-scholes market :which '(:delta)) :delta))))
             (:final (:value (+ (* -1d0 (payoff-value (option-payoff call) s)) (* shares s))))))
         (book-run
           (lambda ()
             (simulate ((make-gbm) m dates :n-paths 300 :keep (:quantiles (:paths 3)))
               (:portfolio book (make-portfolio "hedge" (make-position call :quantity -1))
                           :record (:value :delta) :engine :black-scholes)
               (:every (date market)
                 (record :serial (if fincl::*serial-pricing* 1 0))
                 (when (date< date #D"2027-09-24")
                   (setf book (adjust-position book hedge
                                               :to (getf (greeks call :black-scholes market :which '(:delta))
                                                         :delta))))))))
         (one (with-workers 1 (lambda () (funcall book-run))))
         (eight (with-workers 8 (lambda () (funcall book-run)))))
    (check-that "the delta hedge as a :portfolio gives = final values to the :accumulate version"
                (equalp (simulation-values accumulated) (simulation-values one :value)))
    (check-that "with no :final the one output is :value, the book's value on the last date's market (=)"
                (iter (for path below 3)
                      (always (= (aref (simulation-values one :value) path)
                                 (price (aref (simulation-held one path :book) (length dates))
                                        :black-scholes (cdr (a:lastcar (path-markets one path))))))))
    (check-that "(:book :value) recorded at each date = the sum of quantity x price on that path's market (1e-12)"
                (let ((table (gethash '(:book :value) (fincl::simulation-store-quantile-tables
                                                       (fincl::simulation-result-store one))))
                      (n (length (simulation-values one :value))))
                  (iter (for path below 3)
                        (always (iter (for (date . market) in (path-markets one path))
                                      (for k from 0)
                                      (for book = (aref (simulation-held one path :book) k))
                                      (always (< (abs (- (aref table (+ path (* k n)))
                                                         (iter (for p in (portfolio-positions book))
                                                               (sum (* (position-quantity p)
                                                                       (price (position-instrument p)
                                                                              :black-scholes market))))))
                                                 1d-12)))))))
    (check-that "under 8 workers clauses price serially (*serial-pricing*) and results are = to 1 worker"
                (and (= 1d0 (reduce #'min (subseq (simulation-statistic eight :serial :min) 1)))
                     (equalp (simulation-values one :value) (simulation-values eight :value))
                     (equalp (simulation-statistic one '(:book :delta) :mean)
                             (simulation-statistic eight '(:book :delta) :mean))))
    (check-error "a clause that sets the book to a non-portfolio signals invalid-argument"
                 (simulate ((make-gbm) m dates :n-paths 2)
                   (:portfolio book)
                   (:every (date) (setf book 42)))
                 invalid-argument))
  (let* ((call (make-option (call 100) (expires #D"2027-09-24")))
         (book (make-portfolio "b" (make-position call :quantity 2)))
         (once (adjust-position book (make-holding) :by 3))
         (twice (adjust-position once (make-holding) :by -1)))
    (check-that "adjust-position adds a position, then moves its quantity"
                (and (equal '(2d0 3d0) (mapcar #'position-quantity (portfolio-positions once)))
                     (equal '(2d0 2d0) (mapcar #'position-quantity (portfolio-positions twice)))
                     (equal '(2d0 -1d0) (mapcar #'position-quantity
                                                (portfolio-positions (adjust-position twice (make-holding) :to -1))))))))

(defun cholesky-checks ()
  (format t "~&Cholesky factor~%")
  (let* ((a (coerce '(4d0 2d0 2d0 3d0) 'fincl:double-vector))
         (l (fincl::cholesky-factor a 2)))
    (check-that "L L' = A" (and (= 2d0 (aref l 0)) (= 1d0 (aref l 1)) (= 0d0 (aref l 2))
                                (< (abs (- (aref l 3) (sqrt 2d0))) 1d-15)))
    (check-equal "an indefinite matrix reports its pivot" '(nil 1)
                 (multiple-value-list (fincl::cholesky-factor (coerce '(1d0 2d0 2d0 1d0) 'fincl:double-vector) 2)))))

(define-checks simulation-checks
  (cholesky-checks)
  (simulation-core-checks)
  (simulation-market-checks)
  (simulation-multi-checks)
  (simulation-docstring-checks)
  (simulation-portfolio-checks))

(defun run-simulation-tests ()
  "Run only the market path simulation checks. Returns T when all pass."
  (with-pricing-kernel ()
    (run-suite-list '(simulation-checks))))
