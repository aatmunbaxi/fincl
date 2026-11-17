;;;; simulation-components.lisp --- simulated market components (task 32)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Spot-only runs, bit for bit against the code before task 32
;;; --------------------------------------------------------------------

(defun spot-only-path ()
  (asdf:system-relative-pathname "fincl" "tests/fixtures/simulation-spot-only.sexp"))

(defun spot-only-runs ()
  "(name thunk) for each spot-only run of the fixture. Every run records at
the valuation date too, so no statistic holds NaN."
  (let ((dividend-market (make-market :valuation-date *today*
                                      :underlyings '((:spx :spot 100 :currency :usd :vol 0.2d0 :dividend 0.01d0
                                                      :dividend-schedule (("2027-01-15" 1.5d0) ("2027-06-15" 1d0))))
                                      :curves '((:usd :rate 0.03d0))))
        (call (make-option (call 100) (expires #D"2027-09-24") (underlying :spx))))
    `((:gbm-dividends
       ,(lambda ()
          (simulate ((make-gbm) dividend-market (every-n-days 30) :n-paths 400 :antithetic t
                     :keep (:quantiles (:paths 3)))
            (:accumulate (sum 0d0))
            (:state s :spot)
            (:start (market) (record :s s) (record :call (price call :black-scholes market)))
            (:every (date market)
              (incf sum s)
              (record :s s)
              (when (date< date #D"2027-09-24")
                (record :call (price call :black-scholes market))))
            (:final (:s s) (:mean (/ sum 12d0))))))
      (:heston-gbm-correlated
       ,(lambda ()
          (simulate (((:aapl (make-heston :v0 0.09d0 :theta 0.06d0 :rho -0.6d0)) (:msft (make-gbm)))
                     (three-underlying-market) (sim-quarterly-dates) :n-paths 300 :n-steps 20
                     :correlation '((:aapl :msft 0.6d0)) :keep (:paths 2))
            (:state a :spot :aapl) (:state v :variance :aapl) (:state b :spot :msft)
            (:start (record :a a) (record :v v))
            (:every (date market)
              (record :a a) (record :v v)
              (record :ibm (spot (market-view market :underlying :ibm))))
            (:final (:a a) (:b b)))))
      (:portfolio
       ,(lambda ()
          (simulate ((make-gbm) (sim-market) (every-n-days 60) :n-paths 100 :keep (:paths 2))
            (:portfolio book (make-portfolio "b" (make-position (make-holding) :quantity 10)
                                             (make-position (make-option (call 100) (expires #D"2027-09-24"))
                                                            :quantity -1))
                        :record (:value :delta) :engine :black-scholes)))))))

(defun nan-coded (vector)
  "VECTOR as a list, NaN written :NAN (a NaN cannot be printed readably)."
  (map 'list (lambda (x) (if (fincl::nan-p x) :nan x)) vector))

(defun spot-only-summary (result)
  "Everything the fixture compares of RESULT: outputs, every statistic of every
recorded name, and the stored paths' states, as lists of doubles."
  (let* ((plan (fincl::simulation-result-plan result))
         (legs (fincl::simulation-plan-legs plan)))
    `(:outputs ,(iter (for name in (simulation-output-names result))
                      (collect `(,name ,(coerce (simulation-values result name) 'list))))
      :statistics ,(iter (for name in (simulation-recorded-names result))
                         (collect `(,name ,@(iter (for s in '(:mean :std-dev :min :max :count))
                                                  (collect (nan-coded (simulation-statistic result name s)))))))
      :quantiles ,(iter (for name in (simulation-recorded-names result))
                        (when (simulation-quantiles-p result name)
                          (collect `(,name ,(nan-coded (simulation-statistic result name :quantile 0.25d0))))))
      :paths ,(iter (for path below (simulation-kept-paths result))
                    (collect (iter (for leg in-vector legs)
                                   (appending
                                    (iter (for name in (process-state-names (fincl::simulation-leg-process leg)))
                                          (collect (nan-coded (if (= 1 (length legs))
                                                               (simulation-path-state result path name)
                                                               (simulation-path-state
                                                                result path name
                                                                (fincl::simulation-leg-underlying leg)))))))))))))

(defun evaluate-spot-only-runs ()
  (iter (for (name thunk) in (spot-only-runs))
        (collect `(,name ,(spot-only-summary (funcall thunk))))))

(defun write-spot-only-fixture (&key (path (spot-only-path)))
  "Evaluate the spot-only runs and overwrite PATH (run once, before task 32)."
  (let ((entries (with-workers 4 #'evaluate-spot-only-runs)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (format s ";;;; simulation-spot-only.sexp --- spot-only SIMULATE runs before task 32~%~
;;;; Generated by tests/fixtures/make-simulation-fixture.lisp; never regenerate:~%~
;;;; task 32 requires these runs to stay = to the code before it.~%~%")
        (dolist (e entries) (prin1 e s) (terpri s))))
    (length entries)))

(defun read-spot-only-fixture ()
  (with-open-file (s (spot-only-path))
    (with-standard-io-syntax
      (iter (for entry = (read s nil s))
            (until (eq entry s))
            (collect entry)))))

(defun spot-only-fixture-checks ()
  (format t "~&Simulated components: spot-only runs against the fixture~%")
  (let ((fixture (read-spot-only-fixture)))
    (dolist (workers '(1 8))
      (let ((actual (with-workers workers #'evaluate-spot-only-runs)))
        (iter (for (name summary) in fixture)
              (check-that (format nil "~(~A~) under ~D worker~:P: outputs, statistics, quantiles and stored ~
paths = to the code before task 32" name workers workers)
                          (equal summary (second (assoc name actual)))))))))

;;; --------------------------------------------------------------------
;;; Phase 1: targets, legs and :derive
;;; --------------------------------------------------------------------

(defun dividend-market ()
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :currency :usd :vol 0.2d0 :dividend 0.01d0))
               :curves '((:usd :rate 0.03d0))))

;; A test-local process driving a dividend curve: GBM whose state sets a flat
;; yield of SCALE x spot / 10000 (no change to src/).
(defclass yield-gbm (fincl::gbm) ())

(defmethod process-targets ((p yield-gbm)) '(:dividend))

(defmethod structure-at-state ((p yield-gbm) target initial market date state &key (scale 1d0))
  (declare (ignore target initial market date))
  (make-instance 'fincl::flat-dividend-curve :rate (* scale (aref state 0) 1d-4)))

(defun simulation-target-checks ()
  (format t "~&Simulated components: targets, legs and :derive~%")
  (let* ((m (dividend-market))
         (call (make-option (call 100) (expires #D"2027-09-24") (underlying :spx)))
         (dates (sim-quarterly-dates)))
    (check-equal "market-component reads a curve, a vol, a spot by its bare keyword"
                 '(t t 100d0)
                 (list (eq (market-component m '(:curve :usd)) (gethash :usd (fincl::market-yield-curves m)))
                       (eq (market-component m '(:vol :spx)) (gethash :spx (fincl::market-vol-structures m)))
                       (market-component m :spx)))
    ;; A dividend yield derived from the spot.
    (let ((r (simulate ((make-gbm) m dates :n-paths 50 :keep (:paths 5))
               (:state s :spot)
               (:derive (:dividend :spx) ()
                 (make-instance 'fincl::flat-dividend-curve :rate (* s 1d-4)))
               (:final (:s s)))))
      (check-that ":derive of a dividend yield from the spot: a European on a stored market = Black-Scholes at that spot and yield (1e-12)"
                  (iter (for path below 5)
                        (always (iter (for (date . market) in (rest (path-markets r path)))
                                      (for s = (spot (market-view market :underlying :spx)))
                                      (always (< (abs (- (price call :black-scholes market)
                                                         (price call :black-scholes
                                                                (make-market :valuation-date date
                                                                             :underlyings `((:spx :spot ,s :currency :usd :vol 0.2d0
                                                                                             :dividend ,(* s 1d-4)))
                                                                             :curves '((:usd :rate 0.03d0))))))
                                                 1d-12)))))))
    ;; An identity :derive changes nothing.
    (flet ((run (identity)
             (if identity
                 (simulate ((make-heston) m dates :n-paths 200)
                   (:accumulate (c 0d0))
                   (:derive (:vol :spx) (market) (market-component market '(:vol :spx)))
                   (:every (date market) (setf c (record :call (price call :black-scholes market))))
                   (:final (:c c)))
                 (simulate ((make-heston) m dates :n-paths 200)
                   (:accumulate (c 0d0))
                   (:every (date market) (setf c (record :call (price call :black-scholes market))))
                   (:final (:c c))))))
      (let ((with (run t)) (without (run nil)))
        (check-that "a :derive returning the component unchanged gives = prices to a run without it"
                    (and (equalp (simulation-values with :c) (simulation-values without :c))
                         (equalp (subseq (simulation-statistic with :call :mean) 1) (subseq (simulation-statistic without :call :mean) 1))))))
    ;; A deterministic sequence of surfaces, by date.
    (let ((table (make-hash-table :test 'equalp)))
      (iter (for d in (cons *today* dates)) (for v from 20)
            (setf (gethash d table) (make-instance 'flat-black-vol :vol (/ v 100d0))))
      (let ((r (simulate ((make-gbm) m dates :n-paths 100 :antithetic t)
                 (:derive (:vol :spx) (market date) (gethash date table))
                 (:start (market) (record :same (if (eq (market-component market '(:vol :spx)) (gethash *today* table)) 1 0)))
                 (:every (date market)
                   (record :same (if (eq (market-component market '(:vol :spx)) (gethash date table)) 1 0))))))
        (check-that "a :derive of the date alone gives the table's surface (eq) on every path and date"
                    (and (every (lambda (x) (= x 1d0)) (simulation-statistic r :same :min))
                         (every (lambda (x) (= x 100d0)) (simulation-statistic r :same :count))))))
    ;; One copy per (path, date), with two :derive forms.
    (let ((counts '()))
      (count-calls (copies 'fincl::copy-composite-market)
        (simulate ((make-gbm) m dates :n-paths 3)
          (:derive (:dividend :spx) (market) (market-component market '(:dividend :spx)))
          (:derive (:vol :spx) () (make-instance 'flat-black-vol :vol 0.3d0))
          (:every (date market)
            (setf (car copies) 0)
            (spot market)
            (spot market)
            (push (car copies) counts))))
      (check-that "one copy-composite-market per (path, date) with two :derive forms"
                  (and (= 12 (length counts)) (every (lambda (n) (= n 1)) counts))))
    ;; Compile-time checks of :derive bodies.
    (check-that "a :derive body referring to an :accumulate variable warns at compile time"
                (let ((warned nil))
                  (handler-bind ((warning (lambda (c) (setf warned t) (muffle-warning c))))
                    (compile nil '(lambda (m d)
                                   (simulate ((make-gbm) m d)
                                     (:accumulate (total 0d0))
                                     (:derive (:dividend :spx) ()
                                       (make-instance 'fincl::flat-dividend-curve :rate total))
                                     (:every (date) (incf total))))))
                  warned))
    (check-error "record inside a :derive signals at macroexpansion"
                 (macroexpand '(simulate ((make-gbm) m d)
                                (:derive (:vol :spx) (market) (record :x 1) (market-component market '(:vol :spx)))))
                 invalid-argument)
    (check-error "stop inside a :derive signals at macroexpansion"
                 (macroexpand '(simulate ((make-gbm) m d) (:derive (:vol :spx) () (stop))))
                 invalid-argument)
    ;; The data form.
    (let* ((derive `(((:vol :spx) ,(lambda (market date states)
                                     (declare (ignore market date))
                                     (make-instance 'flat-black-vol :vol (* 0.002d0 (aref (svref states 0) 0)))))))
           (plan (fincl::make-simulation-plan (make-gbm) m dates :n-paths 10 :derive derive))
           (states (vector (coerce '(150d0) 'fincl:double-vector))))
      (check-that "make-simulation-plan :derive (data form): market-at-state sets the spot then the derived vol"
                  (let ((built (market-at-state m plan #D"2027-03-24" states)))
                    (and (= 150d0 (market-component built :spx))
                         (= 0.3d0 (fincl::flat-vol (market-component built '(:vol :spx))))
                         (date= #D"2027-03-24" (valuation-date built))))))
    ;; Leg syntax.
    (let ((m3 (three-underlying-market)))
      (flet ((run (form) (simulation-values (eval form) :a)))
        (check-that "legs written as ((:spot u) process) and correlated by target give = paths to the pairs of task 29"
                    (let ((old (run `(simulate (((:aapl (make-heston)) (:msft (make-gbm))) ,m3 ',dates :n-paths 200
                                                :correlation '((:aapl :msft 0.6d0)))
                                       (:state a :spot :aapl) (:final (:a a)))))
                          (new (run `(simulate ((((:spot :aapl) (make-heston)) ((:spot :msft) (make-gbm))) ,m3 ',dates
                                                :n-paths 200 :correlation (((:spot :aapl) :msft 0.6d0)))
                                       (:state a :spot (:spot :aapl)) (:final (:a a))))))
                      (equalp old new)))))
    (check-equal "simulation-state-names names a non-spot leg by its target"
                 '((:spot :spx) (:spot (:dividend :spx)))
                 (simulation-state-names `(((:spot :spx) ,(make-gbm)) ((:dividend :spx) ,(make-instance 'yield-gbm)))))
    ;; A test-local process driving a dividend curve, with a leg option.
    (let ((r (simulate ((((:dividend :spx) (make-instance 'yield-gbm) :scale 2d0)) m dates :n-paths 20 :keep (:paths 2))
               (:state x :spot (:dividend :spx))
               (:every (date market)
                 (record :gap (abs (- (dividend-yield (market-view market :underlying :spx) 1d0) (* 2d-4 x))))))))
      (check-that "a user process drives (:dividend u) through STRUCTURE-AT-STATE with its :scale option"
                  (every (lambda (x) (< x 1d-15)) (subseq (simulation-statistic r :gap :max) 1))))
    (check-that "a process given alone drives the default target of its first kind"
                (let ((r (simulate ((make-instance 'yield-gbm) m dates :n-paths 4)
                           (:every (date market) (record :q (dividend-yield (market-view market :underlying :spx) 1d0))))))
                  (/= 0.01d0 (aref (simulation-statistic r :q :mean) 4))))
    ;; Errors.
    (flet ((report (thunk)
             (handler-case (progn (funcall thunk) nil)
               (error (c) (princ-to-string c)))))
      (check-that "an unknown target kind lists the valid kinds"
                  (a:when-let ((s (report (lambda () (simulate ((((:volatility :spx) (make-gbm))) m dates) (:final 1))))))
                    (search ":DIVIDEND-SCHEDULE" s)))
      (check-that "a target the market lacks signals missing-market-data listing the keys"
                  (handler-case (progn (simulate ((make-gbm) m dates) (:derive (:curve :eur) () nil) (:final 1)) nil)
                    (missing-market-data (c) (search ":USD" (princ-to-string c)))))
      (check-that "a wrong value type names the target and the date"
                  (a:when-let ((s (report (lambda () (simulate ((make-gbm) m dates :n-paths 2)
                                                       (:derive (:vol :spx) () 0.2d0)
                                                       (:every (date market) (spot market)))))))
                    (and (search "(:VOL :SPX)" s) (search "2026-12-24" s) (search "BLACK-VOL-STRUCTURE" s))))
      (check-error "two legs on one target signal invalid-argument"
                   (simulate (((:spx (make-gbm)) ((:spot :spx) (make-heston))) m dates) (:final 1))
                   invalid-argument)
      (check-error "a :derive on a leg's target signals invalid-argument"
                   (simulate ((make-gbm) m dates) (:derive (:spot :spx) () 100d0) (:final 1))
                   invalid-argument)
      (check-that "a process driving another kind than the target lists its kinds"
                  (a:when-let ((s (report (lambda () (simulate ((((:vol :spx) (make-gbm))) m dates) (:final 1))))))
                    (search "drives: kinds :SPOT" s)))
      (check-that "a leg option the process does not take is refused before any path runs, listing what it takes"
                  (a:when-let ((s (report (lambda () (simulate (((:spx (make-gbm) :knots 3)) m dates) (:final 1))))))
                    (search "none" s)))
      (check-that "... and a user process's options are listed"
                  (a:when-let ((s (report (lambda () (simulate ((((:dividend :spx) (make-instance 'yield-gbm) :size 3))
                                                                m dates) (:final 1))))))
                    (search ":SCALE" s)))
      (check-error "a CGMY process is still refused by the simulator"
                   (simulate ((make-cgmy) m dates) (:final 1)) invalid-argument)
      (check-error "... and by the Monte Carlo engine"
                   (price call (make-engine :mc :process (make-cgmy) :n-paths 100) m)
                   unsupported-combination)
      (check-error "Hull-White is not an equity process for the Monte Carlo engine"
                   (price call (make-engine :mc :process (make-hull-white) :n-paths 100) m)
                   error))))

;;; --------------------------------------------------------------------
;;; Phase 2: curves under Hull-White, discounting and fixings
;;; --------------------------------------------------------------------

(defun years-after (date n) (add-tenor date (make-tenor n :years)))

(defun hw-initial-curve ()
  "An upward-sloping EUR curve: zero rates 2% to 3.8% out to 30 years."
  (let ((dates (cons *today* (mapcar (lambda (n) (years-after *today* n)) '(1 2 5 10 30))))
        (zeros '(0d0 0.02d0 0.025d0 0.03d0 0.034d0 0.038d0)))
    (make-instance 'interpolated-discount-curve
                   :dates dates
                   :discounts (mapcar (lambda (d z) (exp (- (* z (year-fraction +actual-365-fixed+ *today* d)))))
                                      dates zeros))))

(defun hw-sim-market ()
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :currency :eur :vol 0.2d0))
               :curves `((:eur :curve ,(hw-initial-curve)))
               :discounting '((:eur . :eur))))

(defun monthly-knots (&optional (months 240))
  (iter (for n from 1 to months) (collect (make-tenor n :months))))

(defun yearly-knots (&optional (years 30))
  (iter (for n from 1 to years) (collect (make-tenor n :years))))

(defun mean-within-3-se-p (result name k expected)
  (let ((mean (aref (simulation-statistic result name :mean) k))
        (sd (aref (simulation-statistic result name :std-dev) k))
        (n (aref (simulation-statistic result name :count) k)))
    (<= (abs (- mean expected)) (* 3 (/ sd (sqrt n))))))

(defun output-within-3-se-p (result name expected)
  (multiple-value-bind (mean se) (simulation-summary result (simulation-values result name))
    (values (<= (abs (- mean expected)) (* 3 se)) mean se)))

(defun euribor-swap (type rate start end)
  "A EUR swap from START to END: annual 30/360 fixed against EURIBOR 6M
forecast on the :EUR curve."
  (make-vanilla-swap type 1d6 (make-schedule start end #T"1Y" :calendar +target+)
                     rate (make-instance 'thirty-360 :variant :bond-basis)
                     (make-schedule start end #T"6M" :calendar +target+)
                     (euribor #T"6M" :forecast-curve :eur)
                     :floating-day-counter +actual-360+))

(defun hw-martingale-checks ()
  (format t "~&Simulated components: Hull-White martingales and the exact step~%")
  (let* ((m (hw-sim-market))
         (curve (hw-initial-curve))
         (dates (mapcar (lambda (n) (years-after *today* n)) '(1 3 5)))
         (maturities (mapcar (lambda (n) (years-after *today* n)) '(7 10)))
         (r (simulate ((((:curve :eur) (make-hull-white :a 0.05d0 :sigma 0.01d0) :knots (yearly-knots)))
                       m dates :n-paths 20000)
              (:every (date market)
                (record :d (path-discount))
                (iter (for maturity in maturities) (for name in '(:dp7 :dp10))
                      (record name (* (path-discount) (curve-discount (market-component market '(:curve :eur))
                                                                      maturity))))))))
    (check-that "the mean of path-discount at 1, 3 and 5 years is P(0, t) within 3 SE"
                (iter (for d in dates) (for k from 1)
                      (always (mean-within-3-se-p r :d k (curve-discount curve d)))))
    (check-that "the mean of path-discount x P(t, T) is P(0, T) for T = 7 and 10 years within 3 SE"
                (iter (for name in '(:dp7 :dp10)) (for maturity in maturities)
                      (always (iter (for k from 1 to 3)
                                    (always (mean-within-3-se-p r name k (curve-discount curve maturity))))))))
  ;; One step's (x, I) covariance, from a step not at 0.
  (dolist (a '(0.1d0 0d0))
    (let* ((m (hw-sim-market))
           (sigma 0.012d0)
           (d1 (years-after *today* 1)) (d2 (years-after *today* 3))
           (n 40000)
           (r (simulate ((((:curve :eur) (make-hull-white :a a :sigma sigma))) m (list d1 d2) :n-paths n)
                (:accumulate (r1 0d0) (i1 0d0))
                (:state rate :short-rate (:curve :eur))
                (:state integral :integral (:curve :eur))
                (:every (date) (when (date= date d1) (setf r1 rate i1 integral)))
                (:final (:r1 r1) (:i1 i1) (:r2 rate) (:i2 integral))))
           (dt (- (market-time m d2) (market-time m d1)))
           (b (if (zerop a) dt (/ (- 1 (exp (- (* a dt)))) a)))
           (v1 (if (zerop a) (* sigma sigma dt) (* (/ (* sigma sigma) (* 2 a)) (- 1 (exp (* -2 a dt))))))
           (v2 (if (zerop a)
                   (/ (* sigma sigma dt dt dt) 3)
                   (* (/ (* sigma sigma) (* a a)) (+ dt (* -2 b) (/ (- 1 (exp (* -2 a dt))) (* 2 a))))))
           (c (* 0.5d0 sigma sigma b b))
           (e1 (map 'list (lambda (r1 r2) (- r2 (* (exp (- (* a dt))) r1)))
                    (simulation-values r :r1) (simulation-values r :r2)))
           (e2 (map 'list (lambda (r1 i1 i2) (- i2 i1 (* b r1)))
                    (simulation-values r :r1) (simulation-values r :i1) (simulation-values r :i2))))
      (flet ((cov (x y)
               (let ((mx (/ (reduce #'+ x) n)) (my (/ (reduce #'+ y) n)))
                 (/ (reduce #'+ (mapcar (lambda (u v) (* (- u mx) (- v my))) x y)) (1- n)))))
        (let ((s1 (cov e1 e1)) (s2 (cov e2 e2)) (s12 (cov e1 e2)))
          (check-that (format nil "a = ~A: one step's Var x (~,3E), Var I (~,3E) and Cov (~,3E) match D5 within 3 SE~@[ (the Ho-Lee limit)~]"
                              a s1 s2 s12 (zerop a))
                      (and (<= (abs (- s1 v1)) (* 3 v1 (sqrt (/ 2d0 n))))
                           (<= (abs (- s2 v2)) (* 3 v2 (sqrt (/ 2d0 n))))
                           (<= (abs (- s12 c)) (* 3 (sqrt (/ (+ (* v1 v2) (* c c)) n))))))))))
  ;; The curve at its knots.
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.03d0 :sigma 0.008d0))
         (fitted (fincl::resolve-process hw (fincl::target-view :curve m :eur) 1d0))
         (rate-curve (fincl::fitted-rate-curve fitted))
         ;; The default curve is exact (task 33); the knots are given to test their cut.
         (r (simulate ((((:curve :eur) hw :knots fincl::*default-curve-knots*)) m
                       (mapcar (lambda (n) (years-after *today* n)) '(1 2)) :n-paths 20 :keep (:paths 4))
              (:final (:x 0d0))))
         (worst 0d0))
    (dotimes (path 4)
      (iter (for (date . market) in (path-markets r path))
            (for rate in-vector (simulation-path-state r path :short-rate))
            (for curve = (market-component market '(:curve :eur)))
            (dolist (knot (fincl::curve-knot-dates date fincl::*default-curve-knots* (years-after *today* 30)))
              (setf worst (max worst (abs (1- (/ (curve-discount curve knot)
                                                 (hull-white-discount-bond fitted rate-curve (market-time m date)
                                                                           (market-time m knot) rate)))))))))
    (check-that (format nil "the simulated curve at each knot = hull-white-discount-bond (worst ~,1E, below 1e-14)" worst)
                (< worst 1d-14))
    (check-that "the default knots are cut at the initial curve's last date"
                (every (lambda (d) (not (date< (years-after *today* 30) d)))
                       (fincl::curve-dates (market-component (cdr (a:lastcar (path-markets r 0))) '(:curve :eur)))))))

(defun hw-instrument-checks ()
  (format t "~&Simulated components: rates instruments on simulated curves~%")
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.03d0 :sigma 0.008d0))
         (start #D"2028-09-28")
         (swaps (mapcar (lambda (type) (euribor-swap type 0.033d0 start #D"2033-09-28")) '(:payer :receiver)))
         (expiry (first (fixing-dates (first swaps))))
         (r (simulate ((((:curve :eur) hw :knots (monthly-knots))) m (list expiry) :n-paths 8000)
              (:final (:payer (* (path-discount) (max 0d0 (swap-npv (first swaps) market))))
                      (:receiver (* (path-discount) (max 0d0 (swap-npv (second swaps) market))))
                      (:swap (* (path-discount) (swap-npv (first swaps) market)))))))
    (iter (for swap in swaps) (for name in '(:payer :receiver))
          (for jamshidian = (price (make-swaption swap expiry) (make-engine :jamshidian :process hw) m))
          (multiple-value-bind (ok mean se) (output-within-3-se-p r name jamshidian)
            (check-that (format nil "~(~A~) swaption: mean discounted max(swap value, 0) at expiry ~,1F = Jamshidian ~,1F within 3 SE (~,1F)"
                                name mean jamshidian se)
                        ok)))
    (multiple-value-bind (ok mean se) (output-within-3-se-p r :swap (swap-npv (first swaps) m))
      (check-that (format nil "the forward swap's discounted value at expiry ~,1F = its value today ~,1F within 3 SE (~,1F)"
                          mean (swap-npv (first swaps) m) se)
                  ok)))
  ;; A par swap seasoned past its first fixing, and a bond.
  (let* ((m (hw-sim-market))
         (hw (make-hull-white :a 0.03d0 :sigma 0.008d0))
         (start #D"2026-12-29")
         (swap (let ((s (euribor-swap :payer 0.03d0 start #D"2031-12-29")))
                 (euribor-swap :payer (fair-rate s m) start #D"2031-12-29")))
         (bond (make-fixed-rate-bond (make-schedule #D"2027-09-24" #D"2032-09-24" #T"1Y" :calendar +target+)
                                     0.035d0 +actual-365-fixed+ :settlement-days 0 :currency :eur))
         (horizon #D"2027-04-15")
         (dates (sort `(,horizon ,@(remove-if-not (lambda (d) (date< d horizon)) (fixing-dates swap m))) #'date<))
         (r (simulate ((((:curve :eur) hw :knots (monthly-knots))) m dates :n-paths 8000 :fix (list (euribor #T"6M" :forecast-curve :eur)))
              (:final (:pv (* (path-discount) (swap-npv swap market)))
                      (:bond (* (path-discount) (price bond :discounting market)))))))
    (multiple-value-bind (ok mean se) (output-within-3-se-p r :pv 0d0)
      (check-that (format nil "a par swap's discounted value after its first fixing has mean ~,1F, 0 within 3 SE (~,1F)" mean se)
                  ok))
    (multiple-value-bind (ok mean se) (output-within-3-se-p r :bond (price bond :discounting m))
      (check-that (format nil "a fixed-rate bond's discounted value before its first coupon ~,4F = today's ~,4F within 3 SE (~,4F)"
                          mean (price bond :discounting m) se)
                  ok)))
  ;; An uncorrelated spot leg: the discounted spot keeps its mean.
  (let* ((m (hw-sim-market))
         (dates (mapcar (lambda (n) (years-after *today* n)) '(1 3)))
         (r (simulate ((((:curve :eur) (make-hull-white :a 0.03d0 :sigma 0.01d0)) ((:spot :spx) (make-gbm)))
                       m dates :n-paths 20000)
              (:state s :spot :spx)
              (:every (date) (record :ds (* (path-discount) s))))))
    (check-that "a spot uncorrelated with the curve: the mean of path-discount x spot is the initial spot within 3 SE"
                (iter (for k from 1 to 2) (always (mean-within-3-se-p r :ds k 100d0)))))
  (let* ((m (hw-sim-market))
         (r (simulate ((((:curve :eur) (make-hull-white)) ((:spot :spx) (make-gbm))) m (list (years-after *today* 1))
                       :n-paths 20000 :correlation (((:curve :eur) :spx 0.5d0)))
              (:state s :spot :spx) (:state rate :short-rate (:curve :eur))
              (:final (:z (log s)) (:r rate))))
         (x (coerce (simulation-values r :z) 'list)) (y (coerce (simulation-values r :r) 'list))
         (n 20000)
         (mx (/ (reduce #'+ x) n)) (my (/ (reduce #'+ y) n))
         (rho (/ (reduce #'+ (mapcar (lambda (u v) (* (- u mx) (- v my))) x y))
                 (sqrt (* (reduce #'+ (mapcar (lambda (u) (expt (- u mx) 2)) x))
                          (reduce #'+ (mapcar (lambda (v) (expt (- v my) 2)) y)))))))
    ;; Over one step the short rate's shock is the Brownian increment's
    ;; (exactly, up to its mean-reversion weighting), so the sample
    ;; correlation is close to 0.5.
    (check-that (format nil "a curve leg correlated by target with a spot leg: correlation ~,3F near 0.5" rho)
                (< (abs (- rho 0.5d0)) 0.03d0))))

;; A test-local Vasicek process driving a curve (no change to src/).
(define-process test-vasicek (stochastic-process) (:state-size 2 :factors 2)
  ((kappa 0.2d0 :bounds ((0) nil))
   (long-rate 0.04d0)
   (vol 0.01d0 :bounds (0 nil))
   (r0 0.02d0)))

(defmethod process-targets ((p test-vasicek)) '(:curve))
(defmethod process-state-names ((p test-vasicek)) '(:short-rate :integral))
(defmethod initial-state ((p test-vasicek) market)
  (declare (ignore market))
  (coerce `(,(r0 p) 0d0) 'fincl:double-vector))

(defun vasicek-bond (p tau rate)
  (let* ((k (kappa p)) (s (vol p))
         (b (/ (- 1 (exp (- (* k tau)))) k)))
    (exp (- (* (- (long-rate p) (/ (* s s) (* 2 k k))) (- b tau))
            (/ (* s s b b) (* 4 k))
            (* b rate)))))

(defmethod fincl::make-stepper ((p test-vasicek) market times)
  (declare (ignore market))
  (lambda (state normals j)
    (let* ((k (kappa p)) (s (vol p)) (theta (long-rate p))
           (dt (- (aref times (1+ j)) (aref times j)))
           (b (/ (- 1 (exp (- (* k dt)))) k))
           (v1 (* (/ (* s s) (* 2 k)) (- 1 (exp (* -2 k dt)))))
           (v2 (* (/ (* s s) (* k k)) (+ dt (* -2 b) (/ (- 1 (exp (* -2 k dt))) (* 2 k)))))
           (c (* 0.5d0 s s b b))
           (l21 (/ c (sqrt v1)))
           (x (- (aref state 0) theta)))
      (incf (aref state 1) (+ (* theta dt) (* x b) (* l21 (aref normals 0))
                              (* (sqrt (max 0d0 (- v2 (* l21 l21)))) (aref normals 1))))
      (setf (aref state 0) (+ theta (* x (exp (- (* k dt)))) (* (sqrt v1) (aref normals 0))))
      nil)))

(defmethod structure-at-state ((p test-vasicek) target initial market date state &key)
  (declare (ignore target initial))
  (let ((knots (mapcar (lambda (n) (years-after date n)) '(1 2 3 4 5 6 7 8 9 10))))
    (make-instance 'interpolated-discount-curve
                   :dates (cons date knots)
                   :discounts (cons 1d0 (mapcar (lambda (d) (vasicek-bond p (year-fraction +actual-365-fixed+ date d)
                                                                         (aref state 0)))
                                                knots)))))

(defun vasicek-protocol-checks ()
  (format t "~&Simulated components: a test-local Vasicek drives a curve~%")
  (let* ((m (hw-sim-market))
         (p (make-test-vasicek))
         (dates (mapcar (lambda (n) (years-after *today* n)) '(1 3)))
         (maturity (years-after *today* 5))
         (r (simulate ((((:curve :eur) p)) m dates :n-paths 20000)
              (:every (date market)
                (record :d (path-discount))
                (record :dp (* (path-discount) (curve-discount (market-component market '(:curve :eur)) maturity)))))))
    (check-that "Vasicek: the mean of path-discount is its P(0, t), and of path-discount x P(t, 5Y) its P(0, 5Y), within 3 SE"
                (iter (for d in dates) (for k from 1)
                      (always (and (mean-within-3-se-p r :d k (vasicek-bond p (market-time m d) 0.02d0))
                                   (mean-within-3-se-p r :dp k (vasicek-bond p (market-time m maturity) 0.02d0))))))))

(defun fixing-checks ()
  (format t "~&Simulated components: fixings along a path~%")
  (let* ((m (hw-sim-market))
         (index (euribor #T"6M" :forecast-curve :eur))
         (swap (euribor-swap :payer 0.03d0 #D"2026-12-29" #D"2029-12-28"))
         (horizon #D"2027-09-10")
         (dates (sort `(,horizon ,@(remove-if-not (lambda (d) (date< d horizon)) (fixing-dates swap m))) #'date<))
         (run (lambda (n-paths)
                (simulate ((((:curve :eur) (make-hull-white) :knots (monthly-knots 60))) m dates :n-paths n-paths
                           :fix (list index) :keep (:paths 3))
                  (:every (date market) (record :npv (swap-npv swap market)))
                  (:final (:npv (swap-npv swap market))))))
         (r (funcall run 200)))
    (check-that "a fixing on an earlier date of a path = index-fixing on that date's market of the path (=)"
                (iter (for path below 3)
                      (for markets = (path-markets r path))
                      (always (iter (for (date . market) in (rest markets))
                                    (always (iter (for (fixing-date . fixing-market) in (rest markets))
                                                  (while (date< fixing-date date))
                                                  (always (= (index-fixing index market fixing-date)
                                                             (index-fixing index fixing-market fixing-date)))))))))
    (check-that "different paths fix differently"
                (let ((d (first (fixing-dates swap m))))
                  (/= (index-fixing index (cdr (a:lastcar (path-markets r 0))) d)
                      (index-fixing index (cdr (a:lastcar (path-markets r 1))) d))))
    (check-that "outputs = under 1, 2 and 8 workers with :fix"
                (let ((runs (mapcar (lambda (n) (with-workers n (lambda () (simulation-values (funcall run 64) :npv))))
                                    '(1 2 8))))
                  (every (lambda (v) (equalp v (first runs))) runs)))
    (check-that "path i is unchanged when n-paths doubles, with :fix"
                (equalp (simulation-values (funcall run 64) :npv) (subseq (simulation-values (funcall run 128) :npv) 0 64)))
    (check-that "a fixing date missing from DATES signals missing-fixing suggesting FIXING-DATES"
                (handler-case (progn (simulate ((((:curve :eur) (make-hull-white))) m (list horizon) :n-paths 4
                                                :fix (list index))
                                       (:final (:npv (swap-npv swap market))))
                                     nil)
                  (missing-fixing (c) (search "FIXING-DATES" (princ-to-string c)))))
    (check-equal "fixing-dates of a swap with a market: its Ibor fixing dates after the valuation date"
                 6 (length (fixing-dates swap m)))))

;;; --------------------------------------------------------------------
;;; Phase 3: derived surfaces and the desk
;;; --------------------------------------------------------------------

(defun ssvi-base ()
  (make-ssvi-surface '(0.25 0.5 1 2) '(0.01 0.02 0.04 0.08) :rho -0.4 :eta 1 :gamma 0.4))

(defun desk-market (&optional (vol (ssvi-base)))
  "Example 3 and 5's market: r = 0, no dividends, SPX at 100 on VOL."
  (make-market :valuation-date *today*
               :underlyings `((:spx :spot 100 :currency :usd :vol ,vol))
               :curves '((:usd :rate 0d0))))

(defun heston-atm-variance (heston v)
  "a(tau) = theta tau + (v - theta)(1 - e^(-kappa tau))/kappa, HESTON's
expected integrated variance from variance V."
  (let ((kappa (process-parameter heston :kappa)) (theta (process-parameter heston :theta)))
    (lambda (tau) (+ (* theta tau) (* (- v theta) (/ (- 1 (exp (- (* kappa tau)))) kappa))))))

(defun rescale-checks ()
  (format t "~&Simulated components: rescaled surfaces~%")
  (let* ((base (ssvi-base))
         (a (lambda (tau) (+ (* 0.05d0 tau) (* 0.01d0 (- 1 (exp (* -2 tau)))))))
         (s (rescale-vol-structure base a))
         (worst-atm 0d0) (worst-shape 0d0))
    (dolist (tau '(0.1d0 0.5d0 1d0 3d0))
      (setf worst-atm (max worst-atm (abs (1- (/ (fincl::surface-variance s 0d0 tau) (funcall a tau))))))
      (dolist (k '(-0.6d0 -0.2d0 0.1d0 0.5d0))
        (setf worst-shape (max worst-shape (abs (1- (/ (/ (fincl::surface-variance s k tau) (fincl::surface-variance s 0d0 tau))
                                                       (/ (fincl::surface-variance base k tau) (fincl::surface-variance base 0d0 tau)))))))))
    (check-that (format nil "rescale-vol-structure: ATM-forward total variance = a(tau) (worst ~,1E, 1e-12)" worst-atm)
                (< worst-atm 1d-12))
    (check-that (format nil "... and w(k, tau) / w(0, tau) is the base surface's at every k (worst ~,1E, 1e-12)" worst-shape)
                (< worst-shape 1d-12))
    (let* ((m (desk-market s)) (view (market-view m :underlying :spx)) (d #D"2027-09-24"))
      (check-that "... read through a market, the at-the-money-forward Black variance is a(tau) (1e-12)"
                  (< (abs (1- (/ (black-variance view (forward view d) d) (funcall a (market-time m d))))) 1d-12)))
    (check-that "a flat base rescales to a(tau) at every strike"
                (let ((f (rescale-vol-structure (make-instance 'flat-black-vol :vol 0.2d0) a)))
                  (< (abs (- (fincl::surface-variance f 0.7d0 2d0) (funcall a 2d0))) 1d-15)))
    (check-error "a structure that is neither a moneyness surface nor flat is refused"
                 (rescale-vol-structure (hw-initial-curve) a) invalid-argument)))

(defun example-3-run (heston m dates &key (n-paths 100) (keep 100) (expiry #D"2028-09-24"))
  "Reference example 3: a straddle marked by Black-Scholes on a surface
derived from the world's Heston variance."
  (let ((call (make-option (call 100) (expires expiry) (underlying :spx)))
        (put (make-option (put 100) (expires expiry) (underlying :spx))))
    (simulate (heston m dates :n-paths n-paths :keep (:paths keep))
      (:state v :variance)
      (:derive (:vol :spx) (market)
        (rescale-vol-structure (market-component market '(:vol :spx)) (heston-atm-variance heston v)))
      (:every (date market)
        (record :straddle (+ (price call :black-scholes market) (price put :black-scholes market)))))))

(defun derived-surface-checks ()
  (format t "~&Simulated components: example 3, a surface derived from the Heston variance~%")
  (let* ((heston (make-heston :v0 0.04d0 :theta 0.06d0 :kappa 1.5d0 :xi 0d0 :rho -0.7d0))
         (m (desk-market (make-instance 'flat-black-vol :vol 0.2d0)))
         (call (make-option (call 105) (expires #D"2028-09-24") (underlying :spx)))
         (r (example-3-run heston m (sim-quarterly-dates) :n-paths 5 :keep 5))
         (worst 0d0))
    (dotimes (path 5)
      (iter (for (date . market) in (rest (path-markets r path)))
            (for v in-vector (subseq (simulation-path-state r path :variance) 1))
            (for cos = (price call (make-engine :cos :process (make-heston :v0 v :theta 0.06d0 :kappa 1.5d0
                                                                            :xi 0d0 :rho -0.7d0)
                                                :n-terms 512)
                              market))
            (setf worst (max worst (abs (- (price call :black-scholes market) cos))))))
    (check-that (format nil "xi = 0: Black-Scholes on the derived surface = COS Heston at the path's variance (worst ~,1E, 1e-8)" worst)
                (< worst 1d-8)))
  (let* ((heston (make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0 :xi 0.5d0 :rho -0.7d0))
         (r (example-3-run heston (desk-market) (sim-quarterly-dates)))
         (failures 0))
    (dotimes (path 100)
      (dolist (dm (rest (path-markets r path)))
        (unless (calendar-arbitrage-free-p (market-component (cdr dm) '(:vol :spx))
                                           :times '(0.05d0 0.25d0 0.5d0 1d0 2d0 3d0 5d0))
          (incf failures))))
    (check-that (format nil "example 3's derived surfaces on 100 stored paths are free of calendar arbitrage (~D of 400 fail)" failures)
                (zerop failures))))

(defun surface-quote-checks ()
  (format t "~&Simulated components: surface-quotes~%")
  (let* ((m (desk-market))
         (view (market-view m :underlying :spx))
         (options (iter (for k in '(90 100 110))
                        (appending (list (make-option (call k) (expires #D"2027-03-24") (underlying :spx))
                                         (make-option (put k) (expires #D"2027-09-24") (underlying :spx))))))
         (q (surface-quotes m :spx options :weight 2)))
    (check-that "surface-quotes: each quote's vol = the market's Black vol at its strike and expiry (=), with the weight"
                (iter (for quote in (fincl::quote-set-quotes q))
                      (for o = (fincl::quote-instrument quote))
                      (always (and (= (fincl::%quote-vol quote)
                                      (black-vol view (fincl::strike (option-payoff o)) (fincl::expiry (option-exercise o))))
                                   (= 2 (fincl::quote-weight quote))))))
    (let ((later (derive-market m :valuation-date #D"2027-04-01")))
      (check-equal "options expired on the market's date are dropped"
                   3 (length (fincl::quote-set-quotes (surface-quotes later :spx options))))
      (check-error "no live option signals invalid-argument"
                   (surface-quotes (derive-market m :valuation-date #D"2027-10-01") :spx options)
                   invalid-argument))))

(defun desk-options ()
  (iter (for expiry in '(#D"2027-03-24" #D"2027-09-24"))
        (appending (iter (for k in '(90 100 110))
                         (collect (if (< k 100)
                                      (make-option (put k) (expires expiry) (underlying :spx))
                                      (make-option (call k) (expires expiry) (underlying :spx))))))))

(defun desk-calibrate (desk market options)
  (handler-bind ((convergence-failure #'use-best))
    (calibrate desk market (surface-quotes market :spx options) :engine :cos :max-iterations 30)))

(defun example-5-run (heston-0 &key (refit t) (n-paths 3) (dates (list #D"2026-10-01" #D"2026-10-08")) serial-seen)
  "Reference example 5 at a few paths and dates: the desk refits its own
Heston to the observed surface each date (warm-started on the path) and
marks a straddle with COS under it. The world is example 3's."
  (let* ((world (make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0 :xi 0.5d0 :rho -0.7d0))
         (m (desk-market))
         (options (desk-options))
         (call (make-option (call 100) (expires #D"2027-06-24") (underlying :spx)))
         (put (make-option (put 100) (expires #D"2027-06-24") (underlying :spx))))
    (simulate (world m dates :n-paths n-paths :keep (:paths n-paths))
      (:state v :variance)
      (:state s :spot)
      (:derive (:vol :spx) (market)
        (rescale-vol-structure (market-component market '(:vol :spx)) (heston-atm-variance world v)))
      (:hold (desk heston-0) (marks '()))
      (:accumulate (shares 0d0) (cash (+ (price call :black-scholes m) (price put :black-scholes m))))
      (:every (date market)
        (when refit
          (when serial-seen (push fincl::*serial-pricing* (car serial-seen)))
          (multiple-value-bind (fit report) (desk-calibrate desk market options)
            (setf desk fit)
            (record :fit-rmse (getf report :rmse))))
        (let* ((engine (make-engine :cos :process desk))
               (delta (- (+ (getf (greeks call engine market :which '(:delta)) :delta)
                            (getf (greeks put engine market :which '(:delta)) :delta)))))
          (decf cash (* (- delta shares) s))
          (setf shares delta)
          (let ((mark (- (+ (price call engine market) (price put engine market)))))
            (push mark marks)
            (record :mark mark))))
      (:final (:pnl (+ cash (* shares s) (- (+ (price call :black-scholes market) (price put :black-scholes market)))))))))

(defun desk-checks ()
  (format t "~&Simulated components: example 5, a desk refitting its model~%")
  (let* ((m (desk-market))
         (heston-0 (desk-calibrate (make-heston) m (desk-options)))
         (serial (list '()))
         (one (with-workers 1 (lambda () (example-5-run heston-0 :serial-seen serial))))
         (eight (with-workers 8 (lambda () (example-5-run heston-0))))
         (fixed (example-5-run heston-0 :refit nil)))
    (flet ((parameters (p) (mapcar (lambda (name) (process-parameter p name)) '(:v0 :kappa :theta :xi :rho))))
      (check-that "example 5: outputs, marks and refit parameters = under 1 and 8 workers"
                  (and (equalp (simulation-values one :pnl) (simulation-values eight :pnl))
                       (iter (for path below 3)
                             (always (and (equal (aref (simulation-held one path :marks) 2) (aref (simulation-held eight path :marks) 2))
                                          (equal (parameters (aref (simulation-held one path :desk) 2))
                                                 (parameters (aref (simulation-held eight path :desk) 2))))))))
      (check-that (format nil "*serial-pricing* is true at every calibrate call from a clause (~D calls)" (length (car serial)))
                  (and (car serial) (every #'identity (car serial))))
      (check-that "refitting outside the run on path-markets, date by date from heston-0, gives = parameters and marks"
                  (iter (for path below 3)
                        (for desk = heston-0)
                        (always (iter (for (date . market) in (rest (path-markets one path)))
                                      (for k from 1)
                                      (setf desk (desk-calibrate desk market (desk-options)))
                                      (for engine = (make-engine :cos :process desk))
                                      (always (and (equal (parameters desk) (parameters (aref (simulation-held one path :desk) k)))
                                                   (= (- (+ (price (make-option (call 100) (expires #D"2027-06-24") (underlying :spx))
                                                                   engine market)
                                                            (price (make-option (put 100) (expires #D"2027-06-24") (underlying :spx))
                                                                   engine market)))
                                                      (first (aref (simulation-held one path :marks) k)))))))))
      (check-that "the world does not see the desk: stored states and markets = to the run without the refit"
                  (iter (for path below 3)
                        (always (and (equalp (simulation-path-state one path :spot) (simulation-path-state fixed path :spot))
                                     (equalp (simulation-path-state one path :variance) (simulation-path-state fixed path :variance))
                                     (iter (for (nil . a) in (path-markets one path))
                                           (for (nil . b) in (path-markets fixed path))
                                           (always (and (= (market-component a :spx) (market-component b :spx))
                                                        (iter (for k in '(-0.3d0 0d0 0.3d0))
                                                              (always (iter (for tau in '(0.25d0 1d0))
                                                                            (always (= (fincl::surface-variance (market-component a '(:vol :spx)) k tau)
                                                                                       (fincl::surface-variance (market-component b '(:vol :spx)) k tau))))))))))))))))

;;; --------------------------------------------------------------------
;;; Phase 5: documentation examples and tone
;;; --------------------------------------------------------------------

(defun docstring-form (symbol kind marker)
  "The form in SYMBOL's KIND docstring starting at MARKER, read in this package
with fincl:syntax."
  (let ((doc (documentation symbol kind))
        (*package* (find-package :fincl/tests))
        (*readtable* (named-readtables:find-readtable 'fincl:syntax)))
    (read-from-string (subseq doc (or (search marker doc)
                                      (error "~S's docstring has no ~S" symbol marker))))))

(defun docs-market ()
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :currency :usd :vol 0.2d0))
               :curves '((:usd-ois :rate 0.04d0))
               :discounting '((:usd . :usd-ois))))

(defun docstring-example-checks ()
  (format t "~&Simulated components: docstring examples~%")
  (let ((m (docs-market)) (d #D"2027-03-24")
        (*package* (find-package :fincl/tests))) ; SIMULATE interns MARKET in the current package
    (flet ((run (symbol kind marker bindings)
             (eval `(let ,(mapcar (lambda (b) `(,(first b) ',(second b))) bindings)
                      (declare (ignorable ,@(mapcar #'first bindings)))
                      ,(docstring-form symbol kind marker)))))
      (check-that "market-component's examples return a yield curve and a spot"
                  (and (typep (run 'market-component 'function "(market-component m '(:curve" `((m ,m))) 'yield-curve)
                       (= 100d0 (run 'market-component 'function "(market-component m :spx)" `((m ,m))))))
      (check-equal "process-targets's example" '(:spot)
                   (run 'process-targets 'function "(process-targets (make-gbm))" '()))
      (check-equal "structure-at-state's example" 104.2d0
                   (run 'structure-at-state 'function "(structure-at-state (make-gbm)" `((m ,m) (d ,d))))
      (let ((swap (euribor-swap :payer 0.03d0 #D"2026-12-29" #D"2028-12-29"))
            (hw-market (hw-sim-market)))
        (check-that "fixing-dates's example returns ascending dates after the valuation date"
                    (let ((dates (run 'fixing-dates 'function "(fixing-dates swap m)" `((swap ,swap) (m ,hw-market)))))
                      (and dates (every (lambda (x) (date< *today* x)) dates)
                           (every #'date< dates (rest dates))))))
      (check-that "surface-quotes's example returns a calibrated process"
                  (typep (run 'surface-quotes 'function "(calibrate desk market"
                              `((desk ,(make-heston)) (market ,(desk-market)) (options ,(desk-options))))
                         'heston))
      (check-that "rescale-vol-structure's example returns a vol structure"
                  (typep (run 'rescale-vol-structure 'function "(rescale-vol-structure surface"
                              `((surface ,(ssvi-base)) (theta 0.04d0) (kappa 1.5d0) (v 0.06d0)))
                         'black-vol-structure))
      (check-that "market-at-state's example returns the market on its date"
                  (let ((plan (fincl::make-simulation-plan (make-gbm) m (list d) :n-paths 4)))
                    (date= d (valuation-date (run 'market-at-state 'function "(market-at-state m plan"
                                                  `((m ,m) (plan ,plan) (states ,(vector (coerce '(101d0) 'fincl:double-vector)))))))))
      (check-that "simulate's :derive example (a vol following the spot) sets the vol"
                  (let* ((clause (docstring-form 'simulate 'function "(:derive (:vol :spx) ()"))
                         (r (eval `(simulate ((make-gbm) ,m (list ,d) :n-paths 8)
                                     (:state s :spot)
                                     ,clause
                                     (:final (:gap (- (black-vol (market-view market :underlying :spx) 100 #D"2027-09-24")
                                                      (* 0.002d0 s))))))))
                    (every (lambda (x) (< (abs x) 1d-15)) (simulation-values r :gap))))
      (check-that "simulate's swap exposure example records :exposure and :discounted and outputs :pv"
                  (let* ((hw-market (hw-sim-market))
                         (r (eval `(let ((market ,hw-market)
                                         (dates ',(mapcar (lambda (n) (date+ *today* (* 30 n))) '(1 2 3 4 5 6)))
                                         (swap ,(euribor-swap :payer 0.03d0 #D"2026-12-29" #D"2028-12-29"))
                                         (index ,(euribor #T"6M" :forecast-curve :eur)))
                                     (declare (ignorable market dates swap index))
                                     ,(subst 200 5000 (docstring-form 'simulate 'function "(simulate ((make-hull-white"))))))
                    (and (equal '(:pv) (simulation-output-names r))
                         (equal '(:discounted :exposure) (simulation-recorded-names r))
                         (simulation-quantiles-p r :exposure)))))))

(defun reference-example-checks ()
  (format t "~&Simulated components: the brief's reference examples~%")
  ;; 1. Equity P&L of a held stock and a short call, unchanged.
  (let* ((m (desk-market (make-instance 'flat-black-vol :vol 0.2d0)))
         (dates (sim-quarterly-dates))
         (call (make-option (call 100) (expires #D"2027-09-24") (underlying :spx)))
         (r (simulate ((make-heston) m dates :n-paths 500)
              (:portfolio book (make-portfolio "b" (make-position (make-holding :underlying :spx) :quantity 100)
                                               (make-position call :quantity -1))
                          :record (:value)))))
    (check-that "example 1: a :portfolio run gives the book's :value output and records (:book :value)"
                (and (equal '(:value) (simulation-output-names r))
                     (equal '((:book :value)) (simulation-recorded-names r)))))
  ;; 2. Swap exposure under Hull-White, with fixings.
  (let* ((m (make-market :valuation-date *today* :curves '((:sofr :rate 0.04d0)) :discounting '((:usd . :sofr))))
         (swap (make-ois-swap :payer 1d6 (make-schedule #D"2026-09-28" #D"2027-03-29" #T"3M" :calendar +united-states-sofr+)
                              0.04d0 +actual-360+ (sofr)))
         (monthly-dates (mapcar (lambda (n) (add-tenor *today* (make-tenor n :months))) '(1 2 3 4 5)))
         (r (simulate ((make-hull-white :a 0.03d0 :sigma 0.008d0) m
                       (sort (append monthly-dates (fixing-dates swap m)) #'date<)
                       :n-paths 200 :fix (list (sofr)) :keep :quantiles)
              (:every (date market)
                (record :exposure (max 0d0 (swap-npv swap market)))
                (record :discounted (* (path-discount) (swap-npv swap market))))
              (:final (:pv (* (path-discount) (swap-npv swap market)))))))
    (check-that "example 2: a SOFR swap's exposure profile with :fix, EPE and PFE by date"
                (and (simulation-quantiles-p r :exposure)
                     (every (lambda (x) (>= x 0d0)) (subseq (simulation-statistic r :exposure :mean) 1))
                     (= 1 (length (simulation-output-names r))))))
  ;; 3 and 5 run in DERIVED-SURFACE-CHECKS and DESK-CHECKS (EXAMPLE-3-RUN, EXAMPLE-5-RUN).
  ;; 4. An Ibor curve at a basis over a simulated OIS curve, and a book on both.
  (let* ((m (make-market :valuation-date *today*
                         :underlyings '((:spx :spot 100 :currency :eur :vol 0.2d0))
                         :curves '((:eur-ois :rate 0.03d0) (:eur-6m :rate 0.0315d0))
                         :discounting '((:eur . :eur-ois))))
         (swap (make-vanilla-swap :payer 1d6 (make-schedule #D"2026-12-29" #D"2029-12-28" #T"1Y" :calendar +target+)
                                  0.032d0 (make-instance 'thirty-360 :variant :bond-basis)
                                  (make-schedule #D"2026-12-29" #D"2029-12-28" #T"6M" :calendar +target+)
                                  (euribor #T"6M" :forecast-curve :eur-6m)))
         (call (make-option (call 100) (expires #D"2027-09-24") (underlying :spx)))
         (book (make-portfolio "b" (make-position swap :engine :discounting) (make-position call)))
         (dates (list #D"2026-11-24"))
         (r (simulate ((((:curve :eur-ois) (make-hull-white)) ((:spot :spx) (make-gbm))) m dates :n-paths 50 :keep (:paths 2))
              (:derive (:curve :eur-6m) (market)
                (bump-structure (market-component market '(:curve :eur-ois)) '(:absolute 0.0015d0)))
              (:every (date market) (record :book (price book :auto market))))))
    (check-that "example 4: the derived 6M curve is the simulated OIS curve's zero rates plus 15bp on a stored path"
                (let* ((market (cdr (a:lastcar (path-markets r 0))))
                       (d (years-after #D"2026-11-24" 2))
                       (z (lambda (id) (curve-zero-rate (market-component market `(:curve ,id)) d))))
                  (< (abs (- (funcall z :eur-6m) (funcall z :eur-ois) 0.0015d0)) 1d-6)))
    (check-that "... and the book (swap by :discounting, call by :auto) is priced on every path"
                (= 50 (aref (simulation-statistic r :book :count) 1)))))

(defparameter *jargon* '("martingale" "modulator" "hybrid drift" "convexity" "Jensen" "degenerate" "unhedgeable"))

(defun simulated-components-manual-text ()
  "The text of the manual's @SIMULATED-COMPONENTS section, from docgen/docs.lisp."
  (let* ((text (a:read-file-into-string (asdf:system-relative-pathname "fincl" "docgen/docs.lisp")))
         (start (search "(defsection @simulated-components" text)))
    (and start (subseq text start (or (search "(defsection" text :start2 (1+ start)) (length text))))))

(defun tone-checks ()
  (format t "~&Simulated components: tone of the new documentation~%")
  (let ((texts `(,@(mapcar (lambda (s) `(,s ,(documentation s 'function)))
                           '(simulate market-at-state map-simulated-paths market-component process-targets
                             structure-at-state fixing-dates surface-quotes rescale-vol-structure
                             simulation-state-names))
                 (@simulated-components ,(simulated-components-manual-text))))
        (hits '()))
    (iter (for (name text) in texts)
          (when text
            (dolist (word *jargon*)
              (let ((at (search word text :test #'char-equal)))
                (when at
                  (push `(,name ,word ,(subseq text (max 0 (- at 60)) (min (length text) (+ at 60)))) hits))))))
    (if hits
        (dolist (h hits)
          (format t "~&  review ~(~A~): ~S in ...~A...~%" (first h) (second h) (substitute #\Space #\Newline (third h))))
        (check-that (format nil "no unglossed jargon (~{~A~^, ~}) in the new docstrings or the manual section" *jargon*)
                    t)))
  (let ((manual (simulated-components-manual-text)))
    (check-that "the manual's pitfalls each appear with a numeric example"
                (and manual
                     (iter (for title in '("*Rates move together.*" "*The stock does not feel the rates.*"
                                           "*A derived surface keeps today's shape.*" "*A refit changes the desk, not the world.*"))
                           (for at = (search title manual))
                           (for end = (and at (or (search (format nil "~%- *") manual :start2 (1+ at))
                                                  (search "**Glossary.**" manual :start2 at))))
                           (always (and at end
                                        (or (some #'digit-char-p (subseq manual at end))
                                            (search "bit for bit" (subseq manual at end))))))))
    (check-that "the manual defines world, desk, target and derived component in its glossary"
                (and manual (every (lambda (w) (search w manual))
                                   '("- *World*" "- *Desk*" "- *Target*" "- *Derived component*"))))))

(define-checks simulation-component-checks
  (spot-only-fixture-checks)
  (simulation-target-checks)
  (hw-martingale-checks)
  (hw-instrument-checks)
  (vasicek-protocol-checks)
  (fixing-checks)
  (rescale-checks)
  (derived-surface-checks)
  (surface-quote-checks)
  (desk-checks)
  (docstring-example-checks)
  (reference-example-checks)
  (tone-checks))

(defun run-simulation-component-tests ()
  "Run only the simulated market component checks (task 32). Returns T when
all pass."
  (with-pricing-kernel ()
    (run-suite-list '(simulation-component-checks))))
