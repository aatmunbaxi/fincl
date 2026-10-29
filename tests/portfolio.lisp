;;;; portfolio.lisp --- holdings, portfolios, scenarios and risk (task 22)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun two-underlying-market ()
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 5000 :currency :usd :dividend 0.015d0 :vol 0.18d0)
                              (:ndx :spot 18000 :currency :usd :vol 0.22d0))
               :curves '((:usd-ois :rate 0.04d0))))

(defun spx-option (kind strike &rest clauses)
  (make-option-from-clauses `((,kind ,strike) ,@clauses (:underlying :spx)
                              ,@(unless (find :expires clauses :key #'first)
                                  `((:expires ,#D"2027-09-24"))))))

;;; --------------------------------------------------------------------
;;; Phase 1: holdings, portfolios, engine selection
;;; --------------------------------------------------------------------

(defun holding-checks ()
  (format t "~&Portfolios: holdings and positions~%")
  (let* ((m (two-underlying-market))
         (h (make-holding :underlying :ndx)))
    (check-that "a holding prices at its underlying's spot under any designator"
                (every (lambda (engine) (= 18000d0 (price h engine m)))
                       `(:auto :black-scholes :cos ,(make-engine :mc) ,(make-heston))))
    (check-that "... and on the underlying's view" (= 18000d0 (price h :fd (market-view m :underlying :ndx))))
    (check-that "its Greeks are delta 1 and zero otherwise"
                (equal (greeks h :auto m) '(:delta 1d0 :gamma 0d0 :vega 0d0 :theta 0d0 :rho 0d0
                                            :dividend-rho 0d0 :vanna 0d0 :volga 0d0)))
    (check-error "a holding paying in another currency is a quanto"
                 (price (make-holding :underlying :ndx :currency :eur) :auto m) unsupported-quanto))
  (let* ((m (two-underlying-market))
         (call (spx-option :call 5000))
         (put (spx-option :put 4800 '(:american)))
         (flat (make-portfolio "flat" (make-position call :quantity 1) (make-position call :quantity -1)))
         (net (make-portfolio "net" (make-position call :quantity 2) (make-position call :quantity -1)))
         (mixed (make-portfolio "mixed"
                                (make-position call :quantity 3 :tags '(:desk :a))
                                (make-portfolio "puts" (make-position put :quantity -2 :tags '(:desk :b)))
                                (make-position (make-holding :underlying :spx) :quantity -1 :tags '(:desk :a)))))
    (check-that "long 1 and short 1 of one option are worth 0 (=)" (= 0d0 (price flat :auto m)))
    (check "long 2 and short 1 are worth the option" (price call :auto m) (price net :auto m) 1d-12)
    (let ((long (price (portfolio-long mixed) :auto m))
          (short (price (portfolio-short mixed) :auto m)))
      (check-that "long and short books split the positions"
                  (and (= 1 (length (portfolio-positions (portfolio-long mixed))))
                       (= 2 (length (portfolio-positions (portfolio-short mixed))))))
      (check "long + short values = the book's" (price mixed :auto m) (+ long short) 1d-9))
    (check-that "positions grouped by tag"
                (equal (mapcar (lambda (g) (cons (car g) (length (portfolio-positions (cdr g)))))
                               (portfolio-by-tag mixed :desk))
                       '((:a . 2) (:b . 1))))
    (let* ((delta (getf (greeks call :black-scholes m :which '(:delta)) :delta))
           (hedged (make-portfolio "hedged" (make-position call)
                                   (make-position (make-holding :underlying :spx) :quantity (- delta)))))
      (check "delta-hedged book: closed-form delta 0" 0d0
             (getf (greeks hedged :black-scholes m :which '(:delta)) :delta) 1d-12)
      (check "delta-hedged book: bumped delta 0" 0d0
             (getf (greeks hedged :black-scholes m :which '(:delta) :method :bump) :delta) 1d-6))
    (check-error "a position needs an instrument" (make-position 42) invalid-argument)
    (check-error "a position's tags are a plist" (make-position call :tags '(:desk)) invalid-argument)))

(defun engine-introspection-checks ()
  (format t "~&Portfolios: engines that can price an instrument~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (european (make-option (call 100) (expires #D"2027-09-24")))
         (keys (mapcar #'fincl::engine-designator-key (applicable-engines european m))))
    (format t "~&  info European GBM: ~S~%" keys)
    (check-that "European GBM: Black-Scholes, COS and Monte Carlo, in that order"
                (let ((bs (position :black-scholes keys)) (cos (position :cos keys)) (mc (position :mc keys)))
                  (and bs cos mc (< bs cos mc))))
    (check-that "European GBM: FD applies, the tree does not"
                (and (member :fd keys) (not (member :binomial keys))))
    (check-that "select-engine is the first" (typep (select-engine european m) 'black-scholes-engine))
    (let* ((american (make-option (put 100) (american) (expires #D"2027-09-24")))
           (engines (applicable-engines american m :process (make-heston))))
      (format t "~&  info Heston American: ~S~%" (mapcar #'fincl::engine-designator-key engines))
      (check-that "Heston American: only engines that price it (each one priced)"
                  (and engines
                       (every (lambda (e) (realp (handler-case (price american e m) (error () nil))))
                              engines)
                       (notany (lambda (e) (typep e '(or cos-engine binomial-engine analytic-engine)))
                               engines))))))

(defun mixed-book (n)
  "N positions cycling through Europeans, Americans, Bermudans, barriers,
Asians, holdings, under several engines, long and short."
  (let ((dates (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24")))
    (apply #'make-portfolio "mixed"
           (iter (for i below n)
                 (for k = (+ 4500 (* 50 (mod i 20))))
                 (for q = (if (evenp i) (1+ (mod i 7)) (- (1+ (mod i 5)))))
                 (collect
                     (make-position
                      (case (mod i 6)
                        (0 (spx-option :call k))
                        (1 (spx-option :put k '(:american)))
                        (2 (spx-option :put k `(:bermudan :dates ,dates)))
                        (3 (spx-option :call k '(:barrier :up-and-out 6000 :monitoring :continuous)))
                        (4 (spx-option :call k `(:average :arithmetic :dates ,dates)))
                        (t (make-holding :underlying :spx)))
                      :quantity q
                      :engine (case (mod i 4) (0 nil) (1 :fd) (2 :cos) (t '(:mc :n-paths 4000)))
                      :tags `(:desk ,(if (< (mod i 10) 5) :a :b))))))))

(defun portfolio-pricing-checks ()
  (format t "~&Portfolios: pricing a book~%")
  (let* ((m (two-underlying-market))
         (book (mixed-book 200))
         (results (iter (for workers in '(1 8))
                        (collect (let ((lp:*kernel* (lp:make-kernel workers)))
                                   (unwind-protect (price-portfolio book m)
                                     (lp:end-kernel :wait t)))))))
    (multiple-value-bind (value se unpriced) (portfolio-value (first results))
      (format t "~&  info 200 positions: ~,4F +- ~,4F, ~D unpriced~%" value se (length unpriced))
      (check-that "a mixed 200-position book prices every position" (null unpriced))
      (check-that "... with declines recovered by falling back (COS on barriers and Asians, ...)"
                  (some #'position-result-condition (coerce (portfolio-result-results (first results)) 'list))))
    (check-that "identical results with 1 and 8 workers (=)"
                (every (lambda (a b) (and (eql (position-result-unit-price a) (position-result-unit-price b))
                                          (eql (position-result-standard-error a)
                                               (position-result-standard-error b))))
                       (portfolio-result-results (first results))
                       (portfolio-result-results (second results))))))

;;; --------------------------------------------------------------------
;;; Phase 2: scenarios and risk
;;; --------------------------------------------------------------------

(defun symmetric-eigen-checks ()
  (format t "~&Symmetric eigendecomposition (Jacobi)~%")
  (let* ((n 12)
         (state (sb-ext:seed-random-state 11))
         (a (make-array (* n n) :element-type 'double-float)))
    (dotimes (j n) (dotimes (i (1+ j))
                     (let ((x (- (random 2d0 state) 1d0)))
                       (setf (aref a (+ i (* j n))) x (aref a (+ j (* i n))) x))))
    (multiple-value-bind (lambdas vectors) (fincl::symmetric-eigen a n)
      (let ((residual 0d0) (orthogonality 0d0))
        (dotimes (k n)
          (dotimes (i n)
            (setf residual (max residual
                                (abs (- (iter (for j below n) (sum (* (aref a (+ i (* j n))) (aref vectors (+ j (* k n))))))
                                        (* (aref lambdas k) (aref vectors (+ i (* k n)))))))))
          (dotimes (l n)
            (setf orthogonality (max orthogonality
                                     (abs (- (iter (for i below n) (sum (* (aref vectors (+ i (* k n)))
                                                                            (aref vectors (+ i (* l n))))))
                                             (if (= k l) 1 0)))))))
        (check-that (format nil "A v = lambda v to 1e-13 (~,1E), V'V = I to 1e-13 (~,1E), ascending"
                            residual orthogonality)
                    (and (< residual 1d-13) (< orthogonality 1d-13)
                         (every #'<= lambdas (subseq lambdas 1))))))
    (let* ((c (make-array 4 :element-type 'double-float :initial-contents '(1d0 1.2d0 1.2d0 1d0)))
           (fixed (repair-positive-semidefinite c 2)))
      (check-that "repair: an indefinite correlation becomes semidefinite and stays symmetric"
                  (and (>= (aref (fincl::symmetric-eigen fixed 2) 0) -1d-15)
                       (= (aref fixed 1) (aref fixed 2)))))))

(defun scenario-risk-checks ()
  (format t "~&Risk: scenarios, VaR, ladders~%")
  (let* ((m (two-underlying-market))
         (holding (make-portfolio "stock" (make-position (make-holding :underlying :spx) :quantity 10)))
         (sigma 0.02d0)
         (n 2000)
         (z (let ((state (sb-ext:seed-random-state 5)))
              (iter (repeat n)
                    (collect (* sigma (norm-quantile (+ 1d-12 (random (- 1d0 2d-12) state))))))))
         (scenarios (historical-scenarios m `(((:spot :spx) :relative ,z))))
         (pnl (scenario-pnl (revalue holding m scenarios)))
         (exact (* 10 5000 sigma (norm-quantile 0.99d0)))
         ;; The empirical quantile's standard error: sqrt(p (1 - p) / n) / phi(z).
         (bound (* 3 10 5000 sigma (/ (sqrt (/ (* 0.01d0 0.99d0) n)) (norm-pdf (norm-quantile 0.99d0))))))
    (check (format nil "historical 99% VaR of a linear position vs the normal quantile (3 SE = ~,1F)" bound)
           exact (historical-var pnl 0.99d0) bound)
    (check-that "expected shortfall >= VaR" (>= (expected-shortfall pnl 0.99d0) (historical-var pnl 0.99d0)))
    (check-that "revaluation is linear in the scenario for a holding"
                (< (abs (- (aref pnl 0) (* 10 5000 (first z)))) 1d-8))
    (multiple-value-bind (var s)
        (delta-normal-var holding m '(((:spot :spx) :relative)) (make-array '(1 1) :initial-element (* sigma sigma))
                          0.99d0)
      (check-that (format nil "delta-normal VaR of a linear book = z sigma S q to 1e-12 relative (~,2E)"
                          (/ (abs (- var exact)) exact))
                  (< (/ (abs (- var exact)) exact) 1d-12))
      (check "its sensitivity is S q" 50000d0 (aref s 0) 1d-8))
    (let* ((call (spx-option :call 5000))
           (book (make-portfolio "calls" (make-position call :quantity 3)))
           (grid (spot-vol-grid :spx '(-1/10 0 1/10) '(-5/100 0 5/100)))
           (pnl (revalue book m grid)))
      (check-that "a spot-vol grid has 9 scenarios, P&L 0 in the unmoved one"
                  (and (= 9 (array-dimension pnl 1)) (< (abs (aref pnl 0 4)) 1d-10)))
      (check "the up-spot scenario P&L is 3 (C(1.1 S) - C(S))"
             (* 3 (- (price call :auto (bump m '(:spot :spx) '(:relative 1/10))) (price call :auto m)))
             (aref pnl 0 7) 1d-10)
      (let ((ladder (vega-ladder book m (list #T"6M" #T"2Y"))))
        (check "vega ladder: the one-year call's vega falls in the 2Y bucket"
               (* 3 (getf (greeks call :auto m :which '(:vega)) :vega)) (cdr (second ladder)) 1d-10))))
  ;; Curve-delta ladder on a bootstrapped curve.
  (let* ((today *today*)
         (helpers (list (ois-helper 0.04d0 #T"1Y" (sofr) :valuation-date today)
                        (ois-helper 0.041d0 #T"2Y" (sofr) :valuation-date today)
                        (ois-helper 0.042d0 #T"5Y" (sofr) :valuation-date today)))
         (curve (bootstrap-curve helpers))
         (m (make-market :valuation-date today
                         :underlyings '((:spx :spot 5000 :currency :usd :vol 0.2d0))
                         :curves `((:sofr :curve ,curve))))
         (book (make-portfolio "rates" (make-position (spx-option :call 5000 `(:expires ,#D"2029-09-24")) :quantity 5)))
         (ladder (curve-delta-ladder book m :sofr))
         (parallel (aref (factor-sensitivities book m '(((:curve :sofr) :absolute)) :step 1d-4) 0)))
    ;; Relative: per unit of rate the deltas are ~4e4 (4 per basis point).
    (check-that (format nil "curve-delta ladder sums to the parallel-shift delta to 1e-6 relative (~,1E)"
                        (abs (/ (- (reduce #'+ (mapcar #'cdr ladder)) parallel) parallel)))
                (< (abs (/ (- (reduce #'+ (mapcar #'cdr ladder)) parallel) parallel)) 1d-6))))

;;; --------------------------------------------------------------------
;;; Phase 3: P&L explain and snapshots
;;; --------------------------------------------------------------------

(defun explain-checks ()
  (format t "~&Risk: P&L explain~%")
  (let* ((m (make-market :valuation-date *today*
                         :underlyings '((:spx :spot 5000 :currency :usd :dividend 0.015d0 :vol 0.18d0))
                         :curves '((:usd-ois :rate 0.04d0))))
         (book (make-portfolio "europeans"
                               (make-position (make-option (call 5000) (expires #D"2027-09-24")) :quantity 3)
                               (make-position (make-option (put 4500) (expires #D"2027-03-24")) :quantity -5)
                               (make-position (make-option (call 5500) (expires #D"2028-09-24")) :quantity 2)
                               (make-position (make-holding) :quantity -1)))
         (residuals
           (iter (for scale in '(1 1/2))
                 (for moved = (bump (bump m '(:spot :spx) `(:relative ,(* scale 1/200)))
                                    '(:vol :spx) `(:absolute ,(* scale 2/1000))))
                 (for e = (explain-pnl book m moved))
                 (when (= scale 1)
                   (check "explained + unexplained = the full revaluation"
                          (getf e :actual) (+ (getf e :explained) (getf e :unexplained)) 1d-9))
                 (collect (abs (getf e :unexplained)))))
         (ratio (/ (first residuals) (second residuals))))
    (check-that (format nil "halving a spot and vol move cuts the unexplained P&L by a factor in [6, 10] (~,2F)"
                        ratio)
                (<= 6 ratio 10))
    (let ((aged (explain-pnl book m (derive-market m :valuation-date (date+ *today* 1)))))
      (check "one day's aging is explained by theta to the second order"
             (getf aged :actual) (getf aged :theta) (* 1d-2 (abs (getf aged :actual)))))))

(defun snapshot-checks ()
  (format t "~&Snapshots: markets and portfolios as forms~%")
  (let* ((m (make-market :valuation-date *today*
                         :underlyings `((:spx :spot 5000 :currency :usd :dividend 0.01d0
                                              :dividend-schedule (("2027-01-15" 20d0)
                                                                  ("2027-07-15" 0.01d0 :kind :proportional))
                                              :vol ,(make-grid-vol-surface
                                                     '(0.5d0 1d0 2d0) '(-0.2d0 0d0 0.2d0)
                                                     '((0.25d0 0.2d0 0.21d0) (0.24d0 0.2d0 0.205d0)
                                                       (0.23d0 0.2d0 0.2d0))))
                                        (:ndx :spot 18000 :currency :usd
                                              :vol ,(make-ssvi-surface '(0.25 0.5 1 2) '(0.01 0.02 0.04 0.08)
                                                                       :rho -0.4 :eta 1 :gamma 0.4)))
                         :curves `((:usd-ois :curve ,(make-instance 'interpolated-zero-curve
                                                                    :dates (list *today* #D"2027-09-24" #D"2031-09-24")
                                                                    :zero-rates '(0.04d0 0.041d0 0.043d0))))
                         :discounting '((:usd . :usd-ois))
                         :fixings '((:sofr ("2026-09-23" 0.0405d0)))))
         (book (make-portfolio "book"
                               (make-position (make-option (call 5000) (expires #D"2027-09-24") (underlying :spx))
                                              :quantity 3 :tags '(:desk :a))
                               (make-position (make-option (put 18000) (american) (expires #D"2027-09-24")
                                                           (underlying :ndx))
                                              :quantity -2 :engine :fd)
                               (make-portfolio "hedge" (make-position (make-holding :underlying :spx) :quantity -1))))
         (round-trip (lambda (x) (with-input-from-string (s (with-output-to-string (s) (write-snapshot x s)))
                                   (read-snapshot s))))
         (m2 (funcall round-trip m))
         (book2 (funcall round-trip book)))
    (check-that "a market with curves, surfaces and dividends, and a book, round-trip with = prices"
                (= (portfolio-value (price-portfolio book m)) (portfolio-value (price-portfolio book2 m2))))
    (check-that "the round-tripped book keeps its tree, quantities, engines and tags"
                (equal (mapcar (lambda (p) `(,(position-quantity p) ,(position-engine p) ,(position-tags p)))
                               (portfolio-positions book))
                       (mapcar (lambda (p) `(,(position-quantity p) ,(position-engine p) ,(position-tags p)))
                               (portfolio-positions book2))))
    (check-that "a market prints readably as its snapshot form"
                (search "MAKE-MARKET" (let ((*print-readably* t) (*package* (find-package :fincl)))
                                        (prin1-to-string m))))
    (check-error "a snapshot calls only its constructors"
                 (with-input-from-string (s "(delete-file \"x\")") (read-snapshot s)) invalid-argument)
    (check-error "a snapshot does not read-eval"
                 (with-input-from-string (s "#.(delete-file \"x\")") (read-snapshot s)) reader-error)))

(defun portfolio-checks ()
  (holding-checks)
  (engine-introspection-checks)
  (portfolio-pricing-checks)
  (symmetric-eigen-checks)
  (scenario-risk-checks)
  (explain-checks)
  (snapshot-checks))

(defun run-portfolio-tests ()
  "Run only the portfolio and risk checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (portfolio-checks))
    (format t "~&~[All portfolio checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
