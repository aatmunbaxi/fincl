;;;; risk.lisp --- scenarios, VaR, ladders, P&L explain (task 22)
;;;;
;;;; A scenario is a set of risk-factor bumps (BUMP) and an optional aging:
;;;; applied to a market it gives another market, so revaluation is pricing
;;;; on many markets. Each position's engine is chosen once, on the base
;;;; market, and reused on every scenario.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Scenarios
;;; --------------------------------------------------------------------

(defstruct (scenario (:constructor %make-scenario (name bumps age)))
  "NAME, BUMPS (a list of (factor shift), as BUMP takes them) and AGE (NIL,
a number of calendar days, or a date to move the valuation date to)."
  (name nil :read-only t)
  (bumps '() :type list :read-only t)
  (age nil :read-only t))

(defun make-scenario (name &key bumps age)
  "A SCENARIO called NAME: BUMPS, a list of (factor shift) applied in order
(see BUMP), then AGE, calendar days to age the market by or a date.

  (make-scenario \"spot -10%, vol +5\" :bumps '(((:spot :spx) (:relative -1/10))
                                               ((:vol :spx) (:absolute 5/100))))"
  (check-input bumps list "a list of (factor shift)")
  (check-input age (or null integer date) "NIL, a number of days, or a date")
  (%make-scenario name bumps age))

(defmethod print-object ((s scenario) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~S~@[ ~{~S~^ ~}~]~@[ aged ~A~]" (scenario-name s) (scenario-bumps s) (scenario-age s))))

(defun apply-scenario (scenario market)
  "The market SCENARIO turns the composite MARKET into."
  (let ((m (reduce (lambda (m b) (bump m (first b) (second b))) (scenario-bumps scenario)
                   :initial-value market)))
    (etypecase (scenario-age scenario)
      (null m)
      (integer (derive-market m :valuation-date (date+ (valuation-date m) (scenario-age scenario))))
      (date (derive-market m :valuation-date (scenario-age scenario))))))

(defun parallel-shift-scenarios (factor shifts &key (kind :absolute))
  "One scenario per shift of SHIFTS applied to FACTOR as (KIND shift).

  (parallel-shift-scenarios '(:curve :usd-ois) '(-1/100 -1/1000 1/1000 1/100))"
  (iter (for x in shifts)
        (collect (make-scenario (format nil "~S ~(~A~) ~A" factor kind x)
                                :bumps `((,factor (,kind ,x)))))))

(defun spot-vol-grid (underlying spot-shifts vol-shifts)
  "The scenarios of every relative spot shift in SPOT-SHIFTS combined with
every absolute vol shift in VOL-SHIFTS for UNDERLYING, spot shifts varying
slowest.

  (spot-vol-grid :spx '(-1/10 0 1/10) '(-5/100 0 5/100))  ; 9 scenarios"
  (iter outer (for s in spot-shifts)
        (iter (for v in vol-shifts)
              (in outer (collect (make-scenario (format nil "spot ~A vol ~A" s v)
                                                :bumps `(((:spot ,underlying) (:relative ,s))
                                                         ((:vol ,underlying) (:absolute ,v)))))))))

(defun historical-scenarios (market history)
  "One scenario per observation of HISTORY, a list of (factor kind changes):
FACTOR as BUMP takes it, KIND :RELATIVE (returns) or :ABSOLUTE (differences),
CHANGES a sequence of the factor's observed changes, every factor's of the
same length. Scenario i applies change i of every factor. MARKET is used to
check the factors.

  (historical-scenarios m `(((:spot :spx) :relative ,spx-returns)
                            ((:curve :usd-ois) :absolute ,rate-changes)))"
  (check-input history (and list (not null)) "a non-empty list of (factor kind changes)")
  (let ((n (length (third (first history)))))
    (iter (for (factor kind changes) in history)
          (check-input kind (member :relative :absolute) ":RELATIVE or :ABSOLUTE")
          (unless (= (length changes) n)
            (signal-invalid-argument 'history history 'list
                                     (format nil "every factor's changes of length ~D (~S has ~D)"
                                             n factor (length changes))))
          ;; A bad factor fails here, not in the middle of a revaluation.
          (bump market factor `(,kind 0)))
    (iter (for i below n)
          (collect (make-scenario (format nil "historical ~D" i)
                                  :bumps (iter (for (factor kind changes) in history)
                                               (collect `(,factor (,kind ,(elt changes i))))))))))

;;; --------------------------------------------------------------------
;;; Revaluation
;;; --------------------------------------------------------------------

(defun resolved-positions (portfolio market engine)
  "(values positions base-prices engines conditions): each position of
PORTFOLIO with its base unit price and the engine that priced it on MARKET
(NIL and the condition if it could not be priced)."
  (let ((results (portfolio-result-results (price-portfolio portfolio market :engine engine))))
    (values (map 'list #'position-result-position results)
            (map 'list #'position-result-unit-price results)
            (map 'list #'position-result-engine results)
            (map 'list #'position-result-condition results))))

(defun revalue (portfolio market scenarios &key (engine :auto))
  "The P&L of PORTFOLIO's positions under SCENARIOS relative to MARKET:
(values pnl unpriced), PNL an N-POSITIONS x N-SCENARIOS array of doubles,
quantity x (price in the scenario - base price), positions in the order of
PORTFOLIO-POSITIONS. Each position is priced with the engine it got on the
base market (its own, or ENGINE by PRICE-PORTFOLIO's rules), so automatic
selection runs once. A position that cannot be priced on the base market or
in a scenario contributes 0 there and is listed in UNPRICED as (position
scenario-name condition). Scenarios run in parallel under lp:*kernel*; the
result does not depend on the number of workers.

  (scenario-pnl (revalue book m (spot-vol-grid :spx '(-1/10 1/10) '(0))))"
  (check-input portfolio portfolio "a portfolio")
  (multiple-value-bind (positions base-prices engines conditions) (resolved-positions portfolio market engine)
    (let* ((n (length positions))
           (scenarios (coerce scenarios 'simple-vector))
           (pnl (make-array `(,n ,(length scenarios)) :element-type 'double-float :initial-element 0d0))
           (unpriced (iter (for p in positions) (for e in engines) (for c in conditions)
                           (unless e (collect `(,p :base ,c)))))
           (columns
             (flet ((column (scenario)
                      (let ((m (apply-scenario scenario market)))
                        (iter (for p in positions) (for base in base-prices) (for e in engines)
                              (collect
                                  (if (null e)
                                      0d0
                                      (handler-case
                                          (* (position-quantity p)
                                             (- (float (price (position-instrument p) e m) 1d0) base))
                                        (error (c) (list p (scenario-name scenario) c)))))))))
               (if lp:*kernel*
                   (lp:pmap 'vector #'column scenarios)
                   (map 'vector #'column scenarios)))))
      (iter (for column in-vector columns) (for j from 0)
            (iter (for x in column) (for i from 0)
                  (if (realp x)
                      (setf (aref pnl i j) x)
                      (push x unpriced))))
      (values pnl (nreverse unpriced)))))

(defun scenario-pnl (pnl)
  "The portfolio's P&L in each scenario: the column sums of a REVALUE matrix,
as a DOUBLE-VECTOR."
  (destructuring-bind (rows columns) (array-dimensions pnl)
    (let ((totals (make-array columns :element-type 'double-float :initial-element 0d0)))
      (dotimes (j columns totals)
        (dotimes (i rows) (incf (aref totals j) (aref pnl i j)))))))

;;; --------------------------------------------------------------------
;;; Historical VaR and expected shortfall
;;; --------------------------------------------------------------------

(defun tail-losses (pnl confidence)
  "The K largest losses of the P&L sample PNL, largest first, with K =
ceiling(n (1 - CONFIDENCE)), at least 1."
  (check-input confidence (real (0) (1)) "a confidence level in (0, 1)")
  (let* ((losses (sort (map 'list #'- pnl) #'>))
         (k (max 1 (ceiling (- (* (length losses) (- 1 confidence)) 1d-9)))))
    (when (null losses)
      (signal-invalid-argument 'pnl pnl 'sequence "a non-empty sample of P&L"))
    (subseq losses 0 (min k (length losses)))))

(defun historical-var (pnl confidence)
  "Value at risk at CONFIDENCE (e.g. 0.99) from the P&L sample PNL (a
sequence, e.g. SCENARIO-PNL): the K-th largest loss, K = ceiling(n (1 -
CONFIDENCE)), as a positive number when it is a loss. With n = 500 and 0.99,
the 5th largest loss.

  (historical-var (scenario-pnl (revalue book m scenarios)) 0.99d0)"
  (float (a:lastcar (tail-losses pnl confidence)) 1d0))

(defun expected-shortfall (pnl confidence)
  "Expected shortfall at CONFIDENCE from the P&L sample PNL: the mean of the
K largest losses, K as in HISTORICAL-VAR."
  (let ((tail (tail-losses pnl confidence)))
    (float (/ (reduce #'+ tail) (length tail)) 1d0)))

;;; --------------------------------------------------------------------
;;; Delta-normal VaR
;;; --------------------------------------------------------------------

(defun factor-sensitivities (portfolio market factors &key (engine :auto) (step 1d-3))
  "dV/dx for each of FACTORS, a list of (factor kind): the change in
PORTFOLIO's value per unit of the factor's change of KIND (:RELATIVE: per
unit return; :ABSOLUTE: per unit move), by central differences of steps of
STEP, as a DOUBLE-VECTOR. Each position keeps its base engine."
  (multiple-value-bind (positions base-prices engines conditions) (resolved-positions portfolio market engine)
    (declare (ignore base-prices))
    (a:when-let ((bad (position nil engines)))
      (error (nth bad conditions)))
    (flet ((value (m)
             (iter (for p in positions) (for e in engines)
                   (sum (* (position-quantity p) (float (price (position-instrument p) e m) 1d0))))))
      (map 'double-vector
           (lambda (f)
             (destructuring-bind (factor kind) f
               (/ (- (value (bump market factor `(,kind ,step)))
                     (value (bump market factor `(,kind ,(- step)))))
                  (* 2 step))))
           factors))))

(defun as-square-matrix (covariance n)
  "COVARIANCE, an N x N 2-D array or a column-major DOUBLE-VECTOR of N^2, as
the latter."
  (etypecase covariance
    (double-vector (unless (= (length covariance) (* n n))
                     (signal-invalid-argument 'covariance covariance 'double-vector
                                              (format nil "~D entries" (* n n))))
                   covariance)
    (array (unless (equal (array-dimensions covariance) `(,n ,n))
             (signal-invalid-argument 'covariance covariance 'array (format nil "an ~D x ~D matrix" n n)))
           (let ((v (make-array (* n n) :element-type 'double-float)))
             (dotimes (j n v) (dotimes (i n) (setf (aref v (+ i (* j n))) (float (aref covariance i j) 1d0))))))))

(defun delta-normal-var (portfolio market factors covariance confidence &key (engine :auto) (step 1d-3))
  "Parametric VaR at CONFIDENCE: z sqrt(s' C s), z the normal quantile, s the
FACTOR-SENSITIVITIES to FACTORS (a list of (factor kind)), C the COVARIANCE
of the factors' changes over the horizon (an n x n 2-D array or column-major
DOUBLE-VECTOR, in the units of each KIND). (values var sensitivities). Exact
for a portfolio linear in the factors whose changes are jointly normal.

  (delta-normal-var book m '(((:spot :spx) :relative)) #2A((0.0004d0)) 0.99d0)"
  (let* ((s (factor-sensitivities portfolio market factors :engine engine :step step))
         (n (length s))
         (c (as-square-matrix covariance n))
         (variance (iter (for j below n)
                         (sum (iter (for i below n)
                                    (sum (* (aref s i) (aref c (+ i (* j n))) (aref s j))))))))
    (when (minusp variance)
      (signal-invalid-argument 'covariance covariance 'array
                               "a positive-semidefinite covariance (see REPAIR-POSITIVE-SEMIDEFINITE)"))
    (values (* (norm-quantile (float confidence 1d0)) (sqrt variance)) s)))

;;; --------------------------------------------------------------------
;;; Sensitivity ladders
;;; --------------------------------------------------------------------

(defun curve-delta-ladder (portfolio market curve-id &key (engine :auto) (step 1d-4))
  "((label . dV/dq) ...): PORTFOLIO's sensitivity to each input quote of the
bootstrapped curve CURVE-ID, per unit of the quote, by central differences
of STEP with the curve rebootstrapped (BUMP (:quote id index)). LABEL is the
helper's tenor, or its index. Their sum is the sensitivity to a parallel
move of every quote, (:curve id), to first order.

  (curve-delta-ladder book m :usd-sofr)"
  (let ((curve (find-yield-curve market curve-id)))
    (unless (typep curve 'piecewise-yield-curve)
      (signal-invalid-argument 'curve-id curve-id 'keyword "a bootstrapped curve, whose quotes can move"))
    (iter (for h in (curve-helpers curve))
          (for i from 0)
          (collect (cons (or (helper-tenor h) i)
                         (aref (factor-sensitivities portfolio market `(((:quote ,curve-id ,i) :absolute))
                                                     :engine engine :step step)
                               0))))))

(defun vega-ladder (portfolio market buckets &key (engine :auto))
  "((label . vega) ...): PORTFOLIO's vega (per unit vol, quantity-weighted)
by the expiry of its options, BUCKETS a list of tenors from the valuation
date; an option falls in the first bucket whose end is on or after its
expiry, and later ones in a last bucket labelled :BEYOND. Other instruments
have no vega.

  (vega-ladder book m (list #T\"3M\" #T\"1Y\" #T\"5Y\"))"
  (let* ((today (valuation-date market))
         (ends (mapcar (lambda (b) (add-tenor today b)) buckets))
         (totals (make-array (1+ (length buckets)) :element-type 'double-float :initial-element 0d0)))
    (dolist (p (portfolio-positions portfolio))
      (let ((o (position-instrument p)))
        (when (typep o 'option)
          (let* ((expiry (expiry (option-exercise o)))
                 (bucket (or (position-if (lambda (end) (date<= expiry end)) ends) (length buckets))))
            (incf (aref totals bucket)
                  (* (position-quantity p)
                     (getf (greeks o (or (position-engine p) engine) market :which '(:vega)) :vega)))))))
    (iter (for label in `(,@buckets :beyond))
          (for v in-vector totals)
          (collect (cons label v)))))

;;; --------------------------------------------------------------------
;;; P&L explain
;;; --------------------------------------------------------------------

(defun position-factor-moves (instrument market-0 market-1)
  "(values dS dsigma dr dq days): the moves between the composite markets
MARKET-0 and MARKET-1 of the factors INSTRUMENT's Greeks refer to: its
underlying's spot, its Black vol at its strike and expiry (0 without an
expiry), the zero rate and dividend yield to its expiry, and the calendar
days between the valuation dates."
  (let* ((v0 (instrument-view market-0 instrument))
         (v1 (instrument-view market-1 instrument))
         (days (- (date-serial (valuation-date market-1)) (date-serial (valuation-date market-0)))))
    (if (typep instrument 'option)
        (let* ((expiry (expiry (option-exercise instrument)))
               (payoff (option-payoff instrument))
               (strike (if (typep payoff 'strike-payoff) (strike payoff) (forward v0 expiry))))
          (values (- (spot v1) (spot v0))
                  (- (black-vol v1 strike expiry) (black-vol v0 strike expiry))
                  (- (zero-rate v1 expiry) (zero-rate v0 expiry))
                  (- (dividend-yield v1 expiry) (dividend-yield v0 expiry))
                  days))
        (values (- (spot v1) (spot v0)) 0d0 0d0 0d0 days))))

(defun explain-pnl (portfolio market-0 market-1 &key (engine :auto))
  "PORTFOLIO's P&L from the composite MARKET-0 to MARKET-1, explained by a
Taylor expansion in the Greeks at MARKET-0, position by position, as a plist:

  :delta  delta dS             :gamma  gamma dS^2 / 2
  :vega   vega dsigma          :vanna  vanna dS dsigma
  :volga  volga dsigma^2 / 2   :theta  theta x days
  :rho    rho dr               :dividend-rho  dividend-rho dq
  :explained (their sum)  :actual (full revaluation)  :unexplained (the rest)

dS is the move of each position's underlying's spot, dsigma of its Black
vol at its strike and expiry, dr and dq of the zero rate and dividend yield
to its expiry, days the calendar days between the valuation dates. Each
position keeps the engine it gets on MARKET-0. Third-order terms, rate
convexity and cross terms with rates are in :UNEXPLAINED.

  (explain-pnl book m (bump m '(:spot :spx) '(:relative 1/100)))"
  (let ((keys '(:delta :gamma :vega :vanna :volga :theta :rho :dividend-rho))
        (totals (make-hash-table))
        (actual 0d0))
    (multiple-value-bind (positions base-prices engines conditions) (resolved-positions portfolio market-0 engine)
      (iter (for p in positions) (for base in base-prices) (for e in engines) (for c in conditions)
            (unless e (error c))
            (let* ((q (position-quantity p))
                   (instrument (position-instrument p))
                   (g (greeks instrument e market-0 :which keys)))
              (multiple-value-bind (ds dsigma dr dq days) (position-factor-moves instrument market-0 market-1)
                (flet ((add (key value) (incf (gethash key totals 0d0) (* q value))))
                  (add :delta (* (getf g :delta) ds))
                  (add :gamma (* 0.5d0 (getf g :gamma) ds ds))
                  (add :vega (* (getf g :vega) dsigma))
                  (add :vanna (* (getf g :vanna) ds dsigma))
                  (add :volga (* 0.5d0 (getf g :volga) dsigma dsigma))
                  (add :theta (* (getf g :theta) days))
                  (add :rho (* (getf g :rho) dr))
                  (add :dividend-rho (* (getf g :dividend-rho) dq))))
              (incf actual (* q (- (float (price instrument e market-1) 1d0) base))))))
    (let ((explained (iter (for key in keys) (sum (gethash key totals 0d0)))))
      `(,@(iter (for key in keys) (appending `(,key ,(gethash key totals 0d0))))
        :explained ,explained :actual ,actual :unexplained ,(- actual explained)))))
