;;;; books.lisp --- books with cash
;;;;
;;;; Accounting for a portfolio carried along a path: cash balances, trades,
;;;; deposits and the income of positions.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defparameter *income-kinds* '(:discrete-dividend :continuous-dividend :coupon :redemption :expiry)
  "The kinds of income POSITION-INCOME reports and a book's :COLLECT names.")

(defstruct (book-accounting (:constructor %make-book-accounting (plan cash-curve collect)))
  "How SIMULATE runs a :PORTFOLIO book holding cash: the PLAN, the curve id
its cash accrues on (CASH-CURVE; NIL: the currency's discount curve), and
the income kinds it COLLECTs."
  (plan nil :read-only t)
  (cash-curve nil :read-only t)
  (collect '() :type list :read-only t))

(defun make-book-accounting (plan cash-curve collect)
  (unless (or (null cash-curve) (keywordp cash-curve))
    (signal-invalid-argument :cash-curve cash-curve 'keyword "the id of a yield curve, or NIL"))
  (let ((kinds (if (eq collect :all) *income-kinds* collect)))
    (unless (and (listp kinds) (every (lambda (k) (member k *income-kinds*)) kinds))
      (signal-invalid-argument :collect collect 'list
                               (format nil ":ALL or a list of income kinds from ~{~S~^ ~}" *income-kinds*)))
    (%make-book-accounting plan cash-curve kinds)))

(defun simulation-book-currency (market)
  "The currency of a book's cash on MARKET: the composite market's only
currency (that of its only discount curve, else of all its underlyings), or
NIL for a market that is not composite."
  (when (typep market 'composite-market)
    (let ((discounted (sorted-keys (market-discount-curve-for market)))
          (underlying (remove-duplicates (iter (for u in (market-underlyings market))
                                               (collect (gethash u (market-underlying-currency market)))))))
      (cond ((= 1 (length discounted)) (first discounted))
            ((= 1 (length underlying)) (first underlying))
            (t (signal-invalid-argument :cash market 'composite-market
                                        (format nil "a market with one currency for :CASH; it has ~{~S~^, ~}: put ~
(make-position (make-cash-account :currency c) :quantity x) in the book's INIT instead"
                                                (union discounted underlying))))))))

(defun book-cash-position (book)
  "BOOK's top-level CASH-ACCOUNT position, or NIL."
  (find-if (lambda (m) (and (typep m 'portfolio-position) (typep (position-instrument m) 'cash-account)))
           (portfolio-members book)))

(defun book-cash-target (accounting position)
  "The curve, (:curve id), the cash of POSITION accrues on (NIL on a market
that is not composite)."
  (let ((market (simulation-plan-market (book-accounting-plan accounting))))
    (cond ((book-accounting-cash-curve accounting) `(:curve ,(book-accounting-cash-curve accounting)))
          ((not (typep market 'composite-market)) nil)
          (t (let* ((currency (or (instrument-currency (position-instrument position))
                                  (simulation-book-currency market)))
                    (id (gethash currency (market-discount-curve-for market))))
               (unless id
                 (signal-invalid-argument :cash-curve nil 'keyword
                                          (format nil "a curve for the cash in ~S, which the market does not ~
discount: give the book :CASH-CURVE id" currency)))
               `(:curve ,id))))))

(defun open-book (book cash accounting)
  "BOOK at the start of a path, with a cash account of balance CASH (a real,
or NIL for none) added. Returns (values book target): TARGET is the cash
curve when the book holds cash, :NONE when it does not."
  (unless (typep book 'portfolio)
    (if cash
        (signal-invalid-argument 'book book 'portfolio "a PORTFOLIO as the book's INIT")
        (return-from open-book (values book :none))))
  (let ((existing (book-cash-position book)))
    (when (and (null existing) (find-if (lambda (p) (typep (position-instrument p) 'cash-account))
                                        (portfolio-positions book)))
      (signal-invalid-argument 'book book 'portfolio "a book holding its cash account at the top level, not in a sub-portfolio"))
    (cond ((and cash existing)
           (signal-invalid-argument :cash cash 'real
                                    "either :CASH or a cash account in the book's INIT, not both"))
          (cash
           (check-input cash real "a real cash balance")
           (let* ((market (simulation-plan-market (book-accounting-plan accounting)))
                  (position (make-position (make-cash-account :currency (simulation-book-currency market))
                                           :quantity cash)))
             (values (apply #'make-portfolio (portfolio-name book) `(,@(portfolio-members book) ,position))
                     (book-cash-target accounting position))))
          (existing (values book (book-cash-target accounting existing)))
          (t (values book :none)))))

(defun book-balance (book)
  "The cash balance of BOOK, which must hold a cash account."
  (position-quantity (book-cash-position book)))

(defun set-book-balance (book balance)
  "A copy of BOOK whose cash balance is BALANCE."
  (let ((cash (book-cash-position book)))
    (apply #'make-portfolio (portfolio-name book)
           (substitute (derive-position cash :quantity balance) cash (portfolio-members book)))))

(defun check-book-cash (book operator)
  (unless (and (typep book 'portfolio) (book-cash-position book))
    (signal-invalid-argument operator book 'portfolio
                             (format nil "a book holding cash for ~(~A~): give the :PORTFOLIO clause :CASH x ~
(or a cash account in its INIT)" operator))))

(defun book-cash-flow (book amount operator)
  "BOOK with AMOUNT (a non-negative real) added to its cash by DEPOSIT (OPERATOR
DEPOSIT) or taken from it (WITHDRAW)."
  (check-book-cash book operator)
  (unless (and (realp amount) (>= amount 0))
    (signal-invalid-argument operator amount 'real
                             (format nil "a non-negative real to ~(~A~)" operator)))
  (set-book-balance book (if (eq operator 'deposit)
                             (+ (book-balance book) (float amount 1d0))
                             (- (book-balance book) (float amount 1d0)))))

(defun book-trade (book instrument market-fn default-engine &key by to engine)
  "(values book cost): BOOK after buying or selling INSTRUMENT to a quantity
TO, or BY a change, at its unit price on the market MARKET-FN returns, paid
from the book's cash; COST is the cash paid (negative for a sale). A
position whose quantity becomes 0 is removed. The price
uses ENGINE, else the position's engine, else DEFAULT-ENGINE, with
PRICE-POSITION's fallbacks; a price that fails signals its condition."
  (check-book-cash book 'trade)
  (check-input instrument instrument "an instrument")
  (when (typep instrument 'cash-account)
    (signal-invalid-argument 'trade instrument 'instrument
                             "an instrument other than cash (DEPOSIT and WITHDRAW move cash)"))
  (unless (and (or by to) (not (and by to)) (realp (or by to)))
    (signal-invalid-argument 'trade (or by to) 'real "exactly one of :BY and :TO, a real"))
  (let* ((old (find-if (lambda (m) (and (typep m 'portfolio-position)
                                        (same-instrument-p (position-instrument m) instrument)))
                       (portfolio-members book)))
         (q0 (if old (position-quantity old) 0d0))
         (q1 (if to (float to 1d0) (+ q0 (float by 1d0))))
         (dq (- q1 q0))
         ;; A position sold to zero leaves the book (ADJUST-POSITION keeps it).
         (traded (if (zerop q1)
                     (apply #'make-portfolio (portfolio-name book) (remove old (portfolio-members book)))
                     (adjust-position book instrument :to q1))))
    (if (zerop dq)
        (values traded 0d0)
        (let* ((result (price-position (make-position instrument :engine (or engine (and old (position-engine old))))
                                       (funcall market-fn) default-engine #'default-fallback-policy))
               (unit (position-result-unit-price result)))
          (unless unit
            (error (or (position-result-condition result)
                       (make-condition 'unsupported-combination :exercise nil :payoff instrument :path nil
                                                                :engine default-engine :market nil))))
          (let ((cost (* dq unit)))
            (values (set-book-balance traded (- (book-balance book) cost)) cost))))))

;;; --------------------------------------------------------------------
;;; Income of a book's positions
;;; --------------------------------------------------------------------

(defgeneric position-income (instrument from to market-from market-to)
  (:documentation "(values flows finished): FLOWS, a list of (pay-date amount
kind) paid to one unit of INSTRUMENT over (FROM, TO], KIND one of
:DISCRETE-DIVIDEND :CONTINUOUS-DIVIDEND :COUPON :REDEMPTION :EXPIRY, and
FINISHED true if the instrument has nothing left after TO (it is removed
from the book). MARKET-FROM and MARKET-TO are functions of no arguments
returning the simulated markets on FROM and TO, built on first call.

A holding pays its discrete dividends going ex in the interval, on the
ex-date (a cash dividend its drop in the price, the amount discounted from
its pay date; a proportional one the fraction of the spot before the drop),
and its continuous yield q to first order, S(FROM) (e^(q dt) - 1) at TO. A
bond pays its coupons and redemption, a swap or FRA its signed flows, on
their payment dates. An option expiring in the interval pays its payoff
there, which must be on TO. Cash, caps, floors and swaptions pay nothing
(exercise is clause code); so does any other instrument.")
  (:method ((i instrument) from to market-from market-to)
    (declare (ignore from to market-from market-to))
    (values '() nil)))

(defmethod position-income ((h holding) from to market-from market-to)
  (declare (ignore market-to))
  (let* ((view (instrument-view (funcall market-from) h))
         (spot (spot view))
         (flows (iter (for event in (discrete-dividends view))
                      (for ex = (dividend-event-date event))
                      (while (date<= ex to))
                      (when (date< from ex)
                        (collect `(,ex ,(if (eq (dividend-event-kind event) :cash)
                                            (dividend-event-amount event)
                                            ;; The expected spot just before the drop.
                                            (let ((delta (dividend-event-amount event)))
                                              (* delta (/ (forward view ex) (- 1d0 delta)))))
                                   :discrete-dividend)))))
         (dt (market-time view to))
         (q (dividend-yield view to)))
    (values (if (zerop q)
                flows
                `(,@flows (,to ,(* spot (expm1 (* q dt))) :continuous-dividend)))
            nil)))

(defun flows-in (cashflows from to market sign kind-of)
  "(pay-date amount kind) for each of CASHFLOWS paying in (FROM, TO], its
amount times SIGN; MARKET (a function) is called only for floating flows."
  (iter (for cf in cashflows)
        (for pay = (payment-date cf))
        (when (and (date< from pay) (date<= pay to))
          (collect `(,pay ,(* sign (cashflow-amount cf (if (typep cf 'floating-rate-coupon) (funcall market) nil)))
                     ,(funcall kind-of cf))))))

(defmethod position-income ((b bond) from to market-from market-to)
  (declare (ignore market-from))
  (values (flows-in (bond-cashflows b) from to market-to 1d0
                    (lambda (cf) (if (typep cf 'coupon) :coupon :redemption)))
          (date<= (payment-date (a:lastcar (bond-cashflows b))) to)))

(defmethod position-income ((s swap) from to market-from market-to)
  (declare (ignore market-from))
  (values (iter (for leg in (swap-legs s))
                (for sign in (swap-signs s))
                (appending (flows-in leg from to market-to sign (constantly :coupon))))
          (iter (for leg in (swap-legs s))
                (always (date<= (payment-date (a:lastcar leg)) to)))))

(defmethod position-income ((f forward-rate-agreement) from to market-from market-to)
  (declare (ignore market-from))
  (let ((pay (fra-value-date f)))
    (if (and (date< from pay) (date<= pay to))
        (values `((,pay ,(fra-amount f (funcall market-to)) :coupon)) t)
        (values '() (date<= pay to)))))

(defmethod position-income ((o option) from to market-from market-to)
  (declare (ignore market-from))
  (let ((expiry (expiry (option-exercise o))))
    (cond ((not (date<= expiry to)) (values '() nil))
          ((not (date= expiry to))
           (signal-invalid-argument 'option o 'option
                                    (format nil "an option expiring on a simulation date: it expires on ~A, between ~
~A and ~A; add ~:*~:*~:*~A to DATES" expiry from to)))
          (t (values `((,to ,(values (price o :auto (funcall market-to))) :expiry)) t)))))

(defun income-currency (instrument market)
  "The currency INSTRUMENT pays in on MARKET, or NIL if unknown."
  (or (instrument-currency instrument)
      (and (typep market 'composite-market)
           (not (typep instrument '(or rates-instrument cash-account)))
           (gethash (resolve-underlying market (instrument-underlying instrument))
                    (market-underlying-currency market)))))

(defun remove-finished-positions (book finished)
  "BOOK without the positions in FINISHED, at any depth."
  (labels ((prune (portfolio)
             (apply #'make-portfolio (portfolio-name portfolio)
                    (iter (for m in (portfolio-members portfolio))
                          (cond ((typep m 'portfolio) (collect (prune m)))
                                ((not (member m finished)) (collect m)))))))
    (prune book)))

(defun book-income (book accounting k target market-from market-to)
  "BOOK after D5's step 2 at event K: each position's income over
(t_{k-1}, t_k] (POSITION-INCOME) of the kinds the book collects, times its
quantity, paid into cash and grown from its pay date to t_k (exactly on the
initial curve TARGET, at face when a leg simulates TARGET); then the
positions that matured or expired are removed."
  (let* ((plan (book-accounting-plan accounting))
         (from (simulation-date plan (1- k)))
         (to (simulation-date plan k))
         (initial (simulation-plan-market plan))
         (simulated (find target (simulation-plan-legs plan) :key #'simulation-leg-target :test #'equal))
         (cash-currency (instrument-currency (position-instrument (book-cash-position book))))
         (collect (book-accounting-collect accounting))
         (balance (book-balance book))
         (finished '()))
    (flet ((growth (pay)
             (cond ((or simulated (date= pay to)) 1d0)
                   ((null target) (/ (discount-factor initial pay) (discount-factor initial to)))
                   (t (/ (market-curve-discount initial (second target) pay)
                         (market-curve-discount initial (second target) to))))))
      (dolist (p (portfolio-positions book))
        (let ((instrument (position-instrument p)))
          (multiple-value-bind (flows done) (position-income instrument from to market-from market-to)
            (when done (push p finished))
            (dolist (flow flows)
              (destructuring-bind (pay amount kind) flow
                (when (member kind collect)
                  (let ((currency (income-currency instrument initial)))
                    (when (and currency cash-currency (not (eq currency cash-currency)))
                      (error 'currency-mismatch :currencies `(,currency ,cash-currency)
                                                :operation (format nil "income of ~A on ~A into a book's cash" p to))))
                  (incf balance (* (position-quantity p) amount (growth pay)))))))))
      (remove-finished-positions (set-book-balance book balance) finished))))

(defun output-value (name path value)
  (if (realp value)
      (float value 1d0)
      (signal-invalid-argument 'output value 'real
                               (format nil "a real for the output ~S (path ~D)" name path))))

(defun live-book-greeks (book engine market keys)
  "The Greeks KEYS of BOOK on MARKET, leaving out options that expire on or
before MARKET's valuation date (their Greeks are undefined there; they
contribute 0)."
  (let ((today (valuation-date market)))
    (greeks (apply #'make-portfolio (portfolio-name book)
                   (remove-if (lambda (p)
                                (let ((i (position-instrument p)))
                                  (and (typep i 'option)
                                       (not (date< today (expiry (option-exercise i)))))))
                              (portfolio-positions book)))
            engine market :which keys)))
