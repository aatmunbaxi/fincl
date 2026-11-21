;;;; market.lisp --- the market protocol, composite markets and views
;;;;
;;;; Engines never read market slots. They use these generic functions: SPOT, ZERO-RATE,
;;;; DIVIDEND-YIELD, BLACK-VOL, BLACK-VARIANCE, and the defaults that are built on them.
;;;; That protocol describes one underlying and one discount curve. A COMPOSITE-MARKET
;;;; holds many, as named term structures (src/term-structures/). PRICE narrows it to a
;;;; MARKET-VIEW for the instrument that it prices. Thus engines never see more than one
;;;; underlying.
;;;;
;;;; A market is an immutable snapshot. The code passes it to every parallel worker, so
;;;; nothing can mutate it. A scenario is a new market. It is not an edit to an existing
;;;; market.
;;;;
;;;; Every protocol function that takes a point in time accepts a DATE or a REAL. A real is
;;;; a horizon in market time: the year fraction from the valuation date under the day
;;;; counter of the market. The methods on DATE convert with MARKET-TIME and delegate.
;;;; Thus concrete markets implement only the REAL methods.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(define-condition non-positive-volatility (pricing-error)
  ((volatility :initarg :volatility :initform nil :reader error-volatility)
   (where :initarg :where :reader error-where))
  (:report (lambda (c s)
             (format s "The volatility ~@[~,6F ~]~A is not positive; pricing needs a ~
positive volatility."
                     (error-volatility c) (error-where c))))
  (:documentation "A volatility of zero or below where a model needs a positive volatility. PRICE offers
USE-VALUE, which returns a value as the price."))

(defclass market ()
  ()
  (:documentation "The state of the world at a valuation date."))

;;; --------------------------------------------------------------------
;;; Time
;;; --------------------------------------------------------------------

(defgeneric valuation-date (market)
  (:documentation "The date on which the market is observed."))

(defgeneric market-day-counter (market)
  (:documentation "The day counter that turns dates into market time."))

(declaim (ftype (function (t t) (values double-float &optional)) market-time))
(defun market-time (market date)
  "The year fraction from the valuation date of MARKET to DATE, under its day counter.

  (market-time m #D\"2027-09-24\") => 1.0d0   ; valued 2026-09-24, ACT/365F"
  (let ((dc (market-day-counter market)))
    (if (eq (class-of dc) (load-time-value (find-class 'actual-365-fixed)))
        ;; YEAR-FRACTION's ACT/365F method without dispatch: the same double
        ;; (its :AROUND negation of a reversed interval is exact).
        (/ (float (date- date (valuation-date market)) 1d0) 365d0)
        (year-fraction dc (valuation-date market) date))))

;;; --------------------------------------------------------------------
;;; The protocol
;;; --------------------------------------------------------------------

(defgeneric spot (market)
  (:documentation "The current price of the underlying."))

(defgeneric zero-rate (market x)
  (:documentation "The continuously compounded risk-free zero rate to X, a date or a horizon in market
time."))

(defgeneric dividend-yield (market x)
  (:documentation "The continuously compounded dividend yield to X, a date or a horizon in market time."))

(defgeneric black-vol (market strike x)
  (:documentation "The Black-Scholes volatility for STRIKE to X, a date or a horizon in market time. A
flat market ignores STRIKE and X. A surface interpolates."))

(defgeneric black-variance (market strike x)
  (:documentation "The total Black variance for STRIKE to X, a date or a horizon in market time. For a
flat market it is vol^2 * horizon."))

;;; Derived quantities. Defaults are expressed in terms of the protocol
;;; above, so a market only has to implement the primitives; a curve
;;; market can still override these directly with its own interpolation.

(defgeneric discount-factor (market x)
  (:method ((m market) (horizon real))
    (exp (- (* (zero-rate m horizon) horizon)))))

(defgeneric discrete-dividends (market)
  (:documentation "Return the discrete dividends of the underlying that go ex after the valuation date, as
DIVIDEND-EVENTs in market time, ordered by time. The default is NIL, for a market with
only a continuous dividend yield.")
  (:method ((m market)) '()))

(defgeneric carry-factor (market x)
  (:documentation "The growth factor of the underlying between discrete dividends: exp((r - q) horizon),
with the continuous dividend yield q only. For a market without discrete dividends, it
is the forward factor. Processes grow at this factor. Engines that model discrete
dividends apply each dividend at its ex-date.")
  (:method ((m market) (horizon real))
    (exp (* (- (zero-rate m horizon) (dividend-yield m horizon)) horizon))))

(defgeneric forward-factor (market x)
  (:documentation "F(x) / S: the CARRY-FACTOR, reduced by any discrete dividends that go ex by X (see
FORWARD).")
  (:method ((m market) (horizon real))
    (if (discrete-dividends m)
        (/ (forward m horizon) (spot m))
        (carry-factor m horizon))))

(defgeneric forward (market x)
  (:documentation "The forward of the underlying to X, a date or a horizon in market time. When discrete
dividends go ex at t_i <= X,

  F(X) = (S - sum_i PV_i) C(X) prod_j (1 - delta_j)

C is the CARRY-FACTOR. delta_j are the proportional dividends. For a cash dividend D_i
that pays at p_i, the value at its ex-date is D_i P(p_i) / P(t_i). PV_i carries it back
to today: D_i P(p_i) / (P(t_i) C(t_i)). Divide it by (1 - delta_j) for each
proportional dividend up to t_i. With rates only (C = 1/P), F(X) = (S - sum_i D_i
P(p_i)) / P(X). A continuous yield Q scales S by Q(X) and each dividend term by
Q(X)/Q(t_i). The function takes cash dividends in full, whether or not they exceed the
price.")
  (:method ((m market) (horizon real))
    (let ((events (discrete-dividends m)))
      (if events
          (* (- (spot m) (escrowed-dividends m events 0d0 horizon))
             (carry-factor m horizon)
             (proportional-dividend-factor events 0d0 horizon))
          (* (spot m) (forward-factor m horizon))))))

(defun dividends-between (events from to &key cum)
  "The DIVIDEND-EVENTs of EVENTS with FROM < time <= TO; with CUM, FROM <=
time <= TO (the price at FROM taken just before a dividend going ex then)."
  (remove-if-not (lambda (e)
                   (let ((time (dividend-event-time e)))
                     (and (if cum (<= (- from 1d-12) time) (< from time))
                          (<= time (+ to 1d-12)))))
                 events))

(defun proportional-dividend-factor (events from to)
  "The product of (1 - delta) over the proportional dividends of EVENTS going
ex in (FROM, TO]."
  (let ((factor 1d0))
    (dolist (e (dividends-between events from to) factor)
      (when (eq (dividend-event-kind e) :proportional)
        (setf factor (* factor (- 1d0 (dividend-event-amount e))))))))

(defun escrowed-dividend-terms (market events from to &key cum)
  "The cash dividends of EVENTS going ex in (FROM, TO] (with CUM, [FROM,
TO]), as (time . value) pairs, the value in units of the price at FROM: each
drop carried back to FROM at the carry factor, and divided by (1 - delta) for
each proportional dividend before it."
  (let ((growth (carry-factor market from))
        (proportional 1d0))
    (iter (for e in (dividends-between events from to :cum cum))
          (if (eq (dividend-event-kind e) :proportional)
              (setf proportional (* proportional (- 1d0 (dividend-event-amount e))))
              (collect (cons (dividend-event-time e)
                             (/ (* (dividend-event-amount e) growth)
                                (* (carry-factor market (dividend-event-time e)) proportional))))))))

(defun escrowed-dividends (market events from to &key cum)
  "The sum of the ESCROWED-DIVIDEND-TERMS: S(FROM) less this is the escrowed
model's price at FROM (with CUM, just before the dividends going ex at FROM)."
  (let ((total 0d0))
    (dolist (term (escrowed-dividend-terms market events from to :cum cum) total)
      (incf total (cdr term)))))

;;; Dates: convert to market time and delegate to the REAL methods.

(defmethod zero-rate ((m market) (x date))
  (zero-rate m (market-time m x)))
(defmethod dividend-yield ((m market) (x date))
  (dividend-yield m (market-time m x)))
(defmethod black-vol ((m market) strike (x date))
  (black-vol m strike (market-time m x)))
(defmethod black-variance ((m market) strike (x date))
  (black-variance m strike (market-time m x)))
(defmethod discount-factor ((m market) (x date))
  (discount-factor m (market-time m x)))
(defmethod carry-factor ((m market) (x date))
  (carry-factor m (market-time m x)))
(defmethod forward-factor ((m market) (x date))
  (forward-factor m (market-time m x)))
(defmethod forward ((m market) (x date))
  (forward m (market-time m x)))

;;; --------------------------------------------------------------------
;;; Composite markets
;;; --------------------------------------------------------------------

(define-condition ambiguous-underlying (invalid-input)
  ((underlyings :initarg :underlyings :reader error-underlyings))
  (:report (lambda (c s)
             (format s "The market has several underlyings, ~{~S~^, ~}: name the ~
instrument's with (underlying ...) or :UNDERLYING."
                     (error-underlyings c)))))

(define-condition missing-market-data (pricing-error)
  ((what :initarg :what :reader error-what)
   (key :initarg :key :reader error-key)
   (known :initarg :known :reader error-known))
  (:report (lambda (c s)
             (format s "The market has no ~A ~S. It has ~:[none~;~:*~{~S~^, ~}~]."
                     (error-what c) (error-key c) (error-known c))))
  (:documentation "An instrument or a view needs an entry that the market does not hold, for example an
underlying, a curve or a vol structure."))

(define-condition unsupported-quanto (pricing-error)
  ((underlying :initarg :underlying :reader error-underlying)
   (underlying-currency :initarg :underlying-currency :reader error-underlying-currency)
   (currency :initarg :currency :reader error-currency))
  (:report (lambda (c s)
             (format s "The instrument pays in ~S, but ~S trades in ~S: that is a quanto, ~
which needs an FX vol and correlation fincl does not model. Price it in ~S."
                     (error-currency c) (error-underlying c) (error-underlying-currency c)
                     (error-underlying-currency c))))
  (:documentation "An instrument pays in a currency other than the currency of its underlying. To price it
needs an FX vol and a correlation. fincl does not model them."))

(defclass composite-market (market)
  ((valuation-date :initarg :valuation-date :reader valuation-date :type date
                   :initform (a:required-argument :valuation-date))
   (day-counter :initarg :day-counter :reader market-day-counter
                :type day-counter :initform +actual-365-fixed+)
   (underlyings :initarg :underlyings :reader market-underlyings :type list
                :documentation "Underlying names, in no specified order; printing sorts them.")
   (spots :initarg :spots :reader market-spots :type hash-table)
   (yield-curves :initarg :yield-curves :reader market-yield-curves :type hash-table)
   (dividend-curves :initarg :dividend-curves :reader market-dividend-curves :type hash-table)
   (dividend-schedules :initarg :dividend-schedules :reader market-dividend-schedules
                       :type hash-table :initform (make-hash-table :test 'equal)
                       :documentation "Underlying -> DIVIDEND-SCHEDULE, for those that have one.")
   (vol-structures :initarg :vol-structures :reader market-vol-structures :type hash-table)
   (underlying-currency :initarg :underlying-currency :reader market-underlying-currency
                        :type hash-table)
   (discount-curve-for :initarg :discount-curve-for :reader market-discount-curve-for
                       :type hash-table)
   (fixings :initarg :fixings :reader market-fixings :type hash-table
            :initform (make-hash-table :test 'equal)
            :documentation "Index id -> EQUALP table of fixing date -> rate.")
   (extra-components :initarg :extra-components :initform nil :reader market-extra-components
                     :type (or null hash-table)
                     :documentation "Kind -> EQUAL table of key -> value, for the kinds
DEFINE-MARKET-COMPONENT stores outside the tables above; NIL when the market
holds none."))
  (:documentation "Named term structures that are observed on one valuation date. It holds a spot, a
dividend curve, a vol structure and a currency for each underlying (and a
DIVIDEND-SCHEDULE for underlyings that pay discrete dividends). It also holds yield
curves by id, the curve that each currency discounts on, and any kind that
DEFINE-MARKET-COMPONENT registered. The tables are EQUAL hash tables with keyword
keys. The functions that derive or bump a market copy them. Nothing changes them after
construction, so parallel workers can share a market. To price an instrument, the
function narrows the market to a MARKET-VIEW of the underlying of the instrument."))

(defun market-entry (table key what known)
  "The value of KEY in TABLE. KNOWN, the keys to report when KEY is missing,
is a list or a function of no arguments returning one, called only then."
  (multiple-value-bind (value found) (gethash key table)
    (if found
        value
        (error 'missing-market-data :what what :key key
                                    :known (if (functionp known) (funcall known) known)))))

(defun alist-table (alist)
  (let ((table (make-hash-table :test 'equal)))
    (iter (for (key . value) in alist) (setf (gethash key table) value))
    table))

(defun as-structure (value class initarg)
  "VALUE if it is already a term structure, otherwise a flat CLASS of it."
  (if (typep value 'term-structure)
      value
      (make-instance class initarg value)))

(declaim (ftype (function (&key (:valuation-date t) (:day-counter t) (:underlyings list) (:curves list) (:discounting list) (:fixings list) (:components list))
                    (values composite-market &optional))
          make-market-from-specs))
(defun make-market-from-specs (&key valuation-date (day-counter +actual-365-fixed+)
                                    underlyings curves discounting fixings components)
  "A COMPOSITE-MARKET observed on VALUATION-DATE, from keyword arguments and
nested lists: the call shape MAKE-MARKET accepts besides clauses. The
arguments are lists:

  UNDERLYINGS  ((name :spot s [:currency c] [:dividend q] [:dividend-schedule d]
                [:vol v]) ...)
               S a positive real; C a keyword (default :DEFAULT); Q a real
               continuous dividend yield or a DIVIDEND-CURVE (default 0); D
               discrete dividends alongside Q, a DIVIDEND-SCHEDULE or the
               entries MAKE-DIVIDEND-SCHEDULE takes, ((ex-date amount [:kind
               k] [:pay-date p]) ...); V a real Black vol or a
               BLACK-VOL-STRUCTURE (default none, for markets that need no
               vol).
  CURVES       ((id :rate r) ...) or ((id :curve c) ...): R a real zero rate,
               C a YIELD-CURVE.
  DISCOUNTING  ((currency . curve-id) ...): the curve each currency discounts
               on. If omitted and there is one curve, every currency
               discounts on it.
  FIXINGS      ((index-id (date rate) ...) ...): past fixings of interest-rate
               indexes, DATE a date or a \"YYYY-MM-DD\" string.
  COMPONENTS   ((kind (key value) ...) ...): entries of kinds registered with
               DEFINE-MARKET-COMPONENT beyond those above, KEY a keyword and
               VALUE what the kind's clause takes.

Reals are coerced to double-float.

  (make-market-from-specs :valuation-date #D\"2026-09-24\"
               :underlyings '((:spx :spot 5000 :currency :usd :dividend 0.015d0 :vol 0.18d0)
                              (:aapl :spot 230 :currency :usd :vol 0.25d0
                               :dividend-schedule ((\"2027-02-10\" 0.26d0)
                                                   (\"2027-05-12\" 0.26d0))))
               :curves '((:usd-ois :rate 0.04d0))
               :discounting '((:usd . :usd-ois)))"
  (let ((spots '()) (dividends '()) (schedules '()) (vols '()) (currencies '()) (names '()))
    (dolist (entry underlyings)
      (destructuring-bind (name &key spot (currency :default) (dividend 0d0) dividend-schedule vol)
          entry
        (check-input name keyword "a keyword naming the underlying")
        (check-input spot (real (0)) "a positive spot")
        (check-input currency keyword "a currency keyword")
        (push name names)
        (push (cons name (float spot 1d0)) spots)
        (push (cons name currency) currencies)
        (push (cons name (as-structure dividend 'flat-dividend-curve :rate)) dividends)
        (when dividend-schedule
          (push (cons name (if (typep dividend-schedule 'dividend-schedule)
                               dividend-schedule
                               (make-dividend-schedule dividend-schedule)))
                schedules))
        (when vol
          (push (cons name (as-structure vol 'flat-black-vol :vol)) vols))))
    (let* ((curve-alist (iter (for spec in curves)
                              (collect (destructuring-bind (id &key rate curve &allow-other-keys) spec
                                         (check-input id keyword "a keyword naming the curve")
                                         (cons id (or curve (as-structure rate 'flat-yield-curve
                                                                          :rate)))))))
           (discounting (or discounting
                            (and (= 1 (length curve-alist))
                                 (let ((id (car (first curve-alist))))
                                   (->> currencies
                                        (mapcar #'cdr)
                                        remove-duplicates
                                        (mapcar (lambda (currency) `(,currency . ,id)))))))))
      (iter (for (nil . id) in discounting)
            (unless (assoc id curve-alist)
              (signal-invalid-argument 'discounting discounting 'list
                                       (format nil "currencies mapped to curves the market ~
holds (~{~S~^, ~})" (mapcar #'car curve-alist)))))
      (make-instance 'composite-market
                     :valuation-date (or valuation-date (a:required-argument :valuation-date))
                     :day-counter day-counter
                     :underlyings (reverse names)
                     :spots (alist-table spots)
                     :yield-curves (alist-table curve-alist)
                     :dividend-curves (alist-table dividends)
                     :dividend-schedules (alist-table schedules)
                     :vol-structures (alist-table vols)
                     :underlying-currency (alist-table currencies)
                     :discount-curve-for (alist-table discounting)
                     :fixings (fixings-table fixings)
                     :extra-components (components-table components)))))

(defun fixings-table (fixings)
  "An EQUAL table of index id -> EQUALP table of date -> rate, from
((index-id (date rate) ...) ...)."
  (let ((table (make-hash-table :test 'equal)))
    (iter (for (id . pairs) in fixings)
          (check-input id keyword "a keyword naming the index")
          (let ((dates (make-hash-table :test 'equalp)))
            (iter (for (date rate) in pairs)
                  (let ((date (if (stringp date) (parse-date date) date)))
                    (check-input date date "a date or a \"YYYY-MM-DD\" string")
                    (check-input rate real "a real fixing")
                    (setf (gethash date dates) (float rate 1d0))))
            (setf (gethash id table) dates)))
    table))

(declaim (ftype (function (&key (:valuation-date t) (:day-counter t) (:spot real) (:rate real) (:dividend real) (:dividend-schedule t) (:vol t))
                    (values composite-market &optional))
          make-bs-market))
(defun make-bs-market (&key valuation-date (day-counter +actual-365-fixed+)
                            spot rate (dividend 0d0) dividend-schedule vol)
  "Make a flat Black-Scholes market: a COMPOSITE-MARKET with one underlying and one
currency, both :DEFAULT, that discounts on the curve :DEFAULT. The function coerces
numeric inputs to double-float. Pass double-float or rational literals. The literal
0.05 reads as a single-float, and (float 0.05 1d0) is 0.05000000074505806d0.
DIVIDEND-SCHEDULE adds discrete dividends, as in MAKE-MARKET-FROM-SPECS.

  (make-bs-market :valuation-date #D\"2026-09-24\"
                  :spot 100 :rate 0.05d0 :vol 0.2d0
                  :dividend-schedule '((\"2027-03-15\" 2)))"
  (make-market-from-specs
               :valuation-date valuation-date :day-counter day-counter
               :underlyings `((:default :spot ,spot :currency :default
                                        :dividend ,dividend :vol ,vol
                                        ,@(when dividend-schedule
                                            `(:dividend-schedule ,dividend-schedule))))
               :curves `((:default :rate ,rate))
               :discounting '((:default . :default))))

(defun copy-composite-market (m &rest overrides)
  "A copy of M with fresh copies of its tables, then OVERRIDES (initargs)."
  (flet ((copy (table)
           (let ((new (make-hash-table :test 'equal)))
             (maphash (lambda (k v) (setf (gethash k new) v)) table)
             new)))
    (apply #'make-instance 'composite-market
           `(,@overrides
             :valuation-date ,(valuation-date m) :day-counter ,(market-day-counter m)
             :underlyings ,(market-underlyings m)
             :spots ,(copy (market-spots m))
             :yield-curves ,(copy (market-yield-curves m))
             :dividend-curves ,(copy (market-dividend-curves m))
             :dividend-schedules ,(copy (market-dividend-schedules m))
             :vol-structures ,(copy (market-vol-structures m))
             :underlying-currency ,(copy (market-underlying-currency m))
             :discount-curve-for ,(copy (market-discount-curve-for m))
             :fixings ,(copy (market-fixings m))
             :extra-components ,(a:when-let ((extra (market-extra-components m)))
                                  (let ((new (make-hash-table :test 'equal)))
                                    (maphash (lambda (kind table) (setf (gethash kind new) (copy table)))
                                             extra)
                                    new))))))

(defun market-with (market kind key value)
  "A copy of the composite MARKET whose component (KIND KEY) is VALUE, KIND a
registered kind (see DEFINE-MARKET-COMPONENT); MARKET is unchanged.

  (market-with m :vol :spx (make-flat-black-vol :vol 0.2d0))"
  (let ((new (copy-composite-market market)))
    (setf (gethash key (kind-table new (find-market-component-kind kind) t)) value)
    new))

(defgeneric derive-market (market &key &allow-other-keys)
  (:documentation "Return a new market that equals MARKET but is observed on another VALUATION-DATE or has
another DAY-COUNTER (aging, conventions). MARKET does not change. Use BUMP to move
market data.

  (derive-market m :valuation-date #D\"2027-03-24\")"))

(defmethod derive-market ((m composite-market)
                          &key (valuation-date (valuation-date m))
                               (day-counter (market-day-counter m)))
  (copy-composite-market m :valuation-date valuation-date :day-counter day-counter))

(defun underlying-spot (market underlying)
  (market-entry (market-spots market) underlying "underlying" (market-underlyings market)))

(defun find-yield-curve (market id)
  (market-entry (market-yield-curves market) id "yield curve"
                (lambda () (sorted-keys (market-yield-curves market)))))

(defun discount-curve-id (market currency)
  (market-entry (market-discount-curve-for market) currency "discount curve for currency"
                (lambda () (sorted-keys (market-discount-curve-for market)))))

(defun resolve-underlying (market underlying)
  "UNDERLYING, checked against MARKET; NIL means the market's only one."
  (let ((known (market-underlyings market)))
    (cond (underlying (underlying-spot market underlying) underlying)
          ((= 1 (length known)) (first known))
          (t (error 'ambiguous-underlying :underlyings known)))))

;;; --------------------------------------------------------------------
;;; Bumps
;;; --------------------------------------------------------------------

(declaim (ftype (function (t t t) (values market &optional)) bump))
(defun bump (market factor shift)
  "Return a new market with the risk FACTOR moved by SHIFT. MARKET does not change.
FACTOR is one of:
  (:spot u), (:vol u), (:dividend u) (the continuous yield)
                          for underlying U
  (:dividend-schedule u)  every discrete dividend amount of U
  (:dividend-schedule u ex-date)
                          the dividends that go ex on one date
  (:curve id)             a yield curve
  (:quote id selector)    one input quote of a bootstrapped curve, chosen by the
                          tenor or the index of its helper. The function
                          bootstraps the curve again
  (kind key)              any other kind that DEFINE-MARKET-COMPONENT registered
SHIFT is (:relative x), which multiplies by 1 + x, or (:absolute x), which adds x. It
applies to the spot, the vol, the yield, the dividend amounts (limited below at 0), the
rate or the quoted rate.

  (bump m '(:spot :spx) '(:relative 1/100))     ; SPX spot up 1%
  (bump m '(:curve :usd-ois) '(:absolute 1/10000)) ; rates up 1bp
  (bump m `(:quote :usd-sofr ,#T\"5Y\") '(:absolute 1/10000)) ; the 5Y quote up 1bp
  (bump m `(:dividend-schedule :aapl ,#D\"2027-02-10\") '(:absolute 1/100))"
  (check-input market composite-market "a composite market (bump the market a view was taken from)")
  (unless (bump-factor-p factor)
    (signal-invalid-argument 'factor factor 'list
                             (format nil "(kind key), kind one of ~{~S~^, ~}; (:dividend-schedule u ex-date) ~
or (:quote id tenor-or-index)" (target-kinds))))
  (check-input shift shift "(:relative x) or (:absolute x)")
  (when (eq (first factor) :quote)
    (destructuring-bind (id selector) (rest factor)
      (return-from bump
        (market-with market :curve id
                     (bump-structure-quote (find-yield-curve market id) selector shift)))))
  (when (and (eq (first factor) :dividend-schedule) (cddr factor))
    (destructuring-bind (underlying ex-date) (rest factor)
      (return-from bump
        (market-with market :dividend-schedule underlying
                     (bump-dividend (market-entry (market-dividend-schedules market) underlying
                                                  "dividend schedule for"
                                                  (sorted-keys (market-dividend-schedules market)))
                                    ex-date shift)))))
  (destructuring-bind (kind key) factor
    (let ((component (find-market-component-kind kind)))
      (market-with market kind key (bump-value component (kind-entry market component key) shift)))))

;;; --------------------------------------------------------------------
;;; Components by target
;;; --------------------------------------------------------------------

(defun normalize-target (target)
  "TARGET as (kind key): a keyword U is (:SPOT U); a list must be (kind key)
with kind one of TARGET-KINDS and key a keyword."
  (cond ((keywordp target) `(:spot ,target))
        ((and (consp target) (consp (rest target)) (null (cddr target))
              (member (first target) (target-kinds)) (keywordp (second target)))
         target)
        (t (signal-invalid-argument 'target target 'list
                                    (format nil "a target (kind key), kind one of ~{~S~^, ~}, or an underlying keyword"
                                            (target-kinds))))))

(declaim (ftype (function (t t) (values t &optional)) market-component))
(defun market-component (market target)
  "Return the component of the composite MARKET that TARGET names. TARGET uses the factor
syntax of BUMP: (:spot u) is the spot of underlying U (a bare keyword U means the same),
(:vol u) is its vol structure, (:dividend u) is its continuous dividend curve,
(:dividend-schedule u) is its discrete dividends, (:curve id) is the yield curve ID,
and (kind key) is an entry of any other kind that DEFINE-MARKET-COMPONENT registered.
If the market lacks the component, the function signals MISSING-MARKET-DATA. The report
lists what the market has.

  (market-component m '(:curve :usd-ois))
  (market-component m :spx)   ; the SPX spot"
  (check-input market composite-market "a composite market")
  (destructuring-bind (kind key) (normalize-target target)
    (kind-entry market (find-market-component-kind kind) key)))

(defun set-market-component (market target value date)
  "Set TARGET's component of MARKET, a fresh copy not yet shared, to VALUE,
checked against the component's type; DATE names the market in the report."
  (destructuring-bind (kind key) target
    (let* ((component (find-market-component-kind kind))
           (type (market-component-kind-type component)))
      (unless (typep value type)
        (signal-invalid-argument target value type
                                 (format nil "~A for ~S on ~A" (market-component-kind-description component)
                                         target date)))
      (setf (gethash key (kind-table market component t))
            (a:if-let ((coerce (market-component-kind-coerce component))) (funcall coerce value) value)))))

;;; --------------------------------------------------------------------
;;; Views
;;; --------------------------------------------------------------------

(defclass market-view (market)
  ((market :initarg :market :reader view-market :type composite-market)
   (underlying :initarg :underlying :reader view-underlying :type keyword)
   (discount-curve :initarg :discount-curve :reader view-discount-curve :type keyword)
   (spot :initarg :spot :reader view-spot :type double-float)
   (yield-curve :initarg :yield-curve :reader view-yield-curve
                :documentation "The discount curve.")
   (forecast-curve :initarg :forecast-curve :reader view-forecast-curve
                   :documentation "The curve of the underlying's currency, which
sets the forward's drift.")
   (dividend-curve :initarg :dividend-curve :reader view-dividend-curve)
   (dividends :initarg :dividends :initform '() :reader view-dividends
              :documentation "The underlying's DIVIDEND-EVENTs after the valuation date.")
   (vol-structure :initarg :vol-structure :reader view-vol-structure))
  (:documentation "One underlying of a composite market and one discount curve, seen through the
single-underlying market protocol that engines use. The forward grows at the rate of the
curve of the currency of the underlying, less the dividend yield. DISCOUNT-FACTOR and
ZERO-RATE come from the discount curve. MARKET-VIEW makes it. PRICE makes one for each
instrument."))

(declaim (ftype (function (t &key (:underlying t) (:discount-curve t))
                    (values market-view &optional))
          market-view))
(defun market-view (market &key underlying discount-curve)
  "Make a MARKET-VIEW of MARKET for UNDERLYING (NIL: the only underlying of the market).
It discounts on the curve DISCOUNT-CURVE (NIL: the curve of the currency of the
underlying, which always sets the forward).

  (spot (market-view m :underlying :spx)) => 5000.0d0"
  (let* ((underlying (resolve-underlying market underlying))
         (forecast-id (discount-curve-id market (gethash underlying
                                                         (market-underlying-currency market))))
         (curve-id (or discount-curve forecast-id))
         (forecast (curve-in-market (find-yield-curve market forecast-id) market)))
    (make-instance 'market-view
                   :market market :underlying underlying :discount-curve curve-id
                   :spot (underlying-spot market underlying)
                   :yield-curve (curve-in-market (find-yield-curve market curve-id) market)
                   :forecast-curve forecast
                   :dividend-curve (gethash underlying (market-dividend-curves market))
                   :dividends (a:when-let ((schedule (gethash underlying
                                                              (market-dividend-schedules market))))
                                (dividend-events schedule market forecast))
                   :vol-structure (gethash underlying (market-vol-structures market)))))

(defun dividend-events (schedule market forecast)
  "SCHEDULE's dividends going ex after MARKET's valuation date, as
DIVIDEND-EVENTs in market time. A cash amount is discounted from its pay
date to its ex-date on FORECAST, the underlying currency's curve."
  (let ((today (valuation-date market)))
    (iter (for d in (schedule-dividends schedule))
          (when (date< today (dividend-ex-date d))
            (let ((time (market-time market (dividend-ex-date d))))
              (collect (make-dividend-event
                        (dividend-ex-date d) time (dividend-kind d)
                        (if (eq (dividend-kind d) :cash)
                            (* (dividend-amount d)
                               (/ (curve-discount forecast (market-time market (dividend-pay-date d)))
                                  (curve-discount forecast time)))
                            (dividend-amount d)))))))))

(defun view-vol (view)
  (or (view-vol-structure view)
      (error 'missing-market-data :what "vol structure for" :key (view-underlying view)
                                  :known (sorted-keys (market-vol-structures (view-market view))))))

(defmethod valuation-date ((v market-view)) (valuation-date (view-market v)))
(defmethod market-day-counter ((v market-view)) (market-day-counter (view-market v)))
(defmethod spot ((v market-view)) (view-spot v))
(defmethod zero-rate ((v market-view) (horizon real))
  (curve-zero-rate (view-yield-curve v) horizon))
(defmethod dividend-yield ((v market-view) (horizon real))
  (curve-dividend-yield (view-dividend-curve v) horizon))
(defmethod black-vol ((v market-view) strike (horizon real))
  (structure-black-vol (view-vol v) strike horizon (forward v horizon)))
(defmethod black-variance ((v market-view) strike (horizon real))
  (structure-black-variance (view-vol v) strike horizon (forward v horizon)))
(defmethod discount-factor ((v market-view) (horizon real))
  (curve-discount (view-yield-curve v) horizon))
(defmethod discrete-dividends ((v market-view)) (view-dividends v))
(defmethod carry-factor ((v market-view) (horizon real))
  ;; exp((r - q) h), the protocol default's expression, with r from the
  ;; underlying currency's curve.
  (exp (* (- (curve-zero-rate (view-forecast-curve v) horizon)
             (curve-dividend-yield (view-dividend-curve v) horizon))
          horizon)))

;;; A composite market with one underlying is itself a single-underlying
;;; market: the protocol answers through its only view, so a market from
;;; MAKE-BS-MARKET can be queried directly. With several underlyings these
;;; signal AMBIGUOUS-UNDERLYING; take a MARKET-VIEW instead.

(defmethod spot ((m composite-market)) (spot (market-view m)))
(defmethod zero-rate ((m composite-market) (horizon real))
  (zero-rate (market-view m) horizon))
(defmethod dividend-yield ((m composite-market) (horizon real))
  (dividend-yield (market-view m) horizon))
(defmethod black-vol ((m composite-market) strike (horizon real))
  (black-vol (market-view m) strike horizon))
(defmethod black-variance ((m composite-market) strike (horizon real))
  (black-variance (market-view m) strike horizon))
(defmethod discount-factor ((m composite-market) (horizon real))
  (discount-factor (market-view m) horizon))
(defmethod forward-factor ((m composite-market) (horizon real))
  (forward-factor (market-view m) horizon))
(defmethod carry-factor ((m composite-market) (horizon real))
  (carry-factor (market-view m) horizon))
(defmethod discrete-dividends ((m composite-market))
  (discrete-dividends (market-view m)))
(defmethod forward ((m composite-market) (horizon real))
  (forward (market-view m) horizon))

(defmethod derive-market ((v market-view) &key &allow-other-keys)
  (signal-invalid-argument 'market v 'composite-market
                           "a composite market: derive the market the view was taken from"))

(defmethod print-object ((v market-view) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~S of ~A, discounting on ~S" (view-underlying v)
            (valuation-date v) (view-discount-curve v))))
