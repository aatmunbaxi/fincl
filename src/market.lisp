;;;; market.lisp --- the market protocol, composite markets and views
;;;;
;;;; Engines never read market slots. They go through these generic
;;;; functions: SPOT, ZERO-RATE, DIVIDEND-YIELD, BLACK-VOL, BLACK-VARIANCE,
;;;; and the defaults built on them. That protocol describes one underlying
;;;; and one discount curve. A COMPOSITE-MARKET holds many, as named term
;;;; structures (src/term-structures/), and PRICE narrows it to a MARKET-VIEW
;;;; for the instrument being priced, so engines never see more than one.
;;;;
;;;; A market is an immutable snapshot: it is passed to every parallel
;;;; worker, so nothing may mutate it. Scenarios are new markets, not
;;;; edits to an existing one.
;;;;
;;;; Every protocol function taking a point in time accepts either a DATE or
;;;; a REAL. A real is a horizon in market time: the year fraction from the
;;;; valuation date under the market's day counter. Methods on DATE convert
;;;; with MARKET-TIME and delegate, so concrete markets implement only the
;;;; REAL methods.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(define-condition non-positive-volatility (pricing-error)
  ((volatility :initarg :volatility :initform nil :reader error-volatility)
   (where :initarg :where :reader error-where))
  (:report (lambda (c s)
             (format s "The volatility ~@[~,6F ~]~A is not positive; pricing needs a ~
positive volatility."
                     (error-volatility c) (error-where c))))
  (:documentation "A volatility of zero or below where a model needs a
positive one. PRICE offers USE-VALUE, to return a value as the price."))

(defclass market ()
  ()
  (:documentation "State of the world at a valuation date."))

;;; --------------------------------------------------------------------
;;; Time
;;; --------------------------------------------------------------------

(defgeneric valuation-date (market)
  (:documentation "The date the market is observed on."))

(defgeneric market-day-counter (market)
  (:documentation "The day counter that turns dates into market time."))

(defun market-time (market date)
  "Year fraction from MARKET's valuation date to DATE under its day counter.

  (market-time m #D\"2027-09-24\") => 1.0d0   ; valued 2026-09-24, ACT/365F"
  (year-fraction (market-day-counter market) (valuation-date market) date))

;;; --------------------------------------------------------------------
;;; The protocol
;;; --------------------------------------------------------------------

(defgeneric spot (market)
  (:documentation "Current price of the underlying."))

(defgeneric zero-rate (market x)
  (:documentation "Continuously compounded risk-free zero rate to X, a date
or a horizon in market time."))

(defgeneric dividend-yield (market x)
  (:documentation "Continuously compounded dividend yield to X, a date or a
horizon in market time."))

(defgeneric black-vol (market strike x)
  (:documentation "Black-Scholes volatility for STRIKE to X, a date or a
horizon in market time. A flat market ignores STRIKE and X; a surface
interpolates."))

(defgeneric black-variance (market strike x)
  (:documentation "Total Black variance for STRIKE to X, a date or a
horizon in market time: vol^2 * horizon for a flat market."))

;;; Derived quantities. Defaults are expressed in terms of the protocol
;;; above, so a market only has to implement the primitives; a curve
;;; market can still override these directly with its own interpolation.

(defgeneric discount-factor (market x)
  (:method ((m market) (horizon real))
    (exp (- (* (zero-rate m horizon) horizon)))))

(defgeneric forward-factor (market x)
  (:documentation "Growth factor of the underlying: exp((r - q) horizon).")
  (:method ((m market) (horizon real))
    (exp (* (- (zero-rate m horizon) (dividend-yield m horizon)) horizon))))

(defgeneric forward (market x)
  (:method ((m market) (horizon real))
    (* (spot m) (forward-factor m horizon))))

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
  (:documentation "An instrument or a view needs an entry the market does not
hold: an underlying, a curve, a vol structure."))

(define-condition currency-mismatch (pricing-error)
  ((underlying :initarg :underlying :reader error-underlying)
   (underlying-currency :initarg :underlying-currency :reader error-underlying-currency)
   (currency :initarg :currency :reader error-currency))
  (:report (lambda (c s)
             (format s "The instrument pays in ~S, but ~S trades in ~S: that is a quanto, ~
which needs an FX vol and correlation fincl does not model. Price it in ~S."
                     (error-currency c) (error-underlying c) (error-underlying-currency c)
                     (error-underlying-currency c)))))

(defclass composite-market (market)
  ((valuation-date :initarg :valuation-date :reader valuation-date :type date
                   :initform (a:required-argument :valuation-date))
   (day-counter :initarg :day-counter :reader market-day-counter
                :type day-counter :initform +actual-365-fixed+)
   (underlyings :initarg :underlyings :reader market-underlyings :type list
                :documentation "Underlying names, in the order given.")
   (spots :initarg :spots :reader market-spots :type hash-table)
   (yield-curves :initarg :yield-curves :reader market-yield-curves :type hash-table)
   (dividend-curves :initarg :dividend-curves :reader market-dividend-curves :type hash-table)
   (vol-structures :initarg :vol-structures :reader market-vol-structures :type hash-table)
   (underlying-currency :initarg :underlying-currency :reader market-underlying-currency
                        :type hash-table)
   (discount-curve-for :initarg :discount-curve-for :reader market-discount-curve-for
                       :type hash-table))
  (:documentation "Named term structures observed on one valuation date: a spot,
dividend curve, vol structure and currency per underlying, yield curves by
id, and the curve each currency discounts on. The tables are EQUAL hash
tables keyed by keywords. They are copied when a market is derived or bumped
and never mutated after construction, so a market can be shared by parallel
workers. Pricing an instrument narrows the market to a MARKET-VIEW of the
instrument's underlying."))

(defun market-entry (table key what known)
  (multiple-value-bind (value found) (gethash key table)
    (if found
        value
        (error 'missing-market-data :what what :key key :known known))))

(defun table-keys (table)
  (sort (loop for key being the hash-keys of table collect key) #'string<))

(defun alist-table (alist)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (key . value) in alist do (setf (gethash key table) value))
    table))

(defun as-structure (value class initarg)
  "VALUE if it is already a term structure, otherwise a flat CLASS of it."
  (if (typep value 'term-structure)
      value
      (make-instance class initarg value)))

(defun make-market (&key valuation-date (day-counter +actual-365-fixed+)
                         underlyings curves discounting)
  "A COMPOSITE-MARKET observed on VALUATION-DATE. The arguments are lists:

  UNDERLYINGS  ((name :spot s [:currency c] [:dividend q] [:vol v]) ...)
               S a positive real; C a keyword (default :DEFAULT); Q a real
               dividend yield or a DIVIDEND-CURVE (default 0); V a real Black
               vol or a BLACK-VOL-STRUCTURE (default none, for markets that
               need no vol).
  CURVES       ((id :rate r) ...) or ((id :curve c) ...): R a real zero rate,
               C a YIELD-CURVE.
  DISCOUNTING  ((currency . curve-id) ...): the curve each currency discounts
               on. If omitted and there is one curve, every currency
               discounts on it.

Reals are coerced to double-float.

  (make-market :valuation-date #D\"2026-09-24\"
               :underlyings '((:spx :spot 5000 :currency :usd :dividend 0.015d0 :vol 0.18d0)
                              (:ndx :spot 18000 :currency :usd :vol 0.22d0))
               :curves '((:usd-ois :rate 0.04d0))
               :discounting '((:usd . :usd-ois)))"
  (let ((spots '()) (dividends '()) (vols '()) (currencies '()) (names '()))
    (dolist (entry underlyings)
      (destructuring-bind (name &key spot (currency :default) (dividend 0d0) vol) entry
        (check-input name keyword "a keyword naming the underlying")
        (check-input spot (real (0)) "a positive spot")
        (check-input currency keyword "a currency keyword")
        (push name names)
        (push (cons name (float spot 1d0)) spots)
        (push (cons name currency) currencies)
        (push (cons name (as-structure dividend 'flat-dividend-curve :rate)) dividends)
        (when vol
          (push (cons name (as-structure vol 'flat-black-vol :vol)) vols))))
    (let* ((curve-alist (loop for (id &key rate curve) in curves
                              do (check-input id keyword "a keyword naming the curve")
                              collect (cons id (or curve (as-structure rate 'flat-yield-curve
                                                                       :rate)))))
           (discounting (or discounting
                            (and (= 1 (length curve-alist))
                                 (loop for currency in (remove-duplicates
                                                        (mapcar #'cdr currencies))
                                       collect (cons currency (car (first curve-alist))))))))
      (loop for (currency . id) in discounting
            unless (assoc id curve-alist)
              do (signal-invalid-argument 'discounting discounting 'list
                                          (format nil "currencies mapped to curves the market ~
holds (~{~S~^, ~})" (mapcar #'car curve-alist))))
      (make-instance 'composite-market
                     :valuation-date (or valuation-date (a:required-argument :valuation-date))
                     :day-counter day-counter
                     :underlyings (reverse names)
                     :spots (alist-table spots)
                     :yield-curves (alist-table curve-alist)
                     :dividend-curves (alist-table dividends)
                     :vol-structures (alist-table vols)
                     :underlying-currency (alist-table currencies)
                     :discount-curve-for (alist-table discounting)))))

(defun make-bs-market (&key valuation-date (day-counter +actual-365-fixed+)
                            spot rate (dividend 0d0) vol)
  "A flat Black-Scholes market: a COMPOSITE-MARKET with one underlying and one
currency, both :DEFAULT, discounting on the curve :DEFAULT. Numeric inputs
are coerced to double-float; pass double-float or rational literals, since
0.05 reads as a single-float and (float 0.05 1d0) is 0.05000000074505806d0.

  (make-bs-market :valuation-date #D\"2026-09-24\"
                  :spot 100 :rate 0.05d0 :vol 0.2d0)"
  (make-market :valuation-date valuation-date :day-counter day-counter
               :underlyings `((:default :spot ,spot :currency :default
                                        :dividend ,dividend :vol ,vol))
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
             :vol-structures ,(copy (market-vol-structures m))
             :underlying-currency ,(copy (market-underlying-currency m))
             :discount-curve-for ,(copy (market-discount-curve-for m))))))

(defgeneric derive-market (market &key &allow-other-keys)
  (:documentation "A new market equal to MARKET but observed on another
VALUATION-DATE or with another DAY-COUNTER (aging, conventions). MARKET is
not modified. Move market data with BUMP.

  (derive-market m :valuation-date #D\"2027-03-24\")"))

(defmethod derive-market ((m composite-market)
                          &key (valuation-date (valuation-date m))
                               (day-counter (market-day-counter m)))
  (copy-composite-market m :valuation-date valuation-date :day-counter day-counter))

(defun underlying-spot (market underlying)
  (market-entry (market-spots market) underlying "underlying" (market-underlyings market)))

(defun find-yield-curve (market id)
  (market-entry (market-yield-curves market) id "yield curve"
                (table-keys (market-yield-curves market))))

(defun discount-curve-id (market currency)
  (market-entry (market-discount-curve-for market) currency "discount curve for currency"
                (table-keys (market-discount-curve-for market))))

(defun resolve-underlying (market underlying)
  "UNDERLYING, checked against MARKET; NIL means the market's only one."
  (let ((known (market-underlyings market)))
    (cond (underlying (underlying-spot market underlying) underlying)
          ((= 1 (length known)) (first known))
          (t (error 'ambiguous-underlying :underlyings known)))))

;;; --------------------------------------------------------------------
;;; Bumps
;;; --------------------------------------------------------------------

(defun bump (market factor shift)
  "A new market with the risk FACTOR moved by SHIFT; MARKET is unchanged.
FACTOR is (:spot u), (:vol u), (:dividend u) for underlying U, or (:curve
id) for a yield curve. SHIFT is (:relative x), multiplying by 1 + x, or
(:absolute x), adding x: to the spot, the vol, or the rate.

  (bump m '(:spot :spx) '(:relative 1/100))     ; SPX spot up 1%
  (bump m '(:curve :usd-ois) '(:absolute 1/10000)) ; rates up 1bp"
  (check-input market composite-market "a composite market (bump the market a view was taken from)")
  (check-input factor (cons (member :spot :vol :dividend :curve) (cons keyword null))
               "(:spot u), (:vol u), (:dividend u) or (:curve id)")
  (check-input shift shift "(:relative x) or (:absolute x)")
  (destructuring-bind (kind key) factor
    (let ((new (copy-composite-market market)))
      (flet ((bump-entry (table what known)
               (setf (gethash key table)
                     (let ((old (market-entry table key what known)))
                       (if (realp old) (apply-shift old shift) (bump-structure old shift))))))
        (ecase kind
          (:spot (bump-entry (market-spots new) "underlying" (market-underlyings new)))
          (:vol (bump-entry (market-vol-structures new) "vol structure for"
                            (table-keys (market-vol-structures new))))
          (:dividend (bump-entry (market-dividend-curves new) "dividend curve for"
                                 (market-underlyings new)))
          (:curve (bump-entry (market-yield-curves new) "yield curve"
                              (table-keys (market-yield-curves new))))))
      new)))

(defmethod print-object ((m composite-market) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~A" (valuation-date m))
    (dolist (u (market-underlyings m))
      (format stream " ~S S=~,4F ~A ~A ~A" u (underlying-spot m u)
              (gethash u (market-underlying-currency m))
              (gethash u (market-dividend-curves m))
              (or (gethash u (market-vol-structures m)) "no vol")))
    (dolist (id (table-keys (market-yield-curves m)))
      (format stream " | ~S ~A" id (gethash id (market-yield-curves m))))))

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
   (vol-structure :initarg :vol-structure :reader view-vol-structure))
  (:documentation "One underlying of a composite market and one discount
curve, through the single-underlying market protocol that engines use. The
forward grows at the rate of the underlying currency's curve less the
dividend yield; DISCOUNT-FACTOR and ZERO-RATE come from the discount curve.
Made by MARKET-VIEW; PRICE makes one per instrument."))

(defun market-view (market &key underlying discount-curve)
  "A MARKET-VIEW of MARKET for UNDERLYING (NIL: the market's only underlying),
discounting on the curve DISCOUNT-CURVE (NIL: the curve of the underlying's
currency, which always sets the forward).

  (spot (market-view m :underlying :spx)) => 5000.0d0"
  (let* ((underlying (resolve-underlying market underlying))
         (forecast-id (discount-curve-id market (gethash underlying
                                                         (market-underlying-currency market))))
         (curve-id (or discount-curve forecast-id)))
    (make-instance 'market-view
                   :market market :underlying underlying :discount-curve curve-id
                   :spot (underlying-spot market underlying)
                   :yield-curve (find-yield-curve market curve-id)
                   :forecast-curve (find-yield-curve market forecast-id)
                   :dividend-curve (gethash underlying (market-dividend-curves market))
                   :vol-structure (gethash underlying (market-vol-structures market)))))

(defun view-vol (view)
  (or (view-vol-structure view)
      (error 'missing-market-data :what "vol structure for" :key (view-underlying view)
                                  :known (table-keys (market-vol-structures (view-market view))))))

(defmethod valuation-date ((v market-view)) (valuation-date (view-market v)))
(defmethod market-day-counter ((v market-view)) (market-day-counter (view-market v)))
(defmethod spot ((v market-view)) (view-spot v))
(defmethod zero-rate ((v market-view) (horizon real))
  (curve-zero-rate (view-yield-curve v) horizon))
(defmethod dividend-yield ((v market-view) (horizon real))
  (curve-dividend-yield (view-dividend-curve v) horizon))
(defmethod black-vol ((v market-view) strike (horizon real))
  (structure-black-vol (view-vol v) strike horizon))
(defmethod black-variance ((v market-view) strike (horizon real))
  (structure-black-variance (view-vol v) strike horizon))
(defmethod discount-factor ((v market-view) (horizon real))
  (curve-discount (view-yield-curve v) horizon))
(defmethod forward-factor ((v market-view) (horizon real))
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
(defmethod forward ((m composite-market) (horizon real))
  (forward (market-view m) horizon))

(defmethod derive-market ((v market-view) &key &allow-other-keys)
  (signal-invalid-argument 'market v 'composite-market
                           "a composite market: derive the market the view was taken from"))

(defmethod print-object ((v market-view) stream)
  (print-unreadable-object (v stream :type t)
    (format stream "~S of ~A, discounting on ~S" (view-underlying v)
            (valuation-date v) (view-discount-curve v))))
