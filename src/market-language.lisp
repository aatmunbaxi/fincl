;;;; market-language.lisp --- the market construction language
;;;;
;;;; (make-market (valued-on d) (underlying :spx (spot 5000) (vol 0.18d0)) ...)
;;;; builds a COMPOSITE-MARKET from clauses. MAKE-MARKET-FROM-CLAUSES takes the
;;;; same clauses as data. A call whose first argument is a keyword is the
;;;; keyword shape and goes to MAKE-MARKET-FROM-SPECS, which does the building
;;;; for both shapes, so the checks and conditions are the same.

(in-package #:fincl)

(define-fact :valuation-date date "The date a market is observed on.")
(define-fact :market-day-counter day-counter "The day counter a market measures time with.")
(define-fact :underlying-name keyword "The name of an underlying of a market.")
(define-fact :spot real "An underlying's spot price.")
(define-fact :underlying-currency keyword "The currency an underlying trades in.")
(define-fact :dividend-yield (or real dividend-curve) "An underlying's continuous dividend yield: a real or a dividend curve.")
(define-fact :dividend-entries (or list dividend-schedule)
  "An underlying's discrete dividends: a dividend schedule or its entries.")
(define-fact :vol (or real black-vol-structure) "An underlying's volatility: a real or a Black vol structure.")
(define-fact :curve-name keyword "The id of a yield curve of a market.")
(define-fact :curve-spec list
  "How a yield curve is given: (:rate r) for a flat zero rate or (:curve c) for a curve.")
(define-fact :discounting-currency keyword "A currency whose discount curve a market names.")
(define-fact :discount-curve-id keyword "The curve id a currency discounts on.")
(define-fact :fixing-index keyword "The id of an index with past fixings.")
(define-fact :fixing-entries list "Past fixings of an index: ((date rate) ...).")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun market-keyword-shape (clauses)
    "The expansion of (make-market :valuation-date ...), the keyword shape, or
NIL for a call written as clauses."
    (and (keywordp (first clauses))
         `(make-market-from-specs ,@clauses))))

(defun underlying-spec ()
  "The MAKE-MARKET-FROM-SPECS entry of the underlying in the current scope."
  (let ((dividends (optional-fact :dividend-entries))
        (vol (optional-fact :vol)))
    `(,(require-fact :underlying-name)
      :spot ,(require-fact :spot)
      :currency ,(or (optional-fact :underlying-currency) :default)
      :dividend ,(or (optional-fact :dividend-yield) 0d0)
      ,@(when dividends `(:dividend-schedule ,dividends))
      ,@(when vol `(:vol ,vol)))))

(defun curve-spec ()
  "The MAKE-MARKET-FROM-SPECS entry of the curve in the current scope."
  (destructuring-bind (key value) (require-fact :curve-spec)
    `(,(require-fact :curve-name) ,key ,value)))

(defun assemble-market ()
  (flet ((each (fact function)
           (iter (for scope in (scopes-with-fact fact))
                 (collect (in-scope (scope) (funcall function))))))
    (make-market-from-specs
     :valuation-date (require-fact :valuation-date)
     :day-counter (require-fact :market-day-counter)
     :underlyings (each :underlying-name #'underlying-spec)
     :curves (each :curve-name #'curve-spec)
     :discounting (each :discounting-currency
                        (lambda ()
                          `(,(require-fact :discounting-currency)
                            . ,(require-fact :discount-curve-id))))
     :fixings (each :fixing-index
                    (lambda ()
                      `(,(require-fact :fixing-index) ,@(require-fact :fixing-entries)))))))

(define-construction-language market
  (:documentation "Build a composite market from clauses: the date it is valued on,
underlyings with their spot, currency, dividends and vol, yield curves, the curve
each currency discounts on, and past index fixings.

  (make-market (valued-on #D\"2026-09-24\")
               (underlying :spx (spot 5000) (currency :usd) (dividend 0.015d0) (vol 0.18d0))
               (curve :usd-ois (rate 0.04d0))
               (discounting :usd :usd-ois))

A call whose first argument is a keyword is the keyword shape of
MAKE-MARKET-FROM-SPECS: (make-market :valuation-date d :underlyings '((:spx :spot 5000)) ...).
With one curve and no discounting clause, every currency discounts on it.")
  (:macro make-market)
  (:function make-market-from-clauses)
  (:other-shape market-keyword-shape)
  (:defaults :market-day-counter +actual-365-fixed+)
  (:assemble assemble-market))

(define-clause market valued-on (date)
  (:publishes :valuation-date)
  "The date the market is observed on, required. Replaces :VALUATION-DATE."
  (publish :valuation-date date))

(define-clause market day-counter (day-counter)
  (:publishes :market-day-counter)
  "The day counter the market measures time with, default ACT/365F. Replaces :DAY-COUNTER."
  (publish :market-day-counter day-counter))

(define-clause market underlying (name &clauses clauses)
  (:publishes :underlying-name)
  "An underlying named NAME, a keyword, built from the sub-clauses spot, currency,
dividend, dividends and vol. Replaces an entry of :UNDERLYINGS."
  (with-scope ((list :underlying name))
    (publish :underlying-name name)
    (apply-clauses clauses)))

(define-clause market spot (price)
  (:publishes :spot)
  "The spot price of an underlying, a positive real, required. Replaces :SPOT."
  (publish :spot price))

(define-clause market currency (name)
  (:publishes :underlying-currency)
  "The currency NAME, a keyword, an underlying trades in; default :DEFAULT. Replaces :CURRENCY."
  (publish :underlying-currency name))

(define-clause market dividend (yield-or-curve)
  (:publishes :dividend-yield)
  "The continuous dividend yield of an underlying: a real or a dividend curve; default 0.
Replaces :DIVIDEND."
  (publish :dividend-yield yield-or-curve))

(define-clause market dividends (entries-or-schedule)
  (:publishes :dividend-entries)
  "The discrete dividends of an underlying: a dividend schedule, or entries
((ex-date amount [:kind :cash|:proportional] [:pay-date d]) ...). Replaces :DIVIDEND-SCHEDULE."
  (publish :dividend-entries entries-or-schedule))

(define-clause market vol (vol-or-structure)
  (:publishes :vol)
  "The volatility of an underlying: a real Black vol or a Black vol structure; default none.
Replaces :VOL."
  (publish :vol vol-or-structure))

(define-clause market curve (id &clauses clauses)
  (:publishes :curve-name)
  "A yield curve named ID, a keyword, given by the sub-clause rate or structure.
Replaces an entry of :CURVES."
  (with-scope ((list :curve id))
    (publish :curve-name id)
    (apply-clauses clauses)))

(define-clause market rate (zero-rate)
  (:publishes :curve-spec)
  "A curve with the flat zero rate ZERO-RATE. Replaces :RATE in a :CURVES entry."
  (publish :curve-spec `(:rate ,zero-rate)))

(define-clause market structure (yield-curve)
  (:publishes :curve-spec)
  "A curve given as a yield curve object. Replaces :CURVE in a :CURVES entry."
  (publish :curve-spec `(:curve ,yield-curve)))

(define-clause market discounting (currency curve-id)
  (:publishes :discounting-currency :discount-curve-id)
  "The curve CURVE-ID that CURRENCY discounts on. Replaces an entry of :DISCOUNTING."
  (with-scope ((list :discounting currency))
    (publish :discounting-currency currency)
    (publish :discount-curve-id curve-id)))

(define-clause market fixings (index entries)
  (:publishes :fixing-index :fixing-entries)
  "Past fixings of the index INDEX, a keyword, as a quoted list ((date rate) ...),
DATE a date or a \"YYYY-MM-DD\" string. Replaces an entry of :FIXINGS."
  (with-scope ((list :fixings index))
    (publish :fixing-index index)
    (publish :fixing-entries entries)))

;;; --------------------------------------------------------------------
;;; Back to clauses
;;; --------------------------------------------------------------------

(defun flat-or-structure (structure flat-class reader)
  "The number of STRUCTURE if it is exactly the flat FLAT-CLASS, else STRUCTURE."
  (if (eq (class-name (class-of structure)) flat-class)
      (funcall reader structure)
      structure))

(defun underlying-clause (market name)
  `(:underlying ,name
    (:spot ,(underlying-spot market name))
    (:currency ,(gethash name (market-underlying-currency market)))
    ,@(let ((dividend (flat-or-structure (gethash name (market-dividend-curves market))
                                         'flat-dividend-curve #'flat-rate)))
        (unless (eql dividend 0d0) `((:dividend ,dividend))))
    ,@(a:when-let ((schedule (gethash name (market-dividend-schedules market))))
        `((:dividends ,(schedule-entries schedule))))
    ,@(a:when-let ((vol (gethash name (market-vol-structures market))))
        `((:vol ,(flat-or-structure vol 'flat-black-vol #'flat-vol))))))

(defun curve-clause (market id)
  (let ((curve (gethash id (market-yield-curves market))))
    `(:curve ,id ,(if (eq (class-name (class-of curve)) 'flat-yield-curve)
                      `(:rate ,(flat-rate curve))
                      `(:structure ,curve)))))

(defmethod object-clauses ((language (eql 'market)) (m composite-market))
  `((:valued-on ,(valuation-date m))
    ,@(unless (eq (market-day-counter m) +actual-365-fixed+)
        `((:day-counter ,(market-day-counter m))))
    ,@(mapcar (lambda (name) (underlying-clause m name))
            (sort (copy-list (market-underlyings m)) #'string<))
    ,@(mapcar (lambda (id) (curve-clause m id)) (sorted-keys (market-yield-curves m)))
    ,@(mapcar (lambda (currency)
                `(:discounting ,currency ,(gethash currency (market-discount-curve-for m))))
              (sorted-keys (market-discount-curve-for m)))
    ,@(mapcar (lambda (index)
                (let ((dates (gethash index (market-fixings m))))
                  `(:fixings ,index
                             ,(mapcar (lambda (date) `(,date ,(gethash date dates)))
                                      (sort (a:hash-table-keys dates) #'date<)))))
              (sorted-keys (market-fixings m)))))

(defmethod print-object ((m composite-market) stream)
  (print-clause-form 'market m stream))
