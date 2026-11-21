;;;; market-language.lisp --- the market construction language
;;;;
;;;; (make-market (valued-on d) (underlying :spx (spot 5000) (vol 0.18d0)) ...)
;;;; builds a COMPOSITE-MARKET from clauses. MAKE-MARKET-FROM-CLAUSES takes the same
;;;; clauses as data. A call whose first argument is a keyword is the keyword shape. It
;;;; goes to MAKE-MARKET-FROM-SPECS, which builds the market for both shapes. Thus the
;;;; checks and the conditions are the same.

(in-package #:fincl)

(define-fact :valuation-date date "The date a market is observed on.")
(define-fact :market-day-counter day-counter "The day counter a market measures time with.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun market-keyword-shape (clauses)
    "The expansion of (make-market :valuation-date ...), the keyword shape, or
NIL for a call written as clauses."
    (and (keywordp (first clauses))
         `(make-market-from-specs ,@clauses))))

(define-construction-language market
  (:documentation "Build a composite market from clauses. The clauses give the date on
which the market is valued, underlyings with their spot, currency, dividends and vol,
yield curves, the curve on which each currency discounts, and past index fixings.

  (make-market (valued-on #D\"2026-09-24\")
               (underlying :spx (spot 5000) (currency :usd) (dividend 0.015d0) (vol 0.18d0))
               (curve :usd-ois (rate 0.04d0))
               (discounting :usd :usd-ois))

A call whose first argument is a keyword has the keyword shape of
MAKE-MARKET-FROM-SPECS: (make-market :valuation-date d :underlyings '((:spx :spot 5000)) ...).
With one curve and no discounting clause, every currency discounts on that curve.")
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

(define-market-component-group underlying
  :clause (underlying (name &clauses clauses))
  :key-fact :underlying-name
  :key-documentation "The name of an underlying of a market."
  :documentation "An underlying named NAME, a keyword, built from the sub-clauses spot, currency,
dividend, dividends and vol. Replaces an entry of :UNDERLYINGS.")

(defun underlying-view (market underlying)
  "The view of MARKET a process driving a component of UNDERLYING steps on."
  (market-view market :underlying underlying))

(defun flat-or-structure (structure flat-class reader)
  "The number of STRUCTURE if it is exactly the flat FLAT-CLASS, else STRUCTURE."
  (if (eq (class-name (class-of structure)) flat-class)
      (funcall reader structure)
      structure))

(define-market-component :spot
  :type real :description "a real spot" :what "underlying"
  :storage 'market-spots :known 'market-underlyings :coerce 'real-to-double :view 'underlying-view
  :within (underlying spot (price)) :required t :spec '(:underlyings :spot)
  :fact-documentation "An underlying's spot price."
  :documentation "The spot price of an underlying, a positive real, required. Replaces :SPOT."
  :sealed t)

(define-market-component :currency
  :type keyword :description "a currency keyword" :what "currency of" :bumpable nil
  :storage 'market-underlying-currency :known 'market-underlyings
  :within (underlying currency (name)) :fact :underlying-currency :spec '(:underlyings :currency)
  :fact-documentation "The currency an underlying trades in."
  :documentation "The currency NAME, a keyword, an underlying trades in; default :DEFAULT. Replaces :CURRENCY."
  :sealed t)

(define-market-component :dividend
  :type dividend-curve :description "a DIVIDEND-CURVE" :what "dividend curve for"
  :input-type (or real dividend-curve)
  :storage 'market-dividend-curves :known 'market-underlyings :view 'underlying-view
  :within (underlying dividend (yield-or-curve)) :fact :dividend-yield :spec '(:underlyings :dividend)
  :print (lambda (curve)
           (let ((dividend (flat-or-structure curve 'flat-dividend-curve #'flat-rate)))
             (unless (eql dividend 0d0) `(,dividend))))
  :fact-documentation "An underlying's continuous dividend yield: a real or a dividend curve."
  :documentation "The continuous dividend yield of an underlying: a real or a dividend curve; default 0.
Replaces :DIVIDEND."
  :sealed t)

(define-market-component :dividend-schedule
  :type dividend-schedule :description "a DIVIDEND-SCHEDULE" :what "dividend schedule for"
  :input-type (or list dividend-schedule)
  :storage 'market-dividend-schedules :view 'underlying-view
  :within (underlying dividends (entries-or-schedule)) :fact :dividend-entries
  :spec '(:underlyings :dividend-schedule)
  :print (lambda (schedule) `(,(schedule-entries schedule)))
  :fact-documentation "An underlying's discrete dividends: a dividend schedule or its entries."
  :documentation "The discrete dividends of an underlying: a dividend schedule, or entries
((ex-date amount [:kind :cash|:proportional] [:pay-date d]) ...). Replaces :DIVIDEND-SCHEDULE."
  :sealed t)

(define-market-component :vol
  :type black-vol-structure :description "a BLACK-VOL-STRUCTURE" :what "vol structure for"
  :input-type (or real black-vol-structure)
  :storage 'market-vol-structures :view 'underlying-view
  :within (underlying vol (vol-or-structure)) :fact :vol :spec '(:underlyings :vol)
  :print (lambda (vol) `(,(flat-or-structure vol 'flat-black-vol #'flat-vol)))
  :fact-documentation "An underlying's volatility: a real or a Black vol structure."
  :documentation "The volatility of an underlying: a real Black vol or a Black vol structure; default none.
Replaces :VOL."
  :sealed t)

(defun curve-spec-entry (id spec)
  "The :CURVES entry of MAKE-MARKET-FROM-SPECS for the curve ID given by SPEC,
(:rate r) or (:structure c)."
  (destructuring-bind (part value) spec
    `(,id ,(if (eq part :structure) :curve :rate) ,value)))

(define-market-component :curve
  :type yield-curve :description "a YIELD-CURVE" :what "yield curve"
  :storage 'market-yield-curves
  :view (lambda (market id) (make-instance 'curve-view :market market :curve-id id))
  :clause (curve (id &clauses clauses)) :key-fact :curve-name :fact :curve-spec
  :parts ((rate (zero-rate) "A curve with the flat zero rate ZERO-RATE. Replaces :RATE in a :CURVES entry.")
          (structure (yield-curve) "A curve given as a yield curve object. Replaces :CURVE in a :CURVES entry."))
  :spec '(:curves curve-spec-entry)
  :print (lambda (curve)
           `(,(if (eq (class-name (class-of curve)) 'flat-yield-curve)
                  `(:rate ,(flat-rate curve))
                  `(:structure ,curve))))
  :key-documentation "The id of a yield curve of a market."
  :fact-documentation "How a yield curve is given: (:rate r) for a flat zero rate or (:structure c) for a curve."
  :documentation "A yield curve named ID, a keyword, given by the sub-clause rate or structure.
Replaces an entry of :CURVES."
  :sealed t)

(defun discounting-spec-entry (currency id)
  "The :DISCOUNTING entry of MAKE-MARKET-FROM-SPECS for CURRENCY discounting on ID."
  `(,currency . ,id))

(defun fixings-spec-entry (index entries)
  "The :FIXINGS entry of MAKE-MARKET-FROM-SPECS for INDEX's ENTRIES."
  `(,index ,@entries))

(define-market-component :discounting
  :type keyword :description "a curve id" :what "discount curve for currency" :bumpable nil
  :storage 'market-discount-curve-for
  :clause (discounting (currency curve-id)) :key-fact :discounting-currency :fact :discount-curve-id
  :spec '(:discounting discounting-spec-entry)
  :key-documentation "A currency whose discount curve a market names."
  :fact-documentation "The curve id a currency discounts on."
  :documentation "The curve CURVE-ID that CURRENCY discounts on. Replaces an entry of :DISCOUNTING."
  :sealed t)

(define-market-component :fixings
  :type hash-table :description "an EQUALP table of fixing date -> rate" :what "fixings of"
  :input-type list :bumpable nil
  :storage 'market-fixings
  :clause (fixings (index entries)) :key-fact :fixing-index :fact :fixing-entries
  :spec '(:fixings fixings-spec-entry)
  :print (lambda (dates)
           `(,(mapcar (lambda (date) `(,date ,(gethash date dates)))
                      (sort (a:hash-table-keys dates) #'date<))))
  :key-documentation "The id of an index with past fixings."
  :fact-documentation "Past fixings of an index: ((date rate) ...)."
  :documentation "Past fixings of the index INDEX, a keyword, as a quoted list ((date rate) ...),
DATE a date or a \"YYYY-MM-DD\" string. Replaces an entry of :FIXINGS."
  :sealed t)

(define-market-component :rates-vol
  :type rates-vol-structure :description "a RATES-VOL-STRUCTURE" :what "rates vol structure"
  :input 'ensure-rates-vol :input-type (or real rates-vol-structure)
  :clause (rates-vol (id structure)) :key-fact :rates-vol-id :fact :rates-vol
  :snapshot-constructors (make-flat-rates-vol make-expiry-rates-vol)
  :snapshot-classes (flat-rates-vol expiry-rates-vol)
  :key-documentation "The id of a rates vol structure of a market."
  :fact-documentation "A rates vol: a real for a flat vol or a rates vol structure."
  :documentation "The rates vol structure ID, a keyword: a real for a flat vol or a rates vol structure.
An engine given :VOLATILITY ID reads it from the market, so bumps and scenarios move it."
  :sealed t)

;;; --------------------------------------------------------------------
;;; Assembly
;;; --------------------------------------------------------------------

(defun clause-value (kind value)
  "VALUE, published by KIND's clause, through the kind's entry function."
  (a:if-let ((entry (market-component-kind-entry kind))) (funcall entry value) value))

(defun assemble-market ()
  "The market of the current fact table: each kind's entries collected from
the scopes of its clauses, the kinds the keyword shape takes as its keyword
arguments and the others as :COMPONENTS."
  (let ((arguments '()) (components '()))
    (labels ((add-argument (argument entry)
               (push entry (getf arguments argument)))
             (add (kind key value)
               (a:if-let ((spec (market-component-kind-spec kind)))
                 (add-argument (first spec) (funcall (second spec) key value))
                 (let ((cell (assoc (market-component-kind-name kind) components)))
                   (if cell
                       (push `(,key ,value) (cdr cell))
                       (push `(,(market-component-kind-name kind) (,key ,value)) components))))))
      (iter (for (nil group) in-hashtable *market-component-groups*)
            (for members = (mapcar (lambda (member) (find-market-component-kind (car member)))
                                   (market-component-group-members group)))
            (for key-fact = (market-component-group-key-fact group))
            (dolist (scope (scopes-with-fact key-fact))
              (in-scope (scope)
                (let ((name (require-fact key-fact)) (plist '()) (argument nil))
                  (dolist (kind members)
                    (let* ((fact (market-component-kind-fact kind))
                           (value (if (market-component-kind-required kind)
                                      (require-fact fact)
                                      (optional-fact fact)))
                           (spec (market-component-kind-spec kind)))
                      (cond ((null value))
                            (spec (setf argument (first spec)
                                        plist `(,@plist ,(second spec) ,value)))
                            (t (add kind name (clause-value kind value))))))
                  (when argument (add-argument argument `(,name ,@plist)))))))
      (dolist (kind (market-component-kinds))
        (when (and (market-component-kind-clause kind) (null (market-component-kind-group kind)))
          (dolist (scope (scopes-with-fact (market-component-kind-key-fact kind)))
            (in-scope (scope)
              (add kind (require-fact (market-component-kind-key-fact kind))
                   (clause-value kind (require-fact (market-component-kind-fact kind))))))))
      (apply #'make-market-from-specs
             :valuation-date (require-fact :valuation-date)
             :day-counter (require-fact :market-day-counter)
             :components (reverse (mapcar (lambda (cell) `(,(car cell) ,@(reverse (cdr cell)))) components))
             (iter (for (argument entries) in-plist arguments)
                   (collect argument)
                   (collect (reverse entries)))))))


;;; --------------------------------------------------------------------
;;; Back to clauses
;;; --------------------------------------------------------------------

(defun printing-kinds ()
  "The kinds with a clause in the order a market prints them: the sealed kinds
in definition order, then the others by name."
  (multiple-value-bind (sealed others)
      (iter (for kind in (market-component-kinds))
            (when (market-component-kind-clause kind)
              (if (market-component-kind-sealed kind)
                  (collect kind into sealed)
                  (collect kind into others)))
            (finally (return (values sealed others))))
    `(,@sealed ,@(sort others #'string< :key #'market-component-kind-name))))

(defun kind-clause (kind key value)
  "The clause giving VALUE as KIND's entry KEY, or NIL if the kind omits it."
  (let ((arguments (a:if-let ((print (market-component-kind-print kind)))
                     (funcall print value)
                     `(,value)))
        (head (a:make-keyword (symbol-name (market-component-kind-clause kind)))))
    (when arguments
      (if (market-component-kind-group kind)
          `(,head ,@arguments)
          `(,head ,key ,@arguments)))))

(defun kind-entries (market kind)
  "The (key . value) entries of KIND in MARKET, sorted by key."
  (a:when-let ((table (kind-table market kind)))
    (mapcar (lambda (key) `(,key . ,(gethash key table))) (sorted-keys table))))

(defun market-clauses-of-kinds (market)
  "MARKET's clauses after the valuation date and day counter, each with the
kinds it gives: ((clause kind ...) ...)."
  (let ((kinds (printing-kinds)) (done '()))
    (iter (for kind in kinds)
          (for group = (market-component-kind-group kind))
          (cond ((null group)
                 (dolist (entry (kind-entries market kind))
                   (a:when-let ((clause (kind-clause kind (car entry) (cdr entry))))
                     (collect `(,clause ,kind)))))
                ((not (member group done :test #'string=))
                 (push group done)
                 (let* ((g (gethash group *market-component-groups*))
                        (members (remove group kinds :key #'market-component-kind-group
                                                     :test-not #'equal))
                        (keys (sorted-keys (kind-table market (find-market-component-kind
                                                               (car (first (market-component-group-members g)))))))
                        (head (a:make-keyword (symbol-name (market-component-group-clause g)))))
                   (dolist (key keys)
                     (collect `((,head ,key
                                 ,@(iter (for member in members)
                                         (for table = (kind-table market member))
                                         (multiple-value-bind (value found)
                                             (if table (gethash key table) (values nil nil))
                                           (when found
                                             (a:when-let ((clause (kind-clause member key value)))
                                               (collect clause))))))
                                ,@members)))))))))

(defun market-date-clauses (market)
  "MARKET's valued-on clause, and its day-counter clause unless it is ACT/365F."
  `((:valued-on ,(valuation-date market))
    ,@(unless (eq (market-day-counter market) +actual-365-fixed+)
        `((:day-counter ,(market-day-counter market))))))

(defmethod object-clauses ((language (eql 'market)) (m composite-market))
  `(,@(market-date-clauses m) ,@(mapcar #'first (market-clauses-of-kinds m))))

(defmethod print-object ((m composite-market) stream)
  (print-clause-form 'market m stream))
