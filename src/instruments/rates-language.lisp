;;;; instruments/rates-language.lisp --- swaps, bonds and FRAs from clauses
;;;;
;;;; (make-swap (nominal 1d6)
;;;;            (fixed-leg (pays) (schedule s) (rate 0.033d0) (day-counter +actual-360+))
;;;;            (floating-leg (index (sofr)) (schedule s)))
;;;;
;;;; builds a swap. (make-bond ...) builds a fixed-rate bond (callable with a (callable
;;;; ...) clause). (make-fra ...) builds a forward rate agreement. The index of the
;;;; floating leg decides if a swap is vanilla or overnight indexed. The code checks a
;;;; clause that applies to one kind only against the kind that the index produced. The
;;;; old constructors (MAKE-VANILLA-SWAP and the others) call the data functions of these
;;;; languages.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Facts
;;; --------------------------------------------------------------------

(define-fact :nominal real "The nominal both legs of a swap are written on.")
(define-fact :leg-side (member :pays :receives) "Whether a leg of a swap is paid or received.")
(define-fact :schedule schedule "A schedule of payment periods.")
(define-fact :effective date "The date a schedule starts.")
(define-fact :termination (or date tenor)
  "The date a schedule ends, or the tenor from its start to its end.")
(define-fact :schedule-tenor tenor "The length of a schedule's periods."
  :coerce (lambda (f)
            (if (keywordp f) (make-tenor (/ 12 (frequency-periods f)) :months) f)))
(define-fact :termination-convention symbol "The business-day convention of a schedule's last date.")
(define-fact :schedule-rule (member :backward :forward :zero :twentieth)
  "How a schedule's dates are generated.")
(define-fact :first-date date "An explicit first date of a schedule, for a stub.")
(define-fact :next-to-last-date date "An explicit next-to-last date of a schedule, for a stub.")
(define-fact :fixed-rate (or real interest-rate) "A fixed coupon rate, or an interest rate.")
(define-fact :day-counter day-counter "The day counter a leg or bond accrues with.")
(define-fact :index ibor-index "The Ibor or overnight index a floating leg pays.")
(define-fact :spread real "A spread over the index.")
(define-fact :payment-lag (integer 0) "Business days between a period's end and its payment.")
(define-fact :payment-convention symbol "The business-day convention of payment dates.")
(define-fact :payment-calendar calendar "The calendar payment dates fall on.")
(define-fact :face (or real money) "A bond's face amount: a real (with a currency) or money.")
(define-fact :redemption real "A bond's redemption per 100 of face.")
(define-fact :settlement-days (integer 0) "Business days from a bond trade to its settlement.")
(define-fact :issue-date date "A bond's first settlement date.")
(define-fact :call-entries list "Call or put dates of a bond: ((date clean-price [:call|:put]) ...).")
(define-fact :value-date date "The date an FRA settles on.")
(define-fact :position (member :long :short) "The side of an FRA: long receives the rate.")
(define-fact :fra-notional real "The notional of an FRA.")
(define-fact :fra-maturity date "The date an FRA's period ends.")

;;; --------------------------------------------------------------------
;;; Parts shared by the languages
;;; --------------------------------------------------------------------

(defparameter *schedule-part-facts*
  '(:effective :termination :schedule-tenor :calendar :roll-convention :termination-convention
    :schedule-rule :end-of-month :first-date :next-to-last-date)
  "The facts a schedule is derived from when no clause gave it whole.")

(defun check-exclusive (whole parts)
  "Signal CONFLICTING-CLAUSES if explicit clauses gave WHOLE and also one of
PARTS in the current scope."
  (multiple-value-bind (whole-provenance whole-priority) (fact-provenance whole)
    (when (eq whole-priority :explicit)
      (dolist (part parts)
        (multiple-value-bind (provenance priority) (fact-provenance part)
          (when (eq priority :explicit)
            (error 'conflicting-clauses :fact whole :scope *scope*
                                        :earlier whole-provenance :later provenance)))))))

(defun resolve-schedule ()
  "The schedule of the current scope: the one a (schedule s) clause gave, or
the one built from the schedule clauses. Giving both is a conflict."
  (check-exclusive :schedule *schedule-part-facts*)
  (prog1 (require-fact :schedule)
    (consume-facts *schedule-part-facts*)))

(defun schedule-from-facts (effective termination tenor calendar convention termination-convention
                            rule end-of-month first-date next-to-last-date)
  "The schedule the schedule clauses describe; a TERMINATION that is a tenor
is added to EFFECTIVE."
  (apply #'make-schedule
         `(:effective ,effective
           :termination ,(if (typep termination 'tenor) (add-tenor effective termination) termination)
           :tenor ,tenor
           ,@(when calendar `(:calendar ,calendar))
           ,@(when convention `(:convention ,convention))
           ,@(when termination-convention `(:termination-convention ,termination-convention))
           ,@(when rule `(:rule ,rule))
           ,@(when end-of-month `(:end-of-month ,end-of-month))
           ,@(when first-date `(:first-date ,first-date))
           ,@(when next-to-last-date `(:next-to-last-date ,next-to-last-date)))))

(defmacro define-publishing-clauses (language &rest specs)
  "Define for each spec (name fact documentation) the clause (name name),
which publishes its argument as FACT."
  `(progn
     ,@(iter (for (name fact documentation) in specs)
             (collect `(define-clause ,language ,name (,name)
                         (:publishes ,fact)
                         ,documentation
                         (publish ,fact ,name))))))

(defmacro define-schedule-clauses (language)
  "Give LANGUAGE the clauses that give a schedule whole or in parts, and the
rule that builds it from the parts."
  `(progn
     (define-publishing-clauses ,language
       (schedule :schedule
        "A schedule object, instead of the schedule clauses effective, termination and frequency.")
       (effective :effective "The date the schedule starts.")
       (termination :termination "The date the schedule ends, or a tenor after its effective date.")
       (frequency :schedule-tenor
        "The length of the schedule's periods: a tenor, or a frequency keyword such as :semiannual.")
       (calendar :calendar "The calendar the schedule's dates are adjusted on.")
       (roll-convention :roll-convention "The business-day convention of the schedule's dates.")
       (termination-convention :termination-convention
        "The business-day convention of the schedule's last date.")
       (rule :schedule-rule "How the dates are generated: :backward, :forward, :zero or :twentieth.")
       (end-of-month :end-of-month "Whether month-end dates stay at month end.")
       (first-date :first-date "An explicit first date, giving a stub.")
       (next-to-last-date :next-to-last-date "An explicit next-to-last date, giving a stub."))
     (define-fact-rule ,language :schedule
         ((effective :effective) (termination :termination) (tenor :schedule-tenor)
          &optional (calendar :calendar nil) (convention :roll-convention nil)
          (termination-convention :termination-convention nil) (rule :schedule-rule nil)
          (end-of-month :end-of-month nil) (first-date :first-date nil)
          (next-to-last-date :next-to-last-date nil))
       (schedule-from-facts effective termination tenor calendar convention termination-convention
                            rule end-of-month first-date next-to-last-date))))

(defun check-kind-clauses (kind-name facts applies-to)
  "Signal CLAUSE-ARGUMENT-ERROR if one of FACTS was given by a clause in the
current scope: the clause applies to APPLIES-TO instances only, and the
object being built is of KIND-NAME."
  (dolist (fact facts)
    (when (fact-set-here-p fact)
      (error 'clause-argument-error
             :format-control "~A applies to ~A only, but the clauses build ~A."
             :format-arguments `(,(describe-provenance (fact-provenance fact)) ,applies-to
                                 ,kind-name)))))

;;; --------------------------------------------------------------------
;;; Swaps
;;; --------------------------------------------------------------------

(defparameter *ois-only-facts* '(:payment-lag :payment-calendar)
  "Swap facts that apply to overnight indexed swaps only.")

(defun swap-type-from-sides ()
  "The swap type (:payer or :receiver) the (pays) and (receives) clauses of
the two legs give."
  (let ((fixed (with-scope (:fixed) (optional-fact :leg-side)))
        (floating (with-scope (:floating) (optional-fact :leg-side))))
    (let ((from-fixed (case fixed (:pays :payer) (:receives :receiver)))
          (from-floating (case floating (:pays :receiver) (:receives :payer))))
      (when (and from-fixed from-floating (not (eq from-fixed from-floating)))
        (error 'clause-argument-error
               :format-control "The fixed leg ~(~A~) and the floating leg ~(~A~): one leg is paid and the other received."
               :format-arguments `(,fixed ,floating)))
      (or from-fixed from-floating
          (error 'missing-fact :fact :leg-side :scope '(:fixed)
                               :suppliers '("(pays)" "(receives)"))))))

(defun legs-nominal ()
  "The nominal the legs share."
  (let ((fixed (with-scope (:fixed) (require-fact :nominal)))
        (floating (with-scope (:floating) (require-fact :nominal))))
    (unless (= fixed floating)
      (error 'clause-argument-error
             :format-control "The legs of a swap share one nominal, but the fixed leg has ~S and the floating leg ~S."
             :format-arguments `(,fixed ,floating)))
    fixed))

(defun swap-kind-description (index ois)
  "A phrase naming the kind of swap INDEX makes, for reports."
  (format nil "~A swap (the index ~(~A~) is ~A)" (if ois "an overnight indexed" "a vanilla")
          (index-name index) (if ois "an overnight index" "an Ibor index")))

(defun check-swap-kind-clauses (kind ois)
  "Signal CLAUSE-ARGUMENT-ERROR for a clause that applies only to the other
kind of swap than the one being built (KIND describes it, OIS is true for an
overnight indexed swap)."
  (if ois
      (with-scope (:floating)
        (check-kind-clauses kind '(:day-counter) "vanilla swaps"))
      (dolist (scope '(() (:fixed) (:floating)))
        (let ((*scope* scope))
          (check-kind-clauses kind *ois-only-facts* "overnight indexed swaps")))))

(defun check-shared-ois-schedule (fixed-schedule floating-schedule)
  "Signal CLAUSE-ARGUMENT-ERROR unless the two legs of an overnight indexed
swap have the same dates."
  (unless (or (eq fixed-schedule floating-schedule)
              (equalp (schedule-dates fixed-schedule) (schedule-dates floating-schedule)))
    (error 'clause-argument-error
           :format-control "An overnight indexed swap has one schedule for both legs, but the fixed leg has ~S and the floating leg ~S."
           :format-arguments `(,fixed-schedule ,floating-schedule))))

(defun assemble-swap ()
  (let* ((index (with-scope (:floating) (require-fact :index)))
         (ois (typep index 'overnight-index)))
    (check-swap-kind-clauses (swap-kind-description index ois) ois)
    (let* ((type (swap-type-from-sides))
           (nominal (legs-nominal))
           (fixed-schedule (with-scope (:fixed) (resolve-schedule)))
           (floating-schedule (with-scope (:floating) (resolve-schedule)))
           (fixed-rate (with-scope (:fixed) (require-fact :fixed-rate)))
           (fixed-day-counter (with-scope (:fixed) (require-fact :day-counter)))
           (spread (or (with-scope (:floating) (optional-fact :spread)) 0))
           (payment-convention (optional-fact :payment-convention))
           (currency (index-currency index)))
      (check-input fixed-rate real "a real fixed rate")
      (check-input spread real "a real spread")
      (when ois (check-shared-ois-schedule fixed-schedule floating-schedule))
      (let* ((lag (and ois (or (optional-fact :payment-lag) 0)))
             (calendar (and ois (optional-fact :payment-calendar)))
             (convention (if ois
                             (or payment-convention :following)
                             (or payment-convention (schedule-convention floating-schedule))))
             (floating-schedule (if ois fixed-schedule floating-schedule))
             (floating-day-counter (and (not ois)
                                        (with-scope (:floating) (optional-fact :day-counter))))
             (fixed (fixed-leg :schedule fixed-schedule :notional nominal :rate fixed-rate :day-counter fixed-day-counter
                               :currency currency :payment-convention convention
                               :payment-lag (or lag 0) :payment-calendar calendar))
             (floating (floating-leg :schedule floating-schedule :notional nominal :index index :spread spread
                                     :day-counter floating-day-counter
                                     :payment-convention convention
                                     :payment-lag (or lag 0) :payment-calendar calendar)))
        (apply #'make-instance (if ois 'ois-swap 'vanilla-swap)
               :type type :nominal (float nominal 1d0)
               :fixed-rate (float fixed-rate 1d0) :spread (float spread 1d0)
               :index index :currency currency :fixed-schedule fixed-schedule
               :floating-schedule floating-schedule
               :fixed-day-counter fixed-day-counter
               :floating-day-counter floating-day-counter
               :payment-convention convention
               :legs `(,fixed ,floating) :signs (swap-signs-for type)
               (when ois `(:payment-lag ,lag :payment-calendar ,calendar)))))))

(define-construction-language swap
  (:documentation "Build a swap of a fixed leg against a floating leg. The clauses give the
nominal, the two legs with their schedules, the fixed rate, the day counter and the
index. An overnight index on the floating leg makes an overnight indexed swap. The legs
share the schedule, the payment lag and the calendar. An Ibor index makes a vanilla
swap. Each leg has its own schedule. A clause that is given outside a leg applies to both
legs.

  (make-swap (nominal 1d6)
             (fixed-leg (pays) (schedule s) (rate 0.033d0) (day-counter +actual-360+))
             (floating-leg (index (sofr)) (schedule s)))")
  (:macro make-swap)
  (:function make-swap-from-clauses)
  (:assemble assemble-swap))

(define-schedule-clauses swap)

(define-clause swap fixed-leg (&clauses clauses)
  "The fixed leg. Its sub-clauses apply to it alone: pays or receives, rate, day-counter,
schedule or the schedule clauses. Replaces the fixed-leg positional arguments of
MAKE-VANILLA-SWAP and MAKE-OIS-SWAP."
  (with-scope (:fixed) (apply-clauses clauses)))

(define-clause swap floating-leg (&clauses clauses)
  "The floating leg. Its sub-clauses apply to it alone: index, spread, schedule or the
schedule clauses, and pays or receives. Replaces the index, spread and floating schedule
arguments of MAKE-VANILLA-SWAP and MAKE-OIS-SWAP."
  (with-scope (:floating) (apply-clauses clauses)))

(define-clause swap pays ()
  (:publishes :leg-side)
  "The leg that pays: with it on the fixed leg the swap is a payer swap. Replaces TYPE :PAYER."
  (publish :leg-side :pays))

(define-clause swap receives ()
  (:publishes :leg-side)
  "The leg that receives: with it on the fixed leg the swap is a receiver swap. Replaces TYPE :RECEIVER."
  (publish :leg-side :receives))

(define-publishing-clauses swap
  (nominal :nominal "The nominal of both legs. Replaces NOMINAL.")
  (rate :fixed-rate "The fixed rate of the fixed leg. Replaces FIXED-RATE.")
  (day-counter :day-counter
   "The day counter of a leg: of the fixed leg, or of a vanilla swap's floating leg (default
the index's). Replaces FIXED-DAY-COUNTER and FLOATING-DAY-COUNTER.")
  (index :index "The index the floating leg pays. Replaces INDEX.")
  (spread :spread "The spread over the index, default 0. Replaces SPREAD.")
  (payment-lag :payment-lag
   "Business days after each period's end that it pays, default 0, for overnight indexed swaps.
Replaces PAYMENT-LAG of MAKE-OIS-SWAP.")
  (payment-convention :payment-convention
   "The convention payment dates are adjusted by. Replaces PAYMENT-CONVENTION.")
  (payment-calendar :payment-calendar
   "The calendar payment dates fall on, default the schedule's, for overnight indexed swaps.
Replaces PAYMENT-CALENDAR of MAKE-OIS-SWAP."))

(defun leg-clauses (swap leg)
  "The clauses of one leg of SWAP that are not shared."
  (ecase leg
    (:fixed `(:fixed-leg (,(if (eq (swap-type swap) :payer) :pays :receives))
                         ,@(unless (typep swap 'ois-swap) `((:schedule ,(swap-fixed-schedule swap))))
                         (:rate ,(swap-fixed-rate swap))
                         (:day-counter ,(swap-fixed-day-counter swap))))
    (:floating `(:floating-leg (:index ,(swap-index swap))
                               ,@(unless (typep swap 'ois-swap)
                                   `((:schedule ,(swap-floating-schedule swap))))
                               (:spread ,(swap-spread swap))
                               ,@(when (swap-floating-day-counter swap)
                                   `((:day-counter ,(swap-floating-day-counter swap))))))))

(defmethod object-clauses ((language (eql 'swap)) (s fixed-vs-floating-swap))
  `((:nominal ,(swap-nominal s))
    ,@(when (typep s 'ois-swap)
        `((:schedule ,(swap-fixed-schedule s))
          (:payment-lag ,(swap-payment-lag s))
          ,@(when (swap-payment-calendar s) `((:payment-calendar ,(swap-payment-calendar s))))))
    (:payment-convention ,(swap-payment-convention s))
    ,(leg-clauses s :fixed)
    ,(leg-clauses s :floating)))

(defmethod print-object ((s fixed-vs-floating-swap) stream)
  (print-clause-form 'swap s stream))

;;; --------------------------------------------------------------------
;;; Conventions: the terms in swap-conventions.lisp as clauses
;;; --------------------------------------------------------------------

(defun swap-convention-clauses (name)
  "The swap-language clauses (data) of the named swap convention NAME, in the
terms the helpers read from the same table."
  (let* ((terms (swap-convention-terms name))
         (index (getf terms :index))
         (calendar (index-fixing-calendar index)))
    (ecase (getf terms :kind)
      (:ois
       (let ((convention (getf terms :roll-convention)))
         `((:calendar ,calendar) (:roll-convention ,convention)
           (:termination-convention ,convention) (:rule :backward)
           (:frequency ,(getf terms :payment-frequency))
           (:payment-lag ,(getf terms :payment-lag))
           (:payment-convention ,(getf terms :payment-convention))
           (:fixed-leg (:day-counter ,(index-day-counter index)))
           (:floating-leg (:index ,index)))))
      (:vanilla
       (let ((fixed-convention (getf terms :fixed-convention))
             (floating-convention (index-convention index)))
         `((:calendar ,calendar) (:rule :backward) (:payment-convention ,floating-convention)
           (:fixed-leg (:frequency ,(getf terms :fixed-frequency))
                       (:roll-convention ,fixed-convention)
                       (:termination-convention ,fixed-convention)
                       (:day-counter ,(getf terms :fixed-day-counter)))
           (:floating-leg (:index ,index) (:frequency ,(index-tenor index))
                          (:roll-convention ,floating-convention)
                          (:termination-convention ,floating-convention))))))))

(dolist (name *named-swap-conventions*)
  (set-convention 'swap name (swap-convention-clauses name)))

;;; --------------------------------------------------------------------
;;; Bonds
;;; --------------------------------------------------------------------

(defun assemble-bond ()
  (let* ((schedule (resolve-schedule))
         (coupon (require-fact :fixed-rate))
         (day-counter (if (interest-rate-p coupon)
                          (optional-fact :day-counter)
                          (require-fact :day-counter)))
         (face (require-fact :face))
         (currency (optional-fact :currency))
         (settlement-days (require-fact :settlement-days))
         (payment-convention (require-fact :payment-convention))
         (redemption (require-fact :redemption))
         (issue-date (optional-fact :issue-date))
         (payment-calendar (optional-fact :payment-calendar))
         (call-entries (optional-fact :call-entries)))
    (check-input settlement-days (integer 0) "a whole number of business days")
    (check-input redemption real "a real redemption per 100 of face")
    (when call-entries
      (check-input call-entries (and list (not null)) "a non-empty list of (date clean-price [type])"))
    (multiple-value-bind (nominal currency) (notional-and-currency face currency)
      (let* ((calendar (or payment-calendar (schedule-calendar schedule)))
             (coupons (fixed-leg :schedule schedule :notional (make-money nominal currency) :rate coupon :day-counter day-counter
                                 :payment-convention payment-convention :payment-calendar calendar))
             (redemption (float redemption 1d0))
             (maturity (payment-date (a:lastcar coupons)))
             (bond (make-instance 'fixed-rate-bond
                                  :currency currency :face nominal :settlement-days settlement-days
                                  :calendar calendar :issue-date issue-date :schedule schedule
                                  :coupon (fixed-coupon-rate (first coupons))
                                  :payment-convention payment-convention :redemption redemption
                                  :cashflows `(,@coupons
                                               ,(make-instance 'simple-cashflow
                                                               :payment-date maturity :currency currency
                                                               :amount (/ (* nominal redemption) 100d0))))))
        (if call-entries
            (change-class bond 'callable-fixed-rate-bond
                          :call-schedule (sort (mapcar #'check-call-entry call-entries)
                                               #'date< :key #'first))
            bond)))))

(defun check-call-entry (entry)
  "ENTRY, (date clean-price [type]), as (date price type) with a double price."
  (destructuring-bind (date price &optional (type :call)) entry
    (check-input date date "a call date")
    (check-input price real "a clean price per 100 of face")
    (check-input type (member :call :put) ":CALL or :PUT")
    `(,date ,(float price 1d0) ,type)))

(define-construction-language bond
  (:documentation "Build a fixed-rate bond. The clauses give its coupon, day counter,
schedule and face. They can also give the redemption, the settlement days, the issue
date and the call dates. A callable clause makes the bond callable. A lattice engine
prices it.

  (make-bond (coupon 0.04375d0) (day-counter dc) (schedule s) (currency :usd)
             (settlement-days 1))")
  (:macro make-bond)
  (:function make-bond-from-clauses)
  (:defaults :face 100 :redemption 100 :settlement-days 2 :payment-convention :following)
  (:assemble assemble-bond))

(define-schedule-clauses bond)

(define-publishing-clauses bond
  (coupon :fixed-rate
   "The coupon: a simple annual rate under the day-counter, or an interest rate. Replaces COUPON.")
  (day-counter :day-counter "The day counter the coupon accrues under. Replaces DAY-COUNTER.")
  (face :face "The face amount: a real with a currency clause, or money; default 100. Replaces :FACE.")
  (currency :currency "The currency of a real face. Replaces :CURRENCY.")
  (redemption :redemption "The redemption per 100 of face, paid with the last coupon; default 100. Replaces :REDEMPTION.")
  (settlement-days :settlement-days "Business days from a trade to its settlement; default 2. Replaces :SETTLEMENT-DAYS.")
  (issue-date :issue-date "The first settlement date. Replaces :ISSUE-DATE.")
  (payment-convention :payment-convention "The convention payment dates are adjusted by; default :following. Replaces :PAYMENT-CONVENTION.")
  (payment-calendar :payment-calendar "The calendar payments and settlement use, default the schedule's. Replaces :PAYMENT-CALENDAR.")
  (callable :call-entries
   "Call or put dates, a quoted list ((date clean-price [:call|:put]) ...), prices per 100 of face; makes the bond callable. Replaces CALL-SCHEDULE of MAKE-CALLABLE-FIXED-RATE-BOND."))

(defmethod object-clauses ((language (eql 'bond)) (b fixed-rate-bond))
  (let ((coupon (bond-coupon b)))
    `((:schedule ,(bond-schedule b))
      ,@(if (and (eq (interest-rate-compounding coupon) :simple)
                 (eq (interest-rate-frequency coupon) :annual))
            `((:coupon ,(interest-rate-rate coupon))
              (:day-counter ,(interest-rate-day-counter coupon)))
            `((:coupon ,coupon)))
      (:face ,(bond-face b))
      ,@(when (instrument-currency b) `((:currency ,(instrument-currency b))))
      (:settlement-days ,(bond-settlement-days b))
      (:redemption ,(bond-redemption b))
      (:payment-convention ,(bond-payment-convention b))
      (:payment-calendar ,(bond-calendar b))
      ,@(when (bond-issue-date b) `((:issue-date ,(bond-issue-date b))))
      ,@(when (typep b 'callable-fixed-rate-bond) `((:callable ,(bond-call-schedule b)))))))

(defmethod print-object ((b fixed-rate-bond) stream)
  (print-clause-form 'bond b stream))

;;; --------------------------------------------------------------------
;;; Forward rate agreements
;;; --------------------------------------------------------------------

(defun assemble-fra ()
  (let ((index (require-fact :index))
        (value-date (require-fact :value-date))
        (position (require-fact :position))
        (strike (require-fact :strike))
        (notional (require-fact :fra-notional))
        (maturity-date (optional-fact :fra-maturity)))
    (check-input index ibor-index "an Ibor index")
    (check-input notional (real (0)) "a positive notional")
    (let ((maturity (adjust (index-fixing-calendar index)
                            (or maturity-date (index-maturity-date index value-date))
                            (index-convention index))))
      (unless (date< value-date maturity)
        (signal-invalid-argument 'maturity-date maturity 'date (format nil "a date after ~A" value-date)))
      (make-instance 'forward-rate-agreement
                     :index index :value-date value-date :maturity-date maturity :position position
                     :strike strike :notional (float notional 1d0)
                     :indexed (null maturity-date) :currency (index-currency index)))))

(define-construction-language fra
  (:documentation "Build a forward rate agreement on an Ibor index. The clauses give the
value date, the side, the strike and the notional. Without a maturity clause, the period
is the tenor of the index and the rate is the fixing of the index. With a maturity
clause, the period ends on that date and the rate is the forward over the period.

  (make-fra (index (euribor #T\"6M\")) (value-date #D\"2026-12-29\") (long) (strike 0.02d0)
            (notional 1d6))")
  (:macro make-fra)
  (:function make-fra-from-clauses)
  (:assemble assemble-fra))

(define-publishing-clauses fra
  (index :index "The Ibor index the FRA is on. Replaces INDEX.")
  (value-date :value-date "The date the FRA settles on. Replaces VALUE-DATE.")
  (strike :strike "The simple rate the FRA is struck at. Replaces STRIKE.")
  (notional :fra-notional "The notional, positive. Replaces NOTIONAL.")
  (maturity :fra-maturity "The end of the period, adjusted; makes the rate the forward over it. Replaces :MATURITY-DATE."))

(define-clause fra long ()
  (:publishes :position)
  "The long side, which receives the rate. Replaces POSITION :LONG."
  (publish :position :long))

(define-clause fra short ()
  (:publishes :position)
  "The short side. Replaces POSITION :SHORT."
  (publish :position :short))

(defmethod object-clauses ((language (eql 'fra)) (f forward-rate-agreement))
  `((:index ,(fra-index f))
    (:value-date ,(fra-value-date f))
    (,(fra-position f))
    (:strike ,(fra-strike f))
    (:notional ,(fra-notional f))
    ,@(unless (fra-indexed-p f) `((:maturity ,(fra-maturity-date f))))))

(defmethod print-object ((f forward-rate-agreement) stream)
  (print-clause-form 'fra f stream))

;;; --------------------------------------------------------------------
;;; The constructors that take their arguments positionally
;;; --------------------------------------------------------------------

(declaim (ftype (function (t t t t t t t &key (:spread t) (:floating-day-counter t) (:payment-convention t))
                    (values vanilla-swap &optional))
          make-vanilla-swap))
(defun make-vanilla-swap (type nominal fixed-schedule fixed-rate fixed-day-counter
                          float-schedule index
                          &key (spread 0) floating-day-counter payment-convention)
  "Make a vanilla swap. It pays fixed coupons at FIXED-RATE under FIXED-DAY-COUNTER on
FIXED-SCHEDULE. It receives par Ibor coupons on INDEX plus SPREAD on FLOAT-SCHEDULE
(FLOATING-DAY-COUNTER, default: the day counter of the index). Both legs use NOMINAL
in the currency of the index. TYPE is :PAYER or :RECEIVER of the fixed leg.
PAYMENT-CONVENTION adjusts the payment dates. The default is the convention of the
floating schedule. The function calls MAKE-SWAP-FROM-CLAUSES.

  (make-vanilla-swap :payer 1d6 fixed 0.024d0 (make-instance 'thirty-360 :variant :bond-basis)
                     floating (euribor #T\"6M\"))"
  (check-input type swap-type ":PAYER (pays the fixed leg) or :RECEIVER")
  (check-input index ibor-index "an Ibor index")
  (make-swap-from-clauses
   `((:nominal ,nominal)
     ,@(when payment-convention `((:payment-convention ,payment-convention)))
     (:fixed-leg (,(if (eq type :payer) :pays :receives)) (:schedule ,fixed-schedule)
                 (:rate ,fixed-rate) (:day-counter ,fixed-day-counter))
     (:floating-leg (:index ,index) (:schedule ,float-schedule) (:spread ,spread)
                    ,@(when floating-day-counter `((:day-counter ,floating-day-counter)))))))

(declaim (ftype (function (t t t t t t &key (:spread t) (:payment-lag t) (:payment-convention t) (:payment-calendar t))
                    (values ois-swap &optional))
          make-ois-swap))
(defun make-ois-swap (type nominal schedule fixed-rate fixed-day-counter index
                      &key (spread 0) (payment-lag 0) (payment-convention :following) payment-calendar)
  "Make an overnight indexed swap. It pays fixed coupons at FIXED-RATE under
FIXED-DAY-COUNTER. It receives daily compounded coupons on the overnight INDEX plus
SPREAD. Both legs use SCHEDULE and NOMINAL. Each period pays PAYMENT-LAG business days
after its end, on PAYMENT-CALENDAR (default: the calendar of the schedule), adjusted
by PAYMENT-CONVENTION. The function calls MAKE-SWAP-FROM-CLAUSES.

  (make-ois-swap :payer 1d6 schedule 0.033d0 +actual-360+ (sofr) :payment-lag 2)"
  (check-input type swap-type ":PAYER (pays the fixed leg) or :RECEIVER")
  (check-input index overnight-index "an overnight index")
  (make-swap-from-clauses
   `((:nominal ,nominal) (:schedule ,schedule)
     (:payment-lag ,payment-lag) (:payment-convention ,payment-convention)
     ,@(when payment-calendar `((:payment-calendar ,payment-calendar)))
     (:fixed-leg (,(if (eq type :payer) :pays :receives)) (:rate ,fixed-rate)
                 (:day-counter ,fixed-day-counter))
     (:floating-leg (:index ,index) (:spread ,spread)))))

(defun fixed-rate-bond-clauses (schedule coupon day-counter face currency settlement-days
                                payment-convention redemption issue-date payment-calendar)
  `((:schedule ,schedule) (:coupon ,coupon) (:day-counter ,day-counter) (:face ,face)
    ,@(when currency `((:currency ,currency)))
    (:settlement-days ,settlement-days) (:payment-convention ,payment-convention)
    (:redemption ,redemption)
    ,@(when issue-date `((:issue-date ,issue-date)))
    ,@(when payment-calendar `((:payment-calendar ,payment-calendar)))))

(declaim (ftype (function (t t t &key (:face t) (:currency t) (:settlement-days t) (:payment-convention t) (:redemption t) (:issue-date t) (:payment-calendar t))
                    (values fixed-rate-bond &optional))
          make-fixed-rate-bond))
(defun make-fixed-rate-bond (schedule coupon day-counter
                             &key (face 100) currency (settlement-days 2)
                                  (payment-convention :following) (redemption 100) issue-date
                                  payment-calendar)
  "Make a fixed-rate bond. COUPON is a simple annual rate under DAY-COUNTER (or an
INTEREST-RATE). FACE is a real (with CURRENCY) or MONEY. The bond pays over the periods
of SCHEDULE, on dates that PAYMENT-CONVENTION adjusts on PAYMENT-CALENDAR (default: the
calendar of the schedule, which is also the settlement calendar). REDEMPTION per 100
of face is paid with the last coupon. The function calls MAKE-BOND-FROM-CLAUSES.

  (make-fixed-rate-bond (make-schedule :effective #D\"2024-05-15\" :termination #D\"2034-05-15\" :tenor #T\"6M\"
                                       :calendar +united-states-government-bond+
                                       :convention :unadjusted)
                        0.04375d0 (make-instance 'actual-actual-icma :schedule s)
                        :currency :usd :settlement-days 1)"
  (make-bond-from-clauses
   (fixed-rate-bond-clauses schedule coupon day-counter face currency settlement-days
                            payment-convention redemption issue-date payment-calendar)))

(defparameter *bond-keys*
  '(:face :currency :settlement-days :payment-convention :redemption :issue-date :payment-calendar)
  "The keys of MAKE-FIXED-RATE-BOND and MAKE-CALLABLE-FIXED-RATE-BOND. Both lambda
lists name the same keys, and this list must match them: MAKE-CALLABLE-FIXED-RATE-BOND
accepts any key in its lambda list so that an unknown one signals INVALID-ARGUMENT
here, where a plain keyword list would signal a PROGRAM-ERROR.")

(defun check-known-keys (keys valid)
  "Signal INVALID-ARGUMENT for the first key of the property list KEYS that is
not among VALID."
  (iter (for (key nil) in-plist keys)
        (unless (member key valid)
          (signal-invalid-argument 'keys key 'keyword
                                   (format nil "one of ~{~S~^, ~}" valid)))))

(declaim (ftype (function (t t t t &rest t &key (:face t) (:currency t) (:settlement-days t) (:payment-convention t) (:redemption t) (:issue-date t) (:payment-calendar t) &allow-other-keys)
                    (values callable-fixed-rate-bond &optional))
          make-callable-fixed-rate-bond))
(defun make-callable-fixed-rate-bond (schedule coupon day-counter call-schedule
                                      &rest keys
                                      &key (face 100) currency (settlement-days 2)
                                           (payment-convention :following) (redemption 100)
                                           issue-date payment-calendar
                                      &allow-other-keys)
  "Make a fixed-rate bond like MAKE-FIXED-RATE-BOND, with the same keys and no others. Any
other key signals INVALID-ARGUMENT. The holder can call or put the bond on
CALL-SCHEDULE, a list of (date clean-price) or (date clean-price type). TYPE is :CALL
(default) or :PUT. The prices are per 100 of face. The function calls
MAKE-BOND-FROM-CLAUSES.

  (make-callable-fixed-rate-bond schedule 0.03d0 dc `((,#D\"2029-09-28\" 100) (,#D\"2030-09-30\" 100))
                                 :currency :eur)"
  (check-known-keys keys *bond-keys*)
  (check-input call-schedule (and list (not null)) "a non-empty list of (date clean-price [type])")
  (make-bond-from-clauses
   `(,@(fixed-rate-bond-clauses schedule coupon day-counter face currency settlement-days
                                payment-convention redemption issue-date payment-calendar)
     (:callable ,call-schedule))))

(declaim (ftype (function (t t t t t &key (:maturity-date t))
                    (values forward-rate-agreement &optional))
          make-forward-rate-agreement))
(defun make-forward-rate-agreement (index value-date position strike notional &key maturity-date)
  "Make an FRA on the Ibor INDEX from VALUE-DATE. POSITION is :LONG (receives the rate)
or :SHORT. The simple STRIKE applies to NOTIONAL. Without MATURITY-DATE, the period
is the tenor of the index and the rate is the fixing of the index. With it, the period
ends on that date (adjusted) and the rate is the forward over the period. The function
calls MAKE-FRA-FROM-CLAUSES.

  (make-forward-rate-agreement (euribor #T\"6M\") #D\"2026-12-29\" :long 0.02d0 1d6)"
  (check-input position (member :long :short) ":LONG or :SHORT")
  (check-input value-date date "the value date")
  (check-input strike real "a real strike rate")
  (make-fra-from-clauses
   `((:index ,index) (:value-date ,value-date) (,position) (:strike ,strike) (:notional ,notional)
     ,@(when maturity-date `((:maturity ,maturity-date))))))
;;; --------------------------------------------------------------------
;;; Caps and floors
;;; --------------------------------------------------------------------

(define-fact :cap-leg list "The Ibor coupons a cap or floor is on.")
(define-fact :cap-rate (or real list) "The cap rate: a real, or one rate per coupon.")
(define-fact :floor-rate (or real list) "The floor rate: a real, or one rate per coupon.")

(defparameter *cap-leg-part-facts*
  `(:schedule :nominal :index :spread :day-counter :payment-convention :payment-lag
    :payment-calendar ,@*schedule-part-facts*)
  "The facts a cap's leg is built from when no clause gave it whole.")

(defun cap-leg-from-facts (schedule nominal index spread day-counter payment-convention payment-lag
                           payment-calendar)
  (floating-leg :schedule schedule :notional nominal :index index :spread spread :day-counter day-counter
                                       :payment-convention payment-convention
                                       :payment-lag payment-lag :payment-calendar payment-calendar))

(defun check-ibor-leg (leg)
  "LEG, checked to be a non-empty list of Ibor coupons."
  (check-input leg (and list (not null)) "a non-empty leg of Ibor coupons")
  (unless (every (lambda (c) (typep c 'ibor-coupon)) leg)
    (signal-invalid-argument 'leg leg 'list "a leg of Ibor coupons (a floating leg on an Ibor index)"))
  leg)

(defun cap-floor-rates (rate leg which)
  "One double rate per coupon of LEG from RATE, a real or a list of one per coupon."
  (cond ((realp rate) (make-list (length leg) :initial-element (float rate 1d0)))
        ((and (listp rate) (= (length rate) (length leg)) (every #'realp rate))
         (mapcar (lambda (r) (float r 1d0)) rate))
        (t (signal-invalid-argument which rate 'real
                                    (format nil "a real rate, or a list of ~D (one per coupon)"
                                            (length leg))))))

(defun assemble-cap-floor ()
  (check-exclusive :cap-leg *cap-leg-part-facts*)
  (let* ((leg (check-ibor-leg (prog1 (require-fact :cap-leg)
                                (consume-facts *cap-leg-part-facts*))))
         (cap (optional-fact :cap-rate))
         (floor (optional-fact :floor-rate)))
    (unless (or cap floor)
      (error 'missing-fact :fact :cap-rate :suppliers '("(cap)" "(floor)")))
    (make-instance 'cap-floor
                   :type (cond ((and cap floor) :collar) (cap :cap) (t :floor))
                   :leg leg
                   :cap-rates (and cap (cap-floor-rates cap leg 'cap-rate))
                   :floor-rates (and floor (cap-floor-rates floor leg 'floor-rate))
                   :currency (cashflow-currency (first leg)))))

(define-construction-language cap-floor
  (:documentation "Build caplets, floorlets or both on a leg of Ibor coupons. The clauses
give the cap and floor rates, and the leg. The leg is already built, or the clauses build
it. A cap clause alone makes a cap. A floor clause alone makes a floor. Both clauses make
a collar (long the cap, short the floor).

  (make-cap-floor (cap 0.025d0)
                  (floating-leg (nominal 1d6) (index (euribor #T\"6M\")) (schedule s)))
  (make-cap-floor (floor 0.01d0) (on-leg leg))")
  (:macro make-cap-floor)
  (:function make-cap-floor-from-clauses)
  (:assemble assemble-cap-floor))

(define-schedule-clauses cap-floor)

(define-fact-rule cap-floor :cap-leg
    ((schedule :schedule) (nominal :nominal) (index :index)
     &optional (spread :spread 0) (day-counter :day-counter nil)
     (payment-convention :payment-convention :following) (payment-lag :payment-lag 0)
     (payment-calendar :payment-calendar nil))
  (cap-leg-from-facts schedule nominal index spread day-counter payment-convention payment-lag
                      payment-calendar))

(define-publishing-clauses cap-floor
  (cap :cap-rate "The cap rate: a real, or a list with one rate per coupon. Replaces :CAP-RATE.")
  (floor :floor-rate "The floor rate: a real, or a list with one rate per coupon. Replaces :FLOOR-RATE.")
  (on-leg :cap-leg "A leg of Ibor coupons already built, instead of the leg clauses. Replaces LEG.")
  (nominal :nominal "The nominal of the leg built in place.")
  (index :index "The Ibor index of the leg built in place.")
  (spread :spread "The spread over the index, default 0.")
  (day-counter :day-counter "The day counter of the coupons, default the index's.")
  (payment-convention :payment-convention "The convention payment dates are adjusted by; default :following.")
  (payment-lag :payment-lag "Business days after each period's end that it pays; default 0.")
  (payment-calendar :payment-calendar "The calendar payment dates fall on, default the schedule's."))

(define-clause cap-floor floating-leg (&clauses clauses)
  "Groups the clauses of the leg built in place: nominal, index, spread, day-counter,
payment clauses, and the schedule or the schedule clauses. The leg clauses may also be
written without it."
  (apply-clauses clauses))

(defun rates-or-single (rates)
  "The single rate RATES holds if every coupon has the same, else the list."
  (if (every (lambda (r) (= r (first rates))) rates) (first rates) rates))

(defmethod object-clauses ((language (eql 'cap-floor)) (c cap-floor))
  `(,@(when (cap-rates c) `((:cap ,(rates-or-single (cap-rates c)))))
    ,@(when (floor-rates c) `((:floor ,(rates-or-single (floor-rates c)))))
    (:on-leg ,(cap-floor-leg c))))

(defmethod print-object ((c cap-floor) stream)
  (print-clause-form 'cap-floor c stream))

;;; --------------------------------------------------------------------
;;; Swaptions
;;; --------------------------------------------------------------------

;; Type T: ASSEMBLE-SWAPTION checks it, so a wrong value signals INVALID-ARGUMENT.
(define-fact :swap t "The swap a swaption is on.")
(define-fact :swaption-settlement (member :physical :cash-annuity)
  "Whether exercise enters the swap, or pays its value with the fixed leg discounted at the swap rate.")

(defun assemble-swaption ()
  (let* ((swap (require-fact :swap))
         (settlement (require-fact :swaption-settlement))
         (style (require-fact :exercise-style))
         (exercise (progn
                     (unless (member style '(:european :bermudan))
                       (signal-invalid-argument :exercise-style style 'keyword
                                                "a swaption exercise: :EUROPEAN or :BERMUDAN"))
                     (assemble-exercise style))))
    (check-input swap fixed-vs-floating-swap
                 "a fixed-vs-floating swap (MAKE-SWAP)")
    (make-instance 'swaption :swap swap :exercise exercise :settlement settlement
                             :currency (instrument-currency swap))))

(define-construction-language swaption
  (:documentation "Build the right to enter a swap on an exercise date. The clauses give
the swap, a European or Bermudan exercise, and how exercise settles. The swap is already
built, or the clauses build it. The exercise clauses are those of the option language:
expires, european, bermudan.

  (make-swaption (on-swap swap) (expires #D\"2027-09-24\"))
  (make-swaption (on-swap swap) (bermudan :dates dates) (settlement :cash-annuity))")
  (:macro make-swaption)
  (:function make-swaption-from-clauses)
  (:defaults :exercise-style :european :swaption-settlement :physical)
  (:assemble assemble-swaption))

(share-clauses 'option 'swaption '("EUROPEAN" "BERMUDAN" "EXPIRES"))
(share-fact-rules 'option 'swaption '(:exercise-dates :expiry))

(define-clause swaption on-swap (swap)
  (:publishes :swap)
  "A swap already built. A payer swap makes a payer swaption. Replaces SWAP."
  (publish :swap swap))

(define-clause swaption swap (&clauses clauses)
  (:publishes :swap)
  (:clauses-of swap)
  "The swap, built in place from the clauses of the swap language: nominal, fixed-leg,
floating-leg and the rest. Replaces SWAP."
  (publish :swap (make-swap-from-clauses clauses)))

(define-clause swaption settlement (kind)
  (:publishes :swaption-settlement)
  "How exercise settles: :physical (the default) enters the swap, :cash-annuity pays its
value with the fixed leg discounted at the swap rate. Replaces :SETTLEMENT."
  (publish :swaption-settlement kind))

(defmethod object-clauses ((language (eql 'swaption)) (s swaption))
  `((:on-swap ,(swaption-swap s))
    ,@(option-component-clauses (swaption-exercise s))
    (:settlement ,(swaption-settlement s))))

(defmethod print-object ((s swaption) stream)
  (print-clause-form 'swaption s stream))
