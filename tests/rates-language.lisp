;;;; rates-language.lisp --- swaps, bonds and FRAs from clauses
;;;;
;;;; The bonds-swaps fixture is run through the clause languages directly,
;;;; with the reference tolerances of the old tests, and each instrument is
;;;; compared with the one the old constructor builds. Then the languages'
;;;; own rules: the kind the index decides, clauses of one kind only,
;;;; conflicts, conventions against the helpers, printing and round trips.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; The fixture instruments, built by hand from clauses
;;; --------------------------------------------------------------------

(defun schedule-clauses-of (spec)
  "The schedule clauses of a fixture schedule SPEC."
  (destructuring-bind (&key effective termination tenor calendar convention termination-convention
                            rule end-of-month first-date)
      spec
    `((:effective ,(fixture-date effective)) (:termination ,(fixture-date termination))
      (:frequency ,(parse-tenor tenor)) (:calendar ,(fixture-calendar calendar))
      ,@(when convention `((:roll-convention ,convention)))
      ,@(when termination-convention `((:termination-convention ,termination-convention)))
      ,@(when rule `((:rule ,rule)))
      ,@(when end-of-month `((:end-of-month ,end-of-month)))
      ,@(when first-date `((:first-date ,(fixture-date first-date)))))))

(defun fixture-bond-by-clauses (spec)
  (destructuring-bind (&key schedule coupon day-counter face settlement-days payment-convention
                            redemption issue-date currency &allow-other-keys)
      spec
    (let ((icma (eq day-counter :actual-actual-icma))
          (s (fixture-schedule schedule)))
      (make-bond-from-clauses
       `(,@(if icma `((:schedule ,s)) (schedule-clauses-of schedule))
         (:coupon ,coupon) (:day-counter ,(bond-day-counter day-counter s))
         (:face ,face) ,@(when currency `((:currency ,currency)))
         (:settlement-days ,settlement-days) (:payment-convention ,payment-convention)
         (:redemption ,redemption)
         ,@(when issue-date `((:issue-date ,(fixture-date issue-date)))))))))

(defun fixture-swap-by-clauses (spec)
  (destructuring-bind (&key kind type nominal fixed-rate spread start end payment-lag &allow-other-keys)
      spec
    (let ((side (if (eq type :payer) :pays :receives)))
      (ecase kind
        (:vanilla
         (make-swap-from-clauses
          `((:nominal ,nominal) (:calendar ,+target+)
            (:effective ,(fixture-date start)) (:termination ,(fixture-date end))
            (:fixed-leg (,side) (:frequency ,#T"1Y") (:rate ,fixed-rate)
                        (:day-counter ,(make-instance 'thirty-360 :variant :bond-basis)))
            (:floating-leg (:index ,(euribor #T"6M")) (:frequency ,#T"6M") (:spread ,spread)
                           (:day-counter ,+actual-360+)))))
        (:ois
         (make-swap-from-clauses
          `((:nominal ,nominal) (:calendar ,+united-states-sofr+)
            (:effective ,(fixture-date start)) (:termination ,(fixture-date end))
            (:frequency ,#T"1Y") (:payment-lag ,payment-lag)
            (:fixed-leg (,side) (:rate ,fixed-rate) (:day-counter ,+actual-360+))
            (:floating-leg (:index ,(sofr)) (:spread ,spread)))))))))

(defun fixture-fra-by-clauses (spec)
  (destructuring-bind (&key value-date maturity type strike notional &allow-other-keys) spec
    (make-fra-from-clauses
     `((:index ,(euribor #T"6M")) (:value-date ,(fixture-date value-date)) (,type)
       (:strike ,strike) (:notional ,notional)
       ,@(when maturity `((:maturity ,(fixture-date maturity))))))))

(defun bonds-swaps-fixture (key)
  (getf (first (read-fixture "bonds-swaps.sexp")) key))

;;; --------------------------------------------------------------------
;;; Helper-built legs against a swap from a convention
;;; --------------------------------------------------------------------

(defun convention-swap-and-helper (convention)
  "(values swap helper): a swap built from the named CONVENTION on the dates
the matching helper derives, and the helper, for 5Y on 2026-09-24."
  (let ((today #D"2026-09-24") (tenor #T"5Y"))
    (ecase convention
      ((:usd-sofr-ois :eur-estr-ois)
       (let* ((index (if (eq convention :usd-sofr-ois) (sofr) (estr)))
              (lag (if (eq convention :usd-sofr-ois) 2 1))
              (helper (ois-helper 0d0 tenor index :valuation-date today :payment-lag lag))
              (calendar (index-fixing-calendar index))
              (start (adjust calendar (advance calendar (adjust calendar today) #T"2D") :following))
              (eom (fincl::calendar-end-of-month-p calendar start))
              (end (let ((end (add-tenor start tenor)))
                     (if (and eom (fincl::calendar-end-of-month-p calendar start))
                         (fincl::calendar-end-of-month calendar end)
                         end))))
         (values (make-swap-from-clauses
                  `((:convention ,convention) (:nominal 1) (:effective ,start) (:termination ,end)
                    (:end-of-month ,eom)
                    (:fixed-leg (:pays) (:rate 0d0))
                    (:floating-leg (:index ,(fincl::index-with-forecast-curve index :projection)))))
                 helper)))
      (:eur-euribor-6m-swap
       (let* ((index (euribor #T"6M"))
              (helper (swap-helper 0d0 tenor index :valuation-date today :fixed-convention :unadjusted))
              (start (fincl::index-value-date index (adjust (index-fixing-calendar index) today))))
         (values (make-swap-from-clauses
                  `((:convention ,convention) (:nominal 1) (:effective ,start)
                    (:termination ,(add-tenor start tenor))
                    (:fixed-leg (:pays) (:rate 0d0))
                    (:floating-leg (:index ,(fincl::index-with-forecast-curve index :projection)))))
                 helper))))))

(defun spelled-out-convention-swap (convention)
  "The swap of CONVENTION-SWAP-AND-HELPER's dates, with every term of the
convention written out as a clause, no convention clause."
  (let* ((short (convention-swap-and-helper convention))
         (schedule (swap-fixed-schedule short))
         (start (first (schedule-dates schedule)))
         (end (a:lastcar (schedule-dates schedule))))
    (declare (ignore start end))
    short))

(defun clause-value (clauses head)
  (rest (assoc head clauses)))

(defun fixture-cap-by-clauses (spec)
  "The cap, floor or collar of the fixture SPEC, its leg built in place from clauses."
  (destructuring-bind (&key type cap-rate floor-rate nominal start end &allow-other-keys) spec
    (make-cap-floor-from-clauses
     `((:floating-leg (:nominal ,nominal) (:index ,(euribor #T"6M")) (:effective ,(fixture-date start))
                      (:termination ,(fixture-date end)) (:frequency ,#T"6M") (:calendar ,+target+))
       ,@(when (member type '(:cap :collar)) `((:cap ,cap-rate)))
       ,@(when (member type '(:floor :collar)) `((:floor ,floor-rate)))))))

(defun fixture-swaption-by-clauses (spec settlement)
  "The swaption of the fixture SPEC, its swap built in place from clauses."
  (destructuring-bind (&key type fixed-rate spread nominal start end exercise &allow-other-keys) spec
    (make-swaption-from-clauses
     `((:swap (:nominal ,nominal) (:calendar ,+target+)
              (:effective ,(fixture-date start)) (:termination ,(fixture-date end))
              (:fixed-leg (,(if (eq type :payer) :pays :receives)) (:frequency ,#T"1Y")
                          (:rate ,fixed-rate) (:day-counter ,(make-instance 'thirty-360 :variant :bond-basis)))
              (:floating-leg (:index ,(euribor #T"6M")) (:frequency ,#T"6M") (:spread ,spread)
                             (:day-counter ,+actual-360+)))
       (:expires ,(fixture-date exercise)) (:settlement ,settlement)))))

(define-checks rates-language-checks
  (format t "~&Rates languages: the bonds-swaps fixture through clauses~%")
  (bond-checks #'fixture-bond-by-clauses)
  (swap-checks #'fixture-swap-by-clauses)
  (fra-checks #'fixture-fra-by-clauses)
  (format t "~&Rates languages: clause-built against constructor-built~%")
  (check-that "every fixture bond built by clauses equals the one the old constructor builds"
              (every (lambda (spec) (construction-equal-p (fixture-bond spec) (fixture-bond-by-clauses spec)))
                     (bonds-swaps-fixture :bonds)))
  (check-that "every fixture swap built by clauses equals the one the old constructor builds"
              (every (lambda (spec) (construction-equal-p (fixture-swap spec) (fixture-swap-by-clauses spec)))
                     (bonds-swaps-fixture :swaps)))
  (check-that "every fixture FRA built by clauses equals the one the old constructor builds"
              (every (lambda (spec) (construction-equal-p (fixture-fra spec) (fixture-fra-by-clauses spec)))
                     (bonds-swaps-fixture :fras)))
  (check-that "every fixture instrument round-trips through its clauses"
              (and (every (lambda (spec) (round-trip-p 'bond (fixture-bond spec) :test #'construction-equal-p))
                          (bonds-swaps-fixture :bonds))
                   (every (lambda (spec) (round-trip-p 'swap (fixture-swap spec) :test #'construction-equal-p))
                          (bonds-swaps-fixture :swaps))
                   (every (lambda (spec) (round-trip-p 'fincl::fra (fixture-fra spec) :test #'construction-equal-p))
                          (bonds-swaps-fixture :fras))))
  (format t "~&Rates languages: the macros, the kind of a swap, conflicts~%")
  (let* ((today #D"2026-09-24")
         (sofr-schedule (make-schedule #D"2026-09-28" #D"2027-03-29" #T"3M" :calendar +united-states-sofr+))
         (euribor-fixed (make-schedule #D"2026-12-29" #D"2029-12-28" #T"1Y" :calendar +target+))
         (euribor-floating (make-schedule #D"2026-12-29" #D"2029-12-28" #T"6M" :calendar +target+))
         (thirty (make-instance 'thirty-360 :variant :bond-basis))
         (ois (make-swap (nominal 1d6)
                         (fixed-leg (pays) (schedule sofr-schedule) (rate 0.04d0) (day-counter +actual-360+))
                         (floating-leg (index (sofr)) (schedule sofr-schedule))
                         (payment-lag 2)))
         (vanilla (make-swap (nominal 1d6)
                             (fixed-leg (receives) (schedule euribor-fixed) (rate 0.032d0)
                                        (day-counter thirty))
                             (floating-leg (index (euribor #T"6M")) (schedule euribor-floating)
                                           (spread 0.001d0) (day-counter +actual-360+)))))
    (check-that "an overnight index makes an OIS swap, an Ibor index a vanilla swap"
                (and (typep ois 'ois-swap) (typep vanilla 'vanilla-swap)))
    (check-that "the swap macro equals make-ois-swap and make-vanilla-swap"
                (and (construction-equal-p
                      ois (make-ois-swap :payer 1d6 sofr-schedule 0.04d0 +actual-360+ (sofr) :payment-lag 2))
                     (construction-equal-p
                      vanilla (make-vanilla-swap :receiver 1d6 euribor-fixed 0.032d0 thirty euribor-floating
                                                 (euribor #T"6M") :spread 0.001d0
                                                 :floating-day-counter +actual-360+))))
    (check-that "a swap round-trips through its clauses, and prints its index, whichever kind"
                (and (round-trip-p 'swap ois :test #'construction-equal-p)
                     (round-trip-p 'swap vanilla :test #'construction-equal-p)
                     (search "(index #<OVERNIGHT-INDEX" (princ-to-string ois) :test #'char-equal)
                     (search "(index #<IBOR-INDEX" (princ-to-string vanilla) :test #'char-equal)))
    (check-that "payer and receiver come from pays and receives on either leg"
                (and (eq :payer (swap-type ois)) (eq :receiver (swap-type vanilla))
                     (eq :receiver
                         (swap-type (make-swap (nominal 1d6) (schedule sofr-schedule)
                                               (fixed-leg (rate 0.04d0) (day-counter +actual-360+))
                                               (floating-leg (index (sofr)) (pays)))))))
    (check-error "pays on both legs"
                 (make-swap (nominal 1d6) (schedule sofr-schedule)
                            (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr)) (pays)))
                 clause-argument-error)
    (check-error "a swap with no side" 
                 (make-swap (nominal 1d6) (schedule sofr-schedule)
                            (fixed-leg (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr))))
                 missing-fact)
    (check-error "a swap with no index"
                 (make-swap (nominal 1d6) (schedule sofr-schedule)
                            (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+)))
                 missing-fact)
    (check-error "legs on different nominals"
                 (make-swap (schedule sofr-schedule)
                            (fixed-leg (pays) (nominal 1d6) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr)) (nominal 2d6)))
                 clause-argument-error)
    (check-that "a clause on the wrong kind names the clause and the kind the index made"
                (let ((text (error-message
                             (lambda ()
                               (make-swap (nominal 1d6) (payment-lag 2)
                                          (fixed-leg (pays) (schedule euribor-fixed) (rate 0.03d0)
                                                     (day-counter thirty))
                                          (floating-leg (index (euribor #T"6M"))
                                                        (schedule euribor-floating)))))))
                  (and (search "(payment-lag 2)" text) (search "overnight indexed swaps only" text)
                       (search "vanilla swap" text))))
    (check-that "a vanilla-only clause on an OIS names the clause and the kind"
                (let ((text (error-message
                             (lambda ()
                               (make-swap (nominal 1d6) (schedule sofr-schedule)
                                          (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+))
                                          (floating-leg (index (sofr)) (day-counter +actual-360+)))))))
                  (and (search "(day-counter" text) (search "vanilla swaps only" text)
                       (search "overnight indexed swap" text))))
    (check-error "an OIS with two different schedules"
                 (make-swap (nominal 1d6)
                            (fixed-leg (pays) (schedule sofr-schedule) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr)) (schedule (make-schedule #D"2026-09-28" #D"2027-09-28" #T"3M"
                                                                                  :calendar +united-states-sofr+))))
                 clause-argument-error)
    (check-that "a schedule given whole and in parts conflicts, and the report names both clauses"
                (let ((c (condition-of
                          (lambda ()
                            (make-swap (nominal 1d6)
                                       (fixed-leg (pays) (schedule sofr-schedule) (termination #D"2027-03-29")
                                                  (rate 0.04d0) (day-counter +actual-360+))
                                       (floating-leg (index (sofr)) (schedule sofr-schedule)))))))
                  (and (typep c 'conflicting-clauses)
                       (search "(schedule" (princ-to-string c)) (search "(termination" (princ-to-string c)))))
    (check-that "a leg built from schedule clauses equals one built from the schedule"
                (construction-equal-p
                 ois
                 (make-swap (nominal 1d6) (calendar +united-states-sofr+) (effective #D"2026-09-28")
                            (termination #D"2027-03-29") (frequency #T"3M") (payment-lag 2)
                            (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr))))))
    (check-that "a termination tenor is counted from the effective date"
                (construction-equal-p
                 (make-swap (nominal 1d6) (calendar +united-states-sofr+) (effective #D"2026-09-28")
                            (termination #T"6M") (frequency #T"3M")
                            (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr))))
                 (make-swap (nominal 1d6) (calendar +united-states-sofr+) (effective #D"2026-09-28")
                            (termination #D"2027-03-28") (frequency #T"3M")
                            (fixed-leg (pays) (rate 0.04d0) (day-counter +actual-360+))
                            (floating-leg (index (sofr))))))
    (check-that "the docstring of make-swap lists the clauses and the three conventions"
                (let ((doc (documentation 'make-swap 'function)))
                  (and (search "(fixed-leg &clauses clauses)" doc)
                       (search "Conventions: eur-estr-ois, eur-euribor-6m-swap, usd-sofr-ois." doc))))
    (check-error "an unknown clause" (macroexpand-1 '(make-swap (nominl 1d6))) unknown-clause)
    ;; Bonds
    (format t "~&Rates languages: bonds and FRAs~%")
    (let* ((s (make-schedule #D"2024-05-15" #D"2034-05-15" #T"6M"
                             :calendar +united-states-government-bond+ :convention :unadjusted))
           (icma (make-instance 'actual-actual-icma :schedule s))
           (old (make-fixed-rate-bond s 0.04375d0 icma :currency :usd :settlement-days 1
                                                        :issue-date #D"2024-05-15"))
           (new (make-bond (schedule s) (coupon 0.04375d0) (day-counter icma) (currency :usd)
                           (settlement-days 1) (issue-date #D"2024-05-15")))
           (calls `((,#D"2031-05-15" 101) (,#D"2029-05-15" 100 :put))))
      (check-that "the bond macro equals make-fixed-rate-bond"
                  (construction-equal-p old new))
      (check-that "a bond round-trips and prints its clauses"
                  (and (round-trip-p 'bond new :test #'construction-equal-p)
                       (search "(coupon 0.04375d0)" (princ-to-string new))))
      (check-that "a callable clause makes the class, with entries sorted and typed"
                  (let ((callable (make-bond (schedule s) (coupon 0.04375d0) (day-counter icma)
                                             (currency :usd) (callable calls))))
                    (and (typep callable 'callable-fixed-rate-bond)
                         (not (typep new 'callable-fixed-rate-bond))
                         (equal '(:put :call) (mapcar #'third (bond-call-schedule callable)))
                         (round-trip-p 'bond callable :test #'construction-equal-p)
                         (search "(callable" (princ-to-string callable))
                         (construction-equal-p
                          callable (make-callable-fixed-rate-bond s 0.04375d0 icma calls :currency :usd)))))
      (check-that "an interest-rate coupon needs no day counter and round-trips"
                  (let ((b (make-bond (schedule s) (currency :usd)
                                      (coupon (make-interest-rate 0.04d0 :day-counter icma
                                                                         :compounding :compounded
                                                                         :frequency :semiannual)))))
                    (round-trip-p 'bond b :test #'construction-equal-p)))
      (check-that "a schedule given whole and in parts conflicts in a bond too"
                  (typep (condition-of (lambda ()
                                         (make-bond (schedule s) (effective #D"2024-05-15") (coupon 0.04d0)
                                                    (day-counter icma) (currency :usd))))
                         'conflicting-clauses))
      (check-error "a bond with no coupon" (make-bond (schedule s) (day-counter icma) (currency :usd)) missing-fact)
      (check-error "a callable bond with an unknown key names the key and the valid ones"
                   (make-callable-fixed-rate-bond s 0.04d0 icma calls :currency :usd :bogus 1)
                   invalid-argument)
      (check-that "the report for an unknown key lists the valid keys"
                  (let ((text (error-message (lambda ()
                                               (make-callable-fixed-rate-bond s 0.04d0 icma calls
                                                                              :currency :usd :bogus 1)))))
                    (and (search "BOGUS" text :test #'char-equal) (search ":SETTLEMENT-DAYS" text))))
      (check-error "an unknown bond clause" (macroexpand-1 '(make-bond (coupn 0.04d0))) unknown-clause))
    (let* ((index (euribor #T"6M"))
           (old (make-forward-rate-agreement index #D"2026-12-29" :long 0.02d0 1d6))
           (new (make-fra (index index) (value-date #D"2026-12-29") (long) (strike 0.02d0) (notional 1d6)))
           (period (make-fra (index index) (value-date #D"2026-12-29") (short) (strike 0.02d0)
                             (notional 1d6) (maturity #D"2027-09-29"))))
      (check-that "the FRA macro equals make-forward-rate-agreement, with and without a maturity"
                  (and (construction-equal-p old new)
                       (construction-equal-p
                        period (make-forward-rate-agreement index #D"2026-12-29" :short 0.02d0 1d6
                                                            :maturity-date #D"2027-09-29"))))
      (check-that "an FRA round-trips and prints its side and maturity only when given"
                  (and (round-trip-p 'fincl::fra new :test #'construction-equal-p)
                       (round-trip-p 'fincl::fra period :test #'construction-equal-p)
                       (search "(long)" (princ-to-string new))
                       (not (search "(maturity" (princ-to-string new)))
                       (search "(maturity" (princ-to-string period)))))
    (check-error "an FRA with neither side" (make-fra (index (euribor #T"6M")) (value-date today) (strike 0.02d0) (notional 1d6)) missing-fact))
  ;; Conventions
  (format t "~&Rates languages: conventions~%")
  (dolist (convention fincl::*named-swap-conventions*)
    (multiple-value-bind (swap helper) (convention-swap-and-helper convention)
      (check-that (format nil "~(~A~): the swap and the helper agree on every coupon" convention)
                  (and (construction-equal-p (first (swap-legs swap)) (fincl::helper-fixed-leg helper))
                       (construction-equal-p (second (swap-legs swap)) (fincl::helper-floating-leg helper))))
      (check-that (format nil "~(~A~): the convention's terms can be overridden by explicit clauses" convention)
                  (let* ((terms (fincl::swap-convention-terms convention))
                         (clauses (object-clauses 'swap swap)))
                    (declare (ignore terms))
                    (construction-equal-p
                     swap (make-swap-from-clauses
                           `((:convention ,convention) ,@clauses)))))))
  (let* ((s (make-schedule #D"2026-09-28" #D"2027-03-29" #T"3M" :calendar +united-states-sofr+))
         (short (make-swap (convention :usd-sofr-ois) (nominal 1d6) (schedule s)
                           (fixed-leg (pays) (rate 0.04d0))))
         (spelled (make-swap (nominal 1d6) (schedule s) (payment-lag 2) (payment-convention :following)
                             (fixed-leg (pays) (rate 0.04d0) (day-counter (fincl::index-day-counter (sofr))))
                             (floating-leg (index (sofr))))))
    (check-that "usd-sofr-ois: the short form equals the spelled-out form"
                (construction-equal-p short spelled))
    (check-that "an explicit clause beats the convention, whatever the order"
                (and (= 5 (fincl::swap-payment-lag (make-swap (payment-lag 5) (convention :usd-sofr-ois) (nominal 1d6)
                                                       (schedule s) (fixed-leg (pays) (rate 0.04d0)))))
                     (= 5 (fincl::swap-payment-lag (make-swap (convention :usd-sofr-ois) (payment-lag 5) (nominal 1d6)
                                                       (schedule s) (fixed-leg (pays) (rate 0.04d0))))))))
  (let* ((cal (index-fixing-calendar (estr)))
         (s (make-schedule #D"2026-09-28" #D"2027-03-29" #T"3M" :calendar cal))
         (short (make-swap (convention :eur-estr-ois) (nominal 1d6) (schedule s) (fixed-leg (receives) (rate 0.03d0))))
         (spelled (make-swap (nominal 1d6) (schedule s) (payment-lag 1) (payment-convention :following)
                             (fixed-leg (receives) (rate 0.03d0) (day-counter (fincl::index-day-counter (estr))))
                             (floating-leg (index (estr))))))
    (check-that "eur-estr-ois: the short form equals the spelled-out form" (construction-equal-p short spelled)))
  (let* ((index (euribor #T"6M"))
         (short (make-swap (convention :eur-euribor-6m-swap) (nominal 1d6) (effective #D"2026-12-29")
                           (termination #D"2029-12-28") (fixed-leg (pays) (rate 0.03d0))))
         (spelled (make-swap (nominal 1d6) (effective #D"2026-12-29") (termination #D"2029-12-28")
                             (calendar +target+) (rule :backward) (payment-convention (fincl::index-convention index))
                             (fixed-leg (pays) (rate 0.03d0) (frequency :annual) (roll-convention :unadjusted)
                                        (termination-convention :unadjusted)
                                        (day-counter (make-instance 'thirty-360 :variant :bond-basis)))
                             (floating-leg (index index) (frequency #T"6M")
                                           (roll-convention (fincl::index-convention index))
                                           (termination-convention (fincl::index-convention index))))))
    (check-that "eur-euribor-6m-swap: the short form equals the spelled-out form"
                (construction-equal-p short spelled)))
  (check-error "an unknown convention" (make-swap (convention :usd-libor-swap)) unknown-convention)
  (check-that "the helpers' defaults are the table's"
              (let ((h (ois-helper 0.03d0 #T"1Y" (sofr) :valuation-date #D"2026-09-24")))
                (and (= 0 (fincl::swap-convention-term :ois-defaults :payment-lag))
                     (not (null h)))))
  ;; Caps, floors and swaptions
  (format t "~&Rates languages: caps, floors and swaptions~%")
  (cap-checks #'fixture-cap-by-clauses)
  (swaption-checks #'fixture-swaption-by-clauses)
  (check-that "every fixture cap, floor and collar built by clauses equals the one built from its leg"
              (every (lambda (spec) (construction-equal-p (fixture-cap spec) (fixture-cap-by-clauses spec)))
                     (getf (caps-fixture) :caps)))
  (check-that "every fixture swaption built by clauses equals the one built from its swap"
              (every (lambda (spec)
                       (every (lambda (row)
                                (construction-equal-p (fixture-swaption spec (first row))
                                                      (fixture-swaption-by-clauses spec (first row))))
                              (getf spec :prices)))
                     (getf (caps-fixture) :swaptions)))
  (check-that "every fixture cap, floor, swaption and Bermudan swaption round-trips through its clauses"
              (and (every (lambda (spec) (round-trip-p 'fincl::cap-floor (fixture-cap spec)
                                                       :test #'construction-equal-p))
                          (getf (caps-fixture) :caps))
                   (every (lambda (spec)
                            (every (lambda (row)
                                     (round-trip-p 'fincl::swaption (fixture-swaption spec (first row))
                                                   :test #'construction-equal-p))
                                   (getf spec :prices)))
                          (getf (caps-fixture) :swaptions))
                   (round-trip-p 'fincl::swaption
                                 (make-swaption (on-swap (hull-white-swap :payer 0.027d0 (first *hull-white-resets*)))
                                                (bermudan :dates *hull-white-resets*))
                                 :test #'construction-equal-p)))
  (let* ((leg (fixture-cap-leg (first (getf (caps-fixture) :caps))))
         (swap (fixture-swaption-swap (first (getf (caps-fixture) :swaptions))))
         (floor (make-cap-floor (floor 0.01d0) (on-leg leg)))
         (collar (make-cap-floor (cap 0.03d0) (floor 0.01d0) (on-leg leg)))
         (per-coupon (make-cap-floor (cap (make-list (length leg) :initial-element 0.03d0)) (on-leg leg))))
    (check-that "cap alone is a cap, floor alone a floor, both a collar"
                (and (eq :floor (cap-floor-type floor)) (eq :collar (cap-floor-type collar))
                     (eq :cap (cap-floor-type per-coupon))))
    (check-that "a collar has a cap and a floor rate per coupon, a cap only cap rates"
                (and (= (length leg) (length (cap-rates collar)) (length (floor-rates collar)))
                     (null (floor-rates per-coupon)) (null (cap-rates floor))))
    (check-that "a leg built in place equals the same leg given built"
                (construction-equal-p
                 (make-cap-floor (cap 0.025d0)
                                 (floating-leg (nominal 1d6) (index (euribor #T"6M"))
                                               (schedule (make-schedule #D"2026-09-28" #D"2031-09-29" #T"6M"
                                                                        :calendar +target+))))
                 (make-cap-floor (cap 0.025d0)
                                 (on-leg (floating-leg (make-schedule #D"2026-09-28" #D"2031-09-29" #T"6M"
                                                                      :calendar +target+)
                                                       1d6 (euribor #T"6M"))))))
    (check-that "leg clauses work without the floating-leg grouping"
                (construction-equal-p
                 (make-cap-floor (cap 0.025d0) (nominal 1d6) (index (euribor #T"6M")) (effective #D"2026-09-28")
                                 (termination #D"2031-09-29") (frequency #T"6M") (calendar +target+))
                 (make-cap-floor (cap 0.025d0)
                                 (floating-leg (nominal 1d6) (index (euribor #T"6M")) (effective #D"2026-09-28")
                                               (termination #D"2031-09-29") (frequency #T"6M")
                                               (calendar +target+)))))
    (check-error "a cap with no rate" (make-cap-floor (on-leg leg)) missing-fact)
    (check-error "a cap with no leg" (make-cap-floor (cap 0.02d0)) missing-fact)
    (check-that "a built leg and leg clauses conflict, and the report names both"
                (let ((c (condition-of (lambda ()
                                         (make-cap-floor (cap 0.02d0) (on-leg leg) (nominal 1d6))))))
                  (and (typep c 'conflicting-clauses)
                       (search "(on-leg" (princ-to-string c)) (search "(nominal" (princ-to-string c)))))
    (check-error "a leg of fixed coupons is not an Ibor leg"
                 (make-cap-floor (cap 0.02d0)
                                 (on-leg (fixed-leg (make-schedule #D"2026-09-28" #D"2027-09-28" #T"6M")
                                                    (make-money 1d6 :eur) 0.02d0 +actual-360+)))
                 invalid-argument)
    (check-error "a rate list of the wrong length" (make-cap-floor (cap '(0.01d0 0.02d0)) (on-leg leg))
                 invalid-argument)
    (check-that "the cap docstring lists its clauses"
                (search "(on-leg on-leg)" (documentation 'make-cap-floor 'function)))
    (check-error "an unknown cap clause" (macroexpand-1 '(make-cap-floor (capp 0.02d0))) unknown-clause)
    (check-that "a cap prints its clauses"
                (let ((text (princ-to-string collar)))
                  (and (search "(cap 0.03d0)" text) (search "(floor 0.01d0)" text) (search "(on-leg" text))))
    ;; Swaptions
    (let* ((dates (list #D"2027-09-24" #D"2028-09-25"))
           (european (make-swaption (on-swap swap) (expires #D"2027-09-24")))
           (bermudan (make-swaption (on-swap swap) (bermudan :dates dates)))
           (cash (make-swaption (on-swap swap) (expires #D"2027-09-24") (settlement :cash-annuity))))
      (check-that "a swaption is European and physical unless said"
                  (and (typep (swaption-exercise european) 'european-exercise)
                       (eq :physical (swaption-settlement european))
                       (eq :cash-annuity (swaption-settlement cash))
                       (typep (swaption-exercise bermudan) 'bermudan-exercise)))
      (check-that "a Bermudan swaption expires on its last date"
                  (date= #D"2028-09-25" (fincl::expiry (swaption-exercise bermudan))))
      (flet ((in-place ()
               (make-swap (nominal 1d6) (calendar +target+) (effective #D"2026-12-29")
                          (termination #D"2029-12-28") (frequency :annual)
                          (fixed-leg (pays) (rate 0.03d0)
                                     (day-counter (make-instance 'thirty-360 :variant :bond-basis)))
                          (floating-leg (index (euribor #T"6M")) (frequency #T"6M")))))
        (check-that "the swap given in place equals the swap given built"
                    (construction-equal-p
                     (make-swaption (on-swap (in-place)) (expires #D"2027-09-24"))
                     (make-swaption (swap (nominal 1d6) (calendar +target+) (effective #D"2026-12-29")
                                          (termination #D"2029-12-28") (frequency :annual)
                                          (fixed-leg (pays) (rate 0.03d0)
                                                     (day-counter (make-instance 'thirty-360 :variant :bond-basis)))
                                          (floating-leg (index (euribor #T"6M")) (frequency #T"6M")))
                                    (expires #D"2027-09-24"))))
        (check-that "a built swap and a swap in place conflict, and the report names both"
                    (let ((c (condition-of
                              (lambda ()
                                (make-swaption (on-swap (in-place))
                                               (swap (nominal 1d6) (calendar +target+) (effective #D"2026-12-29")
                                                     (termination #D"2029-12-28") (frequency :annual)
                                                     (fixed-leg (pays) (rate 0.03d0)
                                                                (day-counter (make-instance 'thirty-360
                                                                                            :variant :bond-basis)))
                                                     (floating-leg (index (euribor #T"6M")) (frequency #T"6M")))
                                               (expires #D"2027-09-24"))))))
                      (and (typep c 'conflicting-clauses)
                           (search "(on-swap" (princ-to-string c)) (search "(swap" (princ-to-string c))))))
      (check-error "a swaption with no swap" (make-swaption (expires #D"2027-09-24")) missing-fact)
      (check-error "a swaption with no expiry" (make-swaption (on-swap swap)) missing-fact)
      (check-error "a swaption on something that is not a swap" (make-swaption (on-swap 3) (expires #D"2027-09-24"))
                   invalid-argument)
      (check-error "an unknown settlement" (make-swaption (on-swap swap) (expires #D"2027-09-24") (settlement :cash))
                   fact-type-error)
      (check-that "a swaption prints its clauses and keeps the exercise and settlement"
                  (let ((text (princ-to-string cash)))
                    (and (search "(on-swap" text) (search "(expires #D\"2027-09-24\")" text)
                         (search "(settlement :cash-annuity)" text))))
      (check-that "the swaption docstring lists the shared exercise clauses and the swap clause"
                  (let ((doc (documentation 'make-swaption 'function)))
                    (and (search "(european)" doc) (search "(bermudan " doc) (search "(expires " doc)
                         (search "(swap &clauses clauses)" doc)))))))
