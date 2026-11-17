;;;; simulate.lisp --- the SIMULATE macro
;;;;
;;;; SIMULATE writes the visitor of MAP-SIMULATED-PATHS from clauses.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *simulate-clauses* '(:accumulate :hold :portfolio :state :derive :start :every :on :final))

  (defun simulate-clause-error (clause reason)
    (signal-invalid-argument 'clause clause 'list (format nil "a SIMULATE clause: ~A" reason)))

  (defun simulate-book-engine (operator book books)
    "The :ENGINE form of the book BOOK names in (OPERATOR book ...), BOOKS being
((var . engine) ...) of the :PORTFOLIO clauses; signals unless BOOK is one."
    (let ((entry (and (symbolp book) (assoc book books))))
      (unless entry
        (signal-invalid-argument operator book 'symbol
                                 (format nil "a :PORTFOLIO variable as the first argument of ~(~A~), one of ~
~:[none~;~:*~{~S~^, ~}~]" operator (mapcar #'car books))))
      (cdr entry)))

  (defun clause-binding-list-p (x)
    "True if X is the optional ([market] &key time) list of :START: a list
holding &KEY, or of at most one symbol that names no function."
    (and (listp x)
         (or (member '&key x)
             (and (<= (length x) 1)
                  (every (lambda (s) (and (symbolp s) s (not (keywordp s)) (not (fboundp s)))) x)))))

  (defun parse-clause-binding (clause binding min max keys syntax)
    "(values variables key-variables) of a date clause's BINDING: MIN to MAX
variables, then optionally &KEY and variables named by KEYS (strings), each
at most once. KEY-VARIABLES holds one symbol or NIL per key, in KEYS' order.
SYNTAX describes the clause in the report."
    (unless (a:proper-list-p binding) (simulate-clause-error clause syntax))
    (let* ((at (position '&key binding))
           (variables (subseq binding 0 at))
           (key-part (if at (subseq binding (1+ at)) '())))
      (flet ((variable-p (s) (and (symbolp s) s (not (keywordp s)) (not (member s lambda-list-keywords)))))
        (unless (and (<= min (length variables) max) (every #'variable-p variables)
                     (every #'variable-p key-part)
                     (every (lambda (s) (member (symbol-name s) keys :test #'string=)) key-part)
                     (= (length key-part) (length (remove-duplicates key-part :key #'symbol-name :test #'string=))))
          (simulate-clause-error clause syntax)))
      (values variables
              (mapcar (lambda (key) (find key key-part :key #'symbol-name :test #'string=)) keys))))

  (defun parse-final (clause)
    "The (name form) entries of a :FINAL clause."
    (let ((entries (if (and (= 2 (length clause))
                            (not (and (consp (second clause)) (keywordp (first (second clause)))
                                      (= 2 (length (second clause))))))
                       `((:value ,(second clause)))
                       (rest clause))))
      (dolist (e entries)
        (unless (and (consp e) (= 2 (length e)) (keywordp (first e)))
          (simulate-clause-error clause "(:final (name form) ...) with keyword names, or (:final form)")))
      (let ((names (mapcar #'first entries)))
        (unless (= (length names) (length (remove-duplicates names)))
          (simulate-clause-error clause "distinct output names")))
      entries))

  (defun literal-target-p (x)
    "True if X is written as a target: an underlying keyword or (kind key)."
    (or (keywordp x) (and (consp x) (keywordp (first x)))))

  (defun literal-legs-form (process)
    "PROCESS as a form: a literal list of legs (target process-form &rest
options), the target written as data, becomes a form making the list with
the process and option forms evaluated; anything else is evaluated as it is."
    (if (and (consp process) (consp (first process)) (literal-target-p (first (first process))))
        `(list ,@(mapcar (lambda (leg)
                           (unless (and (consp leg) (literal-target-p (first leg)) (<= 2 (length leg)))
                             (signal-invalid-argument 'process process 'list
                                                      "a list of legs (target process &rest options)"))
                           `(list ',(first leg) ,@(rest leg)))
                         process))
        process))

  (defun literal-correlation-form (correlation)
    "CORRELATION as a form: a literal list of (leg leg rho-form), the legs
written as data, becomes a form with each rho evaluated; anything else (a
quoted list, a variable, an array form) is evaluated as it is."
    (if (and (consp correlation) (consp (first correlation))
             (literal-target-p (first (first correlation))))
        `(list ,@(mapcar (lambda (entry)
                           (if (and (consp entry) (= 3 (length entry)))
                               `(list ',(first entry) ',(second entry) ,(third entry))
                               `',entry))
                         correlation))
        correlation))

  (defun calls-named-p (tree names)
    "True if TREE contains a form whose operator's name is one of NAMES."
    (and (consp tree)
         (or (and (symbolp (first tree)) (member (symbol-name (first tree)) names :test #'string=))
             (some (lambda (x) (calls-named-p x names)) (if (a:proper-list-p tree) tree (list (car tree) (cdr tree)))))))

  (defun named-call-arguments (tree name)
    "The first arguments of the forms in TREE whose operator's name is NAME."
    (when (consp tree)
      (append (when (and (symbolp (first tree)) (string= (symbol-name (first tree)) name)
                         (consp (rest tree)))
                `(,(second tree)))
              (if (a:proper-list-p tree)
                  (a:mappend (lambda (x) (named-call-arguments x name)) tree)
                  (append (named-call-arguments (car tree) name) (named-call-arguments (cdr tree) name))))))

  (defun parse-portfolio (clause)
    "(values var init record engine cash cash-curve collect) of a :PORTFOLIO
clause; CASH is NIL without :CASH."
    (let ((syntax "(:portfolio var [init] &key record engine cash cash-curve collect)"))
      (destructuring-bind (var &rest rest) (rest clause)
        (unless (and (symbolp var) var (not (keywordp var)))
          (simulate-clause-error clause syntax))
        (let* ((init (if (and rest (not (keywordp (first rest)))) (pop rest) nil))
               (options rest))
          (unless (and (evenp (length options))
                       (iter (for (k nil) on options by #'cddr)
                             (always (member k '(:record :engine :cash :cash-curve :collect)))))
            (simulate-clause-error clause syntax))
          (unless (and (listp (getf options :record))
                       (every (lambda (k) (member k `(:value :cash ,@*greek-keys*))) (getf options :record)))
            (simulate-clause-error clause (format nil ":RECORD keys from :VALUE, :CASH and the Greeks ~{~S~^ ~}"
                                                  *greek-keys*)))
          (values var
                  (or init `(make-portfolio ,(string-downcase (symbol-name var))))
                  (getf options :record)
                  (getf options :engine :auto)
                  (getf options :cash)
                  (getf options :cash-curve)
                  (getf options :collect :all)))))))

(defmacro simulate ((process market dates &rest options) &body clauses)
  "Simulate paths of PROCESS from MARKET's valuation date through DATES, run
the clauses' logic on every path, and return a SIMULATION-RESULT. DATES is an
ascending list of dates after the valuation date; the last is the end of every
path.

PROCESS is a process, or a literal list of legs (target process &rest
options), each leg's process driving one market component, its target:
(:spot u) the spot of underlying U, (:vol u), (:dividend u),
(:dividend-schedule u) or (:curve id). A bare keyword U is (:spot u), so
((:spx (make-gbm)) (:vix (make-heston))) simulates two spots. The leg options
go to STRUCTURE-AT-STATE. A process given alone drives the market's only
underlying's spot. OPTIONS:
  :n-paths 10000     paths simulated.
  :seed 42           the random seed.
  :rng :counter      :counter (a path's draws do not depend on N-PATHS, chunks
                     or workers), :sobol, or a random-state designator.
  :antithetic NIL    pair each path with its mirror image; statistics and
                     SIMULATION-SUMMARY then work on pair means.
  :n-steps :max-dt   fill the gaps between DATES with extra steps (default one
                     step per gap); the clauses still run only on DATES.
  :correlation       correlations between legs, a list of (leg leg rho) naming
                     legs by target or underlying, e.g. ((:spx :ndx 0.6d0)),
                     or a factor matrix, as in MAP-SIMULATED-PATHS.
  :keep              what to store for later: :QUANTILES (per-date quantiles of
                     every recorded name), (:PATHS k) (states and held values of
                     the first k paths; k is evaluated), :STATES, or a list of
                     these.
  :market-from-state a function building a date's market from the simulated
                     states, replacing MARKET-AT-STATE.
  :fix               a list of rate indexes, e.g. (list (euribor #T\"6M\")). On
                     each date the index fixes, its fixing is read from that
                     date's simulated curve, and the markets of later dates
                     hold it, so instruments priced there use the path's own
                     past fixings. Pricing needs every fixing date among DATES
                     (FIXING-DATES lists an instrument's).

Optional clauses. :ACCUMULATE and :HOLD may repeat, :PORTFOLIO once per
variable, and :FINAL appears once; the logic clauses run in the order
written.
  (:accumulate (var init) ...)
                     per-path DOUBLE-FLOAT variables, reset to INIT on every
                     path (an integer INIT is coerced; assigning a non-double
                     is an error).
  (:hold (var init) ...)
                     per-path variables of any type; INIT is evaluated afresh
                     on every path, so (:hold (v (make-array 5))) gives each
                     path its own array. Readable later with SIMULATION-HELD
                     for the kept paths.
  (:portfolio var [init] &key record engine cash cash-curve collect)
                     VAR is a per-path PORTFOLIO (INIT, default empty), changed
                     with ADJUST-POSITION and priced with ENGINE. RECORD lists
                     :VALUE, :CASH and Greek keys; each is recorded every date
                     under the name (var-keyword key), e.g. (:book :delta).
                     Several clauses simulate several books on the same
                     paths, each with its own cash. Without :FINAL the
                     outputs are the books' values: :VALUE for one book,
                     (var-keyword) for each of several.
                     CASH (a real, evaluated per path), or a CASH-ACCOUNT
                     position in INIT, makes the book self-financing: on each
                     date after the first, before the clauses, its cash grows
                     by D(t_k-1)/D(t_k) (the path's discount on the
                     currency's discount curve, or CASH-CURVE's; the initial
                     curve's when no leg simulates it), then each position's
                     income over the interval (dividends, coupons and
                     redemptions, option payoffs at expiry; COLLECT lists the
                     kinds paid, default :ALL of :DISCRETE-DIVIDEND
                     :CONTINUOUS-DIVIDEND :COUPON :REDEMPTION :EXPIRY) is paid
                     into it, and matured bonds and expired options leave
                     the book. An option held must expire on a date of DATES.
  (:state var name [leg])
                     VAR is bound to the double-float state component NAME
                     (:spot, :variance, ... see SIMULATION-STATE-NAMES) of the
                     leg (an underlying, or a target) at the current date.
  (:derive target ([market [date]]) form ...)
                     sets TARGET's component in every market built from the
                     paths to the value of the forms, computed from DATE,
                     MARKET (that date's market, with the legs' components
                     and earlier :DERIVE targets already set) and the :STATE
                     variables. The forms cannot see :ACCUMULATE or :HOLD
                     variables, RECORD or STOP, so the component depends only
                     on the date and the simulated states. TARGET may not be
                     a leg's. For example, a flat implied vol that
                     follows the spot S (a :STATE variable):
                       (:derive (:vol :spx) ()
                         (make-instance 'flat-black-vol :vol (* 0.002d0 s)))
  (:start [([market] &key time)] form ...)
                     at the valuation date, before the first step. The optional
                     (market) binds that date's market, TIME 0. RECORD here
                     goes to the valuation date, the first of SIMULATION-DATES.
  (:every (date [market] &key time dt) form ...)
                     at every date of DATES. DATE is bound to the date, MARKET
                     to the market on it built from the simulated states, TIME
                     to its market time and DT to TIME less the previous
                     date's (doubles).
  (:on date-list (i [market] &key time dt) form ...)
                     at the dates of DATE-LIST (a subset of DATES), I being the
                     date's position in the list.
  (:final (name form) ...)
                     one clause: named per-path outputs, evaluated after the
                     last date (or after STOP) from the final state; NAME is a
                     keyword. (:final form) is (:final (:value form)). MARKET
                     in the forms is the market of the path's last date.
MARKET is built only if the
clause uses it, and a global special variable may serve as its name. Clause code runs in
 serial. An error in a clause is re-signalled in the caller.

Inside :START, :EVERY and :ON bodies these local operators are available,
matched by name in any package:
  (record name value)  adds the real VALUE to the statistic NAME at the current
                       date. NAME is a keyword. The statistic at a date is
                       over all the values recorded under NAME at it. The
                       streamed statistics are the mean, standard deviation, minimum,
                       maximum and count (quantiles also with :KEEP :QUANTILES);
                       individual values are not stored. It returns VALUE.
  (stop)               ends the path: its later dates run no clauses and
                       record nothing, and :FINAL still runs.
  (path-discount [curve])
                       the discount from the valuation date to the current
                       date along this path, on CURVE ((:curve id); default the
                       discount curve of the market's only currency). When a
                       leg simulates that curve it is exp(-integral of the
                       short rate) on the path; otherwise it is the initial
                       market's discount factor. Also available in :FINAL.
  (previous var)       the :STATE variable VAR's value at the previous date
                       (the valuation date's at the first).
With :PORTFOLIO clauses, also, BOOK being one of their variables and
holding cash:
  (trade book instrument &key by to engine)
                       buys or sells INSTRUMENT in BOOK to quantity TO, or BY
                       a change, at its price on the date's market (ENGINE,
                       else the position's, else the book's), paid from
                       BOOK's cash; returns the cash paid. A position sold
                       to 0 leaves the book.
  (deposit book amount) (withdraw book amount)
                       move a non-negative AMOUNT into or out of BOOK's
                       cash; return the balance.

A leg's process may be a PHYSICAL-PROCESS (CHANGE-MEASURE): its state then
evolves under the physical measure, while each date's market is built from
that state with the risk-neutral process, so prices in the clauses stay
arbitrage-free. Legs under both measures form one physical model in which
the risk-neutral legs carry no premium. SIMULATION-MEASURE is then
:PHYSICAL, and discounted means of outputs are physical expectations, not
prices.

Results of simulation in SIMULATION-RESULT:
  :final outputs   SIMULATION-VALUES (one number per path), SIMULATION-MEAN,
                   SIMULATION-STANDARD-ERROR, SIMULATION-QUANTILE. Differences
                   of outputs are paired estimates on common random numbers.
  recorded names   SIMULATION-STATISTIC (a vector over SIMULATION-DATES),
                   SIMULATION-RECORDED-NAMES.
  kept paths       SIMULATION-PATH-STATE, SIMULATION-HELD, SIMULATION-PATH-RECORD
                   (each RECORD of the path, by date), PATH-MARKETS.
  any vector       SIMULATION-SUMMARY gives the mean and standard error.
The function underneath is MAP-SIMULATED-PATHS.

  ;; A short CALL delta-hedged on DATES, which end on its EXPIRY. The 
  ;; balance is recorded each date (read it with SIMULATION-STATISTIC) and the
  ;; terminal P&L is a :final output (read it with SIMULATION-VALUES).
  (simulate ((make-gbm) market dates :n-paths 20000)
    (:accumulate (shares 0d0) (cash 0d0) (premium 0d0))
    (:state s :spot)
    (:start (market)
      (setf premium (price call :black-scholes market))
      (incf cash premium))
    (:every (date market)
      (when (date< date expiry)
        (let ((delta (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
          (decf cash (* (- delta shares) s))
          (setf shares delta)))
      (record :balance (- (+ cash (* shares s)) (price call :black-scholes market))))
    (:final (:hedged (+ cash (* shares s) (- (payoff-value (option-payoff call) s))))
            (:unhedged (- premium (payoff-value (option-payoff call) s)))))

  ;; The same hedge as a self-financing book: the short call's payoff is
  ;; paid from cash at EXPIRY, so the book's final value is the P&L.
  (simulate ((make-gbm) market dates :n-paths 20000)
    (:portfolio book (make-portfolio \"hedge\" (make-position call :quantity -1))
                :cash (price call :black-scholes market) :engine :black-scholes)
    (:every (date market)
      (when (date< date expiry)
        (trade book (make-holding) :to (getf (greeks call :black-scholes market :which '(:delta)) :delta))))
    (:final (:pnl (price book :black-scholes market))))

  ;; The exposure of a SWAP when the curve it prices on follows Hull-White:
  ;; its INDEX fixes along each path on the simulated curve, and
  ;; PATH-DISCOUNT discounts along the path.
  (simulate ((make-hull-white :a 0.03d0 :sigma 0.008d0) market
             (sort (append dates (fixing-dates swap market)) #'date<)
             :n-paths 5000 :fix (list index) :keep :quantiles)
    (:every (date market)
      (record :exposure (max 0d0 (swap-npv swap market)))
      (record :discounted (* (path-discount) (swap-npv swap market))))
    (:final (:pv (* (path-discount) (swap-npv swap market)))))"
  (let* ((record-name (intern "RECORD")) (stop-name (intern "STOP"))
         (discount-name (intern "PATH-DISCOUNT")) (previous-name (intern "PREVIOUS"))
         (fixings (gensym "FIXINGS"))
         (event-times (gensym "TIMES")) (event-steps (gensym "STEPS")) (previous (gensym "PREVIOUS"))
         (trade-name (intern "TRADE")) (deposit-name (intern "DEPOSIT")) (withdraw-name (intern "WITHDRAW"))
         (book-states (gensym "BOOK-STATES"))
         (book-market (gensym "BOOK-MARKET"))
         (plan (gensym "PLAN")) (store (gensym "STORE")) (stats (gensym "STATS"))
         (path (gensym "PATH")) (k (gensym "K")) (states (gensym "STATES"))
         (alive (gensym "ALIVE")) (cached (gensym "MARKET")) (market-fn (gensym "MARKET-AT"))
         (values-vector (gensym "VALUES"))
         (keep (getf options :keep))
         (plan-options (let ((rest (a:remove-from-plist options :keep)))
                         (if (getf rest :correlation)
                             `(:correlation ,(literal-correlation-form (getf rest :correlation))
                               ,@(a:remove-from-plist rest :correlation))
                             rest)))
         accumulators holds portfolios state-vars derives starts everys ons finals)
    (dolist (clause clauses)
      (unless (and (consp clause) (member (first clause) *simulate-clauses*))
        (simulate-clause-error clause (format nil "one of ~{~S~^, ~}" *simulate-clauses*)))
      (ecase (first clause)
        (:accumulate (dolist (b (rest clause))
                       (unless (and (consp b) (symbolp (first b)) (= 2 (length b)))
                         (simulate-clause-error clause "(:accumulate (var init) ...)"))
                       (push b accumulators)))
        (:hold (dolist (b (rest clause))
                 (unless (and (consp b) (symbolp (first b)) (= 2 (length b)))
                   (simulate-clause-error clause "(:hold (var init) ...)"))
                 (push b holds)))
        (:portfolio (push (multiple-value-list (parse-portfolio clause)) portfolios))
        (:state (unless (<= 3 (length clause) 4)
                  (simulate-clause-error clause "(:state var name [underlying])"))
                (push `(,(second clause) ,(third clause)
                        ,(if (consp (fourth clause)) `',(fourth clause) (fourth clause))
                        ,(gensym "LEG") ,(gensym "INDEX"))
                      state-vars))
        (:derive (destructuring-bind (&optional target binding &rest body) (rest clause)
                   (unless (and (literal-target-p target) (listp binding) (<= (length binding) 2)
                                (every (lambda (s) (and (symbolp s) s (not (keywordp s)))) binding))
                     (simulate-clause-error clause "(:derive target ([market [date]]) form ...)"))
                   (when (calls-named-p body '("RECORD" "STOP"))
                     (simulate-clause-error clause "a :DERIVE body without RECORD or STOP (it runs per market, not per path)"))
                   (push `(,target ,(or (first binding) (gensym "MARKET")) ,(or (second binding) (gensym "DATE"))
                           ,body)
                         derives)))
        (:start (destructuring-bind (&rest body) (rest clause)
                  (let ((binding (when (clause-binding-list-p (first body)) (pop body))))
                    (multiple-value-bind (variables keys)
                        (parse-clause-binding clause binding 0 1 '("TIME") "(:start [([market] &key time)] form ...)")
                      (push `(,(first variables) ,body ,(first keys)) starts)))))
        (:every (destructuring-bind (binding &rest body) (rest clause)
                  (multiple-value-bind (variables keys)
                      (parse-clause-binding clause binding 1 2 '("TIME" "DT")
                                            "(:every (date [market] &key time dt) form ...)")
                    (push `(:every ,(first variables) ,(second variables) ,body ,@keys) everys))))
        (:on (destructuring-bind (dates-form binding &rest body) (rest clause)
               (multiple-value-bind (variables keys)
                   (parse-clause-binding clause binding 1 2 '("TIME" "DT")
                                         "(:on date-list (i [market] &key time dt) form ...)")
                 (push `(:on ,(first variables) ,(second variables) ,body ,dates-form
                             ,(gensym "ON-DATES") ,(gensym "POSITIONS") ,@keys)
                       everys))
               (push (first everys) ons)))
        (:final (when finals (simulate-clause-error clause "one :FINAL clause"))
                (setf finals (parse-final clause)))))
    (iter (for (var) on (mapcar #'first portfolios))
          (when (member var (rest (member var (mapcar #'first portfolios))))
            (simulate-clause-error `(:portfolio ,var) "one :PORTFOLIO clause per variable")))
    ;; Each book: (var init record engine cash cash-curve collect accounting
    ;; cash-target last-discount), the last three its own variables.
    (setf portfolios (mapcar (lambda (p) `(,@p ,(gensym "ACCOUNTING") ,(gensym "CASH-TARGET")
                                              ,(gensym "LAST-DISCOUNT")))
                             (nreverse portfolios)))
    (setf accumulators (nreverse accumulators) holds (nreverse holds)
          state-vars (nreverse state-vars) derives (nreverse derives) starts (nreverse starts)
          everys (nreverse everys) ons (nreverse ons))
    (let* ((books (mapcar (lambda (p) (cons (first p) (fourth p))) portfolios))
           (outputs (or finals
                        (iter (for (var nil nil engine) in portfolios)
                              (collect `(,(if (rest portfolios) (a:make-keyword (symbol-name var)) :value)
                                         (price ,var ,engine (,market-fn)))))))
           (held-vars `(,@(mapcar #'first holds) ,@(mapcar #'first portfolios)))
           (uses-previous (calls-named-p clauses '("PREVIOUS"))))
      (dolist (var (named-call-arguments clauses "PREVIOUS"))
        (unless (find var state-vars :key #'first)
          (signal-invalid-argument 'previous var 'symbol
                                   (format nil "a :STATE variable in (previous var), one of ~:[none~;~:*~{~S~^, ~}~] ~
(carry other values from date to date in an :ACCUMULATE variable)"
                                           (mapcar #'first state-vars)))))
      (when portfolios
        (dolist (operator '(trade deposit withdraw))
          (dolist (book (named-call-arguments clauses (symbol-name operator)))
            (simulate-book-engine operator book books))))
      (labels ((with-states (body)
                 `(let ,(iter (for (var nil nil leg index) in state-vars)
                              (collect `(,var (aref (the double-vector (svref ,states ,leg)) ,index))))
                    (declare (ignorable ,@(mapcar #'first state-vars))
                             ,@(when state-vars `((double-float ,@(mapcar #'first state-vars)))))
                    ,@body))
               (with-times (time-var dt-var body)
                 ;; TIME and DT of event K, from vectors the plan gives once.
                 `(let (,@(when time-var `((,time-var (aref ,event-times ,k))))
                        ,@(when dt-var `((,dt-var (aref ,event-steps ,k)))))
                    (declare (ignorable ,@(remove nil `(,time-var ,dt-var))))
                    ,@body))
               (with-market (market-var body)
                 (cond ((null market-var) `(progn ,@body))
                       ;; SYMBOL-MACROLET cannot bind a special variable (a
                       ;; global MARKET from DEFVAR): rebind it, when used.
                       ((special-variable-p market-var)
                        (if (member market-var (a:flatten body))
                            `(let ((,market-var (,market-fn))) ,@body)
                            `(progn ,@body)))
                       (t `(symbol-macrolet ((,market-var (,market-fn))) ,@body))))
               (record-book ()
                 (iter (for (book nil record engine) in portfolios)
                       (for book-key = (a:make-keyword (symbol-name book)))
                       (collect
                           `(unless (typep ,book 'portfolio)
                              (signal-invalid-argument ',book ,book 'portfolio
                                                       "a PORTFOLIO (set the book with ADJUST-POSITION or MAKE-PORTFOLIO)")))
                       (when (member :cash record)
                         (collect `(record-value ,stats ,store '(,book-key :cash) ,path ,k
                                                 (progn (check-book-cash ,book :record) (book-balance ,book)))))
                       (when (intersection record `(:value ,@*greek-keys*))
                         (let ((greek-keys (remove :cash (remove :value record))))
                           (collect
                               `(let ((m (,market-fn)))
                                  ,@(when (member :value record)
                                      `((record-value ,stats ,store '(,book-key :value) ,path ,k
                                                      (price ,book ,engine m))))
                                  ,@(when greek-keys
                                      `((let ((g (live-book-greeks ,book ,engine m ',greek-keys)))
                                          ,@(iter (for key in greek-keys)
                                                  (collect `(record-value ,stats ,store '(,book-key ,key)
                                                                          ,path ,k (getf g ,key)))))))))))))
               (accrue-book ()
                 ;; D5 steps 1 and 2 for each book holding cash: interest at
                 ;; the growth of cash over the interval, from the path's
                 ;; discount.
                 (iter (for (book nil nil nil nil nil nil accounting cash-target last-discount) in portfolios)
                       (collect
                           `(unless (eq ,cash-target :none)
                              (let ((d (simulation-path-discount ,plan ,k ,states ,cash-target)))
                                (check-book-cash ,book :interest)
                                (setf ,book (set-book-balance ,book (* (book-balance ,book) (/ ,last-discount d)))
                                      ,last-discount d))
                              ;; Income over the interval; the previous date's
                              ;; market is the one built then, or built now
                              ;; from its states.
                              (setf ,book (book-income ,book ,accounting ,k ,cash-target
                                                       (lambda ()
                                                         (or ,book-market
                                                             (setf ,book-market
                                                                   (simulation-market ,plan (1- ,k) ,book-states
                                                                                      ,fixings))))
                                                       #',market-fn))))))
               (remember-book-date ()
                 ;; What the next date's income needs of this one.
                 (when portfolios
                   `((unless (and ,@(iter (for p in portfolios) (collect `(eq ,(ninth p) :none))))
                       (setf ,book-market ,cached
                             ,book-states (copy-states-into ,book-states ,states))))))
               (keep-hook ()
                 `(when (keeping-path-p ,store ,path)
                    (store-path-date ,store ,path ,k ,states
                                     (list ,@(iter (for var in held-vars)
                                                   (appending `(,(a:make-keyword (symbol-name var)) ,var))))))))
        ;; The state locations are bound before the plan, which the :DERIVE
        ;; functions go into, and set from it before any path runs.
        `(let* (,@(iter (for (nil nil nil nil dates-form on-dates) in ons)
                        (collect `(,on-dates ,dates-form)))
                ,@(iter (for (nil nil nil leg index) in state-vars)
                        (appending `((,leg 0) (,index 0))))
                (,plan (make-simulation-plan
                        ,(literal-legs-form process) ,market ,dates
                        :on-dates (append ,@(mapcar #'sixth ons))
                        ,@(when derives
                            `(:derive
                              (list ,@(iter (for (target market-var date-var body) in derives)
                                            (collect
                                                `(list ',target
                                                       (lambda (,market-var ,date-var ,states)
                                                         (declare (ignorable ,market-var ,date-var ,states)
                                                                  (simple-vector ,states))
                                                         ,(with-states body))))))))
                        ,@plan-options))
                (,store (make-simulation-store ,plan ,(keep-form keep)))
                (,event-times (simulation-event-times ,plan))
                (,event-steps (simulation-event-steps ,event-times))
                ,@(iter (for (nil nil nil nil nil cash-curve collect accounting) in portfolios)
                        (collect `(,accounting (make-book-accounting ,plan ,cash-curve ,collect))))
                ,@(iter (for (nil nil nil nil nil on-dates positions) in ons)
                        (collect `(,positions (on-positions ,plan ,on-dates)))))
           (declare (fixnum ,@(iter (for (nil nil nil leg index) in state-vars) (appending `(,leg ,index))))
                    (type double-vector ,event-times ,event-steps)
                    (ignorable ,event-times ,event-steps))
           (progn
             ,@(iter (for (nil name underlying leg index) in state-vars)
                     (collect `(multiple-value-setq (,leg ,index)
                                 (simulation-state-location ,plan ,name ,underlying)))))
             (multiple-value-bind (outputs chunks)
                 (run-simulation
                  ,plan
                  (lambda (chunk)
                    (declare (ignore chunk))
                    (let ((,stats (make-hash-table :test 'equal))
                          (,path 0) (,k 0) (,states nil) (,alive t) (,cached nil)
                          (,fixings (make-fixing-buffer ,plan))
                          ,@(when uses-previous `((,previous nil)))
                          ,@(iter (for p in portfolios) (appending `((,(ninth p) :none) (,(tenth p) 1d0))))
                          ,@(when portfolios `((,book-states nil) (,book-market nil)))
                          ,@(iter (for (var) in accumulators) (collect `(,var 0d0)))
                          ,@(mapcar (lambda (v) `(,v nil)) held-vars))
                      (declare (fixnum ,path ,k)
                               ,@(when portfolios `((double-float ,@(mapcar #'tenth portfolios))))
                               ,@(when accumulators `((double-float ,@(mapcar #'first accumulators))))
                               (ignorable ,stats ,alive))
                      (flet ((,market-fn ()
                               (or ,cached (setf ,cached (simulation-market ,plan ,k ,states ,fixings))))
                             (,discount-name (&optional target)
                               (simulation-path-discount ,plan ,k ,states target)))
                        (declare (ignorable #',market-fn #',discount-name))
                        (macrolet ((,record-name (name value)
                                     `(record-value ,',stats ,',store ,name ,',path ,',k ,value))
                                   (,stop-name () `(setf ,',alive nil))
                                   ,@(when uses-previous
                                       `((,previous-name (var)
                                           (let ((entry (assoc var ',(iter (for (var nil nil leg index) in state-vars)
                                                                           (collect `(,var ,leg ,index))))))
                                             (unless entry
                                               (signal-invalid-argument
                                                'previous var 'symbol
                                                "a :STATE variable (carry other values from date to date in an :ACCUMULATE variable)"))
                                             `(aref (the double-vector (svref ,',previous ,(second entry)))
                                                    ,(third entry))))))
                                   ,@(when portfolios
                                       `((,trade-name (book instrument &rest keys)
                                           (let ((engine (simulate-book-engine 'trade book ',books))
                                                 (b (gensym "BOOK")) (cost (gensym "COST")))
                                             `(multiple-value-bind (,b ,cost)
                                                  (book-trade ,book ,instrument (function ,',market-fn)
                                                              ,engine ,@keys)
                                                (setf ,book ,b)
                                                ,cost)))
                                         (,deposit-name (book amount)
                                           (simulate-book-engine 'deposit book ',books)
                                           `(progn (setf ,book (book-cash-flow ,book ,amount 'deposit))
                                                   (book-balance ,book)))
                                         (,withdraw-name (book amount)
                                           (simulate-book-engine 'withdraw book ',books)
                                           `(progn (setf ,book (book-cash-flow ,book ,amount 'withdraw))
                                                   (book-balance ,book))))))
                          (values
                           (lambda (p s)
                             (setf ,path p ,k 0 ,states s ,cached nil ,alive t)
                             (path-fixings! ,plan 0 s ,fixings)
                             ,@(when uses-previous `((setf ,previous (copy-states-into ,previous s))))
                             ,@(iter (for (var init) in accumulators) (collect `(setf ,var (float ,init 1d0))))
                             ,@(iter (for (var init) in holds) (collect `(setf ,var ,init)))
                             ,@(iter (for (book init nil nil cash nil nil accounting cash-target last-discount)
                                      in portfolios)
                                     (appending `((setf ,book ,init ,last-discount 1d0)
                                                  (multiple-value-setq (,book ,cash-target)
                                                    (open-book ,book ,cash ,accounting)))))
                             ,@(iter (for (market-var body time-var) in starts)
                                     (collect (with-states `(,(with-times time-var nil
                                                                `(,(with-market market-var body)))))))
                             ,@(record-book)
                             ,(keep-hook)
                             ,@(remember-book-date)
                             ,alive)
                           (lambda (p kk s)
                             (declare (ignore p))
                             (setf ,k kk ,states s ,cached nil)
                             (path-fixings! ,plan kk s ,fixings)
                             ,@(accrue-book)
                             ,@(iter (for clause in everys)
                                     (collect
                                         (ecase (first clause)
                                           (:every
                                            (destructuring-bind (date-var market-var body time-var dt-var) (rest clause)
                                              `(let ((,date-var (aref (simulation-plan-event-dates ,plan) (1- ,k))))
                                                 (declare (ignorable ,date-var))
                                                 ,(with-states `(,(with-times time-var dt-var
                                                                    `(,(with-market market-var body))))))))
                                           (:on
                                            (destructuring-bind (index-var market-var body dates-form on-dates positions
                                                                 time-var dt-var)
                                                (rest clause)
                                              (declare (ignore dates-form on-dates))
                                              `(let ((,index-var (aref ,positions ,k)))
                                                 (when (>= ,index-var 0)
                                                   ,(with-states `(,(with-times time-var dt-var
                                                                      `(,(with-market market-var body))))))))))))
                             ,@(record-book)
                             ,(keep-hook)
                             ,@(remember-book-date)
                             ,@(when uses-previous `((copy-states-into ,previous s)))
                             ,alive)
                           (lambda (p s ,values-vector)
                             (declare (ignore p) (type double-vector ,values-vector) (ignorable ,values-vector))
                             (setf ,states s)
                             ,(with-states
                                  `(,(with-market
                                      (intern "MARKET")
                                      (iter (for (name form) in outputs)
                                            (for o from 0)
                                            (collect `(setf (aref ,values-vector ,o)
                                                            (output-value ,name ,path ,form))))))))
                           (lambda () ,stats))))))
                  :outputs ,(length outputs))
               (finish-simulation ,plan ,store ',(mapcar #'first outputs)
                                  ,(if outputs 'outputs nil) chunks)))))))

(defun on-positions (plan dates)
  "A fixnum vector over PLAN's event indices (1-based; 0 unused) holding each
event date's position in DATES, or -1."
  (let ((events (simulation-plan-event-dates plan))
        (positions (make-array (1+ (length (simulation-plan-event-dates plan)))
                               :element-type 'fixnum :initial-element -1)))
    (iter (for d in dates) (for i from 0)
          (setf (aref positions (1+ (position d events :test #'date=))) i))
    positions))
