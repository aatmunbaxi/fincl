;;;; portfolio.lisp --- holdings, positions, portfolios
;;;;
;;;; A position is an instrument and a signed quantity: long when positive, short when
;;;; negative. A portfolio is a named tree of positions and sub-portfolios. To price a
;;;; portfolio, the code prices each position on the same market snapshot, in parallel,
;;;; under a recovery policy. It reports results for each position and does not signal.
;;;; Thus a book is priced even when one of its positions cannot be.
;;;;
;;;; A HOLDING is the underlying itself. Thus a book can hold its hedge.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Holding the underlying
;;; --------------------------------------------------------------------

(defclass holding (instrument) ()
  (:metaclass component-class)
  (:documentation "One unit of an underlying, worth its spot. Its currency is the currency of the
underlying. Another currency is a quanto and signals UNSUPPORTED-QUANTO. The class does
not model borrow and financing costs. Dividends that are received are not part of its
value.

  (make-holding :underlying :spx)"))

(declaim (ftype (function (&key (:underlying t) (:currency t))
                    (values holding &optional))
          make-holding))
(defun make-holding (&key underlying currency)
  "Make a HOLDING of UNDERLYING (a keyword). NIL means the only underlying of the market.
CURRENCY is the currency of payment (NIL: the currency of the underlying).

  (make-holding :underlying :spx)"
  (make-instance 'holding :underlying underlying :currency currency))

;;; Nothing to model: any engine designator, :AUTO included, gives the spot.
;;; Specialized on HOLDING, so automatic selection (for options) is never
;;; entered; the composite market's :AROUND method narrows to the view.
(defmethod price ((h holding) engine market)
  (values (spot market) nil (and (typep engine 'engine) engine)))

(defmethod greeks ((h holding) engine market &key (which *greek-keys*) bumps)
  (declare (ignore engine market bumps))
  (iter (for key in which)
        (appending `(,key ,(if (eq key :delta) 1d0 0d0)))))

(defmethod print-object ((h holding) stream)
  (if *print-readably*
      (prin1 `(make-holding ,@(a:when-let ((u (instrument-underlying h))) `(:underlying ,u))
                            ,@(a:when-let ((c (instrument-currency h))) `(:currency ,c)))
             stream)
      (print-unreadable-object (h stream :type t)
        (format stream "~:[the market's underlying~;~:*~S~]" (instrument-underlying h)))))

;;; --------------------------------------------------------------------
;;; Cash
;;; --------------------------------------------------------------------

(defclass cash-account (instrument) ()
  (:metaclass component-class)
  (:documentation "One unit of a currency, worth 1 in that currency. The quantity of a position is a cash
balance (a negative balance is borrowed). Interest is not part of its value. The books of
SIMULATE accrue it.

  (make-position (make-cash-account :currency :usd) :quantity 1000)"))

(declaim (ftype (function (&key (:currency t)) (values cash-account &optional)) make-cash-account))
(defun make-cash-account (&key currency)
  "Make a CASH-ACCOUNT in CURRENCY (a keyword). NIL means the currency of the market.

  (make-cash-account :currency :usd)"
  (make-instance 'cash-account :currency currency))

;;; Priced on the market as a whole: cash has no underlying.
(defmethod instrument-view (market (c cash-account))
  market)

(defmethod price ((c cash-account) engine market)
  (declare (ignore market))
  (values 1d0 nil (and (typep engine 'engine) engine)))

(defmethod greeks ((c cash-account) engine market &key (which *greek-keys*) bumps)
  (declare (ignore engine market bumps))
  (iter (for key in which) (appending `(,key 0d0))))

(defmethod print-object ((c cash-account) stream)
  (if *print-readably*
      (prin1 `(make-cash-account ,@(a:when-let ((cur (instrument-currency c))) `(:currency ,cur))) stream)
      (print-unreadable-object (c stream :type t)
        (format stream "~:[the market's currency~;~:*~S~]" (instrument-currency c)))))

;;; --------------------------------------------------------------------
;;; Positions and portfolios
;;; --------------------------------------------------------------------

(defclass portfolio-position ()
  ((instrument :initarg :instrument :reader position-instrument)
   (quantity :initarg :quantity :reader position-quantity :type double-float)
   (engine :initarg :engine :initform nil :reader position-engine
           :documentation "An engine designator, or NIL for the portfolio's default.")
   (tags :initarg :tags :initform '() :reader position-tags :type list
         :documentation "A plist, e.g. (:desk :a :strategy :hedge)."))
  (:documentation "QUANTITY units of INSTRUMENT: long when positive, short when negative, closed at zero.
MAKE-POSITION makes it. It is immutable."))

(declaim (ftype (function (t &key (:quantity t) (:engine t) (:tags t))
                    (values portfolio-position &optional))
          make-position))
(defun make-position (instrument &key (quantity 1) engine tags)
  "Make a position of QUANTITY (a real, negative for short) in INSTRUMENT. ENGINE prices it
(a designator, NIL: the default engine of the portfolio). TAGS is a plist.

  (make-position (make-option (put 95) (expires #D\"2027-09-24\")) :quantity -10
                 :tags '(:desk :a))"
  (check-input instrument instrument "an instrument")
  (check-input quantity real "a real quantity (negative for a short position)")
  (check-input tags list "a plist of tags")
  (unless (evenp (length tags))
    (signal-invalid-argument 'tags tags 'list "a plist: keys and values in pairs"))
  (make-instance 'portfolio-position :instrument instrument :quantity (float quantity 1d0)
                                     :engine engine :tags tags))

(declaim (ftype (function (portfolio-position &key (:quantity real) (:engine t) (:tags list))
                    (values portfolio-position &optional))
          derive-position))
(defun derive-position (position &key (quantity (position-quantity position))
                                      (engine (position-engine position))
                                      (tags (position-tags position)))
  "Return a copy of POSITION with QUANTITY, ENGINE or TAGS replaced."
  (make-position (position-instrument position) :quantity quantity :engine engine :tags tags))

(defmethod print-object ((p portfolio-position) stream)
  (if *print-readably*
      (prin1 `(make-position ,(position-instrument p) :quantity ,(position-quantity p)
                             ,@(a:when-let ((e (position-engine p))) `(:engine ',e))
                             ,@(a:when-let ((tags (position-tags p))) `(:tags ',tags)))
             stream)
      (print-unreadable-object (p stream :type t)
        (format stream "~@D x ~A~@[ ~S~]" (position-quantity p) (position-instrument p)
                (position-tags p)))))

(defclass portfolio ()
  ((name :initarg :name :reader portfolio-name)
   (members :initarg :members :reader portfolio-members :type list
            :documentation "Positions and sub-portfolios, in order."))
  (:documentation "A named tree of positions and sub-portfolios. MAKE-PORTFOLIO makes it. It is immutable."))

(declaim (ftype (function (t &rest t) (values portfolio &optional)) make-portfolio))
(defun make-portfolio (name &rest members)
  "Make a PORTFOLIO called NAME (a string or keyword) of MEMBERS. Each member is a position
or a portfolio.

  (make-portfolio \"book\"
                  (make-position call :quantity 10 :tags '(:desk :a))
                  (make-portfolio \"hedges\" (make-position (make-holding) :quantity -6)))"
  (dolist (m members)
    (check-input m (or portfolio-position portfolio) "a position or a portfolio"))
  (make-instance 'portfolio :name name :members members))

(defgeneric same-instrument-p (a b)
  (:documentation "True if the instruments A and B are the same contract, so that ADJUST-POSITION adds to
an existing position. Options are compared by identity. Holdings are compared by
underlying and currency.")
  (:method (a b) (eq a b))
  (:method ((a holding) (b holding))
    (and (eq (instrument-underlying a) (instrument-underlying b))
         (eq (instrument-currency a) (instrument-currency b))))
  (:method ((a cash-account) (b cash-account))
    (eq (instrument-currency a) (instrument-currency b))))

(declaim (ftype (function (t t &key (:by (or null real)) (:to (or null real)))
                    (values portfolio &optional))
          adjust-position))
(defun adjust-position (portfolio instrument &key by to)
  "Return a copy of PORTFOLIO. In the copy, the top-level position in INSTRUMENT (see
SAME-INSTRUMENT-P) has its quantity moved BY a real, or set TO a real. If there is no
such position, the function appends one with quantity BY or TO. Give exactly one of BY
and TO. PORTFOLIO does not change.

  (adjust-position book (make-holding :underlying :spx) :to -0.53d0)   ; rebalance a hedge"
  (check-input portfolio portfolio "a portfolio")
  (unless (and (or by to) (not (and by to)))
    (signal-invalid-argument 'by by 'real "exactly one of :BY and :TO, a real"))
  (let* ((members (portfolio-members portfolio))
         (old (find-if (lambda (m) (and (typep m 'portfolio-position)
                                        (same-instrument-p (position-instrument m) instrument)))
                       members)))
    (apply #'make-portfolio (portfolio-name portfolio)
           (if old
               (substitute (derive-position old :quantity (if to
                                                               (float to 1d0)
                                                               (+ (position-quantity old) by)))
                           old members)
               `(,@members ,(make-position instrument :quantity (or to by)))))))

(declaim (ftype (function (portfolio) (values list &optional)) portfolio-positions))
(defun portfolio-positions (portfolio)
  "Return every position in the tree of PORTFOLIO, depth first, in order."
  (iter (for m in (portfolio-members portfolio))
        (if (typep m 'portfolio)
            (appending (portfolio-positions m))
            (collect m))))

(defun filter-portfolio (portfolio predicate &optional (name (portfolio-name portfolio)))
  "The portfolio of PORTFOLIO's positions satisfying PREDICATE, flat."
  (apply #'make-portfolio name (remove-if-not predicate (portfolio-positions portfolio))))

(declaim (ftype (function (portfolio) (values portfolio &optional)) portfolio-long))
(defun portfolio-long (portfolio)
  "Return the positions of PORTFOLIO with a positive quantity, as a flat portfolio."
  (filter-portfolio portfolio (lambda (p) (plusp (position-quantity p)))
                    (format nil "~A (long)" (portfolio-name portfolio))))

(declaim (ftype (function (portfolio) (values portfolio &optional)) portfolio-short))
(defun portfolio-short (portfolio)
  "Return the positions of PORTFOLIO with a negative quantity, as a flat portfolio."
  (filter-portfolio portfolio (lambda (p) (minusp (position-quantity p)))
                    (format nil "~A (short)" (portfolio-name portfolio))))

(declaim (ftype (function (portfolio t) (values list &optional)) portfolio-by-tag))
(defun portfolio-by-tag (portfolio key)
  "Return ((value . portfolio) ...): the positions of PORTFOLIO grouped by the value of
their tag KEY, in order of first appearance. Positions without the tag have the value
NIL.

  (portfolio-by-tag book :desk) => ((:a . #<PORTFOLIO ...>) (:b . ...))"
  (let ((groups '()))
    (dolist (p (portfolio-positions portfolio))
      (let* ((value (getf (position-tags p) key))
             (group (assoc value groups :test #'equal)))
        (if group (push p (cdr group)) (push (list value p) groups))))
    (iter (for (value . positions) in (reverse groups))
          (collect (cons value (apply #'make-portfolio (format nil "~A ~S=~S" (portfolio-name portfolio)
                                                               key value)
                                      (reverse positions)))))))

(defmethod print-object ((p portfolio) stream)
  (if *print-readably*
      (prin1 `(make-portfolio ,(portfolio-name p) ,@(portfolio-members p)) stream)
      (print-unreadable-object (p stream :type t)
        (format stream "~S, ~D position~:P" (portfolio-name p) (length (portfolio-positions p))))))

;;; --------------------------------------------------------------------
;;; Which engines can price an instrument
;;; --------------------------------------------------------------------

(defgeneric engine-preference (engine)
  (:documentation "The rank of ENGINE (an instance) in the order of APPLICABLE-ENGINES, lower first: closed
forms 10 (Black-Scholes before RGW and BAW), COS 20, finite differences 30, the tree 40,
Monte Carlo 90.")
  (:method ((e engine)) 50)
  (:method ((e analytic-engine)) 12)
  (:method ((e black-scholes-engine)) 10)
  (:method ((e roll-geske-whaley-engine)) 11)
  (:method ((e cos-engine)) 20)
  (:method ((e fd-engine)) 30)
  (:method ((e binomial-engine)) 40)
  (:method ((e monte-carlo-engine)) 90))

(defgeneric engine-dispatch (engine option)
  (:documentation "(values generic-function arguments): the pricing generic
function ENGINE's PRICE method dispatches OPTION to, and its arguments.
APPLICABLE-ENGINES inspects its methods.")
  (:method ((e analytic-engine) o)
    (values #'price-analytic `(,(option-exercise o) ,(option-payoff o) ,(option-path o) ,e nil)))
  (:method ((e monte-carlo-engine) o)
    (values #'price-mc `(,(option-exercise o) ,(option-payoff o) ,(option-path o) ,e nil)))
  (:method ((e cos-engine) o)
    (values #'price-cos `(,(option-exercise o) ,(option-payoff o) ,(option-path o) ,(process e) ,e nil)))
  (:method ((e binomial-engine) o)
    (values #'price-binomial `(,(option-exercise o) ,(option-payoff o) ,(option-path o)
                               ,(process e) ,e nil)))
  (:method ((e fd-engine) o)
    (values #'price-fd `(,(option-exercise o) ,(option-payoff o) ,(option-path o) ,(process e) ,e nil))))

(defun fallback-method-p (method)
  "True for the methods that only signal UNSUPPORTED-COMBINATION: every
specializer T except ENGINE's."
  (every (lambda (s) (member (class-name s) '(t engine)))
         (remove-if-not (lambda (s) (typep s 'class)) (c2mop:method-specializers method))))

(defun engine-has-method-p (engine option market)
  "True if ENGINE has a pricing method for OPTION more specific than the
UNSUPPORTED-COMBINATION fallback, by its classes alone."
  (multiple-value-bind (gf arguments) (engine-dispatch engine option)
    (let ((classes (mapcar #'class-of (substitute market nil arguments))))
      (multiple-value-bind (methods definitive)
          (c2mop:compute-applicable-methods-using-classes gf classes)
        (let ((methods (if definitive
                           methods
                           (compute-applicable-methods gf (substitute market nil arguments)))))
          (some (lambda (m) (and (null (method-qualifiers m)) (not (fallback-method-p m))))
                methods))))))

(defvar *default-engines* nil
  "The engine designators that APPLICABLE-ENGINES tries by default. NIL means every
registered designator whose engine class has an ENGINE-DISPATCH method (see
DEFAULT-ENGINES). Thus an engine that a user defines is found. A list replaces
the default.")

(defparameter *built-in-engine-order*
  '(:black-scholes :rgw :baw :cos :fd :binomial :mc :merton-series :cev)
  "The built-in designators in the order that breaks ties of ENGINE-PREFERENCE.")

(defun default-engines ()
  "The designators of the registered engines that price options: the
built-in ones in *BUILT-IN-ENGINE-ORDER*, then the others sorted. An engine
prices options when ENGINE-DISPATCH has a method for its class.

  (default-engines) => (:black-scholes :rgw :baw ... :cev :my-engine)"
  (let* ((option (find-class 'option))
         (pricing (remove-if-not
                   (lambda (key)
                     (c2mop:compute-applicable-methods-using-classes
                      #'engine-dispatch `(,(designated-class key) ,option)))
                   (engine-designators))))
    `(,@(remove-if-not (lambda (key) (member key pricing)) *built-in-engine-order*)
      ,@(remove-if (lambda (key) (member key *built-in-engine-order*)) pricing))))

(defgeneric applicable-engines (instrument market &key candidates process)
  (:documentation "Return the engines (as instances) that have a pricing method for INSTRUMENT, ordered by
ENGINE-PREFERENCE. They come from CANDIDATES (designators, default *DEFAULT-ENGINES*,
else DEFAULT-ENGINES). Each engine must model PROCESS (default: GBM on the market). The
pricing generic function of the engine must have a method for the exercise, payoff and
path classes of the instrument that is more specific than the UNSUPPORTED-COMBINATION
fallback. A listed engine can still decline in a market regime (MODEL-DOMAIN-ERROR, for
example discrete dividends that it does not model). SELECT-ENGINE and PRICE-PORTFOLIO
move on when it does.

  (applicable-engines (make-option (put 100) (american) (expires d)) m :process (make-heston))
  => (#<ENGINE (make-engine :fd :process (make-heston))> #<ENGINE (make-engine :mc ...)>)")
  (:method ((o option) market &key (candidates (or *default-engines* (default-engines))) (process (make-gbm)))
    (require-risk-neutral process "APPLICABLE-ENGINES")
    (let ((market (pricing-market market o)))
      (stable-sort (iter (for key in candidates)
                         (for class = (designated-class key))
                         (when (class-models-process-p class process)
                           (let ((engine (handler-case (ensure-engine key process)
                                           (invalid-argument () nil))))
                             (when (and engine (engine-has-method-p engine o market))
                               (collect engine)))))
                   #'< :key #'engine-preference)))
  (:method ((h holding) market &key candidates process)
    (declare (ignore market candidates process))
    ;; Any engine prices a holding; the Black-Scholes one stands for all.
    `(,(ensure-engine :black-scholes)))
  (:method ((c cash-account) market &key candidates process)
    (declare (ignore market candidates process))
    `(,(ensure-engine :black-scholes))))

(declaim (ftype (function (t t &rest t &key (:candidates t) (:process t))
                    (values t &optional))
          select-engine))
(defun select-engine (instrument market &rest options &key candidates process)
  "Return the first of APPLICABLE-ENGINES for INSTRUMENT, or NIL."
  (declare (ignore candidates process))
  (first (apply #'applicable-engines instrument market options)))

;;; --------------------------------------------------------------------
;;; Pricing a portfolio
;;; --------------------------------------------------------------------

(defstruct (position-result (:constructor make-position-result
                                (position unit-price standard-error engine condition)))
  "The pricing of one position. UNIT-PRICE and STANDARD-ERROR are for one unit of the
instrument (NIL if it could not be priced). ENGINE is the engine that priced it.
CONDITION is the condition that the pricing recovered from or, if the position is
unpriced, the last condition."
  (position nil :read-only t)
  (unit-price nil :read-only t)
  (standard-error nil :read-only t)
  (engine nil :read-only t)
  (condition nil :read-only t))

(declaim (ftype (function (position-result)
                    (values (or number null) &optional))
          position-result-value))
(defun position-result-value (r)
  "Quantity times unit price, or NIL if the position was not priced."
  (a:when-let ((p (position-result-unit-price r)))
    (* (position-quantity (position-result-position r)) p)))

(defstruct (portfolio-result (:constructor make-portfolio-result (portfolio results)))
  "A priced PORTFOLIO. RESULTS is a vector of POSITION-RESULTs in the order of
PORTFOLIO-POSITIONS."
  (portfolio nil :read-only t)
  (results nil :read-only t))

(declaim (ftype (function (portfolio-result)
                    (values double-float (or double-float null) list &optional))
          portfolio-value))
(defun portfolio-value (result)
  "Return (values value standard-error unpriced) of a PORTFOLIO-RESULT. VALUE is the sum of
the values of the priced positions. STANDARD-ERROR is the standard error of that sum
(NIL if no position has one). UNPRICED is the list of positions that could not be
priced."
  (let ((value 0d0) (variance nil) (unpriced '()))
    (iter (for r in-vector (portfolio-result-results result))
          (a:if-let ((v (position-result-value r)))
            (progn
              (incf value v)
              (a:when-let ((se (position-result-standard-error r)))
                (setf variance (+ (or variance 0d0)
                                  (expt (* (position-quantity (position-result-position r)) se) 2)))))
            (push (position-result-position r) unpriced)))
    (values value (and variance (sqrt variance)) (nreverse unpriced))))

(defmethod print-object ((r portfolio-result) stream)
  (print-unreadable-object (r stream :type t)
    (multiple-value-bind (value se unpriced) (portfolio-value r)
      (format stream "~S ~,4F~@[ +- ~,4F~]~@[, ~D unpriced~]" (portfolio-name (portfolio-result-portfolio r))
              value se (and unpriced (length unpriced))))))

(defun default-fallback-policy (position market engine condition)
  "The engines tried after ENGINE declined POSITION with CONDITION: the
APPLICABLE-ENGINES of its instrument, other than ENGINE."
  (declare (ignore condition))
  (let ((instrument (position-instrument position)))
    (and (typep instrument 'option)
         (remove-if (lambda (e) (and (typep engine 'engine) (eq (class-of e) (class-of engine))))
                    (applicable-engines instrument market)))))

(defun price-position (position market default-engine policy)
  "A POSITION-RESULT for POSITION on MARKET: its engine (or DEFAULT-ENGINE),
then, on UNSUPPORTED-COMBINATION or MODEL-DOMAIN-ERROR, the engines POLICY
returns, in order. Other errors are recorded, not signalled."
  (let ((first-condition nil)
        (designator (or (position-engine position) default-engine))
        (instrument (position-instrument position)))
    (flet ((attempt (engine)
             (handler-case
                 (multiple-value-bind (v se resolved) (price instrument engine market)
                   (return-from price-position
                     (make-position-result position (float v 1d0) se (or resolved engine)
                                           first-condition)))
               ((or unsupported-combination model-domain-error) (c)
                 (unless first-condition (setf first-condition c))
                 nil)
               (error (c)
                 (return-from price-position
                   (make-position-result position nil nil engine c))))))
      (attempt designator)
      (dolist (engine (and policy (funcall policy position market designator first-condition)))
        (attempt engine))
      (make-position-result position nil nil nil first-condition))))

(declaim (ftype (function (portfolio t &key (:engine t) (:policy t) (:parallel t))
                    (values portfolio-result &optional))
          price-portfolio))
(defun price-portfolio (portfolio market &key (engine :auto) (policy #'default-fallback-policy)
                                              (parallel t))
  "Price PORTFOLIO on MARKET. Return a PORTFOLIO-RESULT (see PORTFOLIO-VALUE) with one
POSITION-RESULT for each position, in order. Each position is priced with its own
engine, or ENGINE (default :AUTO). If that engine declines (UNSUPPORTED-COMBINATION,
MODEL-DOMAIN-ERROR), POLICY gives the engines to try next. POLICY is a function of
(position market engine condition). The default is the APPLICABLE-ENGINES of the
instrument. The function records a position that it cannot price with its condition. It
signals nothing. It prices positions in parallel under lp:*kernel*. With PARALLEL NIL it
prices them in serial, for example inside code that already runs in parallel. The result
does not depend on the number of workers.

  (portfolio-value (price-portfolio book m)) => 1234.56d0, NIL, NIL"
  (check-input portfolio portfolio "a portfolio")
  (let* ((positions (portfolio-positions portfolio))
         (results (if (and parallel lp:*kernel*)
                      (parallel-map 'vector (lambda (p) (price-position p market engine policy)) positions)
                      (map 'vector (lambda (p) (price-position p market engine policy)) positions))))
    (make-portfolio-result portfolio results)))

(defmethod price ((p portfolio) engine market)
  "A portfolio's value: (values value standard-error), by PRICE-PORTFOLIO with
ENGINE as the default engine. Signals the first position's condition if any
position cannot be priced."
  (let ((result (price-portfolio p market :engine engine)))
    (multiple-value-bind (value se unpriced) (portfolio-value result)
      (when unpriced
        (let ((r (find (first unpriced) (portfolio-result-results result) :key #'position-result-position)))
          (error (or (position-result-condition r)
                     (make-condition 'unsupported-combination
                                     :exercise nil :payoff nil :path nil :engine nil :market market)))))
      (values value se))))

;;; --------------------------------------------------------------------
;;; Greeks of a portfolio
;;; --------------------------------------------------------------------

(defmethod greeks ((p portfolio) engine market &key (which *greek-keys*) bumps (method :default))
  "The sum over PORTFOLIO's positions of quantity x the instrument's Greeks,
each under the position's engine or ENGINE. METHOD :DEFAULT uses each
instrument's own GREEKS method (closed forms where there are some);
:BUMP uses BUMP-GREEKS for every position. Positions whose Greeks fail
signal their condition."
  (let ((positions (portfolio-positions p))
        (totals (iter (for key in which) (appending `(,key 0d0)))))
    (dolist (position positions totals)
      (let* ((designator (or (position-engine position) engine))
             (instrument (position-instrument position))
             (g (if (eq method :bump)
                    (bump-greeks instrument designator market :which which :bumps bumps)
                    (greeks instrument designator market :which which :bumps bumps))))
        (iter (for key in which)
              (incf (getf totals key) (* (position-quantity position) (getf g key))))))))
