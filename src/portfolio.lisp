;;;; portfolio.lisp --- holdings, positions, portfolios (task 22)
;;;;
;;;; A position is an instrument and a signed quantity: long when positive,
;;;; short when negative. A portfolio is a named tree of positions and
;;;; sub-portfolios. Pricing a portfolio prices each position on the same
;;;; market snapshot, in parallel, under a recovery policy, and reports
;;;; per-position results rather than signalling: a book is priced even when
;;;; one of its positions cannot be.
;;;;
;;;; A HOLDING is the underlying itself, so that a book can hold its hedge.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Holding the underlying
;;; --------------------------------------------------------------------

(defclass holding (instrument) ()
  (:metaclass component-class)
  (:documentation "One unit of an underlying, worth its spot. Its currency is
the underlying's; another is a quanto and signals UNSUPPORTED-QUANTO. Borrow
and financing costs are not modelled, and dividends received are not part of
its value.

  (make-holding :underlying :spx)"))

(defun make-holding (&key underlying currency)
  "A HOLDING of UNDERLYING (a keyword; NIL means the market's only
underlying), paying in CURRENCY (NIL: the underlying's).

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
;;; Positions and portfolios
;;; --------------------------------------------------------------------

(defclass portfolio-position ()
  ((instrument :initarg :instrument :reader position-instrument)
   (quantity :initarg :quantity :reader position-quantity :type double-float)
   (engine :initarg :engine :initform nil :reader position-engine
           :documentation "An engine designator, or NIL for the portfolio's default.")
   (tags :initarg :tags :initform '() :reader position-tags :type list
         :documentation "A plist, e.g. (:desk :a :strategy :hedge)."))
  (:documentation "QUANTITY units of INSTRUMENT: long when positive, short
when negative, closed at zero. Made by MAKE-POSITION; immutable."))

(defun make-position (instrument &key (quantity 1) engine tags)
  "A position of QUANTITY (a real; negative is short) in INSTRUMENT, priced
with ENGINE (a designator; NIL: the portfolio's default) and carrying TAGS,
a plist.

  (make-position (make-option (put 95) (expires #D\"2027-09-24\")) :quantity -10
                 :tags '(:desk :a))"
  (check-input instrument instrument "an instrument")
  (check-input quantity real "a real quantity (negative for a short position)")
  (check-input tags list "a plist of tags")
  (unless (evenp (length tags))
    (signal-invalid-argument 'tags tags 'list "a plist: keys and values in pairs"))
  (make-instance 'portfolio-position :instrument instrument :quantity (float quantity 1d0)
                                     :engine engine :tags tags))

(defun derive-position (position &key (quantity (position-quantity position))
                                      (engine (position-engine position))
                                      (tags (position-tags position)))
  "A copy of POSITION with QUANTITY, ENGINE or TAGS replaced."
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
  (:documentation "A named tree of positions and sub-portfolios. Made by
MAKE-PORTFOLIO; immutable."))

(defun make-portfolio (name &rest members)
  "A PORTFOLIO called NAME (a string or keyword) of MEMBERS, each a position
or a portfolio.

  (make-portfolio \"book\"
                  (make-position call :quantity 10 :tags '(:desk :a))
                  (make-portfolio \"hedges\" (make-position (make-holding) :quantity -6)))"
  (dolist (m members)
    (check-input m (or portfolio-position portfolio) "a position or a portfolio"))
  (make-instance 'portfolio :name name :members members))

(defun portfolio-positions (portfolio)
  "Every position in PORTFOLIO's tree, depth first, in order."
  (iter (for m in (portfolio-members portfolio))
        (if (typep m 'portfolio)
            (appending (portfolio-positions m))
            (collect m))))

(defun filter-portfolio (portfolio predicate &optional (name (portfolio-name portfolio)))
  "The portfolio of PORTFOLIO's positions satisfying PREDICATE, flat."
  (apply #'make-portfolio name (remove-if-not predicate (portfolio-positions portfolio))))

(defun portfolio-long (portfolio)
  "The positions of PORTFOLIO with a positive quantity, as a flat portfolio."
  (filter-portfolio portfolio (lambda (p) (plusp (position-quantity p)))
                    (format nil "~A (long)" (portfolio-name portfolio))))

(defun portfolio-short (portfolio)
  "The positions of PORTFOLIO with a negative quantity, as a flat portfolio."
  (filter-portfolio portfolio (lambda (p) (minusp (position-quantity p)))
                    (format nil "~A (short)" (portfolio-name portfolio))))

(defun portfolio-by-tag (portfolio key)
  "((value . portfolio) ...): PORTFOLIO's positions grouped by the value of
their tag KEY (NIL for positions without it), in order of first appearance.

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
  (:documentation "The rank of ENGINE (an instance) in APPLICABLE-ENGINES'
order, lower first: closed forms 10 (Black-Scholes before RGW and BAW),
COS 20, finite differences 30, the tree 40, Monte Carlo 90.")
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

(defvar *default-engines* '(:black-scholes :rgw :baw :cos :fd :binomial :mc :merton-series :cev)
  "The engine designators APPLICABLE-ENGINES considers by default.")

(defgeneric applicable-engines (instrument market &key candidates process)
  (:documentation "The engines that have a pricing method for INSTRUMENT, as
instances, ordered by ENGINE-PREFERENCE: those of CANDIDATES (designators,
default *DEFAULT-ENGINES*) that model PROCESS (default GBM on the market)
and whose pricing generic function has a method for the instrument's
exercise, payoff and path classes more specific than the
UNSUPPORTED-COMBINATION fallback. An engine listed may still decline in a
market regime (MODEL-DOMAIN-ERROR, e.g. discrete dividends it does not
model); SELECT-ENGINE and PRICE-PORTFOLIO move on when it does.

  (applicable-engines (make-option (put 100) (american) (expires d)) m :process (make-heston))
  => (#<ENGINE (make-engine :fd :process (make-heston))> #<ENGINE (make-engine :mc ...)>)")
  (:method ((o option) market &key (candidates *default-engines*) (process (make-gbm)))
    (let ((market (pricing-market market o)))
      (-> (iter (for key in candidates)
                (for class = (designated-class key))
                (when (class-models-process-p class process)
                  (let ((engine (handler-case (ensure-engine key process)
                                  (invalid-argument () nil))))
                    (when (and engine (engine-has-method-p engine o market))
                      (collect engine)))))
          (stable-sort #'< :key #'engine-preference))))
  (:method ((h holding) market &key candidates process)
    (declare (ignore market candidates process))
    ;; Any engine prices a holding; the Black-Scholes one stands for all.
    `(,(ensure-engine :black-scholes))))

(defun select-engine (instrument market &rest options &key candidates process)
  "The first of APPLICABLE-ENGINES for INSTRUMENT, or NIL."
  (declare (ignore candidates process))
  (first (apply #'applicable-engines instrument market options)))

;;; --------------------------------------------------------------------
;;; Pricing a portfolio
;;; --------------------------------------------------------------------

(defstruct (position-result (:constructor make-position-result
                                (position unit-price standard-error engine condition)))
  "One position's pricing: UNIT-PRICE and STANDARD-ERROR per unit of the
instrument (NIL if it could not be priced), the ENGINE that priced it, and
the CONDITION recovered from or, if unpriced, the last one."
  (position nil :read-only t)
  (unit-price nil :read-only t)
  (standard-error nil :read-only t)
  (engine nil :read-only t)
  (condition nil :read-only t))

(defun position-result-value (r)
  "Quantity x unit price, or NIL if the position was not priced."
  (a:when-let ((p (position-result-unit-price r)))
    (* (position-quantity (position-result-position r)) p)))

(defstruct (portfolio-result (:constructor make-portfolio-result (portfolio results)))
  "PORTFOLIO priced: RESULTS, a vector of POSITION-RESULTs in the order of
PORTFOLIO-POSITIONS."
  (portfolio nil :read-only t)
  (results nil :read-only t))

(defun portfolio-value (result)
  "(values value standard-error unpriced) of a PORTFOLIO-RESULT: the sum of
the priced positions' values, the standard error of that sum (NIL if no
position has one), and the list of positions that could not be priced."
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

(defun price-portfolio (portfolio market &key (engine :auto) (policy #'default-fallback-policy)
                                              (parallel t))
  "PORTFOLIO priced on MARKET: a PORTFOLIO-RESULT (see PORTFOLIO-VALUE) with
one POSITION-RESULT per position, in order. Each position is priced with its
own engine, or ENGINE (default :AUTO); if that declines
(UNSUPPORTED-COMBINATION, MODEL-DOMAIN-ERROR), POLICY, a function of
(position market engine condition), gives the engines to try next (default:
the instrument's APPLICABLE-ENGINES). A position that cannot be priced is
recorded with its condition; nothing is signalled. Positions are priced in
parallel under lp:*kernel* (serially with PARALLEL NIL, e.g. inside code
that already runs in parallel); the result does not depend on the number of
workers.

  (portfolio-value (price-portfolio book m)) => 1234.56d0, NIL, NIL"
  (check-input portfolio portfolio "a portfolio")
  (let* ((positions (portfolio-positions portfolio))
         (results (if (and parallel lp:*kernel*)
                      (lp:pmap 'vector (lambda (p) (price-position p market engine policy)) positions)
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
