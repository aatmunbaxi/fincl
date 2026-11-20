;;;; snapshot.lisp --- markets and portfolios as readable forms
;;;;
;;;; A snapshot is the constructor form that rebuilds an object: MAKE-MARKET
;;;; with its curves, surfaces and dividends, MAKE-PORTFOLIO with its
;;;; positions. WRITE-SNAPSHOT prints it under standard syntax and
;;;; FINCL:SYNTAX (dates as #D); READ-SNAPSHOT reads it back without
;;;; *READ-EVAL* and evaluates it with a small evaluator that only calls the
;;;; constructors below, so reading a snapshot runs no other code. Rebuilt
;;;; structures are made from the same numbers, so prices are = to the
;;;; originals.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defgeneric snapshot-form (object)
  (:documentation "A form that rebuilds OBJECT, made only of literals (numbers,
strings, keywords, dates), QUOTE, LIST and the constructors READ-SNAPSHOT
allows. Defined for composite markets, the flat, interpolated and
dividend-schedule term structures, smile, grid, SVI and SSVI surfaces, day
counters, options, holdings, positions, portfolios and physical processes
whose premia are data; anything else
(bootstrapped curves among them) signals INVALID-ARGUMENT.

  (snapshot-form (make-holding :underlying :spx)) => (MAKE-HOLDING :UNDERLYING :SPX)")
  (:method (object)
    (signal-invalid-argument 'object object 'snapshot-form
                             "an object with a snapshot form: a composite market, its flat, interpolated, ~
dividend-schedule and smile/SSVI structures, an option, a holding, a position or a portfolio ~
(bootstrapped curves are not supported yet)")))

(defmethod snapshot-form ((x real)) x)
(defmethod snapshot-form ((x string)) x)
(defmethod snapshot-form ((x symbol)) (if (keywordp x) x `(quote ,x)))
(defmethod snapshot-form ((x date)) x)

;;; Day counters: the shared instances by name, so a rebuilt curve shares
;;; the market's (CURVE-IN-MARKET compares them).
(defparameter *snapshot-day-counters* '(+actual-360+ +actual-365-fixed+ +actual-actual-isda+))

(defmethod snapshot-form ((dc day-counter))
  (or (find dc *snapshot-day-counters* :key #'symbol-value)
      (if (null (c2mop:class-slots (class-of dc)))
          `(make-instance ',(class-name (class-of dc)))
          (call-next-method))))

(defmethod snapshot-form ((s dividend-schedule))
  `(make-dividend-schedule (quote ,(schedule-entries s))))

(defun interpolated-curve-form (c values-initarg)
  `(make-instance ',(class-name (class-of c))
                  :dates (quote ,(coerce (curve-dates c) 'list))
                  ,values-initarg (quote ,(coerce (curve-values c) 'list))
                  :day-counter ,(snapshot-form (curve-day-counter c))
                  :interpolation ,(curve-interpolation c)))

(defmethod snapshot-form ((c interpolated-discount-curve)) (interpolated-curve-form c :discounts))
(defmethod snapshot-form ((c interpolated-zero-curve)) (interpolated-curve-form c :zero-rates))
(defmethod snapshot-form ((c interpolated-forward-curve)) (interpolated-curve-form c :forwards))

(defmethod snapshot-form ((c state-curve))
  (signal-invalid-argument 'curve c 'interpolated-discount-curve
                           "a curve with a snapshot form: a simulated curve is a model's bond price function; ~
give its leg :KNOTS (tenors) to simulate an interpolated curve instead"))

(defmethod snapshot-form ((s interpolated-smile))
  (let ((i (smile-interpolator s)))
    `(make-interpolated-smile ,(smile-expiry s)
                              (quote ,(coerce (interpolator-knots i) 'list))
                              (quote ,(coerce (interpolator-values i) 'list))
                              :interpolation ,(interpolator-method i))))

(defmethod snapshot-form ((s svi-slice))
  `(make-svi-slice ,(smile-expiry s) :a ,(svi-a s) :b ,(svi-b s) :rho ,(svi-rho s)
                   :m ,(svi-m s) :sigma ,(svi-sigma s)))

(defmethod snapshot-form ((s smile-surface))
  `(make-smile-surface (list ,@(map 'list #'snapshot-form (surface-smiles s)))
                       :class (quote ,(class-name (class-of s)))))

(defmethod snapshot-form ((s ssvi-surface))
  `(make-ssvi-surface (quote ,(coerce (ssvi-times s) 'list)) (quote ,(coerce (ssvi-thetas s) 'list))
                      :rho ,(ssvi-rho s) :eta ,(ssvi-eta s) :gamma ,(ssvi-gamma s)))

(defmethod snapshot-form ((o option))
  `(make-option-from-clauses (quote ,(object-clauses 'option o))))

(defmethod snapshot-form ((h holding))
  `(make-holding ,@(a:when-let ((u (instrument-underlying h))) `(:underlying ,u))
                 ,@(a:when-let ((c (instrument-currency h))) `(:currency ,c))))

(defmethod snapshot-form ((c cash-account))
  `(make-cash-account ,@(a:when-let ((cur (instrument-currency c))) `(:currency ,cur))))

(defmethod snapshot-form ((p portfolio-position))
  `(make-position ,(snapshot-form (position-instrument p)) :quantity ,(position-quantity p)
                  ,@(a:when-let ((e (position-engine p)))
                      (if (typep e 'engine)
                          `(:engine ,(or (engine-form e) (call-next-method)))
                          `(:engine (quote ,e))))
                  ,@(a:when-let ((tags (position-tags p))) `(:tags (quote ,tags)))))

(defmethod snapshot-form ((p portfolio))
  `(make-portfolio ,(portfolio-name p) ,@(mapcar #'snapshot-form (portfolio-members p))))

(defmethod snapshot-form ((p physical-process))
  (a:when-let ((state (function-premium-state p)))
    (signal-invalid-argument 'process p 'physical-process
                             (format nil "a physical process whose premia are data: the premium on ~S ~
is a function, which a snapshot cannot hold" state)))
  (destructuring-bind (head q-form &rest options) (physical-form p)
    `(,head (,(first q-form) ,@(mapcar #'snapshot-form (rest q-form))) ,@options)))

(defun clause-snapshot-form (language clause)
  "CLAUSE (data of LANGUAGE) as a snapshot form that evaluates to it: LIST
calls for the clause and its sub-clauses, QUOTE for lists, SNAPSHOT-FORM for
objects."
  `(list ,(first clause)
         ,@(mapcar (lambda (arg)
                     (cond ((clause-like-p language arg) (clause-snapshot-form language arg))
                           ((typep arg '(or number symbol string date)) arg)
                           ((consp arg) `(quote ,arg))
                           (t (snapshot-form arg))))
                   (rest clause))))

(defmethod snapshot-form ((s flat-rates-vol))
  `(make-instance 'flat-rates-vol :vol ,(rates-vol s)
                                  :day-counter ,(snapshot-form (rates-vol-day-counter s))))

(defmethod snapshot-form ((s expiry-rates-vol))
  `(make-instance 'expiry-rates-vol :dates (quote ,(rates-vol-dates s)) :vols (quote ,(rates-vol-vols s))
                                    :day-counter ,(snapshot-form (rates-vol-day-counter s))))

(defun kind-snapshot-form (language clause kinds)
  "CLAUSE's snapshot form, for a market clause giving entries of KINDS:
signals INVALID-ARGUMENT naming the kind when a value has no snapshot form
or calls a function or class the kind did not register."
  (flet ((refuse (what)
           (signal-invalid-argument 'market clause 'list
                                    (format nil "a market whose entries have snapshot forms: an entry of ~
~{~S~^ or ~} ~A; give its kind a SNAPSHOT-FORM method and list what the form calls in ~
:SNAPSHOT-CONSTRUCTORS or :SNAPSHOT-CLASSES"
                                            (mapcar #'market-component-kind-name kinds) what))))
    (let ((form (handler-case (clause-snapshot-form language clause)
                  (invalid-argument () (refuse "has no snapshot form")))))
      (a:when-let ((head (snapshot-form-problem form)))
        (refuse (format nil "calls ~S, which a snapshot may not call" head)))
      form)))

(defmethod snapshot-form ((m composite-market))
  `(make-market-from-clauses
    (list ,@(mapcar (lambda (clause) (clause-snapshot-form 'market clause))
                    (market-date-clauses m))
          ,@(mapcar (lambda (entry) (kind-snapshot-form 'market (first entry) (rest entry)))
                    (market-clauses-of-kinds m)))))

;;; Printing readably is printing the snapshot form.
(defmethod print-object :around ((m composite-market) stream)
  (if *print-readably*
      (prin1 (snapshot-form m) stream)
      (call-next-method)))

;;; --------------------------------------------------------------------
;;; Writing and reading
;;; --------------------------------------------------------------------

(declaim (ftype (function (t &optional t) (values t &optional)) write-snapshot))
(defun write-snapshot (object &optional (stream *standard-output*))
  "Print OBJECT's SNAPSHOT-FORM to STREAM under standard I/O syntax, with
FINCL:SYNTAX dates, symbols qualified as needed. READ-SNAPSHOT reads it
back.

  (with-open-file (s \"book.sexp\" :direction :output) (write-snapshot book s))"
  (let ((form (snapshot-form object)))
    (a:when-let ((head (snapshot-form-problem form)))
      (signal-invalid-argument 'object object 'snapshot-form
                               (format nil "an object whose snapshot form calls only the snapshot ~
constructors; its form calls ~S" head)))
    (with-standard-io-syntax
      (let ((*package* (find-package '#:fincl))
            (*readtable* (named-readtables:find-readtable 'syntax))
            (*print-readably* t))
        (prin1 form stream)
        (terpri stream)
        object))))

(defparameter *snapshot-constructors*
  '(make-market make-market-from-clauses make-market-from-specs make-dividend-schedule make-instance make-interpolated-smile make-svi-slice
    make-smile-surface make-ssvi-surface make-option-from-clauses make-holding make-position
    make-portfolio make-cash-account make-engine make-gbm make-heston make-cev make-local-vol make-merton-jump
    make-kou-jump make-variance-gamma make-nig make-bates make-garch change-measure)
  "The functions a snapshot may call, besides those market component kinds register.")

(defparameter *snapshot-classes*
  '(interpolated-discount-curve interpolated-zero-curve interpolated-forward-curve
    actual-360 actual-365-fixed actual-actual-isda smile-surface grid-vol-surface svi-surface)
  "The classes MAKE-INSTANCE may make in a snapshot, besides those market
component kinds register.")

(defun snapshot-constructor-p (name)
  "True if a snapshot may call the function NAME."
  (or (member name *snapshot-constructors*)
      (some (lambda (kind) (member name (market-component-kind-snapshot-constructors kind)))
            (market-component-kinds))))

(defun snapshot-class-p (name)
  "True if a snapshot may make an instance of the class NAME."
  (or (member name *snapshot-classes*)
      (some (lambda (kind) (member name (market-component-kind-snapshot-classes kind)))
            (market-component-kinds))))

(defun snapshot-form-problem (form)
  "The first function or (make-instance 'class) FORM calls that SNAPSHOT-EVAL
refuses, or NIL."
  (cond ((consp form)
         (destructuring-bind (head &rest args) form
           (case head
             (quote nil)
             (list (some #'snapshot-form-problem args))
             (t (cond ((not (snapshot-constructor-p head)) head)
                      ((and (eq head 'make-instance)
                            (not (and (typep (first args) '(cons (eql quote) (cons symbol null)))
                                      (snapshot-class-p (second (first args))))))
                       `(make-instance ,(first args)))
                      (t (some #'snapshot-form-problem args)))))))
        ((and (symbolp form) (not (keywordp form)) (not (member form '(t nil)))
              (not (member form *snapshot-day-counters*)))
         form)))

(defun snapshot-eval (form)
  "FORM evaluated as a snapshot: literals as themselves, QUOTE, LIST, the
day-counter constants, and calls of *SNAPSHOT-CONSTRUCTORS* (MAKE-INSTANCE
only of *SNAPSHOT-CLASSES*) on evaluated arguments. Anything else signals
INVALID-ARGUMENT."
  (flet ((refuse (what)
           (signal-invalid-argument 'snapshot form 'list
                                    (format nil "a snapshot form; ~A is not allowed in one" what))))
    (cond ((consp form)
           (destructuring-bind (head &rest args) form
             (case head
               (quote (first args))
               (list (mapcar #'snapshot-eval args))
               (t (unless (snapshot-constructor-p head) (refuse head))
                (let ((args (mapcar #'snapshot-eval args)))
                  (when (and (eq head 'make-instance) (not (snapshot-class-p (first args))))
                    (refuse `(make-instance ',(first args))))
                  ;; Snapshots written before markets had clauses call MAKE-MARKET with keywords.
                  (apply (if (eq head 'make-market) 'make-market-from-specs head) args))))))
          ((and (symbolp form) (member form *snapshot-day-counters*)) (symbol-value form))
          ((and (symbolp form) (not (keywordp form)) (not (member form '(t nil)))) (refuse form))
          (t form))))

(declaim (ftype (function (&optional t) (values t &optional)) read-snapshot))
(defun read-snapshot (&optional (stream *standard-input*))
  "The object WRITE-SNAPSHOT wrote to STREAM: read under standard syntax with
FINCL:SYNTAX and *READ-EVAL* NIL, then evaluated by SNAPSHOT-EVAL, which
calls only the snapshot constructors.

  (with-open-file (s \"book.sexp\") (read-snapshot s))"
  (snapshot-eval
   (with-standard-io-syntax
     (let ((*package* (find-package '#:fincl))
           (*readtable* (named-readtables:find-readtable 'syntax))
           (*read-eval* nil)
           (*read-default-float-format* 'double-float))
       (read stream)))))
