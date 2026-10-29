;;;; snapshot.lisp --- markets and portfolios as readable forms (task 22)
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
counters, options, holdings, positions and portfolios; anything else
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

(defmethod snapshot-form ((p portfolio-position))
  `(make-position ,(snapshot-form (position-instrument p)) :quantity ,(position-quantity p)
                  ,@(a:when-let ((e (position-engine p)))
                      (if (typep e 'engine)
                          `(:engine ,(or (engine-form e) (call-next-method)))
                          `(:engine (quote ,e))))
                  ,@(a:when-let ((tags (position-tags p))) `(:tags (quote ,tags)))))

(defmethod snapshot-form ((p portfolio))
  `(make-portfolio ,(portfolio-name p) ,@(mapcar #'snapshot-form (portfolio-members p))))

(defun structure-or-number (structure flat-class reader)
  "STRUCTURE's snapshot form, or its number if it is the flat FLAT-CLASS (the
form MAKE-MARKET's list DSL takes)."
  (if (eq (class-name (class-of structure)) flat-class)
      (funcall reader structure)
      (snapshot-form structure)))

(defmethod snapshot-form ((m composite-market))
  (flet ((entry (table key) (gethash key table)))
    `(make-market
      :valuation-date ,(valuation-date m)
      :day-counter ,(snapshot-form (market-day-counter m))
      :underlyings
      (list ,@(iter (for u in (market-underlyings m))
                    (collect `(list ,u :spot ,(underlying-spot m u)
                                    :currency ,(entry (market-underlying-currency m) u)
                                    :dividend ,(structure-or-number (entry (market-dividend-curves m) u)
                                                                    'flat-dividend-curve #'flat-rate)
                                    ,@(a:when-let ((s (entry (market-dividend-schedules m) u)))
                                        `(:dividend-schedule ,(snapshot-form s)))
                                    ,@(a:when-let ((v (entry (market-vol-structures m) u)))
                                        `(:vol ,(structure-or-number v 'flat-black-vol #'flat-vol)))))))
      :curves
      (list ,@(iter (for id in (sorted-keys (market-yield-curves m)))
                    (for c = (entry (market-yield-curves m) id))
                    (collect (if (eq (class-name (class-of c)) 'flat-yield-curve)
                                 `(list ,id :rate ,(flat-rate c))
                                 `(list ,id :curve ,(snapshot-form c))))))
      :discounting
      (quote ,(iter (for currency in (sorted-keys (market-discount-curve-for m)))
                    (collect (cons currency (entry (market-discount-curve-for m) currency)))))
      :fixings
      (quote ,(iter (for index in (sorted-keys (market-fixings m)))
                    (for dates = (entry (market-fixings m) index))
                    (collect `(,index ,@(iter (for d in (sort (a:hash-table-keys dates) #'date<))
                                              (collect `(,d ,(gethash d dates)))))))))))

;;; Printing readably is printing the snapshot form.
(defmethod print-object :around ((m composite-market) stream)
  (if *print-readably*
      (prin1 (snapshot-form m) stream)
      (call-next-method)))

;;; --------------------------------------------------------------------
;;; Writing and reading
;;; --------------------------------------------------------------------

(defun write-snapshot (object &optional (stream *standard-output*))
  "Print OBJECT's SNAPSHOT-FORM to STREAM under standard I/O syntax, with
FINCL:SYNTAX dates, symbols qualified as needed. READ-SNAPSHOT reads it
back.

  (with-open-file (s \"book.sexp\" :direction :output) (write-snapshot book s))"
  (let ((form (snapshot-form object)))
    (with-standard-io-syntax
      (let ((*package* (find-package '#:fincl))
            (*readtable* (named-readtables:find-readtable 'syntax))
            (*print-readably* t))
        (prin1 form stream)
        (terpri stream)
        object))))

(defparameter *snapshot-constructors*
  '(make-market make-dividend-schedule make-instance make-interpolated-smile make-svi-slice
    make-smile-surface make-ssvi-surface make-option-from-clauses make-holding make-position
    make-portfolio make-engine make-gbm make-heston make-cev make-local-vol make-merton-jump
    make-kou-jump make-variance-gamma make-nig make-bates make-garch)
  "The functions a snapshot may call.")

(defparameter *snapshot-classes*
  '(interpolated-discount-curve interpolated-zero-curve interpolated-forward-curve
    actual-360 actual-365-fixed actual-actual-isda smile-surface grid-vol-surface svi-surface)
  "The classes MAKE-INSTANCE may make in a snapshot.")

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
               (t (unless (member head *snapshot-constructors*) (refuse head))
                (let ((args (mapcar #'snapshot-eval args)))
                  (when (and (eq head 'make-instance) (not (member (first args) *snapshot-classes*)))
                    (refuse `(make-instance ',(first args))))
                  (apply head args))))))
          ((and (symbolp form) (member form *snapshot-day-counters*)) (symbol-value form))
          ((and (symbolp form) (not (keywordp form)) (not (member form '(t nil)))) (refuse form))
          (t form))))

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
