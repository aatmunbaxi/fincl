;;;; instruments/instrument.lisp --- shared across asset classes
;;;;
;;;; Everything here is meant to be reused by bonds, swaps and anything else
;;;; added later. Option-specific components live in options.lisp.

(in-package #:fincl)

(defclass instrument ()
  ()
  (:documentation "A contract. Carries terms only: no market data, no model.
Instruments are immutable and reusable across scenarios."))

;;; --------------------------------------------------------------------
;;; Exercise (shared: callable bonds and Bermudan swaptions need this too)
;;; --------------------------------------------------------------------

(defclass exercise ()
  ((expiry :initarg :expiry :reader expiry :type date
           :initform (a:required-argument :expiry)
           :documentation "Last date on which the option can be exercised.")))

(defun check-expiry (expiry)
  (typecase expiry
    (date expiry)
    (tenor
     (error "Expiry ~S is a tenor and needs a start date to resolve it: pass ~
:FROM, e.g. :from #D\"2026-09-24\" or :from (valuation-date market)."
            expiry))
    (t
     (error "Expiry ~S is not a date. Expiries are dates, e.g. #D\"2027-09-24\" ~
or (make-date 2027 9 24), or tenors such as #T\"1Y\" with :FROM; the market ~
converts dates to year fractions with its day counter."
            expiry))))

(defun resolve-expiry (expiry &key from calendar (convention :following)
                                   end-of-month)
  "Return EXPIRY as a date. A date is returned as is. A tenor is added to the
date FROM (with END-OF-MONTH as in ADD-TENOR) and, if CALENDAR is given,
moved onto a business day with CONVENTION (see ADJUST).

  (resolve-expiry #T\"1Y\" :from #D\"2026-09-26\"
                  :calendar (make-instance 'weekends-only))
  => #D\"2027-09-27\""
  (if (and (typep expiry 'tenor) from)
      (let ((date (add-tenor (check-expiry from) expiry :end-of-month end-of-month)))
        (if calendar (adjust calendar date convention) date))
      (check-expiry expiry)))

(defmethod initialize-instance :after ((x exercise) &key)
  (check-expiry (expiry x)))

(defclass european-exercise (exercise) ()
  (:documentation "Exercise on the expiry date only."))

(defclass american-exercise (exercise) ()
  (:documentation "Exercise on any date up to and including the expiry."))

(defclass bermudan-exercise (exercise)
  ((expiry :initform nil)
   (dates :initarg :dates :reader exercise-dates
          :initform (a:required-argument :dates)))
  (:documentation "Exercise on any of DATES. EXPIRY is the last of them.

  (make-instance 'bermudan-exercise
                 :dates (list #D\"2027-03-24\" #D\"2027-09-24\"))"))

(defmethod initialize-instance :around ((x bermudan-exercise) &rest initargs
                                        &key dates &allow-other-keys)
  (unless (and dates (listp dates))
    (error "A Bermudan exercise needs a non-empty list of dates, not ~S." dates))
  (mapc #'check-expiry dates)
  (let ((sorted (sort (copy-list dates) #'date<)))
    (apply #'call-next-method x :dates sorted :expiry (a:lastcar sorted) initargs)))

(defgeneric exercise-allowed-p (exercise date)
  (:documentation "Can the holder exercise on DATE? One generic function is
the entire difference between European, American and Bermudan in a lattice
or LSM engine.")
  (:method ((x european-exercise) date) (date= date (expiry x)))
  (:method ((x american-exercise) date) (date<= date (expiry x)))
  (:method ((x bermudan-exercise) date)
    (and (member date (exercise-dates x) :test #'date=) t)))

(defgeneric exercise-mask (exercise market times)
  (:documentation "A SIMPLE-BIT-VECTOR as long as TIMES, with 1 at the grid
indices where EXERCISE allows exercise. TIMES are the grid times in MARKET
time: evenly spaced, from 0 at the valuation date to the expiry. Lattice and
LSM engines read this instead of dispatching on the exercise class
themselves. Bermudan dates on or before the valuation date are dropped,
except that index 0 is set when exercise is allowed today; the others snap to
the nearest grid index, clamped to the last one.

  (exercise-mask (make-instance 'american-exercise :expiry d) m times) ; all ones")
  (:method ((x european-exercise) market times)
    (declare (ignore market))
    (let ((mask (make-array (length times) :element-type 'bit :initial-element 0)))
      (setf (sbit mask (1- (length times))) 1)
      mask))
  (:method ((x american-exercise) market times)
    (declare (ignore market))
    (make-array (length times) :element-type 'bit :initial-element 1))
  (:method ((x bermudan-exercise) market times)
    (let* ((n (1- (length times)))
           (tau (aref times n))
           (today (valuation-date market))
           (mask (make-array (1+ n) :element-type 'bit :initial-element 0)))
      (dolist (date (exercise-dates x))
        (when (date< today date)
          ;; Nearest index of an evenly spaced grid.
          (setf (sbit mask (min n (round (* n (market-time market date)) tau))) 1)))
      (when (exercise-allowed-p x today)
        (setf (sbit mask 0) 1))
      mask)))

(defun uniform-time-grid (tau n-steps)
  "N-STEPS + 1 evenly spaced times from 0 to TAU, as a (SIMPLE-ARRAY
DOUBLE-FLOAT (*)). The last element is TAU exactly.

  (uniform-time-grid 1d0 4) => #(0d0 0.25d0 0.5d0 0.75d0 1d0)"
  (let ((times (make-array (1+ n-steps) :element-type 'double-float))
        (dt (/ tau n-steps)))
    (dotimes (j n-steps)
      (setf (aref times j) (* j dt)))
    (setf (aref times n-steps) (float tau 1d0))
    times))

(defun time-to-expiry (exercise market)
  "Year fraction from MARKET's valuation date to EXERCISE's expiry under the
market's day counter."
  (market-time market (expiry exercise)))

;;; --------------------------------------------------------------------
;;; Path dependence
;;; --------------------------------------------------------------------

(defclass path-feature () ())
(defclass path-independent (path-feature) ())
;; Barrier, Asian and lookback subclasses go here; engines dispatch on them.

;;; --------------------------------------------------------------------
;;; Payoff protocol
;;; --------------------------------------------------------------------

(defclass payoff () ()
  (:documentation "What a contract pays as a function of the observable.
Carries no slots: call/put and the option's K belong to the option payoffs
in options.lisp, since a coupon or a swap leg has neither."))

(defgeneric payoff-value (payoff x)
  (:documentation "Dispatching payoff. Convenient; too slow to call per path."))

(defgeneric payoff-kernel (payoff)
  (:documentation "Dispatch once, return a typed (double-float ->
double-float) closure. Engines call this before entering their loops. The
default wraps PAYOFF-VALUE, so any payoff with a PAYOFF-VALUE method works in
a closure-driven engine; it dispatches on every call, so DEFINE-PAYOFF's
typed kernel is much faster.")
  (:method ((p payoff))
    (lambda (s) (float (payoff-value p s) 1d0))))

;;; DEFINE-PAYOFF writes a payoff's formula once and derives both
;;; PAYOFF-VALUE and a typed PAYOFF-KERNEL from it. The variables a formula
;;; may use (its own slots, and whatever its superclasses provide, such as
;;; PHI for calls and puts) are recorded per class at compile time, so a
;;; subclass's formula can use its parents' variables.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *payoff-variables* (make-hash-table :test 'eq)
    "Class name -> list of (variable reader type): what a DEFINE-PAYOFF
formula for the class or a subclass may refer to.")

  (defvar *formula-payoffs* (make-hash-table :test 'eq)
    "Names of payoff classes defined by DEFINE-PAYOFF, i.e. with a formula.")

  (define-condition formula-payoff-superclass (style-warning simple-condition) ()
    (:documentation "DEFINE-PAYOFF was given a payoff with its own formula as
a superclass. Engines with closed forms for that superclass would apply them
to the subclass and return the superclass's price."))

  (defun inherited-payoff-variables (superclasses)
    "The variables SUPERCLASSES declare, renamed into *PACKAGE*. Variables
are matched by name, as LOOP keywords are, so a formula written in any
package can say PHI without FINCL exporting it."
    (remove-duplicates (loop for class in superclasses
                             append (loop for (variable . rest) in (gethash class *payoff-variables*)
                                          collect (cons (intern (symbol-name variable))
                                                        rest)))
                       :key #'first :from-end t)))

(defmacro declare-payoff-variables (class &rest variables)
  "Record VARIABLES, each (variable reader type), as usable in DEFINE-PAYOFF
formulas for subclasses of CLASS, for a payoff class written by hand.

  (declare-payoff-variables call-put-payoff (phi phi double-float))"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',class *payoff-variables*) ',variables)))

(defmacro define-payoff (name superclasses slots (observable) &body body)
  "Define the payoff class NAME and derive PAYOFF-VALUE and PAYOFF-KERNEL
from one formula. Each of SLOTS is (slot-name type &key initform reader doc);
without INITFORM the initarg is required. BODY, optionally preceded by a
docstring, computes the payoff from OBSERVABLE (a double-float) and may use
every slot of NAME, and the variables its superclasses declare, as typed
local variables. PAYOFF-KERNEL reads them once and closes over them, so the
closure makes no generic call per evaluation.

Inherit from data classes such as CALL-PUT-PAYOFF or STRIKE-PAYOFF, not from
a payoff with its own formula: engines with a closed form for the parent
would price the subclass with it. Doing so signals a style warning.

  (define-payoff digital (call-put-payoff)
      ((level double-float :doc \"Pays 1 beyond this level.\"))
    (s)
    (if (plusp (* phi (- s level))) 1d0 0d0))"
  (dolist (super superclasses)
    (when (gethash super *formula-payoffs*)
      (warn 'formula-payoff-superclass
            :format-control "~S inherits from ~S, which has its own formula. Engines ~
with a closed form for ~S would price ~S with it. Inherit from a data class ~
such as STRIKE-PAYOFF instead."
            :format-arguments (list name super super name))))
  (let* ((documentation (when (and (stringp (first body)) (rest body)) (first body)))
         (body (if documentation (rest body) body))
         (own (loop for (slot type . options) in slots
                    collect (list slot (getf options :reader slot) type)))
         (variables (append (inherited-payoff-variables superclasses) own))
         (payoff (gensym "PAYOFF")))
    (flet ((bindings ()
             (loop for (variable reader) in variables
                   collect `(,variable (,reader ,payoff))))
           (declarations ()
             ;; A formula need not use every inherited variable.
             (cons `(ignorable ,@(mapcar #'first variables))
                   (loop for (variable nil type) in variables
                         collect `(type ,type ,variable)))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *payoff-variables*) ',variables
                 (gethash ',name *formula-payoffs*) t))
         (defclass ,name ,superclasses
           ,(loop for (slot type . options) in slots
                  collect `(,slot :initarg ,(a:make-keyword slot)
                                  :reader ,(getf options :reader slot)
                                  :type ,type
                                  :initform ,(if (member :initform options)
                                                 (getf options :initform)
                                                 `(a:required-argument ,(a:make-keyword slot)))
                                  ,@(when (getf options :doc)
                                      `(:documentation ,(getf options :doc)))))
           ,@(when documentation `((:documentation ,documentation))))
         ,@(let ((floats (loop for (slot type) in slots
                               when (eq type 'double-float) collect slot)))
             ;; Accept any real for a double-float slot, as MAKE-BS-MARKET does.
             (when floats
               `((defmethod initialize-instance :after ((,payoff ,name) &key)
                   ,@(loop for slot in floats
                           collect `(let ((value (slot-value ,payoff ',slot)))
                                      (check-type value real)
                                      (setf (slot-value ,payoff ',slot)
                                            (float value 1d0))))))))
         (defmethod payoff-value ((,payoff ,name) ,observable)
           (let ((,observable (float ,observable 1d0)) ,@(bindings))
             (declare (double-float ,observable) ,@(declarations))
             ,@body))
         (defmethod payoff-kernel ((,payoff ,name))
           (let (,@(bindings))
             (declare ,@(declarations))
             (lambda (,observable)
               (declare (double-float ,observable) (optimize (speed 3) (safety 0)))
               ,@body)))
         ',name))))

(defgeneric validate-payoff (payoff market expiry)
  (:documentation "Signal an error if MARKET cannot support pricing PAYOFF to
EXPIRY, a date. Called once per PRICE, before the engine runs; the default
accepts everything.")
  (:method ((p payoff) market expiry)
    (declare (ignore market expiry))
    nil))

(defgeneric describe-payoff (payoff stream)
  (:documentation "Write a short description of PAYOFF to STREAM, for the
printed form of the instrument that holds it; the default writes the class
name.

  (describe-payoff put-payoff *standard-output*)  ; writes \"put K=40.00\"")
  (:method ((p payoff) stream)
    (format stream "~(~A~)" (class-name (class-of p)))))
