;;;; instruments/options.lisp --- options as component bundles
;;;;
;;;; An option is payoff x exercise x path feature. Anything that only
;;;; changes a number inside a method (call vs. put) is a slot; anything that
;;;; changes which method applies (exercise style, path dependence) is a
;;;; class.

(in-package #:fincl)

(defclass call-put-payoff (payoff)
  ((kind :initarg :kind :reader kind :type (member :call :put)
         :initform (a:required-argument :kind)))
  (:documentation "A payoff that comes as a call or a put."))

(declaim (inline phi))
(defun phi (payoff)
  "+1 for a call, -1 for a put."
  (if (eq (kind payoff) :call) 1d0 -1d0))

;;; DEFINE-PAYOFF formulas for call/put payoffs may use PHI.
(declare-payoff-variables call-put-payoff (phi phi double-float))

(defclass strike-payoff (call-put-payoff)
  ((strike :initarg :strike :reader strike :type double-float
           :initform (a:required-argument :strike)))
  (:documentation "A call or put with a strike: the data a struck payoff
needs, with no formula. Build new struck payoffs on this, not on
VANILLA-PAYOFF: engines with closed forms for VANILLA-PAYOFF would apply them
to a subclass and return the vanilla price.

  (define-payoff digital (strike-payoff) () (s)
    (if (plusp (* phi (- s strike))) 1d0 0d0))"))

(defmethod initialize-instance :after ((p strike-payoff) &key)
  (check-type (slot-value p 'strike) real)
  (setf (slot-value p 'strike) (float (slot-value p 'strike) 1d0)))

;;; DEFINE-PAYOFF formulas for struck payoffs may use PHI and STRIKE.
(declare-payoff-variables strike-payoff (phi phi double-float) (strike strike double-float))

(defmethod validate-payoff ((p strike-payoff) market expiry)
  (assert (plusp (black-vol market (strike p) expiry))
          () "Volatility must be positive."))

(defmethod describe-payoff ((p strike-payoff) stream)
  (format stream "~(~A~) K=~,2F" (kind p) (strike p)))

(define-payoff vanilla-payoff (strike-payoff) ()
  (s)
  "max(phi (S - K), 0). Engines with closed forms specialize on this class.

  (make-instance 'vanilla-payoff :kind :put :strike 40d0)"
  (max 0d0 (* phi (- s strike))))


;;; --------------------------------------------------------------------
;;; The option
;;; --------------------------------------------------------------------

(defclass option (instrument)
  ((payoff :initarg :payoff :reader option-payoff :type payoff
           :initform (a:required-argument :payoff))
   (exercise :initarg :exercise :reader option-exercise :type exercise
             :initform (a:required-argument :exercise))
   (path :initarg :path :reader option-path :type path-feature
         :initform (make-instance 'path-independent))))

(defmethod initialize-instance :after ((o option) &key)
  ;; SBCL does not check slot :TYPEs in MAKE-INSTANCE at default safety.
  ;; Options are built rarely, so check them once here.
  (check-type (slot-value o 'payoff) payoff)
  (check-type (slot-value o 'exercise) exercise)
  (check-type (slot-value o 'path) path-feature))

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (let ((e (option-exercise o)))
      (format stream "~(~A~) " (class-name (class-of e)))
      (describe-payoff (option-payoff o) stream)
      (format stream " ~A" (expiry e)))))

(defun make-vanilla-option (kind strike expiry
                            &rest resolve-args
                            &key (exercise :european) from calendar convention
                                 end-of-month)
  "Return a vanilla OPTION. EXERCISE is :EUROPEAN or :AMERICAN. EXPIRY is a
date, or a tenor resolved to a date now from FROM with CALENDAR, CONVENTION
and END-OF-MONTH (see RESOLVE-EXPIRY). The option stores the date.

  (make-vanilla-option :put 40 #D\"2027-09-24\" :exercise :american)
  (make-vanilla-option :put 40 #T\"1Y\" :from #D\"2026-09-24\")"
  (declare (ignore from calendar convention end-of-month))
  (let ((date (apply #'resolve-expiry expiry
                     (a:remove-from-plist resolve-args :exercise))))
    (make-instance
     'option
     :payoff (make-instance 'vanilla-payoff :kind kind :strike (float strike 1d0))
     :exercise (make-instance (ecase exercise
                                (:european 'european-exercise)
                                (:american 'american-exercise))
                              :expiry date))))

;;; The spec DSL. Each head keyword is a method on PARSE-OPTION-SPEC, so a
;;; new kind of option (a barrier, an Asian) adds syntax with one DEFMETHOD
;;; in its own file, or in user code, without editing MAKE-OPTION.
;;;
;;;   (make-option '(:american :put 40 #D"2027-09-24"))
;;;   (make-option '(:call 100 #D"2027-03-24"))       ; European by default
;;;   (make-option '(:call 100 #T"6M") :from #D"2026-09-24")

(define-condition invalid-option-spec (error)
  ((spec :initarg :spec :reader invalid-option-spec-spec)
   (expected :initarg :expected :initform nil :reader invalid-option-spec-expected))
  (:report
   (lambda (c s)
     (let ((spec (invalid-option-spec-spec c))
           (expected (invalid-option-spec-expected c)))
       (if expected
           (format s "~S is not a valid option spec: expected ~A." spec expected)
           (format s "~S is not an option spec. A spec is a list starting with ~
one of ~{~S~^, ~}." spec (option-spec-heads)))))))

(defgeneric parse-option-spec (head spec resolve-args)
  (:documentation "Build the option described by SPEC, a list whose first
element is HEAD. RESOLVE-ARGS is the plist of expiry-resolution keywords given
to MAKE-OPTION (:FROM, :CALENDAR, :CONVENTION, :END-OF-MONTH). Specialize HEAD
with an EQL specializer to add syntax:

  (defmethod parse-option-spec ((head (eql :digital)) spec resolve-args) ...)")
  (:method (head spec resolve-args)
    (declare (ignore head resolve-args))
    (error 'invalid-option-spec :spec spec)))

(defun option-spec-heads ()
  "The head keywords PARSE-OPTION-SPEC has methods for, in the order the
methods were defined."
  (reverse (loop for method in (c2mop:generic-function-methods #'parse-option-spec)
                 for specializer = (first (c2mop:method-specializers method))
                 when (typep specializer 'c2mop:eql-specializer)
                   collect (c2mop:eql-specializer-object specializer))))

(macrolet ((vanilla (head pattern expected &body body)
             `(defmethod parse-option-spec ((head (eql ,head)) spec resolve-args)
                (match spec
                  (,pattern ,@body)
                  (_ (error 'invalid-option-spec :spec spec :expected ,expected))))))
  (vanilla :call (list _ (guard strike (realp strike)) expiry)
           "(:call strike expiry)"
           (apply #'make-vanilla-option :call strike expiry resolve-args))
  (vanilla :put (list _ (guard strike (realp strike)) expiry)
           "(:put strike expiry)"
           (apply #'make-vanilla-option :put strike expiry resolve-args))
  (vanilla :european (list _ (and kind (or :call :put)) (guard strike (realp strike)) expiry)
           "(:european :call|:put strike expiry)"
           (apply #'make-vanilla-option kind strike expiry :exercise :european resolve-args))
  (vanilla :american (list _ (and kind (or :call :put)) (guard strike (realp strike)) expiry)
           "(:american :call|:put strike expiry)"
           (apply #'make-vanilla-option kind strike expiry :exercise :american resolve-args)))

(defun make-option (spec &rest resolve-args &key from calendar convention end-of-month)
  "Build an OPTION from a list SPEC. The first element selects a
PARSE-OPTION-SPEC method; the built-in shapes are

  (:call strike expiry)  (:put strike expiry)             ; European
  (:european :call|:put strike expiry)
  (:american :call|:put strike expiry)

EXPIRY is a date, or a tenor resolved to a date now from FROM with CALENDAR,
CONVENTION and END-OF-MONTH (see RESOLVE-EXPIRY). A malformed spec signals
INVALID-OPTION-SPEC at construction, naming the expected shape or, for an
unknown head, every head currently defined.

  (make-option '(:american :call 100 #T\"1Y\") :from #D\"2026-09-24\")"
  (declare (ignore from calendar convention end-of-month))
  (if (consp spec)
      (parse-option-spec (first spec) spec resolve-args)
      (error 'invalid-option-spec :spec spec)))
