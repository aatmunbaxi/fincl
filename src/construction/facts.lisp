;;;; construction/facts.lisp --- the fact registry and construction conditions
;;;;
;;;; A fact is a named, typed piece of information that clauses publish and
;;;; components read: :EXPIRY, :STRIKE, :CALENDAR. Facts are registered once,
;;;; globally, so that a name means the same thing in every construction
;;;; language.

(in-package #:fincl)

(defstruct (fact-definition (:constructor make-fact-definition (name type documentation coerce)))
  (name nil :type keyword :read-only t)
  (type t :read-only t)
  (documentation nil :read-only t)
  (coerce nil :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *facts* (make-hash-table :test 'eq)
    "Fact name -> FACT-DEFINITION."))

(defmacro define-fact (name type documentation &key coerce)
  "Register the fact NAME (a keyword) with values of TYPE. COERCE, a function
designator, normalizes a value before the type check. A name means the same
thing in every language, so define a fact once and share it.

  (define-fact :strike double-float \"The option's strike.\"
    :coerce (lambda (x) (float x 1d0)))"
  (check-type name keyword)
  `(progn
     (setf (gethash ,name *facts*)
           (make-fact-definition ,name ',type ,documentation ,coerce))
     ,name))

(defun find-fact-definition (name)
  (or (gethash name *facts*)
      (error 'undeclared-fact :fact name)))

(defun real-to-double (x)
  "Coerce a real to double-float; leave anything else for the type check."
  (if (realp x) (float x 1d0) x))

;;; --------------------------------------------------------------------
;;; Provenance: where a fact came from
;;; --------------------------------------------------------------------

(defstruct (provenance (:constructor make-provenance (form &optional enclosing)))
  "FORM is the clause (as data) that set a fact, (:DEFAULT) or (:RULE fact);
ENCLOSING lists the clauses around it, innermost first."
  (form nil :read-only t)
  (enclosing nil :read-only t))

(defun describe-clause (form)
  "A clause as the user wrote it: (expires #D\"2027-09-24\")."
  (if (and (consp form) (symbolp (first form)))
      (format nil "(~(~A~)~{ ~A~})" (symbol-name (first form))
              (mapcar (lambda (arg)
                        (cond ((and (consp arg) (keywordp (first arg))) (describe-clause arg))
                              ((keywordp arg) (format nil ":~(~A~)" (symbol-name arg)))
                              (t (prin1-to-string arg))))
                      (rest form)))
      (prin1-to-string form)))

(defun describe-provenance (provenance)
  (if (null provenance)
      "an unknown source"
      (let ((form (provenance-form provenance)))
        (format nil "~A~{ in ~A~}"
                (case (and (consp form) (first form))
                  (:default "a language default")
                  (:rule (format nil "the rule for ~S" (second form)))
                  (t (describe-clause form)))
                (mapcar #'describe-clause (provenance-enclosing provenance))))))

;;; --------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------

(define-condition construction-error (invalid-input) ()
  (:documentation "Superclass of every error signalled while building an
object from clauses."))

(define-condition clause-argument-error (construction-error simple-error) ()
  (:documentation "A clause was given arguments it cannot use."))

(define-condition undeclared-fact (construction-error)
  ((fact :initarg :fact :reader construction-fact)
   (clause :initarg :clause :initform nil :reader construction-clause))
  (:report (lambda (c s)
             (if (construction-clause c)
                 (format s "~A publishes ~S, which it does not declare in its :PUBLISHES."
                         (describe-clause (construction-clause c)) (construction-fact c))
                 (format s "~S is not a fact; define it with DEFINE-FACT." (construction-fact c))))))

(define-condition fact-type-error (construction-error)
  ((fact :initarg :fact :reader construction-fact)
   (value :initarg :value :reader construction-value)
   (expected-type :initarg :type :reader construction-type)
   (provenance :initarg :provenance :initform nil :reader construction-provenance))
  (:report (lambda (c s)
             (format s "~S from ~A is ~S, which is not of type ~S."
                     (construction-fact c) (describe-provenance (construction-provenance c))
                     (construction-value c) (construction-type c)))))

(define-condition conflicting-clauses (construction-error)
  ((fact :initarg :fact :reader construction-fact)
   (scope :initarg :scope :initform nil :reader construction-scope)
   (earlier :initarg :earlier :reader conflict-earlier)
   (later :initarg :later :reader conflict-later))
  (:report (lambda (c s)
             (format s "~A and ~A both set ~S~@[ in scope ~S~]."
                     (describe-provenance (conflict-earlier c))
                     (describe-provenance (conflict-later c))
                     (construction-fact c) (construction-scope c)))))

(define-condition missing-fact (construction-error)
  ((fact :initarg :fact :reader construction-fact)
   (scope :initarg :scope :initform nil :reader construction-scope)
   (needed-by :initarg :class :initform nil :reader missing-class)
   (slot :initarg :slot :initform nil :reader missing-slot)
   (missing-inputs :initarg :missing-inputs :initform nil :reader missing-inputs)
   (suppliers :initarg :suppliers :initform nil :reader missing-suppliers))
  (:report (lambda (c s)
             (format s "~@[~A needs ~]~S~@[ (slot ~(~A~))~]~@[ in scope ~S~], which no clause gave."
                     (and (missing-class c) (class-name (missing-class c)))
                     (construction-fact c)
                     (missing-slot c) (construction-scope c))
             (when (missing-suppliers c)
               (format s " Supply it with ~{~A~^, ~}." (missing-suppliers c)))
             (when (missing-inputs c)
               (format s " It can also be derived, but that needs ~{~S~^, ~}."
                       (missing-inputs c))))))

(define-condition fact-rule-cycle (missing-fact)
  ((facts :initarg :facts :reader cycle-facts))
  (:report (lambda (c s)
             (format s "~{~S~^, ~} are each derived from the others, and no clause gave ~
any of them."
                     (cycle-facts c))
             (when (missing-suppliers c)
               (format s " Break the cycle with ~{~A~^, ~}." (missing-suppliers c))))))

(define-condition unknown-clause (construction-error)
  ((name :initarg :name :reader construction-name)
   (language :initarg :language :reader construction-language-of)
   (known :initarg :known :initform nil :reader known-clauses)
   (suggestion :initarg :suggestion :initform nil :reader clause-suggestion))
  (:report (lambda (c s)
             (format s "~(~A~) is not a clause of the ~(~A~) language.~@[ Did you mean ~(~A~)?~]"
                     (construction-name c) (construction-language-of c) (clause-suggestion c))
             (format s " Clauses: ~{~(~A~)~^, ~}." (known-clauses c)))))

(define-condition ambiguous-clause (construction-error)
  ((name :initarg :name :reader construction-name)
   (language :initarg :language :reader construction-language-of)
   (candidates :initarg :candidates :reader clause-candidates))
  (:report (lambda (c s)
             (format s "~(~A~) names more than one clause of the ~(~A~) language: ~{~S~^, ~}. ~
Rename one of them, or use the general clause with the full symbol."
                     (construction-name c) (construction-language-of c)
                     (clause-candidates c)))))

(define-condition malformed-clause (construction-error)
  ((form :initarg :form :reader malformed-form)
   (expected :initarg :expected :reader malformed-expected))
  (:report (lambda (c s)
             (format s "~S is not valid: expected ~A." (malformed-form c) (malformed-expected c))))
  (:documentation "A clause form, or a DEFINE-CLAUSE lambda list, of the wrong
shape."))

(define-condition unknown-language (construction-error)
  ((name :initarg :name :reader construction-name)
   (known :initarg :known :initform nil :reader known-languages))
  (:report (lambda (c s)
             (format s "~S is not a construction language. Languages: ~{~(~A~)~^, ~}."
                     (construction-name c) (known-languages c)))))

(define-condition unknown-convention (construction-error)
  ((name :initarg :name :reader construction-name)
   (language :initarg :language :reader construction-language-of)
   (known :initarg :known :initform nil :reader known-conventions))
  (:report (lambda (c s)
             (format s "~S is not a convention of the ~(~A~) language. Conventions: ~
~:[none~;~:*~{~S~^, ~}~]."
                     (construction-name c) (construction-language-of c)
                     (known-conventions c)))))

(define-condition unused-fact (style-warning)
  ((fact :initarg :fact :reader construction-fact)
   (scope :initarg :scope :initform nil :reader construction-scope)
   (provenance :initarg :provenance :reader construction-provenance))
  (:report (lambda (c s)
             (format s "~S from ~A~@[ in scope ~S~] was not used by anything built."
                     (construction-fact c) (describe-provenance (construction-provenance c))
                     (construction-scope c)))))
