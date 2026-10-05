;;;; construction/languages.lisp --- DEFINE-CONSTRUCTION-LANGUAGE
;;;;
;;;; A construction language is a vocabulary of clauses plus an assembly
;;;; function. Every language builds the same way:
;;;;
;;;;   1. a fresh fact table;
;;;;   2. the language defaults, at :DEFAULT priority;
;;;;   3. the clauses, in order, at :EXPLICIT priority;
;;;;   4. the assembly function, which reads facts (REQUIRE-FACT,
;;;;      MAKE-FROM-FACTS) and returns the object;
;;;;   5. a style warning for every explicit or convention fact nothing read.
;;;;
;;;; Clauses, conventions and rules are added to a language with
;;;; DEFINE-CLAUSE, DEFINE-CONVENTION and DEFINE-FACT-RULE, from any file.

(in-package #:fincl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-language-form (language function clauses)
    "The expansion of the language macro: FUNCTION applied to CLAUSES as data,
with every clause head checked now."
    `(,function (list ,@(mapcar (lambda (clause) (expand-clause-form language clause))
                                clauses)))))

(defun build-from-clauses (language-name clauses)
  "Build an object of the language LANGUAGE-NAME from CLAUSES (data)."
  (let ((language (find-language language-name)))
    (with-fact-table (:language language)
      (let ((*priority* :default)
            (*provenance* (make-provenance '(:default))))
        (loop for (fact value) on (construction-language-defaults language) by #'cddr
              do (publish fact value)))
      (apply-clauses clauses)
      (let ((object (funcall (construction-language-assemble language))))
        (loop for (fact scope provenance) in (unused-fact-entries)
              do (warn 'unused-fact :fact fact :scope scope :provenance provenance))
        object))))

(defmacro define-construction-language (name &body options)
  "Define the construction language NAME. OPTIONS:

  (:macro m)           M is defined as (m clause...): clause heads checked at
                       macroexpansion, arguments evaluated.
  (:function f)        F is defined as (f clauses), CLAUSES as data: a list
                       of (head . arguments).
  (:assemble a)        A, a function of no arguments, builds the object from
                       the fact table.
  (:defaults fact value ...)
                       Facts published at :DEFAULT priority before the clauses.
  (:resolver r (fact ...) description)
                       R, given an unknown clause name (a string), may return
                       a clause definition built on demand (for vocabularies
                       that grow with classes); the facts it publishes and a
                       description are used in reports.
  (:documentation string)

Every language also gets the clause (convention name).

  (define-construction-language option
    (:macro option) (:function make-option)
    (:defaults :exercise-style :european)
    (:assemble assemble-option))"
  (flet ((option (key) (rest (assoc key options))))
    (let ((macro (first (option :macro)))
          (function (first (option :function)))
          (documentation (or (first (option :documentation))
                             (format nil "Build an object of the ~(~A~) language from clauses."
                                     name)))
          (defaults (option :defaults))
          (assemble (first (option :assemble)))
          (resolver (option :resolver)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (let ((language (ensure-language ',name)))
             (setf (construction-language-documentation language) ,documentation
                   (construction-language-macro language) ',macro
                   (construction-language-function language) ',function
                   (construction-language-defaults language) (list ,@defaults)
                   (construction-language-assemble language) ',assemble
                   (construction-language-resolver language) ',(first resolver)
                   (construction-language-resolver-publishes language) ',(second resolver)
                   (construction-language-resolver-description language) ,(third resolver))
             (register-convention-clause ',name)))
         (defun ,function (clauses)
           ,(format nil "~A CLAUSES is a list of (head . arguments), as data." documentation)
           (build-from-clauses ',name clauses))
         (defmacro ,macro (&rest clauses)
           ,(format nil "~A Clause arguments are evaluated." documentation)
           (expand-language-form ',name ',function clauses))
         ',name))))
