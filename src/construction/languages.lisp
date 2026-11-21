;;;; construction/languages.lisp --- DEFINE-CONSTRUCTION-LANGUAGE
;;;;
;;;; A construction language is a vocabulary of clauses and an assembly function. Every
;;;; language builds an object in these steps:
;;;;
;;;;   1. A new fact table.
;;;;   2. The language defaults, at :DEFAULT priority.
;;;;   3. The clauses, in order, at :EXPLICIT priority.
;;;;   4. The assembly function. It reads facts (REQUIRE-FACT, MAKE-FROM-FACTS) and
;;;;      returns the object.
;;;;   5. A style warning for every explicit or convention fact that nothing read.
;;;;
;;;; DEFINE-CLAUSE, DEFINE-CONVENTION and DEFINE-FACT-RULE add clauses, conventions and
;;;; rules to a language, from any file.

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
        (iter (for (fact value) in-plist (construction-language-defaults language))
              (publish fact value)))
      (apply-clauses clauses)
      (let ((object (funcall (construction-language-assemble language))))
        (iter (for (fact scope provenance) in (unused-fact-entries))
              (warn 'unused-fact :fact fact :scope scope :provenance provenance))
        object))))

(defmacro define-construction-language (name &body options)
  "Define the construction language NAME. OPTIONS:

  (:macro m)           Define M as (m clause...). The macro checks the clause
                       heads at macroexpansion. It evaluates the arguments.
  (:function f)        Define F as (f clauses). CLAUSES is data: a list of
                       (head . arguments).
  (:assemble a)        A is a function of no arguments. It builds the object
                       from the fact table.
  (:defaults fact value ...)
                       Facts that are published at :DEFAULT priority before the
                       clauses.
  (:resolver r (fact ...) description)
                       R receives an unknown clause name (a string). It can
                       return a clause definition that it builds on demand.
                       Use it for vocabularies that grow with classes. The
                       facts and the description are used in reports.
  (:other-shape f)     F is a function of the arguments of the macro. It must
                       be defined before the language. It returns the
                       expansion of a call that is not written as clauses, or
                       NIL for a call that is. It lets a macro accept the call
                       shape of the function that it replaced.
  (:documentation string)

Every language also has the clause (convention name).

  (define-construction-language option
    (:macro make-option) (:function make-option-from-clauses)
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
          (resolver (option :resolver))
          (other-shape (first (option :other-shape))))
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
           (or ,@(when other-shape `((,other-shape clauses)))
               (expand-language-form ',name ',function clauses)))
         (refresh-language-documentation ',name)
         ',name))))
