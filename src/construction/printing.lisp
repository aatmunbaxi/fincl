;;;; construction/printing.lisp --- objects as the clauses that build them
;;;;
;;;; An object built by a language prints as its clauses, and with
;;;; *PRINT-READABLY* as the language macro form, which reads (with
;;;; FINCL:SYNTAX for dates) and evaluates back to an equivalent object.

(in-package #:fincl)

(defgeneric object-clauses (language object)
  (:documentation "The clauses (as data, (head . arguments)) that LANGUAGE's
data function turns back into an object equivalent to OBJECT.

  (object-clauses 'option o) => ((:PUT 95d0) (:AMERICAN) (:EXPIRES #D\"2027-09-24\"))"))

(defun clause-like-p (language value)
  (and (consp value) (keywordp (first value))
       (gethash (clause-key (first value))
                (construction-language-clauses (find-language language)))))

(defun argument-source (language value)
  "A form that evaluates to VALUE: quoted unless it evaluates to itself."
  (cond ((clause-like-p language value) (clause-source language value))
        ((or (consp value) (and (symbolp value) (not (keywordp value))
                                (not (member value '(t nil)))))
         `(quote ,value))
        (t value)))

(defun clause-source (language clause)
  "CLAUSE (data) as it would be written in the language macro."
  `(,(first clause) ,@(mapcar (lambda (arg) (argument-source language arg)) (rest clause))))

(defun print-clause-form (language object stream)
  "Print OBJECT as its clauses: readably as the language macro form under
*PRINT-READABLY*, otherwise as #<TYPE clause...>."
  (let ((clauses (object-clauses language object)))
    (if *print-readably*
        (prin1 `(,(construction-language-macro (find-language language))
                 ,@(mapcar (lambda (clause) (clause-source language clause)) clauses))
               stream)
        (print-unreadable-object (object stream :type t)
          (format stream "~{~A~^ ~}" (mapcar #'describe-clause clauses))))))

(defun round-trip-p (language object &key (test #'equalp))
  "True if rebuilding OBJECT from its clauses gives an object with the same
clauses, under TEST."
  (let* ((clauses (object-clauses language object))
         (rebuilt (funcall (construction-language-function (find-language language)) clauses)))
    (funcall test clauses (object-clauses language rebuilt))))
