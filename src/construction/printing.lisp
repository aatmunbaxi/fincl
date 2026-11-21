;;;; construction/printing.lisp --- objects as the clauses that build them
;;;;
;;;; An object that a language built prints as its clauses. Under *PRINT-READABLY* it
;;;; prints as the macro form of the language. The reader reads that form (with
;;;; FINCL:SYNTAX for dates) and evaluates it back to an equivalent object.

(in-package #:fincl)

(defgeneric object-clauses (language object)
  (:documentation "Return the clauses (as data, (head . arguments)) that the data function of LANGUAGE
turns back into an object equivalent to OBJECT.

  (object-clauses 'option o) => ((:PUT 95d0) (:AMERICAN) (:EXPIRES #D\"2027-09-24\"))"))

(defun clause-like-p (language value)
  (and (consp value) (keywordp (first value))
       (registered-clause (find-language language) (clause-key (first value)))
       t))

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

(declaim (ftype (function (t t t) (values t &optional)) print-clause-form))
(defun print-clause-form (language object stream)
  "Print OBJECT as its clauses. Under *PRINT-READABLY* the output is the macro form of
the language that can be read again. Otherwise it is #<TYPE clause...>."
  (let ((clauses (object-clauses language object)))
    (if *print-readably*
        (prin1 `(,(construction-language-macro (find-language language))
                 ,@(mapcar (lambda (clause) (clause-source language clause)) clauses))
               stream)
        (print-unreadable-object (object stream :type t)
          (format stream "~{~A~^ ~}" (mapcar #'describe-clause clauses))))))

(declaim (ftype (function (t t &key (:test t)) (values t &optional)) round-trip-p))
(defun round-trip-p (language object &key (test #'equalp))
  "True if a rebuild of OBJECT from its clauses gives an object with the same clauses,
under TEST."
  (let* ((clauses (object-clauses language object))
         (rebuilt (funcall (construction-language-function (find-language language)) clauses)))
    (funcall test clauses (object-clauses language rebuilt))))
