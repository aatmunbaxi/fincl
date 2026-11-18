;;;; construction/clauses.lisp --- languages and their clauses
;;;;
;;;; A construction language owns a vocabulary of clauses. A clause is a small
;;;; form, (expires #D"2027-09-24"), whose head names it and whose arguments
;;;; are values; running it publishes facts. Clause names are matched by
;;;; symbol name, as iterate matches its clauses, so they work from any
;;;; package without exports.
;;;;
;;;; As data, a clause is (head . arguments) with HEAD a symbol (the language
;;;; macros emit keywords). A clause whose lambda list ends in &CLAUSES takes
;;;; trailing sub-clauses, which it runs itself, usually in a child scope.

(in-package #:fincl)

(defstruct (construction-language (:constructor make-construction-language (name)))
  (name nil :read-only t)
  (documentation nil)
  (macro nil)
  (function nil)
  (defaults '())
  (assemble nil)
  (resolver nil)
  (resolver-publishes '())
  (resolver-description nil)
  (clauses (make-hash-table :test 'equal) :read-only t)
  (conventions (make-hash-table :test 'equal) :read-only t)
  (rules (make-hash-table :test 'eq) :read-only t))

(defstruct (clause-definition (:constructor make-clause-definition
                                  (name lambda-list required subclauses publishes
                                   documentation function &optional subclause-language)))
  (name "" :type string :read-only t)
  (lambda-list nil :read-only t)
  (required 0 :type fixnum :read-only t)
  (subclauses nil :read-only t)
  (publishes '() :read-only t)
  (documentation nil :read-only t)
  (function nil :read-only t)
  (subclause-language nil :read-only t)
  (serial 0 :type fixnum))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *construction-languages* (make-hash-table :test 'eq)
    "Language name -> CONSTRUCTION-LANGUAGE.")

  (defun ensure-language (name)
    (or (gethash name *construction-languages*)
        (setf (gethash name *construction-languages*) (make-construction-language name))))

  (defun find-language (name)
    (or (gethash name *construction-languages*)
        (error 'unknown-language
               :name name
               :known (iter (for (language nil) in-hashtable *construction-languages*)
                            (collect language)))))

  (defun clause-key (name)
    "Clauses are keyed by symbol name."
    (string name))

  (defvar *clause-serial* 0
    "Counter giving each clause its place in definition order.")

  (defun lambda-list-text (form)
    "FORM, a lambda list or part of one, in lower case with symbols written by
name alone, so a clause defined in any package reads the same."
    (typecase form
      (keyword (format nil ":~(~A~)" form))
      (symbol (string-downcase (symbol-name form)))
      (cons (if (and (eq (first form) 'quote) (null (cddr form)))
                (format nil "'~A" (lambda-list-text (second form)))
                (format nil "(~{~A~^ ~})" (mapcar #'lambda-list-text form))))
      (t (prin1-to-string form))))

  (defun clause-signature (definition)
    "The clause written as it is used: (name parameter ...), in lower case."
    (format nil "(~(~A~)~{ ~A~})"
            (clause-definition-name definition)
            (mapcar #'lambda-list-text (clause-definition-lambda-list definition))))

  (defun first-sentence (string)
    "The first sentence of the first paragraph of STRING on one line (up to a
full stop followed by a space or the end), or NIL for no string."
    (when string
      (let* ((paragraph (subseq string 0 (search (format nil "~%~%") string)))
             (words (iter (with start = 0)
                          (for end = (position-if (lambda (c) (member c '(#\Space #\Newline)))
                                                  paragraph :start start))
                          (when (< start (or end (length paragraph)))
                            (collect (subseq paragraph start end)))
                          (while end)
                          (setf start (1+ end))))
             ;; A word ending in a full stop closes the sentence unless the
             ;; next word starts in lower case ("e.g. a thing").
             (sentence (iter (for (word next) on words)
                             (collect word into taken)
                             (when (and (> (length word) 1)
                                        (char= (char word (1- (length word))) #\.)
                                        (or (null next) (not (lower-case-p (char next 0)))))
                               (return taken))
                             (finally (return taken)))))
        (format nil "~{~A~^ ~}" sentence))))

  (defun clauses-in-definition-order (language)
    (sort (iter (for (nil definition) in-hashtable (construction-language-clauses language))
                (collect definition))
          #'< :key #'clause-definition-serial))

  (defun language-documentation-text (language)
    "The documentation of the language's macro and data function: its
description, one line per clause (lambda list and first line of the clause's
documentation, in definition order), the clauses its resolver makes, and the
conventions."
    (let ((resolver (construction-language-resolver-description language))
          (conventions (sorted-keys (construction-language-conventions language))))
      (with-output-to-string (out)
        (write-string (or (construction-language-documentation language) "") out)
        (format out "~%~%Clauses:")
        (dolist (definition (clauses-in-definition-order language))
          (format out "~%  ~A~@[ ~A~]" (clause-signature definition)
                  (first-sentence (clause-definition-documentation definition))))
        (when resolver
          (format out "~%  ~A" resolver))
        (when conventions
          (format out "~%~%Conventions: ~{~(~A~)~^, ~}." conventions)))))

  (defun refresh-language-documentation (language-name)
    "Recompute the documentation of the language's macro and data function from
its clauses and conventions; does nothing for a name not defined yet."
    (let ((language (gethash language-name *construction-languages*)))
      (when language
        (let ((text (language-documentation-text language))
              (macro (construction-language-macro language))
              (function (construction-language-function language)))
          (when (and function (fboundp function))
            (setf (documentation function 'function)
                  (format nil "~A~%~%CLAUSES is a list of (head . arguments), as data." text)))
          (when (and macro (macro-function macro))
            (setf (documentation macro 'function)
                  (format nil "~A~%~%Clause arguments are evaluated." text)))))))

  (defun register-clause (language definition)
    (let* ((table (construction-language-clauses (ensure-language language)))
           (old (gethash (clause-definition-name definition) table)))
      (setf (clause-definition-serial definition)
            (if old (clause-definition-serial old) (incf *clause-serial*))
            (gethash (clause-definition-name definition) table) definition))
    (refresh-language-documentation language))

  (defun find-language-by-name (name)
    "The language whose name has the symbol name of NAME (a symbol or string),
whatever package NAME is in; signals UNKNOWN-LANGUAGE."
    (or (iter (for (language-name language) in-hashtable *construction-languages*)
              (when (string-equal (symbol-name language-name) (string name))
                (return language)))
        (error 'unknown-language
               :name name
               :known (iter (for (language nil) in-hashtable *construction-languages*)
                            (collect language)))))

  (defun parse-clause-lambda-list (lambda-list)
    "Return (values function-lambda-list required-count subclauses-p). With
&CLAUSES VAR, only required parameters may precede it."
    (let ((position (position "&CLAUSES" lambda-list
                              :test #'equal :key (lambda (p) (and (symbolp p) (symbol-name p))))))
      (if position
          (let ((required (subseq lambda-list 0 position))
                (var (nth (1+ position) lambda-list)))
            (unless (and var (notany (lambda (p) (member p lambda-list-keywords)) required))
              (error 'malformed-clause
                     :form lambda-list
                     :expected "a lambda list of required parameters, then &CLAUSES and one variable"))
            (values `(,@required ,var) (length required) t))
          (values lambda-list
                  (or (position-if (lambda (p) (member p lambda-list-keywords)) lambda-list)
                      (length lambda-list))
                  nil))))

  (defun clause-names (language)
    (sorted-keys (construction-language-clauses language)))

  (defun find-clause (language-name head)
    "The clause definition for HEAD in the language, trying the language's
resolver for clauses it creates on demand; signals UNKNOWN-CLAUSE."
    (let* ((language (find-language language-name))
           (key (clause-key head)))
      (or (gethash key (construction-language-clauses language))
          (let ((resolver (construction-language-resolver language)))
            (and resolver (fboundp resolver) (funcall resolver key)))
          (error 'unknown-clause
                 :name key :language language-name :known (clause-names language)
                 :also (construction-language-resolver-description language)))))

  (defun expand-clause-form (language-name form)
    "The form that evaluates to clause FORM as data: its head checked now (at
macroexpansion), its arguments evaluated at run time, its sub-clauses
expanded in turn."
    (unless (and (consp form) (symbolp (first form)))
      (error 'malformed-clause :form form :expected "a list starting with the clause name"))
    ;; (m '((head ...) ...)): clauses passed as data to the macro.
    (when (member (symbol-name (first form)) '("QUOTE" "QUASIQUOTE") :test #'string=)
      (error 'malformed-clause
             :form form
             :expected (format nil "clauses written directly, as (~(~A~) (head arg ...) ...); ~
for clauses as data use ~(~A~)"
                               (construction-language-macro (find-language language-name))
                               (construction-language-function (find-language language-name)))))
    (let* ((definition (find-clause language-name (first form)))
           (head (a:make-keyword (clause-key (first form))))
           (args (rest form)))
      (if (clause-definition-subclauses definition)
          (let ((n (clause-definition-required definition)))
            `(list ',head ,@(subseq args 0 (min n (length args)))
                   ,@(mapcar (lambda (sub)
                               (expand-clause-form (or (clause-definition-subclause-language definition)
                                                       language-name)
                                                   sub))
                             (nthcdr n args))))
          `(list ',head ,@args)))))

(defmacro define-clause (language name lambda-list &body body)
  "Define the clause NAME of LANGUAGE. LAMBDA-LIST receives the clause's
arguments, already evaluated; if it ends in &CLAUSES VAR (after required
parameters only), the remaining forms are sub-clauses, bound to VAR as data,
for the body to run with APPLY-CLAUSES, usually inside WITH-SCOPE. BODY may
start with a docstring, a (:PUBLISHES fact...) form declaring every fact it
may set, and a (:CLAUSES-OF language) form saying the sub-clauses are those of
another language, which the body builds with that language's data function.

  (define-clause option expires (when &key from calendar)
    (:publishes :expiry)
    \"The expiry: a date, or a tenor resolved from FROM.\"
    (publish :expiry (resolve-expiry when :from from :calendar calendar)))"
  (let ((publishes '()) (documentation nil) (subclause-language nil) (forms body))
    (iter (while forms)
          (cond ((and (stringp (first forms)) (rest forms) (null documentation))
                 (setf documentation (pop forms)))
                ((and (consp (first forms)) (eq (first (first forms)) :publishes))
                 (setf publishes (rest (pop forms))))
                ((and (consp (first forms)) (eq (first (first forms)) :clauses-of))
                 (setf subclause-language (second (pop forms))))
                (t (return))))
    (multiple-value-bind (function-lambda-list required subclauses)
        (parse-clause-lambda-list lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (register-clause ',language
                          (make-clause-definition ,(clause-key name) ',lambda-list ,required
                                                  ,subclauses ',publishes ,documentation
                                                  (lambda ,function-lambda-list ,@forms)
                                                  ',subclause-language))
         ',name))))

(defun apply-clause (form)
  "Run clause FORM (data) of the current language."
  (destructuring-bind (head &rest args) form
    (let* ((definition (find-clause (construction-language-name *language*) head))
           (*provenance* (make-provenance form (and *provenance*
                                                    `(,(provenance-form *provenance*)
                                                      ,@(provenance-enclosing *provenance*)))))
           (*publishable* (clause-definition-publishes definition))
           (function (clause-definition-function definition)))
      (if (clause-definition-subclauses definition)
          (let ((n (clause-definition-required definition)))
            (apply function `(,@(subseq args 0 (min n (length args))) ,(nthcdr n args))))
          (apply function args)))))

(declaim (ftype (function (list) (values  &optional)) apply-clauses))
(defun apply-clauses (forms)
  "Run clause FORMS (data) in order, in the current scope."
  (mapc #'apply-clause forms)
  (values))

(defun fact-suppliers (fact)
  "Descriptions of the clauses (and resolver) of the current language that
can publish FACT."
  (when *language*
    (let ((suppliers
            (iter (for (nil definition) in-hashtable (construction-language-clauses *language*))
                  (when (member fact (clause-definition-publishes definition))
                    (collect (format nil "(~(~A~))" (clause-definition-name definition)))))))
      (when (and (member fact (construction-language-resolver-publishes *language*))
                 (construction-language-resolver-description *language*))
        (setf suppliers `(,@suppliers ,(construction-language-resolver-description *language*))))
      (sort suppliers #'string<))))

(declaim (ftype (function (t) (values list &optional)) language-clauses))
(defun language-clauses (language)
  "The clauses of the construction language LANGUAGE (a symbol or its name), in
definition order, as property lists with keys :NAME (a string), :LAMBDA-LIST,
:DOCUMENTATION (a string or NIL), :PUBLISHES (the facts the clause may set) and
:SUBCLAUSES (true when it takes trailing sub-clauses). Clauses a language's
resolver makes on demand are not listed.

  (language-clauses 'option) => ((:name \"CALL\" :lambda-list (strike) ...) ...)"
  (mapcar (lambda (definition)
            `(:name ,(clause-definition-name definition)
              :lambda-list ,(clause-definition-lambda-list definition)
              :documentation ,(clause-definition-documentation definition)
              :publishes ,(clause-definition-publishes definition)
              :subclauses ,(and (clause-definition-subclauses definition) t)))
          (clauses-in-definition-order (find-language-by-name language))))

(defun share-clauses (from to names)
  "Give the language TO the clauses NAMES (strings) of the language FROM, so
the two languages mean the same by them."
  (let ((source (find-language from)))
    (dolist (name names)
      (register-clause to (copy-clause-definition
                           (or (gethash name (construction-language-clauses source))
                               (error 'unknown-clause :name name :language from
                                                      :known (clause-names source))))))))

(defun share-fact-rules (from to facts)
  "Give the language TO the rules of the language FROM for FACTS."
  (dolist (fact facts)
    (setf (gethash fact (construction-language-rules (ensure-language to)))
          (gethash fact (construction-language-rules (find-language from))))))
