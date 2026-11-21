;;;; construction/clauses.lisp --- languages and their clauses
;;;;
;;;; A construction language owns a vocabulary of clauses. A clause is a small form,
;;;; for example (expires #D"2027-09-24"). Its head names it and its arguments are
;;;; values. When the clause runs, it publishes facts. The library matches clause names
;;;; by symbol name, as iterate matches its clauses. Thus they work from any package
;;;; without exports.
;;;;
;;;; As data, a clause is (head . arguments). HEAD is a symbol (the language macros
;;;; emit keywords). A clause whose lambda list ends in &CLAUSES takes trailing
;;;; sub-clauses. The clause runs them itself, usually in a child scope.

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
  (rules (make-hash-table :test 'eq) :read-only t)
  (shared-clauses '())                  ; ((source-language name...) ...), see SHARE-CLAUSES
  (shared-rules '()))                   ; ((source-language fact...) ...), see SHARE-FACT-RULES

(defun clause-maximum-arguments (lambda-list)
  "The most arguments a clause with LAMBDA-LIST takes, or NIL where there is
no limit (&REST, &KEY, &BODY or &CLAUSES). Markers are matched by name, as
&CLAUSES is a symbol of any package."
  (let ((count 0))
    (dolist (parameter lambda-list count)
      (let ((marker (and (symbolp parameter) (symbol-name parameter))))
        (cond ((member marker '("&REST" "&KEY" "&BODY" "&CLAUSES") :test #'equal) (return nil))
              ((equal marker "&AUX") (return count))
              ((equal marker "&OPTIONAL"))
              (t (incf count)))))))

(defstruct (clause-definition (:constructor make-clause-definition
                                  (name lambda-list required subclauses publishes
                                   documentation function &optional subclause-language symbol
                                   subclause-names
                                   &aux (maximum (clause-maximum-arguments lambda-list)))))
  (name "" :type string :read-only t)
  (lambda-list nil :read-only t)
  (required 0 :type fixnum :read-only t)
  (maximum nil :type (or null fixnum) :read-only t)
  (symbol nil :type symbol :read-only t)
  (subclauses nil :read-only t)
  (publishes '() :read-only t)
  (documentation nil :read-only t)
  (function nil :read-only t)
  (subclause-language nil :read-only t)
  (subclause-names nil :read-only t)    ; NIL, or a function of no arguments -> names allowed
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

  (defun registered-clause (language key)
    "The definition of the clause with name KEY in LANGUAGE: its own, else
one it shares, looked up now in the language it shares it from. NIL if there
is none; the resolver is not asked."
    (or (gethash key (construction-language-clauses language))
        (iter (for (source names) in (construction-language-shared-clauses language))
              (when (member key names :test #'string=)
                (return (a:when-let ((source-language (gethash source *construction-languages*)))
                          (registered-clause source-language key)))))))

  (defun clauses-in-definition-order (language)
    "The definitions of LANGUAGE's clauses: its own in definition order, then
those it shares, in the order they were shared."
    (let* ((own (sort (iter (for (nil definition) in-hashtable (construction-language-clauses language))
                            (collect definition))
                      #'< :key #'clause-definition-serial))
           (shared (iter (for (nil names) in (construction-language-shared-clauses language))
                         (appending (iter (for name in names)
                                          (for definition = (registered-clause language name))
                                          (when definition (collect definition)))))))
      (remove-duplicates `(,@own ,@shared) :from-end t)))

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

  (defun package-name-of (symbol)
    (a:when-let ((package (and symbol (symbol-package symbol))))
      (package-name package)))

  (defun check-clause-redefinition (language old new)
    "Warn with CLAUSE-REDEFINED when NEW, a clause of LANGUAGE, replaces OLD
and the two names are symbols of different packages."
    (let ((old-symbol (clause-definition-symbol old))
          (new-symbol (clause-definition-symbol new)))
      (when (and old-symbol new-symbol
                 (not (eq (symbol-package old-symbol) (symbol-package new-symbol))))
        (warn 'clause-redefined :name (clause-definition-name new) :language language
                                :old-package (package-name-of old-symbol)
                                :new-package (package-name-of new-symbol)))))

  (defun register-clause (language definition)
    (let* ((table (construction-language-clauses (ensure-language language)))
           (old (gethash (clause-definition-name definition) table)))
      (when old
        (check-clause-redefinition language old definition))
      (setf (clause-definition-serial definition)
            (if old (clause-definition-serial old) (incf *clause-serial*))
            (gethash (clause-definition-name definition) table) definition))
    (refresh-language-documentation language)
    ;; A language that shares clauses of this one lists the new definition.
    (iter (for (other-name other) in-hashtable *construction-languages*)
          (when (assoc language (construction-language-shared-clauses other))
            (refresh-language-documentation other-name))))

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
    "The names (strings) of LANGUAGE's clauses, its own and shared, sorted."
    (sort (mapcar #'clause-definition-name (clauses-in-definition-order language))
          #'string<))

  (defun find-clause (language-name head)
    "The clause definition for HEAD in the language, trying the language's
resolver for clauses it creates on demand; signals UNKNOWN-CLAUSE."
    (let* ((language (find-language language-name))
           (key (clause-key head)))
      (or (registered-clause language key)
          (let ((resolver (construction-language-resolver language)))
            (and resolver (fboundp resolver) (funcall resolver key)))
          (error 'unknown-clause
                 :name key :language language-name :known (clause-names language)
                 :also (construction-language-resolver-description language)))))

  (defun arity-text (minimum maximum)
    (cond ((eql minimum maximum) (format nil "exactly ~D" minimum))
          ((null maximum) (format nil "at least ~D" minimum))
          (t (format nil "~D to ~D" minimum maximum))))

  (defun check-clause-arity (definition form)
    "Signal CLAUSE-ARGUMENT-ERROR unless the number of arguments of the clause
FORM, (head . arguments), is one DEFINITION's lambda list takes. For a clause
with &REST, &KEY or &CLAUSES only the required arguments are counted."
    (let ((given (length (rest form)))
          (minimum (clause-definition-required definition))
          (maximum (clause-definition-maximum definition)))
      (unless (and (>= given minimum) (or (null maximum) (<= given maximum)))
        (error 'clause-argument-error
               :format-control "~A gives ~D argument~:P, but the clause is ~A, which takes ~A."
               :format-arguments `(,(describe-clause form) ,given ,(clause-signature definition)
                                   ,(arity-text minimum maximum))))))

  (defun check-subclause-names (definition form subclauses)
    "Signal CLAUSE-ARGUMENT-ERROR if a form in SUBCLAUSES, the sub-clauses of
the clause FORM, is not one DEFINITION allows (when it limits them)."
    (a:when-let ((names (clause-definition-subclause-names definition)))
      (let ((allowed (funcall names)))
        (dolist (sub subclauses)
          (unless (and (consp sub) (symbolp (first sub))
                       (member (clause-key (first sub)) allowed :test #'string=))
            (error 'clause-argument-error
                   :format-control "~A in ~A is not a sub-clause of ~A, which takes ~{(~(~A~))~^, ~}."
                   :format-arguments `(,(if (consp sub) (describe-clause sub) sub)
                                       ,(describe-clause form) ,(clause-signature definition)
                                       ,allowed)))))))

  (defun expand-clause-form (language-name form)
    "The form that evaluates to clause FORM as data: its head checked now (at
macroexpansion), and its argument count too when the clause is registered
(a clause made by the language's resolver is checked when it runs), its
arguments evaluated at run time, its sub-clauses expanded in turn."
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
      (when (registered-clause (find-language language-name) (clause-key (first form)))
        (check-clause-arity definition form))
      (if (clause-definition-subclauses definition)
          (let ((n (clause-definition-required definition)))
            (check-subclause-names definition form (nthcdr n args))
            `(list ',head ,@(subseq args 0 (min n (length args)))
                   ,@(mapcar (lambda (sub)
                               (expand-clause-form (or (clause-definition-subclause-language definition)
                                                       language-name)
                                                   sub))
                             (nthcdr n args))))
          `(list ',head ,@args)))))

(defmacro define-clause (language name lambda-list &body body)
  "Define the clause NAME of LANGUAGE. LAMBDA-LIST receives the arguments of the
clause, already evaluated. If LAMBDA-LIST ends in &CLAUSES VAR (after required
parameters only), the remaining forms are sub-clauses. They are bound to VAR as
data. The body runs them with APPLY-CLAUSES, usually inside WITH-SCOPE.

BODY can start with a docstring and these forms:
  (:PUBLISHES fact...)       declares every fact that the clause can set
  (:CLAUSES-OF language)     the sub-clauses are those of another language. The
                             body builds them with the data function of that
                             language
  (:SUBCLAUSE-NAMES function) FUNCTION returns the names (strings) of the
                             sub-clauses that are allowed. It is called when a
                             form is expanded or run

  (define-clause option expires (when &key from calendar)
    (:publishes :expiry)
    \"The expiry: a date, or a tenor resolved from FROM.\"
    (publish :expiry (resolve-expiry when :from from :calendar calendar)))"
  (let ((publishes '()) (documentation nil) (subclause-language nil) (subclause-names nil)
        (forms body))
    (iter (while forms)
          (cond ((and (stringp (first forms)) (rest forms) (null documentation))
                 (setf documentation (pop forms)))
                ((and (consp (first forms)) (eq (first (first forms)) :publishes))
                 (setf publishes (rest (pop forms))))
                ((and (consp (first forms)) (eq (first (first forms)) :clauses-of))
                 (setf subclause-language (second (pop forms))))
                ((and (consp (first forms)) (eq (first (first forms)) :subclause-names))
                 (setf subclause-names (second (pop forms))))
                (t (return))))
    (multiple-value-bind (function-lambda-list required subclauses)
        (parse-clause-lambda-list lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (register-clause ',language
                          (make-clause-definition ,(clause-key name) ',lambda-list ,required
                                                  ,subclauses ',publishes ,documentation
                                                  (lambda ,function-lambda-list ,@forms)
                                                  ',subclause-language ',name
                                                  ,subclause-names))
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
      (check-clause-arity definition form)
      (if (clause-definition-subclauses definition)
          (let ((n (clause-definition-required definition)))
            (check-subclause-names definition form (nthcdr n args))
            (apply function `(,@(subseq args 0 (min n (length args))) ,(nthcdr n args))))
          (apply function args)))))

(declaim (ftype (function (list) (values  &optional)) apply-clauses))
(defun apply-clauses (forms)
  "Run the clause FORMS (data) in order, in the current scope."
  (mapc #'apply-clause forms)
  (values))

(defun fact-suppliers (fact)
  "Descriptions of the clauses (and resolver) of the current language that
can publish FACT."
  (when *language*
    (let ((suppliers
            (iter (for definition in (clauses-in-definition-order *language*))
                  (when (member fact (clause-definition-publishes definition))
                    (collect (format nil "(~(~A~))" (clause-definition-name definition)))))))
      (when (and (member fact (construction-language-resolver-publishes *language*))
                 (construction-language-resolver-description *language*))
        (setf suppliers `(,@suppliers ,(construction-language-resolver-description *language*))))
      (sort suppliers #'string<))))

(declaim (ftype (function (t) (values list &optional)) language-clauses))
(defun language-clauses (language)
  "Return the clauses of the construction language LANGUAGE (a symbol or its name), in
the order of definition, as property lists. The keys are :NAME (a string),
:LAMBDA-LIST, :DOCUMENTATION (a string or NIL), :PUBLISHES (the facts that the
clause can set) and :SUBCLAUSES (true when the clause takes trailing sub-clauses).
The list does not show clauses that the resolver of the language makes on demand.

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
the two languages mean the same by them. The clauses are looked up in FROM
whenever TO needs one, so a later redefinition reaches TO; a clause TO
defines itself wins over a shared one."
  (let ((source (find-language from))
        (language (ensure-language to)))
    (dolist (name names)
      (unless (registered-clause source (clause-key name))
        (error 'unknown-clause :name name :language from :known (clause-names source))))
    (let ((shared (construction-language-shared-clauses language)))
      (setf (construction-language-shared-clauses language)
            `(,@(remove from shared :key #'first)
              (,from ,(remove-duplicates `(,@(second (assoc from shared)) ,@(mapcar #'clause-key names))
                                         :test #'string= :from-end t)))))
    (refresh-language-documentation to)
    (values)))

(defun share-fact-rules (from to facts)
  "Give the language TO the rules of the language FROM for FACTS, looked up in
FROM whenever TO derives one of them; a rule TO defines itself wins."
  (find-language from)
  (let* ((language (ensure-language to))
         (shared (construction-language-shared-rules language)))
    (setf (construction-language-shared-rules language)
          `(,@(remove from shared :key #'first)
            (,from ,(remove-duplicates `(,@(second (assoc from shared)) ,@facts) :from-end t))))
    (values)))
