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
                                   documentation function)))
  (name "" :type string :read-only t)
  (lambda-list nil :read-only t)
  (required 0 :type fixnum :read-only t)
  (subclauses nil :read-only t)
  (publishes '() :read-only t)
  (documentation nil :read-only t)
  (function nil :read-only t))

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
               :known (loop for language being the hash-keys of *construction-languages*
                            collect language))))

  (defun clause-key (name)
    "Clauses are keyed by symbol name."
    (string name))

  (defun register-clause (language definition)
    (setf (gethash (clause-definition-name definition)
                   (construction-language-clauses (ensure-language language)))
          definition))

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

  (defun edit-distance (a b)
    "Levenshtein distance between strings A and B, case-insensitively."
    (let* ((a (string-downcase a)) (b (string-downcase b))
           (row (loop for j to (length b) collect j)))
      (loop for i from 1 to (length a)
            do (let ((new `(,i)))
                 (loop for j from 1 to (length b)
                       for cost = (if (char= (char a (1- i)) (char b (1- j))) 0 1)
                       do (setf new `(,@new ,(min (1+ (nth j row))
                                                  (1+ (nth (1- j) new))
                                                  (+ (nth (1- j) row) cost)))))
                 (setf row new)))
      (nth (length b) row)))

  (defun clause-names (language)
    (sort (loop for name being the hash-keys of (construction-language-clauses language)
                collect name)
          #'string<))

  (defun find-clause (language-name head)
    "The clause definition for HEAD in the language, trying the language's
resolver for clauses it creates on demand; signals UNKNOWN-CLAUSE."
    (let* ((language (find-language language-name))
           (key (clause-key head)))
      (or (gethash key (construction-language-clauses language))
          (let ((resolver (construction-language-resolver language)))
            (and resolver (fboundp resolver) (funcall resolver key)))
          (let* ((known (clause-names language))
                 (best (first (sort (copy-list known) #'<
                                    :key (lambda (k) (edit-distance key k))))))
            (error 'unknown-clause
                   :name key :language language-name :known known
                   :suggestion (and best
                                    (<= (edit-distance key best) (max 2 (floor (length key) 3)))
                                    best))))))

  (defun expand-clause-form (language-name form)
    "The form that evaluates to clause FORM as data: its head checked now (at
macroexpansion), its arguments evaluated at run time, its sub-clauses
expanded in turn."
    (unless (and (consp form) (symbolp (first form)))
      (error 'malformed-clause :form form :expected "a list starting with the clause name"))
    (let* ((definition (find-clause language-name (first form)))
           (head (a:make-keyword (clause-key (first form))))
           (args (rest form)))
      (if (clause-definition-subclauses definition)
          (let ((n (clause-definition-required definition)))
            `(list ',head ,@(subseq args 0 (min n (length args)))
                   ,@(mapcar (lambda (sub) (expand-clause-form language-name sub))
                             (nthcdr n args))))
          `(list ',head ,@args)))))

(defmacro define-clause (language name lambda-list &body body)
  "Define the clause NAME of LANGUAGE. LAMBDA-LIST receives the clause's
arguments, already evaluated; if it ends in &CLAUSES VAR (after required
parameters only), the remaining forms are sub-clauses, bound to VAR as data,
for the body to run with APPLY-CLAUSES, usually inside WITH-SCOPE. BODY may
start with a docstring and a (:PUBLISHES fact...) form declaring every fact
it may set.

  (define-clause option expires (when &key from calendar)
    (:publishes :expiry)
    \"The expiry: a date, or a tenor resolved from FROM.\"
    (publish :expiry (resolve-expiry when :from from :calendar calendar)))"
  (let ((publishes '()) (documentation nil) (forms body))
    (loop while forms
          do (cond ((and (stringp (first forms)) (rest forms) (null documentation))
                    (setf documentation (pop forms)))
                   ((and (consp (first forms)) (eq (first (first forms)) :publishes))
                    (setf publishes (rest (pop forms))))
                   (t (return))))
    (multiple-value-bind (function-lambda-list required subclauses)
        (parse-clause-lambda-list lambda-list)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (register-clause ',language
                          (make-clause-definition ,(clause-key name) ',lambda-list ,required
                                                  ,subclauses ',publishes ,documentation
                                                  (lambda ,function-lambda-list ,@forms)))
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

(defun apply-clauses (forms)
  "Run clause FORMS (data) in order, in the current scope."
  (mapc #'apply-clause forms)
  (values))

(defun fact-suppliers (fact)
  "Descriptions of the clauses (and resolver) of the current language that
can publish FACT."
  (when *language*
    (let ((suppliers
            (loop for definition being the hash-values of (construction-language-clauses *language*)
                  when (member fact (clause-definition-publishes definition))
                    collect (format nil "(~(~A~))" (clause-definition-name definition)))))
      (when (and (member fact (construction-language-resolver-publishes *language*))
                 (construction-language-resolver-description *language*))
        (setf suppliers `(,@suppliers ,(construction-language-resolver-description *language*))))
      (sort suppliers #'string<))))
