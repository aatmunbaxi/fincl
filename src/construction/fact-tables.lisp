;;;; construction/fact-tables.lisp --- scoped fact tables
;;;;
;;;; While the library builds an object, clauses publish facts into a table. Each entry
;;;; has the key (scope . fact). The root scope is (). A nested clause such as (leg :pay
;;;; ...) works in the scope (:PAY). A read falls back from a scope to its parents. Thus
;;;; a leg sees the calendar of the swap unless it sets its own.
;;;;
;;;; Every entry records a priority. A higher priority replaces a lower priority, in any
;;;; clause order. A lower priority never replaces a higher priority. Two settings at the
;;;; same priority conflict. The exception is defaults: the last default wins.
;;;;
;;;;   :DEFAULT    language defaults
;;;;   :CONVENTION clauses that a convention bundle applies
;;;;   :EXPLICIT   clauses that the user wrote
;;;;
;;;; The library records derived facts (rules.lisp) only where no entry exists.

(in-package #:fincl)

(defvar *language* nil "The construction language being built.")
(defvar *fact-table* nil "The fact table being built.")
(defvar *scope* '() "The current scope: a list of keys, outermost first.")
(defvar *provenance* nil "The provenance recorded for facts published now.")
(defvar *priority* :explicit "The priority of facts published now.")
(defvar *publishable* :any
  "The facts the running clause declared in its :PUBLISHES, or :ANY.")

(declaim (ftype function derive-fact fact-suppliers)) ; rules.lisp and clauses.lisp, loaded after this file

(defstruct (fact-entry (:constructor make-fact-entry (value priority provenance)))
  value
  (priority :explicit :type (member :default :derived :convention :explicit))
  provenance
  (read nil))

(defstruct (fact-table (:constructor make-fact-table ()))
  (entries (make-hash-table :test 'equal) :read-only t))

(defun priority-rank (priority)
  (ecase priority
    ((:default :derived) 0)
    (:convention 1)
    (:explicit 2)))

(defmacro with-fact-table ((&key language) &body body)
  "Run BODY with a new fact table in the root scope. Construction languages do this
for every object that they build."
  `(let ((*fact-table* (make-fact-table))
         (*language* ,language)
         (*scope* '())
         (*provenance* nil)
         (*priority* :explicit)
         (*publishable* :any))
     ,@body))

(defmacro with-scope ((key) &body body)
  "Run BODY one scope deeper, under KEY. The facts that BODY publishes belong to the
new scope. Reads fall back to the enclosing scopes.

  (with-scope (:pay) (make-from-facts 'leg))"
  `(let ((*scope* `(,@*scope* ,,key)))
     ,@body))

(defmacro in-scope ((scope) &body body)
  "Run BODY in SCOPE, a scope as SCOPES-WITH-FACT returns it, with reads
falling back to its enclosing scopes."
  `(let ((*scope* ,scope))
     ,@body))

(defun scopes-with-fact (fact)
  "The scopes holding an entry for FACT, in no specified order.
A language with a variable number of parts (one scope per part) lists them by
a fact each part's clause publishes.

  (scopes-with-fact :underlying-name) => (((:underlying :spx)) ((:underlying :aapl)))"
  (iter (for ((scope . entry-fact) nil) in-hashtable (fact-table-entries *fact-table*))
        (when (eq entry-fact fact) (collect scope))))

(defun record-fact (fact value priority provenance)
  "Store VALUE for FACT in the current scope, resolving priorities: a higher
priority replaces a lower one and a lower never replaces a higher; a default
replaces an earlier default of equal rank; other equal priorities conflict."
  (let* ((key (cons *scope* fact))
         (entries (fact-table-entries *fact-table*))
         (old (gethash key entries))
         (new (make-fact-entry value priority provenance)))
    (cond ((null old)
           (setf (gethash key entries) new))
          ((> (priority-rank priority) (priority-rank (fact-entry-priority old)))
           (setf (gethash key entries) new))
          ((< (priority-rank priority) (priority-rank (fact-entry-priority old))))
          ((eq priority :default)
           (setf (gethash key entries) new))
          (t (error 'conflicting-clauses :fact fact :scope *scope*
                                         :earlier (fact-entry-provenance old)
                                         :later provenance)))
    value))

(defun check-fact-value (fact value provenance)
  "VALUE coerced and type-checked against FACT's definition."
  (let* ((definition (find-fact-definition fact))
         (coerce (fact-definition-coerce definition))
         (value (if coerce (funcall coerce value) value)))
    (unless (typep value (fact-definition-type definition))
      (error 'fact-type-error :fact fact :value value :type (fact-definition-type definition)
                              :provenance provenance))
    value))

(declaim (ftype (function (t t) (values t &optional)) publish))
(defun publish (fact value)
  "Set FACT to VALUE in the current scope, from the running clause. DEFINE-FACT must
define FACT. The :PUBLISHES form of the clause must declare it.

  (publish :expiry #D\"2027-09-24\")"
  (unless (or (eq *publishable* :any) (member fact *publishable*))
    (error 'undeclared-fact :fact fact
                            :clause (and *provenance* (provenance-form *provenance*))))
  (record-fact fact (check-fact-value fact value *provenance*) *priority* *provenance*))

(defun find-fact-entry (fact)
  "The entry for FACT in the current scope or the nearest enclosing one."
  (let ((entries (fact-table-entries *fact-table*)))
    (iter (for scope first *scope* then (butlast scope))
          (for entry = (gethash (cons scope fact) entries))
          (when entry (return entry))
          (while scope))))

(declaim (ftype (function (t) (values boolean &optional)) fact-set-here-p))
(defun fact-set-here-p (fact)
  "True if a clause or convention gave FACT in exactly the current scope, not
in an enclosing one. Does not derive and does not mark the fact used."
  (and (gethash (cons *scope* fact) (fact-table-entries *fact-table*)) t))

(declaim (ftype (function (t) (values t &rest t)) fact))
(defun fact (fact)
  "Return the value of FACT as seen from the current scope. If no clause gave it, the
function derives it by rule. It returns (values value t). If neither works, it
returns (values nil nil missing-inputs). MISSING-INPUTS are the facts that a rule
lacked. The function marks the fact as used.

  (fact :calendar) => #<WEEKENDS-ONLY>, T"
  (let ((entry (find-fact-entry fact)))
    (if entry
        (progn (setf (fact-entry-read entry) t)
               (values (fact-entry-value entry) t))
        (derive-fact fact))))

(defun optional-fact (fact)
  "The value of FACT seen from the current scope, or NIL if no clause, default
or rule gave it. Marks the fact used."
  (values (fact fact)))

(defun consume-facts (facts)
  "Mark each of FACTS used in the current scope, so that a part a clause gave
but another clause superseded does not raise an UNUSED-FACT warning."
  (dolist (fact facts)
    (optional-fact fact)))

(declaim (ftype (function (t) (values boolean &optional)) fact-present-p))
(defun fact-present-p (fact)
  "True if a clause or a default gave FACT, as seen from the current scope. The function
does not derive the fact and does not mark it as used."
  (and (find-fact-entry fact) t))

(declaim (ftype (function (t) (values t &optional t)) fact-provenance))
(defun fact-provenance (fact)
  "Return (values provenance priority) for FACT as seen from the current scope, or NIL."
  (let ((entry (find-fact-entry fact)))
    (and entry (values (fact-entry-provenance entry) (fact-entry-priority entry)))))

(defvar *cycles* nil
  "Facts found in a derivation cycle during the current REQUIRE-FACT.")

(declaim (ftype (function (t &key (:class t) (:slot t)) (values t &optional)) require-fact))
(defun require-fact (fact &key class slot)
  "Return the value of FACT, as FACT does. If no clause gave it and no rule can derive
it, the function signals MISSING-FACT (or FACT-RULE-CYCLE). CLASS and SLOT name the
component that needed the fact, for the report."
  (let ((*cycles* '())
        (class (if (symbolp class) (and class (find-class class)) class)))
    (multiple-value-bind (value found missing) (fact fact)
      (cond (found value)
            (*cycles*
             (let ((facts (remove-duplicates *cycles*)))
               (error 'fact-rule-cycle :fact fact :scope *scope* :facts facts
                                       :class class :slot slot
                                       :suppliers (remove-duplicates
                                                   (iter (for f in facts)
                                                         (appending (fact-suppliers f)))
                                                   :test #'string=))))
            (t (error 'missing-fact :fact fact :scope *scope*
                                    :class class :slot slot
                                    :missing-inputs missing
                                    :suppliers (fact-suppliers fact)))))))

(defun unused-fact-entries ()
  "(fact scope provenance) of explicit and convention facts nobody read."
  (iter (for ((scope . fact) entry) in-hashtable (fact-table-entries *fact-table*))
        (when (and (not (fact-entry-read entry))
                   (member (fact-entry-priority entry) '(:explicit :convention)))
          (collect `(,fact ,scope ,(fact-entry-provenance entry))))))
