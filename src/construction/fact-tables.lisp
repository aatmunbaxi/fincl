;;;; construction/fact-tables.lisp --- scoped fact tables
;;;;
;;;; While an object is built, clauses publish facts into a table. Each entry
;;;; is keyed by (scope . fact): the root scope is (), and a nested clause such
;;;; as (leg :pay ...) works in the scope (:PAY). A read falls back from a scope
;;;; to its parents, so a leg sees the swap's calendar unless it sets its own.
;;;;
;;;; Every entry records a priority. A higher priority replaces a lower one
;;;; whatever the clause order; two settings at the same priority conflict
;;;; (except defaults, which the language sets once):
;;;;
;;;;   :DEFAULT    language defaults
;;;;   :CONVENTION clauses applied from a convention bundle
;;;;   :EXPLICIT   clauses the user wrote
;;;;
;;;; Derived facts (rules.lisp) are recorded only where no entry exists.

(in-package #:fincl)

(defvar *language* nil "The construction language being built.")
(defvar *fact-table* nil "The fact table being built.")
(defvar *scope* '() "The current scope: a list of keys, outermost first.")
(defvar *provenance* nil "The provenance recorded for facts published now.")
(defvar *priority* :explicit "The priority of facts published now.")
(defvar *publishable* :any
  "The facts the running clause declared in its :PUBLISHES, or :ANY.")

(declaim (ftype function derive-fact fact-suppliers))

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
  "Run BODY with a fresh fact table in the root scope. Construction
languages do this for every object they build."
  `(let ((*fact-table* (make-fact-table))
         (*language* ,language)
         (*scope* '())
         (*provenance* nil)
         (*priority* :explicit)
         (*publishable* :any))
     ,@body))

(defmacro with-scope ((key) &body body)
  "Run BODY one scope deeper, under KEY. Facts published in BODY belong to
the new scope; reads fall back to the enclosing scopes.

  (with-scope (:pay) (make-from-facts 'leg))"
  `(let ((*scope* `(,@*scope* ,,key)))
     ,@body))

(defun record-fact (fact value priority provenance)
  "Store VALUE for FACT in the current scope, resolving priorities."
  (let* ((key (cons *scope* fact))
         (entries (fact-table-entries *fact-table*))
         (old (gethash key entries))
         (new (make-fact-entry value priority provenance)))
    (cond ((or (null old)
               (> (priority-rank priority) (priority-rank (fact-entry-priority old)))
               (eq priority :default))
           (setf (gethash key entries) new))
          ((< (priority-rank priority) (priority-rank (fact-entry-priority old))))
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

(defun publish (fact value)
  "Set FACT to VALUE in the current scope, from the running clause. FACT must
be defined (DEFINE-FACT) and declared in the clause's :PUBLISHES.

  (publish :expiry #D\"2027-09-24\")"
  (unless (or (eq *publishable* :any) (member fact *publishable*))
    (error 'undeclared-fact :fact fact
                            :clause (and *provenance* (provenance-form *provenance*))))
  (record-fact fact (check-fact-value fact value *provenance*) *priority* *provenance*))

(defun find-fact-entry (fact)
  "The entry for FACT in the current scope or the nearest enclosing one."
  (let ((entries (fact-table-entries *fact-table*)))
    (loop for scope = *scope* then (butlast scope)
          for entry = (gethash (cons scope fact) entries)
          when entry return entry
          while scope)))

(defun fact (fact)
  "The value of FACT seen from the current scope, deriving it by rule if no
clause gave it: (values value t). If neither works, (values nil nil
missing-inputs), MISSING-INPUTS being the facts a rule lacked. Marks the
fact used.

  (fact :calendar) => #<WEEKENDS-ONLY>, T"
  (let ((entry (find-fact-entry fact)))
    (if entry
        (progn (setf (fact-entry-read entry) t)
               (values (fact-entry-value entry) t))
        (derive-fact fact))))

(defun fact-present-p (fact)
  "True if a clause or default gave FACT, seen from the current scope. Does
not derive and does not mark the fact used."
  (and (find-fact-entry fact) t))

(defun fact-provenance (fact)
  "Return (values provenance priority) for FACT as seen from the current
scope, or NIL."
  (let ((entry (find-fact-entry fact)))
    (and entry (values (fact-entry-provenance entry) (fact-entry-priority entry)))))

(defvar *cycles* nil
  "Facts found in a derivation cycle during the current REQUIRE-FACT.")

(defun require-fact (fact &key class slot)
  "The value of FACT, as FACT; signals MISSING-FACT (or FACT-RULE-CYCLE) if
no clause gave it and no rule could derive it. CLASS and SLOT name the
component that needed it, for the report."
  (let ((*cycles* '())
        (class (if (symbolp class) (and class (find-class class)) class)))
    (multiple-value-bind (value found missing) (fact fact)
      (cond (found value)
            (*cycles*
             (let ((facts (remove-duplicates *cycles*)))
               (error 'fact-rule-cycle :fact fact :scope *scope* :facts facts
                                       :class class :slot slot
                                       :suppliers (remove-duplicates
                                                   (loop for f in facts
                                                         append (fact-suppliers f))
                                                   :test #'string=))))
            (t (error 'missing-fact :fact fact :scope *scope*
                                    :class class :slot slot
                                    :missing-inputs missing
                                    :suppliers (fact-suppliers fact)))))))

(defun unused-fact-entries ()
  "(fact scope provenance) of explicit and convention facts nobody read."
  (loop for (scope . fact) being the hash-keys of (fact-table-entries *fact-table*)
          using (hash-value entry)
        when (and (not (fact-entry-read entry))
                  (member (fact-entry-priority entry) '(:explicit :convention)))
          collect `(,fact ,scope ,(fact-entry-provenance entry))))
