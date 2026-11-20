;;;; market-components.lisp --- the kinds of data a composite market holds
;;;;
;;;; A kind of market component (spots, vol structures, yield curves, an FX
;;;; rate) is registered once with DEFINE-MARKET-COMPONENT: its value type,
;;;; where the market stores it, how a missing entry is reported, how BUMP and
;;;; simulation reach it, its market-language clause, and how it prints and
;;;; snapshots. MARKET-COMPONENT, BUMP, SET-MARKET-COMPONENT, the market
;;;; language, printing, snapshots and simulation targets read the registry,
;;;; so a new kind is one form. The kinds built into fincl keep their own
;;;; slots of COMPOSITE-MARKET; every other kind lives in the market's extra
;;;; table.
;;;;
;;;; Clause names and facts are recorded when a definition is compiled as
;;;; well as when it is loaded, so a MAKE-MARKET form later in the same file
;;;; can use the new clause. Functions in a definition are function
;;;; designators evaluated at load time.

(in-package #:fincl)

(defstruct (market-component-kind (:constructor make-market-component-kind))
  (name nil :type keyword :read-only t)
  (type t :read-only t)
  (description "" :type string :read-only t)
  (what "" :type string :read-only t)
  (storage nil :read-only t)            ; a reader of the market's own table, or NIL
  (known nil :read-only t)              ; NIL (the sorted keys) or a function of the market
  (coerce nil :read-only t)             ; NIL or a function of a value of TYPE
  (input nil :read-only t)              ; NIL or a function of a clause's argument
  (view nil :read-only t)               ; NIL or a function of (market key)
  (bumpable t :read-only t)
  (group nil :read-only t)              ; the group's name (a string) or NIL
  (clause nil :type symbol :read-only t)
  (key-fact nil :read-only t)
  (fact nil :read-only t)
  (required nil :read-only t)
  (parts '() :read-only t)              ; ((name lambda-list documentation) ...)
  (entry nil :read-only t)              ; NIL or a function of the part form
  (spec nil :read-only t)               ; (argument function) for MAKE-MARKET-FROM-SPECS
  (print nil :read-only t)              ; NIL or a function of the value -> clause arguments
  (snapshot-constructors '() :read-only t)
  (snapshot-classes '() :read-only t)
  (sealed nil :read-only t)
  (serial 0 :type fixnum))

(defstruct (market-component-group (:constructor make-market-component-group (name clause key-fact)))
  (name "" :type string :read-only t)
  (clause nil :type symbol :read-only t)
  (key-fact nil :type keyword :read-only t)
  (members '()))                        ; ((kind . clause-name) ...), in definition order

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *market-component-kinds* (make-hash-table :test 'eq)
    "Kind keyword -> MARKET-COMPONENT-KIND.")

  (defvar *market-component-declarations* (make-hash-table :test 'eq)
    "Kind keyword -> (package type sealed), recorded at compile and load time.")

  (defvar *market-component-owners* (make-hash-table :test 'equal)
    "A clause name (string) or a fact (keyword) -> the kind or group (a string) that owns it.")

  (defvar *market-component-groups* (make-hash-table :test 'equal)
    "Group name (string) -> MARKET-COMPONENT-GROUP.")

  (defvar *market-component-serial* 0
    "Counter giving each kind its place in definition order.")

  (defun market-clause-symbol (name)
    "The symbol of the market-language clause NAME (a string), or NIL."
    (a:when-let* ((language (gethash 'market *construction-languages*))
                  (definition (registered-clause language name)))
      (clause-definition-symbol definition)))

  (defun claim-market-component-names (owner clauses facts)
    "Record OWNER (a kind or a group name) as the owner of CLAUSES, ((name .
symbol) ...), and FACTS, ((fact . type) ...). Signal INVALID-ARGUMENT for a
name another owner holds, a market clause no kind defined, or a fact defined
outside the registry with another type."
    (flet ((refuse (what name holder)
             (signal-invalid-argument 'name name 'string
                                      (format nil "a ~A not already used: ~S belongs to ~S" what name holder))))
      (iter (for (name . symbol) in clauses)
            (for holder = (gethash name *market-component-owners*))
            (cond ((and holder (not (equal holder owner))) (refuse "clause name" name holder))
                  ((and (null holder) (market-clause-symbol name)
                        (not (eq symbol (market-clause-symbol name))))
                   (refuse "clause name" name "the market language"))))
      (iter (for (fact . type) in facts)
            (for holder = (gethash fact *market-component-owners*))
            (cond ((and holder (not (equal holder owner))) (refuse "fact" fact holder))
                  ((and (null holder) (gethash fact *facts*)
                        (not (equal type (fact-definition-type (gethash fact *facts*)))))
                   (refuse "fact" fact "a fact defined outside the market components"))))
      (dolist (entry `(,@clauses ,@facts))
        (setf (gethash (car entry) *market-component-owners*) owner))))

  (defun declare-market-component-group (name clause key-fact)
    "Record the group NAME (a string), its clause symbol and key fact."
    (claim-market-component-names name `((,(symbol-name clause) . ,clause)) `((,key-fact . keyword)))
    (let ((old (gethash name *market-component-groups*)))
      (setf (gethash name *market-component-groups*)
            (let ((group (make-market-component-group name clause key-fact)))
              (when old
                (setf (market-component-group-members group) (market-component-group-members old)))
              group))))

  (defun declare-market-component (kind type package sealed group clauses facts)
    "Record the names of KIND, defined in PACKAGE (a name) with values of TYPE:
its clauses ((name . symbol) ...) and facts ((fact . type) ...), and its
group (a string) if its clause is a sub-clause of one. Signals
INVALID-ARGUMENT when KIND is sealed and PACKAGE is not the one that sealed it,
and warns with MARKET-COMPONENT-REDEFINED when another package defined it."
    (destructuring-bind (&optional old-package old-type old-sealed)
        (gethash kind *market-component-declarations*)
      (when (and old-package (string/= old-package package))
        (if old-sealed
            (signal-invalid-argument 'kind kind 'keyword
                                     (format nil "a kind not built into fincl: ~S is sealed (defined in ~A)"
                                             kind old-package))
            (warn 'market-component-redefined :kind kind :old-package old-package :new-package package
                                              :old-type old-type :new-type type))))
    (when group
      (unless (gethash group *market-component-groups*)
        (signal-invalid-argument 'within group 'symbol
                                 (format nil "a market component group: ~{~(~A~)~^, ~}"
                                         (a:hash-table-keys *market-component-groups*)))))
    (claim-market-component-names kind clauses facts)
    (setf (gethash kind *market-component-declarations*) `(,package ,type ,sealed))
    (when group
      (let ((g (gethash group *market-component-groups*)))
        (unless (assoc kind (market-component-group-members g))
          (setf (market-component-group-members g)
                `(,@(market-component-group-members g) (,kind . ,(car (first clauses))))))))
    kind)

  (defun market-group-member-names (group)
    "The clause names (strings) of the members of the group GROUP, a string."
    (mapcar #'cdr (market-component-group-members (gethash group *market-component-groups*)))))

(define-condition market-component-redefined (warning)
  ((kind :initarg :kind :reader error-kind)
   (old-package :initarg :old-package :reader error-old-package)
   (new-package :initarg :new-package :reader error-new-package)
   (old-type :initarg :old-type :reader error-old-type)
   (new-type :initarg :new-type :reader error-new-type))
  (:report (lambda (c s)
             (format s "The market component kind ~S, defined in package ~A with values of type ~S, ~
is replaced by a definition in package ~A with values of type ~S. To replace it on purpose, ~
muffle this warning with HANDLER-BIND."
                     (error-kind c) (error-old-package c) (error-old-type c)
                     (error-new-package c) (error-new-type c))))
  (:documentation "A market component kind was defined again from a package
other than the one that defined it."))

(defvar *market-component-order* '()
  "Every registered kind, in definition order.")

(defvar *target-kinds* '()
  "The names of the bumpable kinds, in definition order.")

(defun register-market-component (kind)
  "Store KIND, a MARKET-COMPONENT-KIND, keeping the place in definition order
of an earlier definition of the same name."
  (let ((old (gethash (market-component-kind-name kind) *market-component-kinds*)))
    (setf (market-component-kind-serial kind)
          (if old (market-component-kind-serial old) (incf *market-component-serial*))
          (gethash (market-component-kind-name kind) *market-component-kinds*) kind)
    (let ((order (sort (a:hash-table-values *market-component-kinds*) #'<
                       :key #'market-component-kind-serial)))
      (setf *market-component-order* order
            *target-kinds* (iter (for k in order)
                                 (when (market-component-kind-bumpable k)
                                   (collect (market-component-kind-name k))))))
    (market-component-kind-name kind)))

(defun market-component-kinds ()
  "Every registered kind, in definition order."
  *market-component-order*)

(defun find-market-component-kind (kind)
  "The registered MARKET-COMPONENT-KIND named KIND; signals INVALID-ARGUMENT
listing the registered kinds."
  (or (and (keywordp kind) (gethash kind *market-component-kinds*))
      (signal-invalid-argument 'kind kind 'keyword
                               (format nil "a registered market component kind: ~{~S~^, ~}"
                                       (mapcar #'market-component-kind-name (market-component-kinds))))))

(defun target-kinds ()
  "The kinds a target or a BUMP factor may name, in definition order."
  *target-kinds*)

(defun kind-table (market kind &optional create)
  "The table of KIND (a MARKET-COMPONENT-KIND) in MARKET, or NIL if MARKET holds
no entry of an extra kind. CREATE makes the extra table, in a market not yet
shared."
  (let ((reader (market-component-kind-storage kind)))
    (if reader
        (funcall reader market)
        (let ((extra (market-extra-components market))
              (name (market-component-kind-name kind)))
          (or (and extra (gethash name extra))
              (when create
                (setf (gethash name (or extra
                                        (setf (slot-value market 'extra-components)
                                              (make-hash-table :test 'equal))))
                      (make-hash-table :test 'equal))))))))

(defun kind-known (market kind table)
  "The keys a report of a missing entry of KIND lists."
  (let ((known (market-component-kind-known kind)))
    (cond (known (funcall known market))
          (table (sorted-keys table))
          (t '()))))

(defun kind-entry (market kind key)
  "The value of KEY of KIND in MARKET; signals MISSING-MARKET-DATA listing the keys it has."
  (let ((table (kind-table market kind)))
    (multiple-value-bind (value found) (if table (gethash key table) (values nil nil))
      (if found
          value
          (error 'missing-market-data :what (market-component-kind-what kind) :key key
                                      :known (kind-known market kind table))))))

(defun kind-value (kind key value)
  "VALUE, an argument of KIND's clause for KEY, as the market stores it:
through the kind's input function, checked against its type, coerced."
  (let ((value (a:if-let ((input (market-component-kind-input kind))) (funcall input value) value)))
    (unless (typep value (market-component-kind-type kind))
      (signal-invalid-argument (market-component-kind-name kind) value (market-component-kind-type kind)
                               (format nil "~A for ~S" (market-component-kind-description kind) key)))
    (a:if-let ((coerce (market-component-kind-coerce kind))) (funcall coerce value) value)))

(defun components-table (components)
  "The extra table of a market from COMPONENTS, ((kind (key value) ...) ...),
or NIL when there are none."
  (check-input components list "a list ((kind (key value) ...) ...)")
  (when components
    (let ((extra (make-hash-table :test 'equal)))
      (dolist (entry components extra)
        (unless (and (consp entry) (listp (rest entry)))
          (signal-invalid-argument 'components entry 'list "an entry (kind (key value) ...)"))
        (let ((kind (find-market-component-kind (first entry))))
          (when (market-component-kind-storage kind)
            (signal-invalid-argument 'components entry 'list
                                     (format nil "entries of kinds the market keyword arguments do not ~
take; give ~S by its own argument or clause" (first entry))))
          (let ((table (a:ensure-gethash (first entry) extra (make-hash-table :test 'equal))))
            (dolist (pair (rest entry))
              (unless (and (consp pair) (keywordp (first pair)) (consp (rest pair)) (null (cddr pair)))
                (signal-invalid-argument 'components pair 'list
                                         (format nil "an entry (key value) of ~S, KEY a keyword" (first entry))))
              (setf (gethash (first pair) table) (kind-value kind (first pair) (second pair))))))))))

(defun bump-factor-p (factor)
  "True if FACTOR is a factor BUMP takes."
  (or (and (consp factor) (consp (rest factor)) (null (cddr factor)) (keywordp (second factor))
           (member (first factor) (target-kinds)))
      (typep factor '(cons (eql :dividend-schedule) (cons keyword (cons date null))))
      (typep factor '(cons (eql :quote) (cons keyword (cons (or integer tenor) null))))))

(defun bump-value (kind value shift)
  "VALUE of KIND moved by SHIFT: APPLY-SHIFT for a real, BUMP-STRUCTURE otherwise."
  (if (realp value)
      (apply-shift value shift)
      (handler-case (bump-structure value shift)
        (no-bump-structure ()
          (signal-invalid-argument (market-component-kind-name kind) value 'term-structure
                                   (format nil "a value of ~S with a BUMP-STRUCTURE method (~(~A~) has none)"
                                           (market-component-kind-name kind)
                                           (class-name (class-of value))))))))

;;; --------------------------------------------------------------------
;;; Definitions
;;; --------------------------------------------------------------------

(defmacro define-market-component-group (name &key clause key-fact key-documentation documentation)
  "Define NAME, a group of kinds whose clauses are sub-clauses of one market
clause. CLAUSE is (clause-name (key &clauses var)); the clause publishes
KEY-FACT, a keyword fact, and runs its sub-clauses, each a member kind's
clause, in a scope of its own."
  (destructuring-bind (clause-name (key clauses-marker var)) clause
    (declare (ignore clauses-marker))
    (let ((group (symbol-name name)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (declare-market-component-group ,group ',clause-name ,key-fact))
         (define-fact ,key-fact keyword ,key-documentation)
         (define-clause market ,clause-name (,key &clauses ,var)
           (:publishes ,key-fact)
           (:subclause-names (lambda () (market-group-member-names ,group)))
           ,documentation
           (with-scope ((list ,(a:make-keyword (symbol-name clause-name)) ,key))
             (publish ,key-fact ,key)
             (apply-clauses ,var)))))))

(defmacro define-market-component (kind &key type description what storage known coerce input
                                          (input-type nil input-type-p) view (bumpable t)
                                          clause within documentation parts
                                          key-fact fact key-documentation fact-documentation
                                          required entry spec print
                                          snapshot-constructors snapshot-classes sealed)
  "Define KIND, a keyword naming a kind of data a composite market holds (an FX
rate, a hazard curve), so that MAKE-MARKET builds it, MARKET-COMPONENT and
BUMP reach it as (KIND key), and the market prints and snapshots it.

  TYPE, DESCRIPTION  the type of a stored value, and how a report names one
                     (\"a positive FX rate\").
  WHAT               the noun a MISSING-MARKET-DATA report uses (default the
                     kind's name).
  CLAUSE             (name (key value)): a top-level clause (name :eurusd
                     1.08d0), or (name (key &clauses var)) with PARTS.
  WITHIN             (group name (value)): a sub-clause of a group's clause,
                     keyed by the group's key, as (underlying :spx (spot 100)).
  PARTS              ((name lambda-list documentation) ...): the sub-clauses of
                     a CLAUSE with &CLAUSES; each publishes (:name arg ...) and
                     ENTRY, a function of that list, makes the value.
  DOCUMENTATION      the clause's documentation.
  INPUT, INPUT-TYPE  a function turning a clause's argument into a value (a
                     real into a flat structure), and the type it accepts
                     (default TYPE).
  COERCE             a function normalizing a value of TYPE before it is
                     stored (REAL-TO-DOUBLE for a real).
  BUMPABLE           true (the default) if (KIND key) is a BUMP factor and a
                     simulation target; BUMP moves a real by APPLY-SHIFT and
                     anything else by BUMP-STRUCTURE.
  VIEW               a function of (market key) returning the market a
                     simulated process steps on; NIL: the kind is not simulated.
  PRINT              a function of a value returning the clause's arguments
                     after the key (NIL: omit the clause); default the value.
  SNAPSHOT-CONSTRUCTORS, SNAPSHOT-CLASSES
                     the functions and classes the snapshot form of a value
                     calls, which READ-SNAPSHOT is then allowed to call.
  KEY-FACT, FACT     the facts the clause publishes (default :KIND-KEY and
                     :KIND), with KEY-DOCUMENTATION and FACT-DOCUMENTATION.
  REQUIRED           true if every member of a group needs the clause.
  STORAGE, KNOWN, SPEC, SEALED
                     for the kinds built into fincl: the reader of their table,
                     a function of the market listing keys for reports, how
                     the keyword shape of MAKE-MARKET takes them, and a seal
                     against redefinition from another package.

Other kinds live in the market's extra table. Function arguments are
function designators evaluated at load time; name a function as a quoted
symbol. A definition belongs to the package current when it is expanded.
Redefining a kind from another package warns with
MARKET-COMPONENT-REDEFINED; a clause name or fact another kind owns signals
INVALID-ARGUMENT.

  (define-market-component :fx
    :type (real (0)) :description \"a positive FX rate\"
    :clause (fx (pair rate))
    :documentation \"The FX rate of PAIR: units of the quote currency per unit of the base.\")
  (bump (make-market (valued-on d) (fx :eurusd 1.08d0)) '(:fx :eurusd) '(:relative 1/100))"
  (check-input kind keyword "a keyword naming the kind")
  (when (and clause within)
    (signal-invalid-argument 'within within 'null "either :CLAUSE or :WITHIN, not both"))
  (let* ((within-p (and within t))
         (group (and within (symbol-name (first within))))
         (clause-name (if within (second within) (first clause)))
         (lambda-list (if within (third within) (second clause)))
         (fact (or fact kind))
         (key-fact (and clause (or key-fact (a:format-symbol :keyword "~A-KEY" kind))))
         (input-type (if input-type-p input-type type))
         (fact-type (if parts 'list input-type))
         (package (package-name *package*))
         (clauses `(,@(when clause-name `((,(symbol-name clause-name) . ,clause-name)))
                    ,@(mapcar (lambda (part) `(,(symbol-name (first part)) . ,(first part))) parts)))
         (facts `(,@(when clause-name `((,fact . ,fact-type)))
                  ,@(when key-fact `((,key-fact . keyword))))))
    (cond (within-p
           (unless (and (= 1 (length lambda-list)) (symbolp (first lambda-list)))
             (signal-invalid-argument 'within within 'list "(group name (value)): one value")))
          (parts
           (unless (and (= 3 (length lambda-list)) (string= "&CLAUSES" (symbol-name (second lambda-list))))
             (signal-invalid-argument 'clause clause 'list "(name (key &clauses var)) for a kind with parts")))
          (clause
           (unless (and (= 2 (length lambda-list)) (every #'symbolp lambda-list)
                        (notany (lambda (p) (member p lambda-list-keywords)) lambda-list))
             (signal-invalid-argument 'clause clause 'list "(name (key value))"))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (declare-market-component ,kind ',type ,package ,sealed ,group ',clauses ',facts))
       ,@(when clause-name
           `((define-fact ,fact ,fact-type ,(or fact-documentation documentation))))
       ,@(when key-fact
           `((define-fact ,key-fact keyword ,(or key-documentation
                                                 (format nil "The key of a ~(~A~) entry of a market." kind)))))
       ,@(cond (within-p
                `((define-clause market ,clause-name ,lambda-list
                    (:publishes ,fact)
                    ,documentation
                    (publish ,fact ,(first lambda-list)))))
               (parts
                (destructuring-bind (key marker var) lambda-list
                  `((define-clause market ,clause-name (,key ,marker ,var)
                      (:publishes ,key-fact)
                      (:subclause-names (lambda () ',(mapcar (lambda (p) (symbol-name (first p))) parts)))
                      ,documentation
                      (with-scope ((list ,kind ,key))
                        (publish ,key-fact ,key)
                        (apply-clauses ,var)))
                    ,@(mapcar (lambda (part)
                                (destructuring-bind (name part-lambda-list &optional part-documentation) part
                                  `(define-clause market ,name ,part-lambda-list
                                     (:publishes ,fact)
                                     ,part-documentation
                                     (publish ,fact (list ,(a:make-keyword (symbol-name name))
                                                          ,@part-lambda-list)))))
                              parts))))
               (clause
                (destructuring-bind (key value) lambda-list
                  `((define-clause market ,clause-name (,key ,value)
                      (:publishes ,key-fact ,fact)
                      ,documentation
                      (with-scope ((list ,kind ,key))
                        (publish ,key-fact ,key)
                        (publish ,fact ,value)))))))
       (register-market-component
        (make-market-component-kind
         :name ,kind :type ',type :description ,(or description (format nil "a ~(~A~)" kind))
         :what ,(or what (string-downcase (symbol-name kind)))
         :storage ,storage :known ,known :coerce ,coerce :input ,input :view ,view
         :bumpable ,bumpable :group ,group :clause ',clause-name :key-fact ,key-fact :fact ,fact
         :required ,required :parts ',parts :entry ,entry :spec ,spec :print ,print
         :snapshot-constructors ',snapshot-constructors :snapshot-classes ',snapshot-classes
         :sealed ,sealed)))))
