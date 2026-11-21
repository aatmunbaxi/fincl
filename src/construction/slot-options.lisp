;;;; construction/slot-options.lisp --- custom slot options with inheritance
;;;;
;;;; A slot of an ANNOTATED-CLASS can carry registered slot options ("annotations")
;;;; beside the standard options:
;;;;
;;;;   (defclass c () ((x :initarg :x :fact :strike)) (:metaclass component-class))
;;;;
;;;; Annotations are inherited slot by slot. The most specific class that gave an option
;;;; wins. Thus a subclass can override one option of an inherited slot and keep the
;;;; other options. Process parameters (:PARAMETER, :BOUNDS, :MARKET-DEFAULT) and
;;;; construction facts (:FACT) are both annotations.

(in-package #:fincl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slot-options* '()
    "Keywords registered by DEFINE-SLOT-OPTION.")

  (defvar *slot-option-documentation* (make-hash-table :test 'eq)
    "Slot option keyword -> its documentation string.")

  (defun register-slot-option (keyword documentation)
    (pushnew keyword *slot-options*)
    (setf (gethash keyword *slot-option-documentation*) documentation)
    keyword))

(defmacro define-slot-option (keyword &optional documentation)
  "Register KEYWORD as a slot option that slots of ANNOTATED-CLASS accept and
inherit. DOCUMENTATION is shown by DESCRIBE for a slot that has the option. A slot
of an ANNOTATED-CLASS that has an option that is neither registered nor standard
(:INITARG, :READER, :TYPE, ...) signals INVALID-ARGUMENT. Define an option before
the first class that uses it.

  (define-slot-option :fact \"The construction fact that fills the slot.\")"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-slot-option ,keyword ,documentation)))

(declaim (ftype (function (t) (values (or null string) &optional)) slot-option-documentation))
(defun slot-option-documentation (keyword)
  "The documentation DEFINE-SLOT-OPTION gave KEYWORD, or NIL."
  (values (gethash keyword *slot-option-documentation*)))

(defclass annotated-class (standard-class) ()
  (:documentation "A metaclass whose slots accept registered slot options (see DEFINE-SLOT-OPTION). The
slots inherit them option by option."))

;;; Annotated and standard classes may inherit from each other. A plain
;;; DEFCLASS subclass keeps no annotations of its own: its effective slots
;;; are standard ones.
(defmethod c2mop:validate-superclass ((class annotated-class) (super standard-class)) t)
(defmethod c2mop:validate-superclass ((class standard-class) (super annotated-class)) t)
(defmethod c2mop:validate-superclass ((class annotated-class) (super annotated-class)) t)

(defclass annotated-slot-definition ()
  ((annotations :initform '() :accessor slot-annotations
                :documentation "A plist of registered slot options.")))

(defclass annotated-direct-slot-definition
    (annotated-slot-definition c2mop:standard-direct-slot-definition) ())

(defclass annotated-effective-slot-definition
    (annotated-slot-definition c2mop:standard-effective-slot-definition) ())

;;; The options a plain slot accepts are the initargs of a standard direct slot
;;; definition (:INITARGS, :READERS, :TYPE, ...: DEFCLASS turns :INITARG and
;;; :READER into these), read from the MOP so that they follow the implementation.
(defvar *standard-slot-initargs* nil
  "The initargs STANDARD-DIRECT-SLOT-DEFINITION accepts, once computed.")

(defun standard-slot-initargs ()
  "The initargs a standard direct slot definition accepts, found through the
MOP over its class precedence list."
  (or *standard-slot-initargs*
      (setf *standard-slot-initargs*
            (let ((class (find-class 'c2mop:standard-direct-slot-definition)))
              (c2mop:ensure-finalized class)
              (remove-duplicates
               (iter (for super in (c2mop:class-precedence-list class))
                     (appending (iter (for slot in (c2mop:class-direct-slots super))
                                      (appending (c2mop:slot-definition-initargs slot))))))))))

(defun check-slot-options (slot-name initargs)
  "Signal INVALID-ARGUMENT unless every key of the slot initargs INITARGS
(a plist) is a registered slot option or an initarg of a standard slot."
  (iter (for (key nil) in-plist initargs)
        (unless (or (member key *slot-options*) (member key (standard-slot-initargs)))
          (signal-invalid-argument
           (format nil "option ~S of slot ~(~A~)" key slot-name) key 'keyword
           (format nil "a registered slot option (~{~S~^ ~}) or a standard one (~{~S~^ ~}; ~
DEFCLASS's :INITARG, :READER and :WRITER arrive as :INITARGS, :READERS and :WRITERS). ~
Register another with DEFINE-SLOT-OPTION before the class that uses it"
                   (sort (copy-list *slot-options*) #'string<)
                   (sort (remove :name (copy-list (standard-slot-initargs))) #'string<))))))

;;; &ALLOW-OTHER-KEYS lets DEFCLASS pass every option on; CHECK-SLOT-OPTIONS
;;; rejects the unknown ones and only the registered ones are kept.
(defmethod shared-initialize :after ((slot annotated-direct-slot-definition) slot-names
                                     &rest initargs &key name &allow-other-keys)
  (declare (ignore slot-names))
  (check-slot-options name initargs)
  (setf (slot-annotations slot)
        (iter (for (key value) in-plist initargs)
              (when (member key *slot-options*)
                (nconcing `(,key ,value))))))

(defmethod describe-object :after ((slot annotated-slot-definition) stream)
  (iter (for (option value) in-plist (slot-annotations slot))
        (format stream "~&  Slot option ~S ~S~@[: ~A~]~%"
                option value (slot-option-documentation option))))

(defmethod c2mop:direct-slot-definition-class ((class annotated-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'annotated-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class annotated-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'annotated-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class annotated-class) name direct-slots)
  (declare (ignore name))
  (let ((effective (call-next-method))
        (annotations '()))
    ;; DIRECT-SLOTS is most specific first. Walk it least specific first, so
    ;; that more specific classes overwrite.
    (dolist (direct (reverse direct-slots))
      (when (typep direct 'annotated-slot-definition)
        (iter (for (key value) in-plist (slot-annotations direct))
              (setf (getf annotations key) value))))
    (setf (slot-annotations effective) annotations)
    effective))

(defun slot-annotation (slot option &optional default)
  "The value of slot option OPTION on SLOT (a slot definition), or DEFAULT if
no class in its chain gave it.

  (slot-annotation slot :fact) => :STRIKE"
  (if (typep slot 'annotated-slot-definition)
      (getf (slot-annotations slot) option default)
      default))

(defun annotated-slots (class option)
  "The effective slots of CLASS that carry OPTION."
  (let ((class (if (symbolp class) (find-class class) class)))
    (c2mop:ensure-finalized class)
    (remove-if (lambda (slot) (eq (slot-annotation slot option :absent) :absent))
               (c2mop:class-slots class))))
