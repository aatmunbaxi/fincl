;;;; construction/slot-options.lisp --- custom slot options with inheritance
;;;;
;;;; A slot of an ANNOTATED-CLASS may carry registered slot options
;;;; ("annotations") beside the standard ones:
;;;;
;;;;   (defclass c () ((x :initarg :x :fact :strike)) (:metaclass component-class))
;;;;
;;;; Annotations are inherited slot by slot: the most specific class that gave
;;;; an option wins, so a subclass can override one option of an inherited
;;;; slot and keep the rest. Process parameters (:PARAMETER, :BOUNDS,
;;;; :MARKET-DEFAULT) and construction facts (:FACT) are both annotations.

(in-package #:fincl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *slot-options* '()
    "Keywords registered by DEFINE-SLOT-OPTION."))

(defmacro define-slot-option (keyword &optional documentation)
  "Register KEYWORD as a slot option that ANNOTATED-CLASS slots accept and
inherit.

  (define-slot-option :fact \"The construction fact that fills the slot.\")"
  (declare (ignore documentation))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew ,keyword *slot-options*)
     ,keyword))

(defclass annotated-class (standard-class) ()
  (:documentation "Metaclass whose slots accept registered slot options (see
DEFINE-SLOT-OPTION) and inherit them option by option."))

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

;;; &ALLOW-OTHER-KEYS makes every slot option a valid initarg; only the
;;; registered ones are kept.
(defmethod shared-initialize :after ((slot annotated-direct-slot-definition) slot-names
                                     &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names))
  (setf (slot-annotations slot)
        (loop for (key value) on initargs by #'cddr
              when (member key *slot-options*)
                nconc `(,key ,value))))

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
        (loop for (key value) on (slot-annotations direct) by #'cddr
              do (setf (getf annotations key) value))))
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
