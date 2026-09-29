;;;; processes/parameters.lisp --- model parameters as slot metadata
;;;;
;;;; A slot of a PARAMETERIZED-CLASS may be declared a model parameter, with
;;;; bounds and a market default, as slot options:
;;;;
;;;;   (kappa :initarg :kappa :initform 2d0 :parameter t :bounds ((0) nil))
;;;;
;;;; The metaobjects carry this metadata, so code that needs the parameters
;;;; (calibration, printing, validation, Greeks by parameter) enumerates them
;;;; through the MOP instead of keeping its own list. DEFINE-PROCESS writes
;;;; the slot options; this file only interprets them.
;;;;
;;;; Bounds follow CL's interval designators, as in (REAL LO HI): a real is
;;;; an inclusive bound, a one-element list (X) an exclusive one, NIL no
;;;; bound. ((0) NIL) is "positive", (-1 1) is "in [-1, 1]".

(in-package #:fincl)

(define-condition invalid-parameter (pricing-error)
  ((object :initarg :object :reader invalid-parameter-object)
   (name :initarg :name :reader invalid-parameter-name)
   (value :initarg :value :reader invalid-parameter-value)
   (bounds :initarg :bounds :initform nil :reader invalid-parameter-bounds))
  (:report
   (lambda (c s)
     (let ((bounds (invalid-parameter-bounds c)))
       (format s "~A ~(~A~) = ~S is ~:[not a real number~;outside ~A~]."
               (class-name (class-of (invalid-parameter-object c)))
               (invalid-parameter-name c) (invalid-parameter-value c)
               (and bounds (realp (invalid-parameter-value c)))
               (and bounds (format-bounds bounds)))))))

(defun format-bounds (bounds)
  "Interval notation for (LO HI) designators: ((0) NIL) => \"(0, inf)\"."
  (destructuring-bind (lo hi) bounds
    (format nil "~:[[~A~;(~A~], ~:[~A]~;~A)~]"
            (or (null lo) (consp lo)) (cond ((null lo) "-inf") ((consp lo) (first lo)) (t lo))
            (or (null hi) (consp hi)) (cond ((null hi) "inf") ((consp hi) (first hi)) (t hi)))))

(defun within-bounds-p (x bounds)
  (destructuring-bind (lo hi) bounds
    (typep x `(real ,(or lo '*) ,(or hi '*)))))

;;; --------------------------------------------------------------------
;;; Metaobjects
;;; --------------------------------------------------------------------

(defclass parameterized-class (standard-class) ()
  (:documentation "Metaclass whose slots accept the options :PARAMETER,
:BOUNDS and :MARKET-DEFAULT. Use DEFINE-PROCESS rather than writing the
DEFCLASS by hand."))

(defmethod c2mop:validate-superclass ((class parameterized-class) (super standard-class)) t)
;; A plain DEFCLASS subclass of a process is allowed but keeps no parameter
;; metadata of its own; define subclasses with DEFINE-PROCESS instead.
(defmethod c2mop:validate-superclass ((class standard-class) (super parameterized-class)) t)

(defclass parameter-slot-definition ()
  ;; :UNSPECIFIED on a direct slot means "inherit"; on an effective slot,
  ;; that no class in the chain said. A slot is managed (validated at
  ;; construction) when :PARAMETER was given, T or NIL.
  ((parameter :initarg :parameter :initform :unspecified :accessor slot-parameter)
   (bounds :initarg :bounds :initform :unspecified :accessor slot-bounds)
   (market-default :initarg :market-default :initform :unspecified
                   :accessor slot-market-default)))

(defclass parameter-direct-slot-definition
    (parameter-slot-definition c2mop:standard-direct-slot-definition) ())

(defclass parameter-effective-slot-definition
    (parameter-slot-definition c2mop:standard-effective-slot-definition) ())

(defmethod c2mop:direct-slot-definition-class ((class parameterized-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'parameter-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class parameterized-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'parameter-effective-slot-definition))

(defmethod c2mop:compute-effective-slot-definition ((class parameterized-class) name
                                                    direct-slots)
  (declare (ignore name))
  (let ((effective (call-next-method)))
    ;; DIRECT-SLOTS is most specific first: the first class that specified
    ;; an option wins, so a subclass can override its parent's bounds.
    (flet ((inherit (accessor)
             (dolist (d direct-slots :unspecified)
               (when (typep d 'parameter-direct-slot-definition)
                 (let ((v (funcall accessor d)))
                   (unless (eq v :unspecified) (return v)))))))
      (setf (slot-parameter effective) (inherit #'slot-parameter)
            (slot-bounds effective) (inherit #'slot-bounds)
            (slot-market-default effective) (inherit #'slot-market-default)))
    effective))

(defun managed-slots (class)
  "Effective slot definitions of CLASS declared through DEFINE-PROCESS."
  (c2mop:ensure-finalized class)
  (remove-if-not (lambda (s) (and (typep s 'parameter-effective-slot-definition)
                                  (not (eq (slot-parameter s) :unspecified))))
                 (c2mop:class-slots class)))

(defun parameter-slots (class)
  (remove-if-not (lambda (s) (eq (slot-parameter s) t)) (managed-slots class)))

(defun slot-bounds* (slot)
  (let ((b (slot-bounds slot))) (if (eq b :unspecified) '(nil nil) b)))

(defun slot-default-function (slot)
  (let ((f (slot-market-default slot))) (if (eq f :unspecified) nil f)))

;;; --------------------------------------------------------------------
;;; Validation at construction
;;; --------------------------------------------------------------------

(defclass parameterized () ()
  (:documentation "Mixin that validates managed slots at construction: a
numeric value is coerced to double-float and checked against its bounds; NIL
is allowed only for a slot with a market default."))

(defmethod initialize-instance :after ((object parameterized) &key)
  (dolist (slot (managed-slots (class-of object)))
    (let* ((name (c2mop:slot-definition-name slot))
           (value (slot-value object name))
           (bounds (slot-bounds* slot))
           (numeric (subtypep (c2mop:slot-definition-type slot) '(or null double-float))))
      (cond ((not numeric)
             (unless (typep value (c2mop:slot-definition-type slot))
               (error 'invalid-parameter :object object :name name :value value)))
            ((null value)
             (unless (slot-default-function slot)
               (error 'invalid-parameter :object object :name name :value value)))
            ((not (realp value))
             (error 'invalid-parameter :object object :name name :value value))
            ((not (within-bounds-p value bounds))
             (error 'invalid-parameter :object object :name name :value value
                                       :bounds bounds))
            (t (setf (slot-value object name) (float value 1d0)))))))

(defmethod print-object ((object parameterized) stream)
  (print-unreadable-object (object stream :type t)
    (let ((*read-default-float-format* 'double-float))
      (format stream "~{~(~A~)=~A~^ ~}"
              (loop for slot in (managed-slots (class-of object))
                    for name = (c2mop:slot-definition-name slot)
                    for value = (slot-value object name)
                    collect name
                    collect (if (and (null value) (slot-default-function slot))
                                "market"
                                value))))))

;;; --------------------------------------------------------------------
;;; Introspection
;;; --------------------------------------------------------------------

(defun class-designator (object-or-class)
  (etypecase object-or-class
    (symbol (find-class object-or-class))
    (class object-or-class)
    (standard-object (class-of object-or-class))))

(defun process-parameters (process-or-class)
  "Names of the model parameters of a process, or a process class name, in
declaration order. Settings declared with :PARAMETER NIL (a GARCH period)
are excluded.

  (process-parameters 'heston) => (V0 KAPPA THETA XI RHO)"
  (mapcar #'c2mop:slot-definition-name (parameter-slots (class-designator process-or-class))))

(defun find-managed-slot (object-or-class name)
  "The managed slot called NAME, matched by symbol name, so :RHO, 'RHO and
FINCL::RHO all find the same slot."
  (let ((slots (managed-slots (class-designator object-or-class))))
    (or (find (string name) slots
              :key (lambda (s) (symbol-name (c2mop:slot-definition-name s)))
              :test #'string=)
        (error "~A has no parameter ~S. Its parameters and settings are ~{~(:~A~)~^, ~}."
               (class-name (class-designator object-or-class)) name
               (mapcar #'c2mop:slot-definition-name slots)))))

(defun process-parameter (process name &optional market (horizon 1d0))
  "The value of parameter or setting NAME of PROCESS. NAME is matched by
symbol name, so a keyword works from any package. A NIL value means the
parameter comes from the market. With MARKET, that default is resolved on
MARKET at HORIZON (market time); without it, NIL is returned.

  (process-parameter (make-instance 'heston :rho -0.5d0) :rho) => -0.5d0
  (process-parameter (make-instance 'heston) :v0)            => NIL
  (process-parameter (make-instance 'heston) :v0 market)     => 0.04d0"
  (let* ((slot (find-managed-slot process name))
         (value (slot-value process (c2mop:slot-definition-name slot)))
         (default (slot-default-function slot)))
    (if (and (null value) market default)
        (float (funcall default process market horizon) 1d0)
        value)))

(defun parameter-bounds (process-or-class name)
  "Return (values lo hi), the bounds of parameter NAME as interval
designators: a real is inclusive, (x) exclusive, NIL unbounded. NAME is
matched by symbol name.

  (parameter-bounds 'heston :rho) => -1, 1"
  (values-list (slot-bounds* (find-managed-slot process-or-class name))))

(defun parameter-vector (process market &optional (horizon 1d0))
  "The model parameters of PROCESS as a (SIMPLE-ARRAY DOUBLE-FLOAT (*)), in
the order of PROCESS-PARAMETERS. A NIL parameter is replaced by its market
default, evaluated on MARKET at HORIZON (market time).

  (parameter-vector (make-instance 'heston) m) => #(0.04d0 2d0 0.04d0 0.3d0 -0.7d0)"
  (let* ((slots (parameter-slots (class-of process)))
         (v (make-array (length slots) :element-type 'double-float)))
    (loop for slot in slots
          for i from 0
          for value = (slot-value process (c2mop:slot-definition-name slot))
          do (setf (aref v i)
                   (float (or value (funcall (slot-default-function slot)
                                             process market horizon))
                          1d0)))
    v))

(defun with-parameter-vector (process vector)
  "A new process of PROCESS's class with its model parameters taken from
VECTOR (in the order of PROCESS-PARAMETERS) and every other slot copied.
PROCESS is not modified. The new process is validated like any other.

  (with-parameter-vector heston (parameter-vector heston m))"
  (let* ((class (class-of process))
         (parameters (process-parameters class))
         (initargs '()))
    (assert (= (length vector) (length parameters)) ()
            "~S has ~D parameters, but the vector has ~D elements."
            process (length parameters) (length vector))
    (dolist (slot (c2mop:class-slots class))
      (let ((name (c2mop:slot-definition-name slot))
            (initarg (first (c2mop:slot-definition-initargs slot))))
        (when (and initarg (slot-boundp process name))
          (let ((position (position name parameters)))
            (push initarg initargs)
            (push (if position (aref vector position) (slot-value process name))
                  initargs)))))
    (apply #'make-instance class (nreverse initargs))))
