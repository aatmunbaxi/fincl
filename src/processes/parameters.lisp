;;;; processes/parameters.lisp --- model parameters as slot metadata
;;;;
;;;; A slot of a PARAMETERIZED-CLASS can be declared a model parameter, with bounds and a
;;;; market default, as slot options:
;;;;
;;;;   (kappa :initarg :kappa :initform 2d0 :parameter t :bounds ((0) nil))
;;;;
;;;; The metaobjects carry this metadata. Code that needs the parameters (calibration,
;;;; printing, validation, Greeks by parameter) enumerates them through the MOP. It does not
;;;; keep its own list. DEFINE-PROCESS writes the slot options. This file only interprets
;;;; them.
;;;;
;;;; Bounds follow the interval designators of CL, as in (REAL LO HI). A real is an
;;;; inclusive bound. A one-element list (X) is an exclusive bound. NIL is no bound. ((0)
;;;; NIL) means "positive". (-1 1) means "in [-1, 1]".

(in-package #:fincl)

(define-condition invalid-parameter (invalid-input)
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

(define-condition unknown-parameter (invalid-input)
  ((class :initarg :class :reader unknown-parameter-class)
   (name :initarg :name :reader unknown-parameter-name)
   (known :initarg :known :reader unknown-parameter-known))
  (:report (lambda (c s)
             (format s "~A has no parameter ~S. Its parameters and settings are ~
~{~(:~A~)~^, ~}."
                     (class-name (unknown-parameter-class c)) (unknown-parameter-name c)
                     (unknown-parameter-known c)))))

(defun format-bounds (bounds)
  "Interval notation for (LO HI) designators: ((0) NIL) => \"(0, inf)\"."
  (destructuring-bind (lo hi) bounds
    (format nil "~:[[~A~;(~A~], ~:[~A]~;~A)~]"
            (or (null lo) (consp lo)) (cond ((null lo) "-inf") ((consp lo) (first lo)) (t lo))
            (or (null hi) (consp hi)) (cond ((null hi) "inf") ((consp hi) (first hi)) (t hi)))))

(defun bounds-type (bounds)
  "The type specifier of the double-floats inside BOUNDS, (LO HI) in
interval-designator form: ((0) NIL) => (DOUBLE-FLOAT (0D0) *)."
  (flet ((bound (b)
           (cond ((null b) '*)
                 ((consp b) `(,(float (first b) 1d0)))
                 (t (float b 1d0)))))
    (destructuring-bind (lo hi) bounds
      `(double-float ,(bound lo) ,(bound hi)))))

(defun check-bounds-within-type (class slot bounds type)
  "Signal INVALID-ARGUMENT unless every double-float inside BOUNDS of parameter
SLOT of CLASS is of TYPE, a subtype of DOUBLE-FLOAT or (OR NULL that)."
  (let ((numeric (if (and (consp type) (eq (first type) 'or))
                     `(or ,@(remove 'null (rest type)))
                     type)))
    (unless (subtypep (bounds-type bounds) numeric)
      (signal-invalid-argument 'bounds bounds numeric
                               (format nil "bounds of ~A's parameter ~A inside its type ~S"
                                       class slot type)))))

(defun within-bounds-p (x bounds)
  (destructuring-bind (lo hi) bounds
    (typep x `(real ,(or lo '*) ,(or hi '*)))))

;;; --------------------------------------------------------------------
;;; Metaobjects
;;; --------------------------------------------------------------------

;;; Parameter metadata is three slot options on the general annotated-slot
;;; machinery (construction/slot-options.lisp), which also handles their
;;; inheritance: the most specific class that gave an option wins.
(define-slot-option :parameter "T for a model parameter, NIL for a setting.")
(define-slot-option :bounds "Interval designators (lo hi).")
(define-slot-option :market-default "Function (process market horizon) for NIL.")
(define-slot-option :measure-dependent "T for a parameter whose value differs between measures.")

(defclass parameterized-class (annotated-class) ()
  (:documentation "A metaclass whose slots accept the options :PARAMETER, :BOUNDS and :MARKET-DEFAULT. Use
DEFINE-PROCESS. Do not write the DEFCLASS by hand."))

;;; :UNSPECIFIED means that no class in the chain gave the option. A slot is
;;; managed (validated at construction) when :PARAMETER was given, T or NIL.
(defun slot-parameter (slot) (slot-annotation slot :parameter :unspecified))
(defun slot-bounds (slot) (slot-annotation slot :bounds :unspecified))
(defun slot-market-default (slot) (slot-annotation slot :market-default :unspecified))

(defun managed-slots (class)
  "Effective slot definitions of CLASS declared through DEFINE-PROCESS."
  (annotated-slots class :parameter))

(defun parameter-slots (class)
  (remove-if-not (lambda (s) (eq (slot-parameter s) t)) (managed-slots class)))

(defun slot-bounds* (slot)
  (let ((b (slot-bounds slot))) (if (eq b :unspecified) '(nil nil) b)))

(defun slot-measure-dependent-p (slot)
  (eq (slot-annotation slot :measure-dependent nil) t))

(defun slot-default-function (slot)
  (let ((f (slot-market-default slot))) (if (eq f :unspecified) nil f)))

;;; --------------------------------------------------------------------
;;; Validation at construction
;;; --------------------------------------------------------------------

(defclass parameterized () ()
  (:documentation "A mixin that validates managed slots at construction. A numeric value is coerced to
double-float and checked against its bounds. NIL is allowed only for a slot with a market
default."))

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
              (iter (for slot in (managed-slots (class-of object)))
                    (for name = (c2mop:slot-definition-name slot))
                    (for value = (slot-value object name))
                    (collect name)
                    (collect (if (and (null value) (slot-default-function slot))
                                 "market"
                                 value)))))))

;;; --------------------------------------------------------------------
;;; Introspection
;;; --------------------------------------------------------------------

(defun class-designator (object-or-class)
  (etypecase object-or-class
    (symbol (find-class object-or-class))
    (class object-or-class)
    (standard-object (class-of object-or-class))))

(defgeneric parameter-source (process)
  (:documentation "The process whose parameters PROCESS reports: PROCESS
itself, or the risk-neutral process of a physical one.")
  (:method (process) process))

(declaim (ftype (function (t) (values list &optional)) process-parameters))
(defun process-parameters (process-or-class)
  "Return the names of the model parameters of a process, or of a process class name, in
the order of declaration. It excludes settings that are declared with :PARAMETER NIL (a
GARCH period)."
  (mapcar #'c2mop:slot-definition-name
          (parameter-slots (class-designator (if (or (symbolp process-or-class) (typep process-or-class 'class))
                                                 process-or-class
                                                 (parameter-source process-or-class))))))

(defun measure-dependent-parameters (process-or-class)
  "Names of the model parameters of a process, or a process class name, whose
values differ between the risk-neutral and the physical measure (slot option
:MEASURE-DEPENDENT), in declaration order.

  (measure-dependent-parameters 'merton-jump) => (INTENSITY JUMP-MEAN JUMP-VOL)"
  (iter (for slot in (parameter-slots (class-designator process-or-class)))
        (when (slot-measure-dependent-p slot)
          (collect (c2mop:slot-definition-name slot)))))

(defun explicit-parameters (process)
  "Names of PROCESS's model parameters that hold a value, rather than NIL (the
market default).

  (explicit-parameters (make-gbm :vol 0.4d0)) => (VOL)
  (explicit-parameters (make-gbm))            => NIL"
  (let ((process (parameter-source process)))
    (remove-if-not (lambda (name) (slot-value process name)) (process-parameters process))))

(defun find-managed-slot (object-or-class name)
  "The managed slot called NAME, matched by symbol name, so :RHO, 'RHO and
FINCL::RHO all find the same slot."
  (let ((slots (managed-slots (class-designator object-or-class))))
    (or (find (string name) slots
              :key (lambda (s) (symbol-name (c2mop:slot-definition-name s)))
              :test #'string=)
        (error 'unknown-parameter
               :class (class-designator object-or-class) :name name
               :known (mapcar #'c2mop:slot-definition-name slots)))))

(declaim (ftype (function (t t &optional t real) (values t &optional)) process-parameter))
(defun process-parameter (process name &optional market (horizon 1d0))
  "Return the value of the parameter or setting NAME of PROCESS. The function matches NAME
by symbol name, so a keyword from any package works. A NIL value means that the
parameter comes from the market. With MARKET, the function resolves that default on
MARKET at HORIZON (market time). Without MARKET, it returns NIL.

  (process-parameter (make-instance 'heston :rho -0.5d0) :rho) => -0.5d0
  (process-parameter (make-instance 'heston) :v0)            => NIL
  (process-parameter (make-instance 'heston) :v0 market)     => 0.04d0"
  (let* ((process (parameter-source process))
         (slot (find-managed-slot process name))
         (value (slot-value process (c2mop:slot-definition-name slot)))
         (default (slot-default-function slot)))
    (if (and (null value) market default)
        (float (funcall default process market horizon) 1d0)
        value)))

(declaim (ftype (function (t t) (values t t &optional)) parameter-bounds))
(defun parameter-bounds (process-or-class name)
  "Return (values lo hi), the bounds of the parameter NAME as interval designators. A real
is inclusive. (x) is exclusive. NIL is unbounded. The function matches NAME by symbol
name.

  (parameter-bounds 'heston :rho) => -1, 1"
  (values-list (slot-bounds* (find-managed-slot process-or-class name))))

(declaim (ftype (function (t t &optional real) (values double-vector &optional)) parameter-vector))
(defun parameter-vector (process market &optional (horizon 1d0))
  "Return the model parameters of PROCESS as a (SIMPLE-ARRAY DOUBLE-FLOAT (*)), in the order
of PROCESS-PARAMETERS. A NIL parameter becomes its market default, evaluated on MARKET at
HORIZON (market time).

  (parameter-vector (make-instance 'heston) m) => #(0.04d0 2d0 0.04d0 0.3d0 -0.7d0)"
  (let* ((process (parameter-source process))
         (slots (parameter-slots (class-of process)))
         (v (make-array (length slots) :element-type 'double-float)))
    (iter (for slot in slots)
          (for i from 0)
          (for value = (slot-value process (c2mop:slot-definition-name slot)))
          (setf (aref v i)
                (float (or value (funcall (slot-default-function slot)
                                          process market horizon))
                       1d0)))
    v))

(declaim (ftype (function (t t) (values t &optional)) with-parameter-vector))
(defun with-parameter-vector (process vector)
  "Return a new process of the class of PROCESS. Its model parameters come from VECTOR (in
the order of PROCESS-PARAMETERS). It copies every other slot. PROCESS does not change.
The function validates the new process like any other.

  (with-parameter-vector heston (parameter-vector heston m))"
  (let* ((class (class-of process))
         (parameters (process-parameters class))
         (initargs '()))
    (unless (= (length vector) (length parameters))
      (signal-invalid-argument 'vector vector 'sequence
                               (format nil "one value per parameter of ~A, ~{~(~A~)~^, ~}"
                                       (class-name class) parameters)))
    (dolist (slot (c2mop:class-slots class))
      (let ((name (c2mop:slot-definition-name slot))
            (initarg (first (c2mop:slot-definition-initargs slot))))
        (when (and initarg (slot-boundp process name))
          (let ((position (position name parameters)))
            (push initarg initargs)
            (push (if position (aref vector position) (slot-value process name))
                  initargs)))))
    (apply #'make-instance class (nreverse initargs))))
