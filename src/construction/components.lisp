;;;; construction/components.lisp --- classes built from facts
;;;;
;;;; A slot of a COMPONENT-CLASS can name the fact that fills it:
;;;;
;;;;   (defclass bermudan-exercise (exercise)
;;;;     ((dates :initarg :dates :fact :exercise-dates))
;;;;     (:metaclass component-class))
;;;;
;;;; MAKE-FROM-FACTS builds the class from the current fact table. Thus the assembly
;;;; code of a language never routes facts to initargs by hand. A class that is defined
;;;; outside fincl receives the facts that it declares.

(in-package #:fincl)

(define-slot-option :fact "The construction fact that fills the slot.")

(defclass component-class (annotated-class) ()
  (:documentation "A metaclass whose slots accept :FACT. Subclasses must use it too to keep their
facts. A subclass from a plain DEFCLASS keeps no annotations.

  (defclass leg () ((rate :initarg :rate :fact :rate)) (:metaclass component-class))"))

(defun fact-slots (class)
  "The effective slots of CLASS that carry :FACT."
  (annotated-slots class :fact))

(defun required-slot-p (slot)
  "True if SLOT has no initform, or one that only signals a missing argument."
  (let ((form (c2mop:slot-definition-initform slot)))
    (or (null (c2mop:slot-definition-initfunction slot))
        (and (consp form) (eq (first form) 'alexandria:required-argument)))))

(declaim (ftype (function (t &rest t) (values t &optional)) make-from-facts))
(defun make-from-facts (class &rest explicit-initargs)
  "Make an instance of CLASS (a COMPONENT-CLASS or its name). Fill every slot that
names a fact from the current fact table. EXPLICIT-INITARGS win over facts. If a
required slot has a missing fact, the function signals MISSING-FACT. The report
names the class, the slot and the clauses that supply the fact.

  (make-from-facts 'european-exercise)          ; :EXPIRY from the table
  (make-from-facts 'vanilla-payoff :strike 95)  ; :STRIKE given explicitly"
  (let* ((class (if (symbolp class) (find-class class) class))
         (from-facts '()))
    (dolist (slot (fact-slots class))
      (let ((initarg (first (c2mop:slot-definition-initargs slot)))
            (fact (slot-annotation slot :fact)))
        (when (and initarg (eq (getf explicit-initargs initarg :absent) :absent))
          (multiple-value-bind (value found) (fact fact)
            (cond (found (setf from-facts `(,@from-facts ,initarg ,value)))
                  ((required-slot-p slot)
                   (require-fact fact :class class
                                      :slot (c2mop:slot-definition-name slot))))))))
    (apply #'make-instance class `(,@explicit-initargs ,@from-facts))))
