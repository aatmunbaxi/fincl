;;;; construction.lisp --- the construction kernel, on a toy language
;;;;
;;;; The kernel is tested on a two-legged toy exchange defined here, not on
;;;; options, so that nothing in it can lean on option-specific code.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; The toy language
;;; --------------------------------------------------------------------

(define-fact :toy-notional double-float "Notional of a toy exchange." :coerce #'real-to-double)
(define-fact :toy-rate double-float "A leg's rate." :coerce #'real-to-double)
(define-fact :toy-frequency (member :annual :quarterly) "Payments per year.")
(define-fact :toy-years (integer 1) "Term in whole years.")
(define-fact :toy-payments (integer 0) "Number of payments of a leg.")
(define-fact :toy-side (member :pay :receive) "Which side a leg is.")
(define-fact :toy-label string "A free-text label.")
(define-fact :toy-a integer "Derived from :TOY-B.")
(define-fact :toy-b integer "Derived from :TOY-A.")

(defclass toy-leg ()
  ((side :initarg :side :reader toy-side :fact :toy-side)
   (rate :initarg :rate :reader toy-rate :fact :toy-rate)
   (frequency :initarg :frequency :reader toy-frequency :fact :toy-frequency)
   (payments :initarg :payments :reader toy-payments :fact :toy-payments)
   (label :initarg :label :initform nil :reader toy-label :fact :toy-label))
  (:metaclass component-class))

;;; Overrides only the fact of an inherited slot.
(defclass toy-special-leg (toy-leg)
  ((rate :fact :toy-notional))
  (:metaclass component-class))

(defclass toy-exchange ()
  ((notional :initarg :notional :reader toy-notional)
   (years :initarg :years :reader toy-years)
   (pay :initarg :pay :reader toy-pay)
   (receive :initarg :receive :reader toy-receive)))

(defun assemble-toy-exchange ()
  (make-instance 'toy-exchange
                 :notional (require-fact :toy-notional)
                 :years (require-fact :toy-years)
                 :pay (with-scope (:pay) (make-from-facts 'toy-leg))
                 :receive (with-scope (:receive) (make-from-facts 'toy-leg))))

(define-construction-language toy-exchange
  (:documentation "A toy two-legged exchange.")
  (:macro exchange)
  (:function make-exchange)
  (:defaults :toy-frequency :annual)
  (:assemble assemble-toy-exchange))

(define-clause toy-exchange notional (amount) (:publishes :toy-notional)
  (publish :toy-notional amount))
(define-clause toy-exchange years (n) (:publishes :toy-years)
  (publish :toy-years n))
(define-clause toy-exchange label (text) (:publishes :toy-label)
  (publish :toy-label text))
(define-clause toy-exchange frequency (f) (:publishes :toy-frequency)
  (publish :toy-frequency f))
(define-clause toy-exchange fixed (rate) (:publishes :toy-rate)
  (publish :toy-rate rate))
(define-clause toy-exchange leg (side &clauses clauses) (:publishes :toy-side)
  "A leg: its sub-clauses apply in the leg's own scope."
  (with-scope (side)
    (publish :toy-side side)
    (apply-clauses clauses)))
(define-clause toy-exchange sloppy (n) (:publishes :toy-rate)
  (publish :toy-years n))                      ; publishes what it does not declare

(define-fact-rule toy-exchange :toy-payments ((years :toy-years) (frequency :toy-frequency))
  (* years (ecase frequency (:annual 1) (:quarterly 4))))
(define-fact-rule toy-exchange :toy-a ((b :toy-b)) (1+ b))
(define-fact-rule toy-exchange :toy-b ((a :toy-a)) (1- a))

(define-convention toy-exchange :quarterly-labelled
  (frequency :quarterly)
  (label "standard"))
(define-convention toy-exchange :annual
  (frequency :annual))

(defmethod object-clauses ((language (eql 'toy-exchange)) (x toy-exchange))
  (flet ((leg (leg)
           `(:leg ,(toy-side leg) (:fixed ,(toy-rate leg)) (:frequency ,(toy-frequency leg))
                  ,@(when (toy-label leg) `((:label ,(toy-label leg)))))))
    `((:notional ,(toy-notional x)) (:years ,(toy-years x))
      ,(leg (toy-pay x)) ,(leg (toy-receive x)))))

(defmethod print-object ((x toy-exchange) stream)
  (print-clause-form 'toy-exchange x stream))

;;; --------------------------------------------------------------------
;;; Helpers
;;; --------------------------------------------------------------------

(defmacro with-toy-table (&body body)
  `(with-fact-table (:language (fincl::find-language 'toy-exchange))
     ,@body))

(defmacro as-clause ((form &optional (priority :explicit)) &body body)
  "Run BODY as if inside the clause FORM, at PRIORITY."
  `(let ((fincl::*provenance* (fincl::make-provenance ',form))
         (fincl::*priority* ,priority))
     ,@body))

(defun condition-of (thunk)
  (handler-case (progn (funcall thunk) nil)
    (condition (c) c)))

(defun standard-exchange (&rest extra)
  (make-exchange `((:notional 100) (:years 2)
                   (:leg :pay (:fixed 0.04d0))
                   (:leg :receive (:fixed 0.03d0))
                   ,@extra)))

;;; --------------------------------------------------------------------
;;; Phase 1: facts, tables, components
;;; --------------------------------------------------------------------

(defun construction-table-checks ()
  (format t "~&Construction: facts and tables~%")
  (with-toy-table
    (as-clause ((rate 5))
      (publish :toy-rate 5))
    (check-that "a fact's coercion runs: 5 -> 5.0d0" (eql 5d0 (fact :toy-rate))))
  (check-error "a value of the wrong type"
               (with-toy-table (as-clause ((side :sideways)) (publish :toy-side :sideways)))
               fact-type-error)
  (check-error "a fact nobody defined" (with-toy-table (publish :no-such-fact 1)) undeclared-fact)
  (with-toy-table
    (as-clause ((label "root")) (publish :toy-label "root"))
    (with-scope (:pay)
      (check-equal "a scope reads its parent's fact" "root" (fact :toy-label))
      (as-clause ((label "leg")) (publish :toy-label "leg"))
      (check-equal "a scope's own setting shadows its parent's" "leg" (fact :toy-label)))
    (check-equal "the parent keeps its own value" "root" (fact :toy-label)))
  (with-toy-table
    (as-clause ((frequency :quarterly) :convention) (publish :toy-frequency :quarterly))
    (as-clause ((frequency :annual)) (publish :toy-frequency :annual))
    (check-equal "explicit overrides an earlier convention" :annual (fact :toy-frequency)))
  (with-toy-table
    (as-clause ((frequency :annual)) (publish :toy-frequency :annual))
    (as-clause ((frequency :quarterly) :convention) (publish :toy-frequency :quarterly))
    (check-equal "a later convention does not override explicit" :annual (fact :toy-frequency))
    (check-equal "provenance records the clause" '(frequency :annual)
                 (fincl::provenance-form (fact-provenance :toy-frequency))))
  (let ((c (condition-of (lambda ()
                           (with-toy-table
                             (as-clause ((years 2)) (publish :toy-years 2))
                             (as-clause ((years 3)) (publish :toy-years 3)))))))
    (check-that "two explicit settings conflict, naming both clauses"
                (and (typep c 'conflicting-clauses)
                     (search "(years 2)" (princ-to-string c))
                     (search "(years 3)" (princ-to-string c)))))
  (format t "~&Construction: components~%")
  (with-toy-table
    (as-clause ((all)) (publish :toy-side :pay) (publish :toy-rate 0.05d0)
               (publish :toy-frequency :annual) (publish :toy-payments 3)
               (publish :toy-notional 100))
    (let ((leg (make-from-facts 'toy-leg :rate 0.07d0)))
      (check-that "make-from-facts fills slots from facts, explicit initargs win"
                  (and (eq (toy-side leg) :pay) (= (toy-rate leg) 0.07d0)
                       (= (toy-payments leg) 3) (null (toy-label leg)))))
    (check-equal "a subclass overrides an inherited slot's fact" 100d0
                 (toy-rate (make-from-facts 'toy-special-leg))))
  (let ((c (condition-of (lambda ()
                           (with-toy-table
                             (as-clause ((side :pay)) (publish :toy-side :pay))
                             (make-from-facts 'toy-leg))))))
    (check-that "a missing fact names the class, slot and fact"
                (and (typep c 'missing-fact)
                     (let ((text (princ-to-string c)))
                       (and (search "TOY-LEG" text) (search "slot" text) (search ":TOY-" text)))))))

;;; --------------------------------------------------------------------
;;; Phase 2: clauses, scopes, conventions, rules, languages
;;; --------------------------------------------------------------------

(defun construction-language-checks ()
  (format t "~&Construction: languages~%")
  (let ((x (exchange (notional 100) (years 2)
                     (leg :pay (fixed 0.04d0) (label "mine"))
                     (leg :receive (fixed 0.03d0) (frequency :quarterly)))))
    (check-that "the macro builds both legs in their own scopes"
                (and (eq (toy-side (toy-pay x)) :pay) (eq (toy-side (toy-receive x)) :receive)
                     (= (toy-rate (toy-pay x)) 0.04d0) (= (toy-rate (toy-receive x)) 0.03d0)))
    (check-that "a leg sees the root default and can override it"
                (and (eq (toy-frequency (toy-pay x)) :annual)
                     (eq (toy-frequency (toy-receive x)) :quarterly)))
    (check-that "derived facts use each leg's own inputs: 2 and 8 payments"
                (and (= 2 (toy-payments (toy-pay x))) (= 8 (toy-payments (toy-receive x)))))
    (check-that "a fact set inside one leg stays in that leg"
                (and (equal "mine" (toy-label (toy-pay x))) (null (toy-label (toy-receive x))))))
  (let ((rate 0.045d0))
    (check-that "macro arguments are evaluated, and the macro equals the data function"
                (equalp (object-clauses 'toy-exchange
                                        (exchange (notional 100) (years 2)
                                                  (leg :pay (fixed rate)) (leg :receive (fixed 0.03d0))))
                        (object-clauses 'toy-exchange
                                        (make-exchange `((:notional 100) (:years 2)
                                                         (:leg :pay (:fixed ,rate))
                                                         (:leg :receive (:fixed 0.03d0))))))))
  (let ((x (exchange (convention :quarterly-labelled) (frequency :annual) (notional 1) (years 1)
                     (leg :pay (fixed 0.01d0)) (leg :receive (fixed 0.02d0)))))
    (check-that "a convention fills facts, and an explicit clause overrides it in any order"
                (and (eq (toy-frequency (toy-pay x)) :annual)
                     (equal "standard" (toy-label (toy-pay x))))))
  (check-error "two conventions setting the same fact conflict"
               (standard-exchange '(:convention :quarterly-labelled) '(:convention :annual))
               conflicting-clauses)
  (check-error "an unknown convention" (standard-exchange '(:convention :weekly)) unknown-convention)
  (let ((c (condition-of (lambda ()
                           (make-exchange '((:notional 1)
                                            (:leg :pay (:fixed 0.01d0))
                                            (:leg :receive (:fixed 0.02d0))))))))
    (check-that "a missing fact names the clauses that supply it"
                (and (typep c 'missing-fact) (search "(years)" (princ-to-string c)))))
  (let ((c (condition-of (lambda () (with-toy-table (require-fact :toy-a))))))
    (check-that "a rule cycle is reported as one"
                (and (typep c 'fact-rule-cycle)
                     (search ":TOY-A" (princ-to-string c)) (search ":TOY-B" (princ-to-string c)))))
  (check-error "publishing an undeclared fact" (standard-exchange '(:sloppy 3)) undeclared-fact)
  (let ((c (condition-of (lambda () (macroexpand-1 '(exchange (notionl 100)))))))
    (check-that "an unknown clause is a macroexpansion error that suggests the nearest"
                (and (typep c 'unknown-clause) (search "notional" (princ-to-string c)))))
  (check-error "an unknown clause through the data function"
               (make-exchange '((:notional 1) (:bogus 2))) unknown-clause)
  (let ((warned nil))
    (handler-bind ((unused-fact (lambda (w) (setf warned w) (muffle-warning w))))
      (exchange (notional 1) (years 1) (frequency :quarterly)
                (leg :pay (fixed 0.01d0) (frequency :annual))
                (leg :receive (fixed 0.02d0) (frequency :annual))))
    (check-that "a fact nothing read gives a style warning naming its clause"
                (and warned (search "(frequency :quarterly)" (princ-to-string warned))))))

;;; --------------------------------------------------------------------
;;; Phase 3: printing and round trip
;;; --------------------------------------------------------------------

(defun construction-printing-checks ()
  (format t "~&Construction: printing~%")
  (let ((x (standard-exchange)))
    (check-that "an object prints as its clauses"
                (search "(leg :pay (fixed 0.04d0)" (prin1-to-string x)))
    (check-that "round-trip-p" (round-trip-p 'toy-exchange x))
    (let* ((text (let ((*print-readably* t) (*package* (find-package '#:fincl/tests)))
                   (prin1-to-string x)))
           (rebuilt (eval (let ((*readtable* (named-readtables:find-readtable 'fincl:syntax))
                                (*package* (find-package '#:fincl/tests)))
                            (read-from-string text)))))
      (check-that "the readable form reads and evaluates back to an equal object"
                  (equalp (object-clauses 'toy-exchange x)
                          (object-clauses 'toy-exchange rebuilt))))))

(defun construction-checks ()
  (construction-table-checks)
  (construction-language-checks)
  (construction-printing-checks))
