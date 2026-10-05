;;;; conditions.lisp --- the root of fincl's condition hierarchy
;;;;
;;;;   fincl-error
;;;;   ├── invalid-input   bad arguments at construction time
;;;;   └── pricing-error   failures while pricing
;;;;
;;;; Every error fincl signals is a FINCL-ERROR, so a caller (a portfolio
;;;; pricer, a REPL session) can handle all of them with one clause and
;;;; never sees a bare SIMPLE-ERROR. Concrete conditions live with the code
;;;; that signals them.

(in-package #:fincl)

(define-condition fincl-error (error) ()
  (:documentation "Superclass of every error fincl signals, so a caller can
handle all of them with one clause."))

(define-condition invalid-input (fincl-error) ()
  (:documentation "An argument that cannot describe a valid object: a
malformed date, a parameter out of bounds, an expiry that is not a date.
Signalled when the object is built, before any pricing."))

(define-condition pricing-error (fincl-error) ()
  (:documentation "A failure while pricing valid inputs: an engine without a
method for the instrument, a solver that does not converge, a market regime
outside an approximation's domain."))

(define-condition invalid-argument (invalid-input type-error)
  ((place :initarg :place :reader invalid-argument-place)
   (expected :initarg :expected :initform nil :reader invalid-argument-expected))
  (:report (lambda (c s)
             (format s "~(~A~) is ~S, but must be ~:[of type ~S~;~:*~A~]."
                     (invalid-argument-place c) (type-error-datum c)
                     (invalid-argument-expected c) (type-error-expected-type c))))
  (:documentation "An argument of the wrong type or shape. Also a TYPE-ERROR,
so handlers written for CHECK-TYPE keep working."))

(defmacro check-input (place type &optional expected)
  "Like CHECK-TYPE, but signals INVALID-ARGUMENT (an INVALID-INPUT) with a
STORE-VALUE restart. EXPECTED, a string, describes valid values for the
report; it defaults to the type.

  (check-input n (integer 1) \"a positive integer\")"
  `(loop until (typep ,place ',type)
         do (restart-case (error 'invalid-argument :place ',place :datum ,place
                                                   :expected-type ',type
                                                   :expected ,expected)
              (store-value (value)
                :report (lambda (s) (format s "Supply a new value for ~(~A~)." ',place))
                :interactive (lambda ()
                               (format *query-io* "~&New value for ~(~A~): " ',place)
                               (list (eval (read *query-io*))))
                (setf ,place value)))))

(defun signal-invalid-argument (place datum expected-type &optional expected)
  "Signal INVALID-ARGUMENT: PLACE (a symbol or form naming the argument) is
DATUM, which is not of EXPECTED-TYPE; EXPECTED describes valid values.

  (signal-invalid-argument 'offsets offsets 'list \"distinct real offsets\")"
  (error 'invalid-argument :place place :datum datum :expected-type expected-type
                           :expected expected))
