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
  `(iter (until (typep ,place ',type))
         (restart-case (error 'invalid-argument :place ',place :datum ,place
                                                :expected-type ',type
                                                :expected ,expected)
           (store-value (value)
             :report (lambda (s) (format s "Supply a new value for ~(~A~)." ',place))
             :interactive (lambda ()
                            (format *query-io* "~&New value for ~(~A~): " ',place)
                            (list (eval (read *query-io*))))
             (setf ,place value)))))

(declaim (ftype (function (t t t &optional t) (values nil &optional)) signal-invalid-argument))
(defun signal-invalid-argument (place datum expected-type &optional expected)
  "Signal INVALID-ARGUMENT: PLACE (a symbol or form naming the argument) is
DATUM, which is not of EXPECTED-TYPE; EXPECTED describes valid values.

  (signal-invalid-argument 'offsets offsets 'list \"distinct real offsets\")"
  (error 'invalid-argument :place place :datum datum :expected-type expected-type
                           :expected expected))

;;; --------------------------------------------------------------------
;;; Portability
;;; --------------------------------------------------------------------

(define-condition portability-fallback (warning)
  ((feature :initarg :feature :reader portability-fallback-feature)
   (kind :initarg :kind :reader portability-fallback-kind)
   (reason :initarg :reason :reader portability-fallback-reason))
  (:report (lambda (c s)
             (format s "~(~A~) uses a ~(~A~) fallback on ~A ~A: ~A"
                     (portability-fallback-feature c) (portability-fallback-kind c)
                     (lisp-implementation-type) (lisp-implementation-version)
                     (portability-fallback-reason c))))
  (:documentation "A facility of the running Lisp or of an optional library is
absent, and fincl uses a substitute that is slower or less accurate (KIND
:degraded) or computes the same quantity another way (:real). Signalled once
per image and feature."))

(define-condition seed-unsupported (invalid-input)
  ((designator :initarg :designator :reader seed-unsupported-designator)
   (seed :initarg :seed :reader seed-unsupported-seed))
  (:report (lambda (c s)
             (format s "The ~S random-number generator cannot be seeded on ~A (seed ~S): ~
                        the implementation has no seedable generator. Use :counter, ~
                        :mersenne-twister-64 or (:pcg n), which are seedable everywhere."
                     (seed-unsupported-designator c) (lisp-implementation-type)
                     (seed-unsupported-seed c))))
  (:documentation "A seed was given to the implementation's own generator, which
Common Lisp cannot seed portably."))

(define-condition unsupported-on-this-implementation (pricing-error)
  ((feature :initarg :feature :reader unsupported-feature)
   (alternative :initarg :alternative :initform nil :reader unsupported-alternative))
  (:report (lambda (c s)
             (format s "~A is not available on ~A ~A~@[; ~A~]."
                     (unsupported-feature c) (lisp-implementation-type)
                     (lisp-implementation-version) (unsupported-alternative c))))
  (:documentation "A feature that has no portable definition and no extension
on the running Lisp."))
