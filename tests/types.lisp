;;;; types.lisp --- the domain types of src/types.lisp and their use on slots

(in-package #:fincl/tests)

(defparameter *domain-slot-names*
  '("VOL" "SIGMA" "VARIANCE" "V0" "THETA" "RHO" "CORRELATION" "RATE" "EXPIRY-TIME")
  "Slot names that hold a domain quantity: a slot with one of these names must
declare a type narrower than T.")

(defparameter *domain-type-samples*
  `((fincl::non-negative-double (0d0 3d0) (-1d-9 1))
    (fincl::positive-double (1d-300 2d0) (0d0 -1d0))
    (fincl::unit-interval (0d0 0.5d0 1d0) (-1d-9 1.0000001d0))
    (fincl::correlation (-1d0 0d0 1d0) (1.3d0 -1.0000001d0))
    (fincl::market-time (0d0 0.25d0) (-0.1d0 1/4))
    (fincl::year-fraction (-0.5d0 0.5d0) (1/2))
    (fincl::volatility (0d0 0.2d0) (-0.2d0))
    (fincl::rate (-0.005d0 0.04d0) (0.04))
    (fincl::discount-value (0.96d0) (0d0 -1d0))
    (fincl::complex-double (#c(1d0 0d0)) (1d0 #c(1 0)))
    (fincl::option-kind (:call :put) (:straddle))
    (fincl::state-vector (,(make-array 2 :element-type 'double-float)) (#(1d0 2d0)))
    (fincl::index (0 7) (-1 1d0)))
  "(type accepted-values rejected-values): each type contains its first list and
none of its second.")

(defun domain-type-checks ()
  "Each domain type accepts and rejects the values listed in
*DOMAIN-TYPE-SAMPLES*."
  (dolist (entry *domain-type-samples*)
    (destructuring-bind (type accepted rejected) entry
      (check-that (format nil "~(~A~) accepts ~S" type accepted)
                  (every (lambda (x) (typep x type)) accepted))
      (check-that (format nil "~(~A~) rejects ~S" type rejected)
                  (notany (lambda (x) (typep x type)) rejected)))))

(defun fincl-slot-definitions ()
  "A list of (class-name slot-name slot-type) for every direct slot of every
class and structure defined in the FINCL package."
  (let ((rows '()))
    (do-all-symbols (s '#:fincl)
      (when (eq (symbol-package s) (find-package '#:fincl))
        (let ((class (find-class s nil)))
          (when (and class (typep class '(or standard-class structure-class)))
            (c2mop:ensure-finalized class)
            (dolist (slot (c2mop:class-direct-slots class))
              (pushnew `(,s ,(c2mop:slot-definition-name slot)
                         ,(c2mop:slot-definition-type slot))
                       rows :test #'equal))))))
    (sort rows #'string< :key (lambda (row) (symbol-name (first row))))))

(defun domain-slot-checks ()
  "No slot named for a domain quantity is left at type T."
  (let ((untyped (remove-if-not
                  (lambda (row)
                    (and (member (symbol-name (second row)) *domain-slot-names* :test #'string=)
                         (eq (third row) t)))
                  (fincl-slot-definitions))))
    (check-that (format nil "no domain-named slot is untyped~@[ (~:{~(~A.~A~)~^, ~})~]"
                        (mapcar (lambda (row) `(,(first row) ,(second row))) untyped))
                (null untyped))))

(defun parameter-bounds-type-checks ()
  "DEFINE-PROCESS signals INVALID-ARGUMENT, when the form is expanded, for a
parameter whose bounds exceed its declared type."
  (check-that "bounds (-1 1) inside correlation"
              (null (fincl::check-bounds-within-type 'p 'rho '(-1 1) 'fincl::correlation)))
  (check-that "bounds (0 nil) inside (or null volatility)"
              (null (fincl::check-bounds-within-type 'p 'vol '(0 nil) '(or null fincl::volatility))))
  (check-that "bounds ((0) nil) inside positive-double"
              (null (fincl::check-bounds-within-type 'p 'kappa '((0) nil) 'fincl::positive-double)))
  (check-that "bounds (-1 1) are not inside unit-interval"
              (handler-case (progn (fincl::check-bounds-within-type 'p 'rho '(-1 1) 'fincl::unit-interval)
                                   nil)
                (fincl::invalid-argument () t)))
  (check-that "bounds (0 nil) are not inside positive-double"
              (handler-case (progn (fincl::check-bounds-within-type 'p 'kappa '(0 nil) 'fincl::positive-double)
                                   nil)
                (fincl::invalid-argument () t))))

(defun parameter-rejection-checks ()
  "Process constructors signal INVALID-PARAMETER for a value outside the
declared bounds, and accept the bounds' endpoints where they are inclusive."
  (flet ((rejected-p (thunk)
           (handler-case (progn (funcall thunk) nil)
             (fincl::invalid-parameter () t))))
    (check-that "gbm rejects a negative vol"
                (rejected-p (lambda () (fincl::make-gbm :vol -0.2d0))))
    (check-that "gbm rejects a zero vol"
                (rejected-p (lambda () (fincl::make-gbm :vol 0d0))))
    (check-that "heston rejects rho above 1"
                (rejected-p (lambda () (fincl::make-heston :rho 1.5d0))))
    (check-that "heston rejects a negative v0"
                (rejected-p (lambda () (fincl::make-heston :v0 -0.04d0))))
    (check-that "heston rejects a non-positive kappa"
                (rejected-p (lambda () (fincl::make-heston :kappa 0d0))))
    (check-that "heston accepts rho = -1 and v0 = 0"
                (not (rejected-p (lambda () (fincl::make-heston :rho -1d0 :v0 0d0)))))))

(defun type-checks ()
  "Run the domain-type checks."
  (domain-type-checks)
  (domain-slot-checks)
  (parameter-bounds-type-checks)
  (parameter-rejection-checks))
