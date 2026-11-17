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

;;; --------------------------------------------------------------------
;;; Declared function types
;;; --------------------------------------------------------------------

(defparameter *undeclared-function-exceptions*
  '(("constructors written by DEFINE-PROCESS"
     "MAKE-BATES" "MAKE-CEV" "MAKE-CGMY" "MAKE-GARCH" "MAKE-GBM" "MAKE-HESTON"
     "MAKE-HULL-WHITE" "MAKE-KOU-JUMP" "MAKE-LOCAL-VOL" "MAKE-MERTON-JUMP" "MAKE-NIG"
     "MAKE-VARIANCE-GAMMA")
    ("data function written by DEFINE-CONSTRUCTION-LANGUAGE" "MAKE-OPTION-FROM-CLAUSES")
    ("helper defined by tests/cos.lisp, which shares the FINCL symbol" "COS-ENGINE"))
  "(reason name...): exported ordinary functions with no declared function type.
Functions written by DEFINE-PROCESS, DEFINE-PAYOFF, DEFINE-PATH-PAYOFF and the
other generating macros are not declared by hand; DEFSTRUCT declares its own.")

(defun exported-plain-functions ()
  "Every exported symbol of FINCL that names an ordinary function: not a
generic function, a macro or a special operator."
  (let ((symbols '()))
    (do-external-symbols (s '#:fincl)
      (when (and (fboundp s) (not (macro-function s)) (not (special-operator-p s))
                 (not (typep (fdefinition s) 'generic-function)))
        (push s symbols)))
    (sort symbols #'string< :key #'symbol-name)))

(defun declared-function-type (symbol)
  "The (FUNCTION ARGS VALUES) type declaimed for SYMBOL, or NIL if there is none
beyond the bare FUNCTION of a forward declaration."
  (let* ((declarations (third (multiple-value-list
                               (introspect-environment:function-information symbol))))
         (type (cdr (assoc 'ftype declarations))))
    (and (consp type) (eq (first type) 'function) (cdr type) type)))

(defun values-type-closed-p (type)
  "True if TYPE, the declared type of a function, ends its values with &OPTIONAL
or &REST, so the number of values is stated; a function that never returns,
declared NIL, is closed."
  (let ((values-type (third type)))
    (or (null values-type)
        (and (consp values-type) (eq (first values-type) 'values)
             (or (member '&optional values-type) (member '&rest values-type))
             t))))

(defun wrong-type-call-warns-p (form)
  "True if compiling FORM, a call with an argument of the wrong type, signals a
warning."
  (let ((warned nil))
    (handler-bind ((warning (lambda (condition)
                              (setf warned t)
                              (muffle-warning condition))))
      (let ((*error-output* (make-broadcast-stream)))
        (compile nil `(lambda () ,form))))
    warned))

(defun function-type-checks ()
  "Every exported ordinary function has a declared type that states its values,
except the generated ones listed in *UNDECLARED-FUNCTION-EXCEPTIONS*, and a call
with an argument of the wrong type is reported at compile time."
  (when-sbcl-checks ("function types" "declared types are read through introspect-environment")
    (let* ((exempt (iter (for (nil . names) in *undeclared-function-exceptions*)
                         (appending (mapcar (lambda (name) (find-symbol name '#:fincl)) names))))
           (functions (exported-plain-functions))
           (checked (set-difference functions exempt))
           (undeclared (remove-if #'declared-function-type checked))
           (unclosed (remove-if-not (lambda (s)
                                  (let ((type (declared-function-type s)))
                                    (and type (not (values-type-closed-p type)))))
                                functions)))
      (check-that (format nil "every exported ordinary function has a declared type~@[ (missing: ~{~(~A~)~^ ~})~]"
                          undeclared)
                  (null undeclared))
      (check-that (format nil "declared types state their values~@[ (open: ~{~(~A~)~^ ~})~]" unclosed)
                  (null unclosed))
      (check-that "every exception names an exported ordinary function"
                  (every (lambda (symbol) (and symbol (member symbol functions))) exempt))
      (check-that "no exception is also declared"
                  (notany #'declared-function-type exempt))
      (dolist (form '((fincl:norm-cdf "x")
                      (fincl:date+ 1 2)
                      (fincl:implied-black-vol "x" 1 1 1 1 1)
                      (fincl:swap-leg-npv nil "x" nil)
                      (fincl:portfolio-positions "x")
                      (fincl:historical-var nil "x")
                      (fincl:simulation-held nil "x" :a)))
        (check-that (format nil "~S is reported when compiled" form)
                    (wrong-type-call-warns-p form))))))

(define-checks type-checks
  "Run the domain-type and function-type checks."
  (domain-type-checks)
  (domain-slot-checks)
  (parameter-bounds-type-checks)
  (parameter-rejection-checks)
  (function-type-checks))
