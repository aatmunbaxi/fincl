;;;; platform.lisp --- implementation- and dependency-conditional definitions
;;;;
;;;; Every use of an implementation extension, a foreign facility or an
;;;; optional dependency in fincl is defined here under a portable name, and
;;;; the rest of the library calls only that name. Each name has a
;;;; branch for SBCL, which is the exact expression the library used
;;;; before, and a portable branch (`%portable-NAME') that other
;;;; implementations use and that tests call directly.
;;;;
;;;; A substitute is one of three kinds:
;;;;   :real      computes the same quantity another way;
;;;;   :degraded  correct but slower or less accurate;
;;;;   loud       the feature cannot exist, and an error is signalled.
;;;; Substitutes of the first two kinds are recorded in `*fallbacks*' when
;;;; they are chosen, and PORTABILITY-REPORT lists them. A degraded one also
;;;; signals PORTABILITY-FALLBACK once per image; a real one only does so
;;;; when `*portability-warnings*' is true.
;;;;
;;;; An optional backend (a foreign library behind a separate ASDF system)
;;;; registers itself with REGISTER-OPTIONAL-BACKEND when loaded.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Records of fallbacks and backends
;;; --------------------------------------------------------------------

(defvar *portability-warnings* nil
  "When true, a fallback of kind :real also signals PORTABILITY-FALLBACK.
Fallbacks of kind :degraded always do.")

(defvar *fallbacks* (make-hash-table :test 'eq)
  "Feature keyword -> (kind . reason) of each substitute in use.")

(defvar *optional-backends* '()
  "Alist of (name . description) of the optional backends loaded.")

(defvar *cpu-count* nil
  "Memoized by CPU-COUNT on first use, so it describes the machine that runs
the code, not the one that built it.")

(defvar *cpu-count-source* nil
  "Where `*cpu-count*' came from: :nproc, or :default when it could not be read.")

(defvar *platform-lock* (bt:make-lock "fincl platform"))

(defun note-fallback (feature kind reason)
  "Record that FEATURE uses a substitute of KIND (:real or :degraded) because
REASON, and signal PORTABILITY-FALLBACK the first time unless KIND is :real
and `*portability-warnings*' is false. Returns FEATURE."
  (let ((new (bt:with-lock-held (*platform-lock*)
               (unless (gethash feature *fallbacks*)
                 (setf (gethash feature *fallbacks*) `(,kind . ,reason))))))
    (when (and new (or (eq kind :degraded) *portability-warnings*))
      (warn 'portability-fallback :feature feature :kind kind :reason reason)))
  feature)

(defun register-optional-backend (name description)
  "Record that the optional backend NAME (a keyword) is loaded."
  (bt:with-lock-held (*platform-lock*)
    (setf *optional-backends*
          `(,@(remove name *optional-backends* :key #'car) (,name . ,description))))
  name)

(defun portability-report (&optional (stream *standard-output*))
  "Print the Lisp implementation, the source of the worker count, each active
fallback with its kind and reason, and the optional backends loaded. It only
lists.

  (portability-report)"
  (format stream "~&Implementation: ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format stream "Worker count: ~A (~A)~%"
          (or *cpu-count* "not yet read") (or *cpu-count-source* "unread"))
  (let ((active (bt:with-lock-held (*platform-lock*)
                  (a:hash-table-alist *fallbacks*))))
    (if active
        (progn (format stream "Fallbacks in use:~%")
               (dolist (entry (sort active #'string< :key #'car))
                 (format stream "  ~(~A~) [~(~A~)]: ~A~%" (car entry) (cadr entry) (cddr entry))))
        (format stream "Fallbacks in use: none~%")))
  (if *optional-backends*
      (progn (format stream "Optional backends:~%")
             (dolist (entry *optional-backends*)
               (format stream "  ~(~A~): ~A~%" (car entry) (cdr entry))))
      (format stream "Optional backends: none~%"))
  (values))

;;; --------------------------------------------------------------------
;;; Floating-point classification and NaN
;;; --------------------------------------------------------------------

(declaim (inline %portable-nan-p %portable-infinity-p nan-p infinity-p))

(defun %portable-nan-p (x)
  "A NaN is the one double not equal to itself. The comparison must be quiet:
where the invalid-operation trap is enabled it can signal instead."
  (declare (double-float x))
  (/= x x))

(defun %portable-infinity-p (x)
  (declare (double-float x))
  (> (abs x) most-positive-double-float))

(defun nan-p (x)
  "True if the double-float X is a NaN. SBCL tests the bits and never traps;
the portable branch compares X with itself, which assumes a quiet comparison."
  (declare (double-float x))
  #+sbcl (sb-ext:float-nan-p x)
  #-sbcl (%portable-nan-p x))

(defun infinity-p (x)
  "True if the double-float X is positive or negative infinity."
  (declare (double-float x))
  #+sbcl (sb-ext:float-infinity-p x)
  #-sbcl (%portable-infinity-p x))

(declaim (notinline %infinity-minus-infinity))
(defun %infinity-minus-infinity (huge)
  "Infinity minus infinity, each overflowed from HUGE separately, so the
compiler cannot simplify x - x to zero."
  (declare (double-float huge))
  (let ((a (* huge 2d0)) (b (* huge 3d0)))
    (- a b)))

(defun %portable-make-nan ()
  "A quiet NaN made as infinity minus infinity, where the implementation lets
that run. Signals UNSUPPORTED-ON-THIS-IMPLEMENTATION when the
floating-point traps are enabled and make it an error."
  (handler-case (%infinity-minus-infinity most-positive-double-float)
    (arithmetic-error ()
      (error 'unsupported-on-this-implementation
             :feature "A NaN marking missing values"
             :alternative "run with the invalid-operation and overflow traps masked"))))

(defun make-nan ()
  "A quiet NaN, which marks a missing value in a stored array.

  (nan-p (make-nan)) => T"
  #+sbcl (sb-kernel:make-double-float -524288 0)
  #-sbcl (%portable-make-nan))

;;; --------------------------------------------------------------------
;;; libm functions for the complex elementary functions
;;; --------------------------------------------------------------------

(declaim (inline libm-hypot libm-log1p libm-log))

(defun %portable-hypot (x y)
  "sqrt(x^2 + y^2) for finite doubles by scaling with the larger magnitude,
which is within 2 ulp of the correctly rounded value. A NaN or infinite
argument gives a NaN or an infinity (|x| + |y|)."
  (declare (double-float x y))
  (let ((ax (abs x)) (ay (abs y)))
    (declare (double-float ax ay))
    (cond ((or (%portable-nan-p ax) (%portable-nan-p ay)
               (%portable-infinity-p ax) (%portable-infinity-p ay))
           (+ ax ay))
          (t (when (< ax ay) (rotatef ax ay))
             (if (zerop ax)
                 0d0
                 (let ((q (/ ay ax)))
                   (* ax (sqrt (+ 1d0 (* q q))))))))))

(defun libm-hypot (x y)
  "sqrt(x^2 + y^2) without overflow or underflow in the squares. SBCL calls
the C library's hypot, which is correctly rounded to within 1 ulp; the
portable branch scales by the larger magnitude and is within 2 ulp."
  (declare (double-float x y))
  #+sbcl (sb-kernel:%hypot x y)
  #-sbcl (%portable-hypot x y))

(defun %portable-log1p (x)
  "log(1 + x) by Kahan's method."
  (declare (double-float x))
  (let ((u (+ 1d0 x)))
    (declare (double-float u))
    (if (= u 1d0) x (/ (* (log u) x) (- u 1d0)))))

(defun libm-log1p (x)
  "log(1 + X) for the double-float X > -1: the C library's log1p on SBCL,
Kahan's method elsewhere (a few ulp)."
  (declare (double-float x))
  #+sbcl (sb-kernel:%log1p x)
  #-sbcl (%portable-log1p x))

(defun libm-log (x)
  "Natural logarithm of the positive double-float X, without the float-mode
handling of the generic LOG."
  (declare (double-float x))
  #+sbcl (sb-kernel:%log x)
  #-sbcl (log x))

#-sbcl
(progn
  (note-fallback :libm-hypot :real "no C library hypot; scaled by the larger magnitude, within 2 ulp")
  (note-fallback :libm-log1p :real "no C library log1p; Kahan's method")
  (note-fallback :libm-log :real "generic LOG")
  (note-fallback :nan-p :real "self-comparison, which needs a quiet comparison"))

;;; --------------------------------------------------------------------
;;; Declared special variables
;;; --------------------------------------------------------------------

(defun %portable-special-variable-p (symbol)
  (eq :special (introspect-environment:variable-information symbol)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun special-variable-p (symbol)
    "True if SYMBOL is declaimed special, so a clause cannot make it a symbol
macro. SBCL reads its info database; other implementations ask
introspect-environment."
    #+sbcl (eq :special (sb-int:info :variable :kind symbol))
    #-sbcl (%portable-special-variable-p symbol)))

;;; --------------------------------------------------------------------
;;; Worker count
;;; --------------------------------------------------------------------

(defun %portable-cpu-count ()
  "The number of processors from the `nproc' program through UIOP, or NIL."
  (ignore-errors
   (let ((n (parse-integer (uiop:run-program '("nproc") :output :string
                                                        :ignore-error-status t)
                           :junk-allowed t)))
     (and n (plusp n) n))))

(defun cpu-count ()
  "The number of processors, read once from `nproc'. When it cannot be read the
count is 4 and PORTABILITY-FALLBACK is signalled once."
  (or *cpu-count*
      (let ((n (%portable-cpu-count)))
        (setf *cpu-count-source* (if n :nproc :default))
        (unless n
          (note-fallback :cpu-count :degraded
                         "the processor count could not be read from nproc; assuming 4 workers"))
        (setf *cpu-count* (or n 4)))))

;;; --------------------------------------------------------------------
;;; Seeded random state
;;; --------------------------------------------------------------------

(defun %portable-seeded-random-state (seed)
  (error 'seed-unsupported :designator :native :seed seed))

(defun seeded-random-state (seed)
  "A RANDOM-STATE determined by the integer SEED. SBCL seeds its generator;
elsewhere Common Lisp offers no such generator and SEED-UNSUPPORTED is
signalled.

  (random 1d0 (seeded-random-state 7))"
  #+sbcl (sb-ext:seed-random-state seed)
  #-sbcl (%portable-seeded-random-state seed))

;;; --------------------------------------------------------------------
;;; Memory ordering
;;; --------------------------------------------------------------------

(defvar *barrier-lock* (bt:make-lock "fincl barrier"))

(declaim (inline memory-publication-barrier))
(defun memory-publication-barrier ()
  "Order the writes made so far before the writes that follow, so a reader
without a lock that sees the later write sees the earlier ones. SBCL emits a
write barrier; elsewhere an uncontended lock acquisition and release stands in
for it."
  #+sbcl (sb-thread:barrier (:write))
  #-sbcl (bt:with-lock-held (*barrier-lock*) nil))

#-sbcl
(note-fallback :memory-publication-barrier :degraded
               "no write barrier; a lock acquisition and release stands in for it")
