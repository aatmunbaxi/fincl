;;;; package.lisp --- the test package, check macros and shared fixtures
;;;;
;;;; Plain-CL checks with no test-framework dependency: CHECK compares within a
;;;; tolerance, CHECK-THAT asserts a boolean, and every failure increments
;;;; *FAILURES*.

(defpackage #:fincl/tests
  (:use #:cl #:fincl #:iterate)
  (:shadowing-import-from #:fincl #:minimize)
  (:local-nicknames (#:a #:alexandria) (#:lp #:lparallel))
  (:export #:run-tests #:run-time-tests #:run-syntax-tests #:run-cos-tests
           #:run-binomial-tests #:run-math-tests #:run-invariant-tests
           #:run-golden-tests #:run-fast-tests #:run-levy-tests #:run-rates-tests #:run-selection-tests
           #:run-readme-check #:run-rates-instrument-tests
           #:run-path-tests #:run-surface-tests #:run-fd-tests #:run-dividend-tests
           #:run-portfolio-tests #:run-simulation-tests #:run-simulation-component-tests
           #:run-simulation-validation-tests #:run-simulation-book-tests
           #:run-measure-tests))

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defvar *failures* 0)

(defparameter *today* #D"2026-09-24"
  "Valuation date of every test market. One year later, 2027-09-24, is
365 days away, so under ACT/365F it is exactly 1.0 in market time.")

(defmacro check (name expected actual tolerance)
  (a:with-gensyms (n e a)
    `(let ((,n ,name) (,e (float ,expected 1d0)) (,a (float ,actual 1d0)))
       (if (<= (abs (- ,e ,a)) ,tolerance)
           (format t "~&  ok   ~A (~,4F)~%" ,n ,a)
           (progn (incf *failures*)
                  (format t "~&  FAIL ~A: expected ~,4F, got ~,4F~%" ,n ,e ,a))))))

(defmacro check-that (name form)
  `(if ,form
       (format t "~&  ok   ~A~%" ,name)
       (progn (incf *failures*) (format t "~&  FAIL ~A~%" ,name))))

(defmacro check-equal (name expected actual &key (test '#'equalp))
  (a:with-gensyms (n e a)
    `(let ((,n ,name) (,e ,expected) (,a ,actual))
       (if (funcall ,test ,e ,a)
           (format t "~&  ok   ~A (~A)~%" ,n ,a)
           (progn (incf *failures*)
                  (format t "~&  FAIL ~A: expected ~A, got ~A~%" ,n ,e ,a))))))

(defmacro check-error (name form &optional (type 'error))
  `(if (handler-case (progn ,form nil) (,type () t))
       (format t "~&  ok   ~A signals ~A~%" ,name ',type)
       (progn (incf *failures*)
              (format t "~&  FAIL ~A: no ~A signalled~%" ,name ',type))))

;;; --------------------------------------------------------------------
;;; Implementation-dependent helpers. The tests name no SBCL internal outside
;;; this section: a check that needs one runs under WHEN-SBCL-CHECKS and is
;;; reported as skipped elsewhere.
;;; --------------------------------------------------------------------

(defvar *skipped-checks* '()
  "Names of the checks skipped on this implementation, newest first.")

(defmacro when-sbcl-checks ((name reason) &body body)
  "Run BODY on SBCL; elsewhere print one line saying that the check NAME was
skipped because REASON, and record it in `*skipped-checks*'. Skipped is not passed."
  `(progn
     #+sbcl (progn ,@body)
     #-sbcl (progn (push ,name *skipped-checks*)
                   (format t "~&  skip ~A: ~A~%" ,name ,reason))))

(defun test-random-state (seed)
  "A random state determined by SEED. Where the implementation cannot seed its
generator, a fresh state: the checks that use it draw inputs whose outcome does not
depend on the draw."
  (handler-case (fincl::seeded-random-state seed)
    (seed-unsupported () (make-random-state t))))

(defun bytes-consed ()
  "Bytes allocated so far, or NIL where the implementation does not count."
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl nil)

(defun full-gc ()
  "A full garbage collection where the implementation offers one."
  #+sbcl (sb-ext:gc :full t)
  #-sbcl (progn))

(defvar *count-lock* (bt:make-lock "count"))

(defmacro with-quiet-float-traps (&body body)
  "Run BODY with the invalid-operation, overflow and divide-by-zero traps masked,
where the implementation lets that be done, so NaN and infinity arithmetic is quiet."
  #+sbcl `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero) ,@body)
  #-sbcl `(progn ,@body))

(defun positive-infinity ()
  "Positive infinity as a double-float."
  #+sbcl sb-ext:double-float-positive-infinity
  #-sbcl (with-quiet-float-traps (* 2 most-positive-double-float)))

(defmacro atomic-incf-car (cell)
  "Add one to the CAR of CELL (a fixnum) without losing counts to other threads."
  #+sbcl `(sb-ext:atomic-incf (car ,cell))
  #-sbcl `(bt:with-lock-held (*count-lock*) (incf (car ,cell))))

(defmacro count-calls ((counter function) &body body)
  "Run BODY with the global function FUNCTION (a symbol) wrapped so each call adds
one to the CAR of the cons COUNTER, which is bound to (0) for the extent. The
original function is restored on exit.

  (count-calls (n 'fincl::copy-composite-market) (price o e m) (car n))"
  (a:with-gensyms (original name)
    `(let* ((,counter (list 0)) (,name ,function) (,original (fdefinition ,name)))
       (setf (fdefinition ,name)
             (lambda (&rest args)
               (bt:with-lock-held (*count-lock*) (incf (car ,counter)))
               (apply ,original args)))
       (unwind-protect (progn ,@body)
         (setf (fdefinition ,name) ,original)))))

(defun mc (&rest args)
  (apply #'make-instance 'monte-carlo-engine args))

(defun binomial (n-steps &rest args)
  (apply #'make-instance 'binomial-engine :n-steps n-steps args))


(defun dense-gauss-solve (a n b)
  "The solution of A x = B, A an N x N row-major vector, by Gaussian
elimination with partial pivoting: the test suite's own dense oracle,
independent of fincl's solvers."
  (let ((m (map '(simple-array double-float (*)) (lambda (x) (float x 1d0)) a))
        (x (map '(simple-array double-float (*)) (lambda (x) (float x 1d0)) b)))
    (flet ((at (i j) (aref m (+ (* i n) j))))
      (dotimes (k n)
        (let ((pivot (iter (for i from k below n) (finding i maximizing (abs (at i k))))))
          (unless (= pivot k)
            (dotimes (j n) (rotatef (aref m (+ (* k n) j)) (aref m (+ (* pivot n) j))))
            (rotatef (aref x k) (aref x pivot))))
        (iter (for i from (1+ k) below n)
              (let ((factor (/ (at i k) (at k k))))
                (iter (for j from k below n) (decf (aref m (+ (* i n) j)) (* factor (at k j))))
                (decf (aref x i) (* factor (aref x k))))))
      (iter (for i from (1- n) downto 0)
            (setf (aref x i) (/ (- (aref x i) (iter (for j from (1+ i) below n) (sum (* (at i j) (aref x j)))))
                                (at i i))))
      x)))

(defun read-fixture (name)
  "The forms of tests/fixtures/NAME, read with double-float defaults."
  (let ((*read-default-float-format* 'double-float))
    (with-open-file (in (asdf:system-relative-pathname "fincl" (format nil "tests/fixtures/~A" name)))
      (iter (for form = (read in nil in)) (until (eq form in)) (collect form)))))

(defun relative-difference (expected actual)
  (if (zerop expected) (abs actual) (abs (/ (- actual expected) expected))))

(defun error-message (thunk)
  "The report string of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(defmacro with-standard-fixtures (&body body)
  "Run BODY with the markets, engines and options shared by the pricing
checks bound: MKT (S=100, r=5%, vol 20%), MKT2 (the Longstaff-Schwartz put
market, S=36, r=6%), BS and BAW engines, CALL and PUT (K=100, expiring
2027-09-24, one year after *TODAY*) and APUT (American put, K=40)."
  `(let* ((mkt (make-bs-market :valuation-date *today*
                               :spot 100 :rate 0.05d0 :vol 0.2d0))
          (mkt2 (make-bs-market :valuation-date *today*
                                :spot 36 :rate 0.06d0 :vol 0.2d0))
          (bs (make-instance 'black-scholes-engine))
          (baw (make-instance 'barone-adesi-whaley-engine))
          (call (make-option (call 100) (expires #D"2027-09-24")))
          (put (make-option (put 100) (expires #D"2027-09-24")))
          (aput (make-option (put 40) (american) (expires #D"2027-09-24"))))
     (declare (ignorable mkt mkt2 bs baw call put aput))
     ,@body))
