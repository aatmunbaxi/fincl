;;;; package.lisp --- the test package, check macros and shared fixtures
;;;;
;;;; Plain-CL checks with no test-framework dependency: CHECK compares within a
;;;; tolerance, CHECK-THAT asserts a boolean, and every failure increments
;;;; *FAILURES*.

(defpackage #:fincl/tests
  (:use #:cl #:fincl)
  (:local-nicknames (#:a #:alexandria) (#:lp #:lparallel))
  (:export #:run-tests #:run-time-tests #:run-syntax-tests #:run-cos-tests
           #:run-binomial-tests #:run-math-tests #:run-invariant-tests
           #:run-golden-tests #:run-fast-tests))

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

(defun mc (&rest args)
  (apply #'make-instance 'monte-carlo-engine args))

(defun binomial (n-steps &rest args)
  (apply #'make-instance 'binomial-engine :n-steps n-steps args))


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
          (call (make-option '(:call 100 #D"2027-09-24")))
          (put (make-option '(:put 100 #D"2027-09-24")))
          (aput (make-option '(:american :put 40 #D"2027-09-24"))))
     (declare (ignorable mkt mkt2 bs baw call put aput))
     ,@body))
