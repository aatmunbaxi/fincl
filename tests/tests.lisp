;;;; tests.lisp --- the suite runners
;;;;
;;;; Each area file defines an <area>-CHECKS function; the runners here call
;;;; them in order. Loaded last, so every checks function is already defined.

(in-package #:fincl/tests)

(defclass suite-report (parachute:quiet)
  ((verbose :initarg :verbose :reader report-verbose)
   (out :initarg :out :reader report-out))
  (:default-initargs :verbose nil :out *standard-output*)
  (:documentation "A Parachute report that prints one line per suite when it ends and a
line for every failed check (every check when VERBOSE), and records an error
in a check or in a suite as a failure instead of unwinding."))

(defmethod parachute:eval-in-context ((report suite-report) (result parachute:value-result))
  (handler-case (call-next-method)
    (error (err)
      (setf (parachute:value result) (if (typep result 'parachute:multiple-value-result)
                                         (list err)
                                         err)
            (parachute:status result) :failed))))

(defmethod parachute:eval-in-context ((report suite-report) (result parachute:test-result))
  (handler-case (call-next-method)
    (error (err)
      (setf (parachute:description result)
            (format nil "error outside any check: ~A" err)
            (parachute:status result) :failed))))

(defmethod parachute:eval-in-context :after ((report suite-report) (result check-result))
  (let ((status (parachute:status result)))
    (when (or (report-verbose report) (eq status :failed))
      (format (report-out report) "~&~A~%"
              (check-line (check-kind result) (parachute:description result) status
                          (parachute:expected result)
                          (when (slot-boundp result 'parachute:value) (parachute:value result)))))))

(defvar *failed-suites* '()
  "Names of the suites that failed or errored in the last run, for RUN-FAILED-TESTS.")

(defun suite-counts (result)
  "(values passed failed skipped) among the checks directly under the suite RESULT."
  (flet ((n (status)
           (count-if (lambda (r) (and (eq status (parachute:status r))
                                      (not (typep r 'parachute:test-result))))
                     (parachute:results result))))
    (values (n :passed) (n :failed) (n :skipped))))

(defmethod parachute:eval-in-context :around ((report suite-report) (result parachute:test-result))
  (let ((fresh (eq :unknown (parachute:status result))))
    (prog1 (call-next-method)
      (when (and fresh (eq :skipped (parachute:status result)))
        (format (report-out report) "~&~(~A~): skipped, a suite it depends on did not pass~%"
                (parachute:name (parachute:expression result))))
      (when (and fresh (not (eq :skipped (parachute:status result))))
        (multiple-value-bind (passed failed skipped) (suite-counts result)
          (format (report-out report) "~&~(~A~): ~D passed, ~D failed~:[~;, ~:*~D skipped~] (~,2F s)~@[ [~A]~]~%"
                  (parachute:name (parachute:expression result)) passed failed
                  (when (plusp skipped) skipped) (parachute:duration result)
                  (when (and (eq :failed (parachute:status result)) (zerop failed))
                    (parachute:description result))))))))

(defun run-suite (report name)
  "Evaluate the Parachute test NAME into REPORT."
  (let* ((test (or (parachute:find-test name)
                   (error "No test named ~S: define it with DEFINE-CHECKS." name)))
         (parachute:*context* report)
         (result (parachute:result-for-testable test report)))
    (parachute:eval-in-context report result)
    (when (eq :failed (parachute:status result))
      (setf (parachute:status report) :failed))))

(defun call-with-suite-report (verbose function)
  "Call FUNCTION with a fresh SUITE-REPORT, then print the skipped checks and
the totals. Returns T when no check failed and no suite errored."
  (let* ((report (make-instance 'suite-report :expression 'suites :verbose verbose))
         (*skipped-checks* '()))
    (funcall function report)
    (let* ((suites (remove-if-not (lambda (r) (typep r 'parachute:test-result))
                                  (coerce (parachute:results report) 'list)))
           (broken (remove-if-not (lambda (s)
                                    (and (eq :failed (parachute:status s))
                                         (zerop (nth-value 1 (suite-counts s)))))
                                  suites))
           (failures (+ (length broken)
                        (reduce #'+ suites :key (lambda (s) (nth-value 1 (suite-counts s)))))))
      (when *skipped-checks*
        (format t "~&~D check(s) skipped on ~A: ~{~A~^; ~}~%" (length *skipped-checks*)
                (lisp-implementation-type) (reverse *skipped-checks*)))
      (setf *failed-suites*
            (mapcar (lambda (s) (parachute:name (parachute:expression s)))
                    (remove-if-not (lambda (s) (eq :failed (parachute:status s))) suites)))
      (when (plusp failures)
        (let ((failed (remove-if-not (lambda (s) (eq :failed (parachute:status s))) suites)))
          (when failed
            (format t "~&Failed suites: ~{~(~A~)~^ ~}~%"
                    (mapcar (lambda (s) (parachute:name (parachute:expression s))) failed)))))
      (format t "~&~[All checks passed~:;~:*~D failure(s)~]~%" failures)
      (zerop failures))))

(defun run-suite-list (names &key verbose)
  "Run the suites NAMES in order and print one line per suite, and a line for
each failed check (for every check when VERBOSE). Returns T when all pass."
  (call-with-suite-report verbose
                          (lambda (report)
                            (dolist (name names)
                              (run-suite report name)))))

(defun run-checks (serial parallel &key verbose)
  "Run the SERIAL suites with no lparallel kernel, then the PARALLEL ones
inside a 4-worker kernel, and print the summary. Returns T when every check
passed."
  (call-with-suite-report
   verbose
   (lambda (report)
     (let ((lp:*kernel* nil))
       (dolist (name serial)
         (run-suite report name))
       (setf lp:*kernel* (lp:make-kernel 4))
       ;; A full collection between suites: the LSM checks' path matrices
       ;; (80 MB each) otherwise fragment SBCL's default 1 GB heap.
       (unwind-protect (dolist (name parallel)
                         (run-suite report name)
                         (full-gc))
         (lp:end-kernel :wait t))))))

(defparameter *serial-checks*
  (list 'math-checks 'iteration-helper-checks 'interpolation-checks 'tridiagonal-checks 'time-checks 'calendar-checks 'schedule-checks 'syntax-checks)
  "Checks that run before an lparallel kernel exists.")

(defparameter *fast-checks*
  (list 'construction-checks 'rates-checks 'market-checks 'composite-market-checks 'dated-pricing-checks 'payoff-protocol-checks
        'define-payoff-checks 'option-language-checks 'exercise-mask-checks
        'cos-checks 'cos-term-structure-checks 'binomial-checks 'analytic-checks 'parameter-checks
        'error-checks 'condition-hierarchy-checks 'restart-checks 'lsm-basis-checks
        'lbr-reference-checks 'implied-vol-checks 'engine-implied-vol-checks
        'optimizer-checks 'bivariate-normal-checks 'platform-checks 'type-checks)
  "Checks that take seconds, not minutes.")

(defparameter *slow-checks*
  (list 'rates-instrument-checks 'mc-checks 'counter-stream-checks 'antithetic-se-checks 'process-checks 'levy-checks 'selection-test-checks 'term-structure-checks 'clarke-parrott-checks
        'greeks-checks 'invariant-checks 'option-round-trip-checks 'path-checks 'surface-checks 'fd-checks
        'dividend-checks 'portfolio-checks 'simulation-checks 'simulation-component-checks 'simulation-validation-checks 'simulation-book-checks 'measure-checks 'golden-checks)
  "Monte Carlo-heavy checks, the invariant sweep and the golden values.")

(defun run-tests (&key verbose)
  "Run every check. Prints a line per suite and per failed check, and a line
per check when VERBOSE. Returns T when all pass."
  (run-checks *serial-checks* (append *fast-checks* *slow-checks*) :verbose verbose))

(defun run-fast-tests (&key verbose)
  "Run every check except *SLOW-CHECKS*. Prints as RUN-TESTS does. Returns T
when all pass."
  (run-checks *serial-checks* *fast-checks* :verbose verbose))

(defun run-failed-tests (&key verbose)
  "Run again the suites that failed in the last RUN-TESTS, RUN-FAST-TESTS,
RUN-SUITE-LIST or RUN-FAILED-TESTS, serial ones with no kernel as before.
Returns T when they all pass, or when none failed."
  (let ((failed *failed-suites*))
    (if (null failed)
        (progn (format t "~&No suite failed in the last run.~%") t)
        (run-checks (remove-if-not (lambda (n) (member n *serial-checks*)) failed)
                    (remove-if (lambda (n) (member n *serial-checks*)) failed)
                    :verbose verbose))))
