;;;; tests.lisp --- the suite runners
;;;;
;;;; Each area file defines an <area>-CHECKS function; the runners here call
;;;; them in order. Loaded last, so every checks function is already defined.

(in-package #:fincl/tests)

(defun run-checks (serial parallel)
  "Call the SERIAL check functions with no lparallel kernel, then the
PARALLEL ones inside a 4-worker kernel, and print the summary. Returns T
when every check passed."
  (let ((*failures* 0)
        (lp:*kernel* nil))
    (mapc #'funcall serial)
    (setf lp:*kernel* (lp:make-kernel 4))
    (unwind-protect (mapc #'funcall parallel)
      (lp:end-kernel :wait t))
    (format t "~&~[All checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))

(defparameter *serial-checks*
  (list 'math-checks 'interpolation-checks 'tridiagonal-checks 'time-checks 'calendar-checks 'schedule-checks 'syntax-checks)
  "Checks that run before an lparallel kernel exists.")

(defparameter *fast-checks*
  (list 'construction-checks 'market-checks 'composite-market-checks 'dated-pricing-checks 'payoff-protocol-checks
        'define-payoff-checks 'option-language-checks 'exercise-mask-checks
        'cos-checks 'cos-term-structure-checks 'binomial-checks 'analytic-checks 'parameter-checks
        'error-checks 'condition-hierarchy-checks 'restart-checks 'lsm-basis-checks
        'lbr-reference-checks 'implied-vol-checks 'engine-implied-vol-checks
        'optimizer-checks)
  "Checks that take seconds, not minutes.")

(defparameter *slow-checks*
  (list 'mc-checks 'counter-stream-checks 'antithetic-se-checks 'process-checks 'term-structure-checks 'clarke-parrott-checks
        'greeks-checks 'invariant-checks 'option-round-trip-checks 'golden-checks)
  "Monte Carlo-heavy checks, the invariant sweep and the golden values.")

(defun run-tests ()
  "Run every check. Returns T when all pass."
  (run-checks *serial-checks* (append *fast-checks* *slow-checks*)))

(defun run-fast-tests ()
  "Run every check except *SLOW-CHECKS*. Returns T when all pass."
  (run-checks *serial-checks* *fast-checks*))
