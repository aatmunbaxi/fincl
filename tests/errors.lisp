;;;; errors.lisp --- conditions and restarts

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun error-checks ()
  "Unsupported combinations, refused time steps and the LSM restart."
  (with-standard-fixtures
    (format t "~&Errors and restarts~%")
    (handler-case
        (progn (price aput bs mkt2)
               (incf *failures*)
               (format t "~&  FAIL unsupported combination not signalled~%"))
      (unsupported-combination (c)
        (format t "~&  ok   unsupported: ~A~%" c)))
    ;; A discrete-time process refuses a time grid it cannot represent.
    (handler-case
        (progn (price call (mc :n-paths 1000 :n-steps 7
                               :process (make-instance 'garch))
                      mkt)
               (incf *failures*)
               (format t "~&  FAIL GARCH accepted a wrong time step~%"))
      (incompatible-time-step (c)
        (format t "~&  ok   ~A~%" c)))
    (handler-bind ((regression-failure
                     (lambda (c) (declare (ignore c))
                       (invoke-restart 'skip-exercise-date))))
      (price aput (mc :n-paths 10000) mkt2))
    (format t "~&  ok   regression-failure restart is available~%")))

;;; --------------------------------------------------------------------
;;; The hierarchy (task 10): every error is a FINCL-ERROR
;;; --------------------------------------------------------------------

(defun condition-of-thunk (thunk)
  (handler-case (progn (funcall thunk) nil)
    (error (c) c)))

(defun negative-cases ()
  "(label expected-type thunk): every former untyped signal, and the other
errors the suite provokes, each with the condition type it must signal."
  (with-standard-fixtures
    (let ((garch (make-instance 'garch :alpha 0.3d0 :beta 0.8d0))) ; persistence 1.175
      `(;; invalid input
        ("expiry that is not a date" invalid-exercise
         ,(lambda () (option (call 100) (expires 1))))
        ("tenor expiry without :from" invalid-exercise
         ,(lambda () (option (call 100) (expires #T"1Y"))))
        ("Bermudan with no dates" invalid-exercise
         ,(lambda () (make-instance 'bermudan-exercise :dates '())))
        ("non-real strike" invalid-argument
         ,(lambda () (make-instance 'vanilla-payoff :kind :call :strike "100")))
        ("option payoff that is not a payoff" invalid-argument
         ,(lambda () (make-instance 'option :payoff 3
                                           :exercise (make-instance 'european-exercise
                                                                    :expiry *today*))))
        ("non-real define-payoff slot" invalid-argument
         ,(lambda () (make-instance 'capped-call :kind :call :strike 100 :cap "30")))
        ("make-date with a non-integer" invalid-argument ,(lambda () (make-date 2027 6.5 1)))
        ("impossible date" invalid-date ,(lambda () (make-date 2027 2 30)))
        ("make-tenor with an unknown unit" invalid-argument
         ,(lambda () (make-tenor 3 :fortnights)))
        ("malformed tenor" invalid-tenor ,(lambda () (parse-tenor "3X")))
        ("#D literal with an impossible date" invalid-literal
         ,(lambda () (read-with-syntax "#D\"2027-02-30\"")))
        ("#3D literal" invalid-literal ,(lambda () (read-with-syntax "#3D\"2027-06-15\"")))
        ("#D not followed by a string" invalid-literal
         ,(lambda () (read-with-syntax "#D 20270615")))
        ("FFT length not a power of 2" invalid-argument
         ,(lambda () (fincl::fft! (make-array 6 :element-type '(complex double-float)
                                                :initial-element #c(0d0 0d0)))))
        ("Richardson ratio of 1" invalid-argument ,(lambda () (richardson-weights 2 :ratio 1)))
        ("Richardson exponents of the wrong count" invalid-argument
         ,(lambda () (richardson-weights 3 :exponents '(1))))
        ("Richardson exponents not increasing" invalid-argument
         ,(lambda () (richardson-weights 3 :exponents '(2 1))))
        ("finite differences, too few offsets" invalid-argument
         ,(lambda () (finite-difference-weights '(0 1) 2)))
        ("finite differences, repeated offsets" invalid-argument
         ,(lambda () (finite-difference-weights '(0 0 1) 1)))
        ("parameter out of bounds" invalid-parameter
         ,(lambda () (make-instance 'heston :rho 1.5d0)))
        ("unknown parameter" unknown-parameter
         ,(lambda () (process-parameter (make-instance 'heston) :lambda)))
        ("parameter vector of the wrong length" invalid-argument
         ,(lambda () (with-parameter-vector (make-instance 'heston) #(1d0 2d0))))
        ("unknown construction language" unknown-language
         ,(lambda () (fincl::find-language 'no-such-language)))
        ("a clause that is not a list" malformed-clause
         ,(lambda () (macroexpand-1 '(option 3))))
        ("&clauses after an optional parameter" malformed-clause
         ,(lambda () (macroexpand-1 '(define-clause option bad (x &optional y &clauses z) x))))
        ("ambiguous payoff clause" ambiguous-clause
         ,(lambda ()
            (let ((fincl::*formula-payoffs* (a:copy-hash-table fincl::*formula-payoffs*)))
              (setf (gethash (make-symbol "CAPPED-CALL") fincl::*formula-payoffs*) t)
              (fincl::resolve-payoff-clause "CAPPED-CALL"))))
        ("unknown clause" unknown-clause
         ,(lambda () (make-option '((:call 100) (:expiers #D"2027-09-24")))))
        ("conflicting clauses" conflicting-clauses
         ,(lambda () (make-option '((:call 100) (:put 100) (:expires #D"2027-09-24")))))
        ("LSM regressors not starting with the observable" invalid-argument
         ,(lambda () (price aput (mc :n-paths 1000 :regressors '(1)) mkt2)))
        ("option with no underlying on a two-underlying market" ambiguous-underlying
         ,(lambda () (price call bs (two-index-market))))
        ;; pricing errors
        ("option on an underlying the market lacks" missing-market-data
         ,(lambda () (price (option (call 100) (underlying :dax) (expires #D"2027-09-24")) bs
                            (two-index-market))))
        ("option paying in another currency than its underlying's" currency-mismatch
         ,(lambda () (price (option (call 100) (underlying :spx) (currency :eur)
                                    (expires #D"2027-09-24"))
                            bs (two-index-market))))
        ("zero volatility" non-positive-volatility
         ,(lambda () (price call bs (make-bs-market :valuation-date *today* :spot 100
                                                    :rate 0.05d0 :vol 0d0))))
        ("characteristic function with no variance" non-positive-volatility
         ,(lambda () (fincl::cumulants-from-cf (lambda (u) (declare (ignore u)) #c(1d0 0d0)))))
        ("non-stationary GARCH with a market-default omega" non-stationary-process
         ,(lambda () (price call (mc :n-paths 100 :n-steps 252 :process garch) mkt)))
        ("expired option" option-expired
         ,(lambda () (price (option (put 110) (expires #D"2026-09-23")) bs mkt)))
        ("Black-Scholes on an American" unsupported-combination ,(lambda () (price aput bs mkt2)))
        ("GARCH on a 7-step grid" incompatible-time-step
         ,(lambda () (price call (mc :n-paths 100 :n-steps 7 :process (make-instance 'garch))
                            mkt)))
        ("binomial tree with too few steps for the drift" invalid-tree
         ,(lambda () (price aput (binomial 2)
                            (make-bs-market :valuation-date *today* :spot 36 :rate 0.5d0
                                            :vol 0.2d0))))
        ("BAW with two exercise boundaries" model-domain-error
         ,(lambda () (price (option (put 100) (american) (expires #D"2027-09-24")) baw
                            (make-bs-market :valuation-date *today* :spot 100 :rate -0.005d0
                                            :dividend -0.02d0 :vol 0.25d0))))
        ("Brent without a sign change" convergence-failure
         ,(lambda () (solve-brent (lambda (x) (+ 1 (* x x))) -1d0 1d0)))))))

(defun condition-hierarchy-checks ()
  (format t "~&Condition hierarchy~%")
  (check-that "INVALID-INPUT and PRICING-ERROR are FINCL-ERRORs"
              (and (subtypep 'invalid-input 'fincl-error) (subtypep 'pricing-error 'fincl-error)
                   (subtypep 'construction-error 'invalid-input)
                   (subtypep 'invalid-argument 'type-error)
                   (subtypep 'invalid-literal 'reader-error)))
  (loop for (label type thunk) in (negative-cases)
        do (let ((c (condition-of-thunk thunk)))
             (check-that (format nil "~A signals ~(~A~), a ~(~A~)" label type
                                 (if (typep c 'invalid-input) 'invalid-input 'pricing-error))
                         (and (typep c type)
                              (typep c 'fincl-error)
                              (not (typep c 'simple-error))
                              ;; Every report is printable.
                              (plusp (length (princ-to-string c)))))))
  (check-that "one FINCL-ERROR clause catches every negative case"
              (every (lambda (case)
                       (eq :caught (handler-case (funcall (third case))
                                     (fincl-error () :caught))))
                     (negative-cases))))

;;; --------------------------------------------------------------------
;;; Restarts and policies (task 10)
;;; --------------------------------------------------------------------

(defun restart-checks ()
  (format t "~&Restarts and pricing policies~%")
  (with-standard-fixtures
    (let* ((garch (make-instance 'garch))
           (garch-mc (mc :n-paths 4000 :n-steps 252 :process garch))
           (garch-mc-wrong (mc :n-paths 4000 :n-steps 7 :process garch))
           (garch-cos (make-instance 'cos-engine :process garch))
           (drifting (make-bs-market :valuation-date *today* :spot 36 :rate 0.5d0 :vol 0.2d0))
           (expired (option (put 110) (expires #D"2026-09-23")))
           (flat-vol (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0d0)))
      (check-that "derive-engine copies every setting but the ones given"
                  (let* ((e (mc :n-paths 1234 :n-steps 9 :seed 7))
                         (d (derive-engine e :n-steps 20)))
                    (and (= 20 (n-steps d)) (= 9 (n-steps e))
                         (= 1234 (n-paths d)) (= 7 (seed d)))))
      (check "use-engine: COS on GARCH re-priced by Monte Carlo = the MC price"
             (price call garch-mc mkt)
             (handler-bind ((unsupported-combination (lambda (c) (use-engine garch-mc c))))
               (price call garch-cos mkt))
             0)
      (check "increase-steps: an invalid tree at N = 2 re-priced at N = 2000"
             (price aput (binomial 2000) drifting)
             (handler-bind ((invalid-tree (lambda (c) (increase-steps 2000 c))))
               (price aput (binomial 2) drifting))
             0)
      (check "use-process-period: GARCH at 7 steps re-priced at one step per day"
             (price call garch-mc mkt)
             (handler-bind ((incompatible-time-step (lambda (c) (use-process-period c))))
               (price call garch-mc-wrong mkt))
             0)
      (check-error "use-process-period is not offered when the expiry is not whole periods"
                   (handler-bind ((incompatible-time-step (lambda (c) (use-process-period c))))
                     (price (option (call 100) (expires (date+ *today* 100))) garch-mc-wrong mkt))
                   incompatible-time-step)
      (check "use-value on an expired option" 0d0
             (handler-bind ((option-expired (lambda (c) (use-value 0d0 c))))
               (price expired bs mkt))
             0)
      (check "use-value on a non-positive volatility" 1.5d0
             (handler-bind ((non-positive-volatility (lambda (c) (use-value 1.5d0 c))))
               (price call bs flat-vol))
             0)
      (check-error "use-engine is not offered for an expired option"
                   (handler-bind ((option-expired (lambda (c) (use-engine bs c))))
                     (price expired bs mkt))
                   option-expired)
      (let ((cos (make-instance 'cos-engine)))
        (multiple-value-bind (prices unwound)
            (block policy
              (handler-bind ((error (lambda (c) (declare (ignore c)) (return-from policy (values nil t)))))
                (values (with-pricing-policy ((unsupported-combination (use-engine garch-mc-wrong))
                                              (incompatible-time-step (use-process-period))
                                              (option-expired (use-value 0d0)))
                          (mapcar (lambda (pair) (price (first pair) (second pair) mkt))
                                  `((,call ,cos) (,(option (put 100) (american)
                                                           (expires #D"2027-09-24"))
                                                  ,cos)
                                    (,expired ,cos) (,call ,garch-cos))))
                        nil)))
          (check-that "with-pricing-policy prices a mixed list without unwinding"
                      (and (not unwound)
                           (= (first prices) (price call cos mkt))
                           (= (third prices) 0d0)
                           (= (fourth prices) (price call garch-mc mkt))))))
      (check-error "with-pricing-policy declines conditions it has no clause for"
                   (with-pricing-policy ((unsupported-combination (use-engine bs)))
                     (price expired bs mkt))
                   option-expired))))
