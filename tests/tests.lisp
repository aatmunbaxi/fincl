;;;; tests.lisp --- plain-CL checks, no test-framework dependency

(defpackage #:fincl/tests
  (:use #:cl #:fincl)
  (:local-nicknames (#:a #:alexandria) (#:lp #:lparallel))
  (:export #:run-tests #:run-time-tests #:run-syntax-tests))

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

;;; --------------------------------------------------------------------
;;; Date-aware pricing
;;; --------------------------------------------------------------------

(defun error-message (thunk)
  "The report string of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(defun dated-pricing-checks ()
  (let* ((bs (make-instance 'black-scholes-engine))
         (baw (make-instance 'barone-adesi-whaley-engine))
         (engines (list bs baw)))
    (format t "~&Convention invariance (ACT/365F vs rescaled ACT/360)~%")
    ;; Rates and variance rescaled by 360/365, so D, F and w coincide.
    (let* ((scale (/ 360d0 365d0))
           (m365 (make-bs-market :valuation-date *today* :spot 100
                                 :rate 0.05d0 :dividend 0.02d0 :vol 0.25d0))
           (m360 (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                 :spot 100 :rate (* 0.05d0 scale)
                                 :dividend (* 0.02d0 scale)
                                 :vol (* 0.25d0 (sqrt scale)))))
      (dolist (spec '((:call 100 #D"2027-09-24") (:put 95 #D"2027-03-10")
                      (:american :put 110 #D"2027-09-24")
                      (:american :call 90 #D"2028-01-15")))
        (let ((o (make-option spec)))
          (dolist (e engines)
            (unless (and (typep e 'black-scholes-engine)
                         (typep (option-exercise o) 'american-exercise))
              (check (format nil "~A ~(~A~)" o (class-name (class-of e)))
                     (price o e m365) (price o e m360) 1d-12))))))

    (format t "~&Aging~%")
    ;; Valued half a year later, the option is the same as a fresh one
    ;; with the same number of days left.
    (let* ((m0 (make-bs-market :valuation-date *today* :spot 100
                               :rate 0.05d0 :dividend 0.02d0 :vol 0.25d0))
           (later #D"2027-03-24")
           (aged (derive-market m0 :valuation-date later))
           (days-left (date- #D"2027-09-24" later)))
      (dolist (style '(:european :american))
        (let ((old (make-vanilla-option :put 105 #D"2027-09-24" :exercise style))
              (fresh (make-vanilla-option :put 105 (date+ *today* days-left)
                                          :exercise style)))
          (check (format nil "~(~A~) put aged to ~A" style later)
                 (price fresh (if (eq style :american) baw bs) m0)
                 (price old (if (eq style :american) baw bs) aged)
                 1d-12)))
      (check-that "derive-market leaves the original unchanged"
                  (date= (valuation-date m0) *today*)))

    (format t "~&Expiry~%")
    (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
      (dolist (e (list bs baw (mc :n-paths 1000)))
        (check (format nil "at expiry, ~(~A~) returns the payoff" (class-name (class-of e)))
               10d0
               (price (make-option `(:american :put 110 ,*today*)) e m)
               0))
      (let ((expired (make-option '(:put 110 #D"2026-09-23"))))
        (check-error "past expiry" (price expired bs m) option-expired)
        (check-that "option-expired report names both dates"
                    (let ((msg (error-message (lambda () (price expired bs m)))))
                      (and (search "2026-09-23" msg) (search "2026-09-24" msg))))))

    (format t "~&Expiries are dates~%")
    (check-error "bare-number expiry" (make-option '(:call 100 1)))
    (check-that "bare-number expiry error explains dates"
                (search "not a date" (error-message
                                      (lambda () (make-option '(:call 100 1))))))
    (check-error "bare-number expiry, make-vanilla-option"
                 (make-vanilla-option :call 100 0.5d0))

    (format t "~&Tenor expiries~%")
    (flet ((expiry-of (option) (expiry (option-exercise option))))
      (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
            (weekends (make-instance 'weekends-only)))
        (check-equal "1Y from 2026-09-24 is stored as a date" #D"2027-09-24"
                     (expiry-of (make-option '(:american :call 100 #T"1Y") :from *today*))
                     :test (lambda (e a) (and (typep a 'date) (date= e a))))
        (check-error "tenor without :from" (make-option '(:call 100 #T"1Y")))
        (check-that "tenor without :from error asks for a start date"
                    (search ":FROM" (error-message
                                     (lambda () (make-option '(:call 100 #T"1Y"))))))
        ;; 2026-09-26 is a Saturday, so 1Y later is Sunday 2027-09-26.
        (check-equal "unadjusted without a calendar" #D"2027-09-26"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"))
                     :test #'date=)
        (check-equal "calendar, following" #D"2027-09-27"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"
                                                                 :calendar weekends))
                     :test #'date=)
        (check-equal "calendar, preceding" #D"2027-09-24"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"
                                                                 :calendar weekends
                                                                 :convention :preceding))
                     :test #'date=)
        (check-equal "end of month" #D"2027-03-31"
                     (expiry-of (make-option '(:call 100 #T"1M") :from #D"2027-02-28"
                                                                 :end-of-month t))
                     :test #'date=)
        (check-equal "make-vanilla-option with a tenor" #D"2027-03-24"
                     (expiry-of (make-vanilla-option :put 100 #T"6M" :from *today*
                                                     :exercise :american))
                     :test #'date=)
        (check "tenor-built option prices like the date-built one"
               (price (make-option '(:call 100 #D"2027-09-24")) bs m)
               (price (make-option '(:call 100 #T"1Y") :from *today*) bs m)
               0)
        (let ((o (make-option '(:call 100 #T"1Y") :from *today*)))
          (check-that "expiry does not move when the market ages"
                      (and (date= (expiry-of o) #D"2027-09-24")
                           (price o bs (derive-market m :valuation-date #D"2027-03-24"))
                           (date= (expiry-of o) #D"2027-09-24"))))
        (check-equal "a strike ladder from one tenor" 101
                     (length (loop :for strike :from 50 :upto 150
                                   :collect (make-option `(:american :call ,strike #T"1Y")
                                                         :from *today*))))))

    (format t "~&Exercise~%")
    (let ((b (make-instance 'bermudan-exercise
                            :dates (list #D"2027-09-24" #D"2027-03-24"))))
      (check-equal "bermudan expiry is the last date" #D"2027-09-24" (expiry b)
                   :test #'date=)
      (check-that "bermudan exercise-allowed-p"
                  (and (exercise-allowed-p b #D"2027-03-24")
                       (not (exercise-allowed-p b #D"2027-03-25"))))
      (check-that "european and american exercise-allowed-p"
                  (let ((eu (make-instance 'european-exercise :expiry #D"2027-09-24"))
                        (am (make-instance 'american-exercise :expiry #D"2027-09-24")))
                    (and (exercise-allowed-p eu #D"2027-09-24")
                         (not (exercise-allowed-p eu #D"2027-03-24"))
                         (exercise-allowed-p am #D"2027-03-24")
                         (not (exercise-allowed-p am #D"2027-09-25"))))))))

;; Defined in time.lisp and syntax.lisp, which load after this file.
(declaim (ftype function time-checks syntax-checks))

(defun run-tests ()
  (let ((*failures* 0)
        (lp:*kernel* nil))
    (time-checks)
    (syntax-checks)
    (setf lp:*kernel* (lp:make-kernel 4))
    (unwind-protect
         (let* ((mkt (make-bs-market :valuation-date *today*
                                     :spot 100 :rate 0.05d0 :vol 0.2d0))
                (mkt2 (make-bs-market :valuation-date *today*
                                      :spot 36 :rate 0.06d0 :vol 0.2d0))
                (bs (make-instance 'black-scholes-engine))
                (baw (make-instance 'barone-adesi-whaley-engine))
                (call (make-option '(:call 100 #D"2027-09-24")))
                (put (make-option '(:put 100 #D"2027-09-24")))
                (aput (make-option '(:american :put 40 #D"2027-09-24"))))

           (format t "~&Market protocol~%")
           (check "market time to 2027-09-24" 1d0 (market-time mkt #D"2027-09-24") 0)
           (check "discount factor" (exp -0.05d0) (discount-factor mkt 1d0) 1d-14)
           (check "discount factor, date" (exp -0.05d0)
                  (discount-factor mkt #D"2027-09-24") 1d-14)
           (check "forward" (* 100 (exp 0.05d0)) (forward mkt 1d0) 1d-12)
           (check "forward, date" (* 100 (exp 0.05d0))
                  (forward mkt #D"2027-09-24") 1d-12)
           (check "black vol" 0.2d0 (black-vol mkt 100d0 1d0) 1d-15)
           (check "black variance, date" 0.04d0
                  (black-variance mkt 100d0 #D"2027-09-24") 1d-15)

           (dated-pricing-checks)

           (format t "~&Analytic~%")
           (check "BS call" 10.4506 (price call bs mkt) 1d-4)
           (check "BS put" 5.5735 (price put bs mkt) 1d-4)
           (check "put-call parity" (- 100 (* 100 (exp -0.05d0)))
                  (- (price call bs mkt) (price put bs mkt)) 1d-12)
           (check "BAW American put" 4.46 (price aput baw mkt2) 0.02)

           (format t "~&Monte Carlo, GBM (benchmarks 10.4506 / 4.478)~%")
           (check "European, terminal sampler" 10.4506
                  (price call (mc :n-paths 400000) mkt) 0.03)
           (check "European, PCG stream" 10.4506
                  (price call (mc :n-paths 400000 :rng :pcg) mkt) 0.03)
           (check "European, inverse transform" 10.4506
                  (price call (mc :n-paths 400000 :sampler :inverse-transform) mkt)
                  0.03)
           (check "LSM American put" 4.478
                  (price aput (mc :n-paths 100000 :n-steps 50) mkt2) 0.02)
           (check-that "thread-count invariance"
                       (= (price aput (mc :n-paths 20000) mkt2)
                          (let ((lp:*kernel* (lp:make-kernel 1)))
                            (unwind-protect (price aput (mc :n-paths 20000) mkt2)
                              (lp:end-kernel :wait t)))))

           (format t "~&Processes: each must reduce to GBM in its degenerate case~%")
           ;; CEV with beta = 1 is GBM. Stepped, so it also checks that the
           ;; generic path simulation agrees with the exact terminal sampler.
           (check "CEV beta=1 == BS" 10.4506
                  (price call (mc :n-paths 200000 :n-steps 25
                                  :process (make-instance 'cev :beta 1d0))
                         mkt)
                  0.04)
           ;; Heston with zero vol-of-vol and v0 = theta is GBM.
           (check "Heston xi=0 == BS" 10.4506
                  (price call (mc :n-paths 200000 :n-steps 25
                                  :process (make-instance 'heston :v0 0.04d0
                                                                  :theta 0.04d0
                                                                  :xi 0d0))
                         mkt)
                  0.04)
           ;; GARCH with no ARCH/GARCH/leverage terms is GBM.
           (let ((period (/ 1d0 50d0)))
             (check "GARCH flat == BS" 10.4506
                    (price call (mc :n-paths 200000 :n-steps 50
                                    :process (make-instance 'garch
                                                            :period period
                                                            :alpha 0d0 :beta 0d0
                                                            :gamma 0d0
                                                            :omega (* 0.04d0 period)
                                                            :h0 (* 0.04d0 period)))
                           mkt)
                    0.04))

           (format t "~&Processes: qualitative behaviour~%")
           ;; beta < 1 raises local vol as spot falls: fatter left tail, so an
           ;; out-of-the-money put is worth more than under GBM.
           (let ((otm-put (make-option '(:put 80 #D"2027-09-24"))))
             (check-that "CEV beta=0.5 dearer OTM put than GBM"
                         (> (price otm-put (mc :n-paths 200000 :n-steps 50
                                               :process (make-instance 'cev :beta 0.5d0))
                                   mkt)
                            (price otm-put (mc :n-paths 200000 :n-steps 50) mkt))))
           ;; Negative correlation in Heston does the same thing.
           (let ((otm-put (make-option '(:put 80 #D"2027-09-24"))))
             (check-that "Heston rho<0 dearer OTM put than xi=0"
                         (> (price otm-put (mc :n-paths 200000 :n-steps 50
                                               :process (make-instance 'heston
                                                                       :v0 0.04d0 :theta 0.04d0
                                                                       :xi 0.5d0 :rho -0.8d0))
                                   mkt)
                            (price otm-put (mc :n-paths 200000 :n-steps 50
                                               :process (make-instance 'heston
                                                                       :v0 0.04d0 :theta 0.04d0
                                                                       :xi 0d0))
                                   mkt))))
           ;; American pricing works under any process, not just GBM.
           (check-that "LSM under Heston returns a sane price"
                       (let ((v (price aput (mc :n-paths 50000 :n-steps 25
                                                :process (make-instance 'heston
                                                                        :v0 0.04d0
                                                                        :theta 0.04d0))
                                       mkt2)))
                         (and (> v 4d0) (< v 5d0))))

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
           (format t "~&  ok   regression-failure restart is available~%")

           (format t "~&~[All checks passed~:;~:*~D failure(s)~]~%" *failures*)
           (zerop *failures*))
      (lp:end-kernel :wait t))))
