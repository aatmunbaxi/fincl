;;;; tests.lisp --- plain-CL checks, no test-framework dependency

(defpackage #:fincl/tests
  (:use #:cl #:fincl)
  (:local-nicknames (#:lp #:lparallel))
  (:export #:run-tests))

(in-package #:fincl/tests)

(defvar *failures* 0)

(defmacro check (name expected actual tolerance)
  `(let ((e (float ,expected 1d0)) (a (float ,actual 1d0)))
     (if (<= (abs (- e a)) ,tolerance)
         (format t "~&  ok   ~A (~,4F)~%" ,name a)
         (progn (incf *failures*)
                (format t "~&  FAIL ~A: expected ~,4F, got ~,4F~%" ,name e a)))))

(defmacro check-that (name form)
  `(if ,form
       (format t "~&  ok   ~A~%" ,name)
       (progn (incf *failures*) (format t "~&  FAIL ~A~%" ,name))))

(defun mc (&rest args)
  (apply #'make-instance 'monte-carlo-engine args))

(defun run-tests ()
  (let ((*failures* 0)
        (lp:*kernel* (lp:make-kernel 4)))
    (unwind-protect
         (let* ((mkt (make-bs-market :spot 100 :rate 0.05d0 :vol 0.2d0))
                (mkt2 (make-bs-market :spot 36 :rate 0.06d0 :vol 0.2d0))
                (bs (make-instance 'black-scholes-engine))
                (baw (make-instance 'barone-adesi-whaley-engine))
                (call (make-option '(:call 100 1)))
                (put (make-option '(:put 100 1)))
                (aput (make-option '(:american :put 40 1))))

           (format t "~&Market protocol~%")
           (check "discount factor" (exp -0.05d0) (discount-factor mkt 1d0) 1d-14)
           (check "forward" (* 100 (exp 0.05d0)) (forward mkt 1d0) 1d-12)
           (check "black vol" 0.2d0 (black-vol mkt 100d0 1d0) 1d-15)

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
           (let ((otm-put (make-option '(:put 80 1))))
             (check-that "CEV beta=0.5 dearer OTM put than GBM"
                         (> (price otm-put (mc :n-paths 200000 :n-steps 50
                                               :process (make-instance 'cev :beta 0.5d0))
                                   mkt)
                            (price otm-put (mc :n-paths 200000 :n-steps 50) mkt))))
           ;; Negative correlation in Heston does the same thing.
           (let ((otm-put (make-option '(:put 80 1))))
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
