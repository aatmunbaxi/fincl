;;;; processes.lisp --- stochastic processes

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun process-checks ()
  "Degenerate cases reduce to GBM; qualitative behaviour of the parameters."
  (with-standard-fixtures
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
                  (and (> v 4d0) (< v 5d0))))))

;;; --------------------------------------------------------------------
;;; Parameter metaobjects and DEFINE-PROCESS
;;; --------------------------------------------------------------------

(defun toy-default-c (process market horizon)
  (declare (ignore process market))
  (* 2d0 horizon))

(define-process toy-base (stochastic-process) ()
  ((a 1d0 :bounds (0 10))
   (b 0.5d0 :bounds (-1 1))
   (c nil :market-default toy-default-c)))

(define-process toy-child (toy-base) ()
  ((a 2d0 :bounds (0 5))))            ; tighter bounds; B and C are inherited

;;; A user-defined process: the class and its stepper are all it takes.
;;; Log-spot is pulled back to its starting level at rate PULL; PULL = 0 is
;;; GBM.
(define-process log-ou (stochastic-process) ()
  ((pull 1d0 :bounds (0 nil) :doc "Rate at which log-spot reverts to its start.")
   (vol 0.2d0 :bounds ((0) nil))))

(defmethod make-stepper ((p log-ou) market dt)
  (let* ((pull (pull p)) (vol (vol p)) (x0 (log (spot market)))
         (drift (* (- (zero-rate market dt) (dividend-yield market dt) (* 0.5d0 vol vol)) dt))
         (diffusion (* vol (sqrt dt))))
    (lambda (state normals)
      (let ((x (log (aref state 0))))
        (setf (aref state 0) (exp (+ x drift (- (* pull (- x x0) dt))
                                     (* diffusion (aref normals 0)))))))))

(defun parameter-checks ()
  (format t "~&Process parameters~%")
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
    (flet ((names (process) (mapcar #'symbol-name (process-parameters process))))
      (check-equal "heston parameters in declaration order" '("V0" "KAPPA" "THETA" "XI" "RHO")
                   (names 'heston))
      (check-equal "a garch period is a setting, not a parameter"
                   '("ALPHA" "BETA" "GAMMA" "OMEGA" "H0") (names 'garch))
      (check-equal "a subclass inherits its parent's parameters" '("A" "B" "C")
                   (sort (names 'toy-child) #'string<)))
    (check-equal "a subclass overrides its parent's bounds" '(0 5)
                 (multiple-value-list (parameter-bounds 'toy-child 'a)))
    (check-equal "the parent keeps its own bounds" '(0 10)
                 (multiple-value-list (parameter-bounds 'toy-base 'a)))
    (check-equal "unoverridden bounds are inherited" '(-1 1)
                 (multiple-value-list (parameter-bounds 'toy-child 'b)))
    (check-that "a value inside the parent's bounds but outside the child's"
                (and (make-instance 'toy-base :a 7)
                     (handler-case (progn (make-instance 'toy-child :a 7) nil)
                       (invalid-parameter () t))))
    (check-error "rho outside [-1, 1]" (make-instance 'heston :rho 1.5d0) invalid-parameter)
    (check-error "kappa at its exclusive bound 0" (make-instance 'heston :kappa 0) invalid-parameter)
    (check-error "a parameter that is not a number" (make-instance 'heston :xi "high")
                 invalid-parameter)
    (check-error "NIL without a market default" (make-instance 'heston :kappa nil)
                 invalid-parameter)
    (check-that "the report names the parameter and its bounds"
                (search "rho = 1.5d0 is outside [-1, 1]"
                        (error-message (lambda () (make-instance 'heston :rho 1.5d0)))))
    (check-that "integers are coerced to double-float"
                (eql 3d0 (process-parameter (make-instance 'heston :kappa 3) :kappa)))
    (check-that "the printed form lists parameters and market defaults"
                (let ((printed (prin1-to-string (make-instance 'heston :xi 0.5d0))))
                  (and (search "v0=market" printed) (search "xi=0.5" printed))))
    (let* ((heston (make-instance 'heston :xi 0.5d0 :rho -0.6d0))
           (v (parameter-vector heston m))
           (copy (with-parameter-vector heston v)))
      (check-equal "parameter-vector resolves market defaults" 0.04d0 (aref v 0)
                   :test (lambda (e a) (< (abs (- e a)) 1d-15)))
      (check-that "with-parameter-vector round trip"
                  (equalp v (parameter-vector copy m)))
      (check-that "with-parameter-vector leaves the original unchanged"
                  (and (null (process-parameter heston :v0))
                       (= (process-parameter copy :v0) (aref v 0)))))
    (let ((heston (make-instance 'heston :rho -0.5d0)))
      (check-that "process-parameter by keyword, by symbol and by fincl's own symbol"
                  (= -0.5d0 (process-parameter heston :rho) (process-parameter heston 'rho)
                     (process-parameter heston 'fincl::rho)))
      (check-that "process-parameter returns NIL for a market default without a market"
                  (null (process-parameter heston :v0)))
      (check "process-parameter resolves a market default given a market" 0.04d0
             (process-parameter heston :v0 m) 1d-15)
      (check "process-parameter reads settings too" (/ 1d0 252d0)
             (process-parameter (make-instance 'garch) :period) 0)
      (check-error "process-parameter with an unknown name" (process-parameter heston :lambda))
      (check-that "the unknown-name error lists the parameters"
                  (search ":kappa" (error-message (lambda () (process-parameter heston :lambda)))))
      (check-equal "parameter-bounds by keyword" '(-1 1)
                   (multiple-value-list (parameter-bounds 'heston :rho))))
    (check-equal "a market default receives the horizon" 3d0
                 (aref (parameter-vector (make-instance 'toy-base) m 1.5d0) 2))
    (format t "~&A process defined in a test file~%")
    (multiple-value-bind (v se)
        (price (make-option '(:call 100 #D"2027-09-24"))
               (mc :n-paths 50000 :n-steps 20 :process (make-instance 'log-ou :pull 0))
               m)
      (check "log-ou with pull = 0 is GBM" 10.4506 v (* 4 se)))
    (check-that "log-ou with pull > 0 prices lower: mean reversion cuts variance"
                (< (price (make-option '(:call 100 #D"2027-09-24"))
                          (mc :n-paths 50000 :n-steps 20 :process (make-instance 'log-ou :pull 2))
                          m)
                   10))))
