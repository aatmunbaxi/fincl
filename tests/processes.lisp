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
    (let ((otm-put (make-option (put 80) (expires #D"2027-09-24"))))
      (check-that "CEV beta=0.5 dearer OTM put than GBM"
                  (> (price otm-put (mc :n-paths 200000 :n-steps 50
                                        :process (make-instance 'cev :beta 0.5d0))
                            mkt)
                     (price otm-put (mc :n-paths 200000 :n-steps 50) mkt))))
    ;; Negative correlation in Heston does the same thing.
    (let ((otm-put (make-option (put 80) (expires #D"2027-09-24"))))
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

(defmethod make-stepper ((p log-ou) market times)
  (let ((pull (pull p)) (vol (vol p)) (x0 (log (spot market))))
    (lambda (state normals j)
      (let* ((t0 (aref times j)) (dt (- (aref times (1+ j)) t0))
             (x (log (aref state 0))))
        (setf (aref state 0)
              (exp (+ x (fincl::log-growth market t0 (+ t0 dt)) (* -0.5d0 vol vol dt)
                      (- (* pull (- x x0) dt)) (* vol (sqrt dt) (aref normals 0)))))))))

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
        (price (make-option (call 100) (expires #D"2027-09-24"))
               (mc :n-paths 50000 :n-steps 20 :process (make-instance 'log-ou :pull 0))
               m)
      (check "log-ou with pull = 0 is GBM" 10.4506 v (* 4 se)))
    (check-that "log-ou with pull > 0 prices lower: mean reversion cuts variance"
                (< (price (make-option (call 100) (expires #D"2027-09-24"))
                          (mc :n-paths 50000 :n-steps 20 :process (make-instance 'log-ou :pull 2))
                          m)
                   10))))

;;; --------------------------------------------------------------------
;;; Term structure (task 08)
;;; --------------------------------------------------------------------

;;; GBM without its exact terminal sampler, so European prices go through
;;; the stepper.
(define-process stepped-gbm (gbm) () ())
(defmethod terminal-sampler ((p stepped-gbm) market time)
  (declare (ignore market time))
  nil)

(defun martingale-violations (process market &key (n-paths 200000) (n-steps 8) (horizon 1d0))
  "Grid times where the simulated mean of S differs from the forward by more
than 4 standard errors. Antithetic pairs are averaged first, so the standard
error is that of the pair means."
  (let* ((paths (fincl::simulate-paths (mc :n-paths n-paths :n-steps n-steps :process process)
                                       market horizon))
         (n (fincl::path-matrix-n-paths paths))
         (half (/ n 2))
         (data (fincl::path-matrix-data paths)))
    (iter (for j from 0 to n-steps)
          (for time = (* j (/ horizon n-steps)))
          (for pairs = (iter (for i below half)
                             (collect (* 0.5d0 (+ (aref data (+ i (* j n)))
                                                  (aref data (+ i half (* j n))))))))
          (for mean = (/ (reduce #'+ pairs) half))
          (for se = (sqrt (/ (/ (reduce #'+ pairs :key (lambda (x) (expt (- x mean) 2)))
                                (1- half))
                             half)))
          (unless (<= (abs (- mean (forward market time))) (+ (* 4 se) 1d-12))
            (collect (list time mean (forward market time) se))))))

(defun term-structure-checks ()
  (format t "~&Term structure: processes on a piecewise market~%")
  (let ((pm (make-piecewise-market))
        (flat (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
    (dolist (case (list (list "GBM" (make-instance 'gbm))
                        (list "Heston, xi = 0" (make-instance 'heston :xi 0d0))
                        (list "CEV, beta = 1" (make-instance 'cev :beta 1d0))))
      (destructuring-bind (name process) case
        (let ((violations (martingale-violations process pm)))
          (check-that (format nil "~A: mean S(t) = F(t) at every grid time, 200k paths" name)
                      (null violations))
          (when violations (format t "~&       ~S~%" violations)))))
    ;; Black's formula with the market's integrated D, F and w to expiry.
    (let* ((call (make-option (call 100) (expires #D"2027-09-24")))
           (exact (fincl::black-formula 1d0 (forward pm 1d0) 100d0 (discount-factor pm 1d0)
                                        (black-variance pm 100d0 1d0))))
      (multiple-value-bind (v se)
          (price call (mc :n-paths 200000 :n-steps 8 :process (make-instance 'stepped-gbm)) pm)
        (check "stepped GBM European = Black with integrated rates and variance"
               exact v (* 3 se)))
      (multiple-value-bind (v se) (price call (mc :n-paths 200000) pm)
        (check "terminal-sampled GBM European = Black on the same market" exact v (* 3 se))))
    (format t "~&Term structure: resolved processes~%")
    (let ((gbm (make-instance 'gbm)) (heston (make-instance 'heston)))
      (check-that "GBM with NIL vol resolves to itself: NIL is the term-structure model"
                  (eq gbm (resolve-process gbm pm 1d0)))
      (let ((resolved (resolve-process heston pm 2d0)))
        (check "Heston v0 resolves to w(2)/2 at the ATM forward"
               (/ (black-variance pm (forward pm 2d0) 2d0) 2d0)
               (process-parameter resolved :v0) 1d-15)
        (check "Heston theta resolves to v0" (process-parameter resolved :v0)
               (process-parameter resolved :theta) 0)
        (check-that "a two-year Heston has a different v0 from a one-year one"
                    (/= (process-parameter resolved :v0)
                        (process-parameter (resolve-process heston pm 1d0) :v0)))
        (check-that "resolving is idempotent" (eq resolved (resolve-process resolved pm 2d0)))
        (check-that "the original process is unchanged" (null (process-parameter heston :v0))))
      (check-that "on a flat market the Heston default does not depend on the expiry"
                  (= (process-parameter (resolve-process heston flat 1d0) :v0)
                     (process-parameter (resolve-process heston flat 2d0) :v0))))
    (check-error "falling total variance is calendar arbitrage"
                 (make-stepper (make-instance 'gbm)
                               (make-instance 'inverted-variance-market
                                              :valuation-date *today* :spot 100d0
                                              :knots '(0.25d0 0.5d0 1d0)
                                              :rates '(0.02d0 0.02d0 0.02d0 0.02d0)
                                              :dividends '(0d0 0d0 0d0 0d0)
                                              :vols '(0.2d0 0.2d0 0.2d0 0.2d0))
                               (fincl::uniform-time-grid 1d0 4))
                 calendar-arbitrage)))

(defun antithetic-se-checks ()
  (format t "~&Antithetic standard errors~%")
  ;; The stepped path and the terminal sampler both average antithetic
  ;; pairs before the statistics, so they report the same estimator's SE.
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
        (call (make-option (call 100) (expires #D"2027-09-24"))))
    (multiple-value-bind (v-terminal se-terminal) (price call (mc :n-paths 200000) m)
      (declare (ignore v-terminal))
      (multiple-value-bind (v-stepped se-stepped)
          (price call (mc :n-paths 200000 :process (make-instance 'stepped-gbm)) m)
        (declare (ignore v-stepped))
        (check-that (format nil "stepped SE ~,5F within 5% of terminal-sampler SE ~,5F"
                            se-stepped se-terminal)
                    (< (abs (- (/ se-stepped se-terminal) 1d0)) 0.05d0))))))
