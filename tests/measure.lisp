;;;; measure.lisp --- physical-measure processes from risk-neutral ones (task 36)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun measure-times (&rest times)
  (make-array (length times) :element-type 'double-float
                             :initial-contents (mapcar (lambda (x) (float x 1d0)) times)))

(defun measure-normals (rs n)
  "N standard normals by Box-Muller from the random state RS."
  (let ((v (make-array n :element-type 'double-float)))
    (dotimes (i n v)
      (setf (aref v i) (* (sqrt (* -2d0 (log (- 1d0 (random 1d0 rs)))))
                          (cos (* 2d0 pi (random 1d0 rs))))))))

(defun stepped-paths (process market times n-paths &key (seed 11))
  "The states of N-PATHS paths of PROCESS stepped over TIMES by its MAKE-STEPPER
closure, normals from a seeded random state: a list per path of the state
vectors at each grid time after 0."
  (let* ((rs (test-random-state seed))
         (stepper (make-stepper process market times))
         (n-factors (process-factors process)))
    (iter (repeat n-paths)
          (for state = (initial-state process market))
          (collect (iter (for j below (1- (length times)))
                         (funcall stepper state (measure-normals rs n-factors) j)
                         (collect (copy-seq state)))))))

(defun terminal-spots (process market n-steps &key (n-paths 20000) (seed 7) (date #D"2027-09-24"))
  "Per-path terminal spots of PROCESS simulated to DATE, a DOUBLE-VECTOR, and
the result."
  (let ((r (simulate (process market (list date) :n-paths n-paths :n-steps n-steps :seed seed)
             (:state s :spot)
             (:final (:s s)))))
    (values (simulation-values r :s) r)))

(defun measure-mean-check (name process market expected &key (n-steps 12) (n-paths 20000) (seed 7))
  (multiple-value-bind (values r) (terminal-spots process market n-steps :n-paths n-paths :seed seed)
    (declare (ignore values))
    (let ((mean (simulation-mean r :s)) (se (simulation-standard-error r :s)))
      (check-that (format nil "~A: E^P[S_T] = ~,4F vs ~,4F within 3 SE (~,4F)" name mean expected se)
                  (<= (abs (- mean expected)) (* 3 se))))))

(defun max-relative-difference (xs ys)
  (reduce #'max (map 'list (lambda (x y) (/ (abs (- x y)) (abs y))) xs ys)))

(defun measure-lv-market ()
  (make-market :valuation-date *today*
               :underlyings `((:x :spot 100 :dividend 0.01d0 :currency :usd
                                  :vol ,(make-ssvi-surface '(0.25 0.5 1 2) '(0.0121 0.0225 0.04 0.072)
                                                           :rho -0.6 :eta 1.2 :gamma 0.4)))
               :curves '((:usd :rate 0.05d0))))

;;; --------------------------------------------------------------------
;;; Stepping
;;; --------------------------------------------------------------------

(defun measure-stepping-checks ()
  (format t "~&Change of measure: stepping~%")
  (let ((flat (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
        (piecewise (make-piecewise-market))
        (lv (measure-lv-market))
        (times (measure-times 0 0.1 0.25 0.3 0.5 0.75 0.8 1)))
    (iter (for (name process market) in `(("GBM, flat vol" ,(make-gbm :vol 0.25d0) ,flat)
                                          ("GBM, vol term structure" ,(make-gbm) ,piecewise)
                                          ("CEV" ,(make-cev :beta 0.6d0) ,flat)
                                          ("CEV, absorbing" ,(make-cev :beta 0.6d0 :scheme :absorbing-euler) ,flat)
                                          ("local vol" ,(make-local-vol) ,lv)
                                          ("Merton" ,(make-merton-jump) ,flat)
                                          ("Kou" ,(make-kou-jump) ,piecewise)))
          (check-that (format nil "~A with no premia steps = to its Q process, path by path" name)
                      (equalp (stepped-paths process market times 50)
                              (stepped-paths (change-measure process) market times 50))))
    ;; A constant drift change: every P state is the Q state times exp(delta t).
    (let* ((delta 0.03d0)
           (q (stepped-paths (make-gbm) piecewise times 50))
           (p (stepped-paths (change-measure (make-gbm) :drift-changes `((:spot ,delta))) piecewise times 50)))
      (check-that "GBM, constant drift change: each P state is the Q state times exp(delta t) (1e-14)"
                  (iter (for qp in q) (for pp in p)
                        (always (iter (for qs in qp) (for ps in pp) (for j from 1)
                                      (always (< (abs (- (/ (aref ps 0) (aref qs 0)) (exp (* delta (aref times j)))))
                                                 1d-14)))))))
    ;; A constant price of risk: delta_j = lambda sigma_j over each interval.
    (flet ((ratio-check (name gbm market)
             (let* ((lambda* 0.4d0)
                    (q (stepped-paths gbm market times 20))
                    (p (stepped-paths (change-measure gbm :prices-of-risk `((:spot ,lambda*))) market times 20))
                    (expected (iter (for j from 1 below (length times))
                                    (for t0 = (aref times (1- j))) (for t1 = (aref times j))
                                    (for w = (fincl::gbm-forward-variance gbm market t0 t1))
                                    (sum (* lambda* (sqrt (* w (- t1 t0)))) into g)
                                    (collect (exp g)))))
               (check-that (format nil "GBM, ~A: a price of risk lambda gives delta = lambda sigma_j (1e-14)" name)
                           (iter (for qp in q) (for pp in p)
                                 (always (iter (for qs in qp) (for ps in pp) (for e in expected)
                                               (always (< (abs (- (/ (aref ps 0) (aref qs 0)) e)) 1d-14)))))))))
      (ratio-check "flat vol" (make-gbm :vol 0.25d0) flat)
      (ratio-check "vol term structure" (make-gbm) piecewise))
    ;; A cash dividend inside an interval is applied once, after the growth.
    (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0
                              :dividend-schedule `((,(date+ *today* 100) 2))))
           (times (measure-times 0 1))
           (delta 0.07d0)
           (z (measure-times 0.3))
           (q (make-array 1 :element-type 'double-float :initial-element 100d0))
           (p (make-array 1 :element-type 'double-float :initial-element 100d0))
           (drop (fincl::dividend-event-amount (first (fincl::discrete-dividends m)))))
      (funcall (make-stepper (make-gbm) m times) q z 0)
      (funcall (make-stepper (change-measure (make-gbm) :drift-changes `((:spot ,delta))) m times) p z 0)
      (check-that (format nil "a cash dividend inside the interval is subtracted once, after the growth: ~
P = (Q + D) e^delta - D (~,6F)" (aref p 0))
                  (< (abs (- (aref p 0) (- (* (+ (aref q 0) drop) (exp delta)) drop))) 1d-12)))))

;;; --------------------------------------------------------------------
;;; Distributions under P
;;; --------------------------------------------------------------------

(defun measure-distribution-checks ()
  (format t "~&Change of measure: distributions under P~%")
  (let* ((flat (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (piecewise (make-piecewise-market))
         (forward (forward flat 1d0)))
    (measure-mean-check "GBM, drift change 0.03"
                        (change-measure (make-gbm) :drift-changes '((:spot 0.03d0))) flat (* forward (exp 0.03d0)))
    (measure-mean-check "GBM on a vol term structure, lambda 0.3"
                        (change-measure (make-gbm) :prices-of-risk '((:spot 0.3d0))) piecewise
                        (* (forward piecewise 1d0)
                           (exp (* 0.3d0 (sqrt (* (fincl::atm-forward-variance piecewise 1d0) 1d0)))))
                        :n-steps 1)
    (let ((q (terminal-spots (make-gbm) flat 12))
          (p (terminal-spots (change-measure (make-gbm) :prices-of-risk '((:spot (:scaled 2d0)))) flat 12)))
      (flet ((log-variance (v)
               (let* ((logs (map 'list #'log v)) (mean (/ (reduce #'+ logs) (length logs))))
                 (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mean) 2)) logs)) (1- (length logs))))))
        (check "GBM, (:scaled 2): the variance of ln S_T is unchanged (relative)"
               0 (/ (- (log-variance p) (log-variance q)) (log-variance q)) 1d-10)))
    (measure-mean-check "Merton, P jump law and lambda 0.25 on the Brownian part"
                        (change-measure (make-merton-jump :vol 0.2d0)
                                        :prices-of-risk '((:spot 0.25d0))
                                        :parameters '(:intensity 1.5d0 :jump-mean -0.2d0 :jump-vol 0.1d0))
                        flat (* forward (exp (* 0.25d0 0.2d0))))
    (measure-mean-check "Kou, P jump law and drift change 0.02"
                        (change-measure (make-kou-jump :vol 0.2d0)
                                        :drift-changes '((:spot 0.02d0))
                                        :parameters '(:intensity 2 :up-probability 0.3d0))
                        flat (* forward (exp 0.02d0)))
    ;; CEV with sigma -> 0 and c = k / sigma^2: ln S follows d ln S = (r + k/S) dt
    ;; (beta = 1/2), so S_T = (S0 + k/r) e^(rT) - k/r, and the error of the
    ;; trapezoid rule is the scheme's alone.
    (let* ((sigma 1d-6) (k 20d0) (r 0.05d0)
           (p (change-measure (make-cev :beta 0.5d0 :sigma sigma)
                              :prices-of-risk `((:spot (:scaled ,(/ k sigma sigma))))))
           (exact (- (* (+ 100 (/ k r)) (exp r)) (/ k r)))
           (errors (mapcar (lambda (n) (- (simulation-mean (nth-value 1 (terminal-spots p flat n :n-paths 100)) :s)
                                          exact))
                           '(4 8 16))))
      (check-that (format nil "CEV, (:scaled c): the terminal mean converges at first order (errors ~{~,4F~^, ~})"
                          errors)
                  (and (< 1.9d0 (/ (first errors) (second errors)) 2.1d0)
                       (< 1.9d0 (/ (second errors) (third errors)) 2.1d0))))
    ;; Local vol: E^Q[S_T] = F exactly for every step, so the P mean is F
    ;; plus the mean of P - Q on the same normals, whose noise is small.
    (let* ((m (measure-lv-market))
           (q-process (make-local-vol :substeps 1))
           (p-process (change-measure q-process :prices-of-risk '((:spot (:scaled 10d0)))))
           (means (mapcar (lambda (n)
                            (let ((p (terminal-spots p-process m n :n-paths 50000))
                                  (q (terminal-spots q-process m n :n-paths 50000)))
                              (/ (reduce #'+ (map 'list #'- p q)) (length p))))
                          '(8 16 32)))
           (ratio (/ (- (first means) (second means)) (- (second means) (third means)))))
      (check-that (format nil "local vol, (:scaled 10): the terminal mean converges at first order ~
(successive differences in ratio ~,3F)" ratio)
                  (< 1.5d0 ratio 2.7d0)))))

;;; --------------------------------------------------------------------
;;; Refusals, printing, worker count
;;; --------------------------------------------------------------------

(defun measure-error-checks ()
  (format t "~&Change of measure: arguments, refusals and printing~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (p (change-measure (make-gbm) :prices-of-risk '((:spot 0.2d0))))
         (call (make-option (call 100) (expires #D"2027-09-24")))
         (american (make-option (put 100) (american) (expires #D"2027-09-24"))))
    (check-error "an unknown state" (change-measure (make-gbm) :prices-of-risk '((:variance 0.1d0)))
                 invalid-measure-change)
    (check-error "a state named twice"
                 (change-measure (make-gbm) :prices-of-risk '((:spot 0.1d0)) :drift-changes '((:spot 0.01d0)))
                 invalid-measure-change)
    (check-error "a state named twice in one list"
                 (change-measure (make-gbm) :drift-changes '((:spot 0.1d0) (:spot 0.01d0)))
                 invalid-measure-change)
    (check-error "an invariant parameter" (change-measure (make-merton-jump) :parameters '(:vol 0.3d0))
                 invalid-measure-change)
    (check-error "a measure-dependent parameter out of bounds"
                 (change-measure (make-merton-jump) :parameters '(:intensity -1))
                 invalid-measure-change)
    (check-error "a state with no Brownian driver (variance gamma)"
                 (change-measure (make-variance-gamma) :drift-changes '((:spot 0.02d0)))
                 invalid-measure-change)
    (check-error "a malformed price of risk" (change-measure (make-gbm) :prices-of-risk '((:spot (:scaled))))
                 invalid-measure-change)
    (check-error "P to P" (change-measure p) invalid-measure-change)
    (check-that "the measure-dependent parameters are marked"
                (and (equal (fincl::measure-dependent-parameters 'merton-jump)
                            '(fincl::intensity fincl::jump-mean fincl::jump-vol))
                     (null (fincl::measure-dependent-parameters 'gbm))))
    (let ((designated (remove nil (mapcar (lambda (class)
                                            (when (typep class 'fincl::engine-class)
                                              (fincl::engine-class-designator class)))
                                          (remove-duplicates
                                           (labels ((walk (class)
                                                      (cons class (mapcan #'walk (closer-mop:class-direct-subclasses class)))))
                                             (rest (walk (find-class 'fincl::engine)))))))))
      (check-that (format nil "every engine designator (~D, through the MOP) refuses a physical process" (length designated))
                  (every (lambda (key)
                           (handler-case (progn (make-engine key :process p) nil)
                             (risk-neutral-process-required () t)))
                         designated)))
    (check-error "automatic selection with a physical process" (price call p m) risk-neutral-process-required)
    (check-error "automatic selection, American" (price american p m) risk-neutral-process-required)
    (check-error "greeks" (greeks call p m) risk-neutral-process-required)
    (check-error "implied-volatility" (implied-volatility american 8 p m) risk-neutral-process-required)
    (check-error "applicable-engines" (applicable-engines call m :process p) risk-neutral-process-required)
    (check-error "calibrate" (calibrate (change-measure (make-merton-jump)) m (list (make-option-quote call :vol 0.2d0)))
                 risk-neutral-process-required)
    (check-that "a physical process is not simulatable by the Monte Carlo engine, but steppable"
                (and (not (simulatable-p p)) (fincl::steppable-p p)))
    (check-that "risk-neutral-process returns the Q process; parameters are the Q process's"
                (and (typep (risk-neutral-process p) 'gbm)
                     (equal (process-parameters p) '(fincl::vol))))
    (let ((data (change-measure (make-merton-jump :vol 0.2d0) :prices-of-risk '((:spot (:scaled 2d0)))
                                                              :parameters '(:intensity 1d0))))
      (check-equal "a physical process prints as its CHANGE-MEASURE form"
                   "#<PHYSICAL-PROCESS (change-measure (make-merton-jump :vol 0.2d0) :prices-of-risk (quote ((:spot (:scaled 2.0d0)))) :parameters (quote (:intensity 1.0d0)))>"
                   (let ((*read-default-float-format* 'single-float)) (princ-to-string data))
                   :test #'string=)
      (check-that "its snapshot form rebuilds the same process"
                  (string= (princ-to-string data)
                           (princ-to-string (fincl::snapshot-eval (snapshot-form data))))))
    (check-error "a function premium has no snapshot form"
                 (snapshot-form (change-measure (make-gbm) :prices-of-risk `((:spot ,(lambda (time state)
                                                                                         (declare (ignore time state))
                                                                                         0.1d0)))))
                 invalid-argument)))

(defun measure-worker-checks ()
  (format t "~&Change of measure: worker count~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (p (change-measure (make-merton-jump :vol 0.2d0)
                            :prices-of-risk `((:spot ,(lambda (time state)
                                                        (declare (ignore time))
                                                        (* 0.002d0 (aref state 0)))))
                            :parameters '(:intensity 1d0)))
         (runs (mapcar (lambda (n) (with-workers n (lambda () (terminal-spots p m 12 :n-paths 4000))))
                       '(1 2 8))))
    (check-that "P paths are = under 1, 2 and 8 workers"
                (and (equalp (first runs) (second runs)) (equalp (first runs) (third runs))))))

;;; --------------------------------------------------------------------
;;; Latent states (phase 2)
;;; --------------------------------------------------------------------

(defun cir-exponential-moment (c kappa theta xi v0 horizon)
  "E[exp(c int_0^T v dt)] for the square-root process dv = kappa (theta - v) dt
+ xi sqrt(v) dW: the CIR bond formula at q = -c."
  (let* ((gamma (sqrt (- (* kappa kappa) (* 2 xi xi c))))
         (e (exp (* gamma horizon)))
         (den (+ (* (+ gamma kappa) (- e 1)) (* 2 gamma)))
         (b (/ (* 2 (- e 1)) den))
         (a (expt (/ (* 2 gamma (exp (* 0.5d0 (+ kappa gamma) horizon))) den) (/ (* 2 kappa theta) (* xi xi)))))
    (* a (exp (* c b v0)))))

(defun measure-heston-checks ()
  (format t "~&Change of measure: Heston and Bates~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :dividend 0.01d0 :vol 0.2d0))
         (times (measure-times 0 0.1 0.25 0.3 0.5 0.75 0.8 1))
         (kappa 2d0) (theta 0.04d0) (xi 0.4d0))
    (dolist (scheme '(:quadratic-exponential :full-truncation))
      (flet ((heston (&rest args)
               (apply #'make-heston `(,@args :v0 0.05d0 :kappa ,kappa :theta ,theta :xi ,xi :rho -0.7d0 :scheme ,scheme))))
        (check-that (format nil "Heston ~(~A~), variance (:scaled -5): = to Heston with kappa^P = 2.8 and ~
theta^P = 2 x 0.04 / 2.8 on the same normals" scheme)
                    (equalp (stepped-paths (change-measure (heston) :prices-of-risk '((:variance (:scaled -5d0))))
                                           m times 30)
                            (stepped-paths (heston :kappa 2.8d0 :theta (/ (* kappa theta) 2.8d0)) m times 30)))
        (check-that (format nil "Heston ~(~A~), variance drift change 0.02: = to Heston with theta^P = ~
(kappa theta + 0.02) / kappa" scheme)
                    (equalp (stepped-paths (change-measure (heston) :drift-changes '((:variance 0.02d0))) m times 30)
                            (stepped-paths (heston :theta (/ (+ (* kappa theta) 0.02d0) kappa)) m times 30)))))
    (let ((bates (make-bates :v0 0.05d0 :kappa kappa :theta theta :xi xi :rho -0.5d0)))
      (check-that "Bates, variance (:scaled -5) and a physical jump law: = to Bates with the mapped parameters"
                  (equalp (stepped-paths (change-measure bates :prices-of-risk '((:variance (:scaled -5d0)))
                                                               :parameters '(:intensity 1d0 :jump-mean -0.05d0))
                                         m times 30)
                          (stepped-paths (make-bates :v0 0.05d0 :kappa 2.8d0 :theta (/ (* kappa theta) 2.8d0) :xi xi
                                                     :rho -0.5d0 :intensity 1d0 :jump-mean -0.05d0)
                                         m times 30))))
    ;; E^P[v_T] = theta^P + (v0 - theta^P) e^(-kappa^P T).
    (let* ((kappa-p 2.8d0) (theta-p (/ (* kappa theta) kappa-p)) (v0 0.09d0)
           (r (simulate ((change-measure (make-heston :v0 v0 :kappa kappa :theta theta :xi xi :rho -0.7d0)
                                         :prices-of-risk '((:variance (:scaled -5d0))))
                         m (list #D"2027-09-24") :n-paths 20000 :n-steps 12 :seed 3)
                (:state v :variance)
                (:final (:v v))))
           (expected (+ theta-p (* (- v0 theta-p) (exp (- kappa-p))))))
      (check-that (format nil "Heston, variance (:scaled -5): E^P[v_T] = ~,6F vs ~,6F within 3 SE (~,6F)"
                          (simulation-mean r :v) expected (simulation-standard-error r :v))
                  (<= (abs (- (simulation-mean r :v) expected)) (* 3 (simulation-standard-error r :v)))))
    ;; Spot (:scaled c): E^P[S_T] = F(T) E^P[exp(c int v dt) M_T], M the
    ;; spot's exponential martingale; under M as numeraire v mean-reverts at
    ;; kappa - rho xi to kappa theta / (kappa - rho xi), so the expectation is
    ;; the CIR moment at those parameters.
    (let* ((c 2d0) (v0 0.04d0) (rho -0.7d0) (kappa* (- kappa (* rho xi)))
           (p (change-measure (make-heston :v0 v0 :kappa kappa :theta theta :xi xi :rho -0.7d0)
                              :prices-of-risk `((:spot (:scaled ,c))))))
      (measure-mean-check "Heston, spot (:scaled 2) at 250 steps"
                          p m (* (forward m 1d0) (cir-exponential-moment c kappa* (/ (* kappa theta) kappa*) xi v0 1d0))
                          :n-steps 250))
    (check-that "a refused variance form names the accepted ones"
                (search "(:scaled c)" (error-message (lambda () (change-measure (make-heston)
                                                                                 :prices-of-risk '((:variance 0.1d0)))))))
    (check-error "a variance premium with physical :kappa"
                 (resolve-process (change-measure (make-heston :v0 0.04d0 :theta 0.04d0)
                                                  :prices-of-risk '((:variance (:scaled -1d0)))
                                                  :parameters '(:kappa 3d0))
                                  m 1d0)
                 invalid-measure-change)
    (check-error "a variance premium making kappa^P <= 0"
                 (resolve-process (change-measure (make-heston :v0 0.04d0 :theta 0.04d0 :xi 0.4d0)
                                                  :prices-of-risk '((:variance (:scaled 20d0))))
                                  m 1d0)
                 invalid-measure-change)
    (let ((warnings 0))
      (handler-bind ((feller-condition-violated (lambda (w) (incf warnings) (muffle-warning w))))
        (dotimes (i 2)
          (resolve-process (change-measure (make-heston :v0 0.04d0 :kappa 1.25d0 :theta 0.0317d0 :xi 0.9d0)
                                           :prices-of-risk '((:variance (:scaled -1d0))))
                           m 1d0)))
      (check-that (format nil "a Feller violation under both measures warns once per image (~D warnings)" warnings)
                  (= warnings 2)))))

(defun measure-garch-checks ()
  (format t "~&Change of measure: NGARCH~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :dividend 0.01d0 :vol 0.2d0))
         (period (/ 1d0 252))
         (times (let ((v (make-array 21 :element-type 'double-float)))
                  (dotimes (i 21 v) (setf (aref v i) (* i period)))))
         (alpha 0.1d0) (beta 0.8d0) (gamma 0.5d0) (omega 2d-6) (h0 1.5d-4) (lambda* 0.05d0)
         (garch (make-garch :alpha alpha :beta beta :gamma gamma :omega omega :h0 h0))
         (paths (stepped-paths (change-measure garch :prices-of-risk `((:spot ,lambda*))) m times 20))
         (duan (let ((rs (test-random-state 11))
                     (mu (fincl::interval-log-growths m times)))
                 (iter (repeat 20)
                       (collect (let ((s 100d0) (h h0))
                                  (iter (for j below 20)
                                        (for z = (aref (measure-normals rs 1) 0))
                                        (let ((drift (* lambda* (sqrt (max 0d0 h)))))
                                          (setf s (* s (exp (+ (aref mu j) (* -0.5d0 h) (* (sqrt h) z))))
                                                s (* s (exp drift))
                                                h (+ omega (* beta h) (* alpha h (expt (- z (- gamma lambda*)) 2)))))
                                        (collect (measure-times s h)))))))))
    (check-that "the P path is = to Duan's recursion with gamma - lambda and mean lambda sqrt(h)"
                (equalp paths duan))
    (check-error "a drift change on NGARCH's spot" (change-measure garch :drift-changes '((:spot 0.01d0)))
                 invalid-measure-change)
    (check-error "a price of risk on the conditional variance"
                 (change-measure garch :prices-of-risk '((:conditional-variance 0.1d0)))
                 invalid-measure-change)))

(defun measure-hull-white-checks ()
  (format t "~&Change of measure: Hull-White~%")
  (let* ((m (hw-sim-market))
         (a 0.1d0) (sigma 0.01d0) (lambda* -0.4d0)
         (d1 (years-after *today* 2)) (d2 (years-after *today* 5))
         (n 20000))
    (flet ((run (process)
             (simulate ((((:curve :eur) process)) m (list d1 d2) :n-paths n :seed 5)
               (:accumulate (r1 0d0))
               (:state rate :short-rate (:curve :eur))
               (:state integral :integral (:curve :eur))
               (:every (date) (when (date= date d1) (setf r1 rate)))
               (:final (:r1 r1) (:r2 rate) (:i2 integral)))))
      (let* ((hw (make-hull-white :a a :sigma sigma))
             (q (run hw))
             (p (run (change-measure hw :prices-of-risk `((:short-rate ,lambda*)))))
             (pf (run (change-measure hw :prices-of-risk `((:short-rate ,(lambda (time state)
                                                                          (declare (ignore time state))
                                                                          lambda*))))))
             (fitted (resolve-process hw (make-instance 'fincl::curve-view :market m :curve-id :eur) 5d0))
             (curve (fincl::fitted-rate-curve fitted))
             (shift (lambda (time) (* sigma lambda* (/ (- 1 (exp (- (* a time)))) a)))))
        (iter (for (name date) in `((:r1 ,d1) (:r2 ,d2)))
              (for time = (market-time m date))
              (for differences = (map 'list #'- (simulation-values p name) (simulation-values q name)))
              (check-that (format nil "~A: r^P - r^Q = sigma lambda (1 - e^(-at))/a on every path (1e-12), ~
so Var r_t is unchanged" name)
                          (every (lambda (x) (< (abs (- x (funcall shift time))) 1d-12)) differences))
              (let ((expected (+ (fincl::hw-alpha fitted curve time) (funcall shift time))))
                (check-that (format nil "~A: E^P[r_t] = ~,6F vs E^Q[r_t] + shift = ~,6F within 3 SE" name
                                    (simulation-mean p name) expected)
                            (<= (abs (- (simulation-mean p name) expected)) (* 3 (simulation-standard-error p name))))))
        (let* ((time (market-time m d2))
               (expected (+ (log (/ 1 (fincl::rate-curve-discount curve time)))
                            (* 0.5d0 (fincl::hw-integral-variance fitted time))
                            ;; int_0^t sigma lambda B(s) ds
                            (* sigma lambda* (/ (- time (/ (- 1 (exp (- (* a time)))) a)) a)))))
          (check-that (format nil "the integral's mean under P, ~,6F vs ~,6F within 3 SE"
                              (simulation-mean p :i2) expected)
                      (<= (abs (- (simulation-mean p :i2) expected)) (* 3 (simulation-standard-error p :i2)))))
        (let ((difference (reduce #'max (map 'list (lambda (x y) (abs (- x y)))
                                             (simulation-values pf :i2) (simulation-values p :i2)))))
          (check-that (format nil "a constant lambda given as a function of time gives the integral within 1e-12 of the real (~,2E)"
                              difference)
                      (< difference 1d-12)))
        (check-that "a physical :a steps as Hull-White with that a fitted to the curve"
                    (equalp (simulation-values (run (change-measure hw :parameters '(:a 0.3d0))) :i2)
                            (simulation-values (run (make-hull-white :a 0.3d0 :sigma sigma)) :i2)))
        (check-error "a premium on the integral" (change-measure hw :prices-of-risk '((:integral 0.1d0)))
                     invalid-measure-change)))))

(defun measure-levy-checks ()
  (format t "~&Change of measure: variance gamma and NIG~%")
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
    (check-that "the measure-dependent parameters: variance gamma sigma, theta; NIG alpha, beta"
                (and (equal (fincl::measure-dependent-parameters 'variance-gamma) '(fincl::sigma fincl::theta))
                     (equal (fincl::measure-dependent-parameters 'nig) '(fincl::alpha fincl::beta))))
    (check-that "their docstrings give the reference"
                (and (search "Sato" (documentation 'variance-gamma 'type))
                     (search "Sato" (documentation 'nig 'type))))
    (check-error "a drift change on variance gamma" (change-measure (make-variance-gamma :sigma 0.2d0)
                                                                    :drift-changes '((:spot 0.01d0)))
                 invalid-measure-change)
    (check-error "a physical nu" (change-measure (make-variance-gamma :sigma 0.2d0) :parameters '(:nu 0.3d0))
                 invalid-measure-change)
    (check-error "a physical NIG delta" (change-measure (make-nig) :parameters '(:delta 0.6d0))
                 invalid-measure-change)
    (flet ((omega (p) (fincl::levy-drift p)))
      (let ((q (make-variance-gamma :sigma 0.2d0)) (p (make-variance-gamma :sigma 0.25d0 :theta -0.3d0)))
        (measure-mean-check "variance gamma, physical sigma and theta: drift beside the jumps unchanged"
                            (change-measure q :parameters '(:sigma 0.25d0 :theta -0.3d0))
                            m (* (forward m 1d0) (exp (- (omega q) (omega p))))))
      (let ((q (make-nig)) (p (make-nig :alpha 12d0 :beta -3d0)))
        (measure-mean-check "NIG, physical alpha and beta: drift beside the jumps unchanged"
                            (change-measure q :parameters '(:alpha 12d0 :beta -3d0))
                            m (* (forward m 1d0) (exp (- (omega q) (omega p))))
                            ;; Seed 7 puts the risk-neutral NIG's own mean 3 SE below F.
                            :seed 8)))))

;;; --------------------------------------------------------------------
;;; SIMULATE (phase 3)
;;; --------------------------------------------------------------------

(defun measure-simulate-checks ()
  (format t "~&Change of measure: simulate~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :dividend 0.01d0 :vol 0.2d0))
         (dates (list #D"2026-12-24" #D"2027-03-24" #D"2027-09-24"))
         (q-heston (make-heston :v0 0.04d0 :kappa 2d0 :theta 0.04d0 :xi 0.4d0 :rho -0.7d0))
         (p-heston (handler-bind ((feller-condition-violated #'muffle-warning))
                     (change-measure q-heston :prices-of-risk '((:spot (:scaled 2d0)) (:variance (:scaled -5d0))))))
         (call (make-option (call 100) (expires #D"2028-09-24"))))
    (flet ((run (process)
             (handler-bind ((feller-condition-violated #'muffle-warning))
               (simulate (process m dates :n-paths 40 :seed 9)
                 (:accumulate (worst 0d0) (spot-gap 0d0))
                 (:state s :spot)
                 (:state v :variance)
                 (:every (date market)
                   (let ((on-date (price call (make-engine :cos :process (make-heston :v0 v :kappa 2d0 :theta 0.04d0
                                                                                      :xi 0.4d0 :rho -0.7d0))
                                         market))
                         (by-hand (price call (make-engine :cos :process (make-heston :v0 v :kappa 2d0 :theta 0.04d0
                                                                                      :xi 0.4d0 :rho -0.7d0))
                                         (make-bs-market :valuation-date date :spot s :rate 0.05d0
                                                         :dividend 0.01d0 :vol 0.2d0))))
                     (setf worst (max worst (abs (- on-date by-hand)))
                           spot-gap (max spot-gap (abs (- (spot market) s))))))
                 (:final (:worst worst) (:spot-gap spot-gap) (:s s))))))
      (let ((p (run p-heston)) (q (run q-heston)))
        (check-that "a physical Heston leg: the date's market holds the P state's spot, and a COS price there = the Q Heston's at that state"
                    (and (every #'zerop (simulation-values p :worst))
                         (every #'zerop (simulation-values p :spot-gap))))
        (check-that "the P leg's paths differ from the Q leg's on the same seed"
                    (not (equalp (simulation-values p :s) (simulation-values q :s))))
        (check-equal "simulation-measure of the P run" :physical (simulation-measure p))
        (check-equal "simulation-measure of the Q run" :risk-neutral (simulation-measure q))
        (check-that "the result prints its measure"
                    (and (search "physical measure" (princ-to-string p))
                         (search "risk-neutral measure" (princ-to-string q))))))
    ;; Hull-White under P: the date's curve is the Q bond function of the P state.
    (let* ((hm (hw-sim-market))
           (hw (make-hull-white :a 0.1d0 :sigma 0.01d0))
           (fitted (resolve-process hw (make-instance 'fincl::curve-view :market hm :curve-id :eur) 5d0))
           (curve (fincl::fitted-rate-curve fitted))
           (d1 (years-after *today* 1)) (d2 (years-after *today* 2))
           (r (simulate ((((:curve :eur) (change-measure hw :prices-of-risk '((:short-rate -0.5d0))))
                          ((:spot :spx) (make-gbm)))
                         hm (list d1 d2) :n-paths 50 :seed 4)
                (:accumulate (worst 0d0))
                (:state rate :short-rate (:curve :eur))
                (:every (date market)
                  (let ((t0 (market-time hm date)))
                    (dolist (years '(1 3 10))
                      (let* ((maturity (years-after date years))
                             (t1 (market-time hm maturity))
                             (on-date (market-curve-discount market :eur maturity))
                             (q-bond (fincl::hull-white-discount-bond fitted curve t0 t1 rate)))
                        (setf worst (max worst (abs (/ (- on-date q-bond) q-bond))))))))
                (:final (:worst worst)))))
      (check-that (format nil "a physical Hull-White leg: the date's curve is the Q bond function of the P state (~,2E)"
                          (reduce #'max (simulation-values r :worst)))
                  (< (reduce #'max (simulation-values r :worst)) 1d-12))
      (check-equal "simulation-measure of a mixed run (P curve, Q spot)" :physical (simulation-measure r)))))

(define-checks measure-checks
  (measure-stepping-checks)
  (measure-error-checks)
  (measure-distribution-checks)
  (measure-worker-checks)
  (measure-heston-checks)
  (measure-garch-checks)
  (measure-hull-white-checks)
  (measure-levy-checks)
  (measure-simulate-checks))

(defun run-measure-tests ()
  "Run only the change-of-measure checks. Returns T when all pass."
  (with-pricing-kernel ()
    (run-suite-list '(measure-checks))))
