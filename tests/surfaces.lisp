;;;; surfaces.lisp --- vol surfaces, smile fits, local vol and calibration (task 17)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun surface-market (&key (vol 0.2d0) (rate 0.05d0) (dividend 0.01d0))
  "Spot 100, flat rate and dividend; VOL a real or a vol structure."
  (make-market :valuation-date *today*
               :underlyings `((:x :spot 100 :dividend ,dividend :vol ,vol :currency :usd))
               :curves `((:usd :rate ,rate))))

(defun days-out (days) (add-tenor *today* (make-tenor days :days)))

(defun quotes-from (vol-at market day-list strikes &key price)
  "Quotes for out-of-the-money options (puts below the forward, calls above)
on every DAY-LIST x STRIKES, at the vol (funcall VOL-AT k tau) for k =
ln(K/F(tau)): by Black price with PRICE, else by vol."
  (iter outer
        (for days in day-list)
        (for expiry = (days-out days))
        (for tau = (market-time market expiry))
        (for f = (forward market expiry))
        (iter (for k in strikes)
              (for vol = (funcall vol-at (log (/ k f)) tau))
              (for phi = (if (< k f) -1d0 1d0))
              (for option = (make-option-from-clauses `((,(if (< k f) :put :call) ,k) (:expires ,expiry))))
              (in outer
                  (collect (if price
                               (make-option-quote option :price (fincl::black-formula phi f k (discount-factor market expiry)
                                                                                      (* vol vol tau)))
                               (make-option-quote option :vol vol)))))))

(defun strikes-between (lo hi n)
  (iter (for i below n) (collect (+ lo (* i (/ (- hi lo) (1- n)))))))

;;; --------------------------------------------------------------------
;;; Phase 1: quotes and the grid surface
;;; --------------------------------------------------------------------

(defun grid-surface-checks ()
  (format t "~&Quotes and grid surfaces~%")
  (let* ((m (surface-market))
         (quotes (quotes-from (constantly 0.2d0) m '(30 91 182 365 730) (strikes-between 60 160 11)
                              :price t))
         (surface (quotes-vol-surface (quote-set quotes) m)))
    (check-that "quote vols are the Black implied vols of their prices"
                (every (lambda (q) (< (abs (- (quote-implied-vol q m) 0.2d0)) 1d-13)) quotes))
    (check-that "a grid surface from Black prices at a flat 20% gives 0.2 to 1e-12 inside the grid"
                (iter outer
                      (for tau in (strikes-between (market-time m (days-out 30)) (market-time m (days-out 730)) 23))
                      (iter (for k in (strikes-between -0.4d0 0.3d0 29))
                            (in outer (always (< (abs (- (sqrt (/ (surface-variance surface k tau) tau)) 0.2d0))
                                                 1d-12))))))
    (let ((market (surface-market :vol surface)))
      (check "a market holding the surface prices a European at its vol"
             (price (make-option (call 110) (expires (days-out 200))) :black-scholes m)
             (price (make-option (call 110) (expires (days-out 200))) :black-scholes market)
             1d-10)))
  ;; Interpolating a known SVI surface.
  (let* ((m (surface-market))
         (svi (make-svi-surface
               (iter (for (tau a b rho mm sigma) in '((0.25 0.01 0.06 -0.4 0.02 0.15)
                                                       (0.5 0.02 0.08 -0.4 0.02 0.18)
                                                       (1 0.04 0.1 -0.35 0.03 0.2)))
                     (collect (make-svi-slice tau :a a :b b :rho rho :m mm :sigma sigma)))))
         (knots (strikes-between -0.5d0 0.5d0 21))
         (grid (make-grid-vol-surface '(0.25 0.5 1) knots
                                      (iter (for tau in '(0.25 0.5 1))
                                            (collect (mapcar (lambda (k) (sqrt (/ (surface-variance svi k tau) tau)))
                                                             knots))))))
    (declare (ignorable m))
    (let ((worst (iter outer
                       (for tau in '(0.25 0.4 0.5 0.8 1))
                       (iter (for k in (strikes-between -0.475d0 0.475d0 20))
                             (in outer (maximize (abs (- (sqrt (/ (surface-variance grid k tau) tau))
                                                         (sqrt (/ (surface-variance svi k tau) tau))))))))))
      (check-that (format nil "a grid through an SVI surface interpolates between knots within 1e-4 in vol (worst ~,2E)"
                          worst)
                  (< worst 1d-4))))
  (check-error "a grid surface whose total variance falls in T"
               (make-grid-vol-surface '(0.5 1) '(-0.1 0 0.1) '((0.3 0.3 0.3) (0.2 0.2 0.2)))
               calendar-arbitrage)
  (check-error "a moneyness surface read without a forward"
               (structure-black-variance (make-grid-vol-surface '(1) '(-0.1 0 0.1) '((0.2 0.2 0.2))) 100 1)
               invalid-argument))

;;; --------------------------------------------------------------------
;;; Phase 2: SVI, SSVI, SABR
;;; --------------------------------------------------------------------

(defun smile-fit-checks ()
  (format t "~&SVI, SSVI and SABR~%")
  (let* ((m (surface-market))
         (days 365)
         (tau (market-time m (days-out days)))
         (truth (make-svi-slice tau :a 0.02 :b 0.12 :rho -0.45 :m 0.05 :sigma 0.25))
         (quotes (quotes-from (lambda (k tau) (declare (ignore tau)) (smile-vol truth k))
                              m `(,days) (strikes-between 50 180 25))))
    (multiple-value-bind (fit rmse) (fit-svi (quote-set quotes) m)
      (let ((error (reduce #'max (mapcar (lambda (x y) (abs (- x y)))
                                         (fincl::svi-parameters fit) (fincl::svi-parameters truth)))))
        (check-that (format nil "SVI fit recovers (a b rho m sigma) to 1e-6 (worst ~,2E, vol RMSE ~,2E)"
                            error rmse)
                    (< error 1d-6))
        (check-that "... and the fitted slice is free of butterfly arbitrage"
                    (butterfly-arbitrage-free-p fit)))))
  ;; Gatheral and Jacquier (2014), Example 3.1 (Axel Vogt): checked against
  ;; the paper, (a, b, m, rho, sigma) = (-0.0410, 0.1331, 0.3586, 0.3060,
  ;; 0.4153) at t = 1, with g < 0 for k between about 0.7 and 1.5.
  (let ((vogt (make-svi-slice 1 :a -0.0410 :b 0.1331 :m 0.3586 :rho 0.3060 :sigma 0.4153)))
    (multiple-value-bind (free k g) (butterfly-arbitrage-free-p vogt :range 1.5d0)
      (check-that (format nil "Vogt's slice has butterfly arbitrage: g(~,3F) = ~,4F" k g)
                  (and (not free) (< 0.6d0 k 1.5d0)))))
  ;; SSVI on five expiries.
  (let* ((m (surface-market))
         (day-list '(30 91 182 365 730))
         (truth (make-ssvi-surface (mapcar (lambda (d) (market-time m (days-out d))) day-list)
                                   '(0.0035 0.0105 0.02 0.04 0.075)
                                   :rho -0.55 :eta 1.1 :gamma 0.35))
         (quotes (quotes-from (lambda (k tau) (sqrt (/ (surface-variance truth k tau) tau)))
                              m day-list (strikes-between 70 140 9))))
    (multiple-value-bind (fit rmse) (fit-ssvi (quote-set quotes) m)
      (check-that (format nil "SSVI fit on 5 expiries: vol RMSE ~,2E < 1e-6 (rho ~,4F eta ~,4F gamma ~,4F)"
                          rmse (fincl::ssvi-rho fit) (fincl::ssvi-eta fit) (fincl::ssvi-gamma fit))
                  (< rmse 1d-6))
      (check-that "... and SSVI is free of calendar arbitrage"
                  (calendar-arbitrage-free-p fit))))
  ;; SABR against QuantLib's sabrVolatility (Hagan et al. 2002).
  (let ((rows (first (read-fixture "sabr.sexp"))))
    (check-that (format nil "Hagan lognormal and normal SABR vols match QuantLib's sabrVolatility to 1e-8 (~D cases)"
                        (length rows))
                (every (lambda (row)
                         (destructuring-bind (f tau alpha beta nu rho k lognormal normal) row
                           (and (< (abs (- (sabr-volatility k f tau alpha beta rho nu :hagan) lognormal)) 1d-8)
                                (< (abs (- (sabr-volatility k f tau alpha beta rho nu :normal) normal)) 1d-8))))
                       rows)))
  (check-that "Obloj's SABR equals Hagan's at beta = 1 and at the money"
              (and (< (abs (- (sabr-volatility 120 100 2 0.25 1 -0.4 0.8 :obloj)
                              (sabr-volatility 120 100 2 0.25 1 -0.4 0.8 :hagan)))
                      1d-14)
                   (< (abs (- (sabr-volatility 0.03 0.03 1 0.02 0.5 -0.3 0.4 :obloj)
                              (sabr-volatility 0.03 0.03 1 0.02 0.5 -0.3 0.4 :hagan)))
                      1d-14)))
  (check-that "... and differs from it away from the money for beta < 1"
              (> (abs (- (sabr-volatility 0.015 0.03 5 0.015 0.3 0.2 0.6 :obloj)
                         (sabr-volatility 0.015 0.03 5 0.015 0.3 0.2 0.6 :hagan)))
                 1d-6))
  (let* ((m (surface-market))
         (days 182)
         (expiry (days-out days))
         (tau (market-time m expiry))
         (truth (make-sabr-smile tau (forward m expiry) :alpha 0.9 :beta 0.5 :rho -0.35 :nu 0.6))
         (quotes (quotes-from (lambda (k tau) (declare (ignore tau)) (smile-vol truth k))
                              m `(,days) (strikes-between 70 140 15))))
    (multiple-value-bind (fit rmse) (fit-sabr (quote-set quotes) m :beta 0.5)
      (check-that (format nil "SABR fit round-trips: alpha ~,8F rho ~,8F nu ~,8F (RMSE ~,2E)"
                          (fincl::sabr-alpha fit) (fincl::sabr-rho fit) (fincl::sabr-nu fit) rmse)
                  (and (< (abs (- (fincl::sabr-alpha fit) 0.9d0)) 1d-6)
                       (< (abs (- (fincl::sabr-rho fit) -0.35d0)) 1d-6)
                       (< (abs (- (fincl::sabr-nu fit) 0.6d0)) 1d-6))))))

;;; --------------------------------------------------------------------
;;; Phase 3: local volatility
;;; --------------------------------------------------------------------

(defun local-vol-checks ()
  (format t "~&Dupire local volatility~%")
  (multiple-value-bind (v se)
      (price (make-option (call 100) (expires (days-out 365)))
             (make-engine :mc :process (make-local-vol) :n-paths 200000 :n-steps 50)
             (surface-market :vol 0.2d0 :dividend 0d0))
    (within-se "a flat 20% surface gives Black-Scholes's 10.4506 through Monte Carlo"
               v 10.450583572185565d0 se))
  (let* ((grid (make-grid-vol-surface '(0.25 0.5 1 2) '(-1 -0.5 0 0.5 1)
                                      '((0.3 0.3 0.3 0.3 0.3) (0.25 0.25 0.25 0.25 0.25)
                                        (0.22 0.22 0.22 0.22 0.22) (0.2 0.2 0.2 0.2 0.2))))
         (m (surface-market :vol grid))
         (tau (market-time m (days-out 365))))
    (check-that "on a surface flat in strike, local vol (one substep) gives GBM's paths exactly"
                (equalp (fincl::path-matrix-data
                         (fincl::simulate-paths (make-engine :mc :process (make-local-vol :substeps 1)
                                                             :n-paths 1000 :n-steps 20)
                                                m tau))
                        (fincl::path-matrix-data
                         (fincl::simulate-paths (make-engine :mc :n-paths 1000 :n-steps 20) m tau)))))
  (let* ((ssvi (make-ssvi-surface '(0.25 0.5 1 2) '(0.0121 0.0225 0.04 0.072)
                                  :rho -0.6 :eta 1.2 :gamma 0.4))
         (m (surface-market :vol ssvi))
         (worst 0d0))
    (check-that "MC under SSVI local vol reprices 15 Europeans within 3 SE + 2e-3 (200k paths, 200 steps a year)"
                (iter outer
                      (for days in '(91 182 365))
                      (for engine = (make-engine :mc :process (make-local-vol) :n-paths 200000
                                                     :n-steps (round (* 200 days) 365)))
                      (iter (for k in '(80 90 100 110 120))
                            (for option = (make-option-from-clauses
                                           `((,(if (< k 100) :put :call) ,k) (:expires ,(days-out days)))))
                            (multiple-value-bind (v se) (price option engine m)
                              (let* ((exact (price option :black-scholes m))
                                     (distance (/ (abs (- v exact)) se)))
                                (setf worst (max worst distance))
                                (in outer (always (<= (abs (- v exact)) (+ (* 3 se) 2d-3)))))))))
    (format t "~&  info worst distance ~,2F SE~%" worst))
  (check-error "local vol on a surface with butterfly arbitrage"
               (price (make-option (call 100) (expires (days-out 365)))
                      (make-engine :mc :process (make-local-vol) :n-paths 100)
                      (surface-market :vol (make-svi-surface
                                            `(,(make-svi-slice 1 :a -0.0410 :b 0.1331 :m 0.3586
                                                                 :rho 0.3060 :sigma 0.4153)))))
               butterfly-arbitrage))

;;; --------------------------------------------------------------------
;;; Phase 4: calibration
;;; --------------------------------------------------------------------

(defun calibration-checks ()
  (format t "~&Generic calibration~%")
  (let* ((m (surface-market :vol 0.2d0))
         (truth (make-heston :v0 0.035d0 :theta 0.05d0 :kappa 1.5d0 :xi 0.6d0 :rho -0.6d0))
         (day-list '(30 91 182 365 730))
         (quotes (iter outer (for days in day-list)
                       (iter (for k in '(80 90 100 110 120))
                             (for option = (make-option-from-clauses
                                            `((,(if (< k 100) :put :call) ,k) (:expires ,(days-out days)))))
                             (in outer (collect (make-option-quote
                                                 option :price (price option (make-engine :cos :process truth) m)))))))
         (start (get-internal-real-time)))
    (multiple-value-bind (fit report) (calibrate (make-heston) m (quote-set quotes) :engine :cos)
      (let* ((seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1d0))
             (errors (mapcar (lambda (name) (abs (- (process-parameter fit name) (process-parameter truth name))))
                             '(:v0 :kappa :theta :xi :rho))))
        (check-that (format nil "Heston by COS on 25 quotes: vol RMSE ~,2E, parameters within ~,2E, ~,1F s ~
(~D objective evaluations)"
                            (getf report :rmse) (reduce #'max errors) seconds (getf report :iterations))
                    (and (< (getf report :rmse) 1d-6) (< (reduce #'max errors) 1d-3) (< seconds 30)))))
    ;; FIXED: kappa held at a wrong value; the rest adjusts.
    (multiple-value-bind (fit report) (calibrate (make-heston :kappa 3d0) m (quote-set quotes) :engine :cos
                                                                            :fixed '(:kappa))
      (check-that (format nil "FIXED holds kappa (~,4F) while the rest fit (RMSE ~,2E)"
                          (process-parameter fit :kappa) (getf report :rmse))
                  (= 3d0 (process-parameter fit :kappa))))
    (multiple-value-bind (fit) (calibrate (make-heston) m (quote-set quotes) :engine :cos
                                          :fixed '((:rho -0.6d0) (:xi 0.6d0)))
      (check-that "FIXED with values"
                  (and (= -0.6d0 (process-parameter fit :rho)) (= 0.6d0 (process-parameter fit :xi)))))
    ;; Thread count.
    (flet ((fit-with (workers)
             (let ((lp:*kernel* (lp:make-kernel workers)))
               (unwind-protect (parameter-vector (calibrate (make-heston) m (quote-set quotes) :engine :cos) m)
                 (lp:end-kernel :wait t)))))
      (check-that "identical calibrations with 1 and 8 workers"
                  (equalp (fit-with 1) (fit-with 8)))))
  ;; CEV to its own Monte Carlo prices, with common random numbers.
  (let* ((m (surface-market :vol 0.2d0))
         (engine (make-engine :mc :n-paths 20000 :n-steps 25))
         (truth (make-cev :beta 0.6d0 :sigma 0.8d0))
         (quotes (iter (for k in '(80 90 100 110 120))
                       (for option = (make-option-from-clauses
                                      `((,(if (< k 100) :put :call) ,k) (:expires ,(days-out 365)))))
                       (collect (make-option-quote
                                 option :price (price option (derive-engine engine :process truth) m))))))
    (multiple-value-bind (fit report) (calibrate (make-cev :beta 0.9d0) m (quote-set quotes) :engine engine)
      (check-that (format nil "CEV calibrated to its own Monte Carlo prices: beta ~,4F (RMSE ~,2E)"
                          (process-parameter fit :beta) (getf report :rmse))
                  (< (abs (- (process-parameter fit :beta) 0.6d0)) 1d-2)))))

(defun surface-checks ()
  (grid-surface-checks)
  (smile-fit-checks)
  (local-vol-checks)
  (calibration-checks))

(defun run-surface-tests ()
  "Run only the task 17 checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (surface-checks))
    (format t "~&~[All surface checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
