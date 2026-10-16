;;;; levy.lisp --- Levy and jump processes, Heston QE, analytic CEV, and the
;;;; special functions behind them

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Special functions and variates from normals
;;; --------------------------------------------------------------------

(defun levy-math-checks ()
  (format t "~&Special functions: Bessel K and the noncentral chi-square~%")
  (destructuring-bind ((bessel-tag &rest bessel) (chi-tag &rest chi))
      (read-fixture "special-functions.sexp")
    (assert (and (eq bessel-tag :bessel-k) (eq chi-tag :noncentral-chi-square)))
    (check-that (format nil "K_nu(x) matches mpmath to 1e-14 relative (~D values)" (length bessel))
                (every (lambda (row)
                         (destructuring-bind (nu x value) row
                           (< (relative-difference value (fincl::bessel-k nu x)) 1d-14)))
                       bessel))
    ;; Both the CDF and its complement, deep into either tail (to 1e-69).
    (check-that (format nil "noncentral chi-square F and 1 - F match mpmath to 1e-12 relative (~D cases)"
                        (length chi))
                (every (lambda (row)
                         (destructuring-bind (x df lambda p q) row
                           (multiple-value-bind (gp gq) (fincl::noncentral-chi-square-cdf x df lambda)
                             (and (< (relative-difference p gp) 1d-12)
                                  (< (relative-difference q gq) 1d-12)))))
                       chi)))
  (format t "~&Variates from normals: quantiles at Phi(z)~%")
  (let ((zs '(-6d0 -2.5d0 -0.3d0 0d0 0.7d0 3d0 7d0)))
    (check-that "Poisson quantile: the least n with P(N <= n) >= Phi(z)"
                (every (lambda (mean)
                         (every (lambda (z)
                                  (let* ((n (fincl::poisson-from-normal z mean))
                                         (u (norm-cdf z))
                                         (cdf (lambda (k) (if (minusp k)
                                                              0d0
                                                              (nth-value 1 (fincl::regularized-gamma
                                                                            (+ k 1d0) mean))))))
                                    (and (>= (funcall cdf n) (- u 1d-12))
                                         (< (funcall cdf (1- n)) (+ u 1d-12)))))
                                zs))
                       '(0.02d0 0.7d0 4d0 45d0 400d0)))
    (check-that "gamma quantile: P(shape, x) = Phi(z) to 1e-11"
                (every (lambda (shape)
                         (every (lambda (z)
                                  (let ((x (fincl::gamma-from-normal z shape)))
                                    (if (zerop x)
                                        ;; Underflow: the quantile is below 1e-308.
                                        (< (/ (+ (log (norm-cdf z)) (fincl::log-gamma (+ shape 1d0)))
                                              shape)
                                           -700d0)
                                        (multiple-value-bind (p q) (fincl::regularized-gamma shape x)
                                          (if (plusp z)
                                              (< (relative-difference (norm-cdf (- z)) q) 1d-11)
                                              (< (relative-difference (norm-cdf z) p) 1d-11))))))
                                zs))
                       '(0.01d0 0.1d0 1d0 3d0 50d0))))
  ;; The inverse Gaussian sampler's mean and variance: m and m^3 / shape.
  (let* ((n 200000) (mean 0.4d0) (shape 0.9d0) (sum 0d0) (sum2 0d0))
    (dotimes (i n)
      (let ((v (fincl::inverse-gaussian-from-normals
                (norm-quantile (/ (+ i 0.5d0) n))
                (norm-quantile (/ (+ (mod (* i 7919) n) 0.5d0) n))
                mean shape)))
        (incf sum v) (incf sum2 (* v v))))
    (let* ((m (/ sum n)) (var (- (/ sum2 n) (* m m))))
      (check "inverse Gaussian sampler: mean" mean m 2d-3)
      (check "inverse Gaussian sampler: variance m^3/shape" (/ (expt mean 3) shape) var 3d-3))))

;;; --------------------------------------------------------------------
;;; Kou (2002), Theorem 1 and Appendix: the closed-form call
;;; --------------------------------------------------------------------

(defun kou-hh (n x)
  "Hh_-1(x) .. Hh_n(x) at indices 0 .. n + 1: Hh_-1 = e^(-x^2/2), Hh_0 =
sqrt(2 pi) Phi(-x), n Hh_n = Hh_(n-2) - x Hh_(n-1)."
  (let ((h (make-array (+ n 2) :element-type 'double-float)))
    (setf (aref h 0) (exp (* -0.5d0 x x))
          (aref h 1) (* (sqrt (* 2 pi)) (norm-cdf (- x))))
    (iter (for i from 1 to n)
          (setf (aref h (1+ i)) (/ (- (aref h (1- i)) (* x (aref h i))) i)))
    h))

(defun kou-i (n c alpha beta delta)
  "I_n(c; alpha, beta, delta) = integral from c to infinity of e^(alpha x)
Hh_n(beta x - delta) dx (Kou 2002, Proposition B.3)."
  (let* ((h (kou-hh n (- (* beta c) delta)))
         (head (* (- (/ (exp (* alpha c)) alpha))
                  (iter (for i from 0 to n) (sum (* (expt (/ beta alpha) (- n i)) (aref h (1+ i)))))))
         (factor (* (expt (/ beta alpha) (1+ n)) (/ (sqrt (* 2 pi)) beta)
                    (exp (+ (/ (* alpha delta) beta) (/ (* alpha alpha) (* 2 beta beta)))))))
    (if (plusp beta)
        (+ head (* factor (norm-cdf (+ (- (* beta c)) delta (/ alpha beta)))))
        (- head (* factor (norm-cdf (- (* beta c) delta (/ alpha beta))))))))

(defun kou-pq (n k up e1 e2 &key down-side)
  "P_(n,k) (or Q_(n,k) with DOWN-SIDE): the probability that n jumps net to k
up (down) exponentials (Kou 2002, Proposition B.1)."
  (let ((down (- 1 up)))
    (flet ((binom (n k) (if (or (< k 0) (> k n)) 0 (a:binomial-coefficient n k))))
      (cond ((= k n) (if down-side (expt down n) (expt up n)))
            (down-side
             (iter (for i from k to (1- n))
                   (sum (* (binom (- n k 1) (- i k)) (binom n i)
                           (expt (/ e1 (+ e1 e2)) (- n i)) (expt (/ e2 (+ e1 e2)) (- i k))
                           (expt up (- n i)) (expt down i)))))
            (t
             (iter (for i from k to (1- n))
                   (sum (* (binom (- n k 1) (- i k)) (binom n i)
                           (expt (/ e1 (+ e1 e2)) (- i k)) (expt (/ e2 (+ e1 e2)) (- n i))
                           (expt up i) (expt down (- n i))))))))))

(defun kou-upsilon (mu sigma lam up e1 e2 a horizon &key (n-max 40))
  "P(Z(T) >= a) for Z(t) = mu t + sigma W(t) + the double-exponential jumps
(Kou 2002, Theorem B.1)."
  (let* ((st (* sigma (sqrt horizon)))
         (c (- a (* mu horizon)))
         (weight (lambda (n) (exp (- (* n (log (* lam horizon))) (* lam horizon)
                                     (fincl::log-gamma (+ n 1d0))))))
         (norm (* sigma (sqrt (* 2 pi horizon)))))
    (+ (* (/ (exp (* 0.5d0 (expt (* sigma e1) 2) horizon)) norm)
          (iter (for n from 1 to n-max)
                (sum (* (funcall weight n)
                        (iter (for k from 1 to n)
                              (sum (* (kou-pq n k up e1 e2) (expt (* st e1) k)
                                      (kou-i (1- k) c (- e1) (/ -1d0 st) (- (* st e1))))))))))
       (* (/ (exp (* 0.5d0 (expt (* sigma e2) 2) horizon)) norm)
          (iter (for n from 1 to n-max)
                (sum (* (funcall weight n)
                        (iter (for k from 1 to n)
                              (sum (* (kou-pq n k up e1 e2 :down-side t) (expt (* st e2) k)
                                      (kou-i (1- k) c e2 (/ 1d0 st) (- (* st e2))))))))))
       (* (exp (- (* lam horizon))) (norm-cdf (- (/ c st)))))))

(defun kou-call (spot strike rate sigma lam up e1 e2 horizon)
  "Kou's (2002) European call, Theorem 2."
  (let* ((zeta (+ (/ (* up e1) (- e1 1)) (/ (* (- 1 up) e2) (+ e2 1)) -1))
         (a (log (/ strike spot))))
    (- (* spot (kou-upsilon (+ rate (* 0.5d0 sigma sigma) (- (* lam zeta))) sigma
                            (* lam (+ zeta 1)) (* (/ up (+ 1 zeta)) (/ e1 (- e1 1)))
                            (- e1 1) (+ e2 1) a horizon))
       (* strike (exp (- (* rate horizon)))
          (kou-upsilon (- rate (* 0.5d0 sigma sigma) (* lam zeta)) sigma lam up e1 e2 a horizon)))))

;;; --------------------------------------------------------------------
;;; Phase 1: exponential Levy, Merton, Kou
;;; --------------------------------------------------------------------

(defun jump-diffusion-checks ()
  (format t "~&Jump diffusions: Merton and Kou~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (expiry #D"2027-09-24")
         (merton (make-instance 'merton-jump :intensity 1 :jump-mean -0.1d0 :jump-vol 0.15d0))
         (cos (make-instance 'cos-engine :process merton :n-terms 512))
         (series (make-instance 'merton-series-engine :process merton)))
    (check-that "Merton COS matches the Poisson series to 1e-10 (calls and puts, K = 70, 100, 130)"
                (iter (for kind in '(:call :put))
                      (always (iter (for k in '(70 100 130))
                                    (for o = (make-vanilla-option kind k expiry))
                                    (always (< (abs (- (price o cos m) (price o series m))) 1d-10))))))
    ;; Kou's own example parameters: sigma = 0.16, lambda = 1, p = 0.4,
    ;; eta1 = 10, eta2 = 5, r = 5%, T = 0.5 (182 days here).
    (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.16d0))
           (expiry (date+ *today* 182))
           (tau (market-time m expiry))
           (kou (make-instance 'kou-jump :intensity 1 :up-probability 0.4d0 :eta1 10 :eta2 5
                                         :vol 0.16d0))
           (cos (make-instance 'cos-engine :process kou :n-terms 512)))
      (check-that "Kou COS matches Kou's (2002) closed form to 1e-8 (K = 90, 98, 100, 105, 120)"
                  (iter (for k in '(90 98 100 105 120))
                        (always (< (abs (- (price (make-vanilla-option :call k expiry) cos m)
                                           (kou-call 100d0 (float k 1d0) 0.05d0 0.16d0 1d0 0.4d0
                                                     10d0 5d0 tau)))
                                   1d-8))))
      (let ((o (make-vanilla-option :put 95 expiry)))
        (multiple-value-bind (v se) (price o (mc :process kou :n-paths 200000) m)
          (check-that (format nil "Kou MC terminal within 3 SE of COS (~,5F vs ~,5F, SE ~,5F)"
                              v (price o cos m) se)
                      (< (abs (- v (price o cos m))) (* 3 se))))))
    (let ((o (make-vanilla-option :put 100 expiry)))
      (multiple-value-bind (v se) (price o (mc :process merton :n-paths 200000) m)
        (check-that (format nil "Merton MC terminal within 3 SE of COS (~,5F vs ~,5F, SE ~,5F)"
                            v (price o cos m) se)
                    (< (abs (- v (price o cos m))) (* 3 se))))
      ;; An American put forces stepping, through the jump stepper.
      (let ((a (make-vanilla-option :put 100 expiry :exercise :american)))
        (multiple-value-bind (v se) (price a (mc :process merton :n-paths 50000 :n-steps 200) m)
          (let ((c (price a cos m)))
            (format t "~&  info Merton American put: COS ~,5F, LSM (200 steps) ~,5F +- ~,5F~%"
                    c v se)
            (check-that "Merton COS American within 3 SE + 0.01 of LSM"
                        (< (abs (- v c)) (+ (* 3 se) 0.01d0)))))))
    ;; lambda = 0 is GBM through every engine.
    (let ((call (make-vanilla-option :call 100 expiry)))
      (dolist (process (list (make-instance 'merton-jump :intensity 0)
                             (make-instance 'kou-jump :intensity 0)))
        (let ((name (string-downcase (class-name (class-of process)))))
          (check (format nil "~A lambda=0, COS" name) 10.4506
                 (price call (make-instance 'cos-engine :process process) m) 1d-4)
          (multiple-value-bind (v se) (price call (mc :process process :n-paths 200000) m)
            (check (format nil "~A lambda=0, MC (SE ~,4F)" name se) 10.4506 v (* 3 se)))))
      (check "merton-series lambda=0" 10.4506
             (price call (make-instance 'merton-series-engine
                                        :process (make-instance 'merton-jump :intensity 0))
                    m)
             1d-4))))

;;; --------------------------------------------------------------------
;;; Phase 2: VG, NIG, CGMY
;;; --------------------------------------------------------------------

(defun pure-jump-checks ()
  (format t "~&Pure-jump Levy processes: VG, NIG, CGMY~%")
  ;; Fang and Oosterlee (2008), "A novel pricing method for European options
  ;; based on Fourier-cosine series expansions", SIAM J. Sci. Comput. 31.
  ;; Table 7 (VG, K = 90; T = 0.1 and 1), Tables 8 and 9 (CGMY, Y = 0.5 and
  ;; 1.5), all with S = 100, r = 0.1, q = 0. T = 0.1 is 36 days under
  ;; ACT/360.
  (let ((year (make-bs-market :valuation-date *today* :spot 100 :rate 0.1d0 :vol 0.2d0))
        (act360 (make-bs-market :valuation-date *today* :spot 100 :rate 0.1d0 :vol 0.2d0
                                :day-counter +actual-360+))
        (vg (make-instance 'variance-gamma :sigma 0.12d0 :theta -0.14d0 :nu 0.2d0))
        (engine (lambda (p) (make-instance 'cos-engine :process p :n-terms 16384 :truncation 14))))
    (check "VG T=1 call K=90 (FO 2008 Table 7: 19.099354724)" 19.099354724d0
           (price (make-vanilla-option :call 90 #D"2027-09-24") (funcall engine vg) year) 1d-8)
    (check "VG T=0.1 call K=90 (FO 2008 Table 7: 10.993703187)" 10.993703187d0
           (price (make-vanilla-option :call 90 (date+ *today* 36)) (funcall engine vg) act360) 1d-8)
    (check "CGMY Y=0.5 call (FO 2008 Table 8: 19.812948843)" 19.812948843d0
           (price (make-vanilla-option :call 100 #D"2027-09-24")
                  (funcall engine (make-instance 'cgmy :c 1 :g 5 :m 5 :y 0.5d0)) year)
           1d-8)
    (check "CGMY Y=1.5 call (FO 2008 Table 9: 49.790905469)" 49.790905469d0
           (price (make-vanilla-option :call 100 #D"2027-09-24")
                  (funcall engine (make-instance 'cgmy :c 1 :g 5 :m 5 :y 1.5d0)) year)
           1d-8))
  ;; CGMY's Y = 0 and Y = 1 limits are continuous.
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
        (o (make-vanilla-option :call 100 #D"2027-09-24")))
    (flet ((px (y) (price o (make-instance 'cos-engine :process (make-instance 'cgmy :y y)) m)))
      (check "CGMY Y=0 limit is continuous" (px 1d-7) (px 0d0) 1d-5)
      (check "CGMY Y=1 limit is continuous" (px (+ 1d0 1d-7)) (px 1d0) 1d-5)))
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (o (make-vanilla-option :put 95 #D"2027-09-24")))
    (dolist (p (list (make-instance 'variance-gamma :sigma 0.2d0 :theta -0.14d0 :nu 0.2d0)
                     (make-instance 'nig)))
      (let ((name (string-downcase (class-name (class-of p))))
            (c (price o (make-instance 'cos-engine :process p :n-terms 1024) m)))
        (multiple-value-bind (v se) (price o (mc :process p :n-paths 200000) m)
          (check-that (format nil "~A MC terminal within 3 SE of COS (~,5F vs ~,5F, SE ~,5F)"
                              name v c se)
                      (< (abs (- v c)) (* 3 se))))
        (multiple-value-bind (v se) (price o (mc :process p :n-paths 50000 :n-steps 10) m)
          (check-that (format nil "~A MC stepped within 3 SE of COS (~,5F, SE ~,5F)" name v se)
                      (< (abs (- v c)) (* 3 se))))))
    (check "VG nu -> 0 with theta = 0 is GBM" 10.4506
           (price (make-vanilla-option :call 100 #D"2027-09-24")
                  (make-instance 'cos-engine :process (make-instance 'variance-gamma :nu 1d-8
                                                                                     :theta 0d0))
                  m)
           1d-4))
  ;; NIG density (K_1) against the characteristic function: total mass,
  ;; mean, and the variance.
  (let ((p (make-instance 'nig)))
    (multiple-value-bind (k1 k2) (levy-cumulants p)
      (flet ((moment (f) (fincl::integrate-gauss-legendre
                          (lambda (x) (* (funcall f x) (fincl::nig-density p x 1d0)))
                          -4d0 4d0 :panels 400)))
        (check "NIG density integrates to 1" 1d0 (moment (constantly 1d0)) 1d-10)
        (check "NIG density mean = delta beta / gamma" k1 (moment #'identity) 1d-10)
        (check "NIG density variance = delta alpha^2 / gamma^3" k2
               (- (moment (lambda (x) (* x x))) (* k1 k1)) 1d-10))))
  (format t "~&Levy processes: errors~%")
  (check-error "VG without an exponential moment"
               (make-instance 'variance-gamma :sigma 1d0 :nu 2d0 :theta 0.5d0) no-exponential-moment)
  (check-error "NIG with |beta + 1| >= alpha"
               (make-instance 'nig :alpha 2d0 :beta 1.5d0) no-exponential-moment)
  (check-error "CGMY by Monte Carlo is unsupported"
               (price (make-vanilla-option :call 100 #D"2027-09-24")
                      (mc :process (make-instance 'cgmy))
                      (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
               unsupported-combination))

;;; --------------------------------------------------------------------
;;; Phase 3: Heston QE and Bates
;;; --------------------------------------------------------------------

(defun heston-qe-checks ()
  (format t "~&Heston QE and Bates~%")
  ;; Andersen (2008), case I: kappa = 0.5, theta = 0.04, xi = 1, rho = -0.9,
  ;; v0 = 0.04, T = 10, r = 0; Feller is badly violated. 8 steps a year.
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0 :vol 0.2d0))
         (o (make-vanilla-option :call 100 (date+ *today* 3650)))
         (args '(:v0 0.04d0 :kappa 0.5d0 :theta 0.04d0 :xi 1d0 :rho -0.9d0))
         (reference (price o (make-instance 'cos-engine :process (apply #'make-instance 'heston args)
                                                        :n-terms 4096 :truncation 16)
                           m)))
    (dolist (scheme '(:quadratic-exponential :full-truncation))
      (multiple-value-bind (v se)
          (price o (mc :n-paths 500000 :n-steps 80
                       :process (apply #'make-instance 'heston :scheme scheme args))
                 m)
        (format t "~&  info Andersen case I, ~(~A~): ~,5F vs COS ~,5F, bias ~,2F SE~%"
                scheme v reference (/ (- v reference) se))
        (if (eq scheme :quadratic-exponential)
            (check-that "QE bias within 3 SE (Andersen case I, 8 steps/year)"
                        (< (abs (- v reference)) (* 3 se)))
            (check-that "full truncation bias beyond 3 SE (Andersen case I, 8 steps/year)"
                        (> (abs (- v reference)) (* 3 se)))))))
  ;; Fang and Oosterlee (2008) Table 5: Heston, T = 10, S = K = 100, r = 0.
  (check "Heston COS T=10 (FO 2008 Table 5: 22.318945791)" 22.318945791d0
         (price (make-vanilla-option :call 100 (date+ *today* 3650))
                (make-instance 'cos-engine
                               :process (make-instance 'heston :kappa 1.5768d0 :xi 0.5751d0
                                                               :theta 0.0398d0 :v0 0.0175d0
                                                               :rho -0.5711d0)
                               :n-terms 4096 :truncation 16)
                (make-bs-market :valuation-date *today* :spot 100 :rate 0 :vol 0.2d0))
         1d-8)
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (o (make-vanilla-option :put 95 #D"2027-09-24")))
    (flet ((cos-price (p) (price o (make-instance 'cos-engine :process p :n-terms 1024) m)))
      (check "Bates lambda=0 is Heston" (cos-price (make-instance 'heston :v0 0.04d0 :theta 0.05d0
                                                                          :xi 0.5d0))
             (cos-price (make-instance 'bates :intensity 0 :v0 0.04d0 :theta 0.05d0 :xi 0.5d0))
             1d-12)
      (check "Bates xi -> 0 (1e-8) with v0 = theta is Merton"
             (cos-price (make-instance 'merton-jump :intensity 1 :vol 0.2d0))
             (cos-price (make-instance 'bates :intensity 1 :v0 0.04d0 :theta 0.04d0 :xi 1d-8))
             1d-8)
      (let ((bates (make-instance 'bates :intensity 1 :v0 0.04d0 :theta 0.05d0 :xi 0.5d0)))
        (multiple-value-bind (v se) (price o (mc :process bates :n-paths 200000 :n-steps 50) m)
          (check-that (format nil "Bates MC within 3 SE of COS (~,5F vs ~,5F, SE ~,5F)"
                              v (cos-price bates) se)
                      (< (abs (- v (cos-price bates))) (* 3 se))))))))

;;; --------------------------------------------------------------------
;;; Phase 4: CEV
;;; --------------------------------------------------------------------

(defun cev-analytic-checks ()
  (format t "~&CEV: Schroder's formula and the absorbing scheme~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0
                            :dividend 0.02d0))
         (expiry #D"2027-09-24")
         (bs (make-instance 'black-scholes-engine)))
    (flet ((cev (beta sigma) (make-instance 'cev-engine :process (make-instance 'cev :beta beta
                                                                                     :sigma sigma))))
      (check-that "beta=1 is Black to 1e-10 (calls and puts, K = 80, 105, 130)"
                  (iter (for kind in '(:call :put))
                        (always (iter (for k in '(80 105 130))
                                      (for o = (make-vanilla-option kind k expiry))
                                      (always (< (abs (- (price o (cev 1d0 0.2d0) m) (price o bs m)))
                                                 1d-10))))))
      ;; Close to 1 the price tends to Black's (to first order in 1 - beta).
      (check "beta=0.99999 is within 1e-5 of Black"
             (price (make-vanilla-option :call 105 expiry) bs m)
             (price (make-vanilla-option :call 105 expiry) (cev 0.99999d0 (* 0.2d0 (expt 100 1d-5))) m)
             1d-5)
      ;; beta = 0: Brownian motion absorbed at 0 on the clock of CEV-CLOCK,
      ;; whose call is a difference of Bachelier calls (reflection principle).
      (let* ((sigma 20d0)
             (growth (forward-factor m expiry))
             (df (discount-factor m expiry))
             (w (* sigma sigma (fincl::cev-clock 0d0 m 1d0))))
        (check-that "beta=0 matches the reflection principle to 1e-10 (K = 60, 100, 140)"
                    (iter (for k in '(60 100 140))
                          (for kk = (/ k growth))
                          (always (< (abs (- (price (make-vanilla-option :call k expiry) (cev 0d0 sigma) m)
                                             (* df growth (- (bachelier-formula 1d0 100d0 kk 1d0 w)
                                                             (bachelier-formula 1d0 -100d0 kk 1d0 w)))))
                                     1d-10)))))
      (check-error "beta > 1 is outside the formula"
                   (price (make-vanilla-option :call 100 expiry) (cev 1.5d0 0.2d0) m)
                   model-domain-error)))
  ;; Larguinho, Dias and Braumann (2013), "On the computation of option
  ;; prices and Greeks under the CEV model", Quantitative Finance 13, Table
  ;; 5: S = 100, sigma(S0) = 0.25, r = 10%, q = 0, tau = 0.5, deltas to 4
  ;; decimals. Their beta is twice ours (dS = ... + delta S^(beta/2) dW).
  (let ((today *today*))
    (flet ((delta (beta k kind)
             (flet ((px (s)
                      (price (make-vanilla-option kind k (date+ today 180))
                             (make-instance 'cev-engine
                                            :process (make-instance 'cev :beta beta
                                                                         :sigma (* 0.25d0 (expt 100d0 (- 1 beta)))))
                             (make-bs-market :valuation-date today :spot s :rate 0.1d0 :vol 0.2d0
                                             :day-counter +actual-360+))))
               (/ (- (px 100.01d0) (px 99.99d0)) 0.02d0))))
      (check-that "CEV deltas match Larguinho et al. (2013) Table 5 to 4 decimals"
                  (iter (for (beta k call put) in '((0.5d0 95 0.7293d0 -0.2707d0)
                                                    (0.5d0 100 0.6282d0 -0.3718d0)
                                                    (0.5d0 105 0.5202d0 -0.4798d0)
                                                    (0d0 95 0.7118d0 -0.2882d0)
                                                    (0d0 100 0.6113d0 -0.3887d0)
                                                    (0d0 105 0.5028d0 -0.4972d0)))
                        (always (and (< (abs (- (delta beta k :call) call)) 5d-5)
                                     (< (abs (- (delta beta k :put) put)) 5d-5)))))))
  ;; Monte Carlo schemes on a deep out-of-the-money put at beta = 0.5, with
  ;; a local vol of 60% at the spot, so absorption matters.
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (o (make-vanilla-option :put 25 #D"2027-09-24"))
         (analytic (price o (make-instance 'cev-engine
                                           :process (make-instance 'cev :beta 0.5d0 :sigma 6d0))
                          m)))
    (flet ((run (scheme steps)
             (multiple-value-bind (v se)
                 (price o (mc :n-paths 200000 :n-steps steps
                              :process (make-instance 'cev :beta 0.5d0 :sigma 6d0 :scheme scheme))
                        m)
               (format t "~&  info CEV put K=25, ~(~A~), ~D steps: ~,5F vs ~,5F, ~,2F SE~%"
                       scheme steps v analytic (/ (- v analytic) se))
               (/ (- v analytic) se))))
      (check-that "absorbing Euler within 3 SE of the formula at 400 steps/year"
                  (< (abs (run :absorbing-euler 400)) 3))
      (check-that "log-Euler is biased beyond 3 SE at 12 steps/year"
                  (> (abs (run :log-euler 12)) 3)))))

(defun levy-checks ()
  (levy-math-checks)
  (jump-diffusion-checks)
  (pure-jump-checks)
  (heston-qe-checks)
  (cev-analytic-checks))

(defun run-levy-tests ()
  "Run only the checks of this file. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (levy-checks))
    (format t "~&~[All Levy checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
