;;;; engines/analytic.lisp --- closed forms and approximations
;;;;
;;;; These read the market only through the protocol (FORWARD,
;;;; DISCOUNT-FACTOR, BLACK-VARIANCE at the expiry date), so they price on any
;;;; market with deterministic rates: a flat market, curves, a vol surface.
;;;; The engine class fixes the model (Black-Scholes); the market only
;;;; supplies data. Black's formula in F, D and w is exact for every such
;;;; market.

(in-package #:fincl)

(defun bs-d1 (f k std-dev)
  (+ (/ (log (/ f k)) std-dev) (* 0.5d0 std-dev)))

(defun black-formula (phi forward strike discount variance)
  "Black price of a call (PHI = 1) or put (PHI = -1) from the forward, the
discount factor and the total variance to expiry. No year fraction appears.

  (black-formula 1d0 105.127d0 100d0 0.951229d0 0.04d0) => 10.4506..."
  (let* ((std-dev (sqrt variance))
         (d1 (bs-d1 forward strike std-dev))
         (d2 (- d1 std-dev)))
    (* phi discount (- (* forward (norm-cdf (* phi d1)))
                       (* strike (norm-cdf (* phi d2)))))))

(defun bachelier-formula (phi forward strike discount normal-variance)
  "Bachelier (normal) price of a call (PHI = 1) or put (PHI = -1) from the
forward, the discount factor and the total normal variance sigma_N^2 T. The
forward and strike may be negative. Evaluated as intrinsic value plus
|F - K| (-Phi~(-|F - K| / sqrt(w))) (NORMALISED-BACHELIER), so it keeps full
relative precision far out of the money.

  (bachelier-formula 1d0 0.01d0 0.012d0 0.95d0 (* 0.008d0 0.008d0)) => 0.002277..."
  (* (float discount 1d0)
     (normalised-bachelier (float phi 1d0) (float (- forward strike) 1d0)
                           (sqrt (float normal-variance 1d0)))))

(defun black-scholes (phi market strike x)
  "Black-Scholes-Merton price to X, a date or a horizon in market time."
  (black-formula phi (forward market x) strike (discount-factor market x)
                 (black-variance market strike x)))

;;; Specialized on ANALYTIC-ENGINE, so every analytic engine can price a
;;; European even when it exists for something else.
(defmethod price-analytic ((x european-exercise) (p vanilla-payoff)
                           (f path-independent) (e analytic-engine)
                           (m market))
  (values (black-scholes (phi p) m (strike p) (expiry x)) nil))

;;; Barone-Adesi-Whaley quadratic approximation for American vanillas. The
;;; formula is stated in constant r, q, sigma and T, so flat equivalents are
;;; backed out of the discount factor, forward and variance to expiry. Under
;;; term structure that is BAW's own approximation on top of the quadratic
;;; one: the early-exercise boundary sees only the averages to expiry.
(defmethod price-analytic ((x american-exercise) (p vanilla-payoff)
                           (f path-independent) (e barone-adesi-whaley-engine)
                           (m market))
  (let* ((phi (phi p)) (s (spot m)) (k (strike p)) (date (expiry x))
         (tau (time-to-expiry x m))
         (df (discount-factor m date))
         (fwd (forward m date))
         (w (black-variance m k date))
         (r (/ (- (log df)) tau))
         (q (- r (/ (log (/ fwd s)) tau)))
         (vol (sqrt (/ w tau)))
         (growth (exp (* (- r q) tau))))
    ;; Early exercise is never optimal for a put when r <= 0 and q >= r, nor
    ;; (by put-call symmetry) for a call when q <= 0 and r >= q: the American
    ;; is the European. For a put with q < r < 0, or a call with r < q < 0,
    ;; the exercise region is bounded on both sides (Battauz, De Donno and
    ;; Sbuelz 2015), which BAW's single critical price cannot represent. Q is
    ;; backed out of the forward, so a zero yield comes out as +-1e-18:
    ;; NEGATIVE means below -1e-12.
    (flet ((negative (x) (< x -1d-12)))
      (when (if (plusp phi) (and (<= q 0d0) (>= r q)) (and (<= r 0d0) (>= q r)))
        (return-from price-analytic (values (black-formula phi fwd k df w) nil)))
      (when (if (plusp phi) (and (negative q) (< r q)) (and (negative r) (< q r)))
        (error 'model-domain-error
               :engine e
               :reason (format nil "with r = ~,4F and q = ~,4F the American ~(~A~) has two ~
                                    exercise boundaries; use a binomial or COS engine."
                               r q (kind p)))))
    (flet ((euro (spot-value)
             ;; European price at a trial spot: same D and w, scaled forward.
             (black-formula phi (* spot-value growth) k df w))
           (d1-at (spot-value)
             (bs-d1 (* spot-value growth) k (sqrt w))))
      (let* ((vol2 (* vol vol))
             (big-n (/ (* 2d0 (- r q)) vol2))
             ;; M / K, with M = 2r / sigma^2 and K = 1 - e^(-rT), written so
             ;; that it tends to 2 / (sigma^2 T) as r -> 0 instead of 0/0.
             (m/k (if (zerop r)
                      (/ 2d0 (* vol2 tau))
                      (/ (* 2d0 r) (* vol2 (- (expm1 (- (* r tau))))))))
             (qi (* 0.5d0 (+ (- 1d0 big-n)
                             (* phi (sqrt (+ (expt (- big-n 1d0) 2) (* 4d0 m/k)))))))
             ;; The perpetual option's exponent and exercise boundary, which
             ;; bound the finite-maturity critical price away from the strike:
             ;; above it for a call, below it for a put.
             (perpetual-discriminant (+ (expt (- big-n 1d0) 2) (/ (* 8d0 r) vol2)))
             (q-perpetual (and (plusp perpetual-discriminant)
                               (* 0.5d0 (+ (- 1d0 big-n)
                                           (* phi (sqrt perpetual-discriminant))))))
             (s-perpetual (and q-perpetual
                               (if (plusp phi) (> q-perpetual 1d0) (minusp q-perpetual))
                               (/ (* k q-perpetual) (- q-perpetual 1d0)))))
        (flet ((premium-term (sc)
                 (/ (* (- 1d0 (* (exp (- (* q tau))) (norm-cdf (* phi (d1-at sc)))))
                       sc)
                    qi)))
          (let* ((critical
                   ;; Smooth-pasting condition for the critical price S*.
                   (flet ((g (sc)
                            (- (* phi (- sc k)) (euro sc) (* phi (premium-term sc)))))
                     (if (plusp phi)
                         (solve-brent #'g k (or s-perpetual (* 2d0 k)) :expand-hi t)
                         ;; BAW's own S* can fall below the perpetual boundary
                         ;; at extreme vols, so halve the lower end until g
                         ;; changes sign.
                         (let ((lo (or s-perpetual (* 1d-8 k))) (g-k (g k)))
                           (iter (repeat 60)
                                 (while (plusp (* (g lo) g-k)))
                                 (setf lo (* 0.5d0 lo)))
                           (solve-brent #'g lo k)))))
                 (a (* phi (premium-term critical))))
            (values (if (minusp (* phi (- s critical)))
                        (+ (black-formula phi fwd k df w)
                           (* a (expt (/ s critical) qi)))
                        (* phi (- s k)))
                    nil)))))))

;;; --------------------------------------------------------------------
;;; Merton's jump-diffusion series
;;; --------------------------------------------------------------------

(defmethod initialize-instance :after ((e merton-series-engine) &key)
  (check-input (slot-value e 'process) merton-jump "a MERTON-JUMP process"))

(defun black-or-intrinsic (phi forward strike discount variance)
  "BLACK-FORMULA, or the discounted intrinsic value when VARIANCE is 0."
  (if (plusp variance)
      (black-formula phi forward strike discount variance)
      (* discount (max 0d0 (* phi (- forward strike))))))

;;; Given n jumps by expiry, ln S(T) is normal with variance w + n sigma_J^2
;;; and conditional forward F e^(-lambda k T) (1 + k)^n, k = E[e^Y] - 1: the
;;; price is the Poisson(lambda T) mixture of those Black prices. Terms are
;;; summed from n = 0 until past the mode the weight falls below 1e-16.
(defmethod price-analytic ((x european-exercise) (p vanilla-payoff)
                           (f path-independent) (e merton-series-engine)
                           (m market))
  (let* ((date (expiry x))
         (tau (time-to-expiry x m))
         (process (-> e process (resolve-process m tau)))
         (lambda-t (* (intensity process) tau))
         (s2 (expt (jump-vol process) 2))
         (k (- (exp (+ (jump-mean process) (* 0.5d0 s2))) 1d0))
         (w (float (diffusion-variance process m 0d0 tau) 1d0))
         (fwd (* (forward m date) (exp (- (* lambda-t k)))))
         (df (discount-factor m date))
         (phi (phi p))
         (strike (strike p)))
    (values (iter (for n from 0)
                  (for weight = (exp (- (* n (log (max lambda-t least-positive-double-float)))
                                        lambda-t (log-gamma (+ n 1d0)))))
                  (sum (if (zerop lambda-t)
                           (if (zerop n) (black-or-intrinsic phi fwd strike df w) 0d0)
                           (* weight (black-or-intrinsic phi (* fwd (expt (+ 1d0 k) n)) strike df
                                                         (+ w (* n s2))))))
                  (until (or (zerop lambda-t) (and (> n lambda-t) (< weight 1d-16)))))
            nil)))

;;; --------------------------------------------------------------------
;;; CEV: Schroder's noncentral chi-square formula
;;; --------------------------------------------------------------------

(defmethod initialize-instance :after ((e cev-engine) &key)
  (check-input (slot-value e 'process) cev "a CEV process"))

(defun cev-clock (beta market horizon)
  "integral over [0, HORIZON] of G(t)^(2 (beta - 1)), G the forward factor:
the time on which Y = S/G, a driftless CEV with dY = sigma G^(beta-1) Y^beta
dW, runs as one with dY = sigma Y^beta dW. HORIZON for beta = 1, and
(1 - e^(-2 mu (1 - beta) T)) / (2 mu (1 - beta)) for flat growth mu."
  (if (= beta 1d0)
      horizon
      (integrate-gauss-legendre
       (lambda (s) (expt (forward-factor market s) (* 2d0 (- beta 1d0))))
       0d0 horizon :panels 32)))

(defconstant +cev-max-noncentrality+ 1d12
  "Above this, the noncentral chi-square series takes seconds (its cost grows
like the square root of the noncentrality); it is reached only for beta
within about 1e-6 of 1, where the price is Black's to about 1e-7.")

(defun cev-driftless-call (y0 strike sigma beta clock &optional engine)
  "E[(Y - STRIKE)^+] for dY = SIGMA Y^BETA dW, BETA < 1, absorbed at 0, after
CLOCK units of time (Cox 1975; Schroder 1989): with k = 1 / (2 sigma^2
(1 - beta)^2 clock), x = k y0^(2 (1 - beta)), y = k K^(2 (1 - beta)) and Q,
F the complementary and plain noncentral chi-square distributions,

  y0 Q(2y; 2 + 1/(1 - beta), 2x) - K F(2x; 1/(1 - beta), 2y)."
  (let* ((e (* 2d0 (- 1d0 beta)))
         (k (/ 1d0 (* 2d0 sigma sigma (expt (- 1d0 beta) 2) clock)))
         (x (* k (expt y0 e)))
         (y (* k (expt strike e)))
         (df (/ 1d0 (- 1d0 beta))))
    (when (> (* 2d0 (max x y)) +cev-max-noncentrality+)
      (error 'model-domain-error
             :engine engine
             :reason (format nil "beta = ~,8F is so close to 1 that the noncentral ~
chi-square series needs a noncentrality of ~,1E; price with beta = 1 (Black) or ~
by Monte Carlo." beta (* 2d0 (max x y)))))
    (- (* y0 (nth-value 1 (noncentral-chi-square-cdf (* 2d0 y) (+ 2d0 df) (* 2d0 x))))
       (* strike (nth-value 0 (noncentral-chi-square-cdf (* 2d0 x) df (* 2d0 y)))))))

;;; S = G Y with G the forward factor, so C = D G E[(Y - K/G)^+] on the clock
;;; CEV-CLOCK: rates and dividends enter through the market's forward and
;;; discount curves, as in Black's formula. Puts follow by parity, which holds
;;; because Y is a true martingale for beta <= 1.
(defmethod price-analytic ((x european-exercise) (p vanilla-payoff)
                           (f path-independent) (e cev-engine)
                           (m market))
  (let* ((date (expiry x))
         (tau (time-to-expiry x m))
         (process (-> e process (resolve-process m tau)))
         (beta (beta process))
         (sigma (sigma process))
         (growth (forward-factor m date))
         (df (discount-factor m date))
         (s0 (float (spot m) 1d0))
         (strike (strike p))
         (phi (phi p)))
    (when (> beta 1d0)
      (error 'model-domain-error
             :engine e
             :reason (format nil "beta = ~,4F > 1 makes S a strict local martingale, where ~
put-call parity fails; the formula here covers beta <= 1." beta)))
    (values
     (if (= beta 1d0)
         (black-formula phi (* s0 growth) strike df (* sigma sigma tau))
         (let ((call (* df growth (cev-driftless-call s0 (/ strike growth) sigma beta
                                                      (cev-clock beta m tau) e))))
           (if (plusp phi)
               call
               (- call (* df (- (* s0 growth) strike))))))
     nil)))

;;; --------------------------------------------------------------------
;;; Automatic selection
;;; --------------------------------------------------------------------

(defmethod engine-candidates append ((designator (eql :black-scholes)) (x european-exercise) (p vanilla-payoff)
                                     (f path-independent) (process gbm))
  ;; A GBM with its own vol goes to engines that use it.
  (unless (explicit-parameters process) '((10 :black-scholes))))

(defmethod engine-candidates append ((designator (eql :baw)) (x american-exercise) (p vanilla-payoff)
                                     (f path-independent) (process gbm))
  (unless (explicit-parameters process) '((30 :baw))))

(defmethod engine-candidates append ((designator (eql :merton-series)) (x european-exercise) (p vanilla-payoff)
                                     (f path-independent) (process merton-jump))
  '((10 :merton-series)))

(defmethod engine-candidates append ((designator (eql :cev)) (x european-exercise) (p vanilla-payoff)
                                     (f path-independent) (process cev))
  '((10 :cev)))
