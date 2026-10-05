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
                           (loop repeat 60
                                 while (plusp (* (g lo) g-k))
                                 do (setf lo (* 0.5d0 lo)))
                           (solve-brent #'g lo k)))))
                 (a (* phi (premium-term critical))))
            (values (if (minusp (* phi (- s critical)))
                        (+ (black-formula phi fwd k df w)
                           (* a (expt (/ s critical) qi)))
                        (* phi (- s k)))
                    nil)))))))
