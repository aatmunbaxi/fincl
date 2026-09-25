;;;; engines/analytic.lisp --- closed forms and approximations
;;;;
;;;; These read the market only through the protocol (FORWARD,
;;;; DISCOUNT-FACTOR, BLACK-VARIANCE at the expiry date), so a flat market and
;;;; a curve market both work. They are specialized on BLACK-SCHOLES-MARKET
;;;; anyway, because the formulas assume lognormal dynamics with
;;;; deterministic rates: a HESTON-MARKET would get NO applicable method,
;;;; which is the correct answer rather than a silently wrong price.

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

(defun black-scholes (phi market strike x)
  "Black-Scholes-Merton price to X, a date or a horizon in market time."
  (black-formula phi (forward market x) strike (discount-factor market x)
                 (black-variance market strike x)))

;;; Specialized on ANALYTIC-ENGINE, so every analytic engine can price a
;;; European even when it exists for something else.
(defmethod price-analytic ((x european-exercise) (p vanilla-payoff)
                           (f path-independent) (e analytic-engine)
                           (m black-scholes-market))
  (values (black-scholes (phi p) m (strike p) (expiry x)) nil))

;;; Barone-Adesi-Whaley quadratic approximation for American vanillas. The
;;; formula is stated in r, q, sigma and T, so those are backed out of the
;;; discount factor, forward and variance in market time.
(defmethod price-analytic ((x american-exercise) (p vanilla-payoff)
                           (f path-independent) (e barone-adesi-whaley-engine)
                           (m black-scholes-market))
  (let* ((phi (phi p)) (s (spot m)) (k (strike p)) (date (expiry x))
         (tau (time-to-expiry x m))
         (df (discount-factor m date))
         (fwd (forward m date))
         (w (black-variance m k date))
         (r (/ (- (log df)) tau))
         (q (- r (/ (log (/ fwd s)) tau)))
         (vol (sqrt (/ w tau)))
         (growth (exp (* (- r q) tau))))
    ;; With no dividend yield, early exercise of a call is never optimal.
    (when (and (plusp phi) (<= q 0d0))
      (return-from price-analytic (values (black-formula phi fwd k df w) nil)))
    (flet ((euro (spot-value)
             ;; European price at a trial spot: same D and w, scaled forward.
             (black-formula phi (* spot-value growth) k df w))
           (d1-at (spot-value)
             (bs-d1 (* spot-value growth) k (sqrt w))))
      (let* ((big-m (/ (* 2d0 r) (* vol vol)))
             (big-n (/ (* 2d0 (- r q)) (* vol vol)))
             (kf (- 1d0 df))
             (qi (* 0.5d0 (+ (- (- big-n 1d0))
                             (* phi (sqrt (+ (expt (- big-n 1d0) 2)
                                             (/ (* 4d0 big-m) kf))))))))
        (flet ((premium-term (sc)
                 (/ (* (- 1d0 (* (exp (- (* q tau))) (norm-cdf (* phi (d1-at sc)))))
                       sc)
                    qi)))
          (let* ((critical
                   ;; Smooth-pasting condition for the critical price S*.
                   (flet ((g (sc)
                            (- (* phi (- sc k)) (euro sc) (* phi (premium-term sc)))))
                     (if (plusp phi)
                         (solve-root #'g k (* 2d0 k) :expand-hi t)
                         (solve-root #'g (* 1d-8 k) k))))
                 (a (* phi (premium-term critical))))
            (values (if (minusp (* phi (- s critical)))
                        (+ (black-formula phi fwd k df w)
                           (* a (expt (/ s critical) qi)))
                        (* phi (- s k)))
                    nil)))))))
