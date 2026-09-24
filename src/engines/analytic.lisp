;;;; engines/analytic.lisp --- closed forms and approximations
;;;;
;;;; These read the market only through the protocol (DISCOUNT-FACTOR,
;;;; FORWARD-FACTOR, BLACK-VOL), so a flat market and a curve market both
;;;; work. They are specialized on BLACK-SCHOLES-MARKET anyway, because the
;;;; formulas assume lognormal dynamics with deterministic rates: a
;;;; HESTON-MARKET would get NO applicable method, which is the correct
;;;; answer rather than a silently wrong price.

(in-package #:fincl)

(defun bs-d1 (f k vol-sqrt-t)
  (+ (/ (log (/ f k)) vol-sqrt-t) (* 0.5d0 vol-sqrt-t)))

(defun black-scholes (phi market strike time)
  "Generalized Black-Scholes-Merton via forward and discount factors."
  (let* ((f (forward market time))
         (df (discount-factor market time))
         (vol (black-vol market strike time))
         (vol-sqrt-t (* vol (sqrt time)))
         (d1 (bs-d1 f strike vol-sqrt-t))
         (d2 (- d1 vol-sqrt-t)))
    (* phi df (- (* f (norm-cdf (* phi d1)))
                 (* strike (norm-cdf (* phi d2)))))))

;;; Specialized on ANALYTIC-ENGINE, so every analytic engine can price a
;;; European even when it exists for something else.
(defmethod price-analytic ((x european-exercise) (p vanilla-payoff)
                           (f path-independent) (e analytic-engine)
                           (m black-scholes-market))
  (values (black-scholes (phi p) m (strike p) (expiry x)) nil))

;;; Barone-Adesi-Whaley quadratic approximation for American vanillas.
(defmethod price-analytic ((x american-exercise) (p vanilla-payoff)
                           (f path-independent) (e barone-adesi-whaley-engine)
                           (m black-scholes-market))
  (let* ((phi (phi p)) (s (spot m)) (k (strike p)) (tau (expiry x))
         (r (zero-rate m tau)) (q (dividend-yield m tau))
         (vol (black-vol m k tau))
         (vol-sqrt-t (* vol (sqrt tau))))
    ;; With no dividend yield, early exercise of a call is never optimal.
    (when (and (plusp phi) (<= q 0d0))
      (return-from price-analytic (values (black-scholes phi m k tau) nil)))
    (flet ((euro (spot-value)
             ;; The formula needs European prices at trial spots, so build a
             ;; scenario market rather than re-deriving the formula.
             (black-scholes phi (make-instance 'black-scholes-market
                                               :spot spot-value :rate r
                                               :dividend q :vol vol)
                            k tau))
           (d1-at (spot-value)
             (bs-d1 (* spot-value (exp (* (- r q) tau))) k vol-sqrt-t)))
      (let* ((big-m (/ (* 2d0 r) (* vol vol)))
             (big-n (/ (* 2d0 (- r q)) (* vol vol)))
             (kf (- 1d0 (discount-factor m tau)))
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
                        (+ (black-scholes phi m k tau)
                           (* a (expt (/ s critical) qi)))
                        (* phi (- s k)))
                    nil)))))))
