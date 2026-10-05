;;;; implied-volatility.lisp --- vols from prices
;;;;
;;;; IMPLIED-BLACK-VOL and IMPLIED-NORMAL-VOL invert the Black and Bachelier
;;;; formulas in closed form to machine precision (math/lets-be-rational.lisp,
;;;; math/implied-normal.lisp). IMPLIED-VOLATILITY inverts any engine's price
;;;; in the flat vol of the instrument's underlying.

(in-package #:fincl)

(define-condition price-out-of-bounds (pricing-error)
  ((price :initarg :price :reader error-price)
   (lower :initarg :lower :reader error-lower)
   (upper :initarg :upper :initform nil :reader error-upper)
   (model :initarg :model :reader error-model))
  (:report (lambda (c s)
             (format s "Price ~F has no ~(~A~) implied volatility: it must lie in ~
~:[[~;(~]~F, ~:[infinity~;~:*~F~])."
                     (error-price c) (error-model c)
                     (eq (error-model c) :engine) (error-lower c) (error-upper c))))
  (:documentation "A price outside the range a model's prices span, so no
volatility reproduces it. Offers USE-VALUE, returning a value as the
volatility."))

(defun signal-price-out-of-bounds (price lower upper model)
  (restart-case (error 'price-out-of-bounds :price price :lower lower :upper upper
                                            :model model)
    (use-value (vol)
      :report "Use a value as the implied volatility."
      :interactive (lambda ()
                     (format *query-io* "~&Volatility: ")
                     (list (eval (read *query-io*))))
      vol)))

(defun implied-black-vol (phi price forward strike discount expiry-time)
  "The Black volatility at which a call (PHI = 1) or put (PHI = -1) with
FORWARD, STRIKE, DISCOUNT factor and EXPIRY-TIME (market time) is worth
PRICE, to the attainable precision, by Jäckel's Let's Be Rational. A price
at the intrinsic value D max(PHI (F - K), 0) gives 0; a price below it, or
at or above the limit D F (call) or D K (put), signals PRICE-OUT-OF-BOUNDS.

  (implied-black-vol 1 10.4506d0 105.127d0 100 0.951229d0 1) => 0.2..."
  (let* ((phi (float phi 1d0)) (price (float price 1d0))
         (forward (float forward 1d0)) (strike (float strike 1d0))
         (discount (float discount 1d0)) (tau (float expiry-time 1d0))
         (intrinsic (max (* phi (- forward strike)) 0d0))
         (limit (if (plusp phi) forward strike))
         ;; Bounds are checked on discounted prices, then the undiscounted
         ;; price is kept inside them: PRICE / D can round past D INTRINSIC / D.
         (undiscounted (max (/ price discount) intrinsic)))
    (if (or (< price (* discount intrinsic)) (>= price (* discount limit))
            (>= undiscounted limit))
        (signal-price-out-of-bounds price (* discount intrinsic) (* discount limit) :black)
        (lbr-implied-volatility undiscounted forward strike tau phi))))

(defun implied-normal-vol (phi price forward strike discount expiry-time)
  "The Bachelier (normal) volatility at which a call (PHI = 1) or put (PHI =
-1) is worth PRICE, by Jäckel's closed form (Implied Normal Volatility,
2017). FORWARD and STRIKE may be negative. A price at the intrinsic value
gives 0; below it signals PRICE-OUT-OF-BOUNDS. Bachelier prices have no
upper limit.

  (implied-normal-vol 1 0.002277d0 0.01d0 0.012d0 0.95d0 1) => 0.008..."
  (let* ((phi (float phi 1d0)) (price (float price 1d0))
         (forward (float forward 1d0)) (strike (float strike 1d0))
         (discount (float discount 1d0)) (tau (float expiry-time 1d0))
         (intrinsic (max (* phi (- forward strike)) 0d0))
         (undiscounted (max (/ price discount) intrinsic))
         (distance (abs (- forward strike))))
    (cond ((< price (* discount intrinsic))
           (signal-price-out-of-bounds price (* discount intrinsic) nil :bachelier))
          ((zerop distance)
           ;; At the money: v = sigma sqrt(T) phi(0).
           (/ (* undiscounted +sqrt-two-pi+) (sqrt tau)))
          (t
           ;; (1.6)-(1.8), conditioned on the out-of-the-money option.
           (let ((target (- (/ (abs (- undiscounted intrinsic)) distance))))
             (if (or (zerop target) (> target (phi-tilde -38.4d0)))
                 ;; Time value below what Phi~ resolves: no vol to speak of.
                 0d0
                 (/ distance (* (abs (inverse-phi-tilde target)) (sqrt tau)))))))))

;;; --------------------------------------------------------------------
;;; From any engine
;;; --------------------------------------------------------------------

(defun european-vanilla-p (o)
  (and (typep (option-exercise o) 'european-exercise)
       (typep (option-payoff o) 'vanilla-payoff)
       (typep (option-path o) 'path-independent)))

(defgeneric implied-volatility (instrument price engine market &key bracket)
  (:documentation "The flat Black volatility of INSTRUMENT's underlying at
which ENGINE prices INSTRUMENT at PRICE in MARKET. European vanillas use
IMPLIED-BLACK-VOL directly, whatever the engine. Anything else is inverted
by Brent's method over vols in BRACKET (default (1d-4 5d0)), re-pricing with
the underlying's vol replaced by a flat one, so MARKET must be a
COMPOSITE-MARKET. If the engine cannot price at the low end (a pricing
error, e.g. a binomial tree too coarse for a tiny vol), the low end is
raised geometrically until it can. A price at or below the price at the bracket's low end
(e.g. an American at its intrinsic value, which many vols reproduce) or at
or above the high end's signals PRICE-OUT-OF-BOUNDS.

  (implied-volatility american-put 4.48 (make-instance 'barone-adesi-whaley-engine) m)"))

(defmethod implied-volatility ((o option) price engine market &key (bracket '(1d-4 5d0)))
  (if (european-vanilla-p o)
      (let* ((m (pricing-market market o))
             (x (option-exercise o))
             (p (option-payoff o)))
        (implied-black-vol (phi p) price (forward m (expiry x)) (strike p)
                           (discount-factor m (expiry x)) (time-to-expiry x m)))
      (invert-engine-price o price engine market bracket)))

(defun invert-engine-price (instrument price engine market bracket)
  (check-input market composite-market
               "a composite market, whose vol can be replaced by a flat one")
  (let* ((underlying (resolve-underlying market (instrument-underlying instrument)))
         (target (float price 1d0)))
    (flet ((price-at (vol)
             (nth-value 0 (price instrument engine
                                 (market-with-flat-vol market underlying vol)))))
      (destructuring-bind (lo hi) bracket
        ;; Some engines cannot price at tiny vols (a binomial tree whose drift
        ;; per step exceeds its vol move): raise the low end until they can.
        (loop for attempt from 0
              while (and (< lo hi)
                         (handler-case (progn (price-at lo) nil)
                           (pricing-error (c) (if (< attempt 20) t (error c)))))
              do (setf lo (min hi (* 4 lo))))
        (let ((at-lo (price-at lo)) (at-hi (price-at hi)))
          (if (or (<= target at-lo) (>= target at-hi))
              (signal-price-out-of-bounds target at-lo at-hi :engine)
              (nth-value 0 (solve-brent (lambda (vol) (- (price-at vol) target))
                                        (float lo 1d0) (float hi 1d0)
                                        :tolerance 1d-14))))))))
