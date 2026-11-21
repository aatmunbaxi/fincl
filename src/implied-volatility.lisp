;;;; implied-volatility.lisp --- vols from prices
;;;;
;;;; IMPLIED-BLACK-VOL and IMPLIED-NORMAL-VOL invert the Black and Bachelier formulas in
;;;; closed form to machine precision (math/lets-be-rational.lisp,
;;;; math/implied-normal.lisp). IMPLIED-VOLATILITY inverts the price of any engine in the
;;;; flat vol of the underlying of the instrument.

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
  (:documentation "A price outside the range that the prices of a model span, so no volatility gives it.
The condition offers USE-VALUE, which returns a value as the volatility."))

(defun signal-price-out-of-bounds (price lower upper model)
  (restart-case (error 'price-out-of-bounds :price price :lower lower :upper upper
                                            :model model)
    (use-value (vol)
      :report "Use a value as the implied volatility."
      :interactive (lambda ()
                     (format *query-io* "~&Volatility: ")
                     (list (eval (read *query-io*))))
      vol)))

(declaim (ftype (function (real real real real real real)
                    (values double-float &optional))
          implied-black-vol))
(defun implied-black-vol (phi price forward strike discount expiry-time)
  "Return the Black volatility at which a call (PHI = 1) or put (PHI = -1) is worth
PRICE, to the precision that is attainable. The inputs are FORWARD, STRIKE, DISCOUNT
factor and EXPIRY-TIME (market time). The method is Let's Be Rational of Jäckel. A
price at the intrinsic value D max(PHI (F - K), 0) gives 0. A price below it, or at or
above the limit D F (call) or D K (put), signals PRICE-OUT-OF-BOUNDS.

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

(declaim (ftype (function (real real real real real real)
                    (values double-float &optional))
          implied-normal-vol))
(defun implied-normal-vol (phi price forward strike discount expiry-time)
  "Return the Bachelier (normal) volatility at which a call (PHI = 1) or put (PHI = -1)
is worth PRICE. The method is the closed form of Jäckel (Implied Normal Volatility,
2017). FORWARD and STRIKE can be negative. A price at the intrinsic value gives 0. A
price below it signals PRICE-OUT-OF-BOUNDS. Bachelier prices have no upper limit.

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
  (:documentation "Return the flat Black volatility of the underlying of INSTRUMENT at which ENGINE
prices INSTRUMENT at PRICE in MARKET. For European vanillas the function uses
IMPLIED-BLACK-VOL directly, whatever the engine, unless discrete dividends go ex before
the expiry. For all other instruments it inverts the engine with the method of Brent
over vols in BRACKET (default (1d-4 5d0)). It prices again with the vol of the
underlying replaced by a flat vol, so MARKET must be a COMPOSITE-MARKET. If the engine
cannot price at the low end (a pricing error, for example a binomial tree that is too
coarse for a small vol), the function raises the low end by a constant factor until
the engine can. A price at or below the price at the low end of the bracket signals
PRICE-OUT-OF-BOUNDS. An example is an American option at its intrinsic value, which
many vols give. A price at or above the price at the high end signals it too.

  (implied-volatility american-put 4.48 (make-instance 'barone-adesi-whaley-engine) m)"))

(defmethod implied-volatility ((o option) price engine market &key (bracket '(1d-4 5d0)))
  ;; With discrete dividends only the escrowed model is Black's formula on
  ;; the forward, so the engine's own model is inverted instead.
  (if (and (european-vanilla-p o) (null (option-dividends o (pricing-market market o))))
      (let* ((m (pricing-market market o))
             (x (option-exercise o))
             (p (option-payoff o)))
        (implied-black-vol (phi p) price (forward m (expiry x)) (strike p)
                           (discount-factor m (expiry x)) (time-to-expiry x m)))
      ;; Resolve a designator once: every trial vol then uses that engine.
      (invert-engine-price o price (resolve-designator o engine market) market bracket)))

(defun invert-engine-price (instrument price engine market bracket)
  (check-input market composite-market
               "a composite market, whose vol can be replaced by a flat one")
  (let* ((underlying (resolve-underlying market (instrument-underlying instrument)))
         (target (float price 1d0)))
    (flet ((price-at (vol)
             (->> (market-with-flat-vol market underlying vol)
                  (price instrument engine)
                  (nth-value 0))))
      (destructuring-bind (lo hi) bracket
        ;; Some engines cannot price at tiny vols (a binomial tree whose drift
        ;; per step exceeds its vol move): raise the low end until they can.
        (iter (for attempt from 0)
              (while (and (< lo hi)
                          (handler-case (progn (price-at lo) nil)
                            (pricing-error (c) (if (< attempt 20) t (error c))))))
              (setf lo (min hi (* 4 lo))))
        (let ((at-lo (price-at lo)) (at-hi (price-at hi)))
          (if (or (<= target at-lo) (>= target at-hi))
              (signal-price-out-of-bounds target at-lo at-hi :engine)
              (nth-value 0 (solve-brent (lambda (vol) (- (price-at vol) target))
                                        (float lo 1d0) (float hi 1d0)
                                        :tolerance 1d-14))))))))
