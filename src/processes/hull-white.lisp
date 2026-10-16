;;;; processes/hull-white.lisp --- the Hull-White one-factor short rate
;;;;
;;;;   dr = (theta(t) - a r) dt + sigma dW
;;;;
;;;; theta(t) fits the initial curve exactly, so the model's zero-coupon bonds
;;;; reprice the curve. The model takes its curve from the market at pricing
;;;; time (an id, by default the instrument currency's discount curve): one
;;;; curve both forecasts and discounts. The formulas follow QuantLib's
;;;; HullWhite (A, B, discount bonds and bond options), operation for
;;;; operation (tests/fixtures/hull-white.sexp).

(in-package #:fincl)

(define-process hull-white (stochastic-process) ()
  ((a 0.05d0 :bounds (0 nil) :reader hull-white-a
             :doc "Mean-reversion speed of the short rate.")
   (sigma 0.01d0 :bounds (0 nil) :doc "Normal volatility of the short rate."))
  (:documentation "Hull-White one-factor short-rate model, fitted to a yield
curve. Priced through rates engines (:JAMSHIDIAN, :HULL-WHITE-TREE); it does
not drive an equity spot."))

(defmethod simulatable-p ((p hull-white)) nil)

(defstruct (rate-curve (:constructor %make-rate-curve (market curve)))
  "A yield curve of MARKET read in market time from the valuation date."
  (market nil :read-only t)
  (curve nil :read-only t))

(defun make-rate-curve (market curve)
  "CURVE (an id in MARKET, or a curve) in MARKET's time."
  (%make-rate-curve market (curve-in-market (if (keywordp curve) (find-yield-curve market curve) curve)
                                            market)))

(defun rate-curve-discount (curve time)
  (curve-discount (rate-curve-curve curve) (float time 1d0)))

(defun rate-curve-forward (curve time)
  "The instantaneous continuous forward at TIME over *RATE-DT* (QuantLib's
forwardRate(t, t, Continuous))."
  (let* ((t1 (max (- time (/ *rate-dt* 2d0)) 0d0))
         (t2 (+ t1 *rate-dt*)))
    (/ (log (/ (rate-curve-discount curve t1) (rate-curve-discount curve t2))) (- t2 t1))))

(defun rate-curve-short-rate (curve)
  "The continuous zero rate over the first *RATE-DT* (QuantLib's zeroRate(0))."
  (/ (log (/ 1d0 (rate-curve-discount curve *rate-dt*))) *rate-dt*))

(defun hull-white-b (a t0 t1)
  "B(t0, t1) = (1 - e^(-a (t1 - t0))) / a, or t1 - t0 when a is tiny."
  (if (< a (sqrt double-float-epsilon))
      (- t1 t0)
      (/ (- 1d0 (exp (* (- a) (- t1 t0)))) a)))

(defun hull-white-a-factor (process curve t0 t1)
  "A(t0, t1): the bond P(t0, t1) is A e^(-B r)."
  (let* ((a (hull-white-a process))
         (b (hull-white-b a t0 t1))
         (temp (* (sigma process) b))
         (value (- (* b (rate-curve-forward curve t0))
                   (* 0.25d0 temp temp (hull-white-b a 0d0 (* 2d0 t0))))))
    (/ (* (exp value) (rate-curve-discount curve t1)) (rate-curve-discount curve t0))))

(defun hull-white-discount-bond (process curve t0 t1 rate)
  "The zero-coupon bond P(T0, T1) when the short rate at T0 is RATE, CURVE a
RATE-CURVE (times in its market's time).

  (hull-white-discount-bond hw (make-rate-curve m :eur) 1d0 5d0 0.03d0)"
  (* (hull-white-a-factor process curve t0 t1)
     (exp (- (* (hull-white-b (hull-white-a process) t0 t1) rate)))))

(defun hull-white-discount (process curve time)
  "The model's discount factor to TIME: P(0, TIME) at the initial short rate."
  (hull-white-discount-bond process curve 0d0 time (rate-curve-short-rate curve)))

(defun hull-white-bond-option (process curve phi strike maturity bond-start bond-maturity)
  "A call (PHI 1) or put (PHI -1) expiring at MATURITY on the bond paying 1
at BOND-MATURITY, per unit of the bond starting at BOND-START, struck at
STRIKE (QuantLib's HullWhite::discountBondOption)."
  (let* ((a (hull-white-a process))
         (sigma (sigma process))
         (v (if (< a (sqrt double-float-epsilon))
                (* sigma (hull-white-b a bond-start bond-maturity) (sqrt maturity))
                (let ((c (+ (- (exp (* -2d0 a (- bond-start maturity)))
                               (exp (* -2d0 a bond-start))
                               (* 2d0 (- (exp (- (* a (- (+ bond-start bond-maturity) (* 2d0 maturity)))))
                                         (exp (- (* a (+ bond-start bond-maturity)))))))
                            (exp (* -2d0 a (- bond-maturity maturity)))
                            (- (exp (* -2d0 a bond-maturity))))))
                  (* (/ sigma (* a (sqrt (* 2d0 a)))) (sqrt (max c 0d0))))))
         (f (rate-curve-discount curve bond-maturity))
         (k (* (rate-curve-discount curve bond-start) strike)))
    (black-price phi k f v 1d0)))

(defun black-price (phi strike forward std-dev discount)
  "Black's formula from a standard deviation, as QuantLib's blackFormula:
the discounted intrinsic value when STD-DEV is 0."
  (if (zerop std-dev)
      (* (max (* (- forward strike) phi) 0d0) discount)
      (let* ((d1 (+ (/ (log (/ forward strike)) std-dev) (* 0.5d0 std-dev)))
             (d2 (- d1 std-dev)))
        (* discount phi (- (* forward (norm-cdf (* phi d1))) (* strike (norm-cdf (* phi d2))))))))
