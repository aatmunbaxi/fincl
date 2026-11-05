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

(define-process hull-white (stochastic-process) (:state-size 2 :factors 2)
  ((a 0.05d0 :bounds (0 nil) :reader hull-white-a
             :doc "Mean-reversion speed of the short rate.")
   (sigma 0.01d0 :bounds (0 nil) :doc "Normal volatility of the short rate."))
  (:documentation "Hull-White one-factor short-rate model, fitted to a yield
curve. Priced through rates engines (:JAMSHIDIAN, :HULL-WHITE-TREE); it does
not drive an equity spot. In SIMULATE it drives a yield curve, ((:curve id)
(make-hull-white) :knots tenors), its state the short rate and its integral
(:SHORT-RATE :INTEGRAL)."))

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

(defun hull-white-bond-terms (process curve t0 t1)
  "(values A B) of the bond P(T0, T1) = A e^(-B r), which do not depend on the
short rate r.

  (hull-white-bond-terms hw (make-rate-curve m :eur) 1d0 5d0)"
  (values (hull-white-a-factor process curve t0 t1)
          (hull-white-b (hull-white-a process) t0 t1)))

(declaim (inline hull-white-bond-from-terms))
(defun hull-white-bond-from-terms (a-factor b rate)
  "The bond A e^(-B RATE) for the A and B of HULL-WHITE-BOND-TERMS."
  (* a-factor (exp (- (* b rate)))))

(defun hull-white-discount-bond (process curve t0 t1 rate)
  "The zero-coupon bond P(T0, T1) when the short rate at T0 is RATE, CURVE a
RATE-CURVE (times in its market's time).

  (hull-white-discount-bond hw (make-rate-curve m :eur) 1d0 5d0 0.03d0)"
  (multiple-value-bind (a-factor b) (hull-white-bond-terms process curve t0 t1)
    (hull-white-bond-from-terms a-factor b rate)))

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

;;; --------------------------------------------------------------------
;;; Simulation: Hull-White drives a yield curve (task 32)
;;; --------------------------------------------------------------------
;;;
;;; With x = r - alpha(t), alpha(t) = f(0,t) + sigma^2 B(t)^2 / 2 and
;;; B(t) = (1 - e^(-a t))/a, the pair (x, I = int x ds) is Gaussian over a
;;; step of length d: x' = x e^(-a d) + e1 and I' - I = x B(d) + e2, with
;;; Var e1 = sigma^2 d h(2 a d), Var e2 = sigma^2 d^3 g(a d) and
;;; Cov = sigma^2 B(d)^2 / 2, h(y) = (1 - e^(-y))/y and g(u) =
;;; (u - 2(1 - e^(-u)) + (1 - e^(-2u))/2)/u^3. The stepper is exact. The
;;; integral of alpha over a step is log P(0,t0)/P(0,t1) + (V(t1) - V(t0))/2,
;;; V(t) = sigma^2 t^3 g(a t) the variance of int_0^t x, so the mean of
;;; exp(-int r) is P(0,t) exactly.

(defclass curve-view (market)
  ((market :initarg :market :reader view-market)
   (curve-id :initarg :curve-id :reader view-curve-id))
  (:documentation "The composite MARKET as a process driving its yield curve
CURVE-ID in SIMULATE sees it: the valuation date and day counter of MARKET,
no discrete dividends."))

(defmethod valuation-date ((v curve-view)) (valuation-date (view-market v)))
(defmethod market-day-counter ((v curve-view)) (market-day-counter (view-market v)))

(define-process fitted-hull-white (hull-white) ()
  ((rate-curve nil :parameter nil :type t :reader fitted-rate-curve
               :doc "The RATE-CURVE the process is fitted to."))
  (:constructor nil)
  (:documentation "A Hull-White process fitted to one curve of a market, as
SIMULATE resolves a Hull-White leg."))

(defmethod process-targets ((p hull-white)) '(:curve))
(defmethod process-state-names ((p hull-white)) '(:short-rate :integral))
(defmethod steppable-p ((p hull-white)) t)

(defmethod resolve-process ((p hull-white) (market curve-view) horizon)
  (declare (ignore horizon))
  (make-instance 'fitted-hull-white :a (hull-white-a p) :sigma (sigma p)
                                    :rate-curve (make-rate-curve (view-market market) (view-curve-id market))))

(defun hw-h (y)
  "(1 - e^(-y))/y, 1 at 0."
  (if (zerop y) 1d0 (/ (- (expm1 (- y))) y)))

(defun hw-g (u)
  "(u - 2(1 - e^(-u)) + (1 - e^(-2u))/2)/u^3, by its series below 0.01."
  (if (< u 1d-2)
      (+ 1/3 (* u (+ -1/4 (* u (+ 7/60 (* u (+ -1/24 (* u 31/2520))))))))
      (/ (+ u (* 2d0 (expm1 (- u))) (* -0.5d0 (expm1 (* -2d0 u)))) (* u u u))))

(defun hw-alpha (process curve time)
  "alpha(t) = f(0,t) + sigma^2 B(t)^2 / 2, the mean of the short rate."
  (let ((b (* time (hw-h (* (hull-white-a process) time)))))
    (+ (rate-curve-forward curve time) (* 0.5d0 (expt (sigma process) 2) b b))))

(defun hw-integral-variance (process time)
  "V(t) = sigma^2 t^3 g(a t), the variance of the integral of x over [0, t]."
  (* (expt (sigma process) 2) time time time (hw-g (* (hull-white-a process) time))))

(defmethod initial-state ((p fitted-hull-white) (market curve-view))
  (let ((state (make-array 2 :element-type 'double-float :initial-element 0d0)))
    (setf (aref state 0) (hw-alpha p (fitted-rate-curve p) 0d0))
    state))

(defmethod make-stepper ((p fitted-hull-white) (market curve-view) times)
  (declare (type double-vector times))
  (let* ((n (1- (length times)))
         (a (hull-white-a p))
         (s2 (expt (sigma p) 2))
         (curve (fitted-rate-curve p))
         (alpha (map 'double-vector (lambda (time) (hw-alpha p curve time)) times))
         (decay (make-array n :element-type 'double-float))
         (bs (make-array n :element-type 'double-float))
         (alpha-integral (make-array n :element-type 'double-float))
         (s1 (make-array n :element-type 'double-float))
         (l21 (make-array n :element-type 'double-float))
         (l22 (make-array n :element-type 'double-float)))
    (iter (for (t0 t1) in-intervals times)
          (for j from 0)
          (let* ((d (- t1 t0))
                 (b (* d (hw-h (* a d))))
                 (v1 (* s2 d (hw-h (* 2d0 a d))))
                 (v2 (* s2 d d d (hw-g (* a d))))
                 (c (* 0.5d0 s2 b b))
                 (sd1 (sqrt v1)))
            (setf (aref decay j) (exp (- (* a d)))
                  (aref bs j) b
                  (aref alpha-integral j) (+ (log (/ (rate-curve-discount curve t0) (rate-curve-discount curve t1)))
                                             (* 0.5d0 (- (hw-integral-variance p t1) (hw-integral-variance p t0))))
                  (aref s1 j) sd1
                  (aref l21 j) (if (plusp sd1) (/ c sd1) 0d0)
                  (aref l22 j) (sqrt (max 0d0 (- v2 (expt (aref l21 j) 2)))))))
    (lambda (state normals j)
      (declare (type double-vector state normals) (fixnum j)
               (optimize speed))
      (let ((x (- (aref state 0) (aref alpha j)))
            (z0 (aref normals 0))
            (z1 (aref normals 1)))
        (incf (aref state 1) (+ (aref alpha-integral j) (* x (aref bs j))
                                (* (aref l21 j) z0) (* (aref l22 j) z1)))
        (setf (aref state 0) (+ (aref alpha (1+ j)) (* x (aref decay j)) (* (aref s1 j) z0)))
        nil))))

(defparameter *default-curve-knots*
  (append (mapcar (lambda (n) (make-tenor n :months)) '(1 3 6))
          (mapcar (lambda (n) (make-tenor n :years)) '(1 2 3 5 7 10 15 20 30)))
  "The tenors of the curve a simulated short-rate leg builds when the process has no
closed-form bond price (STATE-DISCOUNT-BOND is NIL).")

(defun curve-knot-dates (date knots last)
  "The dates DATE plus each of KNOTS (tenors), ascending and distinct, cut at
LAST (a date or NIL) but keeping at least the first."
  (let* ((dates (-<> (mapcar (lambda (tenor) (add-tenor date tenor)) knots)
                     (remove-if-not (lambda (d) (date< date d)) <>)
                     (sort <> #'date<)
                     (remove-duplicates <> :test #'date=)))
         (kept (if last (remove-if (lambda (d) (date< last d)) dates) dates)))
    (or kept (and dates `(,(first dates)))
        (signal-invalid-argument 'knots knots 'list "tenors after the date (positive tenors)"))))

(defmethod state-discount-bond ((p fitted-hull-white) state t0 t1)
  (hull-white-discount-bond p (fitted-rate-curve p) t0 t1 (aref state 0)))

(defgeneric state-discount-function (process state t0)
  (:documentation "(lambda (t1)) giving STATE-DISCOUNT-BOND of PROCESS at
STATE from T0 to T1, with what depends only on T0 computed once.")
  (:method ((p stochastic-process) state t0)
    (lambda (t1) (state-discount-bond p state t0 t1))))

(defmethod state-discount-function ((p fitted-hull-white) state t0)
  ;; HULL-WHITE-DISCOUNT-BOND's operations, in its order, with the T0 terms
  ;; hoisted: the same doubles.
  (let* ((a (hull-white-a p))
         (sigma (sigma p))
         (curve (fitted-rate-curve p))
         (t0 (float t0 1d0))
         (forward (rate-curve-forward curve t0))
         (b2 (hull-white-b a 0d0 (* 2d0 t0)))
         (d0 (rate-curve-discount curve t0))
         (r (aref state 0)))
    (declare (double-float a sigma t0 forward b2 d0 r))
    (lambda (t1)
      (let* ((b (hull-white-b a t0 t1))
             (temp (* sigma b))
             (value (- (* b forward) (* 0.25d0 temp temp b2))))
        (declare (double-float b temp value))
        (* (/ (* (exp value) (rate-curve-discount curve t1)) d0)
           (exp (- (* b r))))))))

(defclass state-curve (dated-curve)
  ((process :initarg :process :reader state-curve-process)
   (state :initarg :state :reader state-curve-state :type double-vector
          :documentation "A copy of the leg's state on the reference date.")
   (now :initarg :now :reader state-curve-now :type double-float
        :documentation "The reference date in the leg's market time.")
   (shifts :initarg :shifts :initform '() :reader state-curve-shifts
           :documentation "Zero-rate shifts applied in order, from BUMP-STRUCTURE.")
   (discount :initform nil
             :documentation "STATE-DISCOUNT-FUNCTION at NOW, made on the first price."))
  (:documentation "The yield curve on a simulated path: the discount to tau is
STATE-DISCOUNT-BOND of the process at its state from now to now + tau, exact
at every maturity and evaluated only when asked. Its day counter is the leg's
market's, so tau is market time."))

(defmethod curve-discount ((c state-curve) (x real))
  (let ((tau (float x 1d0)))
    (check-curve-time c tau)
    (if (zerop tau)
        1d0
        (let ((d (funcall (the function
                               (or (slot-value c 'discount)
                                   (setf (slot-value c 'discount)
                                         (state-discount-function (state-curve-process c) (state-curve-state c)
                                                                  (state-curve-now c)))))
                          (+ (state-curve-now c) tau))))
          (dolist (shift (state-curve-shifts c) d)
            (setf d (if (eq (first shift) :absolute)
                        (* d (exp (- (* (float (second shift) 1d0) tau))))
                        (exp (- (* (apply-shift (/ (- (log d)) tau) shift) tau))))))))))

(defmethod bump-structure ((c state-curve) shift)
  ;; Zero rates move by SHIFT at every maturity: e^(-z tau) -> e^(-z' tau).
  (check-input shift shift "(:relative x) or (:absolute x)")
  (make-instance 'state-curve :reference-date (curve-reference-date c) :day-counter (curve-day-counter c)
                              :process (state-curve-process c) :state (state-curve-state c)
                              :now (state-curve-now c) :shifts `(,@(state-curve-shifts c) ,shift)))

(defmethod print-object ((c state-curve) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A, ~(~A~) at ~{~,6F~^ ~}" (curve-reference-date c)
            (class-name (class-of (state-curve-process c))) (coerce (state-curve-state c) 'list))))

(defmethod structure-at-state ((p hull-white) target initial market date state &key knots)
  "The curve of the bonds P(t, T | r) at DATE: without :KNOTS a STATE-CURVE,
exact at every maturity; with :KNOTS (tenors, cut at the initial curve's
last date) log-linear in discount factors between them, on the initial
curve's day counter."
  (declare (ignore market))
  (unless (typep p 'fitted-hull-white)
    (signal-invalid-argument 'process p 'fitted-hull-white
                             (format nil "a Hull-White process fitted to ~S (as SIMULATE resolves its legs)" target)))
  (let* ((curve (fitted-rate-curve p))
         (initial-market (rate-curve-market curve))
         (now (market-time initial-market date)))
    (if (null knots)
        (make-instance 'state-curve :reference-date date :day-counter (market-day-counter initial-market)
                                    :process p :state (copy-seq state) :now (float now 1d0))
        (let ((r (aref state 0))
              (dates (curve-knot-dates date knots
                                       (and (typep initial 'interpolated-curve) (a:last-elt (curve-dates initial))))))
          (make-instance 'interpolated-discount-curve
                         :dates (cons date dates)
                         :day-counter (if (typep initial 'dated-curve)
                                          (curve-day-counter initial)
                                          (market-day-counter initial-market))
                         :discounts (cons 1d0 (mapcar (lambda (d)
                                                        (hull-white-discount-bond p curve now
                                                                                  (market-time initial-market d) r))
                                                      dates)))))))
