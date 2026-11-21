;;;; term-structures/vol-surfaces.lisp --- smiles and implied-vol surfaces
;;;;
;;;; A smile is the total Black variance w(k) at one expiry, as a function of log-forward
;;;; moneyness k = ln(K/F(T)). It has its first two k-derivatives
;;;; (SMILE-VARIANCE-DERIVATIVES). A moneyness surface is w(k, T) with the derivatives that
;;;; the formula of Dupire needs (SURFACE-VARIANCE-DERIVATIVES). It answers the
;;;; BLACK-VOL-STRUCTURE protocol for a strike, given the forward that the market view
;;;; passes. The surface moves with the forward (sticky moneyness).
;;;;
;;;;   smiles    interpolated-smile (cubic in k over knots), svi-slice (raw SVI),
;;;;             sabr-smile (Hagan 2002, or the leading term of Obloj 2008)
;;;;   surfaces  smile-surface (smiles at expiries, w linear in T at fixed k), with
;;;;             grid-vol-surface and svi-surface as constructors. ssvi-surface
;;;;             (Gatheral-Jacquier, power-law phi)
;;;;
;;;; Times are market time, as for every structure without a reference date.

(in-package #:fincl)

(define-condition butterfly-arbitrage (pricing-error)
  ((smile :initarg :smile :reader arbitrage-smile)
   (k :initarg :k :reader arbitrage-k)
   (density :initarg :density :reader arbitrage-density))
  (:report (lambda (c s)
             (format s "~A has a negative density factor g(k) = ~,3E at k = ~,4F: the ~
smile admits butterfly arbitrage."
                     (arbitrage-smile c) (arbitrage-density c) (arbitrage-k c))))
  (:documentation "A smile whose implied density is negative somewhere (g(k) < 0 of Gatheral)."))

;;; --------------------------------------------------------------------
;;; Smiles
;;; --------------------------------------------------------------------

(defclass smile ()
  ((expiry :initarg :expiry :reader smile-expiry :type market-time
           :initform (a:required-argument :expiry)))
  (:documentation "The total Black variance at one expiry (market time) as a function of log-forward moneyness."))

(defmethod initialize-instance :after ((s smile) &key)
  (check-input (slot-value s 'expiry) (real (0)) "a positive expiry in market time")
  (setf (slot-value s 'expiry) (float (slot-value s 'expiry) 1d0)))

(defgeneric smile-variance-derivatives (smile k)
  (:documentation "(values w dw/dk d2w/dk2) of SMILE at log-moneyness K."))

(defgeneric smile-variance (smile k)
  (:documentation "Return the total Black variance w(k) of SMILE at log-moneyness K.

  (smile-variance (make-svi-slice 1 :a 0.04 :b 0.1 :rho -0.3 :m 0 :sigma 0.2) 0d0)")
  (:method ((s smile) k) (values (smile-variance-derivatives s k))))

(declaim (ftype (function (t real) (values double-float &optional)) smile-vol))
(defun smile-vol (smile k)
  "Return the Black implied vol of SMILE at log-moneyness K."
  (sqrt (/ (smile-variance smile k) (smile-expiry smile))))

(defun density-factor (w w1 w2 k)
  "Gatheral's g(k) = (1 - k w'/(2w))^2 - w'^2/4 (1/w + 1/4) + w''/2: the
implied density is g / sqrt(2 pi w) exp(-d2^2/2), so g >= 0 rules out
butterfly arbitrage."
  (+ (expt (- 1d0 (/ (* k w1) (* 2d0 w))) 2)
     (- (* (/ (* w1 w1) 4d0) (+ (/ w) 0.25d0)))
     (/ w2 2d0)))

(defun smile-k-range (smile)
  "Log-moneyness range of six at-the-money standard deviations."
  (* 6d0 (sqrt (max (smile-variance smile 0d0) 1d-8))))

(declaim (ftype (function (t &key (:points (integer 1)) (:range t))
                    (values boolean t t &optional))
          butterfly-arbitrage-free-p))
(defun butterfly-arbitrage-free-p (smile &key (points 2001) range)
  "True if g(k) >= 0 of Gatheral at POINTS log-moneyness values that are evenly spread over
[-RANGE, RANGE] (default: six ATM standard deviations). Return (values free-p k-worst
g-worst)."
  (let ((range (or range (smile-k-range smile))))
    (iter (for i below points)
          (for k = (- (* 2d0 range (/ i (1- points))) range))
          (for g = (multiple-value-bind (w w1 w2) (smile-variance-derivatives smile k)
                     (density-factor w w1 w2 k)))
          (finding (list k g) minimizing g into worst)
          (finally (return (values (>= (second worst) -1d-12) (first worst) (second worst)))))))

;;; Cubic interpolation of w in k.

(defclass interpolated-smile (smile)
  ((interpolator :initarg :interpolator :reader smile-interpolator))
  (:documentation "w(k) interpolated through knots (natural or monotone
cubic), flat beyond them."))

(declaim (ftype (function (&key (:expiry t) (:log-moneyness t) (:total-variances t) (:interpolation t))
                    (values interpolated-smile &optional))
          make-interpolated-smile))
(defun make-interpolated-smile (&key (expiry (required make-interpolated-smile :expiry))
                                     (log-moneyness (required make-interpolated-smile :log-moneyness))
                                     (total-variances (required make-interpolated-smile :total-variances))
                                     (interpolation :natural-cubic))
  "Make a smile at EXPIRY (years) through TOTAL-VARIANCES w(k) = vol^2 T at LOG-MONEYNESS k =
ln(K/F). INTERPOLATION (:NATURAL-CUBIC, :MONOTONE-CUBIC or :LINEAR) interpolates it. The
smile is flat beyond the knots.

  (make-interpolated-smile :expiry 1 :log-moneyness '(-0.2 0 0.2)
                           :total-variances '(0.05 0.04 0.042))"
  (check-input expiry real "a real expiry in years")
  (check-input interpolation (member :natural-cubic :monotone-cubic :linear)
               ":NATURAL-CUBIC, :MONOTONE-CUBIC or :LINEAR")
  (make-instance 'interpolated-smile
                 :expiry expiry
                 :interpolator (make-interpolator interpolation log-moneyness total-variances
                                                  :extrapolation :flat)))

(defmethod smile-variance-derivatives ((s interpolated-smile) k)
  (let ((i (smile-interpolator s))
        (k (float k 1d0)))
    (values (funcall i k) (interpolator-derivative i k) (interpolator-second-derivative i k))))

;;; Raw SVI.

(defclass svi-slice (smile)
  ((a :initarg :a :reader svi-a :type double-float)
   (b :initarg :b :reader svi-b :type non-negative-double)
   (rho :initarg :rho :reader svi-rho :type correlation)
   (m :initarg :m :reader svi-m :type double-float)
   (sigma :initarg :sigma :reader svi-sigma :type positive-double))
  (:documentation "The raw SVI of Gatheral: w(k) = a + b (rho (k - m) + sqrt((k - m)^2 + sigma^2))."))

(declaim (ftype (function (real &key (:a real) (:b t) (:rho t) (:m real) (:sigma t))
                    (values svi-slice &optional))
          make-svi-slice))
(defun make-svi-slice (expiry &key a b rho m sigma)
  "Make a raw SVI smile at EXPIRY. It requires b >= 0, |rho| < 1, sigma > 0 and a non-negative
minimum, a + b sigma sqrt(1 - rho^2) >= 0.

  (make-svi-slice 1 :a 0.04 :b 0.1 :rho -0.3 :m 0 :sigma 0.2)"
  (flet ((real-of (x name) (check-input x real (format nil "a real ~(~A~)" name)) (float x 1d0)))
    (let ((a (real-of a :a)) (b (real-of b :b)) (rho (real-of rho :rho))
          (m (real-of m :m)) (sigma (real-of sigma :sigma)))
      (check-input b (real 0) "b >= 0")
      (check-input rho (real (-1) (1)) "|rho| < 1")
      (check-input sigma (real (0)) "sigma > 0")
      (unless (>= (+ a (* b sigma (sqrt (- 1d0 (* rho rho))))) 0d0)
        (signal-invalid-argument 'a a 'real "a + b sigma sqrt(1 - rho^2) >= 0, a non-negative variance"))
      (make-instance 'svi-slice :expiry expiry :a a :b b :rho rho :m m :sigma sigma))))

(defmethod smile-variance-derivatives ((s svi-slice) k)
  (let* ((x (- k (svi-m s)))
         (sigma (svi-sigma s))
         (r (sqrt (+ (* x x) (* sigma sigma))))
         (b (svi-b s)))
    (values (+ (svi-a s) (* b (+ (* (svi-rho s) x) r)))
            (* b (+ (svi-rho s) (/ x r)))
            (/ (* b sigma sigma) (* r r r)))))

(defun svi-parameters (slice)
  "(a b rho m sigma) of SLICE."
  `(,(svi-a slice) ,(svi-b slice) ,(svi-rho slice) ,(svi-m slice) ,(svi-sigma slice)))

;;; SABR.

(defclass sabr-smile (smile)
  ((alpha :initarg :alpha :reader sabr-alpha :type positive-double)
   (beta :initarg :beta :reader sabr-beta :type unit-interval)
   (rho :initarg :rho :reader sabr-rho :type correlation)
   (nu :initarg :nu :reader sabr-nu :type non-negative-double)
   (forward :initarg :forward :reader sabr-forward :type positive-double)
   (approximation :initarg :approximation :reader sabr-approximation
                  :type (member :obloj :hagan)))
  (:documentation "The lognormal SABR smile at one expiry and forward. It uses the expansion of Hagan et al. 2002
(:HAGAN), or the same expansion with the leading term of Obloj 2008 (:OBLOJ).
SABR-VOLATILITY also gives the normal vols of Hagan."))

(declaim (ftype (function (real t &key (:alpha t) (:beta t) (:rho t) (:nu t) (:approximation t))
                    (values sabr-smile &optional))
          make-sabr-smile))
(defun make-sabr-smile (expiry forward &key alpha beta rho nu (approximation :obloj))
  "Make the SABR smile at EXPIRY for FORWARD. The parameters are ALPHA > 0, BETA in [0, 1], |RHO|
< 1 and NU >= 0.

  (make-sabr-smile 1 0.03 :alpha 0.02 :beta 0.5 :rho -0.3 :nu 0.4)"
  (flet ((real-of (x name) (check-input x real (format nil "a real ~(~A~)" name)) (float x 1d0)))
    (let ((alpha (real-of alpha :alpha)) (beta (real-of beta :beta))
          (rho (real-of rho :rho)) (nu (real-of nu :nu)) (forward (real-of forward :forward)))
      (check-input alpha (real (0)) "alpha > 0")
      (check-input beta (real 0 1) "beta in [0, 1]")
      (check-input rho (real (-1) (1)) "|rho| < 1")
      (check-input nu (real 0) "nu >= 0")
      (check-input forward (real (0)) "a positive forward")
      (check-input approximation (member :obloj :hagan) ":OBLOJ or :HAGAN")
      (make-instance 'sabr-smile :expiry expiry :forward forward :alpha alpha :beta beta
                                 :rho rho :nu nu :approximation approximation))))

(defun sabr-z-over-x (z rho)
  "z / x(z), x(z) = ln((sqrt(1 - 2 rho z + z^2) + z - rho)/(1 - rho)), with
its series near z = 0."
  (if (> (* z z) (* 10 double-float-epsilon))
      (/ z (log (/ (+ (sqrt (+ 1d0 (* -2d0 rho z) (* z z))) z (- rho)) (- 1d0 rho))))
      (- 1d0 (* 0.5d0 rho z) (/ (* (- (* 3d0 rho rho) 2d0) z z) 12d0))))

(defun sabr-log-moneyness (forward strike)
  "ln(F/K), by its second-order expansion when F and K are within a few ulps."
  (let ((epsilon (/ (- forward strike) strike)))
    (if (<= (abs (- forward strike)) (* 42 double-float-epsilon (min (abs forward) (abs strike))))
        (- epsilon (* 0.5d0 epsilon epsilon))
        (log (/ forward strike)))))

(declaim (ftype (function (real real real real real real real &optional t)
                    (values double-float &optional))
          sabr-volatility))
(defun sabr-volatility (strike forward expiry alpha beta rho nu &optional (approximation :hagan))
  "Return the SABR implied vol at STRIKE. The method is the lognormal expansion of Hagan (:HAGAN),
the lognormal expansion with the leading term of Obloj nu ln(F/K) / x(z), z = nu (F^(1-b) -
K^(1-b)) / (alpha (1-b)) (:OBLOJ), or the normal expansion (:NORMAL).

  (sabr-volatility 0.03 0.03 1 0.02 0.5 -0.3 0.4)"
  (let* ((strike (float strike 1d0)) (forward (float forward 1d0)) (expiry (float expiry 1d0))
         (alpha (float alpha 1d0)) (beta (float beta 1d0)) (rho (float rho 1d0)) (nu (float nu 1d0))
         (one-minus-beta (- 1d0 beta))
         (a (expt (* forward strike) one-minus-beta))
         (sqrt-a (sqrt a))
         (log-m (sabr-log-moneyness forward strike))
         (c (* one-minus-beta one-minus-beta log-m log-m))
         (time-term (lambda (first)
                      (+ 1d0 (* expiry (+ first
                                          (/ (* 0.25d0 rho beta nu alpha) sqrt-a)
                                          (* (- 2d0 (* 3d0 rho rho)) (/ (* nu nu) 24d0))))))))
    (ecase approximation
      (:hagan
       (let ((z (* (/ nu alpha) sqrt-a log-m)))
         (* (/ alpha (* sqrt-a (+ 1d0 (/ c 24d0) (/ (* c c) 1920d0))))
            (sabr-z-over-x z rho)
            (funcall time-term (/ (* one-minus-beta one-minus-beta alpha alpha) (* 24d0 a))))))
      (:obloj
       ;; nu ln(F/K) / x(z) = (nu ln(F/K) / z) (z / x(z)).
       (let* ((z (if (< one-minus-beta 1d-12)
                     (/ (* nu log-m) alpha)
                     (/ (* nu (- (expt forward one-minus-beta) (expt strike one-minus-beta)))
                        (* alpha one-minus-beta))))
              ;; nu ln(F/K) / z: alpha (1-b) ln(F/K) / (F^(1-b) - K^(1-b)), or its
              ;; limit alpha (FK)^(-(1-b)/2) at the money.
              (leading (if (or (< one-minus-beta 1d-12) (< (abs log-m) 1d-10))
                           (/ alpha sqrt-a)
                           (/ (* alpha one-minus-beta log-m)
                              (- (expt forward one-minus-beta) (expt strike one-minus-beta))))))
         (* leading (sabr-z-over-x z rho)
            (funcall time-term (/ (* one-minus-beta one-minus-beta alpha alpha) (* 24d0 a))))))
      (:normal
       (let* ((z (* (/ nu alpha) sqrt-a log-m))
              (d (* log-m log-m))
              (e (/ (+ 1d0 (/ d 24d0) (/ (* d d) 1920d0))
                    (+ 1d0 (/ c 24d0) (/ (* c c) 1920d0)))))
         (* alpha (expt (* forward strike) (/ beta 2d0)) e (sabr-z-over-x z rho)
            (funcall time-term (/ (* (- beta) (- 2d0 beta) alpha alpha) (* 24d0 a)))))))))

(defmethod smile-variance ((s sabr-smile) k)
  (let ((vol (sabr-volatility (* (sabr-forward s) (exp k)) (sabr-forward s) (smile-expiry s)
                              (sabr-alpha s) (sabr-beta s) (sabr-rho s) (sabr-nu s)
                              (sabr-approximation s))))
    (* vol vol (smile-expiry s))))

;;; SABR has no closed-form derivatives in k: central differences
;;; (FINITE-DIFFERENCE-WEIGHTS) at step 1e-4.
(defmethod smile-variance-derivatives ((s sabr-smile) k)
  (let* ((h 1d-4)
         (values (mapcar (lambda (i) (smile-variance s (+ k (* i h)))) '(-1 0 1))))
    (values (second values)
            (apply-stencil (finite-difference-weights '(-1 0 1) 1) values h 1)
            (apply-stencil (finite-difference-weights '(-1 0 1) 2) values h 2))))

;;; --------------------------------------------------------------------
;;; Surfaces
;;; --------------------------------------------------------------------

(defclass moneyness-vol-structure (black-vol-structure) ()
  (:documentation "A vol structure in log-forward moneyness: w(k, T). A market supplies the forward when the
structure is read through it. Read it alone through SURFACE-VARIANCE."))

(defgeneric surface-variance-derivatives (surface k horizon)
  (:documentation "(values w dw/dk d2w/dk2 dw/dT) of SURFACE at
log-moneyness K and HORIZON (market time)."))

(defgeneric surface-variance (surface k horizon)
  (:documentation "Return the total Black variance w(k, T) of SURFACE.")
  (:method ((s moneyness-vol-structure) k horizon)
    (values (surface-variance-derivatives s k horizon))))

(defmethod structure-black-variance ((s moneyness-vol-structure) strike horizon &optional forward)
  (unless forward
    (signal-invalid-argument 'forward forward 'real
                             (format nil "the forward to ~A: a ~(~A~) is read through a market, ~
or with SURFACE-VARIANCE in log-moneyness" horizon (class-name (class-of s)))))
  (if (zerop horizon)
      0d0
      (surface-variance s (log (/ strike forward)) horizon)))

;;; Smiles at expiries.

(defclass smile-surface (moneyness-vol-structure)
  ((expiries :initarg :expiries :reader surface-expiries :type double-vector)
   (smiles :initarg :smiles :reader surface-smiles :type simple-vector))
  (:documentation "Smiles at expiries T_1 < ... < T_n. Between expiries, w is linear in T at fixed k. Before the
first expiry, w is w(k, T_1) T / T_1. After the last expiry, it is w(k, T_n) T / T_n (constant
vol)."))

(defclass grid-vol-surface (smile-surface) ()
  (:documentation "A SMILE-SURFACE of cubic-interpolated smiles over a grid of expiries and log-moneyness."))

(defclass svi-surface (smile-surface) ()
  (:documentation "A SMILE-SURFACE of raw SVI slices."))

(declaim (ftype (function (t &key (:class t) (:check-range t))
                    (values smile-surface &optional))
          make-smile-surface))
(defun make-smile-surface (smiles &key (class 'smile-surface) (check-range nil))
  "Make the surface through SMILES (at distinct expiries. The function sorts them). It signals
CALENDAR-ARBITRAGE if w falls between consecutive expiries at some k in [-R, R]. R is
CHECK-RANGE, or six ATM standard deviations of the last smile."
  (check-input smiles (and list (not null)) "a non-empty list of smiles")
  (let* ((smiles (sort (copy-list smiles) #'< :key #'smile-expiry))
         (range (or check-range (smile-k-range (a:lastcar smiles)))))
    (iter (for (s0 s1) on smiles)
          (while s1)
          (when (= (smile-expiry s0) (smile-expiry s1))
            (signal-invalid-argument 'smiles smiles 'list "smiles at distinct expiries"))
          (iter (for i below 201)
                (for k = (- (* 2d0 range (/ i 200)) range))
                (for dw = (- (smile-variance s1 k) (smile-variance s0 k)))
                (when (< dw -1d-14)
                  (error 'calendar-arbitrage :t0 (smile-expiry s0) :t1 (smile-expiry s1)
                                             :variance dw))))
    (make-instance class :expiries (map 'double-vector #'smile-expiry smiles)
                         :smiles (coerce smiles 'simple-vector))))

(defmethod surface-variance-derivatives ((s smile-surface) k horizon)
  (let* ((times (surface-expiries s))
         (smiles (surface-smiles s))
         (n (length times))
         (horizon (float horizon 1d0)))
    (flet ((at (i) (multiple-value-list (smile-variance-derivatives (svref smiles i) k))))
      (cond ((<= horizon (aref times 0))
             (destructuring-bind (w w1 w2) (at 0)
               (let ((scale (/ horizon (aref times 0))))
                 (values (* scale w) (* scale w1) (* scale w2) (/ w (aref times 0))))))
            ((>= horizon (aref times (1- n)))
             (destructuring-bind (w w1 w2) (at (1- n))
               (let ((scale (/ horizon (aref times (1- n)))))
                 (values (* scale w) (* scale w1) (* scale w2) (/ w (aref times (1- n)))))))
            (t
             (let* ((i (knot-segment times horizon))
                    (t0 (aref times i)) (t1 (aref times (1+ i)))
                    (u (/ (- horizon t0) (- t1 t0))))
               (destructuring-bind (w0 w01 w02) (at i)
                 (destructuring-bind (w1 w11 w12) (at (1+ i))
                   (values (+ w0 (* u (- w1 w0)))
                           (+ w01 (* u (- w11 w01)))
                           (+ w02 (* u (- w12 w02)))
                           (/ (- w1 w0) (- t1 t0)))))))))))

(declaim (ftype (function (&key (:expiries t) (:log-moneyness t) (:vols t) (:interpolation t))
                    (values grid-vol-surface &optional))
          make-grid-vol-surface))
(defun make-grid-vol-surface (&key (expiries (required make-grid-vol-surface :expiries))
                                   (log-moneyness (required make-grid-vol-surface :log-moneyness))
                                   (vols (required make-grid-vol-surface :vols))
                                   (interpolation :natural-cubic))
  "Make a GRID-VOL-SURFACE. At each of EXPIRIES (market time), the surface is a cubic in
log-moneyness through VOLS. LOG-MONEYNESS is one list of log-moneyness values for every
expiry, or a list of lists, one for each expiry. VOLS is a list of lists of Black vols, one
list for each expiry. The function interpolates total variance. Calendar arbitrage between
expiries signals CALENDAR-ARBITRAGE.

  (make-grid-vol-surface :expiries '(0.5 1) :log-moneyness '(-0.2 0 0.2)
                         :vols '((0.25 0.2 0.21) (0.24 0.2 0.205)))"
  (check-input expiries (and list (not null)) "a non-empty list of expiries")
  (unless (= (length vols) (length expiries))
    (signal-invalid-argument 'vols vols 'list "one list of vols per expiry"))
  (make-smile-surface
   (iter (for expiry in expiries)
         (for row in vols)
         (for ks in (if (listp (first log-moneyness)) log-moneyness
                        (make-list (length expiries) :initial-element log-moneyness)))
         (unless (= (length ks) (length row))
           (signal-invalid-argument 'vols row 'list "one vol per log-moneyness knot"))
         (collect (make-interpolated-smile :expiry expiry :log-moneyness ks
                                           :total-variances (mapcar (lambda (vol) (* vol vol expiry)) row)
                                           :interpolation interpolation)))
   :class 'grid-vol-surface
   :check-range (reduce #'max (mapcar (lambda (ks) (reduce #'max (mapcar #'abs ks)))
                                      (if (listp (first log-moneyness)) log-moneyness `(,log-moneyness))))))

(declaim (ftype (function (t) (values svi-surface &optional)) make-svi-surface))
(defun make-svi-surface (slices)
  "Make an SVI-SURFACE through SLICES (SVI-SLICEs at distinct expiries)."
  (make-smile-surface slices :class 'svi-surface))

;;; SSVI.

(defclass ssvi-surface (moneyness-vol-structure)
  ((rho :initarg :rho :reader ssvi-rho :type correlation)
   (eta :initarg :eta :reader ssvi-eta :type positive-double)
   (gamma :initarg :gamma :reader ssvi-gamma :type unit-interval)
   (times :initarg :times :reader ssvi-times :type double-vector)
   (thetas :initarg :thetas :reader ssvi-thetas :type double-vector))
  (:documentation "The SSVI of Gatheral and Jacquier: w(k, T) = theta/2 (1 + rho phi k + sqrt((phi k + rho)^2 + 1
- rho^2)). Here theta = theta(T) is the ATM total variance (linear between knots, from 0 at T =
0, constant vol beyond the last knot). phi(theta) = eta / (theta^gamma (1 + theta)^(1 -
gamma))."))

(declaim (ftype (function (t t &key (:rho t) (:eta t) (:gamma t))
                    (values ssvi-surface &optional))
          make-ssvi-surface))
(defun make-ssvi-surface (times thetas &key rho eta gamma)
  "Make an SSVI-SURFACE with ATM total variances THETAS at TIMES (increasing), and RHO, ETA and
GAMMA. The power-law surface is free of static arbitrage when ETA (1 + |RHO|) <= 2 (Gatheral
and Jacquier 2014, eq. 4.5) and 0 < GAMMA <= 1/2 (their Corollary 4.1, condition 4, as theta
-> 0). Both conditions are required.

  (make-ssvi-surface '(0.25 0.5 1 2) '(0.01 0.02 0.04 0.08) :rho -0.4 :eta 1 :gamma 0.4)"
  (let ((times (as-double-vector times))
        (thetas (as-double-vector thetas)))
    (dolist (x `(,rho ,eta ,gamma)) (check-input x real "a real SSVI parameter"))
    (let ((rho (float rho 1d0)) (eta (float eta 1d0)) (gamma (float gamma 1d0)))
      (check-input rho (real (-1) (1)) "|rho| < 1")
      (check-input gamma (real (0) 1/2) "0 < gamma <= 1/2")
      (check-input eta (real (0)) "eta > 0")
      (unless (<= (* eta (+ 1d0 (abs rho))) (+ 2d0 1d-12))
        (signal-invalid-argument 'eta eta 'real "eta (1 + |rho|) <= 2, no static arbitrage"))
      (unless (and (= (length times) (length thetas)) (plusp (length times)))
        (signal-invalid-argument 'thetas thetas 'sequence "one ATM total variance per time"))
      (iter (for i from 0 below (length times))
            (for previous-t = (if (zerop i) 0d0 (aref times (1- i))))
            (for previous-theta = (if (zerop i) 0d0 (aref thetas (1- i))))
            (unless (> (aref times i) previous-t)
              (signal-invalid-argument 'times times 'sequence "increasing positive times"))
            (unless (>= (aref thetas i) previous-theta)
              (error 'calendar-arbitrage :t0 previous-t :t1 (aref times i)
                                         :variance (- (aref thetas i) previous-theta))))
      (make-instance 'ssvi-surface :rho rho :eta eta :gamma gamma :times times :thetas thetas))))

(defun ssvi-theta (s horizon)
  "(values theta dtheta/dT) of S at HORIZON."
  (let* ((times (ssvi-times s)) (thetas (ssvi-thetas s)) (n (length times)))
    (cond ((<= horizon (aref times 0))
           (let ((slope (/ (aref thetas 0) (aref times 0))))
             (values (* slope horizon) slope)))
          ((>= horizon (aref times (1- n)))
           (let ((slope (/ (aref thetas (1- n)) (aref times (1- n)))))
             (values (* slope horizon) slope)))
          (t (let* ((i (knot-segment times horizon))
                    (slope (/ (- (aref thetas (1+ i)) (aref thetas i))
                              (- (aref times (1+ i)) (aref times i)))))
               (values (+ (aref thetas i) (* slope (- horizon (aref times i)))) slope))))))

(defun ssvi-variance (rho eta gamma theta k)
  "(values w dw/dk d2w/dk2 dw/dtheta) of SSVI at ATM total variance THETA."
  (let* ((phi (/ eta (* (expt theta gamma) (expt (+ 1d0 theta) (- 1d0 gamma)))))
         (dphi (* phi (+ (/ (- gamma) theta) (/ (- gamma 1d0) (+ 1d0 theta)))))
         (x (* phi k))
         (r (sqrt (+ (expt (+ x rho) 2) (- 1d0 (* rho rho)))))
         (h (+ 1d0 (* rho x) r))
         (dh (+ rho (/ (+ x rho) r))))
    (values (* 0.5d0 theta h)
            (* 0.5d0 theta phi dh)
            (* 0.5d0 theta phi phi (/ (- 1d0 (* rho rho)) (* r r r)))
            (+ (* 0.5d0 h) (* 0.5d0 theta dh k dphi)))))

(defmethod surface-variance-derivatives ((s ssvi-surface) k horizon)
  (if (zerop horizon)
      (values 0d0 0d0 0d0 0d0)
      (multiple-value-bind (theta dtheta) (ssvi-theta s horizon)
        (multiple-value-bind (w w1 w2 dw/dtheta)
            (ssvi-variance (ssvi-rho s) (ssvi-eta s) (ssvi-gamma s) theta k)
          (values w w1 w2 (* dw/dtheta dtheta))))))

(declaim (ftype (function (t real) (values t &optional)) surface-smile))
(defun surface-smile (surface horizon)
  "Return the smile of SURFACE at HORIZON: an object of the smile protocol."
  (make-instance 'surface-section :expiry horizon :surface surface))

(defclass surface-section (smile)
  ((surface :initarg :surface :reader section-surface))
  (:documentation "A moneyness surface at one horizon, as a smile."))

(defmethod smile-variance-derivatives ((s surface-section) k)
  (multiple-value-bind (w w1 w2) (surface-variance-derivatives (section-surface s) k
                                                               (smile-expiry s))
    (values w w1 w2)))

(declaim (ftype (function (t &key (:times t) (:points (integer 1)) (:range t))
                    (values boolean list &optional))
          calendar-arbitrage-free-p))
(defun calendar-arbitrage-free-p (surface &key times (points 201) range)
  "True if w(k, T) does not fall in T between consecutive TIMES (default: the expiries of the
surface plus 3 and 10 years) at POINTS log-moneyness values in [-RANGE, RANGE] (default: six
ATM standard deviations at the last time). Return (values free-p (t0 t1 k) of the worst
fall)."
  (let* ((times (or times
                    (sort (remove-duplicates
                           `(,@(typecase surface
                                 (smile-surface (coerce (surface-expiries surface) 'list))
                                 (ssvi-surface (coerce (ssvi-times surface) 'list)))
                             3d0 10d0))
                          #'<)))
         (range (or range (* 6d0 (sqrt (max 1d-8 (surface-variance surface 0d0 (a:lastcar times))))))))
    (iter outer
          (for (t0 t1) on times)
          (while t1)
          (iter (for i below points)
                (for k = (- (* 2d0 range (/ i (1- points))) range))
                (for dw = (- (surface-variance surface k t1) (surface-variance surface k t0)))
                (in outer (finding `(,t0 ,t1 ,k ,dw) minimizing dw into worst)))
          (finally (return-from outer
                     (values (or (null worst) (>= (fourth worst) -1d-14)) worst))))))

;;; --------------------------------------------------------------------
;;; Bumps: a parallel move of every implied vol
;;; --------------------------------------------------------------------

(defclass shifted-surface (moneyness-vol-structure)
  ((base :initarg :base :reader shifted-base)
   (shift :initarg :shift :reader shifted-shift))
  (:documentation "BASE with every implied vol moved by SHIFT ((:absolute x)
or (:relative x)), as BUMP-STRUCTURE makes it."))

(defmethod bump-structure ((s moneyness-vol-structure) shift)
  (check-input shift shift "(:relative x) or (:absolute x)")
  (make-instance 'shifted-surface :base s :shift shift))

(defmethod surface-variance-derivatives ((s shifted-surface) k horizon)
  (multiple-value-bind (w wk wkk wt) (surface-variance-derivatives (shifted-base s) k horizon)
    (destructuring-bind (kind x) (shifted-shift s)
      (if (or (zerop horizon) (eq kind :relative))
          (let ((scale (expt (+ 1d0 (if (eq kind :relative) x 0d0)) 2)))
            (values (* scale w) (* scale wk) (* scale wkk) (* scale wt)))
          ;; w' = (sigma + x)^2 T with sigma = sqrt(w/T), by the chain rule.
          (let* ((tt (float horizon 1d0))
                 (sigma (sqrt (/ w tt)))
                 (ratio (+ 1d0 (/ x sigma))))
            (values (* (expt (+ sigma x) 2) tt)
                    (* ratio wk)
                    (- (* ratio wkk) (/ (* x wk wk) (* 2d0 sigma sigma sigma tt)))
                    (+ (expt (+ sigma x) 2) (* ratio (- wt (* sigma sigma))))))))))

;;; --------------------------------------------------------------------
;;; Rescaled surfaces: a new ATM level on a surface's shape
;;; --------------------------------------------------------------------

(defclass rescaled-surface (moneyness-vol-structure)
  ((base :initarg :base :reader rescaled-base)
   (atm-variance :initarg :atm-variance :reader rescaled-atm-variance :type function))
  (:documentation "BASE's smile at each maturity scaled to the ATM-forward
total variance ATM-VARIANCE gives: w'(k, T) = w(k, T) a(T) / w(0, T). Make
it with RESCALE-VOL-STRUCTURE."))

(defun base-variance-derivatives (base k horizon)
  "(values w w_k w_kk w_T) of BASE, a moneyness surface or a flat vol."
  (etypecase base
    (moneyness-vol-structure (surface-variance-derivatives base k horizon))
    (flat-black-vol (let ((v2 (expt (flat-vol base) 2)))
                      (values (* v2 horizon) 0d0 0d0 v2)))))

(declaim (ftype (function (t (or function symbol)) (values t &optional)) rescale-vol-structure))
(defun rescale-vol-structure (structure atm-variance)
  "Return a vol structure with the smile shape of STRUCTURE and the ATM level that ATM-VARIANCE
gives. ATM-VARIANCE is a function of the time to maturity tau (market time from the valuation
date of the market on which the result is read). It returns the total at-the-money-forward
variance a(tau) that is wanted there. The total variance of the result at log-moneyness k =
ln(K/F) is w(k, tau) a(tau) / w(0, tau). Thus at each maturity the at-the-money variance is
a(tau). Every other strike keeps its ratio to it in STRUCTURE. STRUCTURE is a moneyness
surface (SVI, SSVI, grid, smile) or a flat vol.

When a(tau) / w(0, tau) changes quickly with tau, the total variance can fall between
maturities at some strikes (calendar arbitrage). CALENDAR-ARBITRAGE-FREE-P checks a result. For
example, a simulated Heston variance v sets the level to the expected integrated variance:

  (rescale-vol-structure surface
                         (lambda (tau) (+ (* theta tau)
                                          (* (- v theta) (/ (- 1 (exp (- (* kappa tau)))) kappa)))))"
  (check-input structure (or moneyness-vol-structure flat-black-vol)
               "a moneyness vol surface or a FLAT-BLACK-VOL")
  (check-input atm-variance function "a function of the time to maturity")
  (make-instance 'rescaled-surface :base structure :atm-variance atm-variance))

(defmethod surface-variance-derivatives ((s rescaled-surface) k horizon)
  (let ((tau (float horizon 1d0)))
    (if (zerop tau)
        (values 0d0 0d0 0d0 (/ (float (funcall (rescaled-atm-variance s) 1d-6) 1d0) 1d-6))
        (let ((a (float (funcall (rescaled-atm-variance s) tau) 1d0))
              (da (difference-quotient (lambda (x) (float (funcall (rescaled-atm-variance s) x) 1d0))
                                       tau (* 1d-5 (max tau 1d-3)))))
          (multiple-value-bind (w wk wkk wt) (base-variance-derivatives (rescaled-base s) k tau)
            (multiple-value-bind (w0 w0k w0kk w0t) (base-variance-derivatives (rescaled-base s) 0d0 tau)
              (declare (ignore w0k w0kk))
              (let ((ratio (/ a w0)))
                (values (* w ratio) (* wk ratio) (* wkk ratio)
                        (+ (* wt ratio) (/ (* w da) w0) (- (/ (* w a w0t) (* w0 w0))))))))))))

;;; --------------------------------------------------------------------
;;; Printing
;;; --------------------------------------------------------------------

(defmethod print-object ((s svi-slice) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "T=~,4F a=~,5F b=~,5F rho=~,4F m=~,4F sigma=~,4F" (smile-expiry s)
            (svi-a s) (svi-b s) (svi-rho s) (svi-m s) (svi-sigma s))))

(defmethod print-object ((s sabr-smile) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "T=~,4F F=~,6F alpha=~,5F beta=~,3F rho=~,4F nu=~,4F ~(~A~)" (smile-expiry s)
            (sabr-forward s) (sabr-alpha s) (sabr-beta s) (sabr-rho s) (sabr-nu s)
            (sabr-approximation s))))

(defmethod print-object ((s smile-surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~D expiries ~,4F..~,4F" (length (surface-expiries s))
            (aref (surface-expiries s) 0) (a:last-elt (surface-expiries s)))))

(defmethod print-object ((s rescaled-surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "rescaled ~A" (rescaled-base s))))

(defmethod print-object ((s ssvi-surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "rho=~,4F eta=~,4F gamma=~,4F, ~D ATM variances" (ssvi-rho s) (ssvi-eta s)
            (ssvi-gamma s) (length (ssvi-times s)))))
