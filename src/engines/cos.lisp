;;;; engines/cos.lisp --- the Fourier-cosine (COS) method
;;;;
;;;; F. Fang and C. W. Oosterlee, "A novel pricing method for European
;;;; options based on Fourier-cosine series expansions", SIAM J. Sci. Comput.
;;;; 31 (2008); and "Pricing early-exercise and discrete barrier options by
;;;; Fourier-cosine series expansions", Numer. Math. 114 (2009).
;;;;
;;;; Work in x = ln(S/K). The transition density is expanded in a cosine
;;;; series on a truncation range [a, b], whose coefficients come straight
;;;; from the characteristic function; the payoff's cosine coefficients are
;;;; closed forms. A European is one inner product of the two. A Bermudan is
;;;; the same step taken backwards between exercise dates, with the early
;;;; exercise boundary found by a root search at each date. An American is
;;;; the Richardson extrapolation of Bermudans with 1, 2, 4 and 8 times as
;;;; many dates.
;;;;
;;;; Europeans only need CHARACTERISTIC-FUNCTION and LOG-CUMULANTS from the
;;;; process. The backward recursion also assumes the log-increment between
;;;; dates is independent of the current state, which holds for Levy
;;;; processes (GBM here) but not for Heston, whose variance is a second
;;;; state variable; so early exercise is specialized on GBM. Calls are
;;;; priced as puts throughout (parity for Europeans, McDonald-Schroder
;;;; symmetry for early exercise), because a put's payoff is bounded.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Building blocks
;;; --------------------------------------------------------------------

(defun cumulants-from-cf (cf)
  "Estimate (values c1 c2 c4) of a log-return from its characteristic
function CF. With K(u) = log CF(u), Re K(u) = -c2 u^2/2 + c4 u^4/24 - ...
and Im K(u) = c1 u - c3 u^3/6 + ..., so Im K(u)/u and -2 Re K(u)/u^2 tend
to c1 and c2 with errors in u^2: samples at u = 2h and h are extrapolated
in u^2 (RICHARDSON-EXTRAPOLATE). c4 comes from eliminating c2 between the
same two samples of Re K."
  (flet ((k (u) (log (funcall cf u)))
         (extrapolate-in-u^2 (at-2u at-u)
           (nth-value 0 (richardson-extrapolate (list at-2u at-u) :exponents '(2)))))
    (let* ((h0 1d-3)
           (c2-guess (/ (* -2d0 (realpart (k h0))) (* h0 h0))))
      (unless (plusp c2-guess)
        (error "Characteristic function has no positive variance near u = 0."))
      (let* ((h (/ 0.3d0 (sqrt c2-guess)))
             (r1 (realpart (k h)))
             (r2 (realpart (k (* 2d0 h))))
             (c1 (extrapolate-in-u^2 (/ (imagpart (k (* 2d0 h0))) (* 2d0 h0))
                                     (/ (imagpart (k h0)) h0)))
             (c2 (extrapolate-in-u^2 (/ (* -2d0 r2) (* 4d0 h h))
                                     (/ (* -2d0 r1) (* h h))))
             (c4 (* 24d0 (/ (- r2 (* 4d0 r1)) (* 12d0 h h h h)))))
        (values c1 c2 c4)))))

(defun cos-range (process market horizon x0 truncation)
  "Truncation range (values a b) around X0 + c1. Uses the process's
LOG-CUMULANTS if it has them, otherwise estimates them from its
characteristic function; NIL if it has neither."
  (multiple-value-bind (c1 c2 c4) (log-cumulants process market horizon)
    (unless c1
      (let ((cf (characteristic-function process market horizon)))
        (when cf
          (multiple-value-setq (c1 c2 c4) (cumulants-from-cf cf)))))
    (when c1
      (let ((half (* truncation (sqrt (+ (abs c2) (sqrt (abs c4)))))))
        (values (- (+ x0 c1) half) (+ x0 c1 half))))))

(defun cos-payoff-coefficients (phi strike n a b c d)
  "Cosine coefficients on [A, B] of K max(PHI (e^x - 1), 0) restricted to
[C, D], where PHI is 1 for a call and -1 for a put and [C, D] lies where
the payoff is positive. Zero when C >= D."
  (declare (double-float phi strike a b c d) (fixnum n))
  (let ((g (make-array n :element-type 'double-float :initial-element 0d0)))
    (when (< c d)
      (let* ((width (- b a))
             (theta (/ pi width))
             (scale (* phi strike (/ 2d0 width)))
             (ec (exp c))
             (ed (exp d)))
        (dotimes (k n)
          (let* ((w (* k theta))
                 (cd (cos (* w (- d a)))) (sd (sin (* w (- d a))))
                 (cc (cos (* w (- c a)))) (sc (sin (* w (- c a))))
                 ;; chi_k: integral of e^x cos(w (x - a)); psi_k: of cos.
                 (chi (/ (+ (- (* cd ed) (* cc ec)) (* w (- (* sd ed) (* sc ec))))
                         (+ 1d0 (* w w))))
                 (psi (if (zerop k) (- d c) (/ (- sd sc) w))))
            (setf (aref g k) (* scale (- chi psi)))))))
    g))

(defun cos-cf-samples (cf n theta)
  "CF at u_k = k THETA for k below N, with the k = 0 term halved (the
primed sum of the COS formulas)."
  (let ((v (make-array n :element-type '(complex double-float))))
    (dotimes (k n v)
      (setf (aref v k) (coerce (funcall cf (* k theta)) '(complex double-float))))
    (setf (aref v 0) (* 0.5d0 (aref v 0)))
    v))

(defun cos-value-at (x cf-samples coeffs a theta discount)
  "DISCOUNT * Re sum_k cf_k V_k exp(i u_k (X - A)): the value one step back
at the point X."
  (declare (double-float x a theta discount)
           (type (simple-array (complex double-float) (*)) cf-samples)
           (type (simple-array double-float (*)) coeffs))
  (let ((sum 0d0))
    (declare (double-float sum))
    (dotimes (k (length coeffs))
      (incf sum (* (aref coeffs k)
                   (realpart (* (aref cf-samples k) (cis (* k theta (- x a))))))))
    (* discount sum)))

(defun cos-integral-table (n x1 x2 a b)
  "I(m) = integral over [X1, X2] of exp(i m theta (x - a)) dx, theta =
pi / (B - A), for m = -(N-1) .. 2N-1, stored at index m + N - 1."
  (declare (fixnum n) (double-float x1 x2 a b))
  (let* ((theta (/ pi (- b a)))
         (offset (1- n))
         (table (make-array (+ offset (* 2 n)) :element-type '(complex double-float))))
    (loop for m from (- offset) below (* 2 n)
          do (setf (aref table (+ m offset))
                   (if (zerop m)
                       (complex (- x2 x1) 0d0)
                       (/ (- (cis (* m theta (- x2 a))) (cis (* m theta (- x1 a))))
                          (complex 0d0 (* m theta))))))
    table))

(defun cos-continuation-coefficients (cf-samples coeffs x1 x2 a b discount)
  "Cosine coefficients on [A, B] of the continuation value restricted to
[X1, X2]:

  C_k = DISCOUNT Re sum_j cf_j V_j M_kj,
  M_kj = (2 / (b - a)) integral_{x1}^{x2} exp(i u_j (x - a)) cos(u_k (x - a)) dx
       = (I(j + k) + I(j - k)) / (b - a),

with I from COS-INTEGRAL-TABLE. The I(j + k) part is a Hankel and the
I(j - k) part a Toeplitz matrix-vector product over the same sequence, so
both come out of one linear convolution of the reversed w_j = cf_j V_j with
the I table: O(N log N) by FFT instead of O(N^2) (Fang and Oosterlee 2009,
section 3.2)."
  (declare (type (simple-array (complex double-float) (*)) cf-samples)
           (type (simple-array double-float (*)) coeffs)
           (double-float x1 x2 a b discount))
  (let* ((n (length coeffs))
         (out (make-array n :element-type 'double-float :initial-element 0d0)))
    (declare (fixnum n))
    (when (< x1 x2)
      (let* ((table (cos-integral-table n x1 x2 a b))
             ;; Linear convolution of lengths N and 3N - 1 needs 4N - 2 points.
             (size (ash 1 (integer-length (- (* 4 n) 3))))
             (reversed (make-array size :element-type '(complex double-float)
                                        :initial-element #c(0d0 0d0)))
             (padded (make-array size :element-type '(complex double-float)
                                      :initial-element #c(0d0 0d0)))
             (scale (/ discount (- b a) size)))
        (declare (type (simple-array (complex double-float) (*)) table reversed padded))
        (dotimes (j n)
          (setf (aref reversed (- n 1 j)) (* (aref cf-samples j) (aref coeffs j))))
        (replace padded table)
        (fft! reversed)
        (fft! padded)
        (dotimes (i size)
          (setf (aref padded i) (* (aref padded i) (aref reversed i))))
        (fft! padded :inverse t)
        ;; With q = 2N - 2: the Hankel term is conv[q + k], Toeplitz conv[q - k].
        (let ((q (- (* 2 n) 2)))
          (dotimes (k n)
            (setf (aref out k)
                  (* scale (realpart (+ (aref padded (+ q k))
                                        (aref padded (- q k))))))))))
    out))

(defun cos-exercise-boundary (phi strike cf-samples coeffs a b discount)
  "The x where continuation equals the payoff. Puts exercise below it,
calls above; the boundary is A (put) or B (call) if exercise never pays."
  (let ((theta (/ pi (- b a))))
    (flet ((h (x)
             (- (cos-value-at x cf-samples coeffs a theta discount)
                (* strike (max 0d0 (* phi (- (exp x) 1d0)))))))
      (if (minusp phi)
          (let ((lo a) (hi (min 0d0 b)))
            (cond ((>= (h lo) 0d0) a)
                  ((<= (h hi) 0d0) hi)
                  (t (nth-value 0 (solve-brent #'h lo hi)))))
          (let ((lo (max 0d0 a)) (hi b))
            (cond ((>= (h hi) 0d0) b)
                  ((<= (h lo) 0d0) lo)
                  (t (nth-value 0 (solve-brent #'h lo hi)))))))))

(defun cos-bermudan-value (phi strike x0 times discount-fn cf-fn n a b)
  "Value at time 0 of a vanilla exercisable at TIMES (increasing, positive,
in market time; the last is expiry) and not at time 0. DISCOUNT-FN maps a
horizon to a discount factor; CF-FN maps a step length to the
characteristic function of the log-increment over it. With one time this
is the European price."
  (let* ((theta (/ pi (- b a)))
         (times (coerce times 'simple-vector))
         (coeffs (if (plusp phi)
                     (cos-payoff-coefficients phi strike n a b (max 0d0 a) b)
                     (cos-payoff-coefficients phi strike n a b a (min 0d0 b)))))
    (loop for i from (- (length times) 2) downto 0
          do (let* ((now (svref times i))
                    (next (svref times (1+ i)))
                    (cf (cos-cf-samples (funcall cf-fn (- next now)) n theta))
                    (discount (/ (funcall discount-fn next) (funcall discount-fn now)))
                    (x* (cos-exercise-boundary phi strike cf coeffs a b discount)))
               (setf coeffs
                     (map '(simple-array double-float (*)) #'+
                          (if (plusp phi)
                              (cos-continuation-coefficients cf coeffs a x* a b discount)
                              (cos-continuation-coefficients cf coeffs x* b a b discount))
                          (if (plusp phi)
                              (cos-payoff-coefficients phi strike n a b x* b)
                              (cos-payoff-coefficients phi strike n a b a x*))))))
    (let ((first (svref times 0)))
      (cos-value-at x0 (cos-cf-samples (funcall cf-fn first) n theta) coeffs a theta
                    (funcall discount-fn first)))))

(defun cos-setup (process engine market horizon strike exercise payoff path)
  "Return (values x0 a b cf-fn discount-fn), or signal
UNSUPPORTED-COMBINATION if PROCESS has no characteristic function or
cumulants."
  (let ((x0 (log (/ (spot market) strike))))
    (multiple-value-bind (a b)
        (cos-range process market horizon x0 (float (truncation engine) 1d0))
      (unless (and a (characteristic-function process market horizon))
        (error 'unsupported-combination
               :exercise exercise :payoff payoff :path path
               :process process :engine engine :market market))
      (values x0 a b
              (lambda (dt) (characteristic-function process market dt))
              (lambda (horizon) (discount-factor market horizon))))))

;;; --------------------------------------------------------------------
;;; European: any process with a characteristic function
;;; --------------------------------------------------------------------

;;; Calls are priced as puts plus put-call parity, C = P + D (F - K): the
;;; put's payoff is bounded, so its expansion is insensitive to the upper
;;; truncation bound, as Fang and Oosterlee recommend.
(defmethod price-cos ((x european-exercise) (p vanilla-payoff) (f path-independent)
                      (process stochastic-process) (e cos-engine) market)
  (let ((tau (time-to-expiry x market))
        (k (strike p)))
    (multiple-value-bind (x0 a b cf-fn discount-fn)
        (cos-setup process e market tau k x p f)
      (let ((put (cos-bermudan-value -1d0 k x0 (list tau) discount-fn cf-fn
                                     (n-terms e) a b)))
        (values (if (plusp (phi p))
                    (+ put (* (discount-factor market (expiry x))
                              (- (forward market (expiry x)) k)))
                    put)
                nil)))))

;;; --------------------------------------------------------------------
;;; Early exercise: Levy processes
;;; --------------------------------------------------------------------

;;; The backward recursion is accurate for puts, whose payoff is bounded,
;;; but a call's payoff grows like e^b at the top of the truncation range
;;; and its coefficients lose about three digits there. Under GBM a call is
;;; priced exactly as a put by McDonald-Schroder symmetry:
;;;
;;;   C(S, K, r, q, sigma) = P(K, S, q, r, sigma)
;;;
;;; for European, Bermudan and American exercise alike.
(defun gbm-put-inputs (process engine market horizon phi strike exercise payoff path)
  "Inputs to COS-BERMUDAN-VALUE for the put that prices this option:
(values x0 strike a b cf-fn discount-fn). A put (PHI = -1) is itself; a
call is its symmetric put."
  (if (minusp phi)
      (multiple-value-bind (x0 a b cf-fn discount-fn)
          (cos-setup process engine market horizon strike exercise payoff path)
        (values x0 strike a b cf-fn discount-fn))
      (let* ((spot (spot market))
             (vol (gbm-vol process market horizon))
             ;; Rate and dividend yield swap roles.
             (rate (lambda (h) (dividend-yield market h)))
             (yield (lambda (h) (zero-rate market h)))
             (x0 (log (/ strike spot)))
             (c1 (- (* (- (funcall rate horizon) (funcall yield horizon)) horizon)
                    (* 0.5d0 vol vol horizon)))
             (half (* (float (truncation engine) 1d0) vol (sqrt horizon))))
        (values x0 spot (- (+ x0 c1) half) (+ x0 c1 half)
                (lambda (dt)
                  (let* ((variance (* vol vol dt))
                         (mean (- (* (- (funcall rate dt) (funcall yield dt)) dt)
                                  (* 0.5d0 variance))))
                    (lambda (u)
                      (declare (double-float u))
                      (exp (complex (* -0.5d0 variance u u) (* mean u))))))
                (lambda (h) (exp (- (* (funcall rate h) h))))))))

(defmethod price-cos ((x bermudan-exercise) (p vanilla-payoff) (f path-independent)
                      (process gbm) (e cos-engine) market)
  (let* ((today (valuation-date market))
         (times (loop for date in (exercise-dates x)
                      when (date< today date)
                        collect (market-time market date)))
         (tau (a:lastcar times)))
    (multiple-value-bind (x0 k a b cf-fn discount-fn)
        (gbm-put-inputs process e market tau (phi p) (strike p) x p f)
      (let ((value (cos-bermudan-value -1d0 k x0 times discount-fn cf-fn
                                       (n-terms e) a b)))
        (values (if (exercise-allowed-p x today)
                    (max value (payoff-value p (spot market)))
                    value)
                nil)))))

(defmethod price-cos ((x american-exercise) (p vanilla-payoff) (f path-independent)
                      (process gbm) (e cos-engine) market)
  (let ((tau (time-to-expiry x market))
        (m (richardson-dates e)))
    (multiple-value-bind (x0 k a b cf-fn discount-fn)
        (gbm-put-inputs process e market tau (phi p) (strike p) x p f)
      (flet ((bermudan (dates)
               (cos-bermudan-value -1d0 k x0
                                   (loop for i from 1 to dates collect (* i (/ tau dates)))
                                   discount-fn cf-fn (n-terms e) a b)))
        ;; Four-point Richardson extrapolation in the exercise interval,
        ;; with errors in dt, dt^2 and dt^3: Fang and Oosterlee (2009),
        ;; section 4.
        (let ((v (nth-value 0 (richardson-extrapolate
                               (mapcar #'bermudan (list m (* 2 m) (* 4 m) (* 8 m)))
                               :exponents '(1 2 3)))))
          (values (max v (payoff-value p (spot market))) nil))))))
