;;;; processes/heston.lisp --- Heston stochastic volatility
;;;;
;;;;   dS = (r - q) S dt + sqrt(v) S dW1
;;;;   dv = kappa (theta - v) dt + xi sqrt(v) dW2,   corr(dW1, dW2) = rho
;;;;
;;;; Two state variables and two correlated factors: the first process here
;;;; that the single-factor protocol has to accommodate without special
;;;; cases. Two discretizations, chosen by the SCHEME slot:
;;;;
;;;; :QUADRATIC-EXPONENTIAL (the default) is L. Andersen, "Simple and
;;;; efficient simulation of the Heston stochastic volatility model", J.
;;;; Computational Finance 11 (2008): the variance is drawn from a quadratic
;;;; Gaussian or an exponential-with-atom law matching the exact conditional
;;;; mean and variance (switching at psi_c = 1.5), and ln S from the
;;;; central-discretized integrated variance (gamma_1 = gamma_2 = 1/2) with
;;;; the martingale correction K0*, so E[S(t+dt) | S(t), V(t)] = S(t)
;;;; F(t+dt)/F(t) exactly. It stays accurate at coarse steps when the Feller
;;;; condition 2 kappa theta > xi^2 fails, as it usually does in practice.
;;;;
;;;; :FULL-TRUNCATION is the Euler scheme of Lord, Koekkoek and van Dijk
;;;; (2010): simple, but biased at coarse steps when Feller fails.

(in-package #:fincl)

(defun heston-default-v0 (process market horizon)
  "sigma_ATM(T)^2 = w(T)/T, from the market's ATM-forward Black variance to
HORIZON = T."
  (declare (ignore process))
  (/ (atm-forward-variance market horizon) horizon))

(defun heston-default-theta (process market horizon)
  "V0, so that variance starts at its long-run level."
  (or (v0 process) (heston-default-v0 process market horizon)))

(define-process heston (stochastic-process) (:state-names (:spot :variance) :factors 2)
  ((v0 nil :type (or null non-negative-double) :bounds (0 nil) :market-default heston-default-v0
           :doc "Initial variance. NIL resolves to sigma_ATM(T)^2 = w(T)/T, T the
expiry, w the market's ATM-forward Black variance.")
   (kappa 2d0 :measure-dependent t :type positive-double :bounds ((0) nil) :doc "Mean-reversion speed of the variance.")
   (theta nil :measure-dependent t :type (or null non-negative-double) :bounds (0 nil) :market-default heston-default-theta
              :doc "Long-run variance. NIL resolves to V0.")
   (xi 0.3d0 :type volatility :bounds (0 nil) :doc "Vol of vol.")
   (rho -0.7d0 :type correlation :bounds (-1 1) :doc "Correlation of the spot and variance shocks.")
   (scheme :quadratic-exponential :parameter nil
           :type (member :quadratic-exponential :full-truncation)
           :doc "Path discretization: :QUADRATIC-EXPONENTIAL (Andersen's QE with
martingale correction) or :FULL-TRUNCATION (Euler)."))
  (:documentation "Heston stochastic volatility model."))

(defun heston-v0 (process market &optional (horizon 1d0))
  "V0, or its market default at HORIZON. Engines resolve the process at the
expiry first; the one-year fallback serves direct callers such as plots."
  (or (v0 process) (heston-default-v0 process market horizon)))

(defmethod spot-loadings ((p heston))
  ;; Quadratic-exponential: factor 0 drives the variance and factor 1 the
  ;; spot's own shock, W = rho Z0 + sqrt(1 - rho^2) Z1. Full truncation
  ;; drives the spot with factor 0. Bates's jump factors load nothing.
  (let ((v (make-array (process-factors p) :element-type 'double-float :initial-element 0d0))
        (rho (float (rho p) 1d0)))
    (ecase (scheme p)
      (:quadratic-exponential (setf (aref v 0) rho
                                    (aref v 1) (sqrt (max 0d0 (- 1d0 (* rho rho))))))
      (:full-truncation (setf (aref v 0) 1d0)))
    v))

(defmethod process-regressors ((p heston)) '(0 1))

(defmethod initial-state ((p heston) market)
  (let ((state (make-array 2 :element-type 'double-float)))
    (setf (aref state 0) (spot market)          ; observable
          (aref state 1) (heston-v0 p market))  ; latent variance
    state))

(defmethod make-stepper ((p heston) market times)
  (declare (type (simple-array double-float (*)) times))
  (let ((p (resolve-process p market (aref times (1- (length times))))))
    (ecase (scheme p)
      (:quadratic-exponential (heston-qe-stepper p market times))
      (:full-truncation (heston-full-truncation-stepper p market times)))))

;;; Given the variance at the start of a step, ln S is Gaussian over it with
;;; variance v+ dt (both schemes freeze or integrate v; this is the
;;; first-order bridge variance).
(defmethod bridge-variance ((p heston) market times)
  (declare (ignore market))
  (let ((dt (interval-lengths times)))
    (declare (type (simple-array double-float (*)) dt))
    (lambda (state j acc slot)
      (declare (type (simple-array double-float (*)) state acc) (fixnum j slot))
      (setf (aref acc slot) (* (max 0d0 (aref state 1)) (aref dt j)))
      nil)))

(defun heston-full-truncation-stepper (p market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((kappa (kappa p))
         (theta (theta p))
         (xi (xi p))
         (rho (rho p))
         (rho-bar (sqrt (- 1d0 (* rho rho))))
         (dt (interval-lengths times))
         (sqrt-dt (map '(simple-array double-float (*)) #'sqrt dt))
         (mu (interval-log-growths market times)))
    (declare (double-float kappa theta xi rho rho-bar)
             (type (simple-array double-float (*)) dt sqrt-dt mu))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals)
               (fixnum j)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (v (aref state 1))
             (v+ (max 0d0 v))                    ; full truncation
             (sqrt-v (sqrt v+))
             (dt (aref dt j))
             (sqrt-dt (aref sqrt-dt j))
             (z1 (aref normals 0))
             ;; Correlate the variance factor with the spot factor.
             (z2 (+ (* rho z1) (* rho-bar (aref normals 1)))))
        (declare (double-float s v v+ sqrt-v dt sqrt-dt z1 z2))
        (setf (aref state 0)
              (* s (exp (+ (- (aref mu j) (* 0.5d0 v+ dt))
                           (* sqrt-v sqrt-dt z1))))
              (aref state 1)
              (+ v
                 (* kappa (- theta v+) dt)
                 (* xi sqrt-v sqrt-dt z2))))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

(defconstant +qe-psi-critical+ 1.5d0
  "Andersen's switching level psi_c between the quadratic and exponential
variance draws.")

(defun heston-qe-stepper (p market times)
  "Andersen's QE step. NORMALS[0] drives the variance (as a normal for the
quadratic draw, as the uniform Phi(z) for the exponential one), NORMALS[1]
the log spot given the variance path."
  (declare (type (simple-array double-float (*)) times))
  (let* ((kappa (kappa p))
         (theta (theta p))
         (xi (xi p))
         (rho (rho p))
         (n (1- (length times)))
         (dt (interval-lengths times))
         (mu (interval-log-growths market times))
         (decay (make-array n :element-type 'double-float))  ; e^(-kappa dt)
         ;; s^2 = V c1 + c2: the conditional variance of V(t + dt).
         (c1 (make-array n :element-type 'double-float))
         (c2 (make-array n :element-type 'double-float))
         ;; ln S(t+dt) = ln S(t) + mu + K0* + K1 V + K2 V' + sqrt(K3 (V + V')) Z
         ;; with gamma_1 = gamma_2 = 1/2, so K3 = K4. The corrected K0* is
         ;; -log E[exp((K2 + K3/2) V') | V] - (K1 + K3/2) V, so K1 cancels.
         (k2 (make-array n :element-type 'double-float))
         (k3 (make-array n :element-type 'double-float))
         (deterministic (< xi 1d-10)))
    (declare (double-float kappa theta xi rho)
             (type (simple-array double-float (*)) dt mu decay c1 c2 k2 k3))
    (dotimes (j n)
      (let* ((h (aref dt j))
             (e (exp (- (* kappa h))))
             (one-e (- (expm1 (- (* kappa h))))))
        (setf (aref decay j) e
              (aref c1 j) (/ (* xi xi e one-e) kappa)
              (aref c2 j) (/ (* theta xi xi one-e one-e) (* 2d0 kappa))
              (aref k3 j) (* 0.5d0 h (- 1d0 (* rho rho))))
        ;; With xi = 0 the variance is deterministic and rho plays no part.
        (if deterministic
            (setf (aref k2 j) (* -0.25d0 h) (aref k3 j) (* 0.5d0 h))
            (setf (aref k2 j) (+ (* 0.5d0 h (- (/ (* kappa rho) xi) 0.5d0)) (/ rho xi))))))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals)
               (fixnum j)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             (v (aref state 1))
             (zv (aref normals 0))
             (m (+ theta (* (- v theta) (aref decay j))))
             (s2 (+ (* v (aref c1 j)) (aref c2 j)))
             (psi (if (plusp m) (/ s2 (* m m)) 0d0))
             (k2 (aref k2 j)) (k3 (aref k3 j))
             ;; The exponent of E[exp(A V') | V] that the correction removes.
             (big-a (+ k2 (* 0.5d0 k3)))
             (v-next 0d0)
             (log-m 0d0))
        (declare (double-float s v zv m s2 psi k2 k3 big-a v-next log-m))
        (cond ((<= m 0d0))                 ; variance pinned at 0
              ((or deterministic (<= psi +qe-psi-critical+))
               ;; V' = a (b + Z)^2, a noncentral chi-square(1) up to scale.
               (let* ((two/psi (if deterministic 0d0 (/ 2d0 psi)))
                      (b2 (if deterministic
                              0d0
                              (+ (- two/psi 1d0)
                                 (* (sqrt (the (double-float 0d0) two/psi))
                                    (sqrt (max 0d0 (- two/psi 1d0)))))))
                      (a (/ m (+ 1d0 b2)))
                      (b (sqrt (the (double-float 0d0) b2))))
                 (declare (double-float two/psi b2 a b))
                 (setf v-next (if deterministic m (* a (expt (+ b zv) 2))))
                 (setf log-m (if (< big-a (/ 0.5d0 a))
                                 (- (/ (* big-a b2 a) (- 1d0 (* 2d0 big-a a)))
                                    (* 0.5d0 (log (the (double-float 0d0)
                                                       (- 1d0 (* 2d0 big-a a))))))
                                 ;; No finite moment: skip the correction.
                                 (* big-a m)))))
              (t
               ;; An atom at 0 of mass p and an exponential tail of rate beta.
               (let* ((prob (/ (- psi 1d0) (+ psi 1d0)))
                      (beta (/ (- 1d0 prob) m))
                      (u (norm-cdf zv)))
                 (declare (double-float prob beta u))
                 (setf v-next (if (<= u prob)
                                  0d0
                                  (/ (log (the (double-float 0d0) (/ (- 1d0 prob) (norm-cdf (- zv))))) beta)))
                 (setf log-m (if (< big-a beta)
                                 (log (the (double-float 0d0)
                                           (+ prob (/ (* beta (- 1d0 prob)) (- beta big-a)))))
                                 (* big-a m))))))
        (setf (aref state 0)
              (* s (exp (+ (aref mu j)
                           (- log-m) (* -0.5d0 k3 v)   ; K0* + K1 V
                           (* k2 v-next)
                           (* (sqrt (the (double-float 0d0) (* k3 (+ v v-next)))) (aref normals 1)))))
              (aref state 1) v-next))
      ;; Steppers return nothing: a double-float result would be boxed.
      nil)))

;;; --------------------------------------------------------------------
;;; Characteristic function (for Fourier engines)
;;; --------------------------------------------------------------------

(defun heston-parameters (p market horizon)
  "Return (values kappa theta xi rho v0 growth), resolved at HORIZON, with
GROWTH = log(F(HORIZON)/S0)."
  (let ((p (resolve-process p market horizon)))
    (values (kappa p) (theta p) (xi p) (rho p) (v0 p)
            (log-growth market 0d0 horizon))))

(defun heston-integrated-variance (kappa theta v0 horizon)
  "Expected integrated variance over HORIZON: the whole variance when xi = 0."
  (+ (* theta horizon)
     (/ (* (- v0 theta) (- 1d0 (exp (- (* kappa horizon))))) kappa)))

(defmethod characteristic-function ((p heston) market horizon)
  (multiple-value-bind (kappa theta xi rho v0 growth) (heston-parameters p market horizon)
    (if (zerop xi)
        ;; Deterministic variance: a Gaussian log-return.
        (let* ((w (heston-integrated-variance kappa theta v0 horizon))
               (mean (- growth (* 0.5d0 w))))
          (lambda (u)
            (declare (double-float u))
            (exp (complex (* -0.5d0 w u u) (* mean u)))))
        ;; Albrecher et al.'s "little trap" form, which stays on the
        ;; principal branch of the logarithm for long maturities. With a =
        ;; i u + u^2, d^2 = beta^2 + xi^2 a, so beta - d = -xi^2 a / (beta + d):
        ;; every (beta - d)/xi^2 is written without the cancellation that
        ;; would otherwise lose about log10(1/xi^2) digits as xi -> 0.
        ;; Typed, so that the closure runs on unboxed doubles; the
        ;; expression and its operation order are unchanged. HORIZON may be
        ;; a rational: converting it first gives the same products.
        (let ((kappa kappa) (theta theta) (xi xi) (rho rho) (v0 v0) (growth growth)
              (horizon (float horizon 1d0))
              (xi2 (* xi xi)))
          (declare (double-float kappa theta xi rho v0 growth horizon xi2))
          (lambda (u)
            (declare (double-float u))
            (let* ((iu (complex 0d0 u))
                   (a (+ iu (* u u)))
                   (beta (- kappa (* rho xi iu)))
                   (d (complex-sqrt (+ (* beta beta) (* xi2 a))))
                   (bmd/xi2 (- (/ a (+ beta d))))         ; (beta - d) / xi^2
                   (g (/ (* xi2 bmd/xi2) (+ beta d)))
                   (e (exp (- (* d horizon))))
                   (c (+ (* growth iu)
                         (* kappa theta
                            (- (* bmd/xi2 horizon)
                               (/ (* 2d0 (complex-log1p (/ (* g (- 1d0 e)) (- 1d0 g)))) xi2)))))
                   (dd (* bmd/xi2 (/ (- 1d0 e) (- 1d0 (* g e))))))
              (declare (type (complex double-float) iu a beta d bmd/xi2 g e c dd))
              (exp (+ c (* dd v0)))))))))


;;; --------------------------------------------------------------------
;;; Change of measure
;;; --------------------------------------------------------------------
;;;
;;; ln S is driven by sqrt(v) W1 and v by xi sqrt(v) W2. A variance premium
;;; is taken only where it keeps the variance a square-root process, so the
;;; QE or Euler scheme runs unchanged with physical parameters (Heston 1993;
;;; Cheridito, Filipovic and Kimmel 2007): lambda = c xi sqrt(v), a drift
;;; change c xi^2 v, gives kappa^P = kappa - c xi^2; a constant drift change d
;;; gives theta^P = (kappa theta + d) / kappa^P.

(define-condition feller-condition-violated (warning)
  ((process :initarg :process :reader feller-process)
   (measure :initarg :measure :reader feller-measure)
   (kappa :initarg :kappa :reader feller-kappa)
   (theta :initarg :theta :reader feller-theta)
   (xi :initarg :xi :reader feller-xi))
  (:report (lambda (c s)
             (format s "Under the ~(~A~) measure, ~A has 2 kappa theta = ~,6F <= xi^2 = ~,6F: ~
the variance reaches 0, and a variance premium keeps the two measures equivalent only when ~
the Feller condition holds under both."
                     (feller-measure c) (feller-process c)
                     (* 2 (feller-kappa c) (feller-theta c)) (expt (feller-xi c) 2))))
  (:documentation "A Heston change of measure whose variance violates the
Feller condition 2 kappa theta > xi^2 under the risk-neutral or the
physical measure, so the measures may not be equivalent. Signalled once per
image for each set of parameters."))

(defvar *feller-warnings* (make-hash-table :test 'equal)
  "Parameter lists FELLER-CONDITION-VIOLATED has been signalled for.")

(defvar *feller-warnings-lock* (bt:make-lock "fincl Feller warnings"))

(defun warn-feller (process measure kappa theta xi)
  "Signal FELLER-CONDITION-VIOLATED once per image for these parameters if
2 KAPPA THETA <= XI^2."
  (when (<= (* 2 kappa theta) (* xi xi))
    (let ((key `(,measure ,kappa ,theta ,xi)))
      (unless (bt:with-lock-held (*feller-warnings-lock*)
                (prog1 (gethash key *feller-warnings*)
                  (setf (gethash key *feller-warnings*) t)))
        (warn 'feller-condition-violated :process process :measure measure
                                         :kappa kappa :theta theta :xi xi)))))

(defmethod state-coordinate ((p heston) name)
  (case name (:spot :log) (:variance :level)))

(defmethod state-diffusion ((p heston) market name times)
  (declare (ignore market times))
  (values (if (eq name :spot)
              (lambda (j time state)
                (declare (ignore j time) (type double-vector state))
                (sqrt (max 0d0 (aref state 1))))
              (let ((xi (xi p)))
                (declare (double-float xi))
                (lambda (j time state)
                  (declare (ignore j time) (type double-vector state))
                  (* xi (sqrt (max 0d0 (aref state 1)))))))
          nil))

(defmethod premium-form-problem ((p heston) name kind spec)
  (when (and (eq name :variance)
             (not (or (and (eq kind :price-of-risk) (consp spec))
                      (and (eq kind :drift-change) (realp spec)))))
    (format nil "on :variance (:scaled c), a price of risk proportional to its diffusion, or a ~
constant drift change, which keep the variance a square-root process; or physical :kappa and ~
:theta through :parameters")))

(defmethod measure-change-dynamics ((q heston) premia market horizon)
  (multiple-value-bind (kind spec) (premium-on premia :variance)
    (let* ((given (getf premia :parameters))
           (kappa (kappa q)) (theta (theta q)) (xi (xi q))
           (kappa-p (getf given 'kappa kappa))
           (theta-p (getf given 'theta theta)))
      (when kind
        (when (or (member 'kappa given) (member 'theta given))
          (refuse-measure-change q "a premium on :variance and physical :kappa or :theta together; ~
give one or the other"))
        (if (eq kind :price-of-risk)
            (setf kappa-p (- kappa (* (second spec) xi xi))
                  theta-p (if (plusp kappa-p) (/ (* kappa theta) kappa-p) theta))
            (setf theta-p (/ (+ (* kappa theta) spec) kappa)))
        (unless (plusp kappa-p)
          (refuse-measure-change q "the variance premium gives kappa^P = ~,6F; it must be positive ~
(c < kappa / xi^2 = ~,6F)" kappa-p (/ kappa (* xi xi))))
        (when (minusp theta-p)
          (refuse-measure-change q "the variance premium gives theta^P = ~,6F; it must be non-negative"
                                 theta-p)))
      (when (or kind (member 'kappa given) (member 'theta given))
        (warn-feller q :risk-neutral kappa theta xi)
        (warn-feller q :physical kappa-p theta-p xi))
      (if kind
          (call-next-method q (premia-with-parameters (premia-without premia :variance)
                                                      'kappa kappa-p 'theta theta-p)
                            market horizon)
          (call-next-method)))))
