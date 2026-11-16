;;;; processes/variance-gamma.lisp --- the variance gamma process
;;;;
;;;;   L(t) = theta G(t) + sigma W(G(t)),   G a gamma process with E[G(t)] = t,
;;;;   Var[G(t)] = nu t
;;;;
;;;; D. B. Madan, P. P. Carr and E. C. Chang, "The variance gamma process and
;;;; option pricing", European Finance Review 2 (1998). psi(u) = -log(1 - i u
;;;; theta nu + sigma^2 nu u^2 / 2) / nu. As nu -> 0 the gamma clock becomes
;;;; calendar time and L is a Brownian motion with drift theta: GBM. An
;;;; increment is sampled exactly by subordination: a Gamma(dt/nu, nu) time
;;;; (its quantile at Phi(z)), then a normal given that time.

(in-package #:fincl)

(define-process variance-gamma (exponential-levy stochastic-process) (:factors 2)
  ((sigma nil :measure-dependent t :bounds ((0) nil) :market-default gbm-default-vol
              :doc "Volatility of the Brownian motion run on the gamma clock. NIL
resolves to sigma_ATM(T) = sqrt(w(T)/T), the market's ATM-forward Black vol to
the expiry, which the process reproduces as nu -> 0 with theta = 0.")
   (nu 0.2d0 :bounds ((0) nil) :doc "Variance rate of the gamma clock: kurtosis.")
   (theta -0.14d0 :measure-dependent t :doc "Drift on the gamma clock: skewness."))
  (:documentation "The variance gamma process of Madan, Carr and Chang.

Under a change of measure SIGMA and THETA may take physical values and NU
may not: with NU fixed the Levy measures C e^(-G|x|)/|x| (x < 0) and C
e^(-Mx)/x (x > 0), C = 1/NU, are equivalent for any G, M > 0 (Sato,
Theorem 33.1), and (SIGMA, THETA) <-> (G, M) is one to one. A drift change
is refused: a pure-jump process has no Brownian driver, and its drift beside
the jumps is fixed by equivalence."))

(defmethod exponential-moment-problem ((p variance-gamma))
  (let ((sigma (sigma p)))
    (when (and sigma (<= (- 1d0 (* (theta p) (nu p)) (* 0.5d0 sigma sigma (nu p))) 0d0))
      "it needs theta nu + sigma^2 nu / 2 < 1")))

(defmethod levy-exponent ((p variance-gamma) u)
  (let ((sigma (sigma p)) (nu (nu p)) (theta (theta p)))
    (/ (- (complex-log (coerce (+ 1d0 (* #c(0d0 -1d0) u theta nu) (* 0.5d0 sigma sigma nu u u))
                               '(complex double-float))))
       nu)))

(defmethod levy-exponent-function ((p variance-gamma))
  (let* ((sigma (sigma p)) (nu (nu p)) (theta (theta p))
         (half-s2-nu (* 0.5d0 sigma sigma nu)))
    (declare (double-float sigma nu theta half-s2-nu))
    (lambda (u)
      (declare (double-float u))
      (/ (- (complex-log (coerce (+ 1d0 (* #c(0d0 -1d0) u theta nu) (* half-s2-nu u u))
                                 '(complex double-float))))
         nu))))

(defmethod levy-cumulants ((p variance-gamma))
  (let ((s2 (expt (sigma p) 2)) (nu (nu p)) (theta (theta p)))
    (values theta
            (+ s2 (* nu theta theta))
            (* 3d0 (+ (* s2 s2 nu) (* 4d0 s2 theta theta nu nu) (* 2d0 (expt theta 4) (expt nu 3)))))))

(defmethod log-increment-sampler ((p variance-gamma) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((drift (levy-drifts p market times))
         (nu (nu p))
         (shapes (map '(simple-array double-float (*)) (lambda (dt) (/ dt nu))
                      (interval-lengths times)))
         (sigma (sigma p))
         (theta (theta p)))
    (declare (type (simple-array double-float (*)) drift shapes)
             (double-float nu sigma theta))
    (lambda (normals j)
      (declare (type (simple-array double-float (*)) normals) (fixnum j))
      (let ((g (* nu (gamma-from-normal (aref normals 0) (aref shapes j)))))
        (declare (double-float g))
        (+ (aref drift j) (* theta g) (* sigma (sqrt g) (aref normals 1)))))))
