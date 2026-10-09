;;;; processes/nig.lisp --- the normal inverse Gaussian process
;;;;
;;;;   L(t) = beta V(t) + W(V(t)),   V an inverse Gaussian subordinator with
;;;;   E[V(t)] = delta t / gamma,  gamma = sqrt(alpha^2 - beta^2)
;;;;
;;;; O. E. Barndorff-Nielsen, "Processes of normal inverse Gaussian type",
;;;; Finance and Stochastics 2 (1998). psi(u) = -delta (sqrt(alpha^2 - (beta +
;;;; i u)^2) - gamma). E[e^L] is finite when |beta + 1| < alpha. An increment
;;;; over dt is sampled exactly: V ~ IG(delta dt / gamma, (delta dt)^2) by
;;;; Michael, Schucany and Haas's method from two normals, then a normal given
;;;; V. The density of L(t), which involves K_1, is NIG-DENSITY.

(in-package #:fincl)

(define-process nig (exponential-levy stochastic-process) (:factors 3)
  ((alpha 15d0 :bounds ((0) nil) :doc "Tail heaviness: larger is lighter.")
   (beta -5d0 :doc "Asymmetry, |beta| < alpha.")
   (delta 0.5d0 :bounds ((0) nil) :doc "Scale per unit of market time."))
  (:documentation "Barndorff-Nielsen's normal inverse Gaussian process."))

(defun nig-gamma (p)
  (sqrt (- (expt (alpha p) 2) (expt (beta p) 2))))

(defmethod exponential-moment-problem ((p nig))
  (cond ((>= (abs (beta p)) (alpha p)) "it needs |beta| < alpha")
        ((>= (abs (+ (beta p) 1d0)) (alpha p)) "it needs |beta + 1| < alpha")))

(defmethod levy-exponent ((p nig) u)
  (let ((a (alpha p)) (b (beta p)))
    (* (- (delta p))
       (- (sqrt (- (* a a) (expt (+ b (* #c(0d0 1d0) u)) 2))) (nig-gamma p)))))

(defmethod levy-cumulants ((p nig))
  (let* ((a (alpha p)) (b (beta p)) (d (delta p)) (g (nig-gamma p)))
    (values (/ (* d b) g)
            (/ (* d a a) (expt g 3))
            (/ (* 3d0 d a a (+ (* a a) (* 4d0 b b))) (expt g 7)))))

(defun nig-density (process x horizon)
  "The density at X of L(HORIZON) for the NIG PROCESS (no drift added):
alpha delta' K_1(alpha s) exp(delta' gamma + beta x) / (pi s), with delta' =
delta HORIZON and s = sqrt(delta'^2 + x^2)."
  (let* ((a (alpha process)) (b (beta process)) (d (* (delta process) horizon))
         (s (sqrt (+ (* d d) (* x x)))))
    (/ (* a d (bessel-k 1 (* a s)) (exp (+ (* d (nig-gamma process)) (* b x))))
       (* pi s))))

(defmethod log-increment-sampler ((p nig) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((drift (levy-drifts p market times))
         (g (nig-gamma p))
         (dt (interval-lengths times))
         (means (map '(simple-array double-float (*)) (lambda (h) (/ (* (delta p) h) g)) dt))
         (shapes (map '(simple-array double-float (*)) (lambda (h) (expt (* (delta p) h) 2)) dt))
         (beta (beta p)))
    (declare (type (simple-array double-float (*)) drift means shapes) (double-float beta))
    (lambda (normals j)
      (declare (type (simple-array double-float (*)) normals) (fixnum j))
      (let ((v (inverse-gaussian-from-normals (aref normals 0) (aref normals 1)
                                              (aref means j) (aref shapes j))))
        (declare (double-float v))
        (+ (aref drift j) (* beta v) (* (sqrt v) (aref normals 2)))))))
