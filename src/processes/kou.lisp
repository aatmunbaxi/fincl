;;;; processes/kou.lisp --- Kou's double-exponential jump diffusion
;;;;
;;;;   dS/S = (r - q - lambda k) dt + sigma dW + (e^Y - 1) dN,
;;;;   Y ~ Exp(eta_1) with probability p, -Exp(eta_2) with probability 1 - p
;;;;
;;;; S. G. Kou, "A jump-diffusion model for option pricing", Management
;;;; Science 48 (2002). eta_1 > 1 keeps E[e^Y] finite. With lambda = 0 it is
;;;; GBM. Up and down jumps are independent Poisson streams (intensities
;;;; lambda p and lambda (1 - p)), and a sum of n Exp(eta) jumps is
;;;; Gamma(n, 1/eta), so an increment is sampled exactly from five normals:
;;;; the Brownian shock, two counts and two gamma sums, all by quantiles at
;;;; Phi(z).

(in-package #:fincl)

(define-process kou-jump (jump-diffusion) (:factors 5)
  ((intensity 1d0 :bounds (0 nil) :doc "Jumps per unit of market time, lambda.")
   (up-probability 0.4d0 :bounds (0 1) :doc "Probability p that a jump is upwards.")
   (eta1 10d0 :bounds ((1) nil) :doc "Rate of the upward exponential jumps; the mean
up jump is 1/eta1.")
   (eta2 5d0 :bounds ((0) nil) :doc "Rate of the downward exponential jumps; the
mean down jump is 1/eta2."))
  (:documentation "Kou's jump diffusion: GBM plus double-exponential log
jumps at Poisson times."))

(defmethod levy-exponent ((p kou-jump) u)
  (let ((iu (* #c(0d0 1d0) u)) (up (up-probability p)) (eta1 (eta1 p)) (eta2 (eta2 p)))
    (* (intensity p) (+ (/ (* up eta1) (- eta1 iu))
                        (/ (* (- 1d0 up) eta2) (+ eta2 iu))
                        -1d0))))

(defmethod levy-cumulants ((p kou-jump))
  (let ((l (intensity p)) (up (up-probability p)) (down (- 1d0 (up-probability p)))
        (eta1 (eta1 p)) (eta2 (eta2 p)))
    ;; The n-th cumulant of the jump part is lambda E[Y^n], and
    ;; E[Y^n] = n! (p / eta1^n + (-1)^n (1 - p) / eta2^n).
    (values (* l (- (/ up eta1) (/ down eta2)))
            (* 2d0 l (+ (/ up (expt eta1 2)) (/ down (expt eta2 2))))
            (* 24d0 l (+ (/ up (expt eta1 4)) (/ down (expt eta2 4)))))))

(defmethod log-increment-sampler ((p kou-jump) market times)
  (declare (type (simple-array double-float (*)) times))
  (multiple-value-bind (drift diffusion) (levy-drifts p market times)
    (let* ((dt (interval-lengths times))
           (ups (map '(simple-array double-float (*))
                     (lambda (h) (* (intensity p) (up-probability p) h)) dt))
           (downs (map '(simple-array double-float (*))
                       (lambda (h) (* (intensity p) (- 1d0 (up-probability p)) h)) dt))
           (eta1 (eta1 p))
           (eta2 (eta2 p)))
      (declare (type (simple-array double-float (*)) drift diffusion ups downs)
               (double-float eta1 eta2))
      (flet ((jumps (z-count z-size mean-count eta)
               (declare (double-float z-count z-size mean-count eta))
               (let ((n (poisson-from-normal z-count mean-count)))
                 (if (zerop n)
                     0d0
                     (/ (gamma-from-normal z-size (float n 1d0)) eta)))))
        (lambda (normals j)
          (declare (type (simple-array double-float (*)) normals) (fixnum j))
          (+ (aref drift j)
             (* (aref diffusion j) (aref normals 0))
             (jumps (aref normals 1) (aref normals 3) (aref ups j) eta1)
             (- (jumps (aref normals 2) (aref normals 4) (aref downs j) eta2))))))))
