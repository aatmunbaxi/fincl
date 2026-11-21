;;;; processes/kou.lisp --- Kou's double-exponential jump diffusion
;;;;
;;;;   dS/S = (r - q - lambda k) dt + sigma dW + (e^Y - 1) dN,
;;;;   Y ~ Exp(eta_1) with probability p, -Exp(eta_2) with probability 1 - p
;;;;
;;;; S. G. Kou, "A jump-diffusion model for option pricing", Management Science 48 (2002).
;;;; eta_1 > 1 keeps E[e^Y] finite. With lambda = 0, the process is GBM. Up jumps and down
;;;; jumps are independent Poisson streams (intensities lambda p and lambda (1 - p)). A sum
;;;; of n Exp(eta) jumps is Gamma(n, 1/eta). Thus the code samples an increment exactly
;;;; from five normals: the Brownian shock, two counts and two gamma sums. It takes the
;;;; counts and sums as quantiles at Phi(z).

(in-package #:fincl)

(define-process kou-jump (jump-diffusion) (:factors 5)
  ((intensity 1d0 :measure-dependent t :type non-negative-double :bounds (0 nil) :doc "Jumps per unit of market time, lambda.")
   (up-probability 0.4d0 :measure-dependent t :type unit-interval :bounds (0 1) :doc "Probability p that a jump is upwards.")
   (eta1 10d0 :measure-dependent t :type positive-double :bounds ((1) nil) :doc "Rate of the upward exponential jumps; the mean
up jump is 1/eta1.")
   (eta2 5d0 :measure-dependent t :type positive-double :bounds ((0) nil) :doc "Rate of the downward exponential jumps; the
mean down jump is 1/eta2."))
  (:documentation "The jump diffusion of Kou: GBM plus double-exponential log jumps at Poisson times."))

(defmethod levy-exponent ((p kou-jump) u)
  (let ((iu (* #c(0d0 1d0) u)) (up (up-probability p)) (eta1 (eta1 p)) (eta2 (eta2 p)))
    (* (intensity p) (+ (/ (* up eta1) (- eta1 iu))
                        (/ (* (- 1d0 up) eta2) (+ eta2 iu))
                        -1d0))))

(defmethod levy-exponent-function ((p kou-jump))
  (let* ((l (intensity p)) (up (up-probability p)) (eta1 (eta1 p)) (eta2 (eta2 p))
         (up-eta1 (* up eta1))
         (down-eta2 (* (- 1d0 up) eta2)))
    (declare (double-float l up eta1 eta2 up-eta1 down-eta2))
    (lambda (u)
      (declare (double-float u))
      (let ((iu (* #c(0d0 1d0) u)))
        (declare (type (complex double-float) iu))
        (* l (+ (/ up-eta1 (- eta1 iu))
                (/ down-eta2 (+ eta2 iu))
                -1d0))))))

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
           (l (intensity p))
           (up (up-probability p))
           (ups (map '(simple-array double-float (*))
                     (lambda (h) (* l up h)) dt))
           (downs (map '(simple-array double-float (*))
                       (lambda (h) (* l (- 1d0 up) h)) dt))
           (eta1 (eta1 p))
           (eta2 (eta2 p)))
      (declare (type (simple-array double-float (*)) drift diffusion ups downs)
               (double-float l up eta1 eta2))
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
