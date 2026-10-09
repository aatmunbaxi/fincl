;;;; processes/merton.lisp --- Merton's jump diffusion
;;;;
;;;;   dS/S = (r - q - lambda k) dt + sigma dW + (e^Y - 1) dN,
;;;;   N Poisson with intensity lambda, Y ~ N(mu_J, sigma_J^2), k = E[e^Y] - 1
;;;;
;;;; R. C. Merton, "Option pricing when underlying stock returns are
;;;; discontinuous", J. Financial Economics 3 (1976). With lambda = 0 it is
;;;; GBM. Over an interval the jump part is a Poisson count n and, given n, a
;;;; normal with mean n mu_J and variance n sigma_J^2, so an increment is
;;;; sampled exactly from three normals: the Brownian shock, the count (its
;;;; quantile at Phi(z)) and the sum of the jumps.

(in-package #:fincl)

(define-process merton-jump (jump-diffusion) (:factors 3)
  ((intensity 0.5d0 :bounds (0 nil) :doc "Jumps per unit of market time, lambda.")
   (jump-mean -0.1d0 :doc "Mean of the log jump size, mu_J.")
   (jump-vol 0.15d0 :bounds (0 nil) :doc "Standard deviation of the log jump size, sigma_J."))
  (:documentation "Merton's jump diffusion: GBM plus normally distributed
log jumps at Poisson times."))

(defmethod levy-exponent ((p merton-jump) u)
  (let ((mu (jump-mean p)) (s (jump-vol p)))
    (* (intensity p) (- (exp (- (* #c(0d0 1d0) u mu) (* 0.5d0 s s u u))) 1d0))))

(defmethod levy-cumulants ((p merton-jump))
  (let ((l (intensity p)) (mu (jump-mean p)) (s2 (expt (jump-vol p) 2)))
    (values (* l mu)
            (* l (+ (* mu mu) s2))
            (* l (+ (expt mu 4) (* 6d0 mu mu s2) (* 3d0 s2 s2))))))

(defmethod log-increment-sampler ((p merton-jump) market times)
  (declare (type (simple-array double-float (*)) times))
  (multiple-value-bind (drift diffusion) (levy-drifts p market times)
    (let ((counts (map '(simple-array double-float (*))
                       (lambda (dt) (* (intensity p) dt))
                       (interval-lengths times)))
          (mu (jump-mean p))
          (sigma (jump-vol p)))
      (declare (type (simple-array double-float (*)) drift diffusion counts)
               (double-float mu sigma))
      (lambda (normals j)
        (declare (type (simple-array double-float (*)) normals) (fixnum j))
        (let ((n (poisson-from-normal (aref normals 1) (aref counts j))))
          (+ (aref drift j)
             (* (aref diffusion j) (aref normals 0))
             (if (zerop n)
                 0d0
                 (+ (* n mu) (* sigma (sqrt (float n 1d0)) (aref normals 2))))))))))
