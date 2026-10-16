;;;; math/distributions.lisp --- non-normal variates from normal draws
;;;;
;;;; The Monte Carlo engine hands a process standard normals only: normal k of
;;;; path i is a pure function of (seed, i, k), which is what makes prices
;;;; independent of the chunking. A process that needs a Poisson count or a
;;;; gamma time therefore takes one normal Z per variate and inverts the
;;;; target CDF at Phi(Z). That is exact in distribution, consumes a fixed
;;;; number of normals per step, and an antithetic partner (-Z) gets the
;;;; mirrored quantile. Upper-half draws are inverted through the survival
;;;; function at Phi(-Z), so they keep their precision.

(in-package #:fincl)

(defun poisson-from-normal (z mean)
  "The Poisson(MEAN) quantile at Phi(Z): the least n with P(N <= n) >=
Phi(Z), found by summing the probabilities from 0 (or from the mode, for a
large MEAN).

  (poisson-from-normal 0d0 0.5d0) => 0"
  (declare (double-float z mean))
  (if (<= mean 0d0)
      0
      (let ((u (norm-cdf z)))
        (declare (double-float u))
        (if (< mean 30d0)
            (let* ((pmf (exp (- mean))) (cdf pmf))
              (declare (double-float pmf cdf))
              (iter (declare (declare-variables))
                    (for (the fixnum n) from 0)
                    (while (and (< cdf u) (plusp pmf)))
                    (setf pmf (/ (* pmf mean) (1+ n)))
                    (incf cdf pmf)
                    (finally (return n))))
            ;; Start at the mode, with P(N <= m) = Q(m + 1, mean), and walk.
            (let* ((m (floor mean))
                   (pmf (exp (- (* m (log mean)) mean (log-gamma (+ m 1d0)))))
                   (cdf (nth-value 1 (regularized-gamma (+ m 1d0) mean))))
              (declare (double-float pmf cdf) (fixnum m))
              (if (>= cdf u)
                  ;; Down: drop P(N = n) while P(N <= n - 1) still covers u.
                  (iter (declare (declare-variables))
                        (for (the fixnum n) from m above 0)
                        (while (>= (- cdf pmf) u))
                        (decf cdf pmf)
                        (setf pmf (/ (* pmf n) mean))
                        (finally (return n)))
                  (iter (declare (declare-variables))
                        (for (the fixnum n) from m)
                        (while (and (< cdf u) (plusp pmf)))
                        (setf pmf (/ (* pmf mean) (1+ n)))
                        (incf cdf pmf)
                        (finally (return n)))))))))

(defun gamma-from-normal (z shape)
  "The Gamma(SHAPE, 1) quantile at Phi(Z), by safeguarded Newton steps on
the regularized incomplete gamma P (or Q, for Z > 0) in log x. Relative
accuracy about 1e-12.

  (gamma-from-normal 0d0 1d0) => 0.6931471805599453d0 (the median, log 2)"
  (declare (double-float z shape))
  (let* ((upper (plusp z))
         (target (norm-cdf (if upper (- z) z)))   ; P = target, or Q = target
         (log-gamma-a (log-gamma shape))
         ;; P(a, x) = x^a / Gamma(a + 1) (1 + O(x)) near 0.
         (log-small-x (/ (+ (log target) log-gamma-a (log shape)) shape)))
    (declare (double-float target log-gamma-a log-small-x))
    ;; Far in the lower tail (small shapes) the small-x form is exact to
    ;; rounding, and may underflow to 0, which is then the quantile.
    (when (and (not upper) (< log-small-x -30d0))
      (return-from gamma-from-normal (exp log-small-x)))
    (gamma-quantile-newton upper target shape log-gamma-a log-small-x z)))

(defun gamma-quantile-newton (upper target shape log-gamma-a log-small-x z)
  "GAMMA-FROM-NORMAL's Newton iteration: P(SHAPE, x) = TARGET, or Q when UPPER."
  (declare (double-float target shape log-gamma-a log-small-x z))
  ;; Start: the small-x branch x = (u Gamma(a + 1))^(1/a) in the lower tail,
  ;; Wilson-Hilferty otherwise.
  (let* ((x (let ((wh (* shape (expt (max 1d-3 (+ (- 1d0 (/ (* 9d0 shape)))
                                                    (/ z (* 3d0 (sqrt shape)))))
                                      3))))
              (if (and (not upper) (< shape 1d0))
                  (min wh (exp log-small-x))
                  wh)))
         (lo 0d0) (hi most-positive-double-float))
    (declare (double-float x lo hi))
    (iter (repeat 200)
          (multiple-value-bind (p q) (regularized-gamma shape x)
            (declare (double-float p q))
            (let ((f (if upper (- target q) (- p target))))  ; increasing in x
              (if (plusp f) (setf hi x) (setf lo x))
              ;; d/dlog x of P is x^a e^-x / Gamma(a).
              (let* ((density (exp (- (* shape (log x)) x log-gamma-a)))
                     (step (if (plusp density) (/ f density) 0d0))
                     (next (* x (exp (- (max -2d0 (min 2d0 step)))))))
                (when (or (<= next lo) (>= next hi))
                  (setf next (if (< hi most-positive-double-float)
                                 (sqrt (* (max lo 1d-300) hi))
                                 (* 2d0 x))))
                (when (< (abs (- next x)) (* 1d-14 x))
                  (return-from gamma-quantile-newton next))
                (setf x next)))))
    x))

(defun inverse-gaussian-from-normals (z1 z2 mean shape)
  "An inverse Gaussian variate with MEAN and SHAPE from two normals, by
Michael, Schucany and Haas (1976): a root of the chi-square(1) Z1^2, chosen
with probability MEAN / (MEAN + root) using the uniform Phi(Z2).

  (inverse-gaussian-from-normals 0.3d0 -1d0 1d0 2d0)"
  (declare (double-float z1 z2 mean shape))
  (let* ((y (* z1 z1))
         (my (* mean y))
         ;; The smaller root, written without cancellation:
         ;; mean + (mean y - sqrt(4 mean shape y + (mean y)^2)) mean / (2 shape).
         (x (/ mean (+ 1d0 (/ (+ my (sqrt (+ (* 4d0 shape my) (* my my)))) (* 2d0 shape))))))
    (declare (double-float y my x))
    (if (<= (norm-cdf z2) (/ mean (+ mean x)))
        x
        (/ (* mean mean) x))))
