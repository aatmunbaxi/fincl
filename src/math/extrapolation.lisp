;;;; math/extrapolation.lisp --- Richardson extrapolation
;;;;
;;;; A quantity computed with step h often has an error expansion
;;;;
;;;;   A(h) = A + a1 h^p1 + a2 h^p2 + ...
;;;;
;;;; with known exponents p1 < p2 < ... and unknown coefficients. Given A at
;;;; the geometric steps h, h/r, h/r^2, ..., Richardson's tableau removes one
;;;; term per column:
;;;;
;;;;   R_k(i) = (r^pk R_{k-1}(i+1) - R_{k-1}(i)) / (r^pk - 1)
;;;;
;;;; The result is a fixed linear combination of the inputs, so the tableau
;;;; is run once on the coefficients (exactly, in rationals, whenever r and
;;;; the exponents are rational) rather than on the values.
;;;;
;;;; Uses in fincl: the BBSR binomial tree (errors in 1/N), the COS American
;;;; (Bermudans with M, 2M, 4M, 8M dates; errors in 1/M, 1/M^2, 1/M^3), and
;;;; the cumulant estimates from a characteristic function (errors in u^2).

(in-package #:fincl)

(defun default-exponents (n)
  "The exponents 1, 2, ..., N-1 of a Taylor-type error expansion in h."
  (loop for p from 1 below n collect p))

(defun check-richardson-arguments (n ratio exponents)
  (check-type n (integer 1))
  (check-type ratio (real (1)))
  (assert (and (= (length exponents) (1- n))
               (every (lambda (p) (and (realp p) (plusp p))) exponents)
               (every #'< exponents (rest exponents)))
          () "Richardson extrapolation of ~D value~:P needs ~D increasing positive ~
exponent~:P, not ~S." n (1- n) exponents))

(defun richardson-weights (n &key (ratio 2) (exponents (default-exponents n)))
  "Weights w_0 ... w_{N-1} such that sum w_i A(h / RATIO^i) cancels the
terms h^p, p in EXPONENTS, of A's error expansion; index 0 is the coarsest
step. EXPONENTS holds N - 1 increasing positive reals and defaults to 1, 2,
..., N - 1. The weights sum to 1. They are exact rationals when RATIO is
rational and the exponents are integers, and floats otherwise.

  (richardson-weights 4) => #(-1/21 2/3 -8/3 64/21)
  (richardson-weights 2 :exponents '(2)) => #(-1/3 4/3)"
  (check-richardson-arguments n ratio exponents)
  ;; Column k of the tableau, as coefficient vectors over the inputs.
  (let ((column (loop for i below n
                      collect (let ((e (make-array n :initial-element 0)))
                                (setf (svref e i) 1)
                                e))))
    (dolist (p exponents (first column))
      (let ((factor (expt ratio p)))
        (setf column
              (loop for (coarse fine) on column
                    while fine
                    collect (map 'simple-vector
                                 (lambda (c f) (/ (- (* factor f) c) (- factor 1)))
                                 coarse fine)))))))

(defun exact-weighted-sum (weights values)
  "Sum of WEIGHTS times VALUES, last element first, returned as
(values sum denominator): divide SUM by DENOMINATOR (and by anything else,
such as a step h, in the same division) to get the weighted sum. Rational
weights are scaled to integers over their common denominator, so a
combination such as 2 v(N) - v(N/2) or (f(x+h) - f(x-h)) / 2h is computed
exactly as written, with one rounding at the division. Float weights give a
DENOMINATOR of 1.

  (exact-weighted-sum #(-1/2 1/2) (list fm fp)) => (- fp fm), 2"
  (let ((pairs (reverse (map 'list #'cons weights values))))
    (if (every (lambda (w) (rationalp (car w))) pairs)
        (let ((denominator (reduce #'lcm pairs :key (lambda (w) (denominator (car w))))))
          (values (reduce (lambda (sum w) (+ sum (* (* (car w) denominator) (cdr w))))
                          pairs :initial-value 0)
                  denominator))
        (values (reduce (lambda (sum w) (+ sum (* (car w) (cdr w)))) pairs :initial-value 0)
                1))))

(defun richardson-combine (weights values)
  (multiple-value-bind (sum denominator) (exact-weighted-sum weights values)
    (/ sum denominator)))

(defun richardson-extrapolate (values &key (ratio 2)
                                           (exponents (default-exponents (length values))))
  "Extrapolate VALUES to step zero. VALUES are one quantity computed at the
steps h, h/RATIO, h/RATIO^2, ..., coarsest first; EXPONENTS are the powers of
h in its error expansion, one per value after the first (default 1, 2, ...).
See RICHARDSON-WEIGHTS.

Returns (values estimate error-estimate). ERROR-ESTIMATE is the absolute
difference from the extrapolation one order lower through the finest values
(the usual tableau error estimate), or NIL for a single value.

  ;; A binomial tree with errors in 1/N: 2 v(N) - v(N/2)
  (richardson-extrapolate (list v-half v-full) :exponents '(1))
  ;; Bermudans with M, 2M, 4M, 8M dates: (64 v8 - 56 v4 + 14 v2 - v1) / 21
  (richardson-extrapolate (list v1 v2 v4 v8))"
  (let* ((n (length values))
         (estimate (richardson-combine
                    (richardson-weights n :ratio ratio :exponents exponents)
                    values)))
    (values estimate
            (when (> n 1)
              (abs (- estimate
                      (richardson-combine
                       (richardson-weights (1- n) :ratio ratio
                                                  :exponents (butlast exponents))
                       (rest values))))))))
