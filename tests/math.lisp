;;;; math.lisp --- special functions, root finders and extrapolation

(in-package #:fincl/tests)

(defun counting (f)
  "Return (values g counter): G calls F and counts its calls in (car COUNTER)."
  (let ((counter (list 0)))
    (values (lambda (x) (incf (car counter)) (funcall f x))
            counter)))

(defparameter *root-problems*
  ;; (name f lo hi root)
  (list (list "x^3 - 2x - 5" (lambda (x) (- (* x x x) (* 2 x) 5)) 2d0 3d0
              2.0945514815423265d0)
        (list "cos x - x" (lambda (x) (- (cos x) x)) 0d0 1d0 0.7390851332151607d0)
        (list "e^x - 10" (lambda (x) (- (exp x) 10)) 0d0 5d0 (log 10d0))
        (list "(x - 1)^3, a triple root" (lambda (x) (expt (- x 1) 3)) 0d0 3d0 1d0)))

(defun reference-expm1 (x)
  "e^X - 1 for rational X, as a rational accurate to 1e-50 relative."
  (loop with sum = 0 and term = 1
        for n from 1
        do (setf term (/ (* term x) n))
           (incf sum term)
        until (and (> n (abs x)) (< (abs term) (* (expt 10 -55) (abs sum))))
        finally (return sum)))

(defun reference-log1p (x)
  "log(1 + X) for rational X > -1, as 2 atanh(X / (2 + X)), a rational
accurate to 1e-50 relative."
  (let ((z (/ x (+ 2 x))))
    (loop with sum = 0
          for k from 0
          for term = (/ (expt z (1+ (* 2 k))) (1+ (* 2 k)))
          do (incf sum term)
          until (< (abs term) (* (expt 10 -55) (abs sum)))
          finally (return (* 2 sum)))))

(defun relative-error (computed reference)
  "|COMPUTED - REFERENCE| / |REFERENCE|, in exact arithmetic."
  (float (abs (/ (- (rational computed) reference) reference)) 1d0))

(defparameter *expm1-log1p-points*
  (let ((magnitudes '(1d-300 1d-20 1d-10 1d-7 1d-5 1d-3 0.1d0 0.3d0 0.4999d0 0.5d0 0.7d0 0.9d0)))
    `(,@magnitudes ,@(mapcar #'- magnitudes) 1d0 5d0 20d0 -5d0 -20d0)))

(defun quantile-checks ()
  "norm-quantile on 10 000 log-spaced u in [1e-300, 1/2] and their
complements. Rounding x = q(u) to a double moves norm-cdf(x) by up to
1/2 ulp(x) |x| phi(x) / Phi(x), about x^2 1e-16 in the tail, so the error is
measured against max(1, x^2)."
  (let ((us (loop for i below 10000
                  collect (min 0.5d0 (expt 10d0 (- (* 300 (/ i 9999d0))))))))
    (flet ((worst (pairs)
             ;; PAIRS: (x . tail-probability) with x <= 0.
             (loop for (x . p) in pairs
                   maximize (/ (abs (- (/ (norm-cdf x) p) 1d0)) (max 1d0 (* x x))))))
      (let ((lower (worst (loop for u in us collect (cons (norm-quantile u) u))))
            (upper (worst (loop for u in us
                                for v = (- 1d0 u)
                                ;; The smaller tail of v is 1 - v, exact for v >= 1/2;
                                ;; for u < 1.1e-16, v rounds to 1.
                                when (< v 1d0)
                                  collect (cons (- (norm-quantile v)) (- 1d0 v))))))
        (check-that (format nil "|Phi(q(u))/u - 1| / max(1, q^2) < 1e-15 for u in [1e-300, 1/2] ~
                                 (worst ~,2E)" lower)
                    (< lower 1d-15))
        (check-that (format nil "the same on the complements 1 - u (worst ~,2E)" upper)
                    (< upper 1d-15))))
    (check-that "q(v) = -q(1 - v) exactly for v in [1/2, 1)"
                (loop for u in us
                      for v = (- 1d0 u)
                      always (or (= v 1d0)
                                 (= (norm-quantile v) (- (norm-quantile (- 1d0 v)))))))
    (check "q(0.975)" 1.959963984540054d0 (norm-quantile 0.975d0) 1d-15)
    (check-that "q(least positive normalized double) is finite"
                (< -38d0 (norm-quantile least-positive-normalized-double-float) -37d0))))

(defun math-checks ()
  (format t "~&Math: special functions~%")
  (let ((worst (loop for x in *expm1-log1p-points*
                     maximize (relative-error (expm1 x) (reference-expm1 (rational x))))))
    (check-that (format nil "expm1 within 2 ulps of 50-digit references (worst ~,2E)" worst)
                (< worst 4.4d-16)))
  (let ((worst (loop for x in *expm1-log1p-points*
                     when (> x -1d0)
                       maximize (relative-error (log1p x) (reference-log1p (rational x))))))
    (check-that (format nil "log1p within 2 ulps of 50-digit references (worst ~,2E)" worst)
                (< worst 4.4d-16)))
  (check "norm-cdf 0" 0.5d0 (norm-cdf 0d0) 1d-16)
  (check "norm-cdf 1.96" 0.9750021048517795d0 (norm-cdf 1.96d0) 1d-15)
  (check "norm-pdf 0 = 1/sqrt(2 pi)" (/ (sqrt (* 2 pi))) (norm-pdf 0d0) 1d-16)
  (quantile-checks)
  ;; Known-answer vectors from Random123 (kat_vectors, philox4x32 10 rounds).
  (check-equal "Philox4x32-10 known answers"
               '((#x6627e8d5 #xe169c58d #xbc57ac4c #x9b00dbd8)
                 (#x408f276d #x41c83b0e #xa20bc7c6 #x6d5451fd)
                 (#xd16cfe09 #x94fdcceb #x5001e420 #x24126ea1))
               (list (multiple-value-list (fincl::philox4x32-10 0 0 0 0 0 0))
                     (multiple-value-list (fincl::philox4x32-10 #xffffffff #xffffffff #xffffffff
                                                                #xffffffff #xffffffff #xffffffff))
                     (multiple-value-list (fincl::philox4x32-10 #x243f6a88 #x85a308d3 #x13198a2e
                                                                #x03707344 #xa4093822 #x299f31d0)))
               :test #'equal)
  (check-that "counter uniforms lie in the open interval (0, 1)"
              (loop for path below 1000
                    always (multiple-value-bind (u v) (fincl::counter-uniforms 42 path 0)
                             (and (< 0d0 u 1d0) (< 0d0 v 1d0)))))

  (format t "~&Math: root finders~%")
  (loop for (name f lo hi root) in *root-problems*
        ;; A triple root is flat: bisection, which stops once |f| < 1e-12,
        ;; is then only accurate to 1e-4 in x. Newton is checked separately
        ;; below.
        for triple = (search "triple" name)
        do (multiple-value-bind (g calls) (counting f)
             (let ((x (solve-bisection g lo hi)))
               (check (format nil "bisection: ~A (~D calls)" name (car calls))
                      root x (if triple 1d-4 1d-11))))
           (multiple-value-bind (g calls) (counting f)
             (let ((x (solve-brent g lo hi)))
               (check (format nil "Brent: ~A (~D calls)" name (car calls))
                      root x (if triple 1d-6 1d-11))))
           (unless triple
             (multiple-value-bind (g calls) (counting f)
               (let ((x (solve-newton g (* 0.5d0 (+ lo hi)))))
                 (check (format nil "Newton: ~A (~D calls)" name (car calls))
                        root x 1d-11)))))
  ;; At a multiple root Newton is only linear, and near it the central
  ;; difference's O(h^2) error swamps f' = 3 (x - 1)^2, so steps stall
  ;; before x is accurate. It must fail loudly rather than return early.
  (check-error "Newton at a triple root" (solve-newton (lambda (x) (expt (- x 1) 3)) 1.5d0)
               convergence-failure)
  (let ((f (second (first *root-problems*))))
    (check-that "Brent needs fewer evaluations than bisection"
                (let ((brent (nth-value 1 (counting f)))
                      (bisection (nth-value 1 (counting f))))
                  (multiple-value-bind (g c) (counting f)
                    (setf brent c) (solve-brent g 2d0 3d0))
                  (multiple-value-bind (g c) (counting f)
                    (setf bisection c) (solve-bisection g 2d0 3d0))
                  (< (* 3 (car brent)) (car bisection)))))

  (format t "~&Math: bracketing, safeguards and restarts~%")
  (check "expand-hi grows the bracket" 100d0
         (solve-brent (lambda (x) (- x 100)) 0d0 1d0 :expand-hi t) 1d-10)
  (check-error "no sign change" (solve-brent (lambda (x) (+ 1 (* x x))) -1d0 1d0)
               convergence-failure)
  (check "use-value restart" 42d0
         (handler-bind ((convergence-failure
                          (lambda (c) (declare (ignore c)) (use-value 42d0))))
           (solve-brent (lambda (x) (+ 1 (* x x))) -1d0 1d0))
         0)
  (check "retry-with-bracket restart" 3d0
         (handler-bind ((convergence-failure
                          (lambda (c) (declare (ignore c))
                            (invoke-restart 'retry-with-bracket 2d0 4d0))))
           (solve-brent (lambda (x) (- x 3)) 0d0 1d0))
         1d-12)
  ;; Newton on atan diverges from |x0| > 1.39: each step overshoots further.
  (check-error "Newton diverges on atan from 2" (solve-newton #'atan 2d0)
               convergence-failure)
  (check "a bracket keeps Newton on atan convergent" 0d0
         (solve-newton #'atan 2d0 :lo -1d0 :hi 3d0) 1d-12)
  (check-error "Newton with a bracket that has no sign change"
               (solve-newton #'atan 2d0 :lo 1d0 :hi 3d0) convergence-failure)
  (check-error "Newton at a stationary point" (solve-newton (lambda (x) (+ 1 (* x x))) 0d0)
               convergence-failure)

  (format t "~&Math: Richardson extrapolation~%")
  (check-equal "weights, errors in h, h^2, h^3 (COS American)"
               #(-1/21 2/3 -8/3 64/21) (richardson-weights 4))
  (check-equal "weights, error in h (BBSR)" #(-1 2) (richardson-weights 2 :exponents '(1)))
  (check-equal "weights, error in h^2, ratio 3" #(-1/8 9/8)
               (richardson-weights 2 :ratio 3 :exponents '(2)))
  (check-that "weights sum to 1"
              (every (lambda (w) (= 1 (reduce #'+ w)))
                     (list (richardson-weights 5)
                           (richardson-weights 3 :ratio 3/2 :exponents '(2 4))
                           (richardson-weights 4 :ratio 4 :exponents '(1 3 5)))))
  (check-that "weights for non-integer exponents are floats summing to 1"
              (let ((w (richardson-weights 3 :exponents '(0.5d0 1.5d0))))
                (and (every #'floatp w) (< (abs (- 1 (reduce #'+ w))) 1d-14))))
  ;; A cubic in h is removed exactly by three columns of the tableau.
  (flet ((cubic (h) (+ 3 (* 2 h) (* -5 h h) (* 7 h h h))))
    (check-equal "exact on a cubic error, in rationals" 3
                 (richardson-extrapolate (mapcar #'cubic '(1 1/2 1/4 1/8)))
                 :test #'=)
    (check-equal "a single value is returned as is, with no error estimate" '(5 nil)
                 (multiple-value-list (richardson-extrapolate '(5)))))
  (check "non-integer ratio and exponents: 1 + h^0.5 + h^1.5" 1d0
         (richardson-extrapolate (mapcar (lambda (h) (+ 1 (sqrt h) (expt h 1.5d0)))
                                         (list 0.1d0 (/ 0.1d0 3) (/ 0.1d0 9)))
                                 :ratio 3 :exponents '(0.5d0 1.5d0))
         1d-13)
  ;; Central differences have errors in h^2, h^4, h^6; extrapolating them is
  ;; Ridders' (1982) differentiation scheme, not his root finder.
  (flet ((central (h) (/ (- (exp h) (exp (- h))) (* 2 h))))
    (multiple-value-bind (estimate error)
        (richardson-extrapolate (mapcar #'central '(0.4d0 0.2d0 0.1d0 0.05d0))
                                :exponents '(2 4 6))
      (check "central-difference derivative of exp at 0" 1d0 estimate 1d-12)
      (check-that "error estimate bounds the actual error"
                  (<= (abs (- estimate 1)) error 1d-6))))
  (check-error "ratio must exceed 1" (richardson-weights 2 :ratio 1) type-error)
  (check-error "one exponent per extra value" (richardson-weights 3 :exponents '(1)))
  (check-error "exponents must increase" (richardson-weights 3 :exponents '(2 1)))
  (differentiation-checks)
  (linear-algebra-checks))

(defun polynomial (coefficients)
  "The polynomial with COEFFICIENTS (constant first), and its derivatives:
(values p p' p'')."
  (flet ((horner (cs) (lambda (x) (reduce (lambda (c acc) (+ c (* x acc))) cs
                                          :from-end t :initial-value 0))))
    (let* ((d1 (loop for c in (rest coefficients) for k from 1 collect (* k c)))
           (d2 (loop for c in (rest d1) for k from 1 collect (* k c))))
      (values (horner coefficients) (horner d1) (horner d2)))))

(defun stencil-exact-p (offsets order degree)
  "Does the stencil on OFFSETS give the ORDER-th derivative of a random-ish
polynomial of DEGREE exactly, in rationals?"
  (multiple-value-bind (p d1 d2) (polynomial (loop for k to degree collect (- (* 3 k) 7/2)))
    (let* ((x 3/2) (h 1/3)
           (estimate (apply-stencil (finite-difference-weights offsets order)
                                    (mapcar (lambda (k) (funcall p (+ x (* k h)))) offsets)
                                    h order)))
      (= estimate (funcall (if (= order 1) d1 d2) x)))))

(defun differentiation-checks ()
  (format t "~&Math: finite differences~%")
  (check-equal "central first derivative, 2nd order" #(-1/2 0 1/2)
               (finite-difference-weights '(-1 0 1) 1))
  (check-equal "central first derivative, 4th order" #(1/12 -2/3 0 2/3 -1/12)
               (finite-difference-weights '(-2 -1 0 1 2) 1))
  (check-equal "central second derivative" #(1 -2 1) (finite-difference-weights '(-1 0 1) 2))
  (check-equal "forward first derivative, 2nd order" #(-3/2 2 -1/2)
               (finite-difference-weights '(0 1 2) 1))
  ;; Non-uniform three points (-a, 0, b), a = 1, b = 3/2, against the
  ;; closed forms.
  (let ((a 1) (b 3/2))
    (check-equal "non-uniform first derivative"
                 (vector (- (/ b (* a (+ a b)))) (/ (- b a) (* a b)) (/ a (* b (+ a b))))
                 (finite-difference-weights (list (- a) 0 b) 1))
    (check-equal "non-uniform second derivative"
                 (vector (/ 2 (* a (+ a b))) (/ -2 (* a b)) (/ 2 (* b (+ a b))))
                 (finite-difference-weights (list (- a) 0 b) 2)))
  (check-that "stencils are exact on polynomials up to their order of accuracy"
              (and (stencil-exact-p '(-1 1) 1 2)
                   (stencil-exact-p '(-2 -1 0 1 2) 1 4)
                   (stencil-exact-p '(-1 0 1) 2 3)
                   (stencil-exact-p '(0 1 2) 1 2)
                   (stencil-exact-p '(-1 0 3/2) 1 2)
                   (stencil-exact-p '(-1 0 3/2) 2 2)
                   (not (stencil-exact-p '(-1 1) 1 3))))
  (let ((state (sb-ext:seed-random-state 5)))
    (check-that "the central quotient is exactly (f(x+h) - f(x-h)) / 2h, as SOLVE-NEWTON had"
                (loop for f in (list #'exp #'sin (lambda (x) (- (* x x x) (* 2 x) 5))
                                     (lambda (x) (- (cos x) x)))
                      always (loop repeat 200
                                   for x = (- (random 6d0 state) 3d0)
                                   for h = (* (random 1d0 state) 1d-4)
                                   always (= (difference-quotient f x h)
                                             (/ (- (funcall f (+ x h)) (funcall f (- x h)))
                                                (* 2d0 h)))))))
  (loop for (name f df points) in (list (list "exp" #'exp #'exp '(-2d0 -0.5d0 0d0 1d0 3d0))
                                        (list "sin" #'sin #'cos '(-2d0 -0.5d0 0d0 1d0 3d0))
                                        (list "log" #'log #'/ '(0.5d0 1d0 2d0 5d0 10d0)))
        do (check-that (format nil "Ridders' derivative of ~A at five points to 1e-12, error estimate >= actual" name)
                       (loop for x in points
                             always (multiple-value-bind (estimate error) (derivative f x)
                                      (let ((actual (abs (- estimate (funcall df x)))))
                                        (and (< actual (* 1d-12 (max 1d0 (abs (funcall df x)))))
                                             (>= error actual)))))))
  (check "second derivative of sin at 1 by Ridders" (- (sin 1d0))
         (derivative #'sin 1d0 :order 2) 1d-10)
  (check-that "default step: central first derivative at 1 is eps^(1/3)"
              (= (default-step 1d0 :central 1) (expt double-float-epsilon (/ 1d0 3))))
  (check-error "repeated offsets" (finite-difference-weights '(0 0 1) 1)))

(defun random-least-squares (m n state)
  "A random well-conditioned M x N least-squares problem: columns of
independent uniforms plus a dominant diagonal band. Returns (values rows y)."
  (let ((rows (loop for i below m
                    collect (let ((row (make-array n :element-type 'double-float)))
                              (dotimes (j n row)
                                (setf (aref row j) (+ (random 1d0 state)
                                                      (if (= j (mod i n)) 3d0 0d0)))))))
        (y (loop repeat m collect (- (random 2d0 state) 1d0))))
    (values rows y)))

(defun qr-fit (rows y n)
  (let ((acc (fincl::make-qr-accumulator n)))
    (loop for row in rows for yi in y
          do (fincl::qr-add-row! acc (copy-seq row) yi))
    (fincl::qr-solve acc)))

(defun normal-equations-fit (rows y n)
  "The oracle: (X'X) b = X'y solved by magicl/LAPACK."
  (let ((xtx (make-array (* n n) :element-type 'double-float :initial-element 0d0))
        (xty (make-array n :element-type 'double-float :initial-element 0d0)))
    (loop for row in rows for yi in y
          do (dotimes (r n)
               (incf (aref xty r) (* (aref row r) yi))
               (dotimes (c n)
                 (incf (aref xtx (+ (* r n) c)) (* (aref row r) (aref row c))))))
    (magicl::storage (magicl:linear-solve (magicl:from-array xtx (list n n) :layout :row-major)
                                          (magicl:from-array xty (list n))))))

(defun max-relative-difference (a b)
  (loop for x across a for y across b
        maximize (/ (abs (- x y)) (max 1d0 (abs y)))))

(defun linear-algebra-checks ()
  (format t "~&Math: least squares by QR~%")
  (let ((state (sb-ext:seed-random-state 11)))
    (check-that "QR = normal equations (magicl) on 20 random well-conditioned problems"
                (loop repeat 20
                      always (multiple-value-bind (rows y) (random-least-squares 200 5 state)
                               (< (max-relative-difference (qr-fit rows y 5)
                                                           (normal-equations-fit rows y 5))
                                  1d-12))))
    (multiple-value-bind (rows y) (random-least-squares 300 4 state)
      (check-that "four merged chunk accumulators = one accumulator over all rows"
                  (let ((total (fincl::make-qr-accumulator 4)))
                    (loop for chunk below 4
                          do (let ((acc (fincl::make-qr-accumulator 4)))
                               (loop for row in (subseq rows (* chunk 75) (* (1+ chunk) 75))
                                     for yi in (subseq y (* chunk 75) (* (1+ chunk) 75))
                                     do (fincl::qr-add-row! acc (copy-seq row) yi))
                               (fincl::qr-merge! total acc)))
                    (and (= 300 (fincl::qr-accumulator-rows total))
                         (< (max-relative-difference (fincl::qr-solve total) (qr-fit rows y 4))
                            1d-13))))))
  (let* ((beta #(1.5d0 -2d0 0.25d0))
         (rows (loop for i below 50
                     collect (let ((x (/ i 10d0)))
                               (make-array 3 :element-type 'double-float
                                             :initial-contents (list 1d0 x (* x x))))))
         (y (mapcar (lambda (row) (loop for j below 3 sum (* (aref beta j) (aref row j)))) rows)))
    (check-that "an exact fit recovers its coefficients"
                (< (max-relative-difference (qr-fit rows y 3) beta) 1d-12)))
  (check-error "a repeated column is rank deficient"
               (qr-fit (loop for i below 10
                             collect (make-array 2 :element-type 'double-float
                                                   :initial-contents (list (float i 1d0)
                                                                           (float i 1d0))))
                       (loop for i below 10 collect (float i 1d0))
                       2)
               fincl::rank-deficient))

(defun run-math-tests ()
  "Run only the math checks, numerics.lisp's included. Returns T when all pass."
  (let ((*failures* 0))
    (math-checks)
    (interpolation-checks)
    (tridiagonal-checks)
    (optimizer-checks)
    (format t "~&~[All math checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
