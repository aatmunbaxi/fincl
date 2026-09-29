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

(defun math-checks ()
  (format t "~&Math: special functions~%")
  (check "norm-cdf 0" 0.5d0 (norm-cdf 0d0) 1d-16)
  (check "norm-cdf 1.96" 0.9750021048517795d0 (norm-cdf 1.96d0) 1d-15)
  (check "norm-pdf 0 = 1/sqrt(2 pi)" (/ (sqrt (* 2 pi))) (norm-pdf 0d0) 1d-16)
  (check "norm-quantile inverts norm-cdf" 0.3d0 (norm-cdf (norm-quantile 0.3d0)) 1d-14)

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
  "Run only the math checks. Returns T when all pass."
  (let ((*failures* 0))
    (math-checks)
    (format t "~&~[All math checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
