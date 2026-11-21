;;;; math/differentiation.lisp --- finite-difference schemes as data
;;;;
;;;; A finite-difference scheme is a set of weights on a set of offsets. The code computes
;;;; the weights once (FINITE-DIFFERENCE-WEIGHTS, exact in rationals). It applies them
;;;; separately (APPLY-STENCIL), because callers do not always evaluate a function at x + k
;;;; h. Bump-and-reprice Greeks price at bumped markets. Tree and PDE Greeks read values
;;;; that are already on a grid. DIFFERENCE-QUOTIENT is the common case of a function of
;;;; one real. DERIVATIVE is the extrapolated derivative of Ridders, for smooth,
;;;; deterministic functions.
;;;;
;;;; The differentiation scheme of Ridders (1982) is unrelated to his root finder of 1979.
;;;; SOLVE-BRENT finds roots.

(in-package #:fincl)

(declaim (ftype (function (sequence t) (values simple-vector &optional)) finite-difference-weights))
(defun finite-difference-weights (offsets order)
  "Return weights w_i such that sum w_i f(x + OFFSETS_i h) / h^ORDER approximates the
ORDER-th derivative of f at x. The algorithm is that of Fornberg (Math. Comp. 51, 1988).
OFFSETS are in units of h. They can be in any order and can be unevenly spaced. The
weights come back in the same order. Rational offsets give exact rational weights.

  (finite-difference-weights '(-1 1) 1)     => #(-1/2 1/2)
  (finite-difference-weights '(-1 0 1) 2)   => #(1 -2 1)
  (finite-difference-weights '(-1 0 3/2) 1) ; a non-uniform grid"
  (let* ((nodes (coerce offsets 'simple-vector))
         (n (length nodes))
         ;; c[i][k]: weight of node i for the k-th derivative, over the
         ;; nodes seen so far.
         (c (make-array (list n (1+ order)) :initial-element 0))
         (c1 1)
         (c4 (svref nodes 0)))
    (check-input order (integer 0) "a non-negative derivative order")
    (unless (< order n)
      (signal-invalid-argument 'offsets offsets 'sequence
                               (format nil "more than ~D offsets for a derivative of order ~D"
                                       order order)))
    (unless (= n (length (remove-duplicates nodes :test #'=)))
      (signal-invalid-argument 'offsets offsets 'sequence "distinct offsets"))
    (setf (aref c 0 0) 1)
    (iter (for i from 1 below n)
          (let ((mn (min i order)) (c2 1) (c5 c4))
            (setf c4 (svref nodes i))
            (iter (for j from 0 below i)
                  (let ((c3 (- (svref nodes i) (svref nodes j))))
                    (setf c2 (* c2 c3))
                    (when (= j (1- i))
                      (iter (for k from mn downto 1)
                            (setf (aref c i k)
                                  (/ (* c1 (- (* k (aref c (1- i) (1- k)))
                                              (* c5 (aref c (1- i) k))))
                                     c2)))
                      (setf (aref c i 0) (/ (- (* c1 c5 (aref c (1- i) 0))) c2)))
                    (iter (for k from mn downto 1)
                          (setf (aref c j k)
                                (/ (- (* c4 (aref c j k)) (* k (aref c j (1- k))))
                                   c3)))
                    (setf (aref c j 0) (/ (* c4 (aref c j 0)) c3))))
            (setf c1 c2)))
    (let ((weights (make-array n)))
      (dotimes (i n weights)
        (setf (svref weights i) (aref c i order))))))

(declaim (ftype (function (sequence sequence real (integer 0))
                    (values number &optional))
          apply-stencil))
(defun apply-stencil (weights values h order)
  "Return the derivative estimate sum WEIGHTS_i VALUES_i / H^ORDER. The function applies
rational weights exactly, with one rounding at the division (see EXACT-WEIGHTED-SUM).
Thus (apply-stencil #(-1/2 1/2) (list fm fp) h 1) is exactly (/ (- fp fm) (* 2 h)).

  (apply-stencil (finite-difference-weights '(-1 0 1) 2) (list fm f0 fp) h 2)"
  (multiple-value-bind (sum denominator) (exact-weighted-sum weights values)
    (/ sum (* denominator (expt h order)))))

(defun scheme-offsets (scheme order)
  "The offsets of the smallest stencil of each scheme. Central stencils are
symmetric, with half-width ceiling(ORDER/2) and no 0 for odd ORDER, and are
second-order accurate; forward and backward ones are first-order accurate.

  (scheme-offsets :central 1) => (-1 1)
  (scheme-offsets :central 2) => (-1 0 1)"
  (ecase scheme
    (:central (let ((half (ceiling order 2)))
                (iter (for k from (- half) to half)
                      (unless (and (zerop k) (oddp order)) (collect k)))))
    (:forward (iter (for k from 0 to order) (collect k)))
    (:backward (iter (for k from (- order) to 0) (collect k)))))

(declaim (ftype (function ((or function symbol) real real &key (:scheme symbol) (:order (integer 1)))
                    (values real &optional))
          difference-quotient))
(defun difference-quotient (f x h &key (scheme :central) (order 1))
  "Return the finite-difference estimate of the ORDER-th derivative of F at X with step H.
SCHEME is :CENTRAL, :FORWARD or :BACKWARD (see SCHEME-OFFSETS).

  (difference-quotient #'exp 0d0 1d-5) => 1.0000000000166667d0"
  (let ((offsets (scheme-offsets scheme order)))
    (apply-stencil (finite-difference-weights offsets order)
                   (mapcar (lambda (k) (funcall f (+ x (* k h)))) offsets)
                   h order)))

(declaim (ftype (function (real symbol (integer 1)) (values double-float &optional)) default-step))
(defun default-step (x scheme order)
  "Return a step that balances truncation error against rounding error. It is
eps^(1/(ORDER+1)) for one-sided schemes and eps^(1/(ORDER+2)) for central schemes,
times max(1, |X|). For a central first derivative at x = 1 it is about 6e-6.

  (default-step 1d0 :central 1) => 6.055454...d-6"
  (* (expt double-float-epsilon
           (/ 1d0 (+ order (if (eq scheme :central) 2 1))))
     (max 1d0 (abs x))))

(declaim (ftype (function ((or function symbol) real &key (:order (integer 1)) (:h0 (or null real)) (:ratio real) (:levels (integer 1)))
                    (values real (or real null) &optional))
          derivative))
(defun derivative (f x &key (order 1) h0 (ratio 2) (levels 6))
  "Return the ORDER-th derivative of F at X by the extrapolation of Ridders (1982).
Central difference quotients at the steps H0, H0/RATIO, ..., H0/RATIO^(LEVELS-1) have
errors in h^2, h^4, ... RICHARDSON-EXTRAPOLATE removes them. Return (values estimate
error-estimate). They come from the prefix of the tableau with the smallest error
estimate. H0 defaults to 0.1 max(1, |X|).

The error estimate of each prefix is the larger of two values: the tableau estimate and
a floor for rounding, 2 eps (|f(x)| + |x f'(x)|) sum_i |w_i| c / h_i^ORDER. The w_i
are the Richardson weights. c is the sum of the absolute weights of the stencil. |f|
covers the rounding in f. |x f'| covers the rounding in the argument x + k h, which is
off by up to eps |x| (f' is a central quotient at H0). Without the floor, the
extrapolations at tiny steps agree to every bit once rounding dominates. They then
report a false zero error.

Use it only for smooth, deterministic F: as a test oracle, and as a derivative for
deterministic engines. It amplifies noise, such as Monte Carlo error. It does not
remove it.

  (derivative #'sin 1d0) => 0.5403023058681398d0, ~1d-14"
  (let* ((h0 (or h0 (* 0.1d0 (max 1d0 (abs x)))))
         (steps (iter (for i below levels) (collect (/ h0 (expt ratio i)))))
         (quotients (mapcar (lambda (h) (difference-quotient f x h :order order)) steps))
         (stencil-size (reduce #'+ (finite-difference-weights (scheme-offsets :central order)
                                                              order)
                               :key #'abs))
         (f-scale (+ (abs (funcall f x))
                     (* (abs x) (abs (difference-quotient f x h0))))))
    (iter (for k from 2 to levels)
          (for exponents = (iter (for p from 1 below k) (collect (* 2 p))))
          (for (values estimate tableau-error) = (richardson-extrapolate (subseq quotients 0 k)
                                                                         :ratio ratio
                                                                         :exponents exponents))
          (for rounding = (* 2 double-float-epsilon f-scale stencil-size
                             (iter (for w in-vector (richardson-weights k :ratio ratio
                                                                          :exponents exponents))
                                   (for h in steps)
                                   (sum (/ (abs w) (expt h order))))))
          (finding estimate minimizing (max tableau-error rounding) into (best best-error))
          (finally (return (values best best-error))))))

(defun stencil-inside (scheme offsets x h lo hi)
  "(values offsets h) for one coordinate at X: OFFSETS of SCHEME if every
X + k H lies strictly inside (LO, HI) (NIL ends unbounded), else the
one-sided stencil pointing into the interval, with H shortened to half the
room if the interval is narrower."
  (flet ((inside-p (offs step)
           ;; Offset 0 is X itself, where F is evaluated anyway.
           (every (lambda (k) (or (zerop k)
                                  (let ((y (+ x (* k step))))
                                    (and (or (null lo) (> y lo)) (or (null hi) (< y hi))))))
                  offs)))
    (if (inside-p offsets h)
        (values offsets h)
        (let* ((room-up (if hi (- hi x) most-positive-double-float))
               (room-down (if lo (- x lo) most-positive-double-float))
               (offs (scheme-offsets (if (>= room-up room-down) :forward :backward) 1))
               (reach (reduce #'max (mapcar #'abs offs)))
               (step (if (inside-p offs h) h (/ (max room-up room-down) (* 2 reach)))))
          (if (and (plusp step) (inside-p offs step))
              (values offs step)
              (signal-invalid-argument 'bounds `(,lo . ,hi) 'cons
                                       (format nil "an interval around ~A wide enough for a ~(~A~) difference"
                                               x scheme)))))))

(declaim (ftype (function ((or function symbol) double-vector double-vector &key (:scheme symbol) (:steps (or null sequence)) (:f0 (or null double-vector)) (:bounds list))
                    (values double-vector &optional))
          jacobian!))
(defun jacobian! (f x out &key (scheme :forward) steps f0 bounds)
  "Compute the Jacobian of the vector function F by finite differences. F maps a
DOUBLE-VECTOR to a DOUBLE-VECTOR of m residuals. X has length n. The function writes the
result in column-major order into OUT (length m n): OUT[i + m j] = d r_i / d x_j.
SCHEME is :FORWARD (n extra evaluations) or :CENTRAL (2n). STEPS is a sequence of n
steps. It defaults to DEFAULT-STEP for each x_j. F0, the value of F at X, saves one
evaluation. BOUNDS is a list of n (lo . hi) intervals (NIL ends are unbounded). It keeps
every point at which F is evaluated inside the intervals. If the stencil of a coordinate
would leave its interval, the function differences that coordinate one-sided, into the
interval. The stencils come from FINITE-DIFFERENCE-WEIGHTS. The function perturbs X and
restores it. It returns OUT.

  (jacobian! residuals x (make-array (* m n) :element-type 'double-float))"
  (declare (type double-vector x out))
  (let* ((n (length x))
         (offsets (scheme-offsets scheme 1))
         (r0 nil)
         (m nil))
    (flet ((base ()
             (or r0 (setf r0 (or f0 (funcall f x))))))
      (dotimes (j n out)
        (let ((xj (aref x j)))
          (multiple-value-bind (offs step)
              (let ((h (if steps (float (elt steps j) 1d0) (default-step xj scheme 1))))
                (if bounds
                    (destructuring-bind (lo . hi) (elt bounds j)
                      (stencil-inside scheme offsets xj h lo hi))
                    (values offsets h)))
            (let* ((weights (finite-difference-weights offs 1))
                   ;; The step actually taken, exactly representable: (x + h) - x.
                   (h (- (+ xj step) xj))
                   (values (mapcar (lambda (k)
                                     (if (zerop k)
                                         (base)
                                         (progn (setf (aref x j) (+ xj (* k h)))
                                                (prog1 (copy-seq (funcall f x))
                                                  (setf (aref x j) xj)))))
                                   offs)))
              (unless m (setf m (length (first values))))
              (unless (= (length out) (* m n))
                (signal-invalid-argument '(length out) (length out) `(integer ,(* m n))
                                         (format nil "~D x ~D = ~D elements" m n (* m n))))
              (dotimes (i m)
                (setf (aref out (+ i (* m j)))
                      (float (apply-stencil weights (mapcar (lambda (r) (aref r i)) values) h 1) 1d0))))))))))
