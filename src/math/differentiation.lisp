;;;; math/differentiation.lisp --- finite-difference schemes as data
;;;;
;;;; A finite-difference scheme is a set of weights on a set of offsets. The
;;;; weights are computed once (FINITE-DIFFERENCE-WEIGHTS, exact in
;;;; rationals) and applied separately (APPLY-STENCIL), because callers do
;;;; not always evaluate a function at x + k h: bump-and-reprice Greeks
;;;; price at bumped markets, and tree and PDE Greeks read values already on
;;;; a grid. DIFFERENCE-QUOTIENT is the common case of a function of one
;;;; real, and DERIVATIVE is Ridders' extrapolated derivative for smooth,
;;;; deterministic functions.
;;;;
;;;; Ridders' 1982 differentiation scheme is unrelated to his 1979 root
;;;; finder; roots are SOLVE-BRENT's job.

(in-package #:fincl)

(defun finite-difference-weights (offsets order)
  "Weights w_i such that sum w_i f(x + OFFSETS_i h) / h^ORDER approximates
the ORDER-th derivative of f at x, by Fornberg's algorithm (Math. Comp. 51,
1988). OFFSETS are in units of h, in any order and not necessarily evenly
spaced; the weights come back in the same order. Rational offsets give exact
rational weights.

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
    (check-type order (integer 0))
    (assert (< order n) () "A derivative of order ~D needs more than ~D offsets." order n)
    (assert (= n (length (remove-duplicates nodes :test #'=))) ()
            "Finite-difference offsets must be distinct, not ~S." offsets)
    (setf (aref c 0 0) 1)
    (loop for i from 1 below n
          do (let ((mn (min i order)) (c2 1) (c5 c4))
               (setf c4 (svref nodes i))
               (loop for j from 0 below i
                     do (let ((c3 (- (svref nodes i) (svref nodes j))))
                          (setf c2 (* c2 c3))
                          (when (= j (1- i))
                            (loop for k from mn downto 1
                                  do (setf (aref c i k)
                                           (/ (* c1 (- (* k (aref c (1- i) (1- k)))
                                                       (* c5 (aref c (1- i) k))))
                                              c2)))
                            (setf (aref c i 0) (/ (- (* c1 c5 (aref c (1- i) 0))) c2)))
                          (loop for k from mn downto 1
                                do (setf (aref c j k)
                                         (/ (- (* c4 (aref c j k)) (* k (aref c j (1- k))))
                                            c3)))
                          (setf (aref c j 0) (/ (* c4 (aref c j 0)) c3))))
               (setf c1 c2)))
    (let ((weights (make-array n)))
      (dotimes (i n weights)
        (setf (svref weights i) (aref c i order))))))

(defun apply-stencil (weights values h order)
  "The derivative estimate sum WEIGHTS_i VALUES_i / H^ORDER. Rational weights
are applied exactly, with a single rounding at the division (see
EXACT-WEIGHTED-SUM), so (apply-stencil #(-1/2 1/2) (list fm fp) h 1) is
exactly (/ (- fp fm) (* 2 h)).

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
                (loop for k from (- half) to half
                      unless (and (zerop k) (oddp order)) collect k)))
    (:forward (loop for k from 0 to order collect k))
    (:backward (loop for k from (- order) to 0 collect k))))

(defun difference-quotient (f x h &key (scheme :central) (order 1))
  "Finite-difference estimate of the ORDER-th derivative of F at X with step
H. SCHEME is :CENTRAL, :FORWARD or :BACKWARD; see SCHEME-OFFSETS.

  (difference-quotient #'exp 0d0 1d-5) => 1.0000000000166667d0"
  (let ((offsets (scheme-offsets scheme order)))
    (apply-stencil (finite-difference-weights offsets order)
                   (mapcar (lambda (k) (funcall f (+ x (* k h)))) offsets)
                   h order)))

(defun default-step (x scheme order)
  "A step balancing truncation against rounding error: eps^(1/(ORDER+1))
for one-sided schemes and eps^(1/(ORDER+2)) for central ones, times
max(1, |X|). For a central first derivative at x = 1 it is about 6e-6.

  (default-step 1d0 :central 1) => 6.055454...d-6"
  (* (expt double-float-epsilon
           (/ 1d0 (+ order (if (eq scheme :central) 2 1))))
     (max 1d0 (abs x))))

(defun derivative (f x &key (order 1) h0 (ratio 2) (levels 6))
  "The ORDER-th derivative of F at X by Ridders' extrapolation (1982):
central difference quotients at steps H0, H0/RATIO, ..., H0/RATIO^(LEVELS-1)
have errors in h^2, h^4, ..., which RICHARDSON-EXTRAPOLATE removes. Returns
(values estimate error-estimate), from whichever prefix of the tableau has
the smallest error estimate. H0 defaults to 0.1 max(1, |X|).

Each prefix's error estimate is the larger of the tableau estimate and a
floor for rounding, 2 eps (|f(x)| + |x f'(x)|) sum_i |w_i| c / h_i^ORDER,
where w_i are the Richardson weights and c is the stencil's sum of absolute
weights. |f| covers rounding in f; |x f'| covers rounding in the argument
x + k h, which is off by up to eps |x| (f' is a central quotient at H0).
Without the floor, extrapolations at tiny steps agree to every bit once
rounding dominates, and report a spurious zero error.

For smooth, deterministic F only: a test oracle, and a derivative for
deterministic engines. Noise, such as Monte Carlo error, is amplified, not
removed.

  (derivative #'sin 1d0) => 0.5403023058681398d0, ~1d-14"
  (let* ((h0 (or h0 (* 0.1d0 (max 1d0 (abs x)))))
         (steps (loop for i below levels collect (/ h0 (expt ratio i))))
         (quotients (mapcar (lambda (h) (difference-quotient f x h :order order)) steps))
         (stencil-size (reduce #'+ (finite-difference-weights (scheme-offsets :central order)
                                                              order)
                               :key #'abs))
         (f-scale (+ (abs (funcall f x))
                     (* (abs x) (abs (difference-quotient f x h0)))))
         (best nil) (best-error nil))
    (loop for k from 2 to levels
          do (let ((exponents (loop for p from 1 below k collect (* 2 p))))
               (multiple-value-bind (estimate tableau-error)
                   (richardson-extrapolate (subseq quotients 0 k)
                                           :ratio ratio :exponents exponents)
                 (let* ((weights (richardson-weights k :ratio ratio :exponents exponents))
                        (rounding (* 2 double-float-epsilon f-scale stencil-size
                                     (loop for w across weights
                                           for h in steps
                                           sum (/ (abs w) (expt h order)))))
                        (error (max tableau-error rounding)))
                   (when (or (null best-error) (< error best-error))
                     (setf best estimate best-error error))))))
    (values best best-error)))
