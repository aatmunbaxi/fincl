;;;; numerics.lisp --- interpolation, tridiagonal solves, optimization

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Interpolation
;;; --------------------------------------------------------------------

(defun sample-points (lo hi n)
  (loop for i to n collect (+ lo (* (- hi lo) (/ i (float n 1d0))))))

(defun worst-error (f g points)
  (loop for x in points maximize (abs (- (funcall f x) (funcall g x)))))

(defun exact-natural-spline (xs ys)
  "The natural cubic spline through rational knots, in exact arithmetic:
second derivatives by Gaussian elimination, then the cubic on each segment."
  (let* ((n (length xs))
         (h (loop for i below (1- n) collect (- (nth (1+ i) xs) (nth i xs))))
         (slope (loop for i below (1- n) collect (/ (- (nth (1+ i) ys) (nth i ys)) (nth i h))))
         (m (make-array n :initial-element 0))
         (k (- n 2))
         (rows (loop for r below k
                     for i = (1+ r)
                     collect (let ((row (make-array (1+ k) :initial-element 0)))
                               (when (> r 0) (setf (aref row (1- r)) (nth (1- i) h)))
                               (setf (aref row r) (* 2 (+ (nth (1- i) h) (nth i h))))
                               (when (< r (1- k)) (setf (aref row (1+ r)) (nth i h)))
                               (setf (aref row k) (* 6 (- (nth i slope) (nth (1- i) slope))))
                               row))))
    ;; Gaussian elimination, exact.
    (loop for p below k
          do (loop for r from (1+ p) below k
                   for factor = (/ (aref (nth r rows) p) (aref (nth p rows) p))
                   do (loop for c from p to k
                            do (decf (aref (nth r rows) c) (* factor (aref (nth p rows) c))))))
    (loop for p from (1- k) downto 0
          do (setf (aref m (1+ p))
                   (/ (- (aref (nth p rows) k)
                         (loop for c from (1+ p) below k sum (* (aref (nth p rows) c) (aref m (1+ c)))))
                      (aref (nth p rows) p))))
    (lambda (x)
      (let* ((x (rational x))
             (i (or (position-if (lambda (knot) (<= knot x)) xs :from-end t :end (1- n)) 0))
             (hi (nth i h)) (a (- (nth (1+ i) xs) x)) (b (- x (nth i xs))))
        (+ (/ (+ (* (aref m i) a a a) (* (aref m (1+ i)) b b b)) (* 6 hi))
           (* (- (/ (nth i ys) hi) (/ (* (aref m i) hi) 6)) a)
           (* (- (/ (nth (1+ i) ys) hi) (/ (* (aref m (1+ i)) hi) 6)) b))))))

(defparameter *rpn14-x* '(7.99d0 8.09d0 8.19d0 8.7d0 9.2d0 10d0 12d0 15d0 20d0))
(defparameter *rpn14-y* '(0d0 2.76429d-5 4.37498d-2 0.169183d0 0.469428d0 0.943740d0
                          0.998636d0 0.999919d0 0.999994d0))

(defun non-decreasing-p (f lo hi n)
  (loop for (a b) on (mapcar f (sample-points lo hi n)) while b always (<= a b)))

(defun interpolation-checks ()
  (format t "~&Interpolation~%")
  (let ((xs '(0d0 0.3d0 1d0 1.7d0 2.5d0 3.1d0 4d0 5.2d0 6d0 7.5d0))
        (inside (sample-points 0d0 7.5d0 157)))
    (flet ((reproduces (method f &rest options)
             (worst-error (apply #'make-interpolator method xs (mapcar f xs) options) f inside)))
      (check-that (format nil "linear reproduces degree 1 (~,1E)" (reproduces :linear (lambda (x) (+ 2 (* 3 x)))))
                  (< (reproduces :linear (lambda (x) (+ 2 (* 3 x)))) 1d-13))
      (check-that "log-linear reproduces exp of degree 1"
                  (< (reproduces :log-linear (lambda (x) (exp (- 0.1d0 (* 0.04d0 x))))) 1d-15))
      ;; z x = 0.03 x + 0.01 is linear; knots away from 0, where z is infinite.
      (check-that "flat-forward reproduces zero rates z with z x linear"
                  (< (worst-error (make-interpolator :flat-forward (rest xs)
                                                     (mapcar (lambda (x) (+ 0.03d0 (/ 0.01d0 x)))
                                                             (rest xs)))
                                  (lambda (x) (+ 0.03d0 (/ 0.01d0 x)))
                                  (sample-points 0.3d0 7.5d0 100))
                     1d-14))
      (let ((cubic (lambda (x) (+ 1 (* -2 x) (* 0.5d0 x x) (* 0.3d0 x x x)))))
        ;; f'' = 1 + 1.8 x: the spline with those end second derivatives is f.
        (check-that (format nil "cubic spline with the cubic's end second derivatives reproduces it ~
on 10 knots (~,1E)" (reproduces :natural-cubic cubic :end-second-derivatives '(1 . 14.5d0)))
                    (< (reproduces :natural-cubic cubic :end-second-derivatives '(1 . 14.5d0)) 1d-12)))
      (check-that "monotone cubic reproduces a monotone quadratic"
                  (< (reproduces :monotone-cubic (lambda (x) (+ 1 x (* 0.2d0 x x)))) 1d-13)))
    (let* ((xs '(0 1/2 3/2 2 4)) (ys '(1 -1/3 2 5/4 0))
           (exact (exact-natural-spline xs ys))
           (spline (make-interpolator :natural-cubic xs ys)))
      (check-that "natural cubic = exact rational solution on 5 knots to 1e-14"
                  (< (loop for x in (sample-points 0d0 4d0 80)
                           maximize (abs (- (funcall spline x) (float (funcall exact x) 1d0))))
                     1d-14)))
    (let ((monotone (make-interpolator :monotone-cubic *rpn14-x* *rpn14-y*))
          (natural (make-interpolator :natural-cubic *rpn14-x* *rpn14-y*)))
      (check-that "monotone cubic stays monotone on Fritsch-Carlson's RPN 14 data"
                  (non-decreasing-p monotone 7.99d0 20d0 5000))
      (check-that "natural cubic does not"
                  (not (non-decreasing-p natural 7.99d0 20d0 5000)))
      (check-that "monotone cubic stays within the data's range"
                  (every (lambda (x) (<= 0d0 (funcall monotone x) 0.999994d0))
                         (sample-points 7.99d0 20d0 5000))))
    (let* ((xs '(0.25d0 1d0 2d0 5d0 10d0))
           (zeros '(0.02d0 0.025d0 0.03d0 0.032d0 0.031d0))
           (curve (make-interpolator :flat-forward xs zeros)))
      (check-that "flat-forward: the instantaneous forward (x z)' is constant on each segment"
                  (loop for (a b) on xs while b
                        always (let ((forwards (mapcar (lambda (x)
                                                         (+ (funcall curve x)
                                                            (* x (interpolator-derivative curve x))))
                                                       (sample-points (+ a 1d-9) (- b 1d-9) 20))))
                                 (< (- (reduce #'max forwards) (reduce #'min forwards)) 1d-12)))))
    (let ((interp (make-interpolator :linear '(0 1 2) '(0 1 4) :extrapolation :error)))
      (check-error "extrapolation-error under :error" (funcall interp 2.5d0) extrapolation-error)
      (check-that "the report names the domain"
                  (search "[0.0, 2.0]" (error-message (lambda () (funcall interp -1))))))
    (check "flat extrapolation" 4d0 (funcall (make-interpolator :linear '(0 1 2) '(0 1 4)) 9) 0)
    (check "linear extrapolation continues the end segment" 10d0
           (funcall (make-interpolator :linear '(0 1 2) '(0 1 4) :extrapolation :linear) 4) 1d-14)
    (check-error "knots must increase" (make-interpolator :linear '(0 2 1) '(0 1 2)) invalid-input)
    (check "derivative of the natural spline of x^3 on many knots at 1.5"
           (* 3 1.5d0 1.5d0)
           (interpolator-derivative (make-interpolator :natural-cubic (sample-points 0d0 3d0 300)
                                                       (mapcar (lambda (x) (expt x 3))
                                                               (sample-points 0d0 3d0 300))
                                                       :end-second-derivatives '(0 . 18))
                                    1.5d0)
           1d-6)
    ;; 10^6 lookups on 50 knots. A full call boxes its double argument and
    ;; result, and that alone takes ~44 ns on a 1.7 GHz laptop, so the bound
    ;; is on the interpolator's own work: the time beyond a trivial closure
    ;; called the same way, in the same run.
    (let* ((knots (sample-points 0d0 10d0 49))
           (interp (make-interpolator :natural-cubic knots (mapcar #'sin knots)))
           (trivial (lambda (x) (declare (double-float x)) (* 2 x)))
           (points (let ((v (make-array 1000000 :element-type 'double-float)))
                     (dotimes (i (length v) v) (setf (aref v i) (* 10d0 (/ (mod (* i 7919) 1000000) 1d6)))))))
      (flet ((ms (f)
               (declare (function f))
               (let ((start (get-internal-real-time)) (sum 0d0))
                 (declare (double-float sum))
                 (loop for x across (the (simple-array double-float (*)) points)
                       do (incf sum (the double-float (funcall f x))))
                 (/ (* 1000d0 (- (get-internal-real-time) start))
                    internal-time-units-per-second))))
        ;; Interleaved, best of 7 each, to shed load from other processes.
        (let ((lookups most-positive-double-float) (calls most-positive-double-float))
          (loop repeat 7
                do (setf lookups (min lookups (ms interp))
                         calls (min calls (ms trivial))))
          (check-that (format nil "10^6 lookups on 50 knots: ~,1F ms, ~,1F ms beyond a trivial ~
closure call (< 40)" lookups (- lookups calls))
                      (< (- lookups calls) 40d0)))))
    (let ((bilinear (make-interpolator-2d :bilinear '(0 1 2) '(0 10)
                                          '((0 10) (1 11) (2 12))))
          (bicubic (make-interpolator-2d :bicubic '(0 1 2 3) '(0 1 2 3)
                                         (loop for x below 4
                                               collect (loop for y below 4 collect (+ x (* 2 y)))))))
      (check "bilinear reproduces x + y" 6.5d0 (funcall bilinear 1.5d0 5d0) 1d-14)
      (check "bicubic reproduces x + 2y" 4.3d0 (funcall bicubic 1.1d0 1.6d0) 1d-13))))

;;; --------------------------------------------------------------------
;;; Tridiagonal systems
;;; --------------------------------------------------------------------

(defun random-dominant-system (n state &key cyclic)
  "(values a b c d), diagonally dominant."
  (flet ((vec (f) (let ((v (make-array n :element-type 'double-float)))
                    (dotimes (i n v) (setf (aref v i) (funcall f i))))))
    (let* ((a (vec (lambda (i) (if (or cyclic (> i 0)) (- (random 2d0 state) 1d0) 0d0))))
           (c (vec (lambda (i) (if (or cyclic (< i (1- n))) (- (random 2d0 state) 1d0) 0d0))))
           (b (vec (lambda (i) (* (if (evenp i) 1 -1) (+ 0.5d0 (abs (aref a i)) (abs (aref c i))
                                                       (random 1d0 state))))))
           (d (vec (lambda (i) (declare (ignore i)) (- (random 20d0 state) 10d0)))))
      (values a b c d))))

(defun dense-solve (a b c d &key cyclic)
  (let* ((n (length d))
         (m (magicl:zeros (list n n))))
    (dotimes (i n)
      (setf (magicl:tref m i i) (aref b i))
      (when (> i 0) (setf (magicl:tref m i (1- i)) (aref a i)))
      (when (< i (1- n)) (setf (magicl:tref m i (1+ i)) (aref c i))))
    (when cyclic
      (setf (magicl:tref m 0 (1- n)) (aref a 0)
            (magicl:tref m (1- n) 0) (aref c (1- n))))
    (magicl::storage (magicl:linear-solve m (magicl:from-array (copy-seq d) (list n 1))))))

(defun vector-relative-error (x reference)
  (/ (loop for i below (length x) maximize (abs (- (aref x i) (aref reference i))))
     (loop for i below (length x) maximize (abs (aref reference i)))))

(defun tridiagonal-checks ()
  (format t "~&Tridiagonal systems~%")
  (let ((state (sb-ext:seed-random-state 11)))
    (dolist (n '(10 100 1000))
      (multiple-value-bind (a b c d) (random-dominant-system n state)
        (let ((x (solve-tridiagonal! a b (copy-seq c) (copy-seq d))))
          (check-that (format nil "n = ~D: Thomas = magicl:linear-solve to 1e-12 (~,1E)" n
                              (vector-relative-error x (dense-solve a b c d)))
                      (< (vector-relative-error x (dense-solve a b c d)) 1d-12)))))
    (multiple-value-bind (a b c d) (random-dominant-system 10000 state)
      (let* ((x (solve-tridiagonal! a b (copy-seq c) (copy-seq d)))
             (ax (tridiagonal-multiply a b c x (make-array 10000 :element-type 'double-float))))
        (check-that (format nil "n = 10 000: residual |A x - d| / |d| < 1e-14 (~,1E)"
                            (vector-relative-error ax d))
                    (< (vector-relative-error ax d) 1d-14))))
    (dolist (n '(3 7 200))
      (multiple-value-bind (a b c d) (random-dominant-system n state :cyclic t)
        (let ((x (solve-tridiagonal-cyclic! a b c (copy-seq d))))
          (check-that (format nil "cyclic n = ~D = dense solve with the corners (~,1E)" n
                              (vector-relative-error x (dense-solve a b c d :cyclic t)))
                      (< (vector-relative-error x (dense-solve a b c d :cyclic t)) 1d-12)))))
    (multiple-value-bind (a b c d) (random-dominant-system 1000 state :cyclic t)
      (let ((c-copy (copy-seq c)) (d-copy (copy-seq d))
            (work (make-array 3000 :element-type 'double-float)))
        (solve-tridiagonal! a b c-copy (copy-seq d))         ; warm up
        (flet ((consed (thunk)
                 (let ((before (sb-ext:get-bytes-consed)))
                   (funcall thunk)
                   (- (sb-ext:get-bytes-consed) before))))
          (check-equal "Thomas allocates nothing" 0
                       (consed (lambda () (solve-tridiagonal! a b c-copy d-copy))))
          (check-equal "the cyclic solve with a workspace allocates nothing" 0
                       (consed (lambda () (solve-tridiagonal-cyclic! a b c d-copy :work work))))))))
  (check-error "a zero pivot signals convergence-failure"
               (solve-tridiagonal! (make-array 2 :element-type 'double-float :initial-element 1d0)
                                   (make-array 2 :element-type 'double-float :initial-element 0d0)
                                   (make-array 2 :element-type 'double-float :initial-element 1d0)
                                   (make-array 2 :element-type 'double-float :initial-element 1d0))
               convergence-failure))

;;; --------------------------------------------------------------------
;;; Optimization
;;; --------------------------------------------------------------------

(defun rosenbrock (x)
  (loop for i below (1- (length x))
        sum (+ (* 100 (expt (- (aref x (1+ i)) (expt (aref x i) 2)) 2))
               (expt (- 1 (aref x i)) 2))))

(defun rosenbrock-residuals (x)
  "Residuals whose sum of squares is ROSENBROCK: 10 (x_{i+1} - x_i^2), 1 - x_i."
  (let* ((n (length x)) (r (make-array (* 2 (1- n)) :element-type 'double-float)))
    (dotimes (i (1- n) r)
      (setf (aref r (* 2 i)) (* 10 (- (aref x (1+ i)) (expt (aref x i) 2)))
            (aref r (1+ (* 2 i))) (- 1 (aref x i))))))

(defun rastrigin (x)
  (+ (* 10 (length x))
     (loop for v across x sum (- (* v v) (* 10 (cos (* 2 pi v)))))))

(defun distance-to-ones (x)
  (loop for v across x maximize (abs (- v 1))))

(defun optimizer-checks ()
  (format t "~&Optimization~%")
  ;; r = (x0 e^x1, sin x2 + x0 x2) at (0.5, -1.5, 2).
  (let ((x (coerce '(0.5d0 -1.5d0 2d0) 'double-vector)))
    (flet ((f (x) (let ((r (make-array 2 :element-type 'double-float)))
                    (setf (aref r 0) (* (aref x 0) (exp (aref x 1)))
                          (aref r 1) (+ (sin (aref x 2)) (* (aref x 0) (aref x 2))))
                    r)))
      (progn
        (dolist (scheme '(:forward :central))
          (let* ((out (jacobian! #'f x (make-array 6 :element-type 'double-float) :scheme scheme))
                 ;; Column-major 2 x 3: (dr0/dx0 dr1/dx0 dr0/dx1 dr1/dx1 dr0/dx2 dr1/dx2).
                 (expected (vector (exp -1.5d0) 2d0 (* 0.5d0 (exp -1.5d0)) 0d0 0d0
                                   (+ (cos 2d0) 0.5d0)))
                 (worst (loop for i below 6 maximize (abs (- (aref out i) (aref expected i))))))
            (check-that (format nil "jacobian!, ~(~A~): within ~:[1e-7~;1e-9~] of the analytic ~
Jacobian (~,1E)" scheme (eq scheme :central) worst)
                        (< worst (if (eq scheme :central) 1d-9 1d-7))))))))
  (multiple-value-bind (x fx iterations)
      (minimize :nelder-mead #'rosenbrock '(-1.2 1) :max-iterations 5000 :tolerance 1d-14)
    (check-that (format nil "Nelder-Mead, Rosenbrock 2-D: |x - 1| = ~,1E < 1e-6 (~D iterations, f ~,1E)"
                        (distance-to-ones x) iterations fx)
                (< (distance-to-ones x) 1d-6)))
  (multiple-value-bind (x fx iterations)
      (minimize :levenberg-marquardt #'rosenbrock-residuals
                (loop for i below 10 collect (if (evenp i) -1.2d0 1d0)) :max-iterations 500)
    (check-that (format nil "Levenberg-Marquardt, Rosenbrock 10-D residuals: |x - 1| = ~,1E < 1e-6 ~
(~D iterations, f ~,1E)" (distance-to-ones x) iterations fx)
                (< (distance-to-ones x) 1d-6)))
  (flet ((de (workers)
           (let ((lp:*kernel* (lp:make-kernel workers)))
             (unwind-protect
                  (multiple-value-list
                   (minimize :differential-evolution #'rastrigin '(3 -2)
                             :bounds '((-5.12 . 5.12) (-5.12 . 5.12)) :tolerance 1d-14
                             :max-iterations 2000))
               (lp:end-kernel :wait t)))))
    (let ((one (de 1)) (eight (de 8)))
      (check-that (format nil "differential evolution, Rastrigin 2-D: |x| = ~,1E < 1e-6 (~D generations)"
                          (loop for v across (first one) maximize (abs v)) (third one))
                  (< (loop for v across (first one) maximize (abs v)) 1d-6))
      (check-that "... identical with 1 and 8 workers" (equalp one eight))))
  ;; (x - 3)^2 + (y + 1)^2 on x in (0, 2), y > 0: the minimum is the corner (2, 0).
  (let ((corner (lambda (x) (+ (expt (- (aref x 0) 3) 2) (expt (+ (aref x 1) 1) 2))))
        (bounds '((0 . 2) (0 . nil))))
    (let ((x (minimize :nelder-mead corner '(1 1) :bounds bounds :max-iterations 5000)))
      (check-that (format nil "Nelder-Mead with bounds reaches the corner (2, 0) to 1e-6 (~,1E)"
                          (max (abs (- (aref x 0) 2)) (abs (aref x 1))))
                  (< (max (abs (- (aref x 0) 2)) (abs (aref x 1))) 1d-6)))
    (let ((x (minimize :levenberg-marquardt
                       (lambda (x) (let ((r (make-array 2 :element-type 'double-float)))
                                     (setf (aref r 0) (- (aref x 0) 3) (aref r 1) (+ (aref x 1) 1))
                                     r))
                       '(1 1) :bounds bounds :max-iterations 5000)))
      (check-that (format nil "Levenberg-Marquardt with bounds reaches the corner to 1e-6 (~,1E)"
                          (max (abs (- (aref x 0) 2)) (abs (aref x 1))))
                  (< (max (abs (- (aref x 0) 2)) (abs (aref x 1))) 1d-6))))
  (check-error "too few iterations signal convergence-failure"
               (minimize :nelder-mead #'rosenbrock '(-1.2 1) :max-iterations 5)
               convergence-failure)
  (multiple-value-bind (x fx iterations)
      (handler-bind ((convergence-failure (lambda (c) (use-best c))))
        (minimize :nelder-mead #'rosenbrock '(-1.2 1) :max-iterations 5))
    (check-that "use-best returns the best point found, its value and the iterations"
                (and (= 2 (length x)) (= fx (rosenbrock x)) (= iterations 5)
                     (< fx (rosenbrock (coerce '(-1.2d0 1d0) 'double-vector))))))
  (check-error "a start outside the bounds" (minimize :nelder-mead #'rosenbrock '(3 1)
                                                      :bounds '((0 . 2) (nil . nil)))
               invalid-input)
  (check-error "differential evolution needs a box" (minimize :differential-evolution #'rastrigin
                                                              '(1 1) :bounds '((0 . nil) (0 . 1)))
               invalid-input))
