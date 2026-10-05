;;;; math/optimize.lisp --- multivariate minimization
;;;;
;;;; (minimize method f x0 &key bounds tolerance max-iterations jacobian)
;;;;
;;;;   :nelder-mead             derivative-free simplex search (adaptive
;;;;                            parameters, Gao and Han 2012)
;;;;   :levenberg-marquardt     nonlinear least squares: F returns a residual
;;;;                            vector; damped Gauss-Newton steps solved by QR
;;;;                            (Nielsen's damping update)
;;;;   :differential-evolution  global search, DE/rand/1/bin, population
;;;;                            evaluated in parallel
;;;;
;;;; Box bounds are a list of (lo . hi) conses, either end possibly NIL. The
;;;; local methods work in unconstrained coordinates u with x = lo + (hi - lo)
;;;; / (1 + exp(-u)) for two-sided bounds, lo + exp(u) or hi - exp(u) for one
;;;; side; convergence is judged in x, where the bounds' approach is
;;;; visible. Differential evolution samples the box itself, so it needs
;;;; both ends of every bound.
;;;;
;;;; Failure to converge signals CONVERGENCE-FAILURE with restarts USE-VALUE
;;;; (return a point) and USE-BEST (return the best point found).

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Results and failure
;;; --------------------------------------------------------------------

(defun minimize-failure (method iterations best-x best-f)
  "Signal CONVERGENCE-FAILURE for METHOD after ITERATIONS; USE-BEST returns
(values BEST-X BEST-F ITERATIONS), USE-VALUE (values x NIL ITERATIONS)."
  (restart-case
      (error 'convergence-failure
             :context (format nil "~(~A~) did not converge in ~D iterations; the best ~
point found has f = ~,6G" method iterations best-f))
    (use-value (x)
      :report "Return a point as the minimizer."
      (values x nil iterations))
    (use-best ()
      :report "Return the best point found."
      (values best-x best-f iterations))))

(defun use-best (&optional condition)
  "Invoke the USE-BEST restart for CONDITION, if there is one, returning the
best point a minimizer found; otherwise return NIL. MINIMIZE offers it when
it does not converge."
  (let ((restart (find-restart 'use-best condition)))
    (when restart (invoke-restart restart))))

;;; --------------------------------------------------------------------
;;; Box bounds by reparameterization
;;; --------------------------------------------------------------------

(defun parse-bounds (bounds n)
  "BOUNDS as a list of N (lo . hi) conses of double-floats or NIL."
  (cond ((null bounds) (make-list n :initial-element '(nil . nil)))
        ((and (listp bounds) (= (length bounds) n)
              (every (lambda (b) (and (consp b)
                                      (or (null (car b)) (realp (car b)))
                                      (or (null (cdr b)) (realp (cdr b)))
                                      (or (null (car b)) (null (cdr b)) (< (car b) (cdr b)))))
                     bounds))
         (mapcar (lambda (b) (cons (and (car b) (float (car b) 1d0))
                                   (and (cdr b) (float (cdr b) 1d0))))
                 bounds))
        (t (signal-invalid-argument 'bounds bounds 'list
                                    (format nil "~D conses (lo . hi), lo < hi, either end NIL" n)))))

(defun to-x (bounds u)
  "The point x of the unconstrained coordinates U."
  (map 'double-vector
       (lambda (b u)
         (destructuring-bind (lo . hi) b
           (cond ((and lo hi) (+ lo (/ (- hi lo) (+ 1d0 (exp (- u))))))
                 (lo (+ lo (exp u)))
                 (hi (- hi (exp u)))
                 (t u))))
       bounds u))

(defun to-u (bounds x)
  "The unconstrained coordinates of the point X, strictly inside BOUNDS."
  (map 'double-vector
       (lambda (b x)
         (destructuring-bind (lo . hi) b
           (unless (and (or (null lo) (> x lo)) (or (null hi) (< x hi)))
             (signal-invalid-argument 'x0 x 'real
                                      (format nil "a start strictly inside (~@[~F~], ~@[~F~])" lo hi)))
           (cond ((and lo hi) (log (/ (- x lo) (- hi x))))
                 (lo (log (- x lo)))
                 (hi (log (- hi x)))
                 (t x))))
       bounds x))

(defun dx/du (bounds u)
  "The diagonal of the reparameterization's Jacobian at U."
  (map 'double-vector
       (lambda (b u)
         (destructuring-bind (lo . hi) b
           (cond ((and lo hi) (let ((s (/ (+ 1d0 (exp (- u))))))
                                (* (- hi lo) s (- 1d0 s))))
                 (lo (exp u))
                 (hi (- (exp u)))
                 (t 1d0))))
       bounds u))

(defun distance-inf (a b)
  (loop for i below (length a) maximize (abs (- (aref a i) (aref b i)))))

;;; --------------------------------------------------------------------
;;; Entry point
;;; --------------------------------------------------------------------

(defun minimize (method f x0 &key bounds (tolerance 1d-10) (max-iterations 1000) jacobian
                                   (seed 1) population)
  "Minimize F from X0 (a sequence) by METHOD; returns (values x fx
iterations), X a DOUBLE-VECTOR.

  :nelder-mead             F maps a DOUBLE-VECTOR to a real. Converged when
                           the simplex spans less than TOLERANCE in x and in f.
  :levenberg-marquardt     F maps a DOUBLE-VECTOR to a DOUBLE-VECTOR of
                           residuals; fx is their sum of squares. JACOBIAN, a
                           function (x out) filling OUT column-major (as
                           JACOBIAN!), replaces finite differences. Converged
                           when the gradient or the step is below TOLERANCE.
  :differential-evolution  F as for Nelder-Mead; BOUNDS must give both ends of
                           every coordinate; X0 joins the initial population.
                           POPULATION (default 15 n) members, a fixed SEED,
                           F evaluated in parallel under lp:*kernel* (so F
                           must be safe to call from several threads); the
                           result does not depend on the thread count.
                           Converged when the population's f spans less than
                           TOLERANCE.

BOUNDS is a list of (lo . hi) per coordinate, either end NIL; X0 must lie
strictly inside. MAX-ITERATIONS counts iterations (generations for DE).
Without convergence, signals CONVERGENCE-FAILURE with restarts USE-BEST and
USE-VALUE.

  (minimize :nelder-mead (lambda (x) (+ (expt (- (aref x 0) 1) 2) (expt (aref x 1) 2)))
            '(0 1) :bounds '((0 . nil) (nil . nil)))"
  (let* ((x0 (as-double-vector x0))
         (n (length x0))
         (bounds (parse-bounds bounds n)))
    (ecase method
      (:nelder-mead
       (let ((u0 (to-u bounds x0)))
         (nelder-mead (lambda (u) (float (funcall f (to-x bounds u)) 1d0))
                      u0 bounds tolerance max-iterations)))
      (:levenberg-marquardt
       (let ((u0 (to-u bounds x0)))
         (levenberg-marquardt (lambda (u) (funcall f (to-x bounds u)))
                              u0 bounds tolerance max-iterations
                              (and jacobian
                                   (lambda (u out)
                                     ;; dr/du = dr/dx diag(dx/du), column by column.
                                     (funcall jacobian (to-x bounds u) out)
                                     (let ((scale (dx/du bounds u)) (m (floor (length out) n)))
                                       (dotimes (j n out)
                                         (dotimes (i m)
                                           (setf (aref out (+ i (* m j)))
                                                 (* (aref out (+ i (* m j))) (aref scale j)))))))))))
      (:differential-evolution
       (unless (every (lambda (b) (and (car b) (cdr b))) bounds)
         (signal-invalid-argument 'bounds bounds 'list
                                  "both ends of every bound, for :DIFFERENTIAL-EVOLUTION"))
       (differential-evolution (lambda (x) (float (funcall f x) 1d0)) x0 bounds tolerance
                               max-iterations seed (or population (* 15 n)))))))

;;; --------------------------------------------------------------------
;;; Nelder-Mead
;;; --------------------------------------------------------------------

(defun nelder-mead (g u0 bounds tolerance max-iterations)
  "Nelder-Mead on G over unconstrained U, with Gao and Han's
dimension-dependent coefficients: reflection 1, expansion 1 + 2/n,
contraction 3/4 - 1/(2n), shrink 1 - 1/n. Returns (values x fx iterations)."
  (let* ((n (length u0))
         (alpha 1d0)
         (beta (+ 1d0 (/ 2d0 n)))
         (gamma (if (> n 1) (- 0.75d0 (/ 1d0 (* 2 n))) 0.5d0))
         (delta (if (> n 1) (- 1d0 (/ 1d0 n)) 0.5d0))
         ;; Vertices: u0 and u0 + tau_i e_i (5% of the coordinate, 2.5e-4 at 0).
         (simplex (cons (copy-seq u0)
                        (loop for i below n
                              collect (let ((v (copy-seq u0)))
                                        (incf (aref v i) (if (zerop (aref v i))
                                                             0.00025d0
                                                             (* 0.05d0 (aref v i))))
                                        v))))
         (vertices (mapcar (lambda (v) (cons (funcall g v) v)) simplex)))
    (flet ((point (centroid direction coefficient)
             ;; centroid + coefficient (direction - centroid)
             (map 'double-vector (lambda (c d) (+ c (* coefficient (- d c)))) centroid direction))
           (evaluated (v) (cons (funcall g v) v)))
      (loop for iteration from 0
            do (setf vertices (sort vertices #'< :key #'car))
               (let* ((best (first vertices)) (worst (a:lastcar vertices))
                      (x-best (to-x bounds (cdr best))))
                 (when (and (<= (- (car worst) (car best)) tolerance)
                            (loop for (nil . v) in (rest vertices)
                                  always (<= (distance-inf (to-x bounds v) x-best) tolerance)))
                   (return (values x-best (car best) iteration)))
                 (when (>= iteration max-iterations)
                   (return (minimize-failure :nelder-mead iteration x-best (car best))))
                 (let* ((centroid (let ((c (make-array n :element-type 'double-float
                                                         :initial-element 0d0)))
                                    (loop for (nil . v) in (butlast vertices)
                                          do (dotimes (i n) (incf (aref c i) (/ (aref v i) n))))
                                    c))
                        (second-worst (car (a:lastcar (butlast vertices))))
                        (reflected (evaluated (point centroid (cdr worst) (- alpha)))))
                   (flet ((replace-worst (vertex) (setf vertices `(,@(butlast vertices) ,vertex)))
                          (shrink ()
                            (setf vertices
                                  (cons best (mapcar (lambda (vertex)
                                                       (evaluated (point (cdr best) (cdr vertex) delta)))
                                                     (rest vertices))))))
                     (cond ((< (car reflected) (car best))
                            (let ((expanded (evaluated (point centroid (cdr reflected) beta))))
                              (replace-worst (if (< (car expanded) (car reflected)) expanded reflected))))
                           ((< (car reflected) second-worst)
                            (replace-worst reflected))
                           ((< (car reflected) (car worst))
                            ;; Outside contraction.
                            (let ((contracted (evaluated (point centroid (cdr reflected) gamma))))
                              (if (<= (car contracted) (car reflected))
                                  (replace-worst contracted)
                                  (shrink))))
                           (t
                            ;; Inside contraction.
                            (let ((contracted (evaluated (point centroid (cdr worst) gamma))))
                              (if (< (car contracted) (car worst))
                                  (replace-worst contracted)
                                  (shrink))))))))))))

;;; --------------------------------------------------------------------
;;; Levenberg-Marquardt
;;; --------------------------------------------------------------------

(defun sum-of-squares (r)
  (loop for v across r sum (* v v)))

(defun levenberg-marquardt (residuals u0 bounds tolerance max-iterations jacobian)
  "Levenberg-Marquardt on the residual function RESIDUALS over U. Each step
solves min |J d + r|^2 + mu |d|^2 by QR of [J; sqrt(mu) I] (never the
normal equations); the damping mu follows Nielsen (1999): start at 1e-3
max diag(J'J), scale by max(1/3, 1 - (2 rho - 1)^3) on an accepted step, by
nu (doubling) on a rejected one. Returns (values x |r|^2 iterations)."
  (let* ((n (length u0))
         (u (copy-seq u0))
         (r (as-double-vector (funcall residuals u)))
         (m (length r))
         (jac (make-array (* m n) :element-type 'double-float))
         (row (make-array n :element-type 'double-float))
         (gradient (make-array n :element-type 'double-float))
         (mu 0d0) (nu 2d0))
    (labels ((update-jacobian ()
               (if jacobian
                   (funcall jacobian u jac)
                   (jacobian! residuals u jac :f0 r))
               ;; g = J' r
               (dotimes (j n)
                 (setf (aref gradient j)
                       (loop for i below m sum (* (aref jac (+ i (* m j))) (aref r i))))))
             (max-diagonal ()
               (loop for j below n
                     maximize (loop for i below m sum (expt (aref jac (+ i (* m j))) 2))))
             (damped-step ()
               ;; Least squares for d: rows of J with -r, and sqrt(mu) I with 0.
               (let ((acc (make-qr-accumulator n)) (root (sqrt mu)))
                 (dotimes (i m)
                   (dotimes (j n) (setf (aref row j) (aref jac (+ i (* m j)))))
                   (qr-add-row! acc row (- (aref r i))))
                 (dotimes (j n)
                   (fill row 0d0)
                   (setf (aref row j) root)
                   (qr-add-row! acc row 0d0))
                 (qr-solve acc))))
      (update-jacobian)
      (setf mu (* 1d-3 (max-diagonal)))
      (when (zerop mu) (setf mu 1d-3))
      (loop for iteration from 0
            do (when (<= (loop for v across gradient maximize (abs v)) tolerance)
                 (return (values (to-x bounds u) (sum-of-squares r) iteration)))
               (when (>= iteration max-iterations)
                 (return (minimize-failure :levenberg-marquardt iteration (to-x bounds u)
                                           (sum-of-squares r))))
               (let* ((d (damped-step))
                      (u-new (map 'double-vector #'+ u d))
                      (x (to-x bounds u)) (x-new (to-x bounds u-new)))
                 (when (<= (distance-inf x-new x)
                           (* tolerance (+ (loop for v across x maximize (abs v)) tolerance)))
                   (return (values x-new (sum-of-squares (as-double-vector (funcall residuals u-new)))
                                   iteration)))
                 (let* ((r-new (as-double-vector (funcall residuals u-new)))
                        (actual (* 0.5d0 (- (sum-of-squares r) (sum-of-squares r-new))))
                        ;; L(0) - L(d) = d' (mu d - g) / 2
                        (predicted (* 0.5d0 (loop for j below n
                                                  sum (* (aref d j)
                                                         (- (* mu (aref d j)) (aref gradient j))))))
                        (rho (if (plusp predicted) (/ actual predicted) -1d0)))
                   (if (plusp rho)
                       (setf u u-new r r-new
                             mu (* mu (max (/ 1d0 3) (- 1 (expt (- (* 2 rho) 1) 3))))
                             nu 2d0)
                       (setf mu (* mu nu) nu (* 2 nu)))
                   (when (plusp rho) (update-jacobian))))))))

;;; --------------------------------------------------------------------
;;; Differential evolution
;;; --------------------------------------------------------------------

(defun de-uniform (seed generation member draw population)
  "Uniform DRAW of MEMBER in GENERATION: a pure function of its indices
(Philox, as the Monte Carlo :counter stream), so the search is the same in
any order and on any number of threads."
  (multiple-value-bind (a b)
      (counter-uniforms (ldb (byte 64 0) seed) (+ member (* generation population)) (floor draw 2))
    (if (evenp draw) a b)))

(defun differential-evolution (g x0 bounds tolerance max-iterations seed population)
  "DE/rand/1/bin (Storn and Price 1997): mutant a + F (b - c) from three
distinct other members, F = 0.8, binomial crossover with CR = 0.9, and a
trial replacing its target if no worse. Coordinates leaving the box bounce
back between the target and the bound. Trials of a generation are evaluated
in parallel; selection is serial, so results do not depend on the thread
count. Returns (values x fx generations)."
  (let* ((n (length x0))
         (np (max 4 population))
         (lows (map 'double-vector #'car bounds))
         (highs (map 'double-vector #'cdr bounds))
         (members (make-array np))
         (values (make-array np :element-type 'double-float)))
    (flet ((evaluate-all (points)
             (if lp:*kernel*
                 (lp:pmap 'vector g points)
                 (map 'vector g points))))
      ;; Generation 0: X0 and uniform points in the box.
      (setf (aref members 0) (copy-seq x0))
      (loop for k from 1 below np
            do (setf (aref members k)
                     (let ((x (make-array n :element-type 'double-float)))
                       (dotimes (j n x)
                         (setf (aref x j) (+ (aref lows j)
                                             (* (- (aref highs j) (aref lows j))
                                                (de-uniform seed 0 k j np))))))))
      (let ((initial (evaluate-all members)))
        (dotimes (k np) (setf (aref values k) (aref initial k))))
      (loop for generation from 1
            do (let* ((best (loop with best = 0 for k from 1 below np
                                  when (< (aref values k) (aref values best)) do (setf best k)
                                  finally (return best)))
                      (spread (- (loop for v across values maximize v) (aref values best))))
                 (when (<= spread tolerance)
                   (return (values (copy-seq (aref members best)) (aref values best) (1- generation))))
                 (when (> generation max-iterations)
                   (return (minimize-failure :differential-evolution (1- generation)
                                             (copy-seq (aref members best)) (aref values best))))
                 (let ((trials (make-array np)))
                   (dotimes (k np)
                     (let ((draw 0))
                       (flet ((u () (prog1 (de-uniform seed generation k draw np) (incf draw))))
                         ;; Three distinct members other than K.
                         (let ((picks '()))
                           (loop while (< (length picks) 3)
                                 do (let ((candidate (floor (* (u) np))))
                                      (unless (or (= candidate k) (member candidate picks))
                                        (push candidate picks))))
                           (destructuring-bind (c b a) picks
                             (let* ((target (aref members k))
                                    (forced (floor (* (u) n)))
                                    (trial (make-array n :element-type 'double-float)))
                               (dotimes (j n)
                                 (let ((value (if (or (= j forced) (< (u) 0.9d0))
                                                  (+ (aref (aref members a) j)
                                                     (* 0.8d0 (- (aref (aref members b) j)
                                                                 (aref (aref members c) j))))
                                                  (aref target j)))
                                       (lo (aref lows j)) (hi (aref highs j)))
                                   ;; Bounce back into the box.
                                   (setf (aref trial j)
                                         (cond ((< value lo) (+ lo (* (u) (- (aref target j) lo))))
                                               ((> value hi) (- hi (* (u) (- hi (aref target j)))))
                                               (t value)))))
                               (setf (aref trials k) trial)))))))
                   (let ((trial-values (evaluate-all trials)))
                     (dotimes (k np)
                       (when (<= (aref trial-values k) (aref values k))
                         (setf (aref members k) (aref trials k)
                               (aref values k) (aref trial-values k)))))))))))
