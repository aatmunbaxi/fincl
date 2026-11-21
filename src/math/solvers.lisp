;;;; math/solvers.lisp --- one-dimensional root finding
;;;;
;;;; All solvers share one failure protocol: CONVERGENCE-FAILURE, signalled with a
;;;; USE-VALUE restart. Bracketing solvers also offer a RETRY-WITH-BRACKET restart. Thus
;;;; the caller chooses the recovery policy.
;;;;
;;;; SOLVE-BRENT-FROM-GUESS reproduces the iterates of the Brent solver with a guess in
;;;; the reference library. QL-CLOSE-P is the close() test of that library.
;;;;
;;;;   SOLVE-BISECTION  bracketed, linear convergence. It cannot fail once the root is
;;;;                    bracketed
;;;;   SOLVE-BRENT      bracketed, superlinear. It is the default choice
;;;;   SOLVE-NEWTON     from a starting point, quadratic near the root, with the
;;;;                    derivative by central differences. A bracket can safeguard it

(in-package #:fincl)

(define-condition convergence-failure (pricing-error)
  ((context :initarg :context :reader failure-context))
  (:report (lambda (c s)
             (format s "Numerical procedure failed to converge: ~A"
                     (failure-context c)))))

(defun signal-convergence-failure (format-control &rest arguments)
  "Signal CONVERGENCE-FAILURE with a USE-VALUE restart."
  (restart-case
      (error 'convergence-failure
             :context (apply #'format nil format-control arguments))
    (use-value (v)
      :report "Supply a value to use instead."
      v)))

;;; --------------------------------------------------------------------
;;; Bracketing
;;; --------------------------------------------------------------------

(defun call-with-bracket (solver f lo hi expand-hi)
  "Check that F changes sign on [LO, HI], doubling HI up to 60 times first
if EXPAND-HI, then return (funcall SOLVER lo hi f-lo f-hi). Without a sign
change, signal CONVERGENCE-FAILURE with USE-VALUE and RETRY-WITH-BRACKET
restarts."
  (let ((flo (funcall f lo))
        (fhi (funcall f hi)))
    (when expand-hi
      (iter (repeat 60)
            (while (plusp (* flo fhi)))
            (setf hi (* hi 2d0)
                  fhi (funcall f hi))))
    (if (plusp (* flo fhi))
        (restart-case
            (error 'convergence-failure
                   :context (format nil "no sign change on [~,6F, ~,6F]" lo hi))
          (use-value (v)
            :report "Supply a value to use instead."
            v)
          (retry-with-bracket (new-lo new-hi)
            :report "Retry with a different bracket."
            (call-with-bracket solver f new-lo new-hi nil)))
        (funcall solver lo hi flo fhi))))

;;; --------------------------------------------------------------------
;;; Bisection
;;; --------------------------------------------------------------------

(declaim (ftype (function ((or function symbol) real real &key (:expand-hi t) (:epsilon real) (:delta real))
                    (values real &optional))
          solve-bisection))
(defun solve-bisection (f lo hi &key expand-hi (epsilon 1d-12) (delta 1d-12))
  "Return the root of F on [LO, HI] by bisection (NU:ROOT-BISECTION). It stops when the
bracket is narrower than DELTA or |F| is below EPSILON. See CALL-WITH-BRACKET for
EXPAND-HI and the restarts.

  (solve-bisection (lambda (x) (- (* x x) 2)) 0d0 2d0) => 1.4142135623...d0"
  (call-with-bracket
   (lambda (lo hi flo fhi)
     (declare (ignore flo fhi))
     (nth-value 0 (nu:root-bisection f (nu:interval lo hi)
                                     :epsilon epsilon :delta delta)))
   f lo hi expand-hi))

;;; --------------------------------------------------------------------
;;; Brent
;;; --------------------------------------------------------------------

(defun brent (f a b fa fb tolerance max-iterations)
  "Brent-Dekker iteration on a bracket [A, B] with F(A) = FA and F(B) = FB
of opposite signs (R. P. Brent, Algorithms for Minimization Without
Derivatives, 1973, ch. 4). Returns (values root iterations)."
  (declare (double-float a b fa fb tolerance) (fixnum max-iterations))
  ;; B is the best estimate, A the previous one, C the contrapoint that
  ;; keeps the root bracketed; D and E are the last two step sizes.
  (let* ((c b) (fc fb) (d (- b a)) (e d))
    (declare (double-float c fc d e))
    (iter (declare (declare-variables))
          (for (the fixnum iteration) from 1 to max-iterations)
          (when (or (and (plusp fb) (plusp fc)) (and (minusp fb) (minusp fc)))
            (setf c a fc fa d (- b a) e d))
          (when (< (abs fc) (abs fb))
            (setf a b b c c a
                  fa fb fb fc fc fa))
          (let ((tol (+ (* 2d0 double-float-epsilon (abs b)) (* 0.5d0 tolerance)))
                (half (* 0.5d0 (- c b))))
            (when (or (<= (abs half) tol) (zerop fb))
              (return-from brent (values b iteration)))
            (if (and (>= (abs e) tol) (> (abs fa) (abs fb)))
                ;; Try interpolation: secant when only two points are
                ;; distinct, inverse quadratic otherwise.
                (let* ((s (/ fb fa))
                       (p 0d0) (q 0d0))
                  (declare (double-float s p q))
                  (if (= a c)
                      (setf p (* 2d0 half s)
                            q (- 1d0 s))
                      (let ((qq (/ fa fc)) (r (/ fb fc)))
                        (setf p (* s (- (* 2d0 half qq (- qq r)) (* (- b a) (- r 1d0))))
                              q (* (- qq 1d0) (- r 1d0) (- s 1d0)))))
                  (if (plusp p) (setf q (- q)) (setf p (- p)))
                  ;; Accept the step only if it stays well inside the
                  ;; bracket and shrinks faster than bisection would.
                  (if (< (* 2d0 p) (min (- (* 3d0 half q) (abs (* tol q)))
                                        (abs (* e q))))
                      (setf e d d (/ p q))
                      (setf d half e d)))
                (setf d half e d))
            (setf a b fa fb)
            (incf b (if (> (abs d) tol) d (float-sign half tol)))
            (setf fb (float (funcall f b) 1d0))))
    (signal-convergence-failure
     "Brent's method: no convergence in ~D iterations near ~,12F" max-iterations b)))

(declaim (ftype (function ((or function symbol) real real &key (:expand-hi t) (:tolerance real) (:max-iterations (integer 0)))
                    (values real &optional integer))
          solve-brent))
(defun solve-brent (f lo hi &key expand-hi (tolerance 1d-12) (max-iterations 1000))
  "Return the root of F on [LO, HI] by the method of Brent, to within TOLERANCE in x. The
method converges superlinearly for smooth F. Its worst case is a flat root such as (x -
1)^3. There it takes about the square of the bisection count (about 1700 iterations for
TOLERANCE 1e-12 on a unit bracket, and 125 in that example). This is why MAX-ITERATIONS
has its default. Return (values root iterations). See CALL-WITH-BRACKET for EXPAND-HI and
the restarts.

  (solve-brent (lambda (x) (- (* x x) 2)) 0d0 2d0) => 1.4142135623730951d0, 7"
  (call-with-bracket
   (lambda (lo hi flo fhi)
     (brent f (float lo 1d0) (float hi 1d0) (float flo 1d0) (float fhi 1d0)
            (float tolerance 1d0) max-iterations))
   f lo hi expand-hi))

;;; --------------------------------------------------------------------
;;; Newton with a finite-difference derivative
;;; --------------------------------------------------------------------

(defun ql-close-p (x y)
  "Whether X and Y are equal within 42 ulps of both (of 0, within
(42 eps)^2)."
  (declare (double-float x y))
  (or (= x y)
      (let ((diff (abs (- x y)))
            (tolerance (* 42 double-float-epsilon)))
        (if (or (zerop x) (zerop y))
            (< diff (* tolerance tolerance))
            (and (<= diff (* tolerance (abs x))) (<= diff (* tolerance (abs y))))))))

(declaim (ftype (function ((or function symbol) real real real &key (:tolerance real) (:max-evaluations (integer 0)))
                    (values real &optional))
          solve-brent-from-guess))
(defun solve-brent-from-guess (f lo hi guess &key (tolerance 1d-8) (max-evaluations 10000))
  "Root of F on [LO, HI] by Brent's method started from GUESS inside the
bracket, to within TOLERANCE in x, with the iterates of the reference
implementation. Use it where a result must match the reference at its
accuracy (Jamshidian's critical rate);
SOLVE-BRENT is the general solver.

  (solve-brent-from-guess (lambda (x) (- (* x x) 2)) 0d0 2d0 1d0) => 1.4142135623730951d0"
  (let* ((tolerance (max (float tolerance 1d0) double-float-epsilon))
         (x-min (float lo 1d0)) (x-max (float hi 1d0))
         (fx-min (float (funcall f x-min) 1d0)))
    (when (ql-close-p fx-min 0d0) (return-from solve-brent-from-guess x-min))
    (let ((fx-max (float (funcall f x-max) 1d0))
          (evaluations 2))
      (when (ql-close-p fx-max 0d0) (return-from solve-brent-from-guess x-max))
      (unless (minusp (* fx-min fx-max))
        (signal-convergence-failure "Brent's method: no sign change on [~,12F, ~,12F]" x-min x-max))
      (unless (< x-min guess x-max)
        (signal-invalid-argument 'guess guess 'real (format nil "a guess inside (~A, ~A)" lo hi)))
      (let* ((root (float guess 1d0))
             (froot (float (funcall f root) 1d0)))
        (incf evaluations)
        ;; Start with ROOT on one side and both ends on the other.
        (if (minusp (* froot fx-min))
            (setf x-max x-min fx-max fx-min)
            (setf x-min x-max fx-min fx-max))
        (let* ((d (- root x-max)) (e d))
          (iter (while (<= evaluations max-evaluations))
                (when (or (and (plusp froot) (plusp fx-max)) (and (minusp froot) (minusp fx-max)))
                  (setf x-max x-min fx-max fx-min
                        d (- root x-min) e d))
                (when (< (abs fx-max) (abs froot))
                  (setf x-min root root x-max x-max x-min
                        fx-min froot froot fx-max fx-max fx-min))
                (let ((acc (+ (* 2d0 double-float-epsilon (abs root)) (* 0.5d0 tolerance)))
                      (mid (/ (- x-max root) 2d0)))
                  (when (or (<= (abs mid) acc) (ql-close-p froot 0d0))
                    (return-from solve-brent-from-guess root))
                  (if (and (>= (abs e) acc) (> (abs fx-min) (abs froot)))
                      (let ((s (/ froot fx-min)) (p 0d0) (q 0d0))
                        (if (ql-close-p x-min x-max)
                            (setf p (* 2d0 mid s)
                                  q (- 1d0 s))
                            (let ((qq (/ fx-min fx-max)) (r (/ froot fx-max)))
                              (setf p (* s (- (* 2d0 mid qq (- qq r)) (* (- root x-min) (- r 1d0))))
                                    q (* (- qq 1d0) (- r 1d0) (- s 1d0)))))
                        (when (plusp p) (setf q (- q)))
                        (setf p (abs p))
                        (let ((min1 (- (* 3d0 mid q) (abs (* acc q))))
                              (min2 (abs (* e q))))
                          (if (< (* 2d0 p) (if (< min1 min2) min1 min2))
                              (setf e d d (/ p q))
                              (setf d mid e d))))
                      (setf d mid e d))
                  (setf x-min root fx-min froot)
                  (incf root (if (> (abs d) acc) d (if (>= mid 0d0) (abs acc) (- (abs acc)))))
                  (setf froot (float (funcall f root) 1d0))
                  (incf evaluations)))
          (signal-convergence-failure "Brent's method: no convergence in ~D evaluations near ~,12F"
                                      max-evaluations root))))))

(declaim (ftype (function ((or function symbol) real &key (:lo (or null real)) (:hi (or null real)) (:tolerance real) (:max-iterations (integer 0)) (:relative-step real))
                    (values real &optional integer))
          solve-newton))
(defun solve-newton (f x0 &key lo hi (tolerance 1d-12) (max-iterations 50)
                              (relative-step 1d-6))
  "Return the root of F by the method of Newton from X0. The function estimates F' with the
central difference (F(x + h) - F(x - h)) / 2h, with h = RELATIVE-STEP max(1, |x|). The
method converges quadratically near a simple root, with three evaluations of F for each
iteration. It stops when a step is shorter than TOLERANCE. Return (values root
iterations).

With LO and HI, F must change sign on [LO, HI]. Each iteration then tightens the
bracket. A step that would leave the bracket becomes a bisection step, so the solver
cannot diverge. Without LO and HI, a poor X0 can diverge. This signals
CONVERGENCE-FAILURE.

  (solve-newton (lambda (x) (- (* x x) 2)) 1d0) => 1.4142135623730951d0, 5"
  (let ((bracketed (and lo hi))
        (x (float x0 1d0))
        (flo 0d0))
    (declare (double-float x))
    (when bracketed
      (setf lo (float lo 1d0) hi (float hi 1d0)
            flo (float (funcall f lo) 1d0))
      (let ((fhi (funcall f hi)))
        (when (plusp (* flo fhi))
          (return-from solve-newton
            (call-with-bracket (constantly nil) f lo hi nil))))
      (unless (< lo x hi) (setf x (* 0.5d0 (+ lo hi)))))
    (iter (declare (declare-variables))
          (for (the fixnum iteration) from 1 to max-iterations)
          (let ((fx (float (funcall f x) 1d0)))
            (when (zerop fx)
              (return-from solve-newton (values x iteration)))
            (when bracketed
              (if (plusp (* fx flo)) (setf lo x flo fx) (setf hi x)))
            (let* ((h (* relative-step (max 1d0 (abs x))))
                   (slope (difference-quotient f x h :scheme :central))
                   ;; NIL when the step is undefined or overflows.
                   (next (handler-case (- x (/ fx slope))
                           (arithmetic-error () nil))))
              (when (and bracketed (or (null next) (not (< lo next hi))))
                (setf next (* 0.5d0 (+ lo hi))))
              (unless next
                (return-from solve-newton
                  (signal-convergence-failure "Newton's method: zero derivative at ~,12F" x)))
              (when (< (abs (- next x)) tolerance)
                (return-from solve-newton (values next iteration)))
              (setf x next))))
    (signal-convergence-failure
     "Newton's method: no convergence in ~D iterations from ~,12F" max-iterations x0)))
