;;;; math/solvers.lisp --- one-dimensional root finding
;;;;
;;;; All solvers share one failure protocol: CONVERGENCE-FAILURE, signalled
;;;; with a USE-VALUE restart, and for bracketing solvers a
;;;; RETRY-WITH-BRACKET restart, so the caller chooses the recovery policy.
;;;;
;;;;   SOLVE-BISECTION  bracketed, linear convergence, cannot fail once bracketed
;;;;   SOLVE-BRENT      bracketed, superlinear; the default choice
;;;;   SOLVE-NEWTON     from a starting point, quadratic near the root, with
;;;;                    the derivative by central differences; optionally
;;;;                    safeguarded by a bracket

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
      (loop repeat 60
            while (plusp (* flo fhi))
            do (setf hi (* hi 2d0)
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

(defun solve-bisection (f lo hi &key expand-hi (epsilon 1d-12) (delta 1d-12))
  "Root of F on [LO, HI] by bisection (NU:ROOT-BISECTION): stops when the
bracket is narrower than DELTA or |F| is below EPSILON. See
CALL-WITH-BRACKET for EXPAND-HI and the restarts.

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
    (loop for iteration fixnum from 1 to max-iterations
          do (when (or (and (plusp fb) (plusp fc)) (and (minusp fb) (minusp fc)))
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

(defun solve-brent (f lo hi &key expand-hi (tolerance 1d-12) (max-iterations 1000))
  "Root of F on [LO, HI] by Brent's method, to within TOLERANCE in x.
Converges superlinearly for smooth F. Its worst case, on flat roots such as
(x - 1)^3, is about the square of the bisection count (~1700 iterations for
TOLERANCE 1e-12 on a unit bracket; 125 in that example), hence the default
MAX-ITERATIONS. Returns (values root iterations). See CALL-WITH-BRACKET for
EXPAND-HI and the restarts.

  (solve-brent (lambda (x) (- (* x x) 2)) 0d0 2d0) => 1.4142135623730951d0, 7"
  (call-with-bracket
   (lambda (lo hi flo fhi)
     (brent f (float lo 1d0) (float hi 1d0) (float flo 1d0) (float fhi 1d0)
            (float tolerance 1d0) max-iterations))
   f lo hi expand-hi))

;;; --------------------------------------------------------------------
;;; Newton with a finite-difference derivative
;;; --------------------------------------------------------------------

(defun solve-newton (f x0 &key lo hi (tolerance 1d-12) (max-iterations 50)
                              (relative-step 1d-6))
  "Root of F by Newton's method from X0, with F' estimated by the central
difference (F(x + h) - F(x - h)) / 2h, h = RELATIVE-STEP max(1, |x|).
Converges quadratically near a simple root, at three evaluations of F per
iteration. Stops when a step is shorter than TOLERANCE. Returns
(values root iterations).

With LO and HI, F must change sign on [LO, HI]; the bracket is then
tightened each iteration and any step that would leave it is replaced by a
bisection step, so the solver cannot diverge. Without them a poor X0 can
diverge, which signals CONVERGENCE-FAILURE.

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
    (loop for iteration fixnum from 1 to max-iterations
          do (let ((fx (float (funcall f x) 1d0)))
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
