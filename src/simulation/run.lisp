;;;; run.lisp --- running a plan
;;;;
;;;; RUN-SIMULATION steps a plan's paths through a visitor; MAP-SIMULATED-PATHS is
;;;; its functional interface.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defun run-simulation (plan visitor &key (outputs 1) (state-form :legs))
  "Run PLAN's paths through VISITOR (see MAP-SIMULATED-PATHS); STATE-FORM :LEGS
shows the visitor a vector of the legs' state vectors, :ARRAY the one leg's
state vector. Returns (values outputs chunk-results): the n-paths x OUTPUTS
column-major output vector, and each chunk's DONE value in chunk order."
  (let* ((engine (simulation-plan-engine plan))
         (legs (simulation-plan-legs plan))
         (n-legs (length legs))
         (times (time-grid-times (simulation-plan-grid plan)))
         (n-steps (1- (length times)))
         (factors (simulation-plan-factors plan))
         (offsets (simulation-plan-offsets plan))
         (n-factors (reduce #'+ factors))
         (cholesky (simulation-plan-cholesky plan))
         (event-of-grid (simulation-plan-event-of-grid plan))
         (every-step (simulation-plan-every-step plan))
         (anti (antithetic engine))
         (n-total (total-paths engine))
         (n-base (if anti (/ n-total 2) n-total))
         (steppers (map 'simple-vector (lambda (l)
                                         (make-stepper (simulation-leg-process l) (simulation-leg-view l) times))
                        legs))
         (templates (map 'simple-vector (lambda (l)
                                          (initial-state (simulation-leg-process l) (simulation-leg-view l)))
                         legs))
         (result (make-array (* n-total outputs) :element-type 'double-float :initial-element 0d0))
         (ranges (chunk-ranges n-base (n-chunks engine))))
    (declare (fixnum n-legs n-steps n-factors n-total n-base outputs)
             (type (simple-array fixnum (*)) factors offsets event-of-grid))
    (labels ((run-chunk (range chunk)
             ;; An error in the path logic is returned, not signalled in the
             ;; worker, and re-signalled below in the caller's thread.
             (handler-case (cons :ok (run-chunk-paths range chunk))
               (error (c) (cons :error c))))
           (run-chunk-paths (range chunk)
             (let ((*serial-pricing* t)
                   (*missing-fixing-hint* "In SIMULATE, add the fixing date to DATES (FIXING-DATES gives an instrument's) and its index to :FIX."))
               (multiple-value-bind (start step finish done) (funcall visitor chunk)
                 (declare (type function start step finish))
                 (multiple-value-bind (fill start-path)
                     (make-normal-filler engine chunk (simulation-plan-sampling-plan plan))
                   (declare (type function fill start-path))
                   (let* ((states (map 'simple-vector #'copy-seq templates))
                          (shown (if (eq state-form :array) (svref states 0) states))
                          (draws (make-array (max 1 (* n-steps n-factors)) :element-type 'double-float))
                          (row (make-array n-factors :element-type 'double-float))
                          (mixed (make-array n-factors :element-type 'double-float))
                          (leg-normals (map 'simple-vector (lambda (f) (make-array f :element-type 'double-float))
                                            factors))
                          (values-buffer (make-array (max 1 outputs) :element-type 'double-float)))
                     (declare (type double-vector draws row mixed))
                     (labels ((advance (j sign)
                                ;; Step every leg from TIMES[j-1] to TIMES[j] on the
                                ;; drawn normals of row j-1, times SIGN.
                                (declare (fixnum j) (double-float sign))
                                (let ((base (* (1- j) n-factors)))
                                  (dotimes (k n-factors)
                                    (setf (aref row k) (* sign (aref draws (+ base k))))))
                                (let ((source row))
                                  (when cholesky
                                    (let ((l cholesky))
                                      (declare (type double-vector l))
                                      (dotimes (i n-factors)
                                        (let ((z 0d0))
                                          (declare (double-float z))
                                          (dotimes (k (1+ i))
                                            (incf z (* (aref l (+ i (* k n-factors))) (aref row k))))
                                          (setf (aref mixed i) z))))
                                    (setf source mixed))
                                  (dotimes (l n-legs)
                                    (let ((target (svref leg-normals l)))
                                      (declare (type double-vector target source))
                                      (replace target source :start2 (aref offsets l))
                                      (funcall (the function (svref steppers l))
                                               (svref states l) target (1- j))))))
                              (run-path (path sign draw)
                                ;; One path: DRAW fills the normals as it goes (a base
                                ;; path), otherwise they are replayed (a partner).
                                (dotimes (l n-legs) (replace (svref states l) (svref templates l)))
                                (let ((alive (funcall start path shown)))
                                  (iter (for j from 1 to n-steps)
                                        (when draw
                                          (fill-normals fill row)
                                          (replace draws row :start1 (* (1- j) n-factors)))
                                        (when alive
                                          (advance j sign)
                                          (let ((k (aref event-of-grid j)))
                                            (when (or every-step (plusp k))
                                              (setf alive (funcall step path (if every-step j k) shown)))))
                                        (while (or alive (and draw anti))))
                                  (funcall finish path shown values-buffer)
                                  (dotimes (o outputs)
                                    (setf (aref result (+ path (* o n-total))) (aref values-buffer o))))))
                       (iter (for i from (car range) below (cdr range))
                             (funcall start-path i)
                             (run-path i 1d0 t)
                             (when anti (run-path (+ i n-base) -1d0 nil)))
                       (and done (funcall done)))))))))
      (let ((chunks (with-pricing-kernel ()
                      (parallel-map 'list #'run-chunk ranges (a:iota (length ranges))))))
        (a:when-let ((failed (find :error chunks :key #'car)))
          (error (cdr failed)))
        (values result (mapcar #'cdr chunks))))))

(declaim (ftype (function (t t t t &key (:n-paths t) (:seed t) (:rng t) (:sampler t) (:antithetic t) (:n-steps t) (:max-dt t) (:every-step t) (:outputs t) (:correlation t) (:derive t))
                    (values double-vector &optional))
          map-simulated-paths))
(defun map-simulated-paths (visitor process market dates
                            &key (n-paths 10000) (seed 42) (rng :counter) (sampler :box-muller)
                                 antithetic n-steps max-dt every-step (outputs 1) correlation derive)
  "Simulate N-PATHS paths of PROCESS from the valuation date of MARKET to the last of DATES.
Show each path to VISITOR. Return the per-path outputs as an n-paths x OUTPUTS column-major
DOUBLE-VECTOR (output k of path i is at i + k n-paths). With one output, the result is the
per-path values in path order.

PROCESS is a stochastic process, or a list of legs (target process &rest options) on the
composite MARKET. Each leg steps on its own view. A target is (:spot u), (:vol u),
(:dividend u), (:dividend-schedule u) or (:curve id). A bare keyword U means (:spot u). The
options go to STRUCTURE-AT-STATE. CORRELATION correlates the first factors of the legs. It
is a list of (leg1 leg2 rho) that names legs by target or underlying. It can also correlate
every factor, with a square 2-D array in leg order. DERIVE is as for MAKE-SIMULATION-PLAN.
It sets components of the markets that MARKET-AT-STATE builds from the plan. The grid
(MAKE-TIME-GRID) holds DATES, the dividend ex-dates before the last date, and steps of at
most the span over N-STEPS (or MAX-DT). With neither, there is one step for each gap.
Normals come from the generators of the Monte Carlo engine (RNG, SEED, SAMPLER). Path i is
then the path i of the engine on the same grid. It does not depend on N-PATHS or on the
number of workers. Under ANTITHETIC, the second half of the paths are the partners of the
first half.

The function calls VISITOR once for each chunk of paths, with the chunk index. VISITOR
returns (values start step finish), closures that the function calls for one path at a
time:
  (start path-index state)       At the valuation date. NIL stops the path
  (step path-index k state)      At the K-th date of DATES (1-based. With EVERY-STEP, at
                                 every grid index K). NIL stops the path
  (finish path-index state values)
                                 Writes the OUTPUTS doubles of the path into VALUES
STATE is the state vector of the process (for a list of legs, a vector of state vectors,
one for each leg). The function reuses it, so copy what you keep.

  (map-simulated-paths (lambda (chunk)
                         (declare (ignore chunk))
                         (values (constantly t) (constantly t)
                                 (lambda (i state values)
                                   (declare (ignore i))
                                   (setf (aref values 0) (aref state 0)))))
                       (make-gbm) m (list #D\"2027-09-24\") :n-paths 1000)"
  (let ((plan (make-simulation-plan process market dates
                                    :n-paths n-paths :seed seed :rng rng :sampler sampler
                                    :antithetic antithetic :n-steps n-steps :max-dt max-dt
                                    :every-step every-step :correlation correlation
                                    :derive derive)))
    (values (run-simulation plan visitor :outputs outputs
                                         :state-form (if (typep process 'stochastic-process) :array :legs)))))
