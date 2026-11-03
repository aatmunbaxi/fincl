;;;; simulation.lisp --- market path simulation (tasks 29 and 32)
;;;;
;;;; Steps one or more underlyings along simulated paths with the process
;;;; protocol the Monte Carlo engine uses (MAKE-STEPPER on a MAKE-TIME-GRID
;;;; grid, the engine's normal generators), shows each path to user logic at
;;;; the dates it names, and collects per-path outputs and per-date
;;;; statistics. MAP-SIMULATED-PATHS is the functional core; SIMULATE writes
;;;; its visitor from clauses. A path is run to its end before its antithetic
;;;; partner, which replays the path's normals negated, so the logic of one
;;;; path never interleaves with another's. A leg drives one market component
;;;; (a target, in BUMP's factor syntax); :DERIVE functions set others from the
;;;; date and the legs' states.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; The plan: legs, grid, normals
;;; --------------------------------------------------------------------

(defstruct (simulation-leg (:constructor make-simulation-leg
                               (underlying process view &optional target options initial)))
  "One simulated market component: its designator UNDERLYING (the underlying
of a spot leg, NIL for a market that is not composite; the target (kind key)
of any other leg), its process (resolved at the horizon by the plan), the
view the process steps on, the TARGET it drives as (kind key) (NIL for a
market that is not composite), the OPTIONS passed to STRUCTURE-AT-STATE, and
the target's INITIAL component."
  (underlying nil :read-only t)
  (process nil :read-only t)
  (view nil :read-only t)
  (target nil :read-only t)
  (options nil :read-only t)
  (initial nil :read-only t))

(defstruct (simulation-plan (:constructor %make-simulation-plan))
  "Everything a run needs but the visitor: the market, the legs, the time grid
with the event dates (the dates the logic sees), the generator settings (as
a MONTE-CARLO-ENGINE, whose normal fillers the run uses), the Cholesky
factor of the factor correlation, and how markets are built from states."
  (market nil :read-only t)
  (legs nil :type simple-vector :read-only t)
  (grid nil :read-only t)
  (event-dates nil :type simple-vector :read-only t)          ; sorted, after the valuation date
  (event-of-grid nil :type (simple-array fixnum (*)) :read-only t) ; grid index -> event k (1-based), 0 if none
  (engine nil :read-only t)
  (sampling-plan nil :read-only t)
  (factors nil :type (simple-array fixnum (*)) :read-only t)  ; per leg
  (offsets nil :type (simple-array fixnum (*)) :read-only t)  ; first factor of each leg
  (cholesky nil :read-only t)
  (every-step nil :read-only t)
  (market-from-state nil :read-only t)
  (derive nil :type list :read-only t)                          ; ((target . function) ...)
  (fix #() :type simple-vector :read-only t))                   ; rate indexes fixed along the paths

(defun simulation-underlyings (simulation)
  "The legs SIMULATION steps, in order: the underlying of a spot leg (NIL for
a market that is not composite), the target (kind key) of any other.
MARKET-AT-STATE's STATES follow this order."
  (map 'list #'simulation-leg-underlying (simulation-plan-legs simulation)))

(defun simulation-n-total (plan) (total-paths (simulation-plan-engine plan)))

(defun simulation-dates (object)
  "The valuation date followed by the dates the path logic sees, in order:
the dates of a SIMULATION-PLAN or of a SIMULATION-RESULT. Per-date statistics
are vectors over these dates."
  (let ((plan (if (simulation-result-p object) (simulation-result-plan object) object)))
    `(,(valuation-date (simulation-plan-market plan))
      ,@(coerce (simulation-plan-event-dates plan) 'list))))

(defun leg-designator (target)
  "How a leg on TARGET, (kind key), is named: its underlying for a spot leg,
TARGET otherwise."
  (if (eq (first target) :spot) (second target) target))

(defun normalize-designator (designator)
  "A user's leg designator (an underlying, or a target) as LEG-DESIGNATOR names
the leg."
  (leg-designator (normalize-target designator)))

(defgeneric target-view (kind market key)
  (:documentation "The market a process driving the component (KIND KEY) of
the composite MARKET steps on (MAKE-STEPPER, INITIAL-STATE).")
  (:method ((kind t) market key)
    (declare (ignore market))
    (signal-invalid-argument 'target `(,kind ,key) 'list
                             (format nil "a target kind a process can be stepped on: ~{~S~^, ~}"
                                     '(:spot :vol :dividend :dividend-schedule :curve))))
  (:method ((kind (eql :curve)) market key) (make-instance 'curve-view :market market :curve-id key))
  (:method ((kind (eql :spot)) market key) (market-view market :underlying key))
  (:method ((kind (eql :vol)) market key) (market-view market :underlying key))
  (:method ((kind (eql :dividend)) market key) (market-view market :underlying key))
  (:method ((kind (eql :dividend-schedule)) market key) (market-view market :underlying key)))

(defun check-simulated-process (p)
  (check-input p stochastic-process "a stochastic process")
  (unless (steppable-p p)
    (signal-invalid-argument 'process p 'stochastic-process
                             (format nil "a process that can be simulated (~(~A~) has no stepper)"
                                     (class-name (class-of p)))))
  p)

(defun method-keywords (methods)
  "The keywords METHODS accept, or T if one allows other keys."
  (iter (for m in methods)
        (for lambda-list = (c2mop:method-lambda-list m))
        (when (member '&allow-other-keys lambda-list) (return t))
        (appending (iter (for k in (rest (member '&key lambda-list)))
                         (until (member k lambda-list-keywords))
                         (collect (let ((spec (if (consp k) (first k) k)))
                                    (if (consp spec) (first spec) (a:make-keyword spec))))))))

(defun check-leg-options (process target options market)
  "Signal unless OPTIONS is a plist whose keys STRUCTURE-AT-STATE accepts for
PROCESS driving TARGET."
  (unless (and (evenp (length options))
               (iter (for (k nil) on options by #'cddr) (always (keywordp k))))
    (signal-invalid-argument 'options options 'list
                             (format nil "leg options for ~S: a list of keywords and values" target)))
  (when options
    (let ((accepted (method-keywords
                     (compute-applicable-methods #'structure-at-state
                                                 `(,process ,target nil ,market ,(valuation-date market) nil)))))
      (unless (eq accepted t)
        (iter (for (k nil) on options by #'cddr)
              (unless (member k accepted)
                (signal-invalid-argument 'option k 'keyword
                                         (format nil "an option ~(~A~) takes for ~S: ~:[none~;~:*~{~S~^, ~}~]"
                                                 (class-name (class-of process)) target accepted))))))))

(defun make-target-leg (target process options market)
  "The leg of PROCESS driving TARGET, (kind key), of the composite MARKET."
  (check-simulated-process process)
  (let ((initial (market-component market target)))
    (unless (member (first target) (process-targets process))
      (signal-invalid-argument 'target target 'list
                               (format nil "a target ~(~A~) drives: kinds ~{~S~^, ~}"
                                       (class-name (class-of process)) (process-targets process))))
    (check-leg-options process target options market)
    (make-simulation-leg (leg-designator target) process (target-view (first target) market (second target))
                         target options initial)))

(defun default-target (process market)
  "The target PROCESS drives when given alone: its first kind of component,
for the composite MARKET's only underlying (the discount curve of its only
currency, for a curve)."
  (let ((kind (first (process-targets process))))
    (if (eq kind :curve)
        (let ((currencies (sorted-keys (market-discount-curve-for market))))
          (unless (= 1 (length currencies))
            (signal-invalid-argument 'process process 'stochastic-process
                                     (format nil "a leg naming its curve, ((:curve id) process): the market ~
discounts ~:[no currency~;~:*~{~S~^, ~}~]" currencies)))
          `(:curve ,(gethash (first currencies) (market-discount-curve-for market))))
        `(,kind ,(resolve-underlying market nil)))))

(defun simulation-legs-from (process market)
  "The legs for PROCESS: a process, or a list of legs (target process &rest
options), TARGET a target (kind key) or an underlying keyword (its spot)."
  (if (typep process 'stochastic-process)
      (cond ((typep market 'composite-market)
             (vector (make-target-leg (default-target process market) process nil market)))
            ((eq :spot (first (process-targets (check-simulated-process process))))
             (vector (make-simulation-leg nil process market)))
            (t (check-input market composite-market
                            (format nil "a composite market for a process driving ~S"
                                    (first (process-targets process))))))
      (progn
        (check-input process (and list (not null))
                     "a process, or a list of legs (target process &rest options)")
        (check-input market composite-market "a composite market for several legs")
        (let ((legs (map 'simple-vector
                         (lambda (leg)
                           (unless (and (consp leg) (<= 2 (length leg)))
                             (signal-invalid-argument 'process process 'list
                                                      "a list of legs (target process &rest options)"))
                           (destructuring-bind (target p &rest options) leg
                             (make-target-leg (normalize-target target) p options market)))
                         process)))
          (let ((targets (map 'list #'simulation-leg-target legs)))
            (unless (= (length targets) (length (remove-duplicates targets :test #'equal)))
              (signal-invalid-argument 'process process 'list
                                       (format nil "legs with distinct targets, not ~{~S~^, ~}" targets))))
          legs))))

(defun correlation-matrix (correlation legs factors offsets)
  "The full factor correlation, a column-major DOUBLE-VECTOR, from
CORRELATION: a list of (leg1 leg2 rho), each correlating the Brownian motions
driving two legs' element 0 at rho through their SPOT-LOADINGS (a leg named
by its target, or by an underlying for its spot), or a square 2-D array over
every factor in leg order."
  (let ((n (reduce #'+ factors))
        (names (map 'list #'simulation-leg-underlying legs)))
    (etypecase correlation
      (list
       (let ((m (make-array (* n n) :element-type 'double-float :initial-element 0d0)))
         (dotimes (i n) (setf (aref m (+ i (* i n))) 1d0))
         (dolist (entry correlation m)
           (unless (and (consp entry) (= 3 (length entry)) (realp (third entry))
                        (<= -1 (third entry) 1))
             (signal-invalid-argument 'correlation correlation 'list
                                      "a list of (leg leg rho), rho in [-1, 1], each leg a target or an underlying"))
           (destructuring-bind (u1 u2 rho) entry
             (let ((a (position (normalize-designator u1) names :test #'equal))
                   (b (position (normalize-designator u2) names :test #'equal)))
               (unless (and a b (/= a b))
                 (signal-invalid-argument 'correlation correlation 'list
                                          (format nil "pairs of distinct simulated legs: ~{~S~^, ~}" names)))
               (let ((la (spot-loadings (simulation-leg-process (aref legs a))))
                     (lb (spot-loadings (simulation-leg-process (aref legs b)))))
                 (dotimes (i (length la))
                   (dotimes (j (length lb))
                     (let ((x (* (float rho 1d0) (aref la i) (aref lb j)))
                           (fi (+ (aref offsets a) i)) (fj (+ (aref offsets b) j)))
                       (setf (aref m (+ fi (* fj n))) x
                             (aref m (+ fj (* fi n))) x))))))))))
      (array
       (unless (equal (array-dimensions correlation) `(,n ,n))
         (signal-invalid-argument 'correlation correlation 'array
                                  (format nil "an ~D x ~D matrix over every factor, in leg order" n n)))
       (let ((m (make-array (* n n) :element-type 'double-float)))
         (dotimes (j n m)
           (dotimes (i n)
             (unless (= (aref correlation i j) (aref correlation j i))
               (signal-invalid-argument 'correlation correlation 'array "a symmetric matrix"))
             (setf (aref m (+ i (* j n))) (float (aref correlation i j) 1d0)))))))))

(defun derive-entries (derive market legs)
  "DERIVE, a list of (target function), checked against MARKET and LEGS, as
((target . function) ...) with each target (kind key)."
  (check-input derive list "a list of (target function)")
  (iter (for entry in derive)
        (unless (and (consp entry) (= 2 (length entry)) (functionp (second entry)))
          (signal-invalid-argument 'derive derive 'list "a list of (target function)"))
        (for target = (normalize-target (first entry)))
        (unless (typep market 'composite-market)
          (signal-invalid-argument 'market market 'composite-market "a composite market for :DERIVE"))
        (market-component market target)
        (when (find target legs :key #'simulation-leg-target :test #'equal)
          (signal-invalid-argument 'derive target 'list
                                   (format nil "a target no leg drives (a leg already sets ~S)" target)))
        (collect (cons target (second entry)))))

(defun factor-owner (k legs offsets)
  "The underlying whose factors include factor K."
  (simulation-leg-underlying (aref legs (1- (or (position-if (lambda (o) (> o k)) offsets)
                                                (length offsets))))))

(defun make-simulation-plan (process market dates
                             &key (n-paths 10000) (seed 42) (rng :counter) (sampler :box-muller)
                                  antithetic n-steps max-dt correlation market-from-state
                                  every-step on-dates (n-chunks 64) derive fix)
  "The plan of a simulation; see MAP-SIMULATED-PATHS for the arguments.
ON-DATES are further dates the logic sees, beside DATES. DERIVE is a list of
(target function): FUNCTION, called as (function market date states) with
the market of DATE built so far and the vector of the legs' states, returns
TARGET's component in the markets built from states (MARKET-AT-STATE). FIX
is a list of rate indexes whose fixings on each date are taken from that
date's simulated forecast curve and carried by the markets of later dates."
  (check-input dates (and list (not null)) "a non-empty list of dates")
  (let ((today (valuation-date market)))
    (dolist (d (append dates on-dates))
      (check-input d date "a date")
      (unless (date< today d)
        (signal-invalid-argument 'dates d 'date (format nil "a date after the valuation date ~A" today)))))
  (let* ((legs (simulation-legs-from process market))
         (derive (derive-entries derive market legs))
         (events (-<> (append dates on-dates nil) ; a fresh list: SORT must not relink ON-DATES
                      (sort <> #'date<)
                      (remove-duplicates <> :test #'date=)
                      (coerce <> 'simple-vector)))
         (horizon (aref events (1- (length events))))
         (tau (float (market-time market horizon) 1d0))
         (grid (make-time-grid market
                               :dates `(,@(coerce events 'list)
                                        ,@(iter (for leg in-vector legs)
                                                (appending (dividend-dates (simulation-leg-view leg) tau))))
                               :n-steps n-steps :max-dt max-dt))
         (legs (map 'simple-vector
                    (lambda (leg)
                      (make-simulation-leg (simulation-leg-underlying leg)
                                           (resolve-process (simulation-leg-process leg)
                                                            (simulation-leg-view leg) tau)
                                           (simulation-leg-view leg)
                                           (simulation-leg-target leg)
                                           (simulation-leg-options leg)
                                           (simulation-leg-initial leg)))
                    legs))
         (factors (map '(simple-array fixnum (*)) (lambda (l) (process-factors (simulation-leg-process l))) legs))
         (offsets (let ((sum 0))
                    (map '(simple-array fixnum (*)) (lambda (f) (prog1 sum (incf sum f))) factors)))
         (engine (make-instance 'monte-carlo-engine
                                :process (simulation-leg-process (aref legs 0))
                                :n-paths n-paths :seed seed :rng rng :sampler sampler
                                :antithetic antithetic :n-chunks n-chunks))
         (n-base (if antithetic (/ (total-paths engine) 2) (total-paths engine)))
         (times (time-grid-times grid))
         (event-of-grid (make-array (length times) :element-type 'fixnum :initial-element 0)))
    (iter (for d in-vector events) (for k from 1)
          (setf (aref event-of-grid (grid-index-of grid d)) k))
    (%make-simulation-plan
     :market market :legs legs :grid grid :event-dates events :event-of-grid event-of-grid
     :engine engine
     :sampling-plan (make-sampling-plan engine times (reduce #'+ factors) n-base)
     :factors factors :offsets offsets
     :cholesky (when correlation
                 (let ((n (reduce #'+ factors)))
                   (multiple-value-bind (l k)
                       (cholesky-factor (correlation-matrix correlation legs factors offsets) n)
                     (or l
                         (signal-invalid-argument
                          'correlation correlation '(or list array)
                          (format nil "a positive-definite correlation; it fails at factor ~D (of ~S): ~
check the entries involving ~:*~S" k (factor-owner k legs offsets)))))))
     :every-step every-step
     :market-from-state market-from-state
     :derive derive
     :fix (progn (check-input fix list "a list of rate indexes")
                 (dolist (index fix (coerce fix 'simple-vector))
                   (check-input index interest-rate-index "a rate index (SOFR, EURIBOR ...)"))))))

;;; --------------------------------------------------------------------
;;; Running a plan
;;; --------------------------------------------------------------------

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

(defun map-simulated-paths (visitor process market dates
                            &key (n-paths 10000) (seed 42) (rng :counter) (sampler :box-muller)
                                 antithetic n-steps max-dt every-step (outputs 1) correlation derive)
  "Simulate N-PATHS paths of PROCESS from MARKET's valuation date to the last
of DATES and show each to VISITOR; returns the per-path outputs, an n-paths x
OUTPUTS column-major DOUBLE-VECTOR (output k of path i at i + k n-paths; with
one output, the per-path values in path order).

PROCESS is a stochastic process, or a list of legs (target process &rest
options) on the composite MARKET, each stepped on its own view; a target is
(:spot u), (:vol u), (:dividend u), (:dividend-schedule u) or (:curve id), a
bare keyword U meaning (:spot u), and the options go to STRUCTURE-AT-STATE.
CORRELATION correlates the legs' first factors, a list of (leg1 leg2 rho)
naming legs by target or underlying, or every factor, a square 2-D array in
leg order. DERIVE is as for MAKE-SIMULATION-PLAN; it sets components of
the markets MARKET-AT-STATE builds from the plan. The grid (MAKE-TIME-GRID) holds
DATES, the dividend ex-dates before the last date, and steps of at most the
span over N-STEPS (or MAX-DT); with neither, one step per gap. Normals come
from the Monte Carlo engine's generators (RNG, SEED, SAMPLER), so path i is
the engine's path i on the same grid and does not depend on N-PATHS or the
number of workers; under ANTITHETIC the second half of the paths are the
first half's partners.

VISITOR is called once per chunk of paths with the chunk index and returns
(values start step finish), closures called for one path at a time:
  (start path-index state)       at the valuation date; NIL stops the path
  (step path-index k state)      at the K-th date of DATES (1-based; with
                                 EVERY-STEP, at every grid index K); NIL stops
  (finish path-index state values)
                                 writes the path's OUTPUTS doubles into VALUES
STATE is the process's state vector (a vector of them, one per leg, for a
list of legs); it is reused, so copy what you keep.

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

;;; --------------------------------------------------------------------
;;; Markets from simulated states
;;; --------------------------------------------------------------------

(defgeneric market-at-state (market simulation date states)
  (:documentation "MARKET as of DATE built from the simulated states: STATES
is a vector of the legs' state vectors in the order of
(SIMULATION-UNDERLYINGS SIMULATION). The default, for a composite market,
ages MARKET to DATE in one copy of its tables, sets each leg's target to
STRUCTURE-AT-STATE of its state (a spot leg: element 0), then each :DERIVE
target to its function's value; every other component (curves, dividend
schedules, vol structures, fixings) is kept. Define a method on your market
class, or pass :MARKET-FROM-STATE to SIMULATE (a function of the same
arguments), to build markets otherwise.

  (market-at-state m plan #D\"2027-03-24\" states)")
  (:method ((m composite-market) simulation date states)
    (let ((new (copy-composite-market m :valuation-date date)))
      (iter (for leg in-vector (simulation-plan-legs simulation))
            (for state in-vector states)
            (for target = (simulation-leg-target leg))
            (set-market-component new target
                                  (apply #'structure-at-state (simulation-leg-process leg) target
                                         (simulation-leg-initial leg) new date state
                                         (simulation-leg-options leg))
                                  date))
      (iter (for (target . function) in (simulation-plan-derive simulation))
            (set-market-component new target (funcall function new date states) date))
      new))
  (:method ((m market) simulation date states)
    (declare (ignore simulation date states))
    (signal-invalid-argument 'market m 'composite-market
                             "a composite market, a MARKET-AT-STATE method for this market class, or :MARKET-FROM-STATE")))

(defun simulation-date (plan k)
  "The date of event K of PLAN (0: the valuation date)."
  (if (zerop k)
      (valuation-date (simulation-plan-market plan))
      (aref (simulation-plan-event-dates plan) (1- k))))

(defun simulation-market (plan k states &optional fixings)
  "The market at event K (0: the valuation date) for STATES, carrying the
path's FIXINGS (a fixing buffer, see PATH-FIXINGS!) of the dates before K."
  (let ((date (simulation-date plan k)))
    (a:if-let ((from-state (simulation-plan-market-from-state plan)))
      (funcall from-state (simulation-plan-market plan) plan date states)
      (let ((market (market-at-state (simulation-plan-market plan) plan date states)))
        (when (and fixings (plusp k))
          (add-path-fixings plan market k fixings))
        market))))

;;; --------------------------------------------------------------------
;;; Discounting and fixings along a path
;;; --------------------------------------------------------------------

(defgeneric state-discount (process state)
  (:documentation "The discount from the valuation date along a path where
PROCESS's state is STATE, exp(-int r ds), or NIL if PROCESS does not drive
a curve: the default reads a state component named :INTEGRAL.")
  (:method ((p stochastic-process) state)
    (a:when-let ((i (position :integral (process-state-names p))))
      (exp (- (aref state i))))))

(defun default-discount-target (market)
  "(:curve id) for the discount curve of the composite MARKET's only currency."
  (let ((currencies (sorted-keys (market-discount-curve-for market))))
    (unless (= 1 (length currencies))
      (signal-invalid-argument 'target nil 'list
                               (format nil "a curve to discount on, (path-discount '(:curve id)): the market ~
discounts ~:[no currency~;~:*~{~S~^, ~}~]" currencies)))
    `(:curve ,(gethash (first currencies) (market-discount-curve-for market)))))

(defun simulation-path-discount (plan k states target)
  "PATH-DISCOUNT in a clause: the discount from the valuation date to event K
on the curve TARGET ((:curve id), default the only currency's discount
curve), from the leg driving it if any, else from the initial market."
  (let ((market (simulation-plan-market plan)))
    (cond ((zerop k) 1d0)
          ((and (null target) (not (typep market 'composite-market)))
           (discount-factor market (simulation-date plan k)))
          (t
           (let* ((target (if target (normalize-target target) (default-discount-target market)))
                  (legs (simulation-plan-legs plan))
                  (i (position target legs :key #'simulation-leg-target :test #'equal)))
             (unless (eq (first target) :curve)
               (signal-invalid-argument 'target target 'list "a curve to discount on, (:curve id)"))
             (if i
                 (or (state-discount (simulation-leg-process (aref legs i)) (svref states i))
                     (signal-invalid-argument 'target target 'list
                                              (format nil "a curve whose leg's process discounts along the path ~
(~(~A~) has no :INTEGRAL state)" (class-name (class-of (simulation-leg-process (aref legs i)))))))
                 (market-curve-discount market (second target) (simulation-date plan k))))))))

(defun make-fixing-buffer (plan)
  "A buffer for one path's fixings of PLAN's :FIX indexes, one per (index,
event), or NIL without :FIX."
  (let ((n (length (simulation-plan-fix plan))))
    (when (plusp n)
      (make-array (* n (1+ (length (simulation-plan-event-dates plan))))
                  :element-type 'double-float :initial-element 0d0))))

(defun fixing-market (plan k states)
  "A market on event K's date holding only the forecast curves of PLAN's :FIX
indexes, as the leg driving each (or the :DERIVE form, or the initial
market) gives it on this path."
  (let* ((initial (simulation-plan-market plan))
         (date (simulation-date plan k))
         (curves (make-hash-table :test 'equal))
         (full nil))
    (iter (for index in-vector (simulation-plan-fix plan))
          (for id = (index-forecast-curve index))
          (for target = `(:curve ,id))
          (unless (gethash id curves)
            (setf (gethash id curves)
                  (let ((i (position target (simulation-plan-legs plan) :key #'simulation-leg-target :test #'equal)))
                    (cond (i (let ((leg (aref (simulation-plan-legs plan) i)))
                               (apply #'structure-at-state (simulation-leg-process leg) target
                                      (simulation-leg-initial leg) initial date (svref states i)
                                      (simulation-leg-options leg))))
                          ((assoc target (simulation-plan-derive plan) :test #'equal)
                           (market-component (or full (setf full (market-at-state initial plan date states)))
                                             target))
                          (t (find-yield-curve initial id)))))))
    (make-instance 'composite-market
                   :valuation-date date :day-counter (market-day-counter initial) :underlyings '()
                   :spots (make-hash-table :test 'equal) :yield-curves curves
                   :dividend-curves (make-hash-table :test 'equal)
                   :vol-structures (make-hash-table :test 'equal)
                   :underlying-currency (make-hash-table :test 'equal)
                   :discount-curve-for (make-hash-table :test 'equal))))

(defun path-fixings! (plan k states buffer)
  "Store in BUFFER each :FIX index's fixing on event K's date (NaN on a day
the index does not fix), from that date's forecast curve on this path."
  (when buffer
    (let* ((date (simulation-date plan k))
           (fix (simulation-plan-fix plan))
           (market nil))
      (iter (for index in-vector fix)
            (for i from 0)
            (setf (aref buffer (+ i (* k (length fix))))
                  (if (business-day-p (index-fixing-calendar index) date)
                      (if (zerop k)
                          (index-fixing index (simulation-plan-market plan) date)
                          (index-fixing index (or market (setf market (fixing-market plan k states))) date))
                      (quiet-nan)))))))

(defun add-path-fixings (plan market k buffer)
  "Add to MARKET, built for event K and not yet shared, the path's fixings in
BUFFER on the dates before K, on top of the initial market's."
  (let ((fix (simulation-plan-fix plan))
        (tables (market-fixings market)))
    (iter (for index in-vector fix)
          (for i from 0)
          (for id = (index-name index))
          (let ((table (make-hash-table :test 'equalp)))
            (a:when-let ((old (gethash id tables)))
              (maphash (lambda (d r) (setf (gethash d table) r)) old))
            (iter (for j below k)
                  (for rate = (aref buffer (+ i (* j (length fix)))))
                  (unless (sb-ext:float-nan-p rate)
                    (setf (gethash (simulation-date plan j) table) rate)))
            (setf (gethash id tables) table)))))

;;; --------------------------------------------------------------------
;;; Streamed statistics and stored values
;;; --------------------------------------------------------------------

(defstruct (date-statistics (:constructor make-date-statistics
                                (n &aux (count (make-array n :element-type 'fixnum :initial-element 0))
                                        (mean (make-array n :element-type 'double-float :initial-element 0d0))
                                        (m2 (make-array n :element-type 'double-float :initial-element 0d0))
                                        (minimum (make-array n :element-type 'double-float
                                                               :initial-element most-positive-double-float))
                                        (maximum (make-array n :element-type 'double-float
                                                               :initial-element most-negative-double-float)))))
  "Count, mean, sum of squared deviations, min and max of one recorded name
at each simulation date, updated one value at a time (Welford)."
  (count nil :type (simple-array fixnum (*)))
  (mean nil :type double-vector)
  (m2 nil :type double-vector)
  (minimum nil :type double-vector)
  (maximum nil :type double-vector))

(defun add-statistic (s k x)
  (declare (type date-statistics s) (fixnum k) (double-float x))
  (let* ((n (1+ (aref (date-statistics-count s) k)))
         (d (- x (aref (date-statistics-mean s) k))))
    (setf (aref (date-statistics-count s) k) n)
    (incf (aref (date-statistics-mean s) k) (/ d n))
    (incf (aref (date-statistics-m2 s) k) (* d (- x (aref (date-statistics-mean s) k))))
    (setf (aref (date-statistics-minimum s) k) (min x (aref (date-statistics-minimum s) k))
          (aref (date-statistics-maximum s) k) (max x (aref (date-statistics-maximum s) k)))))

(defun merge-statistics (a b)
  "A's statistics with B's merged in (Chan et al.), date by date; A is updated."
  (dotimes (k (length (date-statistics-count a)) a)
    (let ((na (aref (date-statistics-count a) k)) (nb (aref (date-statistics-count b) k)))
      (when (plusp nb)
        (let* ((n (+ na nb))
               (d (- (aref (date-statistics-mean b) k) (aref (date-statistics-mean a) k))))
          (setf (aref (date-statistics-mean a) k) (+ (aref (date-statistics-mean a) k) (* d (/ nb n)))
                (aref (date-statistics-m2 a) k) (+ (aref (date-statistics-m2 a) k)
                                                   (aref (date-statistics-m2 b) k)
                                                   (* d d (/ (* na nb) n)))
                (aref (date-statistics-count a) k) n
                (aref (date-statistics-minimum a) k) (min (aref (date-statistics-minimum a) k)
                                                          (aref (date-statistics-minimum b) k))
                (aref (date-statistics-maximum a) k) (max (aref (date-statistics-maximum a) k)
                                                          (aref (date-statistics-maximum b) k))))))))

(defun quiet-nan () (sb-kernel:make-double-float -524288 0))

(defstruct (simulation-store (:constructor %make-simulation-store))
  "What a run keeps beyond its outputs (:KEEP), shared by the chunks: every
array is allocated before the paths run and each path writes only its own
slots, except the quantile tables, made on first use under LOCK."
  (n-total 0 :type fixnum)
  (n-dates 0 :type fixnum)
  (quantiles nil)                 ; T to keep recorded values per (path, date)
  (quantile-tables (make-hash-table :test 'equal))
  (lock (bt:make-lock "simulation store"))
  (kept-paths 0 :type fixnum)     ; the first K paths' states and held values
  (kept-states nil)               ; path -> date -> vector of leg state copies
  (held nil)                      ; path -> date -> plist of held values
  (all-states nil))               ; T: every path's states (into KEPT-STATES)

(defun parse-keep (keep)
  "(values quantiles paths states) from the :KEEP designator(s)."
  (let ((items (if (or (keywordp keep) (and (consp keep) (eq (first keep) :paths))) `(,keep) keep))
        (quantiles nil) (paths 0) (states nil))
    (dolist (item items)
      (match item
        (:quantiles (setf quantiles t))
        (:states (setf states t))
        ((list :paths (guard k (and (integerp k) (>= k 0)))) (setf paths k))
        (_ (signal-invalid-argument 'keep keep 'list
                                    ":QUANTILES, :STATES, (:PATHS k), or a list of them"))))
    (values quantiles paths states)))

(defun keep-form (keep)
  "The form building the :KEEP designator at run time: the keywords are
literal, K in (:PATHS k) is evaluated.

  (keep-form '(:quantiles (:paths n))) => (list :quantiles (list :paths n))

A variable (or NIL) is evaluated as it stands."
  (flet ((item (i)
           (if (and (consp i) (eq (first i) :paths) (= 2 (length i)))
               `(list :paths ,(second i))
               `',i)))
    (cond ((or (keywordp keep) (and (consp keep) (eq (first keep) :paths))) (item keep))
          ((consp keep) `(list ,@(mapcar #'item keep)))
          (t keep))))

(defun make-simulation-store (plan keep)
  (multiple-value-bind (quantiles paths states) (parse-keep keep)
    (let* ((n-total (simulation-n-total plan))
           (n-dates (1+ (length (simulation-plan-event-dates plan))))
           (kept (if states n-total (min paths n-total))))
      (when states
        (let ((doubles (* n-total n-dates
                          (iter (for l in-vector (simulation-plan-legs plan))
                                (sum (process-state-size (simulation-leg-process l)))))))
          (when (> doubles 100000000)
            (warn 'simple-style-warning
                  :format-control "SIMULATE :KEEP :STATES stores ~:D doubles (n-paths x dates x state size)."
                  :format-arguments `(,doubles)))))
      (%make-simulation-store
       :n-total n-total :n-dates n-dates :quantiles quantiles
       :kept-paths kept :all-states states
       :kept-states (when (plusp kept) (make-array kept :initial-element nil))
       :held (when (plusp kept) (make-array kept :initial-element nil))))))

(defun store-path-date (store path k states held)
  "Keep PATH's STATES (copied) and HELD values at date K if PATH is kept."
  (declare (fixnum path k))
  (when (< path (simulation-store-kept-paths store))
    (let ((dates (or (svref (simulation-store-kept-states store) path)
                     (setf (svref (simulation-store-kept-states store) path)
                           (make-array (simulation-store-n-dates store) :initial-element nil)))))
      (setf (svref dates k) (map 'simple-vector #'copy-seq states)))
    (when held
      (let ((dates (or (svref (simulation-store-held store) path)
                       (setf (svref (simulation-store-held store) path)
                             (make-array (simulation-store-n-dates store) :initial-element nil)))))
        (setf (svref dates k) held)))))

(defun keeping-path-p (store path)
  (< path (simulation-store-kept-paths store)))

(defun record-value (statistics store name path k value)
  "Add VALUE to the statistic NAME at date K, in the chunk's STATISTICS."
  (let ((x (if (realp value)
               (float value 1d0)
               (signal-invalid-argument 'value value 'real
                                        (format nil "a real to record under ~S" name)))))
    (add-statistic (or (gethash name statistics)
                       (setf (gethash name statistics)
                             (make-date-statistics (simulation-store-n-dates store))))
                   k x)
    (when (simulation-store-quantiles store)
      (let ((table (or (gethash name (simulation-store-quantile-tables store))
                       (bt:with-lock-held ((simulation-store-lock store))
                         (or (gethash name (simulation-store-quantile-tables store))
                             (setf (gethash name (simulation-store-quantile-tables store))
                                   (make-array (* (simulation-store-n-total store)
                                                  (simulation-store-n-dates store))
                                               :element-type 'double-float :initial-element (quiet-nan))))))))
        (setf (aref table (+ path (* k (simulation-store-n-total store)))) x)))
    value))

;;; --------------------------------------------------------------------
;;; Results
;;; --------------------------------------------------------------------

(defstruct (simulation-result (:constructor %make-simulation-result))
  "What SIMULATE collected: per-path outputs by name, per-date statistics by
name, and what :KEEP stored. Read it with SIMULATION-VALUES,
SIMULATION-MEAN, SIMULATION-STANDARD-ERROR, SIMULATION-QUANTILE,
SIMULATION-SUMMARY, SIMULATION-STATISTIC, SIMULATION-HELD and PATH-MARKETS."
  (plan nil :read-only t)
  (output-names '() :read-only t)
  (outputs nil :read-only t)
  (statistics nil :read-only t)
  (store nil :read-only t))

(defun simulation-output-names (result)
  "The names of RESULT's outputs, in the order of SIMULATE's :FINAL."
  (simulation-result-output-names result))

(defun output-column (result name)
  (let ((names (simulation-result-output-names result)))
    (cond ((null names)
           (signal-invalid-argument 'name name 'keyword "a run with outputs (it has no :FINAL)"))
          ((and (null name) (null (rest names))) 0)
          ((position name names))
          (t (signal-invalid-argument 'name name 'keyword
                                      (format nil "one of the outputs ~{~S~^, ~}" names))))))

(defun simulation-values (result &optional name)
  "The per-path values of RESULT's output NAME (optional with one output), a
DOUBLE-VECTOR in path order; under antithetics the second half holds the
partners of the first."
  (let* ((n (simulation-n-total (simulation-result-plan result)))
         (start (* n (output-column result name))))
    (subseq (simulation-result-outputs result) start (+ start n))))

(defun simulation-summary (result vector)
  "(values mean standard-error) of VECTOR, a per-path vector of RESULT's
length in path order, such as an output or a quantity computed from several
(a difference, a ratio, an indicator). Antithetic partners are averaged
first, and under :SOBOL the randomizations, so the standard error is that of
the estimator. Outputs share their paths, so the standard error of a
difference is that of the paired differences.

  (simulation-summary r (map 'double-vector #'- (simulation-values r :a)
                                                (simulation-values r :b)))"
  (let* ((plan (simulation-result-plan result))
         (engine (simulation-plan-engine plan))
         (samples (coerce vector 'double-vector)))
    (unless (= (length samples) (simulation-n-total plan))
      (signal-invalid-argument 'vector vector 'sequence
                               (format nil "~D per-path values" (simulation-n-total plan))))
    (summarize-samples engine (if (antithetic engine) (pair-means samples) samples))))

(defun simulation-mean (result &optional name)
  "The mean of RESULT's output NAME (see SIMULATION-SUMMARY)."
  (values (simulation-summary result (simulation-values result name))))

(defun simulation-standard-error (result &optional name)
  "The standard error of the mean of RESULT's output NAME (see SIMULATION-SUMMARY)."
  (nth-value 1 (simulation-summary result (simulation-values result name))))

(defun sample-quantile (values p)
  "The P-quantile of VALUES (NaNs dropped), linear between order statistics
(Hyndman and Fan's type 7)."
  (check-input p (real 0 1) "a probability in [0, 1]")
  (let* ((sorted (sort (remove-if #'sb-ext:float-nan-p (copy-seq (coerce values 'double-vector))) #'<))
         (n (length sorted)))
    (if (zerop n)
        (quiet-nan)
        (let* ((h (* (1- n) (float p 1d0)))
               (lo (floor h)))
          (if (>= lo (1- n))
              (aref sorted (1- n))
              (+ (aref sorted lo) (* (- h lo) (- (aref sorted (1+ lo)) (aref sorted lo)))))))))

(defun simulation-quantile (result name p)
  "The P-quantile of RESULT's output NAME over every path (NAME may be NIL with
one output)."
  (sample-quantile (simulation-values result name) p))

(defun recorded-names (statistics)
  "The names in STATISTICS, keywords or lists, in printed order."
  (sort (iter (for (key nil) in-hashtable statistics) (collect key)) #'string< :key #'prin1-to-string))

(defun simulation-statistic (result name statistic &optional p)
  "A DOUBLE-VECTOR over (SIMULATION-DATES RESULT) of STATISTIC of the values
recorded under NAME: :MEAN, :STD-DEV, :MIN, :MAX, :COUNT, or :QUANTILE with P
(needs :KEEP :QUANTILES). A date where nothing was recorded holds NaN (0 for
:COUNT).

  (simulation-statistic r :hedge-value :mean)"
  (let ((s (or (gethash name (simulation-result-statistics result))
               (signal-invalid-argument 'name name t
                                        (format nil "a recorded name: ~{~S~^, ~}"
                                                (recorded-names (simulation-result-statistics result))))))
        (store (simulation-result-store result)))
    (flet ((per-date (f)
             (iter (for k below (length (date-statistics-count s)))
                   (collect-double (if (zerop (aref (date-statistics-count s) k))
                                       (quiet-nan)
                                       (funcall f k))
                                   into v length (length (date-statistics-count s)))
                   (finally (return v)))))
      (ecase statistic
        (:count (map 'double-vector (lambda (c) (float c 1d0)) (date-statistics-count s)))
        (:mean (per-date (lambda (k) (aref (date-statistics-mean s) k))))
        (:std-dev (per-date (lambda (k) (let ((n (aref (date-statistics-count s) k)))
                                          (if (< n 2) 0d0 (sqrt (/ (aref (date-statistics-m2 s) k) (1- n))))))))
        (:min (per-date (lambda (k) (aref (date-statistics-minimum s) k))))
        (:max (per-date (lambda (k) (aref (date-statistics-maximum s) k))))
        (:quantile
         (let ((table (gethash name (simulation-store-quantile-tables store)))
               (n (simulation-store-n-total store)))
           (unless table
             (signal-invalid-argument 'statistic statistic 'keyword
                                      "a statistic of a run with :KEEP :QUANTILES"))
           (per-date (lambda (k) (sample-quantile (subseq table (* k n) (* (1+ k) n)) p)))))))))

(defun kept-dates (result path what)
  (let* ((store (simulation-result-store result))
         (table (if (eq what :states) (simulation-store-kept-states store) (simulation-store-held store))))
    (or (and table (< -1 path (length table)) (svref table path))
        (signal-invalid-argument 'path path 'integer
                                 (format nil "a stored path: 0 .. ~D (use :KEEP (:PATHS k) or :STATES)"
                                         (1- (simulation-store-kept-paths store)))))))

(defun simulation-held (result path name)
  "The values of the :HOLD or :PORTFOLIO variable NAME (a keyword, the
variable's name) on stored PATH at each simulation date, a vector."
  (map 'simple-vector (lambda (plist) (getf plist name)) (kept-dates result path :held)))

(defun path-markets (result path)
  "((date . market) ...) for stored PATH (:KEEP (:PATHS k) or :STATES), each
market built from the path's state on its date as during the run, at every
simulation date from the valuation date on.

  (path-markets r 0)"
  (let* ((plan (simulation-result-plan result))
         (fixings (make-fixing-buffer plan)))
    (iter (for states in-vector (kept-dates result path :states))
          (for k from 0)
          (for date in (simulation-dates plan))
          (when states
            (path-fixings! plan k states fixings)
            (collect (cons date (simulation-market plan k states fixings)))))))

(defun simulation-recorded-names (result)
  "The names RESULT recorded with RECORD, keywords or lists, in printed order."
  (recorded-names (simulation-result-statistics result)))

(defun simulation-kept-paths (result)
  "How many paths RESULT stored the states of (:KEEP (:PATHS k) or :STATES):
paths 0 .. n-1 can be read with SIMULATION-PATH-STATE."
  (let ((store (simulation-result-store result)))
    (if (simulation-store-kept-states store) (simulation-store-kept-paths store) 0)))

(defun simulation-quantiles-p (result name)
  "True if RESULT kept the values recorded under NAME for quantiles
(:KEEP :QUANTILES), so that SIMULATION-STATISTIC can give :QUANTILE."
  (and (gethash name (simulation-store-quantile-tables (simulation-result-store result))) t))

(defun simulation-path-state (result path name &optional underlying)
  "State component NAME (see SIMULATION-STATE-NAMES) of stored PATH at each of
(SIMULATION-DATES RESULT), a DOUBLE-VECTOR with NaN at a date the path had
already stopped.

  (simulation-path-state r 0 :spot)"
  (let ((plan (simulation-result-plan result))
        (dates (kept-dates result path :states)))
    (multiple-value-bind (leg index) (simulation-state-location plan name underlying)
      (iter (for states in-vector dates)
            (collect-double (if states
                                (aref (the double-vector (svref states leg)) index)
                                (quiet-nan))
                            into v length (length dates))
            (finally (return v))))))

(defun simulation-state-names (process)
  "The names a :STATE clause may use for PROCESS, a process or a list of legs
(target process &rest options): the process's state names, or (component
leg) lists for legs, a spot leg named by its underlying and any other by its
target.

  (simulation-state-names (make-heston)) => (:SPOT :VARIANCE)
  (simulation-state-names `((:aapl ,(make-heston)) (:msft ,(make-gbm))))
  => ((:SPOT :AAPL) (:VARIANCE :AAPL) (:SPOT :MSFT))"
  (if (typep process 'stochastic-process)
      (process-state-names process)
      (iter (for leg in process)
            (for designator = (normalize-designator (first leg)))
            (appending (mapcar (lambda (name) `(,name ,designator)) (process-state-names (second leg)))))))

(defun simulation-state-location (plan name underlying)
  "(values leg index) of the state component NAME of the leg UNDERLYING (an
underlying for its spot leg, or a target; NIL: the only leg) in PLAN; a wrong
name lists the valid ones."
  (let* ((legs (simulation-plan-legs plan))
         (underlying (and underlying (normalize-designator underlying)))
         (valid (if (= 1 (length legs))
                    (process-state-names (simulation-leg-process (aref legs 0)))
                    (iter (for l in-vector legs)
                          (appending (mapcar (lambda (n) `(,n ,(simulation-leg-underlying l)))
                                             (process-state-names (simulation-leg-process l)))))))
         (leg (cond ((and (null underlying) (= 1 (length legs))) 0)
                    (t (position underlying legs :key #'simulation-leg-underlying :test #'equal)))))
    (flet ((refuse ()
             (signal-invalid-argument 'state `(,name ,@(when underlying `(,underlying))) 'keyword
                                      (format nil "a state name of this simulation: ~{~S~^, ~} (see SIMULATION-STATE-NAMES)"
                                              valid))))
      (unless leg (refuse))
      (let ((index (position name (process-state-names (simulation-leg-process (aref legs leg))))))
        (unless index (refuse))
        (values leg index)))))

(defmethod print-object ((r simulation-result) stream)
  (print-unreadable-object (r stream :type t)
    (let ((plan (simulation-result-plan r)))
      (format stream "~{~(~A~)~^, ~}, ~D paths, ~D dates"
              (map 'list (lambda (l)
                           (format nil "~@[~S ~]~A" (simulation-leg-underlying l)
                                   (class-name (class-of (simulation-leg-process l)))))
                   (simulation-plan-legs plan))
              (simulation-n-total plan) (length (simulation-plan-event-dates plan)))
      (dolist (name (simulation-result-output-names r))
        (multiple-value-bind (mean se) (simulation-summary r (simulation-values r name))
          (format stream ", ~S ~,4F +- ~,4F" name mean se)))
      (let ((recorded (recorded-names (simulation-result-statistics r))))
        (when recorded (format stream ", recorded ~{~S~^ ~}" recorded))))))

(defun finish-simulation (plan store output-names outputs chunk-statistics)
  "The SIMULATION-RESULT of a run: the chunks' statistics merged in chunk order."
  (let ((statistics (make-hash-table :test 'equal)))
    (dolist (chunk chunk-statistics)
      (when chunk
        (dolist (name (recorded-names chunk))
          (let ((s (gethash name chunk)))
            (a:if-let ((total (gethash name statistics)))
              (merge-statistics total s)
              (setf (gethash name statistics)
                    (merge-statistics (make-date-statistics (length (date-statistics-count s))) s)))))))
    (%make-simulation-result :plan plan :output-names output-names :outputs outputs
                             :statistics statistics :store store)))

(defun live-book-greeks (book engine market keys)
  "The Greeks KEYS of BOOK on MARKET, leaving out options that expire on or
before MARKET's valuation date (their Greeks are undefined there; they
contribute 0)."
  (let ((today (valuation-date market)))
    (greeks (apply #'make-portfolio (portfolio-name book)
                   (remove-if (lambda (p)
                                (let ((i (position-instrument p)))
                                  (and (typep i 'option)
                                       (not (date< today (expiry (option-exercise i)))))))
                              (portfolio-positions book)))
            engine market :which keys)))

(defun output-value (name path value)
  (if (realp value)
      (float value 1d0)
      (signal-invalid-argument 'output value 'real
                               (format nil "a real for the output ~S (path ~D)" name path))))

;;; --------------------------------------------------------------------
;;; The SIMULATE macro
;;; --------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *simulate-clauses* '(:accumulate :hold :portfolio :state :derive :start :every :on :final))

  (defun simulate-clause-error (clause reason)
    (signal-invalid-argument 'clause clause 'list (format nil "a SIMULATE clause: ~A" reason)))

  (defun clause-binding-list-p (x)
    "True if X is the optional (market) list of :START: a list of at most one
symbol that names no function."
    (and (listp x) (<= (length x) 1)
         (every (lambda (s) (and (symbolp s) s (not (keywordp s)) (not (fboundp s)))) x)))

  (defun parse-final (clause)
    "The (name form) entries of a :FINAL clause."
    (let ((entries (if (and (= 2 (length clause))
                            (not (and (consp (second clause)) (keywordp (first (second clause)))
                                      (= 2 (length (second clause))))))
                       `((:value ,(second clause)))
                       (rest clause))))
      (dolist (e entries)
        (unless (and (consp e) (= 2 (length e)) (keywordp (first e)))
          (simulate-clause-error clause "(:final (name form) ...) with keyword names, or (:final form)")))
      (let ((names (mapcar #'first entries)))
        (unless (= (length names) (length (remove-duplicates names)))
          (simulate-clause-error clause "distinct output names")))
      entries))

  (defun literal-target-p (x)
    "True if X is written as a target: an underlying keyword or (kind key)."
    (or (keywordp x) (and (consp x) (keywordp (first x)))))

  (defun literal-legs-form (process)
    "PROCESS as a form: a literal list of legs (target process-form &rest
options), the target written as data, becomes a form making the list with
the process and option forms evaluated; anything else is evaluated as it is."
    (if (and (consp process) (consp (first process)) (literal-target-p (first (first process))))
        `(list ,@(mapcar (lambda (leg)
                           (unless (and (consp leg) (literal-target-p (first leg)) (<= 2 (length leg)))
                             (signal-invalid-argument 'process process 'list
                                                      "a list of legs (target process &rest options)"))
                           `(list ',(first leg) ,@(rest leg)))
                         process))
        process))

  (defun literal-correlation-form (correlation)
    "CORRELATION as a form: a literal list of (leg leg rho-form), the legs
written as data, becomes a form with each rho evaluated; anything else (a
quoted list, a variable, an array form) is evaluated as it is."
    (if (and (consp correlation) (consp (first correlation))
             (literal-target-p (first (first correlation))))
        `(list ,@(mapcar (lambda (entry)
                           (if (and (consp entry) (= 3 (length entry)))
                               `(list ',(first entry) ',(second entry) ,(third entry))
                               `',entry))
                         correlation))
        correlation))

  (defun calls-named-p (tree names)
    "True if TREE contains a form whose operator's name is one of NAMES."
    (and (consp tree)
         (or (and (symbolp (first tree)) (member (symbol-name (first tree)) names :test #'string=))
             (some (lambda (x) (calls-named-p x names)) (if (a:proper-list-p tree) tree (list (car tree) (cdr tree)))))))

  (defun parse-portfolio (clause)
    "(values var init record engine) of a :PORTFOLIO clause."
    (destructuring-bind (var &rest rest) (rest clause)
      (unless (and (symbolp var) var (not (keywordp var)))
        (simulate-clause-error clause "(:portfolio var [init] &key record engine)"))
      (let* ((init (if (and rest (not (keywordp (first rest)))) (pop rest) nil))
             (options rest))
        (unless (evenp (length options))
          (simulate-clause-error clause "(:portfolio var [init] &key record engine)"))
        (values var
                (or init `(make-portfolio ,(string-downcase (symbol-name var))))
                (getf options :record)
                (getf options :engine :auto))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun special-variable-name-p (symbol)
    "True if SYMBOL is declaimed special, so a clause cannot make it a symbol macro."
    (eq :special (sb-int:info :variable :kind symbol))))

(defmacro simulate ((process market dates &rest options) &body clauses)
  "Simulate paths of PROCESS from MARKET's valuation date through DATES, run
the clauses' logic on every path, and return a SIMULATION-RESULT. DATES is an
ascending list of dates after the valuation date; the last is the end of every
path.

PROCESS is a process, or a literal list of legs (target process &rest
options), each leg's process driving one market component, its target:
(:spot u) the spot of underlying U, (:vol u), (:dividend u),
(:dividend-schedule u) or (:curve id). A bare keyword U is (:spot u), so
((:spx (make-gbm)) (:vix (make-heston))) simulates two spots. The leg options
go to STRUCTURE-AT-STATE. A process given alone drives the market's only
underlying's spot. OPTIONS:
  :n-paths 10000     paths simulated.
  :seed 42           the random seed.
  :rng :counter      :counter (a path's draws do not depend on N-PATHS, chunks
                     or workers), :sobol, or a random-state designator.
  :antithetic NIL    pair each path with its mirror image; statistics and
                     SIMULATION-SUMMARY then work on pair means.
  :n-steps :max-dt   fill the gaps between DATES with extra steps (default one
                     step per gap); the clauses still run only on DATES.
  :correlation       correlations between legs, a list of (leg leg rho) naming
                     legs by target or underlying, e.g. ((:spx :ndx 0.6d0)),
                     or a factor matrix, as in MAP-SIMULATED-PATHS.
  :keep              what to store for later: :QUANTILES (per-date quantiles of
                     every recorded name), (:PATHS k) (states and held values of
                     the first k paths; k is evaluated), :STATES, or a list of
                     these.
  :market-from-state a function building a date's market from the simulated
                     states, replacing MARKET-AT-STATE.
  :fix               a list of rate indexes, e.g. (list (euribor #T\"6M\")). On
                     each date the index fixes, its fixing is read from that
                     date's simulated curve, and the markets of later dates
                     hold it, so instruments priced there use the path's own
                     past fixings. Pricing needs every fixing date among DATES
                     (FIXING-DATES lists an instrument's).

Optional clauses. :ACCUMULATE and :HOLD may repeat and :FINAL appears
once; the logic clauses run in the order written.
  (:accumulate (var init) ...)
                     per-path DOUBLE-FLOAT variables, reset to INIT on every
                     path (an integer INIT is coerced; assigning a non-double
                     is an error).
  (:hold (var init) ...)
                     per-path variables of any type; INIT is evaluated afresh
                     on every path, so (:hold (v (make-array 5))) gives each
                     path its own array. Readable later with SIMULATION-HELD
                     for the kept paths.
  (:portfolio var [init] &key record engine)
                     VAR is a per-path PORTFOLIO (INIT, default empty), changed
                     with ADJUST-POSITION and priced with ENGINE. RECORD lists
                     :VALUE and Greek keys; each is recorded every date under
                     the name (var-keyword key), e.g. (:book :delta). Without
                     :FINAL the one output is :VALUE.
  (:state var name [leg])
                     VAR is bound to the double-float state component NAME
                     (:spot, :variance, ... see SIMULATION-STATE-NAMES) of the
                     leg (an underlying, or a target) at the current date.
  (:derive target ([market [date]]) form ...)
                     sets TARGET's component in every market built from the
                     paths to the value of the forms, computed from DATE,
                     MARKET (that date's market, with the legs' components
                     and earlier :DERIVE targets already set) and the :STATE
                     variables. The forms cannot see :ACCUMULATE or :HOLD
                     variables, RECORD or STOP, so the component depends only
                     on the date and the simulated states. TARGET may not be
                     a leg's. For example, a flat implied vol that
                     follows the spot S (a :STATE variable):
                       (:derive (:vol :spx) ()
                         (make-instance 'flat-black-vol :vol (* 0.002d0 s)))
  (:start [(market)] form ...)
                     at the valuation date, before the first step. The optional
                     (market) binds that date's market. RECORD here goes to the
                     valuation date, the first of SIMULATION-DATES.
  (:every (date [market]) form ...)
                     at every date of DATES. DATE is bound to the date, MARKET
                     to the market on it built from the simulated states.
  (:on date-list (i [market]) form ...)
                     at the dates of DATE-LIST (a subset of DATES), I being the
                     date's position in the list.
  (:final (name form) ...)
                     one clause: named per-path outputs, evaluated after the
                     last date (or after STOP) from the final state; NAME is a
                     keyword. (:final form) is (:final (:value form)). MARKET
                     in the forms is the market of the path's last date.
MARKET is built only if the
clause uses it, and a global special variable may serve as its name. Clause
code runs serially: pricing inside it starts no parallel work. An error in
a clause is re-signalled in the caller.

Inside :START, :EVERY and :ON bodies these local operators are available,
matched by name in any package:
  (record name value)  adds the real VALUE to the statistic NAME at the current
                       date. NAME is a keyword. The statistic at a date is
                       over all the values recorded under NAME at it. The
                       streamed statistics are the mean, standard deviation, minimum,
                       maximum and count (quantiles also with :KEEP :QUANTILES);
                       individual values are not stored. It returns VALUE.
  (stop)               ends the path: its later dates run no clauses and
                       record nothing, and :FINAL still runs.
  (path-discount [curve])
                       the discount from the valuation date to the current
                       date along this path, on CURVE ((:curve id); default the
                       discount curve of the market's only currency). When a
                       leg simulates that curve it is exp(-integral of the
                       short rate) on the path; otherwise it is the initial
                       market's discount factor. Also available in :FINAL.

Results of simulation in SIMULATION-RESULT:
  :final outputs   SIMULATION-VALUES (one number per path), SIMULATION-MEAN,
                   SIMULATION-STANDARD-ERROR, SIMULATION-QUANTILE. Differences
                   of outputs are paired estimates on common random numbers.
  recorded names   SIMULATION-STATISTIC (a vector over SIMULATION-DATES),
                   SIMULATION-RECORDED-NAMES.
  kept paths       SIMULATION-PATH-STATE, SIMULATION-HELD, PATH-MARKETS.
  any vector       SIMULATION-SUMMARY gives the mean and standard error.
The function underneath is MAP-SIMULATED-PATHS.

  ;; A short CALL delta-hedged on DATES, which end on its EXPIRY. The 
  ;; balance is recorded each date (read it with SIMULATION-STATISTIC) and the
  ;; terminal P&L is a :final output (read it with SIMULATION-VALUES).
  (simulate ((make-gbm) market dates :n-paths 20000)
    (:accumulate (shares 0d0) (cash 0d0) (premium 0d0))
    (:state s :spot)
    (:start (market)
      (setf premium (price call :black-scholes market))
      (incf cash premium))
    (:every (date market)
      (when (date< date expiry)
        (let ((delta (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
          (decf cash (* (- delta shares) s))
          (setf shares delta)))
      (record :balance (- (+ cash (* shares s)) (price call :black-scholes market))))
    (:final (:hedged (+ cash (* shares s) (- (payoff-value (option-payoff call) s))))
            (:unhedged (- premium (payoff-value (option-payoff call) s)))))

  ;; The exposure of a SWAP when the curve it prices on follows Hull-White:
  ;; its INDEX fixes along each path on the simulated curve, and
  ;; PATH-DISCOUNT discounts along the path.
  (simulate ((make-hull-white :a 0.03d0 :sigma 0.008d0) market
             (sort (append dates (fixing-dates swap market)) #'date<)
             :n-paths 5000 :fix (list index) :keep :quantiles)
    (:every (date market)
      (record :exposure (max 0d0 (swap-npv swap market)))
      (record :discounted (* (path-discount) (swap-npv swap market))))
    (:final (:pv (* (path-discount) (swap-npv swap market)))))"
  (let* ((record-name (intern "RECORD")) (stop-name (intern "STOP"))
         (discount-name (intern "PATH-DISCOUNT")) (fixings (gensym "FIXINGS"))
         (plan (gensym "PLAN")) (store (gensym "STORE")) (stats (gensym "STATS"))
         (path (gensym "PATH")) (k (gensym "K")) (states (gensym "STATES"))
         (alive (gensym "ALIVE")) (cached (gensym "MARKET")) (market-fn (gensym "MARKET-AT"))
         (values-vector (gensym "VALUES"))
         (keep (getf options :keep))
         (plan-options (let ((rest (a:remove-from-plist options :keep)))
                         (if (getf rest :correlation)
                             `(:correlation ,(literal-correlation-form (getf rest :correlation))
                               ,@(a:remove-from-plist rest :correlation))
                             rest)))
         accumulators holds portfolios state-vars derives starts everys ons finals)
    (dolist (clause clauses)
      (unless (and (consp clause) (member (first clause) *simulate-clauses*))
        (simulate-clause-error clause (format nil "one of ~{~S~^, ~}" *simulate-clauses*)))
      (ecase (first clause)
        (:accumulate (dolist (b (rest clause))
                       (unless (and (consp b) (symbolp (first b)) (= 2 (length b)))
                         (simulate-clause-error clause "(:accumulate (var init) ...)"))
                       (push b accumulators)))
        (:hold (dolist (b (rest clause))
                 (unless (and (consp b) (symbolp (first b)) (= 2 (length b)))
                   (simulate-clause-error clause "(:hold (var init) ...)"))
                 (push b holds)))
        (:portfolio (push (multiple-value-list (parse-portfolio clause)) portfolios))
        (:state (unless (<= 3 (length clause) 4)
                  (simulate-clause-error clause "(:state var name [underlying])"))
                (push `(,(second clause) ,(third clause)
                        ,(if (consp (fourth clause)) `',(fourth clause) (fourth clause))
                        ,(gensym "LEG") ,(gensym "INDEX"))
                      state-vars))
        (:derive (destructuring-bind (&optional target binding &rest body) (rest clause)
                   (unless (and (literal-target-p target) (listp binding) (<= (length binding) 2)
                                (every (lambda (s) (and (symbolp s) s (not (keywordp s)))) binding))
                     (simulate-clause-error clause "(:derive target ([market [date]]) form ...)"))
                   (when (calls-named-p body '("RECORD" "STOP"))
                     (simulate-clause-error clause "a :DERIVE body without RECORD or STOP (it runs per market, not per path)"))
                   (push `(,target ,(or (first binding) (gensym "MARKET")) ,(or (second binding) (gensym "DATE"))
                           ,body)
                         derives)))
        (:start (destructuring-bind (&rest body) (rest clause)
                  (let ((binding (when (clause-binding-list-p (first body)) (pop body))))
                    (push `(,(first binding) ,body) starts))))
        (:every (destructuring-bind (binding &rest body) (rest clause)
                  (unless (and (listp binding) (<= 1 (length binding) 2) (every #'symbolp binding))
                    (simulate-clause-error clause "(:every (date [market]) form ...)"))
                  (push `(:every ,(first binding) ,(second binding) ,body) everys)))
        (:on (destructuring-bind (dates-form binding &rest body) (rest clause)
               (unless (and (listp binding) (<= 1 (length binding) 2) (every #'symbolp binding))
                 (simulate-clause-error clause "(:on date-list (i [market]) form ...)"))
               (push `(:on ,(first binding) ,(second binding) ,body ,dates-form
                           ,(gensym "ON-DATES") ,(gensym "POSITIONS"))
                     everys)
               (push (first everys) ons)))
        (:final (when finals (simulate-clause-error clause "one :FINAL clause"))
                (setf finals (parse-final clause)))))
    (when (rest portfolios)
      (simulate-clause-error (first portfolios) "one :PORTFOLIO clause"))
    (setf accumulators (nreverse accumulators) holds (nreverse holds)
          state-vars (nreverse state-vars) derives (nreverse derives) starts (nreverse starts)
          everys (nreverse everys) ons (nreverse ons))
    (let* ((portfolio (first portfolios))
           (book (first portfolio))
           (book-key (when book (a:make-keyword (symbol-name book))))
           (outputs (or finals
                        (when book `((:value (price ,book ,(fourth portfolio) (,market-fn)))))))
           (held-vars `(,@(mapcar #'first holds) ,@(when book `(,book)))))
      (labels ((with-states (body)
                 `(let ,(iter (for (var nil nil leg index) in state-vars)
                              (collect `(,var (aref (the double-vector (svref ,states ,leg)) ,index))))
                    (declare (ignorable ,@(mapcar #'first state-vars))
                             ,@(when state-vars `((double-float ,@(mapcar #'first state-vars)))))
                    ,@body))
               (with-market (market-var body)
                 (cond ((null market-var) `(progn ,@body))
                       ;; SYMBOL-MACROLET cannot bind a special variable (a
                       ;; global MARKET from DEFVAR): rebind it, when used.
                       ((special-variable-name-p market-var)
                        (if (member market-var (a:flatten body))
                            `(let ((,market-var (,market-fn))) ,@body)
                            `(progn ,@body)))
                       (t `(symbol-macrolet ((,market-var (,market-fn))) ,@body))))
               (record-book ()
                 (when book
                   `((unless (typep ,book 'portfolio)
                       (signal-invalid-argument ',book ,book 'portfolio
                                                "a PORTFOLIO (set the book with ADJUST-POSITION or MAKE-PORTFOLIO)"))
                     ,@(destructuring-bind (var init record engine) portfolio
                         (declare (ignore var init))
                         (when record
                           (let ((greek-keys (remove :value record)))
                             `((let ((m (,market-fn)))
                                 ,@(when (member :value record)
                                     `((record-value ,stats ,store '(,book-key :value) ,path ,k
                                                     (price ,book ,engine m))))
                                 ,@(when greek-keys
                                     `((let ((g (live-book-greeks ,book ,engine m ',greek-keys)))
                                         ,@(iter (for key in greek-keys)
                                                 (collect `(record-value ,stats ,store '(,book-key ,key)
                                                                         ,path ,k (getf g ,key)))))))))))))))
               (keep-hook ()
                 `(when (keeping-path-p ,store ,path)
                    (store-path-date ,store ,path ,k ,states
                                     (list ,@(iter (for var in held-vars)
                                                   (appending `(,(a:make-keyword (symbol-name var)) ,var))))))))
        ;; The state locations are bound before the plan, which the :DERIVE
        ;; functions go into, and set from it before any path runs.
        `(let* (,@(iter (for (nil nil nil nil dates-form on-dates) in ons)
                        (collect `(,on-dates ,dates-form)))
                ,@(iter (for (nil nil nil leg index) in state-vars)
                        (appending `((,leg 0) (,index 0))))
                (,plan (make-simulation-plan
                        ,(literal-legs-form process) ,market ,dates
                        :on-dates (append ,@(mapcar #'sixth ons))
                        ,@(when derives
                            `(:derive
                              (list ,@(iter (for (target market-var date-var body) in derives)
                                            (collect
                                                `(list ',target
                                                       (lambda (,market-var ,date-var ,states)
                                                         (declare (ignorable ,market-var ,date-var ,states)
                                                                  (simple-vector ,states))
                                                         ,(with-states body))))))))
                        ,@plan-options))
                (,store (make-simulation-store ,plan ,(keep-form keep)))
                ,@(iter (for (nil nil nil nil nil on-dates positions) in ons)
                        (collect `(,positions (on-positions ,plan ,on-dates)))))
           (declare (fixnum ,@(iter (for (nil nil nil leg index) in state-vars) (appending `(,leg ,index)))))
           (progn
             ,@(iter (for (nil name underlying leg index) in state-vars)
                     (collect `(multiple-value-setq (,leg ,index)
                                 (simulation-state-location ,plan ,name ,underlying)))))
             (multiple-value-bind (outputs chunks)
                 (run-simulation
                  ,plan
                  (lambda (chunk)
                    (declare (ignore chunk))
                    (let ((,stats (make-hash-table :test 'equal))
                          (,path 0) (,k 0) (,states nil) (,alive t) (,cached nil)
                          (,fixings (make-fixing-buffer ,plan))
                          ,@(iter (for (var) in accumulators) (collect `(,var 0d0)))
                          ,@(mapcar (lambda (v) `(,v nil)) held-vars))
                      (declare (fixnum ,path ,k)
                               ,@(when accumulators `((double-float ,@(mapcar #'first accumulators))))
                               (ignorable ,stats ,alive))
                      (flet ((,market-fn ()
                               (or ,cached (setf ,cached (simulation-market ,plan ,k ,states ,fixings))))
                             (,discount-name (&optional target)
                               (simulation-path-discount ,plan ,k ,states target)))
                        (declare (ignorable #',market-fn #',discount-name))
                        (macrolet ((,record-name (name value)
                                     `(record-value ,',stats ,',store ,name ,',path ,',k ,value))
                                   (,stop-name () `(setf ,',alive nil)))
                          (values
                           (lambda (p s)
                             (setf ,path p ,k 0 ,states s ,cached nil ,alive t)
                             (path-fixings! ,plan 0 s ,fixings)
                             ,@(iter (for (var init) in accumulators) (collect `(setf ,var (float ,init 1d0))))
                             ,@(iter (for (var init) in holds) (collect `(setf ,var ,init)))
                             ,@(when book `((setf ,book ,(second portfolio))))
                             ,@(iter (for (market-var body) in starts)
                                     (collect (with-states `(,(with-market market-var body)))))
                             ,@(record-book)
                             ,(keep-hook)
                             ,alive)
                           (lambda (p kk s)
                             (declare (ignore p))
                             (setf ,k kk ,states s ,cached nil)
                             (path-fixings! ,plan kk s ,fixings)
                             ,@(iter (for clause in everys)
                                     (collect
                                         (ecase (first clause)
                                           (:every
                                            (destructuring-bind (date-var market-var body) (rest clause)
                                              `(let ((,date-var (aref (simulation-plan-event-dates ,plan) (1- ,k))))
                                                 (declare (ignorable ,date-var))
                                                 ,(with-states `(,(with-market market-var body))))))
                                           (:on
                                            (destructuring-bind (index-var market-var body dates-form on-dates positions)
                                                (rest clause)
                                              (declare (ignore dates-form on-dates))
                                              `(let ((,index-var (aref ,positions ,k)))
                                                 (when (>= ,index-var 0)
                                                   ,(with-states `(,(with-market market-var body))))))))))
                             ,@(record-book)
                             ,(keep-hook)
                             ,alive)
                           (lambda (p s ,values-vector)
                             (declare (ignore p) (type double-vector ,values-vector) (ignorable ,values-vector))
                             (setf ,states s)
                             ,(with-states
                                  `(,(with-market
                                      (intern "MARKET")
                                      (iter (for (name form) in outputs)
                                            (for o from 0)
                                            (collect `(setf (aref ,values-vector ,o)
                                                            (output-value ,name ,path ,form))))))))
                           (lambda () ,stats))))))
                  :outputs ,(length outputs))
               (finish-simulation ,plan ,store ',(mapcar #'first outputs)
                                  ,(if outputs 'outputs nil) chunks)))))))

(defun on-positions (plan dates)
  "A fixnum vector over PLAN's event indices (1-based; 0 unused) holding each
event date's position in DATES, or -1."
  (let ((events (simulation-plan-event-dates plan))
        (positions (make-array (1+ (length (simulation-plan-event-dates plan)))
                               :element-type 'fixnum :initial-element -1)))
    (iter (for d in dates) (for i from 0)
          (setf (aref positions (1+ (position d events :test #'date=))) i))
    positions))
