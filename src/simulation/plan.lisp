;;;; plan.lisp --- simulation plan: legs, grid and normals
;;;;
;;;; A simulation plan holds the legs (each driving one market component), the
;;;; time grid of the dates and dividend ex-dates, the resolved processes and the
;;;; engine whose generators supply the normals.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

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

(declaim (ftype (function (t) (values list &optional)) simulation-underlyings))
(defun simulation-underlyings (simulation)
  "Return the legs that SIMULATION steps, in order: the underlying of a spot leg (NIL for a
market that is not composite), or the target (kind key) of any other leg. The STATES of
MARKET-AT-STATE follow this order."
  (map 'list #'simulation-leg-underlying (simulation-plan-legs simulation)))

(defun simulation-n-total (plan) (total-paths (simulation-plan-engine plan)))

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
the composite MARKET steps on (MAKE-STEPPER, INITIAL-STATE): by default the
kind's :VIEW (see DEFINE-MARKET-COMPONENT).")
  (:method ((kind t) market key)
    (a:if-let ((view (market-component-kind-view (find-market-component-kind kind))))
      (funcall view market key)
      (signal-invalid-argument 'target `(,kind ,key) 'list
                               (format nil "a target kind a process can be stepped on: ~{~S~^, ~}"
                                       (iter (for k in (market-component-kinds))
                                             (when (market-component-kind-view k)
                                               (collect (market-component-kind-name k)))))))))

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
    (let* ((process (if (typep process 'physical-process) (risk-neutral-process process) process))
           (accepted (method-keywords
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

(defun simulation-event-times (plan)
  "The market times of PLAN's events, a DOUBLE-VECTOR from 0 (the valuation
date) over SIMULATION-DATES."
  (let* ((market (simulation-plan-market plan))
         (events (simulation-plan-event-dates plan))
         (times (make-array (1+ (length events)) :element-type 'double-float :initial-element 0d0)))
    (iter (for d in-vector events) (for k from 1)
          (setf (aref times k) (float (market-time market d) 1d0)))
    times))

(defun simulation-event-steps (times)
  "TIMES[k] - TIMES[k-1] for each event k (0 at the valuation date)."
  (declare (type double-vector times))
  (let ((steps (make-array (length times) :element-type 'double-float :initial-element 0d0)))
    (iter (for k from 1 below (length times))
          (setf (aref steps k) (- (aref times k) (aref times (1- k)))))
    steps))

(defun copy-states-into (target states)
  "STATES (a vector of leg state vectors) copied into TARGET, or into fresh
vectors if TARGET is NIL; returns the copy."
  (if target
      (progn (iter (for v in-vector states) (for i from 0)
                   (replace (the double-vector (svref target i)) (the double-vector v)))
             target)
      (map 'simple-vector #'copy-seq states)))
