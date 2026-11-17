;;;; results.lisp --- simulation results
;;;;
;;;; The SIMULATION-RESULT structure and its accessors: outputs, summaries,
;;;; statistics, quantiles, kept paths and held values.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

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
  (let* ((sorted (sort (remove-if #'nan-p (copy-seq (coerce values 'double-vector))) #'<))
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

(defun simulation-path-record (result path name)
  "The values stored PATH (:KEEP (:PATHS k) or :STATES) recorded under NAME at
each of (SIMULATION-DATES RESULT), a DOUBLE-VECTOR with NaN where the path
recorded nothing (before its first record of NAME, or after STOP); the last
value recorded on a date. A book's records are named by their lists.

  (simulation-path-record r 0 :balance)
  (simulation-path-record r 0 '(:book :value))"
  (let* ((store (simulation-result-store result))
         (records (simulation-store-records store)))
    (unless (gethash name (simulation-result-statistics result))
      (signal-invalid-argument 'name name t
                               (format nil "a recorded name: ~{~S~^, ~}"
                                       (recorded-names (simulation-result-statistics result)))))
    (unless (and records (< -1 path (length records)))
      (signal-invalid-argument 'path path 'integer
                               (format nil "a stored path: 0 .. ~D (use :KEEP (:PATHS k) or :STATES)"
                                       (1- (simulation-store-kept-paths store)))))
    (a:if-let ((values (and (svref records path) (gethash name (svref records path)))))
      (copy-seq values)
      (make-array (simulation-store-n-dates store) :element-type 'double-float
                                                   :initial-element (quiet-nan)))))

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

(defun simulation-measure (simulation)
  "The measure SIMULATION's paths were stepped under: :PHYSICAL when a leg's
process is a PHYSICAL-PROCESS, else :RISK-NEUTRAL. SIMULATION is a
SIMULATION-RESULT or a SIMULATION-PLAN. Discounted means of a :PHYSICAL
result are expectations under the physical measure, not prices.

  (simulation-measure (simulate ((make-gbm) m dates) ...)) => :RISK-NEUTRAL"
  (let ((plan (if (simulation-result-p simulation) (simulation-result-plan simulation) simulation)))
    (if (some (lambda (leg) (typep (simulation-leg-process leg) 'physical-process))
              (simulation-plan-legs plan))
        :physical
        :risk-neutral)))

(defmethod print-object ((r simulation-result) stream)
  (print-unreadable-object (r stream :type t)
    (let ((plan (simulation-result-plan r)))
      (format stream "~{~(~A~)~^, ~}, ~(~A~) measure, ~D paths, ~D dates"
              (map 'list (lambda (l)
                           (let ((process (simulation-leg-process l)))
                             (format nil "~@[~S ~]~A" (simulation-leg-underlying l)
                                     (class-name (class-of (if (typep process 'physical-process)
                                                               (risk-neutral-process process)
                                                               process))))))
                   (simulation-plan-legs plan))
              (simulation-measure plan)
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

(defun simulation-dates (object)
  "The valuation date followed by the dates the path logic sees, in order:
the dates of a SIMULATION-PLAN or of a SIMULATION-RESULT. Per-date statistics
are vectors over these dates."
  (let ((plan (if (simulation-result-p object) (simulation-result-plan object) object)))
    `(,(valuation-date (simulation-plan-market plan))
      ,@(coerce (simulation-plan-event-dates plan) 'list))))
