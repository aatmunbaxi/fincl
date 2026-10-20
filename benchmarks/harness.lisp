;;;; harness.lisp --- DEFINE-BENCHMARK and the measurement protocol
;;;;
;;;; The protocol, the same as harness.hpp's: wall time on CLOCK_MONOTONIC;
;;;; three untimed warm-up calls (one when a call takes over 2 s); a sample
;;;; is a batch of calls lasting at least 20 ms, divided by the batch size;
;;;; 30 samples (10 when quick), fewer for slow calls so that a series lasts
;;;; about 5 s (SAMPLE-COUNT). Lisp also runs a full GC before each series and
;;;; counts the bytes consed per call.

(in-package #:fincl/benchmarks)

(define-condition benchmark-error (fincl-error)
  ((id :initarg :id :reader benchmark-error-id)
   (message :initarg :message :reader benchmark-error-message))
  (:report (lambda (c s)
             (format s "Benchmark ~S: ~A" (benchmark-error-id c) (benchmark-error-message c))))
  (:documentation "A benchmark whose two sides cannot be compared: the C++
program echoed different inputs, or printed something unreadable."))

;;; --------------------------------------------------------------------
;;; Definitions
;;; --------------------------------------------------------------------

(defstruct (benchmark (:constructor %make-benchmark))
  "One job. MAKE builds the inputs (timed as setup) and returns the thunk
that computes the answer (the timed region): (values value std-error
details), DETAILS a plist printed below the group's table; and, as a second
value, a thunk giving a reference value, or NIL."
  (id "" :type string)
  (group nil :type keyword)
  (title "" :type string)
  (engine "" :type string)
  (inputs '() :type list)
  (work '() :type list)
  (kind :deterministic :type (member :deterministic :method-error :statistical :none))
  (tolerance nil :type (or null real))
  (notes nil :type (or null string))
  (parallel nil :type boolean)
  (batch-size :auto :type (or (eql :auto) (integer 1)))
  (make nil :type (or null function)))

(defvar *benchmarks* '()
  "Every defined benchmark, in definition order.")

(defun register-benchmark (benchmark)
  (setf *benchmarks*
        `(,@(remove (benchmark-id benchmark) *benchmarks* :key #'benchmark-id :test #'string=)
          ,benchmark))
  benchmark)

(defmacro define-benchmark (id (&key group (title "") (engine "") inputs work (kind :deterministic)
                                     tolerance notes parallel (batch-size :auto) reference)
                            bindings &body body)
  "Define the benchmark ID (a string, the key shared with the C++ program).
BINDINGS are LET* bindings building the inputs: their evaluation is the
setup, timed once. BODY is the timed region and returns (values value
std-error details).

  INPUTS  plist of the problem's numbers; the C++ program must echo the same
  WORK    plist of the work parameters both sides share (steps, paths, grid)
  KIND    :DETERMINISTIC (agree to TOLERANCE, default 1e-8 relative),
          :METHOD-ERROR (within TOLERANCE, the methods' known difference),
          :STATISTICAL (within 3 combined standard errors), or :NONE (no
          QuantLib counterpart)
  NOTES   what differs between the two sides' configurations
  PARALLEL  also run fincl with every worker (Monte Carlo, calibration)
  REFERENCE a form, in the scope of BINDINGS, giving an accurate value
            printed with the details; evaluated after the setup is timed

  (define-benchmark \"european-call-black\"
      (:group :options-pricing :engine \"black-scholes\" :inputs (equity-inputs))
      ((m (equity-market)) (o (vanilla :call)) (e (make-engine :black-scholes)))
    (price o e m))"
  `(register-benchmark
    (%make-benchmark :id ,id :group ,group :title ,title :engine ,engine
                     :inputs ,inputs :work ,work :kind ,kind :tolerance ,tolerance
                     :notes ,notes :parallel ,parallel :batch-size ,batch-size
                     :make (lambda ()
                             (let* ,bindings
                               (values (lambda () ,@body)
                                       ,(when reference `(lambda () ,reference))))))))

(defun group-benchmarks (group)
  (remove group *benchmarks* :key #'benchmark-group :test-not #'eq))

;;; --------------------------------------------------------------------
;;; The protocol
;;; --------------------------------------------------------------------

(defparameter *sample-seconds* 0.02d0
  "A sample's batch lasts at least this long.")

(defparameter *slow-call-seconds* 0.5d0
  "Calls slower than this get one warm-up call instead of three.")

(defparameter *series-seconds* 5d0
  "A slow call's series lasts about this long.")

(defun sample-count (single quick)
  "Samples for a call taking SINGLE seconds: 30 (10 when QUICK), as many as
fit in *SERIES-SECONDS* when fewer, but at least 5, and 3 above 10 s."
  (if (> single 10d0)
      3
      (max 5 (min (if quick 10 30) (floor *series-seconds* (max single 1d-9))))))

(defun now ()
  "Seconds on CLOCK_MONOTONIC."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ seconds (* nanoseconds 1d-9))))

(defun time-batch (thunk n)
  (let ((start (now)))
    (dotimes (i n) (funcall thunk))
    (- (now) start)))

(defun median (xs)
  (let* ((v (sort (coerce xs 'vector) #'<))
         (n (length v)))
    (if (oddp n)
        (aref v (floor n 2))
        (* 0.5d0 (+ (aref v (1- (floor n 2))) (aref v (floor n 2)))))))

(defun sample-statistics (samples)
  "Plist of SAMPLES' median, minimum, median absolute deviation, mean and
standard deviation."
  (let* ((n (length samples))
         (med (median samples))
         (mean (/ (reduce #'+ samples) n)))
    `(:median ,med :min ,(reduce #'min samples)
      :mad ,(median (mapcar (lambda (x) (abs (- x med))) samples))
      :mean ,mean
      :sd ,(if (> n 1)
               (sqrt (/ (iter (for x in samples) (sum (expt (- x mean) 2))) (1- n)))
               0d0))))

(defun measure (thunk &key quick (min-batch 1) (batch-size :auto))
  "Time THUNK under the protocol. Returns a plist: the value, standard error
and details of the first call, the samples (seconds per call), the batch
size, the bytes consed per call, and SAMPLE-STATISTICS."
  (sb-ext:gc :full t)
  (let* ((start (now))
         (first-call (multiple-value-list (funcall thunk)))
         (single (- (now) start)))
    ;; Warm-up: three calls in all, the last one timed for the batch size.
    (unless (> single *slow-call-seconds*)
      (funcall thunk)
      (setf single (time-batch thunk 1)))
    (destructuring-bind (&optional value std-error details) first-call
      (let* ((batch (max min-batch
                         (if (eq batch-size :auto)
                             (max 1 (ceiling *sample-seconds* (max single 1d-9)))
                             batch-size)))
             (n-samples (sample-count single quick))
             (consed (sb-ext:get-bytes-consed))
             (samples (iter (repeat n-samples)
                            (collect (/ (time-batch thunk batch) batch))))
             (consed (- (sb-ext:get-bytes-consed) consed)))
        `(:value ,(and value (float value 1d0))
          :std-error ,(and std-error (float std-error 1d0))
          :details ,details
          :samples ,samples :batch-size ,batch
          :consed ,(round consed (* n-samples batch))
          ,@(sample-statistics samples))))))

(defmacro with-workers ((n) &body body)
  "Run BODY with an lparallel kernel of N workers."
  `(let ((lp:*kernel* (lp:make-kernel ,n :name "fincl-benchmarks")))
     (unwind-protect (progn ,@body)
       (lp:end-kernel :wait t))))

(defun run-lisp-benchmark (benchmark &key quick (min-batch 1) (workers 1))
  "Set BENCHMARK up (timed once) and MEASURE it with WORKERS lparallel
workers. A signalled error becomes (:error report)."
  (handler-case
      (with-workers (workers)
        (let ((start (now)))
          (multiple-value-bind (thunk reference) (funcall (benchmark-make benchmark))
            (let* ((setup (- (now) start))
                   (reference (and reference (funcall reference)))
                   (result (measure thunk :quick quick :min-batch min-batch
                                          :batch-size (benchmark-batch-size benchmark))))
              (when reference
                (setf (getf result :details) `(:reference ,(float reference 1d0)
                                               ,@(getf result :details))))
              `(:setup-seconds ,setup ,@result)))))
    (error (c) `(:error ,(princ-to-string c)))))

;;; --------------------------------------------------------------------
;;; The input echo
;;; --------------------------------------------------------------------

(defun same-input-p (a b)
  (if (and (realp a) (realp b))
      (= (float a 1d0) (float b 1d0))
      (string-equal (princ-to-string a) (princ-to-string b))))

(defun check-inputs (id lisp cpp)
  "Signal BENCHMARK-ERROR unless the plists LISP and CPP hold the same keys
with the same values (numbers compared as doubles)."
  (let ((lisp-keys (iter (for (k nil) on lisp by #'cddr) (collect k)))
        (cpp-keys (iter (for (k nil) on cpp by #'cddr) (collect k))))
    (unless (and (null (set-exclusive-or lisp-keys cpp-keys))
                 (every (lambda (k) (same-input-p (getf lisp k) (getf cpp k))) lisp-keys))
      (error 'benchmark-error
             :id id
             :message (format nil "the C++ program's inputs ~S differ from the Lisp side's ~S"
                              cpp lisp)))))
