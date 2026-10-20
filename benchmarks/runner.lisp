;;;; runner.lisp --- RUN-BENCHMARKS: build, run both sides, report
;;;;
;;;; The C++ programs run first, one after another (their batch sizes are
;;;; the Lisp side's minimum), then the Lisp side, group by group; nothing
;;;; runs concurrently with a measurement.

(in-package #:fincl/benchmarks)

(defparameter *groups*
  '(:calibration :options-pricing :exotics-analytic :monte-carlo :pde :rates :calibration-models)
  "Every group, in report order.")

(defparameter *compiler-flags* '("-std=c++17" "-O2" "-DNDEBUG"))

(defun benchmarks-directory ()
  (asdf:system-relative-pathname "fincl" "benchmarks/"))

(defun group-source (group)
  (merge-pathnames (format nil "~(~A~)/~:*~(~A~).cpp" group) (benchmarks-directory)))

(defun group-binary (group)
  (merge-pathnames (format nil "build/~(~A~)" group) (benchmarks-directory)))

;;; --------------------------------------------------------------------
;;; The machine
;;; --------------------------------------------------------------------

(defun file-lines (path)
  (with-open-file (s path :if-does-not-exist nil)
    (and s (iter (for line = (read-line s nil)) (while line) (collect line)))))

(defun cpuinfo-field (line)
  (string-trim " " (subseq line (1+ (position #\: line)))))

(defun cpu-model ()
  (or (iter (for line in (file-lines "/proc/cpuinfo"))
            (finding (cpuinfo-field line) such-that (a:starts-with-subseq "model name" line)))
      (machine-version)))

(defun logical-cores ()
  (max 1 (count-if (lambda (l) (a:starts-with-subseq "processor" l)) (file-lines "/proc/cpuinfo"))))

(defun physical-cores ()
  "Distinct (physical id, core id) pairs in /proc/cpuinfo, or the logical
count if it has none."
  (let ((pairs '()) (package nil))
    (dolist (line (file-lines "/proc/cpuinfo"))
      (cond ((a:starts-with-subseq "physical id" line) (setf package (cpuinfo-field line)))
            ((a:starts-with-subseq "core id" line)
             (pushnew `(,package ,(cpuinfo-field line)) pairs :test #'equal))))
    (if pairs (length pairs) (logical-cores))))

(defun governor ()
  (first (file-lines "/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor")))

(defun load-average ()
  (a:when-let ((line (first (file-lines "/proc/loadavg"))))
    (let ((*read-default-float-format* 'double-float))
      (read-from-string line))))

;;; --------------------------------------------------------------------
;;; The C++ side
;;; --------------------------------------------------------------------

(defun stale-p (binary sources)
  (or (not (probe-file binary))
      (some (lambda (s) (> (file-write-date s) (file-write-date binary))) sources)))

(defun build-programs (groups)
  "Compile each of GROUPS' programs whose binary is older than its sources,
concurrently. Returns an alist (group . binary-or-error-message)."
  (ensure-directories-exist (merge-pathnames "build/" (benchmarks-directory)))
  (let* ((headers (mapcar (lambda (f) (merge-pathnames f (benchmarks-directory)))
                          '("harness.hpp" "common.hpp" "../tests/fixtures/fixture_curves.hpp")))
         (jobs (iter (for group in groups)
                     (for source = (group-source group))
                     (for binary = (group-binary group))
                     (when (probe-file source)
                       (collect
                           `(,group ,binary
                                    ,(when (stale-p binary `(,source ,@headers))
                                       (format *error-output* "~&; compiling ~(~A~).cpp~%" group)
                                       (uiop:launch-program
                                        `("c++" ,@*compiler-flags* ,(namestring source)
                                                "-o" ,(namestring binary) "-lQuantLib")
                                        :output nil :error-output :stream))))))))
    (iter (for (group binary process) in jobs)
          (collect
              (cons group
                    (if (null process)
                        binary
                        (let ((errors (uiop:slurp-stream-string (uiop:process-info-error-output process))))
                          (if (zerop (uiop:wait-process process))
                              binary
                              (format nil "compilation failed: ~A"
                                      (one-line (subseq errors 0 (min 400 (length errors)))))))))))))

(defun read-program-output (string)
  "The forms of STRING's lines that start with a parenthesis."
  (let ((*read-default-float-format* 'double-float)
        (*read-eval* nil))
    (iter (for line in (uiop:split-string string :separator '(#\Newline)))
          (when (a:starts-with #\( line)
            (collect (read-from-string line))))))

(defun run-program-for (group binary &key quick pin)
  "Run GROUP's BINARY. Returns (values results header): RESULTS a hash table
from benchmark id to its plist, HEADER the program's (:header ...) plist."
  (format *error-output* "~&; QuantLib: ~(~A~)~%" group)
  (let* ((command `(,@(when pin `("taskset" "-c" ,(princ-to-string pin)))
                    ,(namestring binary) ,@(when quick '("--quick"))))
         (output (uiop:run-program command :output :string :error-output nil :ignore-error-status t))
         (results (make-hash-table :test 'equal))
         (header nil))
    (dolist (form (read-program-output output))
      (if (eq (first form) :header)
          (setf header (rest form))
          (setf (gethash (getf form :id) results)
                (a:if-let ((samples (getf form :samples)))
                  `(,@form ,@(sample-statistics samples))
                  form))))
    (values results header)))

;;; --------------------------------------------------------------------
;;; The Lisp side
;;; --------------------------------------------------------------------

(defun pin-lisp (cpu)
  "Bind every thread of this SBCL (and the threads it starts later) to CPU."
  (uiop:run-program `("taskset" "-a" "-p" "-c" ,(princ-to-string cpu)
                                ,(princ-to-string (sb-unix:unix-getpid)))
                    :output nil :ignore-error-status t))

(defun run-group (group cpp &key quick workers)
  "The rows of GROUP: each benchmark run on the Lisp side, joined with its
C++ result in the table CPP (or NIL)."
  (iter (for b in (group-benchmarks group))
        (for id = (benchmark-id b))
        (for quantlib = (and cpp (gethash id cpp)))
        (when quantlib
          (check-inputs id (benchmark-inputs b) (getf quantlib :inputs)))
        (format *error-output* "~&; fincl: ~(~A~)/~A~%" group id)
        (for min-batch = (or (and quantlib (getf quantlib :batch-size)) 1))
        (collect `(:benchmark ,b :quantlib ,quantlib
                   :fincl ,(run-lisp-benchmark b :quick quick :min-batch min-batch)
                   :parallel ,(and (benchmark-parallel b) (> workers 1)
                                   (run-lisp-benchmark b :quick quick :min-batch min-batch
                                                         :workers workers))
                   :workers ,workers))))

;;; --------------------------------------------------------------------
;;; The run
;;; --------------------------------------------------------------------

(defun timestamp ()
  (multiple-value-bind (s mi h d mo y) (decode-universal-time (get-universal-time) 0)
    (format nil "~D-~2,'0D-~2,'0DT~2,'0D~2,'0D~2,'0DZ" y mo d h mi s)))

(defun print-header (stream header workers quick pin quantlib-notice &key org)
  (let ((governor (governor)) (load (load-average)))
    (format stream "~&~:[~;* ~]fincl benchmarks against QuantLib~@[ ~A~]~%" org (getf header :quantlib))
    (format stream "~&  machine  : ~A, ~D cores (~D threads), governor ~A~:[~; (!)~], load ~,2F~@[, pinned to CPU ~D~]~%"
            (cpu-model) (physical-cores) (logical-cores) (or governor "unknown")
            (and governor (string/= governor "performance")) (or load 0) pin)
    (format stream "~&  fincl    : SBCL ~A, 1 worker (Monte Carlo and calibration also at ~D)~%"
            (lisp-implementation-version) workers)
    (format stream "~&  QuantLib : ~:[not run~;~:*~A~]~@[, g++ ~A~] ~{~A~^ ~}~%"
            (getf header :quantlib) (getf header :compiler) *compiler-flags*)
    (format stream "~&  protocol : ~D samples (fewer for calls over ~,2F s: about ~D s a series, at least 5, 3 above 10 s), ~
median of batches >= ~D ms, wall time (CLOCK_MONOTONIC)~%"
            (if quick 10 30) (/ *series-seconds* 30) (round *series-seconds*) (round (* 1000 *sample-seconds*)))
    (when (and load (> load 0.5))
      (format stream "~&  warning  : the load average is ~,2F; timings will be noisy~%" load))
    (when quantlib-notice
      (format stream "~&  notice   : ~A~%" quantlib-notice))))

(defun row-data (row)
  "ROW as printable data."
  (destructuring-bind (&key benchmark quantlib fincl parallel workers) row
    (multiple-value-bind (verdict difference z) (row-verdict row)
      `(:id ,(benchmark-id benchmark) :group ,(benchmark-group benchmark)
        :title ,(benchmark-title benchmark) :engine ,(benchmark-engine benchmark)
        :kind ,(benchmark-kind benchmark) :tolerance ,(benchmark-tolerance benchmark)
        :inputs ,(benchmark-inputs benchmark) :work ,(benchmark-work benchmark)
        :notes ,(benchmark-notes benchmark)
        :verdict ,verdict :difference ,difference :z ,z
        :quantlib ,quantlib :fincl ,fincl :parallel ,parallel :workers ,workers))))

(defun run-benchmarks (&key (groups *groups*) quick (threads (physical-cores)) pin
                            (output :text) (quantlib t))
  "Run GROUPS (keywords from *GROUPS*, or one) on both sides and print the
report to standard output, as text tables (OUTPUT :TEXT) or org tables
(:ORG); with :SEXP print nothing. Every run's raw results go to
benchmarks/results/<UTC time>.sexp. Returns the rows as data.

  QUICK     10 samples instead of 30
  THREADS   fincl's extra Monte Carlo and calibration rows run this many
            workers (default: the physical cores); the rest run one
  PIN       a CPU number: both sides run under taskset on it
  QUANTLIB  NIL runs the fincl side alone

  (run-benchmarks :groups '(:calibration :options-pricing) :quick t)"
  (let ((groups (a:ensure-list groups)))
    (a:when-let ((unknown (set-difference groups *groups*)))
      (error 'benchmark-error :id unknown
                              :message (format nil "unknown groups; the groups are ~S" *groups*)))
    (setf groups (remove-if-not (lambda (g) (member g groups)) *groups*))
    (when pin (pin-lisp pin))
    (let* ((compiler (and quantlib
                          (ignore-errors
                           (zerop (nth-value 2 (uiop:run-program '("c++" "--version")
                                                                 :output nil :ignore-error-status t))))))
           (built (and compiler (build-programs groups)))
           (notice (cond ((not quantlib) "QuantLib side disabled (:quantlib nil)")
                         ((not compiler) "no C++ compiler on the PATH: the fincl side runs alone (use nix develop .#benchmarks)")
                         (t (a:when-let ((failed (remove-if-not (lambda (b) (stringp (cdr b))) built)))
                              (format nil "~{~A~^; ~}"
                                      (mapcar (lambda (b) (format nil "~(~A~): ~A" (car b) (cdr b)))
                                              failed))))))
           (header nil)
           (cpp (iter (for group in groups)
                      (for binary = (cdr (assoc group built)))
                      (collect (cons group
                                     (when (pathnamep binary)
                                       (multiple-value-bind (results h)
                                           (run-program-for group binary :quick quick :pin pin)
                                         (setf header (or header h))
                                         results))))))
           (stream (if (eq output :sexp) (make-broadcast-stream) *standard-output*))
           (org (eq output :org))
           (all-rows '()))
      (print-header stream header threads quick pin notice :org org)
      (dolist (group groups)
        (let ((rows (run-group group (cdr (assoc group cpp)) :quick quick :workers threads)))
          (print-group group rows stream :org org :workers threads)
          (finish-output stream)
          (setf all-rows (append all-rows rows))))
      (print-discrepancies all-rows stream :org org)
      (let ((data (mapcar #'row-data all-rows))
            (path (merge-pathnames (format nil "results/~A.sexp" (timestamp)) (benchmarks-directory))))
        (ensure-directories-exist path)
        (with-open-file (s path :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*print-readably* nil) (*print-pretty* t) (*package* (find-package '#:fincl/benchmarks)))
              (prin1 `(:machine (:cpu ,(cpu-model) :cores ,(physical-cores) :threads ,(logical-cores)
                                 :governor ,(governor) :load ,(load-average))
                       :fincl (:sbcl ,(lisp-implementation-version) :workers ,threads)
                       :quantlib ,header :flags ,*compiler-flags* :quick ,quick :pin ,pin
                       :rows ,data)
                     s))))
        (format stream "~&~% raw results: ~A~%" (namestring path))
        data))))
