;;;; clock.lisp --- the clock, allocation counter and process id of the benchmark harness
;;;;
;;;; SBCL reads CLOCK_MONOTONIC in nanoseconds and counts bytes consed. Other
;;;; implementations use GET-INTERNAL-REAL-TIME (microseconds in the
;;;; implementations that matter here, whatever INTERNAL-TIME-UNITS-PER-SECOND
;;;; says) and report no allocation. CLOCK-DESCRIPTION goes into the report.

(in-package #:fincl/benchmarks)

(defun now ()
  "Seconds on a monotonic clock."
  #+sbcl (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-monotonic)
           (+ seconds (* nanoseconds 1d-9)))
  #-sbcl (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defun clock-description ()
  "A phrase naming the clock `now' reads."
  #+sbcl "CLOCK_MONOTONIC"
  #-sbcl (format nil "GET-INTERNAL-REAL-TIME (resolution 1/~D s)" internal-time-units-per-second))

(defun bytes-consed ()
  "Bytes allocated so far, or 0 where the implementation does not count."
  #+sbcl (sb-ext:get-bytes-consed)
  #-sbcl 0)

(defun full-gc ()
  "A full garbage collection where the implementation offers one."
  #+sbcl (sb-ext:gc :full t)
  #-sbcl (progn))

(defun process-id ()
  "The process id of this image."
  #+sbcl (sb-unix:unix-getpid)
  #-sbcl (parse-integer (uiop:run-program '("sh" "-c" "echo $PPID") :output :string)
                        :junk-allowed t))
