;;;; calibration.lisp --- checks on the measurement itself
;;;;
;;;;   sleep-50ms   both sides must time a 50 ms sleep within 2%
;;;;   scalar-loop  x <- 1.0000001 x + 1e-9, 10^8 times, written the same way
;;;;                in both languages: the raw SBCL-to-g++ ratio for scalar
;;;;                double arithmetic, against which other ratios are read.
;;;;                Both sides compute the same IEEE result.

(in-package #:fincl/benchmarks)

(defun scalar-loop (n)
  (declare (fixnum n))
  (let ((x 1d0))
    (declare (double-float x))
    (dotimes (i n x)
      (setf x (+ (* x 1.0000001d0) 1d-9)))))

(define-benchmark "sleep-50ms"
    (:group :calibration :title "A 50 ms sleep" :engine "sleep"
     :inputs '(:seconds 0.05d0) :kind :none)
    ()
  (sleep 0.05d0)
  0.05d0)

(define-benchmark "scalar-loop"
    (:group :calibration :title "10^8 dependent multiply-adds" :engine "loop"
     :inputs '(:iterations 100000000) :kind :deterministic :tolerance 0)
    ((n 100000000))
  (scalar-loop n))
