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

;;; The floor for Monte Carlo engines: N terminal GBM draws and call payoffs
;;; with no engine, written the same way in both languages. splitmix64 gives
;;; uniforms in (0, 1]; Box-Muller turns each pair into two normals, one per
;;; path. Returns the discounted mean payoff and its standard error.
(defun gbm-terminal-loop (n)
  (declare (fixnum n))
  (let* ((s0 100d0) (k 100d0) (rate 0.05d0) (dividend 0.02d0) (vol 0.2d0)
         (drift (- rate dividend (* 0.5d0 vol vol)))
         (state 42) (sum 0d0) (sum2 0d0))
    (declare (double-float s0 k rate dividend vol drift sum sum2) (type (unsigned-byte 64) state))
    ;; splitmix64, local so that its 64-bit words are never boxed.
    (flet ((uniform ()
             (setf state (ldb (byte 64 0) (+ state #x9E3779B97F4A7C15)))
             (let* ((z state)
                    (z (ldb (byte 64 0) (* (logxor z (ash z -30)) #xBF58476D1CE4E5B9)))
                    (z (ldb (byte 64 0) (* (logxor z (ash z -27)) #x94D049BB133111EB)))
                    (z (logxor z (ash z -31))))
               (declare (type (unsigned-byte 64) z))
               (* (+ (ash z -11) 1) #.(expt 2d0 -53))))
           (accumulate (z)
             (declare (double-float z))
             (let ((payoff (max (- (* s0 (exp (+ drift (* vol z)))) k) 0d0)))
               (incf sum payoff)
               (incf sum2 (* payoff payoff)))))
      (dotimes (i (floor n 2))
        (let* ((u1 (uniform)) (u2 (uniform))
               (r (sqrt (* -2d0 (log u1))))
               (angle (* 2d0 pi u2)))
          (declare (double-float u1 u2 r angle))
          (accumulate (* r (cos angle)))
          (accumulate (* r (sin angle)))))
      (let* ((df (exp (- rate)))
             (mean (/ sum n))
             (variance (- (/ sum2 n) (* mean mean))))
        (values (* df mean) (* df (sqrt (/ variance n))))))))

(define-benchmark "gbm-terminal-loop"
    (:group :calibration :title "10^6 terminal GBM call payoffs, no engine" :engine "loop"
     :inputs (equity-inputs :kind "call") :work '(:paths 1000000 :steps 1)
     :kind :statistical
     :notes "the same generator (splitmix64, Box-Muller, two paths per pair of uniforms)
and arithmetic on both sides: the floor for the Monte Carlo rows' per-path cost")
    ((n 1000000))
  (gbm-terminal-loop n))
