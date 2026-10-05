;;;; math/random.lisp --- counter-based random numbers
;;;;
;;;; Philox4x32-10 (Salmon, Moraes, Dror and Shaw, "Parallel random numbers:
;;;; as easy as 1, 2, 3", SC'11): a keyed bijection of 128-bit counters. Its
;;;; output for a counter is a pure function of (key, counter), so draw k of
;;;; Monte Carlo path i can be computed directly, with no stream state and no
;;;; dependence on how paths are split between threads. It passes TestU01's
;;;; BigCrush and is the counter-based generator of cuRAND, Random123 and
;;;; NumPy.

(in-package #:fincl)

(deftype word32 () '(unsigned-byte 32))

(defconstant +philox-m0+ #xD2511F53)
(defconstant +philox-m1+ #xCD9E8D57)
(defconstant +philox-w0+ #x9E3779B9 "Key increment per round (golden ratio).")
(defconstant +philox-w1+ #xBB67AE85 "Key increment per round (sqrt 3 - 1).")

(declaim (inline philox4x32-10))
(defun philox4x32-10 (c0 c1 c2 c3 k0 k1)
  "Philox4x32 with 10 rounds: the 4 x 32-bit counter (C0 C1 C2 C3) under the
2 x 32-bit key (K0 K1), as (values x0 x1 x2 x3).

  (philox4x32-10 0 0 0 0 0 0) => #x6627E8D5, #xE169C58D, #xBC57AC4C, #x9B00DBD8"
  (declare (type word32 c0 c1 c2 c3 k0 k1)
           (optimize speed (safety 0)))
  (loop repeat 10
        do (let ((p0 (* +philox-m0+ c0))
                 (p1 (* +philox-m1+ c2)))
             (declare (type (unsigned-byte 64) p0 p1))
             ;; Each new word reads only words not yet overwritten.
             (setf c0 (logxor (ash p1 -32) c1 k0)
                   c1 (ldb (byte 32 0) p1)
                   c2 (logxor (ash p0 -32) c3 k1)
                   c3 (ldb (byte 32 0) p0)
                   k0 (ldb (byte 32 0) (+ k0 +philox-w0+))
                   k1 (ldb (byte 32 0) (+ k1 +philox-w1+)))))
  (values c0 c1 c2 c3))

(declaim (inline words-to-open-unit))
(defun words-to-open-unit (hi lo)
  "The top 53 bits of the 64-bit word HI:LO as a double in the open interval
(0, 1): (m + 1/2) 2^-53 for the 53-bit integer m, so never 0 or 1."
  (declare (type word32 hi lo))
  (* (+ (float (logior (ash hi 21) (ash lo -11)) 1d0) 0.5d0)
     #.(scale-float 1d0 -53)))

(declaim (inline counter-uniforms))
(defun counter-uniforms (key path block)
  "Uniforms 2 BLOCK and 2 BLOCK + 1 of PATH under KEY (all non-negative
integers below 2^64), as (values u v) in (0, 1). The counter is (BLOCK low,
BLOCK high, PATH low, PATH high) and the key KEY's two 32-bit halves.

  (counter-uniforms 42 0 0) => the first two uniforms of path 0 for seed 42"
  (declare (type (unsigned-byte 64) key path block))
  (multiple-value-bind (x0 x1 x2 x3)
      (philox4x32-10 (ldb (byte 32 0) block) (ldb (byte 32 32) block)
                     (ldb (byte 32 0) path) (ldb (byte 32 32) path)
                     (ldb (byte 32 0) key) (ldb (byte 32 32) key))
    (values (words-to-open-unit x0 x1) (words-to-open-unit x2 x3))))
