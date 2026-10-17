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
  (dotimes (round 10)
    (let ((p0 (* +philox-m0+ c0))
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

;;; --------------------------------------------------------------------
;;; Sobol' points
;;; --------------------------------------------------------------------
;;;
;;; Sobol' points in Gray-code order (Antonov and Saleev) with Joe and
;;; Kuo's new-joe-kuo-6.21201 direction numbers (src/math/sobol-directions.sexp,
;;; dimensions 1 to 1111), held as 64-bit integers whose top 32 bits are the
;;; usual 32-bit Sobol' integers. Randomized by Matousek's linear scrambling
;;; (a random lower-triangular binary matrix with unit diagonal, applied to
;;; the direction numbers) and a random digital shift, so the 64-bit
;;; integers have random low bits and the uniforms full double resolution:
;;; normal tails are not cut off at the 2^-32 grid.

(deftype word64 () '(unsigned-byte 64))

(defconstant +sobol-bits+ 32 "Direction numbers per dimension: up to 2^32 points.")

(defparameter *sobol-max-dimension* 1111)

(defvar *sobol-directions* nil
  "The unscrambled direction numbers, a (*SOBOL-MAX-DIMENSION* x 32) array of
WORD64, computed on first use.")

(defun read-sobol-initializers ()
  (with-open-file (in (asdf:system-relative-pathname :fincl "src/math/sobol-directions.sexp"))
    (with-standard-io-syntax
      (let ((*read-eval* nil)) (read in)))))

(defun compute-sobol-directions ()
  "Direction numbers V[d, k] = m_k 2^(64-k) for k = 1..32, the m_k beyond the
initial ones from the recurrence of dimension d's primitive polynomial."
  (let ((v (make-array `(,*sobol-max-dimension* ,+sobol-bits+) :element-type 'word64)))
    ;; Dimension 1: every m_k = 1.
    (dotimes (k +sobol-bits+)
      (setf (aref v 0 k) (ash 1 (- 63 k))))
    (iter (for (s a . m) in (read-sobol-initializers))
          (for d from 1)
          (iter (for k from 0 below s)
                (setf (aref v d k) (ash (nth k m) (- 63 k))))
          ;; V_k = V_{k-s} >> s xor V_{k-s} xor (a_j V_{k-j}, j = 1..s-1).
          (iter (for k from s below +sobol-bits+)
                (let ((next (logxor (aref v d (- k s)) (ash (aref v d (- k s)) (- s)))))
                  (iter (for j from 1 below s)
                        (when (logbitp (- s 1 j) a)
                          (setf next (logxor next (aref v d (- k j))))))
                  (setf (aref v d k) next))))
    v))

(defun sobol-directions ()
  (or *sobol-directions* (setf *sobol-directions* (compute-sobol-directions))))

(defun philox-word64 (key a b c)
  "A random 64-bit word, a pure function of KEY and the counter words A, B, C."
  (multiple-value-bind (x0 x1)
      (philox4x32-10 (ldb (byte 32 0) a) (ldb (byte 32 0) b) (ldb (byte 32 0) c) 0
                     (ldb (byte 32 0) key) (ldb (byte 32 32) key))
    (logior (ash x0 32) x1)))

(defun scramble-word (rows v)
  "L V over GF(2): bit 63 - i of the result is the parity of ROWS[i] AND V."
  (declare (type (simple-array word64 (64)) rows) (type word64 v))
  (let ((result 0))
    (declare (type word64 result))
    (dotimes (i 64 result)
      (when (oddp (logcount (logand (aref rows i) v)))
        (setf result (logior result (ash 1 (- 63 i))))))))

(defun sobol-tables (n-dims &key key (randomization 0))
  "Direction numbers and initial point for N-DIMS dimensions, as (values
directions shift): DIRECTIONS an (N-DIMS x 32) WORD64 array and SHIFT a
WORD64 vector. With KEY, randomization number RANDOMIZATION of KEY: each
dimension's direction numbers are multiplied by its own random
lower-triangular matrix with unit diagonal, and the points are shifted by a
random digital shift; the draws are Philox words keyed by KEY. Without KEY,
the plain Sobol' points (shift 0)."
  (when (> n-dims *sobol-max-dimension*)
    (signal-invalid-argument 'n-dims n-dims `(integer 1 ,*sobol-max-dimension*)
                             (format nil "at most ~D Sobol' dimensions (steps x factors)"
                                     *sobol-max-dimension*)))
  (let ((base (sobol-directions))
        (directions (make-array `(,n-dims ,+sobol-bits+) :element-type 'word64))
        (shift (make-array n-dims :element-type 'word64 :initial-element 0))
        (rows (make-array 64 :element-type 'word64)))
    (dotimes (d n-dims)
      (if key
          (progn
            ;; Row i (output digit i + 1): the diagonal bit and random bits
            ;; at the more significant digits.
            (dotimes (i 64)
              (setf (aref rows i)
                    (logior (ash 1 (- 63 i))
                            (logand (philox-word64 key randomization d (1+ i))
                                    (ldb (byte 64 0) (ash (1- (ash 1 i)) (- 64 i)))))))
            (dotimes (k +sobol-bits+)
              (setf (aref directions d k) (scramble-word rows (aref base d k))))
            (setf (aref shift d) (philox-word64 key randomization d 0)))
          (dotimes (k +sobol-bits+)
            (setf (aref directions d k) (aref base d k)))))
    (values directions shift)))

(defun sobol-point! (point directions shift index)
  "Set POINT to Sobol' point INDEX (Gray-code order, from 0) of the tables
DIRECTIONS and SHIFT."
  (declare (type (simple-array word64 (*)) point shift)
           (type (simple-array word64 (* *)) directions)
           (type (unsigned-byte 32) index))
  (let ((gray (logxor index (ash index -1))))
    (dotimes (d (length point) point)
      (let ((x (aref shift d)))
        (declare (type word64 x))
        (dotimes (k +sobol-bits+)
          (when (logbitp k gray)
            (setf x (logxor x (aref directions d k)))))
        (setf (aref point d) x)))))

(defun sobol-next! (point directions index)
  "Advance POINT from Sobol' point INDEX to INDEX + 1: XOR in the direction
numbers of the lowest zero bit of INDEX."
  (declare (type (simple-array word64 (*)) point)
           (type (simple-array word64 (* *)) directions)
           (type (unsigned-byte 32) index)
           (optimize speed))
  (let ((k (1- (integer-length (logand (lognot index) (1+ index))))))
    (dotimes (d (length point) point)
      (setf (aref point d) (logxor (aref point d) (aref directions d k))))))

(declaim (inline word64-to-open-unit))
(defun word64-to-open-unit (x)
  "The top 53 bits of X as a double in (0, 1): (m + 1/2) 2^-53."
  (declare (type word64 x))
  (* (+ (float (ash x -11) 1d0) 0.5d0) #.(scale-float 1d0 -53)))
