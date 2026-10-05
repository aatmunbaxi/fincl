;;;; math/linear-algebra.lisp --- least squares and banded solves on typed arrays
;;;;
;;;; Typed (SIMPLE-ARRAY DOUBLE-FLOAT (*)) throughout. Per the array-backend
;;;; decision (task 03, phase 4), this is also the only file that may use
;;;; magicl, and only for dense factorizations large enough for LAPACK to
;;;; pay; nothing here needs it yet.
;;;;
;;;; Least squares is solved by QR, never by the normal equations X'X b =
;;;; X'y, which square the condition number. Rows are folded into an upper
;;;; triangular R and Q'y one at a time by Givens rotations, so the design
;;;; matrix is never stored, and two accumulators built from disjoint rows
;;;; merge into the accumulator of all of them. That is what lets Monte Carlo
;;;; chunks factor their own rows in parallel and combine the results in a
;;;; fixed order (a tall-skinny QR).

(in-package #:fincl)

(deftype double-vector () '(simple-array double-float (*)))

(define-condition rank-deficient (pricing-error)
  ((index :initarg :index :reader rank-deficient-index)
   (pivot :initarg :pivot :reader rank-deficient-pivot))
  (:report (lambda (c s)
             (format s "Least-squares system is rank deficient: |R[~D,~D]| = ~,3E ~
is negligible next to R[0,0]."
                     (rank-deficient-index c) (rank-deficient-index c)
                     (abs (rank-deficient-pivot c))))))

(defstruct (qr-accumulator (:constructor %make-qr-accumulator (n r qty)))
  "The R factor (row-major, upper triangular, N x N) and the first N entries
of Q'y of the rows folded in so far."
  (n 0 :type fixnum :read-only t)
  (r nil :type (simple-array double-float (*)) :read-only t)
  (qty nil :type (simple-array double-float (*)) :read-only t)
  (rows 0 :type fixnum))

(defun make-qr-accumulator (n)
  "An empty least-squares accumulator for N unknowns: fold rows in with
QR-ADD-ROW!, combine accumulators with QR-MERGE!, and solve with QR-SOLVE.

  (qr-solve (reduce #'qr-merge! chunk-accumulators
                    :initial-value (make-qr-accumulator 4)))"
  (%make-qr-accumulator n
                        (make-array (* n n) :element-type 'double-float :initial-element 0d0)
                        (make-array n :element-type 'double-float :initial-element 0d0)))

(declaim (inline hypot))
(defun hypot (a b)
  "sqrt(a^2 + b^2) without overflow or underflow in the squares."
  (declare (double-float a b))
  (let ((a (abs a)) (b (abs b)))
    (if (< a b) (rotatef a b))
    (if (zerop a) 0d0 (let ((q (/ b a))) (* a (sqrt (+ 1d0 (* q q))))))))

(declaim (inline qr-add-row!))
(defun qr-add-row! (acc x y)
  "Fold the observation row X (length N; used as scratch and overwritten)
with right-hand side Y into ACC by Givens rotations. Returns ACC."
  (declare (type qr-accumulator acc)
           (type (simple-array double-float (*)) x)
           (double-float y)
           (optimize (speed 3) (safety 0)))
  (let ((n (qr-accumulator-n acc))
        (r (qr-accumulator-r acc))
        (qty (qr-accumulator-qty acc)))
    (declare (fixnum n))
    (dotimes (k n)
      (let ((xk (aref x k)))
        (unless (zerop xk)
          (let* ((kk (+ (* k n) k))
                 (rkk (aref r kk))
                 (h (hypot rkk xk))
                 (c (/ rkk h))
                 (s (/ xk h)))
            (declare (fixnum kk) (double-float rkk h c s))
            (setf (aref r kk) h)
            (loop for j fixnum from (1+ k) below n
                  for kj fixnum = (+ (* k n) j)
                  do (let ((rkj (aref r kj)) (xj (aref x j)))
                       (setf (aref r kj) (+ (* c rkj) (* s xj))
                             (aref x j) (- (* c xj) (* s rkj)))))
            (let ((zk (aref qty k)))
              (setf (aref qty k) (+ (* c zk) (* s y))
                    y (- (* c y) (* s zk))))))))
    (incf (qr-accumulator-rows acc))
    acc))

(defun qr-merge! (acc other)
  "Fold the rows summarized by OTHER into ACC: the result is the
accumulator of both row sets. OTHER is unchanged. Returns ACC."
  (declare (type qr-accumulator acc other))
  (let* ((n (qr-accumulator-n acc))
         (row (make-array n :element-type 'double-float)))
    ;; Internal invariant: engines build every accumulator of one
    ;; regression with the same basis size; no caller input reaches here.
    (assert (= n (qr-accumulator-n other)))
    (dotimes (k n)
      (fill row 0d0)
      (replace row (qr-accumulator-r other) :start1 k :start2 (+ (* k n) k)
                                            :end2 (* (1+ k) n))
      (qr-add-row! acc row (aref (qr-accumulator-qty other) k)))
    ;; Each OTHER row of R counted as one row above; restore the true count.
    (setf (qr-accumulator-rows acc)
          (+ (- (qr-accumulator-rows acc) n) (qr-accumulator-rows other)))
    acc))

(defun qr-solve (acc &key (tolerance 1d-12))
  "The least-squares solution b of the rows folded into ACC, by back
substitution in R b = Q'y. Signals RANK-DEFICIENT when a diagonal entry of R
is below TOLERANCE times |R[0,0]|."
  (declare (type qr-accumulator acc))
  (let* ((n (qr-accumulator-n acc))
         (r (qr-accumulator-r acc))
         (b (copy-seq (qr-accumulator-qty acc)))
         (scale (abs (aref r 0))))
    (declare (type (simple-array double-float (*)) b))
    (loop for i from (1- n) downto 0
          for pivot = (aref r (+ (* i n) i))
          do (when (or (zerop scale) (< (abs pivot) (* tolerance scale)))
               (error 'rank-deficient :index i :pivot pivot))
             (let ((sum (aref b i)))
               (loop for j from (1+ i) below n
                     do (decf sum (* (aref r (+ (* i n) j)) (aref b j))))
               (setf (aref b i) (/ sum pivot))))
    b))

;;; --------------------------------------------------------------------
;;; Tridiagonal systems
;;; --------------------------------------------------------------------
;;;
;;; Row i of a tridiagonal system reads a[i] x[i-1] + b[i] x[i] + c[i] x[i+1]
;;; = d[i]: A, B, C and D all have length n, A[0] and C[n-1] are unused, or,
;;; for a cyclic system, are the corner elements (row 0's coefficient of
;;; x[n-1] and row n-1's of x[0]). Typed Lisp rather than LAPACK's dgtsv:
;;; the same speed at n = 200 and 2000 (task 03), and magicl's generated
;;; %dgtsv drops LAPACK's INFO, so a singular system would go unnoticed.

(declaim (inline %thomas!))
(defun %thomas! (n a b b0 c c0 d d0)
  "The Thomas algorithm on N unknowns, with the diagonal at B[B0 ...], the
super-diagonal at C[C0 ...] (overwritten) and the right-hand side at D[D0
...] (overwritten with the solution). Offsets let the cyclic solver keep its
scratch in one vector. Returns T, or NIL at a zero pivot."
  (declare (fixnum n b0 c0 d0) (type double-vector a b c d)
           (optimize (speed 3) (safety 0)))
  (let ((pivot (aref b b0)))
    (when (zerop pivot) (return-from %thomas! nil))
    (when (> n 1) (setf (aref c c0) (/ (aref c c0) pivot)))
    (setf (aref d d0) (/ (aref d d0) pivot)))
  (loop for i fixnum from 1 below n
        do (let ((pivot (- (aref b (+ b0 i)) (* (aref a i) (aref c (+ c0 i -1))))))
             (declare (double-float pivot))
             (when (zerop pivot) (return-from %thomas! nil))
             (when (< i (1- n))
               (setf (aref c (+ c0 i)) (/ (aref c (+ c0 i)) pivot)))
             (setf (aref d (+ d0 i))
                   (/ (- (aref d (+ d0 i)) (* (aref a i) (aref d (+ d0 i -1)))) pivot))))
  (loop for i fixnum from (- n 2) downto 0
        do (decf (aref d (+ d0 i)) (* (aref c (+ c0 i)) (aref d (+ d0 i 1)))))
  t)

(defun signal-zero-pivot (n)
  (signal-convergence-failure "zero pivot in a tridiagonal solve of size ~D; the matrix ~
is singular or not diagonally dominant" n))

(defun solve-tridiagonal! (a b c d)
  "Solve the tridiagonal system with sub-diagonal A, diagonal B and
super-diagonal C (A[0] and C[n-1] unused) for the right-hand side D by the
Thomas algorithm, in O(n) and without allocating: C is overwritten with
scratch values and D with the solution, which is returned. Stable without
pivoting when the matrix is diagonally dominant (|b_i| >= |a_i| + |c_i|,
strictly in at least one row), as finite-difference and spline systems are.
A zero pivot signals CONVERGENCE-FAILURE, whose USE-VALUE restart returns a
value in place of the solution.

  (solve-tridiagonal! a b (copy-seq c) (copy-seq d))"
  (declare (type double-vector a b c d))
  (if (%thomas! (length d) a b 0 c 0 d 0)
      d
      (signal-zero-pivot (length d))))

(defun solve-tridiagonal-cyclic! (a b c d &key (work (make-array (* 3 (length d))
                                                                 :element-type 'double-float)))
  "Solve the cyclic tridiagonal system whose row i is a[i] x[i-1] + b[i] x[i]
+ c[i] x[i+1] = d[i], indices modulo n (A[0] multiplies x[n-1], C[n-1]
multiplies x[0]), by Sherman-Morrison: two Thomas solves on the matrix with
the corners removed and its first and last diagonal entries modified. D is
overwritten with the solution and returned; A, B and C are unchanged. WORK, a
DOUBLE-VECTOR of length at least 3n, is scratch: pass one to solve without
allocating. Needs n >= 3 and, like SOLVE-TRIDIAGONAL!, diagonal dominance.

  (solve-tridiagonal-cyclic! a b c (copy-seq d))"
  (declare (type double-vector a b c d work))
  (let ((n (length d)))
    (declare (fixnum n))
    (unless (>= n 3)
      (signal-invalid-argument '(length d) n '(integer 3) "at least 3 unknowns"))
    (unless (>= (length work) (* 3 n))
      (signal-invalid-argument '(length work) (length work) `(integer ,(* 3 n))
                               (format nil "a scratch vector of at least ~D elements" (* 3 n))))
    (let* ((alpha (aref c (1- n)))       ; row n-1, column 0
           (beta (aref a 0))             ; row 0, column n-1
           (gamma (- (aref b 0)))
           (z0 (* 2 n)))                 ; WORK: diagonal [0, n), C copy [n, 2n), z [2n, 3n)
      (declare (double-float alpha beta gamma) (fixnum z0))
      (when (zerop gamma) (return-from solve-tridiagonal-cyclic! (signal-zero-pivot n)))
      (replace work b)
      (decf (aref work 0) gamma)
      (decf (aref work (1- n)) (/ (* alpha beta) gamma))
      ;; x: the corner-free system with the right-hand side.
      (replace work c :start1 n)
      (unless (%thomas! n a work 0 work n d 0)
        (return-from solve-tridiagonal-cyclic! (signal-zero-pivot n)))
      ;; z: the same system with u = (gamma, 0, ..., 0, alpha).
      (replace work c :start1 n)
      (fill work 0d0 :start z0 :end (+ z0 n))
      (setf (aref work z0) gamma
            (aref work (+ z0 n -1)) alpha)
      (unless (%thomas! n a work 0 work n work z0)
        (return-from solve-tridiagonal-cyclic! (signal-zero-pivot n)))
      (let ((factor (/ (+ (aref d 0) (/ (* beta (aref d (1- n))) gamma))
                       (+ 1 (aref work z0) (/ (* beta (aref work (+ z0 n -1))) gamma)))))
        (declare (double-float factor))
        (dotimes (i n d)
          (decf (aref d i) (* factor (aref work (+ z0 i)))))))))

(defun tridiagonal-multiply (a b c x out &key cyclic)
  "OUT = M X for the tridiagonal M with sub-diagonal A, diagonal B and
super-diagonal C, in the layout of SOLVE-TRIDIAGONAL! (with CYCLIC, A[0] and
C[n-1] are corner elements). Returns OUT.

  (tridiagonal-multiply a b c x (make-array n :element-type 'double-float))"
  (declare (type double-vector a b c x out)
           (optimize (speed 3) (safety 0)))
  (let ((n (length x)))
    (declare (fixnum n))
    (dotimes (i n out)
      (setf (aref out i)
            (+ (* (aref b i) (aref x i))
               (cond ((> i 0) (* (aref a i) (aref x (1- i))))
                     (cyclic (* (aref a 0) (aref x (1- n))))
                     (t 0d0))
               (cond ((< i (1- n)) (* (aref c i) (aref x (1+ i))))
                     (cyclic (* (aref c (1- n)) (aref x 0)))
                     (t 0d0)))))))
