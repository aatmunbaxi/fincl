;;;; math/linear-algebra.lisp --- small dense least squares on typed arrays
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
