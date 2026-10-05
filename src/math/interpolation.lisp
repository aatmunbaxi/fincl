;;;; math/interpolation.lisp --- one- and two-dimensional interpolation
;;;;
;;;; An interpolator is a funcallable instance: (funcall interp x) evaluates
;;;; it through a closure built once for its method, while
;;;; INTERPOLATOR-DERIVATIVE, INTERPOLATOR-SECOND-DERIVATIVE and
;;;; INTERPOLATOR-KNOTS give its other faces. Knots live in typed vectors and
;;;; are found by binary search.
;;;;
;;;; Every cubic is stored as per-segment coefficients of
;;;; y_i + c1 t + c2 t^2 + c3 t^3, t = x - x_i, so natural and monotone cubics
;;;; differ only in how the coefficients are built.

(in-package #:fincl)

(define-condition extrapolation-error (invalid-input)
  ((x :initarg :x :reader extrapolation-x)
   (lo :initarg :lo :reader extrapolation-lo)
   (hi :initarg :hi :reader extrapolation-hi))
  (:report (lambda (c s)
             (format s "~F is outside the interpolator's domain [~F, ~F], and it was ~
built with :EXTRAPOLATION :ERROR; use :FLAT or :LINEAR to extrapolate."
                     (extrapolation-x c) (extrapolation-lo c) (extrapolation-hi c))))
  (:documentation "An interpolator built with :EXTRAPOLATION :ERROR was asked
for a value outside its knots."))

(defclass interpolator (c2mop:funcallable-standard-object)
  ((method :initarg :method :reader interpolator-method)
   (knots :initarg :knots :reader interpolator-knots :type double-vector)
   (values :initarg :values :reader interpolator-values :type double-vector)
   (extrapolation :initarg :extrapolation :reader interpolator-extrapolation)
   (derivative :initarg :derivative :reader %interpolator-derivative :type function)
   (second-derivative :initarg :second-derivative :reader %interpolator-second-derivative
                      :type function))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A one-dimensional interpolant, called as a function of x.
Made by MAKE-INTERPOLATOR."))

(defun interpolator-derivative (interpolator x)
  "dy/dx of INTERPOLATOR at X (extrapolated as its values are)."
  (funcall (%interpolator-derivative interpolator) (float x 1d0)))

(defun interpolator-second-derivative (interpolator x)
  "d2y/dx2 of INTERPOLATOR at X (extrapolated as its values are)."
  (funcall (%interpolator-second-derivative interpolator) (float x 1d0)))

(defmethod print-object ((i interpolator) stream)
  (print-unreadable-object (i stream :type t)
    (let ((xs (interpolator-knots i)))
      (format stream "~(~A~) on [~F, ~F], ~D knots" (interpolator-method i)
              (aref xs 0) (aref xs (1- (length xs))) (length xs)))))

(declaim (inline knot-segment))
(defun knot-segment (xs x)
  "The i with XS[i] <= X <= XS[i+1], for X within the knots, by bisection."
  (declare (type double-vector xs) (double-float x) (optimize (speed 3) (safety 0)))
  (let ((lo 0) (hi (1- (length xs))))
    (declare (fixnum lo hi))
    (loop while (> (- hi lo) 1)
          do (let ((mid (ash (+ lo hi) -1)))
               (declare (fixnum mid))
               (if (< x (aref xs mid)) (setf hi mid) (setf lo mid))))
    lo))

(defun as-double-vector (sequence)
  (map 'double-vector (lambda (v) (float v 1d0)) sequence))

;;; --------------------------------------------------------------------
;;; Construction
;;; --------------------------------------------------------------------

(defun make-interpolator (method xs ys &key (extrapolation :flat) (end-second-derivatives '(0 . 0)))
  "An interpolator through the knots (XS[i], YS[i]), called as (funcall
interpolator x). XS must be strictly increasing, with at least two knots.
METHOD is

  :linear          piecewise linear
  :log-linear      linear in log y (positive y): for discount factors
  :natural-cubic   C2 cubic spline with second derivatives at the ends
                   END-SECOND-DERIVATIVES, a cons (left . right), by default
                   (0 . 0): the natural spline. Given a cubic's own end values
                   it reproduces that cubic.
  :monotone-cubic  C1 Hermite cubic with three-point knot slopes (exact for
                   quadratics) passed through Hyman's filter: monotone
                   wherever the data are, no overshoot
  :flat-forward    YS are zero rates and x y is piecewise linear, so the
                   instantaneous forward d(x y)/dx is constant between knots
                   (non-negative XS)

EXTRAPOLATION is :flat (the end value), :linear (the end segment continued:
linear, log-linear or flat-forward in its own form, the tangent line for
cubics) or :error (signals EXTRAPOLATION-ERROR).

  (funcall (make-interpolator :natural-cubic '(0 1 2 3) '(0 1 8 27)) 1.5d0)"
  (let ((xs (as-double-vector xs)) (ys (as-double-vector ys)))
    (check-input method (member :linear :log-linear :natural-cubic :monotone-cubic
                                :flat-forward)
                 ":LINEAR, :LOG-LINEAR, :NATURAL-CUBIC, :MONOTONE-CUBIC or :FLAT-FORWARD")
    (check-input extrapolation (member :flat :linear :error) ":FLAT, :LINEAR or :ERROR")
    (unless (and (= (length xs) (length ys)) (>= (length xs) 2))
      (signal-invalid-argument 'ys ys 'sequence
                               (format nil "one value per knot, at least two (~D knots)"
                                       (length xs))))
    (unless (loop for i from 1 below (length xs) always (< (aref xs (1- i)) (aref xs i)))
      (signal-invalid-argument 'xs xs 'sequence "strictly increasing knots"))
    (when (and (eq method :log-linear) (notevery #'plusp ys))
      (signal-invalid-argument 'ys ys 'sequence "positive values, for :LOG-LINEAR"))
    (when (and (eq method :flat-forward) (minusp (aref xs 0)))
      (signal-invalid-argument 'xs xs 'sequence "non-negative knots, for :FLAT-FORWARD"))
    (check-input end-second-derivatives (cons real real) "a cons (left . right) of reals")
    (multiple-value-bind (value d1 d2)
        (interpolation-closures method xs ys extrapolation end-second-derivatives)
      (let ((interpolator (make-instance 'interpolator :method method :knots xs :values ys
                                                       :extrapolation extrapolation
                                                       :derivative d1 :second-derivative d2)))
        ;; The method's closure itself: no wrapper between FUNCALL and it.
        (c2mop:set-funcallable-instance-function interpolator value)
        interpolator))))

(declaim (notinline signal-extrapolation-error))
(defun signal-extrapolation-error (x xs)
  (error 'extrapolation-error :x x :lo (aref xs 0) :hi (aref xs (1- (length xs)))))

(defmacro with-extrapolation ((xs x extrapolation &key flat-left flat-right end-left end-right)
                              &body inside)
  "Evaluate INSIDE (with SEGMENT bound) for X within XS; outside, the
:FLAT form, the :LINEAR end form (also with SEGMENT bound to the end segment)
or EXTRAPOLATION-ERROR. XS, X and EXTRAPOLATION must be variables. X is
boxed only on the error path, through a separate function, so the usual path
allocates nothing beyond the result."
  `(let ((n (length ,xs)))
     (declare (fixnum n))
     (cond ((< ,x (aref ,xs 0))
            (case ,extrapolation
              (:flat ,flat-left)
              (:linear (let ((segment 0)) (declare (ignorable segment)) ,end-left))
              (t (signal-extrapolation-error ,x ,xs))))
           ((> ,x (aref ,xs (1- n)))
            (case ,extrapolation
              (:flat ,flat-right)
              (:linear (let ((segment (- n 2))) (declare (ignorable segment)) ,end-right))
              (t (signal-extrapolation-error ,x ,xs))))
           (t (let ((segment (knot-segment ,xs ,x))) (declare (fixnum segment)) ,@inside)))))

(defun interpolation-closures (method xs ys extrapolation end-second-derivatives)
  "(values value d1 d2): closures of a double-float x for METHOD."
  (declare (type double-vector xs ys))
  (ecase method
    ((:linear :log-linear :flat-forward) (segment-closures method xs ys extrapolation))
    (:natural-cubic (multiple-value-call #'cubic-closures xs ys extrapolation
                      (natural-cubic-coefficients xs ys (float (car end-second-derivatives) 1d0)
                                                  (float (cdr end-second-derivatives) 1d0))))
    (:monotone-cubic (multiple-value-call #'cubic-closures xs ys extrapolation
                       (monotone-cubic-coefficients xs ys)))))

;;; --------------------------------------------------------------------
;;; Linear, log-linear, flat-forward
;;; --------------------------------------------------------------------

(defun segment-closures (method xs ys extrapolation)
  "Methods linear in some transform g of y: g = y, log y or x y. :LINEAR
extrapolation continues the end segment's line in g."
  (declare (type double-vector xs ys))
  (let* ((n (length xs))
         (gs (ecase method
               (:linear ys)
               (:log-linear (map 'double-vector #'log ys))
               (:flat-forward (map 'double-vector #'* xs ys))))
         (slopes (let ((v (make-array (1- n) :element-type 'double-float)))
                   (dotimes (i (1- n) v)
                     (setf (aref v i) (/ (- (aref gs (1+ i)) (aref gs i))
                                         (- (aref xs (1+ i)) (aref xs i)))))))
         (y-first (aref ys 0)) (y-last (aref ys (1- n))))
    (declare (type double-vector gs slopes) (fixnum n) (double-float y-first y-last))
    (macrolet ((define (transform)
                 ;; TRANSFORM computes (values y y' y'') from G and SLOPE at X.
                 `(flet ((at (segment x)
                           (declare (fixnum segment) (double-float x)
                                    (optimize (speed 3) (safety 0)))
                           (let ((g (+ (aref gs segment)
                                       (* (aref slopes segment) (- x (aref xs segment)))))
                                 (slope (aref slopes segment)))
                             (declare (double-float g slope) (ignorable slope))
                             ,transform)))
                    (declare (inline at))
                    (flet ((closure (order flat-left flat-right)
                             (declare (fixnum order) (double-float flat-left flat-right))
                             (lambda (x)
                               (let ((x (if (typep x 'double-float) x (float x 1d0))))
                               (declare (double-float x) (optimize (speed 3) (safety 0)))
                               (with-extrapolation (xs x extrapolation
                                                    :flat-left flat-left :flat-right flat-right
                                                    :end-left (nth-value order (at segment x))
                                                    :end-right (nth-value order (at segment x)))
                                 (nth-value order (at segment x)))))))
                      (values (closure 0 y-first y-last) (closure 1 0d0 0d0)
                              (closure 2 0d0 0d0))))))
      (ecase method
        (:linear (define (values g slope 0d0)))
        (:log-linear (define (let ((y (exp g))) (values y (* y slope) (* y slope slope)))))
        (:flat-forward
         ;; y = g / x, y' = (slope - y) / x, y'' = -2 y' / x; at x = 0 the
         ;; limits along the segment: y = slope, derivatives 0.
         (define (if (zerop x)
                     (values slope 0d0 0d0)
                     (let* ((y (/ g x)) (dy (/ (- slope y) x)))
                       (values y dy (/ (* -2 dy) x))))))))))

;;; --------------------------------------------------------------------
;;; Cubics
;;; --------------------------------------------------------------------

(defun natural-cubic-coefficients (xs ys left right)
  "(values c1 c2 c3) per segment of the cubic spline with end second
derivatives LEFT and RIGHT (0 for the natural spline): second derivatives M
from the tridiagonal system h_{i-1} M_{i-1} + 2 (h_{i-1} + h_i) M_i + h_i
M_{i+1} = 6 (slope_i - slope_{i-1}), M_0 = LEFT, M_{n-1} = RIGHT."
  (declare (type double-vector xs ys) (double-float left right))
  (let* ((n (length xs))
         (m (make-array n :element-type 'double-float :initial-element 0d0))
         (interior (- n 2)))
    (setf (aref m 0) left (aref m (1- n)) right)
    (flet ((h (i) (- (aref xs (1+ i)) (aref xs i)))
           (slope (i) (/ (- (aref ys (1+ i)) (aref ys i)) (- (aref xs (1+ i)) (aref xs i)))))
      (when (plusp interior)
        (let ((a (make-array interior :element-type 'double-float))
              (b (make-array interior :element-type 'double-float))
              (c (make-array interior :element-type 'double-float))
              (d (make-array interior :element-type 'double-float)))
          (dotimes (k interior)
            (let ((i (1+ k)))
              (setf (aref a k) (h (1- i))
                    (aref b k) (* 2 (+ (h (1- i)) (h i)))
                    (aref c k) (h i)
                    (aref d k) (* 6 (- (slope i) (slope (1- i)))))))
          ;; The known end values move to the right-hand side.
          (decf (aref d 0) (* (h 0) left))
          (decf (aref d (1- interior)) (* (h (- n 2)) right))
          (replace m (solve-tridiagonal! a b c d) :start1 1)))
      (let ((c1 (make-array (1- n) :element-type 'double-float))
            (c2 (make-array (1- n) :element-type 'double-float))
            (c3 (make-array (1- n) :element-type 'double-float)))
        (dotimes (i (1- n) (values c1 c2 c3))
          (let ((h (h i)))
            (setf (aref c1 i) (- (slope i) (/ (* h (+ (* 2 (aref m i)) (aref m (1+ i)))) 6))
                  (aref c2 i) (/ (aref m i) 2)
                  (aref c3 i) (/ (- (aref m (1+ i)) (aref m i)) (* 6 h)))))))))

(defun monotone-cubic-coefficients (xs ys)
  "(values c1 c2 c3) of the Hermite cubic whose knot slopes are three-point
estimates passed through Hyman's filter (Fritsch and Carlson's sufficient
condition: slopes of the data's sign, at most three times the smaller
adjacent secant; zero at local extrema)."
  (declare (type double-vector xs ys))
  (let* ((n (length xs))
         (h (make-array (1- n) :element-type 'double-float))
         (delta (make-array (1- n) :element-type 'double-float))
         (d (make-array n :element-type 'double-float)))
    (dotimes (i (1- n))
      (setf (aref h i) (- (aref xs (1+ i)) (aref xs i))
            (aref delta i) (/ (- (aref ys (1+ i)) (aref ys i)) (aref h i))))
    ;; Three-point slope estimates (one-sided at the ends).
    (if (= n 2)
        (setf (aref d 0) (aref delta 0) (aref d 1) (aref delta 0))
        (progn
          (loop for i from 1 below (1- n)
                do (setf (aref d i) (/ (+ (* (aref h i) (aref delta (1- i)))
                                          (* (aref h (1- i)) (aref delta i)))
                                       (+ (aref h (1- i)) (aref h i)))))
          (setf (aref d 0) (/ (- (* (+ (* 2 (aref h 0)) (aref h 1)) (aref delta 0))
                                 (* (aref h 0) (aref delta 1)))
                              (+ (aref h 0) (aref h 1)))
                (aref d (1- n)) (let ((k (- n 2)))
                                  (/ (- (* (+ (* 2 (aref h k)) (aref h (1- k))) (aref delta k))
                                        (* (aref h k) (aref delta (1- k))))
                                     (+ (aref h k) (aref h (1- k))))))))
    ;; Hyman's filter.
    (flet ((limit (slope left right)
             ;; LEFT and RIGHT are the adjacent secants (equal at the ends).
             (if (<= (* left right) 0d0)
                 0d0
                 (let ((bound (* 3 (min (abs left) (abs right)))))
                   (if (plusp right)
                       (min (max slope 0d0) bound)
                       (max (min slope 0d0) (- bound)))))))
      (dotimes (i n)
        (setf (aref d i) (limit (aref d i)
                                (aref delta (max 0 (1- i)))
                                (aref delta (min i (- n 2)))))))
    (let ((c1 (make-array (1- n) :element-type 'double-float))
          (c2 (make-array (1- n) :element-type 'double-float))
          (c3 (make-array (1- n) :element-type 'double-float)))
      (dotimes (i (1- n) (values c1 c2 c3))
        (let ((hi (aref h i)) (s (aref delta i)) (d0 (aref d i)) (d1 (aref d (1+ i))))
          (setf (aref c1 i) d0
                (aref c2 i) (/ (- (* 3 s) (* 2 d0) d1) hi)
                (aref c3 i) (/ (+ d0 d1 (* -2 s)) (* hi hi))))))))

(defun cubic-closures (xs ys extrapolation c1 c2 c3)
  "Value and derivative closures of the piecewise cubic y_i + c1 t + c2 t^2 +
c3 t^3; :LINEAR extrapolation is the tangent line at the end knot."
  (declare (type double-vector xs ys c1 c2 c3))
  (progn
    (let* ((n (length xs))
           (x-first (aref xs 0)) (x-last (aref xs (1- n)))
           (y-first (aref ys 0)) (y-last (aref ys (1- n)))
           (last (- n 2))
           (h-last (- x-last (aref xs last)))
           (slope-first (aref c1 0))
           (slope-last (+ (aref c1 last) (* h-last (+ (* 2 (aref c2 last))
                                                      (* 3 (aref c3 last) h-last))))))
      (declare (fixnum n last) (double-float x-first x-last y-first y-last h-last slope-first
                                            slope-last))
      (macrolet ((closure (inside flat linear-left linear-right)
                   `(lambda (x)
                      (let ((x (if (typep x 'double-float) x (float x 1d0))))
                      (declare (double-float x) (optimize (speed 3) (safety 0)))
                      (with-extrapolation (xs x extrapolation
                                           :flat-left ,(first flat) :flat-right ,(second flat)
                                           :end-left ,linear-left :end-right ,linear-right)
                        (let ((tt (- x (aref xs segment)))
                              (a1 (aref c1 segment)) (a2 (aref c2 segment))
                              (a3 (aref c3 segment)))
                          (declare (double-float tt a1 a2 a3) (ignorable a1))
                          ,inside))))))
        (values
         (closure (+ (aref ys segment) (* tt (+ a1 (* tt (+ a2 (* tt a3))))))
                  (y-first y-last)
                  (+ y-first (* slope-first (- x x-first)))
                  (+ y-last (* slope-last (- x x-last))))
         (closure (+ a1 (* tt (+ (* 2 a2) (* 3 a3 tt))))
                  (0d0 0d0) slope-first slope-last)
         (closure (+ (* 2 a2) (* 6 a3 tt))
                  (0d0 0d0) 0d0 0d0))))))

;;; --------------------------------------------------------------------
;;; Two dimensions
;;; --------------------------------------------------------------------

(defclass interpolator-2d (c2mop:funcallable-standard-object)
  ((method :initarg :method :reader interpolator-method)
   (xs :initarg :xs :reader interpolator-x-knots)
   (ys :initarg :ys :reader interpolator-y-knots))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A two-dimensional interpolant, called as (funcall interp x y).
Made by MAKE-INTERPOLATOR-2D."))

(defun make-interpolator-2d (method xs ys zs &key (extrapolation :flat))
  "An interpolator through Z[i, j] at (XS[i], YS[j]), called as (funcall
interpolator x y). ZS is a 2-D array (or list of rows) of NX x NY values.
METHOD is :bilinear, or :bicubic: a natural cubic spline in y along each row
of knots, then a natural cubic spline in x through those values.
EXTRAPOLATION is :flat (clamp to the knot rectangle) or :error.

  (funcall (make-interpolator-2d :bilinear '(0 1) '(0 1) '((0 1) (1 2))) 0.5 0.5) => 1.0d0"
  (let* ((xs (as-double-vector xs)) (ys (as-double-vector ys))
         (nx (length xs)) (ny (length ys))
         (zs (if (listp zs)
                 (make-array (list nx ny) :element-type 'double-float
                                          :initial-contents (mapcar (lambda (row)
                                                                      (mapcar (lambda (z) (float z 1d0)) row))
                                                                    zs))
                 (let ((a (make-array (list nx ny) :element-type 'double-float)))
                   (dotimes (i nx a) (dotimes (j ny) (setf (aref a i j) (float (aref zs i j) 1d0))))))))
    (check-input method (member :bilinear :bicubic) ":BILINEAR or :BICUBIC")
    (check-input extrapolation (member :flat :error) ":FLAT or :ERROR")
    (dolist (knots (list xs ys))
      (unless (and (>= (length knots) 2)
                   (loop for i from 1 below (length knots) always (< (aref knots (1- i)) (aref knots i))))
        (signal-invalid-argument 'knots knots 'sequence "at least two strictly increasing knots")))
    (flet ((clamp (v knots)
             (let ((lo (aref knots 0)) (hi (aref knots (1- (length knots)))))
               (cond ((<= lo v hi) v)
                     ((eq extrapolation :error)
                      (error 'extrapolation-error :x v :lo lo :hi hi))
                     (t (max lo (min hi v)))))))
      (let* ((rows (and (eq method :bicubic)
                        (loop for i below nx
                              collect (make-interpolator :natural-cubic ys
                                                         (loop for j below ny collect (aref zs i j))))))
             (function
               (ecase method
                 (:bilinear
                  (lambda (x y)
                    (let* ((x (clamp (float x 1d0) xs)) (y (clamp (float y 1d0) ys))
                           (i (knot-segment xs x)) (j (knot-segment ys y))
                           (u (/ (- x (aref xs i)) (- (aref xs (1+ i)) (aref xs i))))
                           (v (/ (- y (aref ys j)) (- (aref ys (1+ j)) (aref ys j)))))
                      (+ (* (- 1 u) (- 1 v) (aref zs i j)) (* u (- 1 v) (aref zs (1+ i) j))
                         (* (- 1 u) v (aref zs i (1+ j))) (* u v (aref zs (1+ i) (1+ j)))))))
                 (:bicubic
                  (lambda (x y)
                    (let ((x (clamp (float x 1d0) xs)) (y (clamp (float y 1d0) ys)))
                      (funcall (make-interpolator :natural-cubic xs
                                                  (mapcar (lambda (row) (funcall row y)) rows))
                               x)))))))
        (let ((interpolator (make-instance 'interpolator-2d :method method :xs xs :ys ys)))
          (c2mop:set-funcallable-instance-function interpolator function)
          interpolator)))))
