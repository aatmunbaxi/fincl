;;;; engines/fd-heston.lisp --- Heston by alternating-direction implicit FD
;;;;
;;;; V(t, x, v), x = ln S, solves V_t + A V = 0 with A = A0 + A1 + A2:
;;;;
;;;;   A0 V = rho xi v V_xv                                     (mixed, explicit)
;;;;   A1 V = 1/2 v V_xx + (g - v/2) V_x - r/2 V                (x, implicit)
;;;;   A2 V = 1/2 xi^2 v V_vv + kappa (theta - v) V_v - r/2 V   (v, implicit)
;;;;
;;;; g the forward's log-growth rate and r the short rate over the step.
;;;; Hundsdorfer-Verwer's scheme (In 't Hout and Foulon 2010), theta = 1/2 +
;;;; sqrt(3)/6, after RANNACHER-STEPS Douglas (theta = 1) substeps at the
;;;; payoff and each Bermudan date; American exercise by Ikonen-Toivanen
;;;; splitting. Boundaries: V linear in S at both x ends; at v = 0 the PDE
;;;; with its diffusion gone (a second-order one-sided V_v); V_v = 0 at the
;;;; largest v. The
;;;; variance grid is In 't Hout and Foulon's, v_j = d sinh(j h), dense near
;;;; 0, up to max(5, 10 max(v0, theta)).

(in-package #:fincl)

(defstruct (heston-grid (:constructor %make-heston-grid))
  (xs nil :type double-vector) (vs nil :type double-vector)
  ;; Three-point first and second derivative weights, per x node and per v node.
  (x1 nil :type (simple-array double-float (* 3))) (x2 nil :type (simple-array double-float (* 3)))
  (v1 nil :type (simple-array double-float (* 3))) (v2 nil :type (simple-array double-float (* 3))))

(defun three-point-weights (nodes order)
  "Weights over (i-1, i, i+1) at each interior node of NODES for the ORDER-th
derivative, as an (n x 3) array; zero rows at the ends."
  (let* ((n (length nodes))
         (w (make-array `(,n 3) :element-type 'double-float :initial-element 0d0)))
    (iter (for i from 1 below (1- n))
          (let ((weights (finite-difference-weights
                          `(,(- (aref nodes (1- i)) (aref nodes i)) 0d0
                            ,(- (aref nodes (1+ i)) (aref nodes i)))
                          order)))
            (dotimes (k 3) (setf (aref w i k) (float (aref weights k) 1d0)))))
    w))

(defun make-heston-grid (xs v-max n-v)
  (let* ((d (/ v-max 500d0))
         (h (/ (asinh (/ v-max d)) (1- n-v)))
         (vs (make-array n-v :element-type 'double-float)))
    (dotimes (j n-v) (setf (aref vs j) (* d (sinh (* j h)))))
    (%make-heston-grid :xs xs :vs vs
                       :x1 (three-point-weights xs 1) :x2 (three-point-weights xs 2)
                       :v1 (three-point-weights vs 1) :v2 (three-point-weights vs 2))))

(defstruct (heston-operators (:constructor %make-heston-operators))
  "Tridiagonal coefficients of A1 (along x) and A2 (along v) at every node,
and the mixed term's coefficient rho xi v_j."
  (a1 nil :type (simple-array double-float (* 3)))
  (a2 nil :type (simple-array double-float (* 3)))
  (mixed nil :type double-vector))

(defun heston-operators (grid kappa theta xi rho growth rate)
  (let* ((xs (heston-grid-xs grid)) (vs (heston-grid-vs grid))
         (nx (length xs)) (nv (length vs)) (n (* nx nv))
         (x1 (heston-grid-x1 grid)) (x2 (heston-grid-x2 grid))
         (v1 (heston-grid-v1 grid)) (v2 (heston-grid-v2 grid))
         (a1 (make-array `(,n 3) :element-type 'double-float :initial-element 0d0))
         (a2 (make-array `(,n 3) :element-type 'double-float :initial-element 0d0))
         (mixed (make-array n :element-type 'double-float :initial-element 0d0))
         (half-r (* 0.5d0 rate)))
    (dotimes (j nv)
      (let* ((v (aref vs j)) (mu (- growth (* 0.5d0 v))))
        (dotimes (i nx)
          (let ((node (+ i (* j nx))))
            ;; A1 along x.
            (cond ((< 0 i (1- nx))
                   (dotimes (k 3)
                     (setf (aref a1 node k) (+ (* 0.5d0 v (aref x2 i k)) (* mu (aref x1 i k)))))
                   (decf (aref a1 node 1) half-r))
                  (t
                   ;; V linear in S: 1/2 v V_xx + mu V_x = g V_x, one-sided.
                   (let ((slope (/ growth (if (zerop i)
                                              (- (aref xs 1) (aref xs 0))
                                              (- (aref xs i) (aref xs (1- i)))))))
                     (if (zerop i)
                         (setf (aref a1 node 1) (- (- slope) half-r) (aref a1 node 2) slope)
                         (setf (aref a1 node 0) (- slope) (aref a1 node 1) (- slope half-r))))))
            ;; A2 along v.
            (cond ((< 0 j (1- nv))
                   (dotimes (k 3)
                     (setf (aref a2 node k) (+ (* 0.5d0 xi xi v (aref v2 j k))
                                               (* kappa (- theta v) (aref v1 j k)))))
                   (decf (aref a2 node 1) half-r))
                  ((zerop j)
                   ;; v = 0: kappa theta V_v, one-sided over v0, v1, v2 (second
                   ;; order). Column 0, unused at j = 0, holds the v2 weight.
                   (let ((w (finite-difference-weights `(0d0 ,(aref vs 1) ,(aref vs 2)) 1))
                         (kt (* kappa theta)))
                     (setf (aref a2 node 1) (- (* kt (aref w 0)) half-r)
                           (aref a2 node 2) (* kt (aref w 1))
                           (aref a2 node 0) (* kt (aref w 2)))))
                  (t
                   ;; Largest v: V_v = 0.
                   (setf (aref a2 node 1) (- half-r))))
            (when (and (< 0 i (1- nx)) (< 0 j (1- nv)))
              (setf (aref mixed node) (* rho xi v)))))))
    (%make-heston-operators :a1 a1 :a2 a2 :mixed mixed)))

(defun apply-a1 (ops nx nv u out)
  "OUT = A1 U."
  (declare (type double-vector u out) (fixnum nx nv) (optimize (speed 3) (safety 0)))
  (let ((a1 (heston-operators-a1 ops)))
    (declare (type (simple-array double-float (* 3)) a1))
    (dotimes (j nv out)
      (dotimes (i nx)
        (let ((node (+ i (the fixnum (* j nx)))))
          (setf (aref out node)
                (+ (* (aref a1 node 1) (aref u node))
                   (if (> i 0) (* (aref a1 node 0) (aref u (1- node))) 0d0)
                   (if (< i (1- nx)) (* (aref a1 node 2) (aref u (1+ node))) 0d0))))))))

(defun apply-a2 (ops nx nv u out)
  "OUT = A2 U."
  (declare (type double-vector u out) (fixnum nx nv) (optimize (speed 3) (safety 0)))
  (let ((a2 (heston-operators-a2 ops)))
    (declare (type (simple-array double-float (* 3)) a2))
    (dotimes (j nv out)
      (dotimes (i nx)
        (let ((node (+ i (the fixnum (* j nx)))))
          (setf (aref out node)
                (+ (* (aref a2 node 1) (aref u node))
                   ;; At j = 0 column 0 multiplies the value at j = 2.
                   (if (> j 0)
                       (* (aref a2 node 0) (aref u (- node nx)))
                       (* (aref a2 node 0) (aref u (+ node nx nx))))
                   (if (< j (1- nv)) (* (aref a2 node 2) (aref u (+ node nx))) 0d0))))))))

(defun apply-a0 (ops grid nx nv u out)
  "OUT = A0 U, the mixed derivative by the product of the central first-
derivative weights."
  (declare (type double-vector u out) (fixnum nx nv) (optimize (speed 3) (safety 0)))
  (let ((mixed (heston-operators-mixed ops))
        (x1 (heston-grid-x1 grid)) (v1 (heston-grid-v1 grid)))
    (declare (type double-vector mixed) (type (simple-array double-float (* 3)) x1 v1))
    (fill out 0d0)
    (iter (declare (declare-variables))
          (for (the fixnum j) from 1 below (1- nv))
          (iter (declare (declare-variables))
                (for (the fixnum i) from 1 below (1- nx))
                (let* ((node (+ i (the fixnum (* j nx))))
                       (sum 0d0))
                  (declare (double-float sum) (fixnum node))
                  (dotimes (b 3)
                    (dotimes (a 3)
                      (incf sum (* (aref x1 i a) (aref v1 j b)
                                   (aref u (+ node (- a 1) (the fixnum (* (- b 1) nx))))))))
                  (setf (aref out node) (* (aref mixed node) sum)))))
    out))

(defun solve-a1 (ops nx nv weight rhs out wa wb wc wd)
  "OUT = (I - WEIGHT A1)^-1 RHS, line by line along x."
  (declare (type double-vector rhs out wa wb wc wd) (fixnum nx nv) (double-float weight))
  (let ((a1 (heston-operators-a1 ops)))
    (declare (type (simple-array double-float (* 3)) a1))
    (dotimes (j nv out)
      (dotimes (i nx)
        (let ((node (+ i (* j nx))))
          (setf (aref wa i) (* (- weight) (aref a1 node 0))
                (aref wb i) (- 1d0 (* weight (aref a1 node 1)))
                (aref wc i) (* (- weight) (aref a1 node 2))
                (aref wd i) (aref rhs node))))
      (solve-tridiagonal! wa wb wc wd)
      (dotimes (i nx) (setf (aref out (+ i (* j nx))) (aref wd i))))))

(defun solve-a2 (ops nx nv weight rhs out wa wb wc wd)
  "OUT = (I - WEIGHT A2)^-1 RHS, line by line along v."
  (declare (type double-vector rhs out wa wb wc wd) (fixnum nx nv) (double-float weight))
  (let ((a2 (heston-operators-a2 ops)))
    (declare (type (simple-array double-float (* 3)) a2))
    (dotimes (i nx out)
      (dotimes (j nv)
        (let ((node (+ i (* j nx))))
          (setf (aref wa j) (* (- weight) (aref a2 node 0))
                (aref wb j) (- 1d0 (* weight (aref a2 node 1)))
                (aref wc j) (* (- weight) (aref a2 node 2))
                (aref wd j) (aref rhs node))))
      ;; Row 0 also has a term in U_2 (coefficient WA[0]); eliminate it with
      ;; row 1 to keep the system tridiagonal.
      (let ((e (aref wa 0)) (ratio 0d0))
        (unless (zerop e)
          (setf ratio (/ e (aref wc 1))
                (aref wb 0) (- (aref wb 0) (* ratio (aref wa 1)))
                (aref wc 0) (- (aref wc 0) (* ratio (aref wb 1)))
                (aref wd 0) (- (aref wd 0) (* ratio (aref wd 1)))
                (aref wa 0) 0d0)))
      (solve-tridiagonal! wa wb wc wd)
      (dotimes (j nv) (setf (aref out (+ i (* j nx))) (aref wd j))))))

(defun heston-adi-solve (x p process e market)
  "Value of the option (X P, path-independent) under the resolved Heston
PROCESS: (values price delta gamma theta-per-day)."
  (let* ((tau (time-to-expiry x market))
         (s0 (spot market)) (x0 (log s0))
         (kappa (kappa process)) (theta (theta process)) (xi (xi process))
         (rho (rho process)) (v0 (v0 process))
         (grid-times (time-grid-times (make-time-grid market :dates (exercise-mandatory-dates x)
                                                             :n-steps (n-time e))))
         (mask (exercise-mask x market grid-times))
         (american (every (lambda (b) (= b 1)) mask))
         (european (typep x 'european-exercise))
         (scale (sqrt (* (max v0 theta 1d-4) tau)))
         (width (* (float (grid-width e) 1d0) scale))
         (log-forward (log (forward market tau)))
         (xs (log-spot-grid (- (min x0 log-forward) width) (+ (max x0 log-forward) width)
                            (if (typep p 'strike-payoff) (log (strike p)) x0) (n-space e)
                            :alpha (and (concentration e) (* (float (concentration e) 1d0) scale))))
         (nx (length xs)) (nv (n-variance e)) (n (* nx nv))
         (grid (make-heston-grid xs (max 5d0 (* 10d0 (max v0 theta))) nv))
         (kernel (payoff-kernel p))
         (payoff (let ((g (make-array n :element-type 'double-float)))
                   (dotimes (j nv g)
                     (dotimes (i nx) (setf (aref g (+ i (* j nx))) (funcall kernel (exp (aref xs i))))))))
         (u (copy-seq payoff))
         (lambdas (and american (make-array n :element-type 'double-float :initial-element 0d0)))
         (vectors (iter (repeat 9) (collect (make-array n :element-type 'double-float :initial-element 0d0))))
         (work (iter (repeat 4) (collect (make-array (max nx nv) :element-type 'double-float))))
         (line-x (iter (repeat 4) (collect (make-array nx :element-type 'double-float))))
         (line-v (iter (repeat 4) (collect (make-array nv :element-type 'double-float))))
         (snapshot nil))
    (declare (ignore work))
    (destructuring-bind (fu y0 y1 y2 a1u a2u fy2 tmp tmp2) vectors
      (labels ((full (ops v out)
                 ;; OUT = (A0 + A1 + A2) V.
                 (apply-a0 ops grid nx nv v out)
                 (apply-a1 ops nx nv v tmp)
                 (dotimes (k n) (incf (aref out k) (aref tmp k)))
                 (apply-a2 ops nx nv v tmp)
                 (dotimes (k n) (incf (aref out k) (aref tmp k)))
                 out)
               (solve-x (ops weight rhs out) (apply #'solve-a1 ops nx nv weight rhs out line-x))
               (solve-v (ops weight rhs out) (apply #'solve-a2 ops nx nv weight rhs out line-v))
               (adi-step (ops dt douglas)
                 ;; One Douglas (DOUGLAS true, theta = 1) or Hundsdorfer-Verwer
                 ;; step of U over DT, with the Ikonen-Toivanen source.
                 (let ((th (if douglas 1d0 (+ 0.5d0 (/ (sqrt 3d0) 6d0)))))
                   (full ops u fu)
                   (dotimes (k n)
                     (setf (aref y0 k) (+ (aref u k) (* dt (aref fu k))
                                          (if lambdas (* dt (aref lambdas k)) 0d0))))
                   (apply-a1 ops nx nv u a1u)
                   (dotimes (k n) (setf (aref tmp2 k) (- (aref y0 k) (* th dt (aref a1u k)))))
                   (solve-x ops (* th dt) tmp2 y1)
                   (apply-a2 ops nx nv u a2u)
                   (dotimes (k n) (setf (aref tmp2 k) (- (aref y1 k) (* th dt (aref a2u k)))))
                   (solve-v ops (* th dt) tmp2 y2)
                   (unless douglas
                     ;; Second stage: Y0~ = Y0 + dt/2 (F Y2 - F U), then the same
                     ;; two implicit corrections, now about Y2.
                     (full ops y2 fy2)
                     (dotimes (k n)
                       (setf (aref y0 k) (+ (aref y0 k) (* 0.5d0 dt (- (aref fy2 k) (aref fu k))))))
                     (apply-a1 ops nx nv y2 a1u)
                     (dotimes (k n) (setf (aref tmp2 k) (- (aref y0 k) (* th dt (aref a1u k)))))
                     (solve-x ops (* th dt) tmp2 y1)
                     (apply-a2 ops nx nv y2 a2u)
                     (dotimes (k n) (setf (aref tmp2 k) (- (aref y1 k) (* th dt (aref a2u k)))))
                     (solve-v ops (* th dt) tmp2 y2))
                   (if lambdas
                       (dotimes (k n)
                         (let ((tilde (aref y2 k)) (g (aref payoff k)) (l (aref lambdas k)))
                           (setf (aref u k) (max (- tilde (* dt l)) g)
                                 (aref lambdas k) (max 0d0 (+ l (/ (- g tilde) dt))))))
                       (replace u y2)))))
        (iter (for j from (- (length grid-times) 2) downto 0)
              (let* ((t0 (aref grid-times j)) (t1 (aref grid-times (1+ j)))
                     (dt (- t1 t0))
                     (ops (heston-operators grid kappa theta xi rho
                                            (/ (log-growth market t0 t1) dt) (step-rate market t0 t1)))
                     (smooth (and (plusp (rannacher-steps e))
                                  (or (= (1+ j) (1- (length grid-times)))
                                      (and (not american) (not european) (= 1 (sbit mask (1+ j))))))))
                (if smooth
                    (let ((k (rannacher-steps e)))
                      (dotimes (s k) (adi-step ops (/ dt k) t)))
                    (adi-step ops dt nil))
                (when (and (= 1 (sbit mask j)) (not american) (not european))
                  (dotimes (k n) (setf (aref u k) (max (aref u k) (aref payoff k)))))
                (when (= j 1) (setf snapshot (copy-seq u)))))))
    ;; Value at (x0, v0): Lagrange in v on the four nearest variance nodes,
    ;; then in x as on the 1-D grid.
    (flet ((row-at-v0 (values)
             (let* ((vs (heston-grid-vs grid))
                    (jv (max 0 (min (- nv 4) (1- (or (position-if (lambda (v) (> v v0)) vs) nv)))))
                    (weights (finite-difference-weights
                              (iter (for j from jv below (+ jv 4)) (collect (- (aref vs j) v0))) 0))
                    (row (make-array nx :element-type 'double-float :initial-element 0d0)))
               (dotimes (i nx row)
                 (iter (for b from 0 below 4)
                       (incf (aref row i) (* (aref weights b) (aref values (+ i (* (+ jv b) nx))))))))))
      (multiple-value-bind (v vx vxx) (grid-value xs (row-at-v0 u) x0 0 (1- nx))
        (values v (/ vx s0) (/ (- vxx vx) (* s0 s0))
                (* (one-day market)
                   (/ (- (grid-value xs (row-at-v0 snapshot) x0 0 (1- nx)) v) (aref grid-times 1))))))))

(defmethod price-fd ((x exercise) (p payoff) (f path-independent) (process heston)
                     (e fd-engine) market)
  (values (heston-adi-solve x p (resolve-process process market (time-to-expiry x market)) e market)
          nil))

(defmethod fd-node-greeks (x p f (process heston) (e fd-engine) market)
  (declare (ignore f))
  (heston-adi-solve x p process e market))

;;; Bates adds jumps, which a local PDE does not model.
(defmethod price-fd ((x exercise) (p payoff) (f path-independent) (process bates)
                     (e fd-engine) market)
  (error 'unsupported-combination :exercise x :payoff p :path f :process process
                                  :engine e :market market))

(defmethod engine-candidates append ((designator (eql :fd)) (x american-exercise) (p vanilla-payoff)
                                     (f path-independent) (process heston))
  (unless (typep process 'bates) '((15 :fd))))

(defmethod engine-candidates append ((designator (eql :fd)) (x bermudan-exercise) (p vanilla-payoff)
                                     (f path-independent) (process heston))
  (unless (typep process 'bates) '((15 :fd))))
