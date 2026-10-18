;;;; processes/local-vol.lisp --- Dupire local volatility
;;;;
;;;;   dS/S = (r - q) dt + sigma_loc(S, t) dW
;;;;
;;;; sigma_loc comes from the implied total variance w(k, T), k = ln(K/F(T)),
;;;; by Gatheral's form of Dupire's formula:
;;;;
;;;;   sigma_loc^2 = dw/dT / (1 - k/w dw/dk + 1/4 (-1/4 - 1/w + k^2/w^2) (dw/dk)^2
;;;;                          + 1/2 d2w/dk2)
;;;;
;;;; The stepper integrates the numerator over each step: the variance of ln S
;;;; over [t_j, t_j+1] at moneyness k is (w(k, t_j+1) - w(k, t_j)) / D(k,
;;;; t_mid), D the denominator at mid-step. It is tabulated on a k grid per
;;;; step when the stepper is built and read by linear interpolation at the
;;;; step's starting moneyness (Euler in ln S). Each grid step is SUBSTEPS
;;;; Euler steps, each with its own normal. On a surface flat in k, D = 1,
;;;; and with one substep the steps are GBM's forward variances exactly.

(in-package #:fincl)

(define-process local-vol (stochastic-process) ()
  ((surface nil :parameter nil :type (or null black-vol-structure)
                :reader local-vol-surface
                :doc "The implied-vol surface the local vol is consistent with.
NIL uses the market's vol structure for the underlying.")
   (substeps 4 :parameter nil :type (integer 1) :reader local-vol-substeps
             :doc "Euler steps per grid step, each with its own normal: the
weak error is O(dt) in the substep. With 1, a surface flat in strike gives
GBM's paths exactly."))
  (:documentation "Dupire local volatility consistent with an implied-vol
surface: Monte Carlo prices of Europeans reproduce the surface."))

(defparameter *local-vol-k-points* 401
  "Log-moneyness points per step on which local variances are tabulated.")

(defun market-vol-structure (market)
  "The vol structure MARKET's protocol reads, when MARKET holds one."
  (typecase market
    (market-view (view-vol market))
    (composite-market (view-vol (market-view market)))
    (t nil)))

(defun variance-function (process market)
  "(lambda (k horizon)) => (values w dw/dk d2w/dk2) for PROCESS's surface on
MARKET: analytic for a moneyness surface; otherwise the market's Black
variance at the strike F e^k, differentiated by central differences."
  (let ((surface (or (local-vol-surface process) (market-vol-structure market))))
    (if (typep surface 'moneyness-vol-structure)
        (lambda (k horizon)
          (multiple-value-bind (w wk wkk) (surface-variance-derivatives surface k horizon)
            (values w wk wkk)))
        (let ((h 1d-4)
              (w1 (finite-difference-weights '(-1 0 1) 1))
              (w2 (finite-difference-weights '(-1 0 1) 2)))
          (lambda (k horizon)
            (let* ((f (forward market horizon))
                   (values (mapcar (lambda (i)
                                     (let ((strike (* f (exp (+ k (* i h))))))
                                       (if surface
                                           (structure-black-variance surface strike horizon f)
                                           (black-variance market strike horizon))))
                                   '(-1 0 1))))
              (values (second values) (apply-stencil w1 values h 1) (apply-stencil w2 values h 2))))))))

(defun dupire-denominator (k w wk wkk)
  (+ (- 1d0 (* (/ k w) wk))
     (* 0.25d0 (+ -0.25d0 (- (/ w)) (/ (* k k) (* w w))) wk wk)
     (* 0.5d0 wkk)))

(defun local-variance-table (process market times)
  "(values ks table): KS the log-moneyness grid, TABLE[j, i] the variance of
ln S over step j at KS[i]."
  (let* ((variance (variance-function process market))
         (n (1- (length times)))
         (horizon (aref times n))
         (half-width (* 8d0 (sqrt (max (funcall variance 0d0 horizon) 1d-6))))
         (nk *local-vol-k-points*)
         (ks (iter (for i below nk)
                   (collect-double (- (* 2d0 half-width (/ i (1- nk))) half-width) into v length nk)
                   (finally (return v))))
         (table (make-array `(,n ,nk) :element-type 'double-float)))
    (dotimes (j n)
      (let ((t0 (aref times j)) (t1 (aref times (1+ j))))
        (dotimes (i nk)
          (let* ((k (aref ks i))
                 (numerator (- (funcall variance k t1)
                               (if (zerop t0) 0d0 (funcall variance k t0))))
                 (denominator (multiple-value-bind (w wk wkk) (funcall variance k (* 0.5d0 (+ t0 t1)))
                                (dupire-denominator k w wk wkk))))
            (when (minusp numerator)
              (error 'calendar-arbitrage :t0 t0 :t1 t1 :variance numerator))
            (unless (plusp denominator)
              (error 'butterfly-arbitrage :smile (or (local-vol-surface process) "the market's vol")
                                          :k k :density denominator))
            (setf (aref table j i) (/ numerator denominator))))))
    (values ks table)))

(defun refine-times (times substeps)
  "TIMES with every interval split into SUBSTEPS equal parts, the original
times kept exactly."
  (let* ((n (1- (length times)))
         (fine (make-array (1+ (* n substeps)) :element-type 'double-float)))
    (dotimes (j n fine)
      (let ((t0 (aref times j)) (t1 (aref times (1+ j))))
        (dotimes (i substeps)
          (setf (aref fine (+ (* j substeps) i))
                (if (zerop i) t0 (+ t0 (* i (/ (- t1 t0) substeps))))))
        (setf (aref fine (* (1+ j) substeps)) t1)))))

(defmethod process-factors ((p local-vol)) (local-vol-substeps p))

(defmethod make-stepper ((p local-vol) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((substeps (local-vol-substeps p))
         (fine (refine-times times substeps)))
    (multiple-value-bind (ks table) (local-variance-table p market fine)
      (let* ((mu (interval-log-growths market fine))
             (log-forwards (map 'double-vector (lambda (time) (log (forward market time))) fine))
             (nk (length ks))
             (k0 (aref ks 0))
             (dk (/ (- (aref ks (1- nk)) k0) (1- nk))))
        (declare (type double-vector ks mu log-forwards)
                 (type (simple-array double-float (* *)) table)
                 (fixnum nk substeps) (double-float k0 dk))
        ;; Euler in ln S over each substep, with the substep's integrated
        ;; variance at the start's moneyness.
        (lambda (state normals j)
          (declare (type (simple-array double-float (*)) state normals)
                   (fixnum j)
                   (optimize (speed 3) (safety 0)))
          (dotimes (sub substeps)
            (let* ((step (+ (the fixnum (* j substeps)) sub))
                   ;; Positive and non-negative, so LOG and SQRT stay real
                   ;; and inline.
                   (s (the (double-float (0d0)) (aref state 0)))
                   (x (/ (- (- (log s) (aref log-forwards step)) k0) dk))
                   (i (max 0 (min (- nk 2) (the fixnum (truncate (the (double-float -2d0 1d9)
                                                                      (max -1d0 (min x (float nk 1d0)))))))))
                   (u (max 0d0 (min 1d0 (- x i))))
                   (a (aref table step i))
                   (variance (the (double-float 0d0)
                                  (max 0d0 (+ a (* u (- (aref table step (1+ i)) a)))))))
              (declare (fixnum step i) (double-float x u a))
              (setf (aref state 0)
                    (* s (exp (+ (- (aref mu step) (* 0.5d0 variance))
                                 (* (sqrt variance) (aref normals sub))))))))
          ;; Steppers return nothing: a double-float result would be boxed.
          nil)))))

;;; Over one grid step, ln S is approximately Gaussian with the step's local
;;; variance at the step's starting moneyness: the sum of its substeps'.
(defmethod bridge-variance ((p local-vol) market times)
  (declare (type (simple-array double-float (*)) times))
  (let ((substeps (local-vol-substeps p)))
    (multiple-value-bind (ks table) (local-variance-table p market (refine-times times substeps))
      (let* ((log-forwards (map 'double-vector (lambda (time) (log (forward market time))) times))
             (nk (length ks))
             (k0 (aref ks 0))
             (dk (/ (- (aref ks (1- nk)) k0) (1- nk))))
        (declare (type double-vector ks log-forwards)
                 (type (simple-array double-float (* *)) table)
                 (fixnum nk substeps) (double-float k0 dk))
        (lambda (state j acc slot)
          (declare (type (simple-array double-float (*)) state acc) (fixnum j slot))
          (let* ((s (the (double-float (0d0)) (aref state 0)))
                 (x (/ (- (- (log s) (aref log-forwards j)) k0) dk))
                 (i (max 0 (min (- nk 2) (the fixnum (truncate (the (double-float -2d0 1d9)
                                                                    (max -1d0 (min x (float nk 1d0)))))))))
                 (u (max 0d0 (min 1d0 (- x i)))))
            (declare (double-float x u) (fixnum i))
            (setf (aref acc slot)
                  (iter (for sub below substeps)
                        (for row = (+ (* j substeps) sub))
                        (for a = (aref table row i))
                        (sum (+ a (* u (- (aref table row (1+ i)) a)))))))
          nil)))))
