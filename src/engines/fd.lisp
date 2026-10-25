;;;; engines/fd.lisp --- finite differences in log-spot
;;;;
;;;; The value V(t, x), x = ln S, solves V_t + 1/2 s2(x) V_xx + mu(x) V_x - r V
;;;; = 0 backward from the payoff. A process supplies s2, mu and r per time
;;;; step (PDE-COEFFICIENTS); the engine discretizes x on a sinh-concentrated
;;;; grid (the strike midway between nodes, a barrier exactly on one) with
;;;; three-point stencils from FINITE-DIFFERENCE-WEIGHTS, and t by a
;;;; theta-scheme with Rannacher start-up after each payoff discontinuity.
;;;;
;;;; The solver rolls back a list of LAYERS (value arrays over index ranges
;;;; of the grid) in one loop, with hooks in a fixed order at each step: the
;;;; scheme's step with each layer's boundary conditions, then events
;;;; (barrier monitoring), then the exercise projection. A layer's boundary
;;;; is a function of time, so a rebate and another layer's value at the
;;;; barrier are the same mechanism: a knock-out is one layer on the grid
;;;; truncated at the barrier, a knock-in two coupled layers. Exercise rights
;;;; come from EXERCISE-MASK: Ikonen-Toivanen splitting over consecutive
;;;; exercise times (American), a projection at isolated ones (Bermudan).
;;;;
;;;; Discrete dividends (task 21): ex-dates are grid times. Under the spot
;;;; model the value just before an ex-date is the value after it at the
;;;; price the dividend leaves, V(x, t-) = V(ln(e^x - D), t+), by a natural
;;;; cubic spline in x on the grid; exercise is then checked on the price
;;;; before the dividend, and the scheme restarts with Rannacher steps.
;;;; Under the escrowed model (GBM) the grid is in the price less the
;;;; dividends to come, R(t), and exercise pays on the price, so the
;;;; exercise value moves with t.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Coefficients
;;; --------------------------------------------------------------------

(defgeneric pde-coefficients (process market t0 t1)
  (:documentation "The coefficients of the pricing PDE in x = ln S over
[T0, T1] (market time), as (values drift-fn diffusion-fn rate): DRIFT-FN and
DIFFUSION-FN are closures of x giving the drift mu(x) and the variance rate
s2(x) of x per unit time, RATE the short rate over the step, from discount
factor ratios. PROCESS is resolved. NIL when the process has no
one-dimensional PDE (the default).")
  (:method ((p stochastic-process) market t0 t1)
    (declare (ignore market t0 t1))
    nil))

(defun step-rate (market t0 t1)
  "-ln(D(T1)/D(T0)) / (T1 - T0): the short rate the discount curve implies
over the step."
  (/ (- (log (/ (discount-factor market t1) (discount-factor market t0)))) (- t1 t0)))

(defmethod pde-coefficients ((p gbm) market t0 t1)
  (let* ((dt (- t1 t0))
         (variance (/ (gbm-forward-variance p market t0 t1) dt))
         (drift (- (/ (log-growth market t0 t1) dt) (* 0.5d0 variance))))
    (values (lambda (x) (declare (ignore x)) drift)
            (lambda (x) (declare (ignore x)) variance)
            (step-rate market t0 t1))))

;;; dS = (r - q) S dt + sigma S^beta dW: in x the variance rate is
;;; sigma^2 e^(2 (beta - 1) x). The log grid never reaches 0, so absorption
;;; is not modelled (negligible where the grid is wide in standard deviations).
(defmethod pde-coefficients ((p cev) market t0 t1)
  (let* ((dt (- t1 t0))
         (growth (/ (log-growth market t0 t1) dt))
         (sigma2 (expt (sigma p) 2))
         (exponent (* 2d0 (- (beta p) 1d0))))
    (flet ((variance (x) (* sigma2 (exp (* exponent x)))))
      (values (lambda (x) (- growth (* 0.5d0 (variance x))))
              #'variance
              (step-rate market t0 t1)))))

;;; Dupire's local variance over the step at moneyness k = x - ln F(t_mid):
;;; (w(k, t1) - w(k, t0)) / (dt D(k, t_mid)), as the local-vol stepper.
(defmethod pde-coefficients ((p local-vol) market t0 t1)
  (let* ((variance-at (variance-function p market))
         (dt (- t1 t0))
         (mid (* 0.5d0 (+ t0 t1)))
         (log-forward (log (forward market mid)))
         (growth (/ (log-growth market t0 t1) dt))
         (last-x nil) (last-variance 0d0))
    (flet ((variance (x)
             (if (eql x last-x)
                 last-variance
                 (let* ((k (- x log-forward))
                        (numerator (- (funcall variance-at k t1)
                                      (if (zerop t0) 0d0 (funcall variance-at k t0))))
                        (denominator (multiple-value-bind (w wk wkk) (funcall variance-at k mid)
                                       (dupire-denominator k w wk wkk))))
                   (when (minusp numerator)
                     (error 'calendar-arbitrage :t0 t0 :t1 t1 :variance numerator))
                   (unless (plusp denominator)
                     (error 'butterfly-arbitrage :smile (or (local-vol-surface p) "the market's vol")
                                                 :k k :density denominator))
                   (setf last-x x last-variance (/ numerator denominator dt))))))
      (values (lambda (x) (- growth (* 0.5d0 (variance x))))
              #'variance
              (step-rate market t0 t1)))))

(defgeneric pde-scale (process market horizon)
  (:documentation "The standard deviation of ln S at HORIZON the grid's width
is measured in: by default the market's ATM-forward Black variance.")
  (:method ((p stochastic-process) market horizon)
    (sqrt (max (atm-forward-variance market horizon) 1d-8)))
  (:method ((p cev) market horizon)
    (* (sigma p) (expt (spot market) (- (beta p) 1d0)) (sqrt horizon)))
  ;; A skewed surface's wing variance far exceeds the ATM one; the grid has
  ;; to reach where the payoff is linear in S under the wing's vol.
  (:method ((p local-vol) market horizon)
    (let* ((variance (variance-function p market))
           (atm (sqrt (max (funcall variance 0d0 horizon) 1d-8))))
      (sqrt (iter (for k in '(-5 -2.5d0 0 2.5d0 5))
                  (maximize (funcall variance (* k atm) horizon)))))))

;;; --------------------------------------------------------------------
;;; Grid
;;; --------------------------------------------------------------------

(defun log-spot-grid (lo hi center n &key alpha anchor)
  "About N nodes in [LO, HI], uniform in xi = asinh((x - CENTER) / ALPHA)
(uniform in x when ALPHA is NIL). CENTER falls midway between two nodes, so
a payoff kink there is smoothed; with ANCHOR, that x is a node exactly,
CENTER still midway when it is more than a quarter step away. Returns
(values xs anchor-index)."
  (flet ((to-xi (x) (if alpha (asinh (/ (- x center) alpha)) (- x center)))
         (from-xi (xi) (if alpha (+ center (* alpha (sinh xi))) (+ center xi))))
    (let* ((xi-lo (to-xi lo)) (xi-hi (to-xi hi))
           (h0 (/ (- xi-hi xi-lo) (1- n)))
           (xi-anchor (and anchor (to-xi anchor)))
           (h (if (and xi-anchor (< xi-lo 0d0 xi-hi) (> (abs xi-anchor) (* 0.25d0 h0)))
                  (/ (abs xi-anchor) (+ (max 0 (round (- (/ (abs xi-anchor) h0) 0.5d0))) 0.5d0))
                  h0))
           (origin (or xi-anchor (* 0.5d0 h)))
           (k-lo (ceiling (- (/ (- xi-lo origin) h) 1d-9)))
           (k-hi (floor (+ (/ (- xi-hi origin) h) 1d-9)))
           (xs (make-array (1+ (- k-hi k-lo)) :element-type 'double-float)))
      (iter (for k from k-lo to k-hi)
            (for i from 0)
            (setf (aref xs i) (if (and xi-anchor (zerop k)) (float anchor 1d0) (from-xi (+ origin (* k h))))))
      (values xs (and anchor (- k-lo))))))

(defstruct (stencils (:constructor %make-stencils))
  "Three-point weights at each interior node of a grid: first (D1-) and
second (D2-) derivatives over the offsets (x[i-1] - x[i], 0, x[i+1] - x[i])."
  (d1m nil :type double-vector) (d10 nil :type double-vector) (d1p nil :type double-vector)
  (d2m nil :type double-vector) (d20 nil :type double-vector) (d2p nil :type double-vector))

(defun make-stencils (xs)
  (let* ((n (length xs))
         (vectors (iter (repeat 6) (collect (make-array n :element-type 'double-float :initial-element 0d0)))))
    (destructuring-bind (d1m d10 d1p d2m d20 d2p) vectors
      (iter (for i from 1 below (1- n))
            (let ((offsets `(,(- (aref xs (1- i)) (aref xs i)) 0d0 ,(- (aref xs (1+ i)) (aref xs i)))))
              (destructuring-bind (a b c) (coerce (finite-difference-weights offsets 1) 'list)
                (setf (aref d1m i) a (aref d10 i) b (aref d1p i) c))
              (destructuring-bind (a b c) (coerce (finite-difference-weights offsets 2) 'list)
                (setf (aref d2m i) a (aref d20 i) b (aref d2p i) c))))
      (%make-stencils :d1m d1m :d10 d10 :d1p d1p :d2m d2m :d20 d20 :d2p d2p))))

;;; --------------------------------------------------------------------
;;; Layers and the roll-back
;;; --------------------------------------------------------------------

(defstruct (fd-layer (:constructor %make-fd-layer))
  "Values over grid indices [LO, HI] (the array spans the whole grid). LOWER
and UPPER are NIL (V linear in S, V_xx = V_x, with a one-sided derivative)
or a function of time giving the boundary value (Dirichlet). INTRINSIC, when the layer may be exercised, is the
exercise value at every node. EVENTS are functions (j values) called after
the step that reaches grid time index J."
  (values nil :type double-vector)
  (lo 0 :type fixnum) (hi 0 :type fixnum)
  (lower nil) (upper nil) (intrinsic nil) (events '())
  (multipliers nil)
  (a nil :type double-vector) (b nil :type double-vector)
  (c nil :type double-vector) (d nil :type double-vector))

(defun make-fd-layer (values lo hi &key lower upper intrinsic events)
  (let ((m (1+ (- hi lo))))
    (flet ((work () (make-array m :element-type 'double-float :initial-element 0d0)))
      (%make-fd-layer :values values :lo lo :hi hi :lower lower :upper upper
                      :intrinsic intrinsic :events events
                      :multipliers (and intrinsic (make-array (length values) :element-type 'double-float
                                                                              :initial-element 0d0))
                      :a (work) :b (work) :c (work) :d (work)))))

(defun fd-step (layer xs stencils drift diffusion rate theta dt time american)
  "One theta-step of LAYER back over DT to TIME, in place. With AMERICAN, the
Ikonen-Toivanen splitting: the multipliers enter the right-hand side, then
V = max(V~ - dt lambda, g) and lambda = max(0, lambda + (g - V~)/dt)."
  (declare (type double-vector xs drift diffusion) (double-float rate theta dt time))
  (let* ((v (fd-layer-values layer))
         (lo (fd-layer-lo layer)) (hi (fd-layer-hi layer))
         (n (length xs))
         (explicit (- 1d0 theta))
         (g (fd-layer-intrinsic layer))
         (lambdas (and american g (fd-layer-multipliers layer)))
         (wa (fd-layer-a layer)) (wb (fd-layer-b layer))
         (wc (fd-layer-c layer)) (wd (fd-layer-d layer)))
    (declare (type double-vector v wa wb wc wd) (fixnum lo hi n))
    (flet ((operator (i)
             ;; (a b c) with L V_i = a V_(i-1) + b V_i + c V_(i+1).
             (let ((mu (aref drift i)) (half (* 0.5d0 (aref diffusion i))))
               (cond ((and (< lo i hi) (< 0 i (1- n)))
                      (values (+ (* half (aref (stencils-d2m stencils) i)) (* mu (aref (stencils-d1m stencils) i)))
                              (- (+ (* half (aref (stencils-d20 stencils) i)) (* mu (aref (stencils-d10 stencils) i)))
                                 rate)
                              (+ (* half (aref (stencils-d2p stencils) i)) (* mu (aref (stencils-d1p stencils) i)))))
                     ;; Linear boundary: V linear in S (V_SS = 0, so V_xx =
                     ;; V_x), as a vanilla is at both ends of the grid, with a
                     ;; one-sided first derivative.
                     ((= i lo)
                      (let ((slope (/ (+ mu half) (- (aref xs (1+ i)) (aref xs i)))))
                        (values 0d0 (- (- slope) rate) slope)))
                     (t
                      (let ((slope (/ (+ mu half) (- (aref xs i) (aref xs (1- i))))))
                        (values (- slope) (- slope rate) 0d0)))))))
      (iter (for i from lo to hi)
            (for row from 0)
            (let ((boundary (cond ((= i lo) (fd-layer-lower layer))
                                  ((= i hi) (fd-layer-upper layer)))))
              (if boundary
                  (setf (aref wa row) 0d0 (aref wb row) 1d0 (aref wc row) 0d0
                        (aref wd row) (float (funcall boundary time) 1d0))
                  (multiple-value-bind (a b c) (operator i)
                    (declare (double-float a b c))
                    (let ((lv (+ (* b (aref v i))
                                 (if (> i lo) (* a (aref v (1- i))) 0d0)
                                 (if (< i hi) (* c (aref v (1+ i))) 0d0))))
                      (setf (aref wa row) (* (- theta) dt a)
                            (aref wb row) (- 1d0 (* theta dt b))
                            (aref wc row) (* (- theta) dt c)
                            (aref wd row) (+ (aref v i) (* explicit dt lv)
                                             (if lambdas (* dt (aref lambdas i)) 0d0)))))))))
    (solve-tridiagonal! wa wb wc wd)
    (iter (for i from lo to hi)
          (for row from 0)
          (let ((tilde (aref wd row)))
            (if (and lambdas
                     (not (or (and (= i lo) (fd-layer-lower layer))
                              (and (= i hi) (fd-layer-upper layer)))))
                (let ((gi (aref g i)) (li (aref lambdas i)))
                  (setf (aref v i) (max (- tilde (* dt li)) gi)
                        (aref lambdas i) (max 0d0 (+ li (/ (- gi tilde) dt)))))
                (setf (aref v i) tilde))))
    v))

(defun fd-dividend-interface (xs values offset jumps)
  "Replace the values at VALUES[OFFSET + i], over the log-price nodes XS, by
the values just before the dividend JUMPS: the value after them at the price
they leave, by a natural cubic spline in x, flat beyond the grid (a price
floored at 0 takes the lowest node's value)."
  (declare (type double-vector xs values) (fixnum offset))
  (let* ((n (length xs))
         (line (subseq values offset (+ offset n)))
         (after (make-interpolator :natural-cubic xs line :extrapolation :flat)))
    (dotimes (i n values)
      (let ((s (apply-dividend-jumps (exp (aref xs i)) jumps)))
        (setf (aref values (+ offset i))
              (if (plusp s) (float (funcall after (log s)) 1d0) (aref line 0)))))))

(defun grid-dividend-jumps (grid events kinds)
  "A simple-vector over GRID's time indices: at the index of each of EVENTS'
ex-dates (mandatory dates of GRID), the (kind . amount) of those of KINDS."
  (let ((jumps (make-array (length (time-grid-times grid)) :initial-element nil)))
    (dolist (e events jumps)
      (when (member (dividend-event-kind e) kinds)
        (let ((j (grid-index-of grid (dividend-event-date e))))
          (setf (svref jumps j)
                (append (svref jumps j) `((,(dividend-event-kind e) . ,(dividend-event-amount e))))))))))

(defun fd-roll-back (xs times layers coefficients &key (theta 0.5d0) (rannacher-steps 2) mask smoothing
                                                     before-step)
  "Roll LAYERS back from TIMES's last time to 0 over the grid XS.
COEFFICIENTS is (lambda (t0 t1)) => (values drift-fn diffusion-fn rate) for
each step. MASK (a bit vector over time indices, or NIL) marks exercise
times; SMOOTHING marks the times after which the step is replaced by
RANNACHER-STEPS fully implicit substeps. At each step, in order:
BEFORE-STEP (a function of the target time index J, if given), the scheme
(with boundaries), each layer's events, then the exercise projection.
Returns the layers' values at time index 1 (for theta), as a list of
copies."
  (let* ((n (length xs))
         (stencils (make-stencils xs))
         (drift (make-array n :element-type 'double-float))
         (diffusion (make-array n :element-type 'double-float))
         (snapshot nil))
    (iter (for j from (- (length times) 2) downto 0)
          (let* ((t0 (aref times j)) (t1 (aref times (1+ j)))
                 (american (and mask (= 1 (sbit mask j)) (= 1 (sbit mask (1+ j)))))
                 (smooth (and smoothing (plusp rannacher-steps) (= 1 (sbit smoothing (1+ j)))))
                 (substeps (if smooth rannacher-steps 1))
                 (dt (/ (- t1 t0) substeps)))
            (when before-step (funcall before-step j))
            (multiple-value-bind (drift-fn diffusion-fn rate) (funcall coefficients t0 t1)
              (dotimes (i n)
                (let ((x (aref xs i)))
                  (setf (aref diffusion i) (float (funcall diffusion-fn x) 1d0)
                        (aref drift i) (float (funcall drift-fn x) 1d0))))
              (dotimes (s substeps)
                (let ((time (- t1 (* (1+ s) dt))))
                  (dolist (layer layers)
                    (fd-step layer xs stencils drift diffusion (float rate 1d0)
                             (if smooth 1d0 theta) dt (if (= s (1- substeps)) t0 time) american)))))
            ;; Events (barrier monitoring), then exercise at isolated dates.
            (dolist (layer layers)
              (dolist (event (fd-layer-events layer))
                (funcall event j (fd-layer-values layer))))
            (when (and mask (= 1 (sbit mask j)) (not american))
              (dolist (layer layers)
                (a:when-let ((g (fd-layer-intrinsic layer)))
                  (let ((v (fd-layer-values layer)))
                    (iter (for i from (fd-layer-lo layer) to (fd-layer-hi layer))
                          (setf (aref v i) (max (aref v i) (aref g i))))))))
            (when (= j 1)
              (setf snapshot (mapcar (lambda (layer) (copy-seq (fd-layer-values layer))) layers)))))
    snapshot))

(defun grid-value (xs values x lo hi)
  "(values v dv/dx d2v/dx2) at X from the four nodes of [LO, HI] nearest it,
by FINITE-DIFFERENCE-WEIGHTS."
  (let* ((i (max lo (min (- hi 3) (1- (iter (for k from lo to hi)
                                            (finding k such-that (> (aref xs k) x)
                                                     on-failure hi))))))
         (offsets (iter (for k from i below (+ i 4)) (collect (- (aref xs k) x))))
         (vs (iter (for k from i below (+ i 4)) (collect (aref values k)))))
    (flet ((derivative (order)
             (iter (for w in-vector (finite-difference-weights offsets order))
                   (for v in vs)
                   (sum (* w v)))))
      (values (derivative 0) (derivative 1) (derivative 2)))))

;;; --------------------------------------------------------------------
;;; Pricing
;;; --------------------------------------------------------------------

(defun fd-solve (x p f process e market)
  "Value of the option (X P F) under the resolved PROCESS by E's 1-D scheme:
(values price delta gamma theta-per-day)."
  (let* ((tau (time-to-expiry x market))
         (dividends (dividends-between (discrete-dividends market) 0d0 tau))
         (escrowed (and dividends (eq (dividend-model process) :escrowed)))
         ;; The grid's price: the spot, or under the escrowed model the spot
         ;; less the dividends to come.
         (s0 (if escrowed
                 (- (spot market) (escrowed-dividends market dividends 0d0 tau))
                 (spot market)))
         (x0 (progn (unless (plusp s0)
                      (error 'model-domain-error :engine e
                                                 :reason "the cash dividends to come exceed the spot"))
                    (log s0)))
         (grid (make-time-grid market :dates `(,@(instrument-grid-dates x p f)
                                               ,@(dividend-dates market tau))
                               :n-steps (n-time e)))
         (times (time-grid-times grid))
         (n-times (length times))
         (mask (exercise-mask x market times))
         (american (every (lambda (b) (= b 1)) mask))
         (barrier (and (typep f 'barrier) f))
         (h (and barrier (log (barrier-level barrier))))
         (up (and barrier (eq (barrier-direction barrier) :up)))
         (out (and barrier (eq (barrier-knock barrier) :out)))
         (continuous (and barrier (null (observation-dates barrier))))
         (monitoring (and barrier (not continuous)
                          (nth-value 0 (grid-date-mask grid (observation-dates barrier)))))
         (kernel (payoff-kernel p))
         (scale (pde-scale process market tau))
         (log-forward (log (forward market tau)))
         (width (* (float (grid-width e) 1d0) scale))
         (lo (- (min x0 log-forward) width))
         (hi (+ (max x0 log-forward) width)))
    (unless (pde-coefficients process market (aref times 0) (aref times 1))
      (error 'unsupported-combination :exercise x :payoff p :path f :process process
                                      :engine e :market market))
    ;; A continuous barrier the spot has already touched: knocked out (the
    ;; rebate, now) or knocked in (the option without the barrier).
    (when (and continuous (if up (>= x0 h) (<= x0 h)))
      (return-from fd-solve
        (if out
            (values (barrier-rebate barrier) 0d0 0d0 0d0)
            (fd-solve x p (make-instance 'path-independent) process e market))))
    ;; A knock-out's domain ends at the barrier.
    (when (and continuous out)
      (if up (setf hi h) (setf lo h)))
    (when (and barrier (not (and continuous out)))
      (setf lo (min lo (- h scale)) hi (max hi (+ h scale))))
    (multiple-value-bind (xs anchor)
        (log-spot-grid lo hi (if (typep p 'strike-payoff) (log (strike p)) x0) (n-space e)
                       ;; Concentrated on the ATM scale, however wide the grid.
                       :alpha (and (concentration e)
                                   (* (float (concentration e) 1d0)
                                      (min scale (sqrt (max (atm-forward-variance market tau) 1d-8)))))
                       :anchor h)
      (let* ((n (length xs))
             (payoff (map 'double-vector (lambda (x) (funcall kernel (exp x))) xs))
             (intrinsic (and (not (typep x 'european-exercise)) payoff))
             (jumps (and dividends
                     (grid-dividend-jumps grid dividends
                                          (if escrowed '(:proportional) '(:cash :proportional)))))
         ;; Under the escrowed model, the exercise value at time index J: on
         ;; the price just before or just after a dividend going ex then,
         ;; whichever pays more.
         (escrow-after (and escrowed
                            (map 'double-vector
                                 (lambda (time) (escrowed-dividends market dividends time tau))
                                 times)))
         (escrow-before (and escrowed
                             (map 'double-vector
                                  (lambda (time) (escrowed-dividends market dividends time tau :cum t))
                                  times)))
         (intrinsic-at (lambda (j)
                         (map 'double-vector
                              (lambda (x)
                                (max (funcall kernel (+ (exp x) (aref escrow-after j)))
                                     (funcall kernel (+ (exp x) (aref escrow-before j)))))
                              xs)))
         (smoothing (let ((bits (make-array n-times :element-type 'bit :initial-element 0)))
                          (setf (sbit bits (1- n-times)) 1)
                          (unless american (bit-ior bits mask bits))
                          (when monitoring (bit-ior bits monitoring bits))
                          (when jumps
                            (dotimes (j n-times) (when (svref jumps j) (setf (sbit bits j) 1))))
                          bits))
             (rebate (and barrier (barrier-rebate barrier)))
             (monitor
               ;; Replace VALUES beyond the barrier by (REPLACEMENT i); the
               ;; node on the barrier, where the value jumps, takes the mean
               ;; of both sides.
               (lambda (values replacement)
                 (dotimes (i n)
                   (cond ((= i anchor)
                          (setf (aref values i) (* 0.5d0 (+ (aref values i) (funcall replacement i)))))
                         ((if up (> (aref xs i) h) (< (aref xs i) h))
                          (setf (aref values i) (funcall replacement i)))))))
             (expiry-monitored (and monitoring (= 1 (sbit monitoring (1- n-times)))))
             (layers
               (cond
                 ((null barrier)
                  `(,(make-fd-layer (copy-seq payoff) 0 (1- n) :intrinsic intrinsic)))
                 ((and continuous out)
                  ;; One layer on the truncated grid, the rebate on the barrier.
                  (let ((v (copy-seq payoff)))
                    (setf (aref v anchor) rebate)
                    `(,(make-fd-layer v 0 (1- n)
                                      :lower (unless up (constantly rebate))
                                      :upper (when up (constantly rebate))))))
                 (continuous
                  ;; Knock-in: the option without the barrier on the whole grid,
                  ;; and the not-yet-knocked-in value on the far side, whose
                  ;; boundary at the barrier is the first layer's value there.
                  (let* ((vanilla (make-fd-layer (copy-seq payoff) 0 (1- n)))
                         (pending (make-array n :element-type 'double-float :initial-element rebate))
                         (at-barrier (lambda (time) (declare (ignore time))
                                       (aref (fd-layer-values vanilla) anchor))))
                    (setf (aref pending anchor) (aref payoff anchor))
                    `(,vanilla
                      ,(if up
                           (make-fd-layer pending 0 anchor :upper at-barrier)
                           (make-fd-layer pending anchor (1- n) :lower at-barrier)))))
                 (out
                  ;; Discrete knock-out: the rebate beyond the barrier on each
                  ;; monitoring date (events run below the expiry's index, so
                  ;; a monitored expiry is applied to the payoff here).
                  (let ((v (copy-seq payoff))
                        (knocked (constantly rebate)))
                    (when expiry-monitored (funcall monitor v knocked))
                    `(,(make-fd-layer v 0 (1- n)
                                      :events `(,(lambda (j values)
                                                   (when (= 1 (sbit monitoring j))
                                                     (funcall monitor values knocked))))))))
                 (t
                  ;; Discrete knock-in: beyond the barrier on a monitoring date,
                  ;; the not-yet-knocked-in value becomes the option's.
                  (let* ((vanilla (make-fd-layer (copy-seq payoff) 0 (1- n)))
                         (pending (make-array n :element-type 'double-float :initial-element rebate)))
                    (when expiry-monitored
                      (funcall monitor pending (lambda (i) (aref payoff i))))
                    `(,vanilla
                      ,(make-fd-layer pending 0 (1- n)
                                      :events `(,(lambda (j values)
                                                   (when (= 1 (sbit monitoring j))
                                                     (funcall monitor values
                                                              (lambda (i) (aref (fd-layer-values vanilla) i))))))))))))
             (result (a:lastcar layers))
             (snapshot
               (progn
                 (when jumps
                   ;; Cross the dividend, then exercise on the price before it.
                   (let ((layer (first layers)))
                     (push (lambda (j values)
                             (a:when-let ((js (svref jumps j)))
                               (fd-dividend-interface xs values 0 js)
                               (a:when-let ((g (fd-layer-intrinsic layer)))
                                 (dotimes (i n) (setf (aref values i) (max (aref values i) (aref g i))))
                                 (fill (fd-layer-multipliers layer) 0d0))))
                           (fd-layer-events layer))))
                 (fd-roll-back xs times layers
                               (lambda (t0 t1) (pde-coefficients process market t0 t1))
                               :theta (float (fd-theta e) 1d0)
                               :rannacher-steps (rannacher-steps e)
                               :mask mask :smoothing smoothing
                               :before-step (and escrowed intrinsic
                                                 (lambda (j)
                                                   (setf (fd-layer-intrinsic (first layers))
                                                         (funcall intrinsic-at j))))))))
        (multiple-value-bind (v vx vxx)
            (grid-value xs (fd-layer-values result) x0 (fd-layer-lo result) (fd-layer-hi result))
          (values v
                  (/ vx s0)
                  (/ (- vxx vx) (* s0 s0))
                  (* (one-day market)
                     (/ (- (grid-value xs (a:lastcar snapshot) x0 (fd-layer-lo result) (fd-layer-hi result))
                           v)
                        (aref times 1)))))))))

(defgeneric fd-node-greeks (exercise payoff path process engine market)
  (:documentation "(values price delta gamma theta-per-day) from the grid of
ENGINE's scheme for the option (EXERCISE PAYOFF PATH) under the resolved
PROCESS.")
  (:method (x p f process (e fd-engine) market)
    (fd-solve x p f process e market)))

(defmethod check-discrete-dividends ((e fd-engine) o market)
  (declare (ignore market))
  (unless (typep (option-path o) 'path-independent)
    (refuse-dividends e "its dividend steps are for options without a path feature")))

(defmethod price-fd ((x exercise) (p payoff) (f path-independent) (process stochastic-process)
                     (e fd-engine) market)
  (values (fd-solve x p f (resolve-process process market (time-to-expiry x market)) e market) nil))

(defmethod price-fd ((x european-exercise) (p payoff) (f barrier) (process stochastic-process)
                     (e fd-engine) market)
  (values (fd-solve x p f (resolve-process process market (time-to-expiry x market)) e market) nil))

;;; Path payoffs carry their own path logic, which a PDE cannot run.
(defmethod price-fd ((x exercise) (p path-payoff) (f path-feature) (process stochastic-process)
                     (e fd-engine) market)
  (error 'unsupported-combination :exercise x :payoff p :path f :process process
                                  :engine e :market market))

;;; --------------------------------------------------------------------
;;; Automatic selection
;;; --------------------------------------------------------------------

;;; After closed forms and the tree (10), before COS (20), BAW (30) and Monte
;;; Carlo (90).
(macrolet ((candidate (exercise path process &optional (priority 15))
             `(defmethod engine-candidates append ((designator (eql :fd)) (x ,exercise) (p vanilla-payoff)
                                                   (f ,path) (process ,process))
                '((,priority :fd)))))
  ;; Europeans under GBM, after Black-Scholes and COS: both decline
  ;; spot-model cash dividends under automatic selection (task 21).
  (candidate european-exercise path-independent gbm 25)
  (candidate american-exercise path-independent gbm)
  (candidate bermudan-exercise path-independent gbm)
  (candidate exercise path-independent cev)
  (candidate exercise path-independent local-vol)
  (candidate european-exercise barrier gbm)
  (candidate european-exercise barrier cev)
  (candidate european-exercise barrier local-vol))
