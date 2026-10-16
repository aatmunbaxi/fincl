;;;; engines/hull-white.lisp --- Hull-White engines: Jamshidian and the tree
;;;;
;;;; JAMSHIDIAN-SWAPTION-ENGINE prices a European swaption as a portfolio of
;;;; zero-coupon bond options (Jamshidian 1989). HULL-WHITE-TREE-ENGINE builds
;;;; Hull and White's (1994) trinomial tree for x = r - phi(t) on a grid that
;;;; contains every cash-flow and exercise time, fits phi(t) by forward
;;;; induction so the tree reprices the curve's discount factors, and rolls
;;;; Bermudan swaptions and callable bonds back. Both follow QuantLib
;;;; (JamshidianSwaptionEngine, TrinomialTree, HullWhite::tree,
;;;; TreeSwaptionEngine, TreeCallableFixedRateBondEngine), and price on one
;;;; curve: the model's, which both forecasts and discounts.

(in-package #:fincl)

(defclass hull-white-engine (rates-engine)
  ((process :initarg :process :reader process :initform (make-hull-white)
            :documentation "The HULL-WHITE process.")
   (curve :initarg :curve :initform nil :reader engine-curve :type (or null keyword)
          :documentation "Id of the curve the model fits; NIL: the curve of the
instrument's currency."))
  (:metaclass engine-class)
  (:models hull-white)
  (:documentation "An engine for the Hull-White model on one curve."))

(defmethod initialize-instance :after ((e hull-white-engine) &key)
  (check-input (slot-value e 'process) hull-white "a HULL-WHITE process (MAKE-HULL-WHITE)"))

(defun model-curve (engine instrument market)
  (make-rate-curve market (rates-discount-curve market (instrument-currency instrument)
                                                 (engine-curve engine))))

(defclass jamshidian-swaption-engine (hull-white-engine) ()
  (:metaclass engine-class)
  (:designator :jamshidian)
  (:documentation "Hull-White European swaptions by Jamshidian's
decomposition into bond options. Physical settlement, no floating spread.

  (price swaption (make-engine :jamshidian :process (make-hull-white :a 0.05d0 :sigma 0.01d0)) m)"))

(defclass hull-white-tree-engine (hull-white-engine)
  ((time-steps :initarg :time-steps :initform 200 :reader engine-time-steps
               :documentation "Steps over the whole grid, spread between its
mandatory times (QuantLib's TimeGrid)."))
  (:metaclass engine-class)
  (:designator :hull-white-tree)
  (:documentation "Hull-White trinomial tree for European and Bermudan
swaptions and callable fixed-rate bonds. Exercise and call dates are grid
times.

  (price bermudan (make-engine :hull-white-tree :process hw :time-steps 400) m)"))

(defmethod initialize-instance :after ((e hull-white-tree-engine) &key)
  (check-input (slot-value e 'time-steps) (integer 1) "a positive number of time steps"))

(defun swap-market-time (market date) (market-time market date))

;;; --------------------------------------------------------------------
;;; Jamshidian
;;; --------------------------------------------------------------------

(defmethod price ((s swaption) (e jamshidian-swaption-engine) market)
  (let* ((swap (swaption-swap s))
         (exercise (swaption-exercise s)))
    (unless (and (typep exercise 'european-exercise) (eq (swaption-settlement s) :physical)
                 (zerop (swap-spread swap)))
      (error 'unsupported-combination :exercise exercise :payoff s :path nil :engine e :market market))
    (let* ((process (process e))
           (curve (model-curve e s market))
           (fixed (first (swap-legs swap)))
           (nominal (swap-nominal swap))
           (amounts (let ((amounts (mapcar (lambda (c) (cashflow-amount c market)) fixed)))
                      (incf (car (last amounts)) nominal)
                      amounts))
           (maturity (swap-market-time market (expiry exercise)))
           (value-time (swap-market-time market (accrual-start (first fixed))))
           (pay-times (mapcar (lambda (c) (swap-market-time market (payment-date c))) fixed))
           (r-star (solve-brent-from-guess
                    (lambda (x)
                      (let ((b (hull-white-discount-bond process curve maturity value-time x))
                            (value nominal))
                        (iter (for time in pay-times)
                              (for amount in amounts)
                              (decf value (* amount (/ (hull-white-discount-bond process curve maturity time x)
                                                       b))))
                        value))
                    -10d0 10d0 0.05d0 :tolerance 1d-8))
           (phi (if (eq (swap-type swap) :payer) -1 1))
           (b (hull-white-discount-bond process curve maturity value-time r-star)))
      (values (iter (for time in pay-times)
                    (for amount in amounts)
                    (sum (* amount (hull-white-bond-option
                                    process curve phi
                                    (/ (hull-white-discount-bond process curve maturity time r-star) b)
                                    maturity value-time time))))
              nil))))

;;; --------------------------------------------------------------------
;;; Time grids
;;; --------------------------------------------------------------------

(defun ql-close-enough-p (x y)
  "QuantLib's close_enough(x, y): equal within 42 ulps of either."
  (or (= x y)
      (let ((diff (abs (- x y)))
            (tolerance (* 42 double-float-epsilon)))
        (if (or (zerop x) (zerop y))
            (< diff (* tolerance tolerance))
            (or (<= diff (* tolerance (abs x))) (<= diff (* tolerance (abs y))))))))

(defun mandatory-time-grid (times steps)
  "QuantLib's TimeGrid(times, steps): 0, then each of the sorted distinct
TIMES with evenly spaced points between consecutive ones, about
last/STEPS apart. A typed vector."
  (let* ((sorted (sort (remove-if #'minusp (map 'list (lambda (x) (float x 1d0)) times)) #'<))
         (mandatory (iter (for x in sorted)
                          (for previous previous x)
                          (unless (and previous (ql-close-enough-p previous x)) (collect x))))
         (dt-max (/ (a:lastcar mandatory) steps))
         (grid (list 0d0))
         (begin 0d0))
    (dolist (end mandatory)
      (unless (zerop end)
        (let* ((n (max (round (/ (- end begin) dt-max)) 1))
               (dt (/ (- end begin) n)))
          (iter (for k from 1 to n) (push (+ begin (* k dt)) grid))))
      (setf begin end))
    (coerce (nreverse grid) 'double-vector)))

(defun grid-index (grid time)
  "The index of TIME in GRID, which must hold it within QuantLib's
close_enough (TimeGrid::index)."
  (let* ((n (length grid))
         (i (let ((upper (or (position-if (lambda (x) (>= x time)) grid) n)))
              (cond ((zerop upper) 0)
                    ((= upper n) (1- n))
                    ((< (- (aref grid upper) time) (- time (aref grid (1- upper)))) upper)
                    (t (1- upper))))))
    (unless (ql-close-enough-p time (aref grid i))
      (signal-invalid-argument 'time time 'real (format nil "a time on the tree's grid (nearest ~F)"
                                                        (aref grid i))))
    i))

;;; --------------------------------------------------------------------
;;; The Hull-White trinomial lattice
;;; --------------------------------------------------------------------

(defstruct (hull-white-lattice (:conc-name lattice-) (:constructor %make-hull-white-lattice))
  "A trinomial lattice for x = r - phi(t), an Ornstein-Uhlenbeck process
from 0, fitted to a curve. Level i has SIZE(i) nodes x = (JMIN(i-1) + j) DX(i);
a node j at step i branches to K(i)[j] - 1, K(i)[j], K(i)[j] + 1 at step i+1
with probabilities P1, P2, P3."
  (times nil :type double-vector)
  (dxs nil :type double-vector)
  (jmins nil :type simple-vector)   ; per step: the branching's lowest node
  (ks nil :type simple-vector)      ; per step: fixnum vector of middle descendants
  (probabilities nil :type simple-vector) ; per step: three double vectors
  (sizes nil :type (simple-array fixnum (*)))
  (phi nil :type double-vector)
  (state-prices nil :type simple-vector) ; kept steps only, NIL elsewhere
  (state-price-sums nil :type double-vector))

(defun lattice-size (lattice i)
  (aref (lattice-sizes lattice) i))

(defun lattice-x (lattice i j)
  (if (zerop i)
      0d0
      (* (+ (svref (lattice-jmins lattice) (1- i)) j) (aref (lattice-dxs lattice) i))))

(defun lattice-dt (lattice i)
  (let ((times (lattice-times lattice)))
    (- (aref times (1+ i)) (aref times i))))

(defun lattice-discount (lattice i j)
  (exp (- (* (+ (lattice-x lattice i j) (aref (lattice-phi lattice) i)) (lattice-dt lattice i)))))

(defun lattice-descendant (lattice i j branch)
  (+ (- (aref (svref (lattice-ks lattice) i) j) (svref (lattice-jmins lattice) i) 1) branch))

(defun lattice-probability (lattice i j branch)
  (aref (svref (svref (lattice-probabilities lattice) i) branch) j))

(defun build-trinomial-tree (a sigma grid)
  "The branching of QuantLib's TrinomialTree for the OU process dx = -a x dt
+ sigma dW from 0 on GRID: (values dxs jmins ks probabilities)."
  (let* ((n (1- (length grid)))
         (variances (make-array n :element-type 'double-float))
         (dt-max 0d0) (floor-variance 0d0))
    (flet ((dt (i) (- (aref grid (1+ i)) (aref grid i))))
      (dotimes (i n)
        (let ((v2 (if (< (abs a) (sqrt double-float-epsilon))
                      (* sigma sigma (dt i))
                      (* (/ (* 0.5d0 sigma sigma) a) (- 1d0 (exp (* -2d0 a (dt i))))))))
          (setf (aref variances i) v2
                dt-max (max dt-max (dt i))
                floor-variance (max floor-variance v2))))
      (let ((dx-floor (sqrt (* 3d0 floor-variance)))
            (dxs (make-array (1+ n) :element-type 'double-float :initial-element 0d0))
            (jmins (make-array n)) (ks (make-array n)) (probabilities (make-array n))
            (jmin 0) (jmax 0))
        (dotimes (i n)
          (let* ((dt (dt i))
                 (v2 (aref variances i))
                 (v (sqrt v2))
                 (dx-natural (* v (sqrt 3d0)))
                 (dx (if (< dt (* 0.01d0 dt-max)) (max dx-natural dx-floor) dx-natural))
                 (floored (> dx dx-natural))
                 (dx2 (* dx dx))
                 (size (1+ (- jmax jmin)))
                 (k (make-array size :element-type 'fixnum))
                 (p1 (make-array size :element-type 'double-float))
                 (p2 (make-array size :element-type 'double-float))
                 (p3 (make-array size :element-type 'double-float)))
            (setf (aref dxs (1+ i)) dx)
            (iter (for j from jmin to jmax)
                  (for index from 0)
                  (let* ((x (* j (aref dxs i)))
                         (m (* x (exp (* (- a) dt))))
                         (temp (floor (+ (/ m dx) 0.5d0)))
                         (e (- m (* temp dx)))
                         (e2 (* e e)))
                    (setf (aref k index) temp)
                    (if floored
                        (setf (aref p1 index) (/ (- (+ v2 e2) (* e dx)) (* 2d0 dx2))
                              (aref p2 index) (- 1d0 (/ (+ v2 e2) dx2))
                              (aref p3 index) (/ (+ v2 e2 (* e dx)) (* 2d0 dx2)))
                        (let ((e3 (* e (sqrt 3d0))))
                          (setf (aref p1 index) (/ (- (+ 1d0 (/ e2 v2)) (/ e3 v)) 6d0)
                                (aref p2 index) (/ (- 2d0 (/ e2 v2)) 3d0)
                                (aref p3 index) (/ (+ 1d0 (/ e2 v2) (/ e3 v)) 6d0))))))
            (setf jmin (1- (reduce #'min k))
                  jmax (1+ (reduce #'max k))
                  (svref jmins i) jmin
                  (svref ks i) k
                  (svref probabilities i) (vector p1 p2 p3))))
        (values dxs jmins ks probabilities)))))

(defun next-state-prices (lattice i prices)
  "State prices at step I + 1 from PRICES at step I (phi(t_I) fitted)."
  (let ((next (make-array (lattice-size lattice (1+ i)) :element-type 'double-float
                                                        :initial-element 0d0)))
    (dotimes (j (lattice-size lattice i) next)
      (let ((discount (lattice-discount lattice i j))
            (price (aref prices j)))
        (dotimes (branch 3)
          (incf (aref next (lattice-descendant lattice i j branch))
                (* price discount (lattice-probability lattice i j branch))))))))

(defun lattice-state-prices-through (lattice i)
  "The state prices at step I, which must be one the lattice kept."
  (or (svref (lattice-state-prices lattice) i)
      (signal-invalid-argument 'i i 'integer "a step whose state prices the lattice kept")))

(defun make-hull-white-lattice (process curve grid &key (keep '(0)))
  "The Hull-White tree on GRID, phi(t_i) fitted in turn so that the state
prices at t_(i+1) sum to the curve's discount factor (HullWhite::tree).
State prices are kept at the steps in KEEP; their sums at every step."
  (multiple-value-bind (dxs jmins ks probabilities)
      (build-trinomial-tree (hull-white-a process) (sigma process) grid)
    (let* ((n (1- (length grid)))
           (sizes (let ((sizes (make-array (1+ n) :element-type 'fixnum :initial-element 1)))
                    (dotimes (i n sizes)
                      (let ((k (svref ks i)))
                        (setf (aref sizes (1+ i)) (+ (- (reduce #'max k) (reduce #'min k)) 3))))))
           (lattice (%make-hull-white-lattice
                     :times grid :dxs dxs :jmins jmins :ks ks :probabilities probabilities
                     :sizes sizes
                     :phi (make-array n :element-type 'double-float :initial-element 0d0)
                     :state-prices (make-array (1+ n) :initial-element nil)
                     :state-price-sums (make-array (1+ n) :element-type 'double-float
                                                          :initial-element 1d0)))
           (prices (make-array 1 :element-type 'double-float :initial-element 1d0)))
      (when (member 0 keep) (setf (svref (lattice-state-prices lattice) 0) prices))
      (dotimes (i n lattice)
        (let* ((bond (rate-curve-discount curve (aref grid (1+ i))))
               (dt (lattice-dt lattice i))
               (dx (aref dxs i))
               (x (lattice-x lattice i 0))
               (value 0d0))
          (dotimes (j (lattice-size lattice i))
            (incf value (* (aref prices j) (exp (- (* x dt)))))
            (incf x dx))
          (setf (aref (lattice-phi lattice) i) (/ (log (/ value bond)) dt)
                prices (next-state-prices lattice i prices)
                (aref (lattice-state-price-sums lattice) (1+ i)) (reduce #'+ prices))
          (when (member (1+ i) keep) (setf (svref (lattice-state-prices lattice) (1+ i)) prices)))))))

(defun lattice-step-back (lattice i values)
  "Values at step I from VALUES at step I + 1: expectation, then discount."
  (let ((new (make-array (lattice-size lattice i) :element-type 'double-float)))
    (dotimes (j (length new) new)
      (let ((value 0d0))
        (dotimes (branch 3)
          (incf value (* (lattice-probability lattice i j branch)
                         (aref values (lattice-descendant lattice i j branch)))))
        (setf (aref new j) (* value (lattice-discount lattice i j)))))))

(defun lattice-roll-back (lattice values from to)
  "VALUES at step FROM rolled back to step TO, with nothing added between."
  (iter (for i from (1- from) downto to)
        (setf values (lattice-step-back lattice i values)))
  values)

(defun lattice-present-value (lattice index values)
  (let ((prices (lattice-state-prices-through lattice index)))
    (iter (for j below (length values)) (sum (* (aref values j) (aref prices j))))))

;;; --------------------------------------------------------------------
;;; Swaptions on the tree
;;; --------------------------------------------------------------------

(defun swaption-events (swaption market)
  "(values fixed floating exercise-times): fixed coupons as (reset-time
pay-time amount), floating ones as (reset-time pay-time accrual spread), in
market time."
  (let* ((swap (swaption-swap swaption))
         (time (lambda (date) (swap-market-time market date))))
    (values (mapcar (lambda (c) `(,(funcall time (accrual-start c)) ,(funcall time (payment-date c))
                                  ,(cashflow-amount c market)))
                    (first (swap-legs swap)))
            (mapcar (lambda (c) `(,(funcall time (accrual-start c)) ,(funcall time (payment-date c))
                                  ,(accrual-period c) ,(coupon-spread c)))
                    (second (swap-legs swap)))
            (mapcar time (let ((x (swaption-exercise swaption)))
                           (if (typep x 'bermudan-exercise) (exercise-dates x) `(,(expiry x))))))))

(defmethod price ((s swaption) (e hull-white-tree-engine) market)
  (unless (eq (swaption-settlement s) :physical)
    (error 'unsupported-combination :exercise (swaption-exercise s) :payoff s :path nil :engine e
                                    :market market))
  (multiple-value-bind (fixed floating exercises) (swaption-events s market)
    (let* ((swap (swaption-swap s))
           (sign (if (eq (swap-type swap) :payer) 1d0 -1d0))
           (nominal (swap-nominal swap))
           (exercises (remove-if #'minusp exercises))
           (grid (mandatory-time-grid (append (mapcan (lambda (f) (list (first f) (second f))) fixed)
                                              (mapcan (lambda (f) (list (first f) (second f))) floating)
                                              exercises)
                                      (engine-time-steps e)))
           (index (lambda (time) (grid-index grid time)))
           (exercise-indices (mapcar index exercises))
           (lattice (make-hull-white-lattice (process e) (model-curve e s market) grid
                                             :keep (list (reduce #'min exercise-indices))))
           (last-payment (funcall index (max (reduce #'max (mapcar #'second fixed))
                                             (reduce #'max (mapcar #'second floating)))))
           (first-exercise (reduce #'min exercise-indices))
           (last-exercise (reduce #'max exercise-indices))
           (swap-values (make-array (lattice-size lattice last-payment) :element-type 'double-float
                                                                        :initial-element 0d0))
           (option nil))
      ;; A coupon enters the swap's value at its reset step, worth its
      ;; discount bond from the payment step (QuantLib's DiscretizedSwap).
      ;; Each bond is a unit vector placed at the payment step and rolled
      ;; back with the swap: PENDING holds (reset-step bond coupon-value).
      (let ((pending '())
            (coupons (append (mapcar (lambda (f)
                                       (destructuring-bind (reset pay amount) f
                                         `(,reset ,pay ,(lambda (p) (- (* sign amount p))))))
                                     fixed)
                             (mapcar (lambda (f)
                                       (destructuring-bind (reset pay accrual spread) f
                                         (let ((accrued-spread (* nominal accrual spread)))
                                           `(,reset ,pay ,(lambda (p)
                                                            (* sign (+ (* nominal (- 1d0 p))
                                                                       (* accrued-spread p))))))))
                                     floating))))
      (flet ((add-coupons (i)
               (dolist (entry pending)
                 (destructuring-bind (reset bond value) entry
                   (when (= reset i)
                     (dotimes (j (length swap-values))
                       (incf (aref swap-values j) (funcall value (aref bond j)))))))
               (setf pending (remove i pending :key #'first))
               ;; Bonds of coupons paying at step I start here.
               (dolist (c coupons)
                 (destructuring-bind (reset pay value) c
                   (when (and (>= reset 0d0) (= i (funcall index pay)))
                     (let ((reset-step (funcall index reset))
                           (bond (make-array (lattice-size lattice i) :element-type 'double-float
                                                                      :initial-element 1d0)))
                       (if (= reset-step i)
                           (dotimes (j (length swap-values))
                             (incf (aref swap-values j) (funcall value (aref bond j))))
                           (push `(,reset-step ,bond ,value) pending)))))))
             (roll-pending (i)
               (setf pending (mapcar (lambda (entry)
                                       (destructuring-bind (reset bond value) entry
                                         `(,reset ,(lattice-step-back lattice i bond) ,value)))
                                     pending)))
             (exercise (i)
               (when (member i exercise-indices)
                 (dotimes (j (length option))
                   (setf (aref option j) (max (aref swap-values j) (aref option j)))))))
        (add-coupons last-payment)
        (iter (for i from (1- last-payment) downto first-exercise)
              (setf swap-values (lattice-step-back lattice i swap-values))
              (roll-pending i)
              (if option
                  (setf option (lattice-step-back lattice i option))
                  (when (= i last-exercise)
                    (setf option (make-array (length swap-values) :element-type 'double-float
                                                                  :initial-element 0d0))))
              (add-coupons i)
              (when option (exercise i)))
        (when (= last-payment last-exercise)
          (setf option (copy-seq swap-values))
          (exercise last-payment))
        (values (lattice-present-value lattice first-exercise option) nil))))))

;;; --------------------------------------------------------------------
;;; Callable bonds on the tree
;;; --------------------------------------------------------------------

(defmethod price ((b callable-fixed-rate-bond) (e hull-white-tree-engine) market)
  (let* ((today (valuation-date market))
         (settlement (bond-settlement-date b today))
         (curve (model-curve e b market))
         (time (lambda (date) (swap-market-time market date)))
         (coupons (mapcar (lambda (c) `(,(funcall time (payment-date c)) ,(cashflow-amount c market) :post
                                        ,(payment-date c)))
                          (remove-if (lambda (cf) (occurred-p cf settlement)) (butlast (bond-cashflows b)))))
         (redemption (a:lastcar (bond-cashflows b)))
         (calls
           ;; A call date up to a week before a coupon date moves onto it
           ;; (its price carried by the discount ratio) and is applied
           ;; after that coupon is added (QuantLib's
           ;; DiscretizedCallableFixedRateBond).
           (iter (for (date clean type) in (bond-call-schedule b))
                 (unless (date<= date settlement)
                   (let ((call-time (funcall time date))
                         (strike (callable-dirty-price b date clean)))
                     (iter (for coupon in coupons)
                           (destructuring-bind (coupon-time amount adjustment coupon-date) coupon
                             (declare (ignore amount adjustment))
                             (when (and (<= call-time coupon-time (+ call-time (/ 1d0 52)))
                                        (date< date coupon-date))
                               (flet ((df (d)
                                        (let* ((tm (funcall time d))
                                               (zero (/ (log (/ 1d0 (rate-curve-discount curve tm))) tm)))
                                          (exp (- (* zero tm))))))
                                 (setf call-time coupon-time
                                       (third coupon) :pre
                                       strike (* strike (/ (df date) (df coupon-date)))))
                               (finish))))
                     (collect `(,call-time ,(* strike (/ (bond-face b) 100d0)) ,type))))))
         (grid (mandatory-time-grid (append (list (funcall time (payment-date redemption)))
                                            (mapcar #'first coupons) (mapcar #'first calls))
                                    (engine-time-steps e)))
         (index (lambda (tm) (grid-index grid tm)))
         (lattice (make-hull-white-lattice (process e) curve grid))
         (last (funcall index (funcall time (payment-date redemption))))
         (values (make-array (lattice-size lattice last) :element-type 'double-float
                                                         :initial-element (cashflow-amount redemption market))))
    (flet ((add-coupons (i when)
             (dolist (c coupons)
               (destructuring-bind (tm amount adjustment date) c
                 (declare (ignore date))
                 (when (and (eq adjustment when) (= i (funcall index tm)))
                   (dotimes (j (length values)) (incf (aref values j) amount))))))
           (apply-calls (i)
             (dolist (call calls)
               (destructuring-bind (tm strike type) call
                 (when (= i (funcall index tm))
                   (dotimes (j (length values))
                     (setf (aref values j) (if (eq type :call)
                                               (min strike (aref values j))
                                               (max (aref values j) strike)))))))))
      ;; At each step: coupons moved before a call, the calls (the holder
      ;; gets the call price or keeps the bond), then the coupons paid there.
      (iter (for i from last downto 0)
            (unless (= i last) (setf values (lattice-step-back lattice i values)))
            (add-coupons i :pre)
            (apply-calls i)
            (add-coupons i :post))
      (values (aref values 0) nil))))

(defun callable-dirty-price (bond date clean)
  "CLEAN (per 100) plus the interest accrued at DATE on the first coupon
paying after it (QuantLib's CallableBond call prices)."
  (let ((next (find-if (lambda (cf) (not (occurred-p cf date))) (bond-cashflows bond))))
    (+ clean (if (typep next 'coupon)
                 (/ (* (accrued-amount-to next date nil) 100d0) (bond-notional bond date))
                 0d0))))

;;; --------------------------------------------------------------------
;;; Calibration
;;; --------------------------------------------------------------------

(defun calibrate-hull-white (process market swaptions
                             &key (method :levenberg-marquardt) fix-a curve (vol-type :black)
                                  (displacement 0d0) (tolerance 1d-12))
  "A Hull-White process fitted to SWAPTIONS, a list of (swaption vol): each
European swaption's price under the model (Jamshidian) matches its price at
VOL, a Black vol with DISPLACEMENT (VOL-TYPE :BLACK) or a normal vol
(:NORMAL). Fits a and sigma, or sigma alone if FIX-A, starting from PROCESS,
by MINIMIZE's METHOD on the relative price errors. CURVE is the id of the
model curve (default the swaptions' currency's). Returns (values process
relative-errors iterations).

  (calibrate-hull-white (make-hull-white) m `((,s1 0.21d0) (,s2 0.205d0)) :fix-a t)"
  (check-input process hull-white "a HULL-WHITE process")
  (check-input swaptions (and list (not null)) "a non-empty list of (swaption vol)")
  (check-input vol-type (member :black :normal) ":BLACK or :NORMAL")
  (let* ((quote-engine (lambda (vol)
                         (if (eq vol-type :black)
                             (make-engine :black-rates :volatility vol :displacement displacement
                                                       :discount-curve curve)
                             (make-engine :bachelier-rates :volatility vol :discount-curve curve))))
         (targets (mapcar (lambda (q) (price (first q) (funcall quote-engine (second q)) market)) swaptions))
         (start (parameter-vector process market))
         (a (aref start 0)))
    (flet ((model (x)
             (with-parameter-vector process (if fix-a
                                                (make-array 2 :element-type 'double-float
                                                              :initial-contents `(,a ,(aref x 0)))
                                                x)))
           (errors (hw)
             (let ((engine (make-instance 'jamshidian-swaption-engine :process hw :curve curve)))
               (map 'double-vector (lambda (q target) (- (/ (price (first q) engine market) target) 1d0))
                    swaptions targets))))
      (multiple-value-bind (x fx iterations)
          (minimize method (lambda (x) (errors (model x)))
                    (if fix-a (subseq start 1) start)
                    :bounds (if fix-a '((0 . nil)) '((0 . nil) (0 . nil)))
                    :tolerance tolerance)
        (declare (ignore fx))
        (let ((fitted (model x)))
          (values fitted (errors fitted) iterations))))))
