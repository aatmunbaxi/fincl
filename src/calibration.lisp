;;;; calibration.lisp --- option quotes, smile fitting and model calibration
;;;;
;;;; Quotes are instruments with a market price or implied vol. From a set
;;;; of them: QUOTES-VOL-SURFACE interpolates a grid surface, FIT-SVI,
;;;; FIT-SSVI and FIT-SABR fit parametric smiles, and CALIBRATE fits any
;;;; process's parameters by repricing the quotes with an engine.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Quotes
;;; --------------------------------------------------------------------

(defclass option-quote ()
  ((instrument :initarg :instrument :reader quote-instrument)
   (price :initarg :price :reader %quote-price :initform nil)
   (vol :initarg :vol :reader %quote-vol :initform nil)
   (weight :initarg :weight :reader quote-weight :initform 1d0))
  (:documentation "A market quote for a European vanilla option: its price or
its Black implied vol, and a weight for fitting."))

(defun make-option-quote (instrument &key price vol (weight 1))
  "A quote for INSTRUMENT, a European vanilla option, by PRICE or by Black
implied VOL (exactly one), with WEIGHT in fits.

  (make-option-quote (make-option (call 100) (expires d)) :vol 0.21)"
  (check-input instrument option "an option")
  (unless (european-vanilla-p instrument)
    (signal-invalid-argument 'instrument instrument 'option "a European vanilla option"))
  (unless (a:xor price vol)
    (signal-invalid-argument 'price price 'real "either a price or a vol, not both"))
  (when price (check-input price (real 0) "a non-negative price"))
  (when vol (check-input vol (real (0)) "a positive vol"))
  (check-input weight (real 0) "a non-negative weight")
  (make-instance 'option-quote :instrument instrument
                               :price (and price (float price 1d0))
                               :vol (and vol (float vol 1d0))
                               :weight (float weight 1d0)))

(defclass quote-set ()
  ((quotes :initarg :quotes :reader quote-set-quotes :type list))
  (:documentation "Quotes fitted together."))

(defun quote-set (quotes)
  "A QUOTE-SET of QUOTES, a non-empty list of OPTION-QUOTEs."
  (check-input quotes (and list (not null)) "a non-empty list of option quotes")
  (dolist (q quotes) (check-input q option-quote "an option quote"))
  (make-instance 'quote-set :quotes (copy-list quotes)))

(defun as-quotes (quotes)
  (etypecase quotes
    (quote-set (quote-set-quotes quotes))
    (list (quote-set-quotes (quote-set quotes)))))

(defun quote-terms (q market)
  "(values forward strike discount tau phi expiry) of Q's option on MARKET."
  (let* ((o (quote-instrument q))
         (m (pricing-market market o))
         (x (option-exercise o))
         (p (option-payoff o)))
    (values (forward m (expiry x)) (strike p) (discount-factor m (expiry x))
            (time-to-expiry x m) (phi p) (expiry x))))

(defun quote-implied-vol (q market)
  "Q's Black implied vol on MARKET: its vol, or the vol of its price."
  (or (%quote-vol q)
      (multiple-value-bind (f k d tau phi) (quote-terms q market)
        (implied-black-vol phi (%quote-price q) f k d tau))))

(defun quote-price (q market)
  "Q's price on MARKET: its price, or Black's price at its vol."
  (or (%quote-price q)
      (multiple-value-bind (f k d tau phi) (quote-terms q market)
        (black-formula phi f k d (* (%quote-vol q) (%quote-vol q) tau)))))

(defun quote-points (quotes market)
  "Each quote as (tau k w weight vol): expiry in market time, log-forward
moneyness, total variance, weight, implied vol."
  (mapcar (lambda (q)
            (multiple-value-bind (f k d tau) (quote-terms q market)
              (declare (ignore d))
              (let ((vol (quote-implied-vol q market)))
                `(,tau ,(log (/ k f)) ,(* vol vol tau) ,(quote-weight q) ,vol))))
          (as-quotes quotes)))

(defun points-by-expiry (points)
  "POINTS grouped by expiry, each group sorted by moneyness."
  (let ((groups '()))
    (dolist (p points)
      (let ((group (assoc (first p) groups :test #'=)))
        (if group (push p (cdr group)) (push `(,(first p) ,p) groups))))
    (mapcar (lambda (g) (sort (copy-list (cdr g)) #'< :key #'second))
            (sort groups #'< :key #'car))))

(defun quotes-vol-surface (quotes market &key (interpolation :natural-cubic))
  "A GRID-VOL-SURFACE through QUOTES (a QUOTE-SET or list of quotes) on
MARKET: at each expiry, total variance interpolated in log-forward
moneyness by INTERPOLATION; between expiries linear in total variance.

  (quotes-vol-surface (quote-set quotes) market)"
  (make-smile-surface
   (mapcar (lambda (group)
             (make-interpolated-smile (first (first group)) (mapcar #'second group)
                                      (mapcar #'third group) :interpolation interpolation))
           (points-by-expiry (quote-points quotes market)))
   :class 'grid-vol-surface
   :check-range (reduce #'max (mapcar (lambda (p) (abs (second p))) (quote-points quotes market)))))

(defun residual-rmse (residuals)
  (sqrt (/ (reduce #'+ (map 'list (lambda (r) (* r r)) residuals)) (length residuals))))

;;; --------------------------------------------------------------------
;;; SVI: Zeletskii-De Marco quasi-explicit fit
;;; --------------------------------------------------------------------

(defun svi-inner-fit (ks ws weights m sigma)
  "For fixed M and SIGMA, the (a d c) minimizing sum weight (a + d y + c
sqrt(y^2 + 1) - w)^2, y = (k - m)/sigma, subject to 0 <= c <= 4 sigma, |d|
<= c, |d| <= 4 sigma - c and a <= max w (De Marco and Martini's
constraints), as (values (a d c) sse). The constrained least squares is
solved on every face of the feasible polytope of up to three active
constraints; the best feasible face solution is the optimum."
  (let* ((n (length ks))
         (rows (iter (for k in ks)
                     (for y = (/ (- k m) sigma))
                     (collect `(1d0 ,y ,(sqrt (+ (* y y) 1d0))))))
         (w-max (reduce #'max ws))
         (constraints `(((0d0 0d0 -1d0) . 0d0) ((0d0 0d0 1d0) . ,(* 4 sigma))
                        ((0d0 1d0 -1d0) . 0d0) ((0d0 -1d0 -1d0) . 0d0)
                        ((0d0 1d0 1d0) . ,(* 4 sigma)) ((0d0 -1d0 1d0) . ,(* 4 sigma))
                        ((1d0 0d0 0d0) . ,w-max)))
         (normal (make-array '(3 3) :element-type 'double-float :initial-element 0d0))
         (rhs (make-array 3 :element-type 'double-float :initial-element 0d0))
         (best nil) (best-sse most-positive-double-float))
    (declare (ignore n))
    (iter (for row in rows) (for w in ws) (for weight in weights)
          (dotimes (i 3)
            (incf (aref rhs i) (* 2 weight (nth i row) w))
            (dotimes (j 3)
              (incf (aref normal i j) (* 2 weight (nth i row) (nth j row))))))
    (labels ((sse (p)
               (iter (for row in rows) (for w in ws) (for weight in weights)
                     (sum (* weight (expt (- (iter (for x in row) (for c in p) (sum (* x c))) w) 2)))))
             (feasible-p (p)
               (every (lambda (con)
                        (<= (iter (for g in (car con)) (for c in p) (sum (* g c)))
                            (+ (cdr con) 1d-12)))
                      constraints))
             (try (active)
               (let* ((r (length active))
                      (size (+ 3 r))
                      (kkt (make-array `(,size ,size) :element-type 'double-float
                                                       :initial-element 0d0))
                      (b (make-array size :element-type 'double-float :initial-element 0d0)))
                 (dotimes (i 3)
                   (setf (aref b i) (aref rhs i))
                   (dotimes (j 3) (setf (aref kkt i j) (aref normal i j))))
                 (iter (for con in active) (for row from 3)
                       (iter (for g in (car con)) (for j from 0)
                             (setf (aref kkt row j) g (aref kkt j row) g))
                       (setf (aref b row) (cdr con)))
                 (handler-case
                     (let* ((x (solve-linear-system kkt b))
                            (p `(,(aref x 0) ,(aref x 1) ,(aref x 2))))
                       (when (feasible-p p)
                         (let ((s (sse p)))
                           (when (< s best-sse) (setf best p best-sse s)))))
                   (rank-deficient () nil)))))
      (try '())
      (iter (for (c1 . rest1) on constraints)
            (try `(,c1))
            (iter (for (c2 . rest2) on rest1)
                  (try `(,c1 ,c2))
                  (dolist (c3 rest2) (try `(,c1 ,c2 ,c3))))))
    (values best best-sse)))

(defun svi-residuals (ks ws weights)
  "The residual function of raw SVI parameters #(a b rho m sigma)."
  (lambda (x)
    (let ((a (aref x 0)) (b (aref x 1)) (rho (aref x 2)) (m (aref x 3)) (sigma (aref x 4)))
      (map 'double-vector
           (lambda (k w weight)
             (* (sqrt weight)
                (- (+ a (* b (+ (* rho (- k m)) (sqrt (+ (expt (- k m) 2) (* sigma sigma)))))) w)))
           ks ws weights))))

(defun fit-svi (quotes market &key initial)
  "A raw SVI-SLICE fitted to QUOTES (all at one expiry) on MARKET, in total
variance with the quotes' weights: Zeletskii and De Marco's quasi-explicit
reduction (a 2-D Nelder-Mead search over m and sigma, with a, b and rho from
a constrained linear least-squares step), then a Levenberg-Marquardt polish
of all five parameters. INITIAL, an SVI-SLICE, replaces the search's start.
Returns (values slice rmse), RMSE that of the implied vols.

  (fit-svi (quote-set quotes) market)"
  (let* ((points (quote-points quotes market))
         (expiries (remove-duplicates (mapcar #'first points) :test #'=)))
    (unless (= 1 (length expiries))
      (signal-invalid-argument 'quotes quotes 'quote-set "quotes at a single expiry"))
    (let* ((tau (first expiries))
           (ks (mapcar #'second points)) (ws (mapcar #'third points)) (weights (mapcar #'fourth points))
           (k-lo (reduce #'min ks)) (k-hi (reduce #'max ks))
           (span (max (- k-hi k-lo) 1d-3))
           (start (if initial
                      `(,(svi-m initial) ,(svi-sigma initial))
                      `(,(nth (position (reduce #'min ws) ws) ks) ,(* 0.2d0 span))))
           (bounds `((,(- k-lo span) . ,(+ k-hi span)) (1d-4 . ,(* 10 span)))))
      (multiple-value-bind (ms)
          (handler-bind ((convergence-failure #'use-best))
            (minimize :nelder-mead
                      (lambda (x) (nth-value 1 (svi-inner-fit ks ws weights (aref x 0) (aref x 1))))
                      start :bounds bounds :tolerance 1d-12 :max-iterations 2000))
        (destructuring-bind (a d c) (svi-inner-fit ks ws weights (aref ms 0) (aref ms 1))
          (let* ((sigma (aref ms 1))
                 (b (max (/ c sigma) 1d-10))
                 (rho (max -0.999999d0 (min 0.999999d0 (if (plusp c) (/ d c) 0d0))))
                 (polished (handler-bind ((convergence-failure #'use-best))
                             (minimize :levenberg-marquardt (svi-residuals ks ws weights)
                                       `(,a ,b ,rho ,(aref ms 0) ,sigma)
                                       :bounds '((nil . nil) (0d0 . nil) (-1d0 . 1d0) (nil . nil) (0d0 . nil))
                                       :tolerance 1d-15 :max-iterations 500)))
                 (slice (make-svi-slice tau :a (aref polished 0) :b (aref polished 1)
                                            :rho (aref polished 2) :m (aref polished 3)
                                            :sigma (aref polished 4))))
            (values slice
                    (residual-rmse (mapcar (lambda (p) (- (smile-vol slice (second p)) (fifth p)))
                                           points)))))))))

;;; --------------------------------------------------------------------
;;; SSVI
;;; --------------------------------------------------------------------

(defun fit-ssvi (quotes market &key (rho -0.3d0) (gamma 0.4d0))
  "An SSVI-SURFACE fitted to QUOTES (several expiries) on MARKET by
Levenberg-Marquardt on implied-vol residuals: rho, gamma in (0, 1/2], eta =
2u/(1 + |rho|) with u in (0, 1] (so eta (1 + |rho|) <= 2, free of static
arbitrage), and the ATM total variance at each expiry, kept increasing.
RHO and GAMMA start the search; the ATM variances start from the quotes.
Returns (values surface rmse)."
  (let* ((points (quote-points quotes market))
         (groups (points-by-expiry points))
         (times (mapcar (lambda (g) (first (first g))) groups))
         (atm (mapcar (lambda (g)
                        (let ((smile (make-interpolated-smile (first (first g)) (mapcar #'second g)
                                                              (mapcar #'third g) :interpolation :linear)))
                          (smile-variance smile 0d0)))
                      groups))
         ;; Increments of the ATM variance, so the fitted ones stay increasing.
         (increments (iter (for theta in atm)
                           (for previous initially 0d0 then (max previous theta))
                           (collect (max 1d-6 (- theta previous)))))
         (n (length times)))
    (flet ((surface (x)
             (let ((rho (aref x 0)) (u (aref x 1)) (gamma (aref x 2)))
               (make-ssvi-surface times
                                  (iter (for i below n) (sum (aref x (+ 3 i)) into theta)
                                        (collect theta))
                                  :rho rho :eta (/ (* 2 u) (+ 1d0 (abs rho))) :gamma gamma))))
      (let* ((x (handler-bind ((convergence-failure #'use-best))
                  (minimize :levenberg-marquardt
                            (lambda (x)
                              (let ((s (surface x)))
                                (map 'double-vector
                                     (lambda (p)
                                       (* (sqrt (fourth p))
                                          (- (sqrt (/ (surface-variance s (second p) (first p))
                                                      (first p)))
                                             (fifth p))))
                                     points)))
                            `(,rho 0.5d0 ,gamma ,@increments)
                            :bounds `((-1d0 . 1d0) (0d0 . 1d0) (0d0 . 0.5d0)
                                      ,@(make-list n :initial-element '(0d0 . nil)))
                            :tolerance 1d-15 :max-iterations 1000)))
             (s (surface x)))
        (values s (residual-rmse (mapcar (lambda (p)
                                           (- (sqrt (/ (surface-variance s (second p) (first p))
                                                       (first p)))
                                              (fifth p)))
                                         points)))))))

;;; --------------------------------------------------------------------
;;; SABR
;;; --------------------------------------------------------------------

(defun fit-sabr (quotes market &key beta (approximation :obloj))
  "A SABR-SMILE fitted to QUOTES (one expiry) on MARKET by
Levenberg-Marquardt on implied-vol residuals: alpha, rho, nu, and beta
unless BETA fixes it. Returns (values smile rmse).

  (fit-sabr (quote-set quotes) market :beta 0.5)"
  (let* ((points (quote-points quotes market))
         (expiries (remove-duplicates (mapcar #'first points) :test #'=)))
    (unless (= 1 (length expiries))
      (signal-invalid-argument 'quotes quotes 'quote-set "quotes at a single expiry"))
    (let* ((tau (first expiries))
           (forward (quote-terms (first (as-quotes quotes)) market))
           (atm-vol (fifth (first (sort (copy-list points) #'< :key (lambda (p) (abs (second p)))))))
           (fixed-beta (and beta (float beta 1d0))))
      (flet ((smile (x)
               (make-sabr-smile tau forward :alpha (aref x 0) :rho (aref x 1) :nu (aref x 2)
                                            :beta (or fixed-beta (aref x 3))
                                            :approximation approximation)))
        (let* ((b0 (or fixed-beta 0.5d0))
               (x (handler-bind ((convergence-failure #'use-best))
                    (minimize :levenberg-marquardt
                              (lambda (x)
                                (let ((s (smile x)))
                                  (map 'double-vector
                                       (lambda (p) (* (sqrt (fourth p)) (- (smile-vol s (second p)) (fifth p))))
                                       points)))
                              `(,(* atm-vol (expt forward (- 1d0 b0))) 0d0 0.3d0
                                ,@(unless fixed-beta '(0.5d0)))
                              :bounds `((0d0 . nil) (-1d0 . 1d0) (0d0 . nil)
                                        ,@(unless fixed-beta '((0d0 . 1d0))))
                              :tolerance 1d-15 :max-iterations 1000)))
               (s (smile x)))
          (values s (residual-rmse (mapcar (lambda (p) (- (smile-vol s (second p)) (fifth p)))
                                           points))))))))

;;; --------------------------------------------------------------------
;;; Generic calibration
;;; --------------------------------------------------------------------

(defun engine-with-process (engine process)
  "ENGINE (an engine, a keyword or a (keyword settings...) list) with
PROCESS, or PROCESS itself (automatic selection) when ENGINE is NIL."
  (etypecase engine
    (null process)
    (keyword (make-engine engine :process process))
    (cons (apply #'make-engine (first engine) :process process (rest engine)))
    (engine (derive-engine engine :process process))))

(defun bound-pair (lo hi)
  "MINIMIZE's (lo . hi) from interval designators: a real or (x), or NIL."
  (flet ((end (x) (if (consp x) (first x) x)))
    `(,(and lo (float (end lo) 1d0)) . ,(and hi (float (end hi) 1d0)))))

(defun calibrate (process market quotes &key engine (objective :implied-vol-rmse)
                                             (method :levenberg-marquardt) fixed
                                             (tolerance 1d-12) (max-iterations 200))
  "Fit PROCESS's parameters to QUOTES (a QUOTE-SET or list of option quotes)
on MARKET. Returns (values calibrated-process report).

  ENGINE     prices each quote with the trial process: an engine, a keyword
             or a (keyword settings...) list, given :PROCESS; NIL selects an
             engine automatically for the process.
  OBJECTIVE  :IMPLIED-VOL-RMSE (residuals are model minus quoted Black vols)
             or :PRICE-RMSE (prices), each times sqrt(weight).
  METHOD     :LEVENBERG-MARQUARDT, or :NELDER-MEAD or :DIFFERENTIAL-EVOLUTION
             on the sum of squares (see MINIMIZE).
  FIXED      parameters held: names (keeping PROCESS's value, or the market
             default if it has none), or (name value) lists.

Parameters, their order and bounds come from PROCESS-PARAMETERS and
PARAMETER-BOUNDS; the search starts from PROCESS's values, market defaults
resolved at the longest quote's expiry. Quotes are repriced in parallel
under lp:*kernel* (serially for Monte Carlo engines, which are parallel
themselves); the result does not depend on the worker count. The report is
a plist: :RMSE, :MAX-ERROR, :ITERATIONS, :RESIDUALS (one per quote, in
order) and :PARAMETERS (name value ...).

  (calibrate (make-heston) market (quote-set quotes) :engine :cos)"
  (check-input objective (member :implied-vol-rmse :price-rmse) ":IMPLIED-VOL-RMSE or :PRICE-RMSE")
  (check-input method (member :levenberg-marquardt :nelder-mead :differential-evolution)
               ":LEVENBERG-MARQUARDT, :NELDER-MEAD or :DIFFERENTIAL-EVOLUTION")
  (let* ((quotes (as-quotes quotes))
         (names (process-parameters process))
         (horizon (reduce #'max (mapcar (lambda (q) (nth-value 3 (quote-terms q market))) quotes)))
         (start (parameter-vector process market horizon))
         (fixed-values (iter (for item in fixed)
                             (for name = (if (consp item) (first item) item))
                             (for slot-name = (c2mop:slot-definition-name (find-managed-slot process name)))
                             (for position = (position slot-name names))
                             (unless position
                               (signal-invalid-argument 'fixed fixed 'list
                                                        (format nil "parameters of ~(~A~): ~{~(~A~)~^, ~}"
                                                                (class-name (class-of process)) names)))
                             (collect `(,position . ,(float (if (consp item) (second item) (aref start position))
                                                            1d0)))))
         (free (iter (for i below (length names))
                     (unless (assoc i fixed-values) (collect i))))
         (bounds (mapcar (lambda (i) (multiple-value-call #'bound-pair (parameter-bounds process (nth i names))))
                         free))
         ;; Start strictly inside the bounds.
         (x0 (iter (for i in free) (for (lo . hi) in bounds)
                   (for x = (aref start i))
                   (collect (let* ((width (if (and lo hi) (- hi lo) 1d0))
                                   (margin (* 1d-3 width)))
                              (cond ((and lo (<= x (+ lo margin))) (+ lo margin))
                                    ((and hi (>= x (- hi margin))) (- hi margin))
                                    (t x))))))
         (targets (mapcar (lambda (q) (if (eq objective :price-rmse)
                                          (quote-price q market)
                                          (quote-implied-vol q market)))
                          quotes))
         (serial (typep (engine-with-process engine process) 'monte-carlo-engine))
         (iterations 0))
    (labels ((trial (x)
               (let ((v (copy-seq start)))
                 (iter (for (i . value) in fixed-values) (setf (aref v i) value))
                 (iter (for i in free) (for k from 0) (setf (aref v i) (aref x k)))
                 (with-parameter-vector process v)))
             (model-value (q trial-engine)
               (let ((p (nth-value 0 (price (quote-instrument q) trial-engine market))))
                 (if (eq objective :price-rmse)
                     p
                     (multiple-value-bind (f k d tau phi) (quote-terms q market)
                       ;; A price outside Black's range is a large residual,
                       ;; not an error: the optimizer moves away from it.
                       (handler-case (implied-black-vol phi p f k d tau)
                         (price-out-of-bounds () 10d0))))))
             (residuals (x)
               (incf iterations)
               (let* ((trial-engine (engine-with-process engine (trial x)))
                      (values (if serial
                                  (mapcar (lambda (q) (model-value q trial-engine)) quotes)
                                  (with-pricing-kernel ()
                                    (lp:pmap 'list (lambda (q) (model-value q trial-engine)) quotes)))))
                 (map 'double-vector (lambda (value target q) (* (sqrt (quote-weight q)) (- value target)))
                      values targets quotes))))
      (let* ((x (handler-bind ((convergence-failure #'use-best))
                  (if (eq method :levenberg-marquardt)
                      ;; Model prices come from numerical methods (COS
                      ;; truncation, quadrature), whose noise swamps a
                      ;; difference quotient at the default step of ~1e-8.
                      ;; Central differences at 1e-5 max(|x|, 0.01) sit well
                      ;; above it.
                      (minimize method #'residuals x0 :bounds bounds :tolerance tolerance
                                                      :max-iterations max-iterations
                                                      :jacobian (lambda (x out)
                                                                  (jacobian! #'residuals x out
                                                                             :scheme :central
                                                                             :steps (map 'list (lambda (v) (* 1d-5 (max 1d-2 (abs v)))) x))))
                      (minimize method (lambda (x) (sum-of-squares (residuals x))) x0
                                :bounds bounds :tolerance tolerance :max-iterations max-iterations))))
             (calibrated (trial x))
             (final (residuals x)))
        (values calibrated
                `(:rmse ,(residual-rmse final)
                  :max-error ,(reduce #'max (map 'list #'abs final))
                  :iterations ,iterations
                  :residuals ,(coerce final 'list)
                  :parameters ,(iter (for name in names)
                                     (for value in-vector (parameter-vector calibrated market horizon))
                                     (appending `(,(a:make-keyword name) ,value)))))))))
