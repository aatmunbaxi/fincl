;;;; calibration.lisp --- option quotes, smile fitting and model calibration
;;;;
;;;; Quotes are instruments with a market price or an implied vol. A set of quotes
;;;; gives these results:
;;;; - QUOTES-VOL-SURFACE interpolates a grid surface.
;;;; - FIT-SVI, FIT-SSVI and FIT-SABR fit parametric smiles.
;;;; - CALIBRATE fits the parameters of any process. It reprices the quotes with an
;;;;   engine.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Quotes
;;; --------------------------------------------------------------------

(defclass option-quote ()
  ((instrument :initarg :instrument :reader quote-instrument)
   (price :initarg :price :reader %quote-price :initform nil :type (or null non-negative-double))
   (vol :initarg :vol :reader %quote-vol :initform nil :type (or null volatility))
   (weight :initarg :weight :reader quote-weight :initform 1d0 :type non-negative-double))
  (:documentation "A market quote for a European vanilla option. It holds a price or a Black implied
vol, and a weight for fitting."))

(declaim (ftype (function (t &key (:price t) (:vol t) (:weight t))
                    (values option-quote &optional))
          make-option-quote))
(defun make-option-quote (instrument &key price vol (weight 1))
  "Make a quote for INSTRUMENT, a European vanilla option. Give the quote by PRICE or
by Black implied VOL (exactly one). WEIGHT is the weight in fits.

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
  (:documentation "Quotes that a fit uses together."))

(declaim (ftype (function (t) (values quote-set &optional)) quote-set))
(defun quote-set (quotes)
  "Make a QUOTE-SET of QUOTES, a non-empty list of OPTION-QUOTEs."
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

(declaim (ftype (function (option-quote t) (values double-float &optional)) quote-implied-vol))
(defun quote-implied-vol (q market)
  "The Black implied vol of Q on MARKET. It is the vol of Q, or the vol of the price of
Q."
  (or (%quote-vol q)
      (multiple-value-bind (f k d tau phi) (quote-terms q market)
        (implied-black-vol phi (%quote-price q) f k d tau))))

(declaim (ftype (function (option-quote t) (values double-float &optional)) quote-price))
(defun quote-price (q market)
  "The price of Q on MARKET. It is the price of Q, or the Black price at the vol of Q."
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

(declaim (ftype (function (t t &key (:interpolation t))
                    (values grid-vol-surface &optional))
          quotes-vol-surface))
(defun quotes-vol-surface (quotes market &key (interpolation :natural-cubic))
  "Make a GRID-VOL-SURFACE through QUOTES (a QUOTE-SET or a list of quotes) on
MARKET. At each expiry, INTERPOLATION interpolates total variance in log-forward
moneyness. Between expiries, total variance is linear.

  (quotes-vol-surface (quote-set quotes) market)"
  (make-smile-surface
   (mapcar (lambda (group)
             (make-interpolated-smile :expiry (first (first group))
                                      :log-moneyness (mapcar #'second group)
                                      :total-variances (mapcar #'third group)
                                      :interpolation interpolation))
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
      ;; The problem is convex: a feasible unconstrained minimum is the optimum.
      (unless best
        (iter (for (c1 . rest1) on constraints)
              (try `(,c1))
              (iter (for (c2 . rest2) on rest1)
                    (try `(,c1 ,c2))
                    (dolist (c3 rest2) (try `(,c1 ,c2 ,c3)))))))
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

(declaim (ftype (function (t t &key (:initial t)) (values svi-slice real &optional)) fit-svi))
(defun fit-svi (quotes market &key initial)
  "Fit a raw SVI-SLICE to QUOTES (all at one expiry) on MARKET. The fit is in total
variance with the weights of the quotes. It uses the quasi-explicit reduction of
Zeletskii and De Marco: a 2-D Nelder-Mead search over m and sigma, with a, b and rho
from a constrained linear least-squares step. Then a Levenberg-Marquardt step
polishes all five parameters. INITIAL, an SVI-SLICE, replaces the start of the
search. Return (values slice rmse). RMSE is the error of the implied vols.

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

(declaim (ftype (function (t t &key (:rho real) (:gamma real))
                    (values ssvi-surface real &optional))
          fit-ssvi))
(defun fit-ssvi (quotes market &key (rho -0.3d0) (gamma 0.4d0))
  "Fit an SSVI-SURFACE to QUOTES (several expiries) on MARKET. The fit is a
Levenberg-Marquardt fit on implied-vol residuals. The fitted values are rho, gamma
in (0, 1/2], eta = 2u/(1 + |rho|) with u in (0, 1] (so eta (1 + |rho|) <= 2, free
of static arbitrage), and the ATM total variance at each expiry, kept increasing.
RHO and GAMMA are the start of the search. The ATM variances start from the quotes.
Return (values surface rmse)."
  (let* ((points (quote-points quotes market))
         (groups (points-by-expiry points))
         (times (mapcar (lambda (g) (first (first g))) groups))
         (atm (mapcar (lambda (g)
                        (let ((smile (make-interpolated-smile :expiry (first (first g))
                                                              :log-moneyness (mapcar #'second g)
                                                              :total-variances (mapcar #'third g)
                                                              :interpolation :linear)))
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

(declaim (ftype (function (t t &key (:beta (or null real)) (:approximation t))
                    (values sabr-smile real &optional))
          fit-sabr))
(defun fit-sabr (quotes market &key beta (approximation :obloj))
  "Fit a SABR-SMILE to QUOTES (one expiry) on MARKET. The fit is a Levenberg-Marquardt
fit on implied-vol residuals. The fitted values are alpha, rho, nu, and beta unless
BETA fixes it. Return (values smile rmse).

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

(declaim (ftype (function (t t t &key (:engine t) (:objective t) (:method t) (:fixed list) (:tolerance real) (:max-iterations (integer 0)))
                    (values t cons &optional))
          calibrate))
(defun calibrate (process market quotes &key engine (objective :implied-vol-rmse)
                                             (method :levenberg-marquardt) fixed
                                             (tolerance 1d-12) (max-iterations 200))
  "Fit the parameters of PROCESS to QUOTES (a QUOTE-SET or a list of option quotes)
on MARKET. Return (values calibrated-process report).

  ENGINE     Prices each quote with the trial process. An engine, a keyword or
             a (keyword settings...) list, given :PROCESS. NIL selects an
             engine automatically for the process.
  OBJECTIVE  :IMPLIED-VOL-RMSE (the residuals are model minus quoted Black vols)
             or :PRICE-RMSE (prices). Each is multiplied by sqrt(weight).
  METHOD     :LEVENBERG-MARQUARDT, or :NELDER-MEAD or :DIFFERENTIAL-EVOLUTION
             on the sum of squares (see MINIMIZE).
  FIXED      The parameters that stay fixed: names (they keep the value of
             PROCESS, or the market default if it has none), or (name value)
             lists.

PROCESS-PARAMETERS and PARAMETER-BOUNDS give the parameters, their order and their
bounds. The search starts from the values of PROCESS. The market defaults are
resolved at the expiry of the longest quote. The function reprices the quotes in
parallel under lp:*kernel*. It reprices them in serial for Monte Carlo engines,
which are parallel themselves. The result does not depend on the worker count.
The report is a plist: :RMSE, :MAX-ERROR, :ITERATIONS, :RESIDUALS (one for each
quote, in order) and :PARAMETERS (name value ...).

  (calibrate (make-heston) market (quote-set quotes) :engine :cos)"
  (require-risk-neutral process "CALIBRATE")
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
               (if (eq objective :price-rmse)
                   (nth-value 0 (price (quote-instrument q) trial-engine market))
                   ;; A trial with no variance (COS refuses it) or a price
                   ;; outside Black's range is a large residual, not an
                   ;; error: the optimizer moves away from it.
                   (handler-case
                       (let ((p (nth-value 0 (price (quote-instrument q) trial-engine market))))
                         (multiple-value-bind (f k d tau phi) (quote-terms q market)
                           (implied-black-vol phi p f k d tau)))
                     ((or price-out-of-bounds non-positive-volatility) () 10d0))))
             (residuals (x)
               (incf iterations)
               (let* ((trial-engine (engine-with-process engine (trial x)))
                      (values (if serial
                                  (mapcar (lambda (q) (model-value q trial-engine)) quotes)
                                  (with-pricing-kernel ()
                                    (parallel-map 'list (lambda (q) (model-value q trial-engine)) quotes)))))
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
                                                                             :scheme :central :bounds bounds
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

;;; --------------------------------------------------------------------
;;; Quotes read off a market's surface
;;; --------------------------------------------------------------------

(declaim (ftype (function (t t t &key (:weight real)) (values quote-set &optional)) surface-quotes))
(defun surface-quotes (market underlying options &key (weight 1))
  "Make a QUOTE-SET that quotes each of OPTIONS (European vanillas) at its Black vol on
the vol structure of MARKET for UNDERLYING. The quote has the strike and the expiry
of the option, and WEIGHT. The function leaves out options that expire on or before
the valuation date of MARKET. Thus one list serves every date of a simulation. If no
option is left, it signals INVALID-ARGUMENT.

  (calibrate desk market (surface-quotes market :spx options) :engine :cos)"
  (check-input market composite-market "a composite market")
  (let* ((view (market-view market :underlying underlying))
         (today (valuation-date market))
         (live (remove-if-not (lambda (o)
                                (check-input o option "a European vanilla option")
                                (date< today (expiry (option-exercise o))))
                              options)))
    (unless live
      (signal-invalid-argument 'options options 'list
                               (format nil "options expiring after ~A, the market's valuation date" today)))
    (quote-set (mapcar (lambda (o)
                         (make-option-quote o :vol (black-vol view (strike (option-payoff o)) (expiry (option-exercise o)))
                                              :weight weight))
                       live))))
