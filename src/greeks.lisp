;;;; greeks.lisp --- sensitivities from any engine
;;;;
;;;; (greeks instrument engine market) returns a plist of sensitivities,
;;;; each by the best method the engine supports:
;;;;
;;;;   Black-Scholes   closed form (European vanillas)
;;;;   binomial        delta, gamma and theta from the tree's step-1 and
;;;;                   step-2 nodes, extrapolated like the price; the rest by
;;;;                   bumping
;;;;   anything else   central differences of prices on bumped markets
;;;;                   (BUMP-GREEKS); Monte Carlo averages independent batches
;;;;                   and reports standard errors
;;;;
;;;; Conventions: vega and volga per unit of vol, rho and dividend rho per
;;;; unit of rate (parallel shifts of the curve), theta per calendar day: the
;;;; price change from aging the market one day. Every finite difference
;;;; goes through FINITE-DIFFERENCE-WEIGHTS and APPLY-STENCIL.

(in-package #:fincl)

(defparameter *greek-keys* '(:delta :gamma :vega :theta :rho :dividend-rho :vanna :volga)
  "Every Greek GREEKS can return.")

(defparameter *default-greek-bumps* '(:spot 1d-4 :vol 1d-4 :rate 1d-4 :dividend 1d-4)
  "Bump sizes: relative for the spot, absolute for the vol and the rates.")

(defgeneric default-greek-bumps (engine)
  (:documentation "The bump sizes BUMP-GREEKS uses with ENGINE unless told
otherwise: *DEFAULT-GREEK-BUMPS*, except where the engine's price is not
smooth at that scale.")
  (:method (engine) (declare (ignore engine)) *default-greek-bumps*)
  (:method ((e binomial-engine))
    ;; A tree's price carries lattice noise as the spot or vol moves the
    ;; nodes against the strike; 1e-3 steps average over it (1e-4 spot steps
    ;; miss the American put delta by 2e-4 at N = 2000, 1e-3 by 5e-6).
    '(:spot 1d-3 :vol 1d-3 :rate 1d-4 :dividend 1d-4)))

(defgeneric greeks (instrument engine market &key which bumps)
  (:documentation "Sensitivities of INSTRUMENT's price under ENGINE in MARKET, as
a plist with the keys in WHICH (default every key of *GREEK-KEYS*: :delta
:gamma :vega :theta :rho :dividend-rho :vanna :volga). BUMPS overrides
*DEFAULT-GREEK-BUMPS* for the methods that bump. Monte Carlo results also
carry :standard-errors, a plist with the same keys.

  (greeks call (make-instance 'black-scholes-engine) m :which '(:delta :vega))
  => (:DELTA 0.6368... :VEGA 37.52...)"))

(macrolet ((designator-method (type)
             `(defmethod greeks (instrument (designator ,type) market &rest options &key which bumps)
                (declare (ignore which bumps))
                ;; Select once, on the unbumped market; that engine prices
                ;; every bumped market.
                (apply #'greeks instrument (resolve-designator instrument designator market) market
                       options))))
  (designator-method symbol)
  (designator-method cons)
  (designator-method stochastic-process))

(defun one-day (market)
  "The market time of one calendar day from MARKET's valuation date."
  (market-time market (date+ (valuation-date market) 1)))

(defun stencil-derivative (offsets order h value-at)
  "The ORDER-th derivative estimate from the values (VALUE-AT k) at OFFSETS
(in units of H), with Fornberg weights; VALUE-AT is called only where a
weight is non-zero."
  (let ((weights (finite-difference-weights offsets order)))
    (apply-stencil weights
                   (map 'list (lambda (k w) (if (zerop w) 0d0 (funcall value-at k)))
                        offsets weights)
                   h order)))

;;; --------------------------------------------------------------------
;;; Bumping
;;; --------------------------------------------------------------------

(defun bump-greeks (instrument engine market &key (which *greek-keys*) bumps)
  "Greeks by central differences of prices on bumped copies of MARKET, a
COMPOSITE-MARKET: the spot, vol and dividend of INSTRUMENT's underlying and
the curve it discounts on, by the sizes in BUMPS (see DEFAULT-GREEK-BUMPS).
Delta, gamma, vega, volga, rho and dividend rho use the stencil (-1 0 1);
vanna the product of two; theta the one-sided (0 1) in calendar days, by
aging the market. Prices are shared between Greeks. Engines with fixed
seeds (Monte Carlo) get common random numbers; LSM Greeks remain noisy,
because the fitted exercise boundary moves discontinuously with the bumps.

  (bump-greeks aput (make-instance 'barone-adesi-whaley-engine) m :which '(:delta))"
  (check-input market composite-market "a composite market, whose named factors can be bumped")
  (let* ((engine (resolve-designator instrument engine market))
         (bumps `(,@bumps ,@(default-greek-bumps engine)))
         (view (instrument-view market instrument))
         (underlying (view-underlying view))
         (curve (view-discount-curve view))
         (spot (spot view))
         (h-spot (getf bumps :spot)) (h-vol (getf bumps :vol))
         (h-rate (getf bumps :rate)) (h-dividend (getf bumps :dividend))
         (prices (make-hash-table :test 'equal)))
    (labels ((age (m days)
               (derive-market m :valuation-date (date+ (valuation-date m) days)))
             (price-at (&key (s 0) (v 0) (r 0) (q 0) (day 0))
               ;; S, V, R, Q and DAY count bump steps of each factor.
               (let ((key `(,s ,v ,r ,q ,day)))
                 (or (gethash key prices)
                     (setf (gethash key prices)
                           (nth-value 0 (price instrument engine
                                               (cond-> market
                                                 (/= s 0) (bump `(:spot ,underlying) `(:relative ,(* s h-spot)))
                                                 (/= v 0) (bump `(:vol ,underlying) `(:absolute ,(* v h-vol)))
                                                 (/= r 0) (bump `(:curve ,curve) `(:absolute ,(* r h-rate)))
                                                 (/= q 0) (bump `(:dividend ,underlying)
                                                                `(:absolute ,(* q h-dividend)))
                                                 (/= day 0) (age day))))))))
             (central (order h &rest factor)
               ;; FACTOR is the keyword of PRICE-AT that moves.
               (stencil-derivative '(-1 0 1) order h
                                   (lambda (k) (apply #'price-at `(,(first factor) ,k))))))
      (iter (for key in which)
            (appending `(,key ,(ecase key
                                 (:delta (central 1 (* h-spot spot) :s))
                                 (:gamma (central 2 (* h-spot spot) :s))
                                 (:vega (central 1 h-vol :v))
                                 (:volga (central 2 h-vol :v))
                                 (:rho (central 1 h-rate :r))
                                 (:dividend-rho (central 1 h-dividend :q))
                                 (:theta (stencil-derivative '(0 1) 1 1 (lambda (k) (price-at :day k))))
                                 (:vanna (stencil-derivative
                                          '(-1 0 1) 1 (* h-spot spot)
                                          (lambda (i)
                                            (stencil-derivative '(-1 0 1) 1 h-vol
                                                                (lambda (j) (price-at :s i :v j)))))))))))))

(defmethod greeks (instrument engine market &key (which *greek-keys*) bumps)
  (bump-greeks instrument engine market :which which :bumps bumps))

;;; --------------------------------------------------------------------
;;; Monte Carlo: independent batches for standard errors
;;; --------------------------------------------------------------------

(defmethod greeks (instrument (e monte-carlo-engine) market
                   &key (which *greek-keys*) bumps (batches 10))
  "Bump Greeks on BATCHES independent engines of N-PATHS / BATCHES paths each
(seeds derived from the engine's), averaged; the standard errors are those
of the batch means."
  (let* ((per-batch (ceiling (n-paths e) batches))
         (estimates (iter (for b below batches)
                          (collect (bump-greeks instrument
                                                (derive-engine e :n-paths per-batch
                                                                 :seed (+ (* 7919 (seed e)) b 1))
                                                market :which which :bumps bumps)))))
    (iter (for key in which)
          (for xs = (mapcar (lambda (estimate) (getf estimate key)) estimates))
          (for mean = (/ (reduce #'+ xs) batches))
          (for variance = (/ (iter (for x in xs) (sum (expt (- x mean) 2))) (1- batches)))
          (appending `(,key ,mean) into values)
          (appending `(,key ,(-> variance (/ batches) sqrt)) into errors)
          (finally (return `(,@values :standard-errors ,errors))))))

;;; --------------------------------------------------------------------
;;; Black-Scholes: closed form
;;; --------------------------------------------------------------------

(defmethod greeks ((o option) (e black-scholes-engine) market &key (which *greek-keys*) bumps)
  (if (european-vanilla-p o)
      (black-scholes-greeks o e market which)
      (call-next-method o e market :which which :bumps bumps)))

(defun black-scholes-greeks (o e market which)
  "Closed-form Greeks in terms of the forward F, discount factor D and total
variance w to expiry. Theta is the exact one-day change of the closed-form
price under aging."
  (let* ((m (pricing-market market o))
         (x (option-exercise o)) (p (option-payoff o))
         (phi (phi p)) (k (strike p)) (date (expiry x))
         (tau (time-to-expiry x m))
         (s (spot m)) (f (forward m date)) (d (discount-factor m date))
         (vol (black-vol m k date)) (w (black-variance m k date))
         (sd (sqrt w))
         (d1 (/ (+ (log (/ f k)) (* 0.5d0 w)) sd))
         (d2 (- d1 sd))
         (n1 (norm-pdf d1))
         (carry (/ (* d f) s))                  ; e^(-q tau)
         (vega (* d f n1 (sqrt tau))))
    (iter (for key in which)
          (appending `(,key ,(ecase key
                               (:delta (* carry phi (norm-cdf (* phi d1))))
                               (:gamma (/ (* carry n1) (* s sd)))
                               (:vega vega)
                               (:volga (/ (* vega d1 d2) vol))
                               (:vanna (/ (* (- carry) n1 d2) vol))
                               (:rho (* phi k tau d (norm-cdf (* phi d2))))
                               (:dividend-rho (* -1 phi tau d f (norm-cdf (* phi d1))))
                               (:theta (- (nth-value 0 (price o e (derive-market
                                                                   market :valuation-date
                                                                   (date+ (valuation-date market) 1))))
                                          (nth-value 0 (price o e market))))))))))

;;; --------------------------------------------------------------------
;;; Binomial: Greeks from the tree's own nodes
;;; --------------------------------------------------------------------

;;; A CRR tree's price oscillates in the vol: the strike's position between
;;; nodes moves with sigma, with a period of about sigma^2 sqrt(dt) /
;;; |ln(K/S)| (0.02 for K/S = 1.05, sigma = 0.2, N = 2000). First
;;; derivatives in sigma survive it; second ones do not (volga -1.55 against
;;; Black-Scholes -1.98 there), so vanna and volga are left out unless asked
;;; for.
(defmethod greeks ((o option) (e binomial-engine) market
                   &key (which '(:delta :gamma :vega :theta :rho :dividend-rho)) bumps)
  (let* ((node-keys '(:delta :gamma :theta))
         (from-nodes (and (intersection which node-keys)
                          (typep (option-payoff o) 'vanilla-payoff)
                          (typep (option-exercise o) '(or american-exercise bermudan-exercise))
                          (typep (option-path o) 'path-independent)
                          (typep (process e) 'gbm)))
         (nodes (and from-nodes (binomial-node-greeks o e market)))
         (bumped (let ((rest (if from-nodes (set-difference which node-keys) which)))
                   (and rest (bump-greeks o e market :which rest :bumps bumps)))))
    (iter (for key in which)
          (appending `(,key ,(getf (if (and from-nodes (member key node-keys)) nodes bumped) key))))))

(defun binomial-node-greeks (o e market)
  "Delta from the step-1 nodes, gamma from the step-2 nodes (spots S u, S d
and S u^2, S, S d^2: unevenly spaced, so the stencils use those offsets),
theta from the step-2 middle node (the same spot two steps later), per
calendar day. With the engine's extrapolation each is 2 g(N) - g(N/2), like
the price."
  (let* ((m (pricing-market market o))
         (x (option-exercise o)) (p (option-payoff o))
         (tau (time-to-expiry x m))
         (vol (gbm-vol (process e) m tau))
         (s (spot m))
         (day (one-day m)))
    (flet ((tree-greeks (n)
             (multiple-value-bind (v0 v-u v-d v-uu v-ud v-dd)
                 (crr-value (phi p) (strike p) m vol tau n
                            (exercise-mask x m (uniform-time-grid tau n)) (smooth e))
               (unless v-dd
                 (signal-invalid-argument 'n-steps (n-steps e) '(integer 8)
                                          "at least 8 steps, for Greeks from the tree's nodes"))
               (let* ((dt (/ tau n)) (u (exp (* vol (sqrt dt)))) (d (/ u)))
                 `(:delta ,(apply-stencil (finite-difference-weights `(,(- d 1) ,(- u 1)) 1)
                                          `(,v-d ,v-u) s 1)
                   :gamma ,(apply-stencil (finite-difference-weights
                                           `(,(- (* d d) 1) 0 ,(- (* u u) 1)) 2)
                                          `(,v-dd ,v-ud ,v-uu) s 2)
                   :theta ,(* day (apply-stencil (finite-difference-weights '(0 2) 1)
                                                 `(,v0 ,v-ud) dt 1)))))))
      (let ((full (tree-greeks (n-steps e))))
        (if (extrapolate e)
            (let ((half (tree-greeks (max 1 (floor (n-steps e) 2)))))
              (iter (for (key value) in-plist full)
                    (appending `(,key ,(nth-value 0 (richardson-extrapolate
                                                     (list (getf half key) value)
                                                     :exponents '(1)))))))
            full)))))
