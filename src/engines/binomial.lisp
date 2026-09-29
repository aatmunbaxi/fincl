;;;; engines/binomial.lisp --- Cox-Ross-Rubinstein binomial tree
;;;;
;;;; J. C. Cox, S. A. Ross and M. Rubinstein, "Option pricing: a simplified
;;;; approach", J. Financial Economics 7 (1979).
;;;;
;;;; Over each step dt the spot moves up by u = exp(sigma sqrt(dt)) or down by
;;;; d = 1/u. The up probability p = (g - d) / (u - d) matches the step's
;;;; growth factor g, and values are discounted by the step's discount
;;;; factor. Both come from the market protocol (ratios of FORWARD-FACTOR and
;;;; DISCOUNT-FACTOR between grid times), so a market with term structure
;;;; changes p and discounting step by step while the tree still recombines.
;;;;
;;;; A plain CRR tree converges at O(1/N) but oscillates between odd and even
;;;; N. By default the engine values the last step with the Black-Scholes
;;;; formula (the BBS tree of Broadie and Detemple, 1996), which removes the
;;;; oscillation, and returns 2 v(N) - v(N/2) to cancel the O(1/N) term
;;;; (their BBSR). Europeans under GBM are left to the
;;;; Black-Scholes engine, which is exact.

(in-package #:fincl)

(define-condition invalid-tree (pricing-error)
  ((probability :initarg :probability :reader tree-probability)
   (n-steps :initarg :n-steps :reader tree-n-steps))
  (:report (lambda (c s)
             (format s "Binomial up probability ~,6F is outside (0, 1) with ~D ~
steps: the drift per step exceeds the volatility move. Increase :n-steps."
                     (tree-probability c) (tree-n-steps c)))))

(defun crr-value (phi strike market vol tau n-steps mask smooth)
  "Value at the root of a CRR tree of N-STEPS steps over TAU (market time)
for payoff max(PHI (S - STRIKE), 0). MASK is a bit vector of length
N-STEPS + 1 flagging the exercise steps (see EXERCISE-MASK); the last step
always pays the payoff. With SMOOTH, the nodes one step before expiry hold
the Black-Scholes value over that step instead of the binomial one."
  (declare (double-float phi strike vol tau) (fixnum n-steps)
           (type simple-bit-vector mask))
  (let* ((spot (spot market))
         (dt (/ tau n-steps))
         (u (exp (* vol (sqrt dt))))
         (d (/ u))
         (probabilities (make-array n-steps :element-type 'double-float))
         (discounts (make-array n-steps :element-type 'double-float))
         ;; u^j for j = -n .. n; the spot at node (step, i) is S u^(step - 2i).
         (powers (make-array (1+ (* 2 n-steps)) :element-type 'double-float))
         (values (make-array (1+ n-steps) :element-type 'double-float)))
    (declare (double-float spot dt u d)
             (type (simple-array double-float (*)) probabilities discounts powers values))
    (dotimes (j n-steps)
      (let* ((t0 (* j dt))
             (t1 (* (1+ j) dt))
             (growth (/ (forward-factor market t1) (forward-factor market t0)))
             (p (/ (- growth d) (- u d))))
        (unless (< 0d0 p 1d0)
          (error 'invalid-tree :probability p :n-steps n-steps))
        (setf (aref probabilities j) p
              (aref discounts j) (/ (discount-factor market t1)
                                    (discount-factor market t0)))))
    (dotimes (j (length powers))
      (setf (aref powers j) (expt u (- j n-steps))))
    (flet ((intrinsic (step i)
             (declare (fixnum step i))
             (max 0d0 (* phi (- (* spot (aref powers (+ n-steps (- step (* 2 i))))) strike))))
           (exercise-p (step)
             (= 1 (sbit mask step))))
      (declare (inline intrinsic))
      (if smooth
          ;; BBS: the last step in closed form, with that step's forward,
          ;; discount factor and variance.
          (let* ((penultimate (1- n-steps))
                 (growth (/ (forward-factor market tau)
                            (forward-factor market (* penultimate dt))))
                 (variance (* vol vol dt))
                 (early (exercise-p penultimate)))
            (dotimes (i (1+ penultimate))
              (let* ((node (* spot (aref powers (+ n-steps (- penultimate (* 2 i))))))
                     (hold (black-formula phi (* node growth) strike
                                          (aref discounts penultimate) variance)))
                (setf (aref values i)
                      (if early (max hold (intrinsic penultimate i)) hold)))))
          (dotimes (i (1+ n-steps))
            (setf (aref values i) (intrinsic n-steps i))))
      (loop for step fixnum from (if smooth (- n-steps 2) (1- n-steps)) downto 0
            for p double-float = (aref probabilities step)
            for disc double-float = (aref discounts step)
            for early = (exercise-p step)
            do (locally (declare (optimize (speed 3) (safety 0)))
                 (dotimes (i (1+ step))
                   (let ((hold (* disc (+ (* p (aref values i))
                                          (* (- 1d0 p) (aref values (1+ i)))))))
                     (setf (aref values i)
                           (if early (max hold (intrinsic step i)) hold))))))
      (aref values 0))))

;;; A lattice needs only a mask per tree: EXERCISE-MASK is the entire
;;; difference between American and Bermudan here.
(defun binomial-price (engine market exercise phi strike vol tau)
  "Tree value for EXERCISE, extrapolated if ENGINE says so. Each tree, N and
N/2 steps, gets the mask for its own grid."
  (flet ((tree (n)
           (crr-value phi strike market vol tau n
                      (exercise-mask exercise market (uniform-time-grid tau n))
                      (smooth engine))))
    (let ((n (n-steps engine)))
      (if (extrapolate engine)
          (nth-value 0 (richardson-extrapolate (list (tree (max 1 (floor n 2))) (tree n))
                                               :exponents '(1)))
          (tree n)))))

;;; --------------------------------------------------------------------
;;; Early exercise under GBM
;;; --------------------------------------------------------------------

(defun binomial-early-exercise (x p process e market)
  (let ((tau (time-to-expiry x market)))
    (values (binomial-price e market x (phi p) (strike p)
                            (gbm-vol process market tau) tau)
            nil)))

(defmethod price-binomial ((x american-exercise) (p vanilla-payoff) (f path-independent)
                           (process gbm) (e binomial-engine) market)
  (binomial-early-exercise x p process e market))

;;; Exercise dates rarely fall on grid times, so each is moved to the
;;; nearest step: an O(dt) shift. A grid whose step divides the gaps
;;; between dates (e.g. N a multiple of the days to expiry) removes it.
(defmethod price-binomial ((x bermudan-exercise) (p vanilla-payoff) (f path-independent)
                           (process gbm) (e binomial-engine) market)
  (binomial-early-exercise x p process e market))
