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
;;;;
;;;; Discrete dividends (task 21). The tree grows at the CARRY-FACTOR, so it
;;;; stays recombining, and each dividend is applied at the step nearest its
;;;; ex-date. Under the spot model the value just before the ex-date at a
;;;; node with spot S is the value after it at S - D, interpolated (natural
;;;; cubic in S) across that step's nodes: B. Vellekoop and J. Nieuwenhuis,
;;;; "Efficient pricing of derivatives on assets with discrete dividends",
;;;; Applied Mathematical Finance 13 (2006). Exercise is then checked on S,
;;;; before the dividend. Under the escrowed model the tree carries S - R(t),
;;;; R the dividends still to go ex, and exercise pays on S.

(in-package #:fincl)

(define-condition invalid-tree (pricing-error)
  ((probability :initarg :probability :reader tree-probability)
   (n-steps :initarg :n-steps :reader tree-n-steps))
  (:report (lambda (c s)
             (format s "Binomial up probability ~,6F is outside (0, 1) with ~D ~
steps: the drift per step exceeds the volatility move. Increase :n-steps, or ~
invoke INCREASE-STEPS."
                     (tree-probability c) (tree-n-steps c)))))

(defun dividend-interface (values nodes step jumps)
  "Replace VALUES[0..STEP], the values just after the dividends JUMPS at the
nodes NODES[0..STEP] (descending), by the values just before them: the value
at the price each node drops to, by a natural cubic spline in the price,
linear beyond the nodes."
  (declare (type double-vector values nodes) (fixnum step))
  (let* ((xs (make-array (1+ step) :element-type 'double-float))
         (ys (make-array (1+ step) :element-type 'double-float)))
    (dotimes (j (1+ step))
      (setf (aref xs j) (aref nodes (- step j))
            (aref ys j) (aref values (- step j))))
    (let ((after (make-interpolator :natural-cubic xs ys :extrapolation :linear)))
      (dotimes (i (1+ step) values)
        (setf (aref values i)
              (float (funcall after (apply-dividend-jumps (aref nodes i) jumps)) 1d0))))))

(defun dividend-map (jumps)
  "(values alpha beta) with S -> alpha S - beta the effect of JUMPS, the
floor at 0 aside."
  (let ((alpha 1d0) (beta 0d0))
    (dolist (jump jumps (values alpha beta))
      (if (eq (car jump) :proportional)
          (let ((keep (- 1d0 (cdr jump))))
            (setf alpha (* alpha keep) beta (* beta keep)))
          (incf beta (cdr jump))))))

(defun tree-dividends (market process tau n-steps)
  "(values root jumps escrow) for a tree of N-STEPS steps over TAU, each
dividend placed on the step nearest its ex-time (at least 1): the tree's
price at the root (the spot, or under the escrowed model the spot less the
dividends to come); a simple-vector over steps 0..N-STEPS of the (kind .
amount) jumps applied at each step; and under the escrowed model a cons of
two vectors over the steps, the cash dividends still to come just after and
just before the step's own dividends (ESCROWED-DIVIDENDS), else NIL."
  (let* ((events (dividends-between (discrete-dividends market) 0d0 tau))
         (escrowed (and events (eq (dividend-model process) :escrowed)))
         (dt (/ tau n-steps)))
    (flet ((step-of (e) (max 1 (min n-steps (round (dividend-event-time e) dt)))))
      (if (null events)
          (values (spot market) nil nil)
          (let ((jumps (make-array (1+ n-steps) :initial-element nil))
                (escrow
                  (and escrowed
                       ;; A cash dividend worth V at 0 is worth V C(t) prod
                       ;; (1 - delta) at t, over the proportional ones by t.
                       (let ((terms (mapcar #'cdr (escrowed-dividend-terms market events 0d0 tau)))
                             (steps (mapcar #'step-of (remove :proportional events
                                                              :key #'dividend-event-kind)))
                             (after (make-array (1+ n-steps) :element-type 'double-float))
                             (before (make-array (1+ n-steps) :element-type 'double-float)))
                         (dotimes (k (1+ n-steps) (cons after before))
                           (let ((growth (* (carry-factor market (* k dt))
                                            (proportional-dividend-factor events 0d0 (* k dt)))))
                             (setf (aref after k)
                                   (* growth (iter (for v in terms) (for s in steps)
                                                   (when (> s k) (sum v))))
                                   (aref before k)
                                   (* growth (iter (for v in terms) (for s in steps)
                                                   (when (>= s k) (sum v)))))))))))
            (dolist (e events)
              (unless (and escrowed (eq (dividend-event-kind e) :cash))
                (let ((k (step-of e)))
                  (setf (svref jumps k)
                        (append (svref jumps k)
                                `((,(dividend-event-kind e) . ,(dividend-event-amount e))))))))
            (values (if escrowed (- (spot market) (aref (car escrow) 0)) (spot market))
                    (and (some #'identity jumps) jumps)
                    escrow))))))

(defun crr-value (phi strike market vol tau n-steps mask smooth &key root jumps escrow)
  "Value at the root of a CRR tree of N-STEPS steps over TAU (market time)
for payoff max(PHI (S - STRIKE), 0). MASK is a bit vector of length
N-STEPS + 1 flagging the exercise steps (see EXERCISE-MASK); the last step
always pays the payoff. With SMOOTH, the nodes one step before expiry hold
the Black-Scholes value over that step instead of the binomial one. ROOT
(default the spot), JUMPS and ESCROW come from TREE-DIVIDENDS.

Further values, for node Greeks: the values at step 1 (spots S u, S d) and
at step 2 (S u^2, S, S d^2), as (values root v-u v-d v-uu v-ud v-dd), with
NIL for steps the backward sweep does not compute (N-STEPS < 4 with SMOOTH,
< 3 without)."
  (declare (double-float phi strike vol tau) (fixnum n-steps)
           (type simple-bit-vector mask))
  (let* ((spot (or root (spot market)))
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
             (growth (/ (carry-factor market t1) (carry-factor market t0)))
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
             (let ((node (* spot (aref powers (+ n-steps (- step (* 2 i)))))))
               (if escrow
                   ;; Exercise just after or just before the step's dividends.
                   (max 0d0
                        (* phi (- (+ node (aref (the double-vector (car escrow)) step)) strike))
                        (* phi (- (+ node (aref (the double-vector (cdr escrow)) step)) strike)))
                   (max 0d0 (* phi (- node strike))))))
           (exercise-p (step)
             (= 1 (sbit mask step)))
           (jumps-at (step)
             (and jumps (svref jumps step)))
           (nodes (step)
             (let ((v (make-array (1+ step) :element-type 'double-float)))
               (dotimes (i (1+ step) v)
                 (setf (aref v i) (* spot (aref powers (+ n-steps (- step (* 2 i))))))))))
      (declare (inline intrinsic))
      (if smooth
          ;; BBS: the last step in closed form, with that step's forward,
          ;; discount factor and variance; dividends at expiry, S -> alpha S
          ;; - beta, shift the strike.
          (multiple-value-bind (alpha beta) (dividend-map (jumps-at n-steps))
            (let* ((penultimate (1- n-steps))
                   (growth (/ (carry-factor market tau)
                              (carry-factor market (* penultimate dt))))
                   (variance (* vol vol dt))
                   (early (exercise-p penultimate)))
              (dotimes (i (1+ penultimate))
                (let* ((node (* spot (aref powers (+ n-steps (- penultimate (* 2 i))))))
                       (hold (if (jumps-at n-steps)
                                 (* alpha (black-formula phi (* node growth) (/ (+ strike beta) alpha)
                                                         (aref discounts penultimate) variance))
                                 (black-formula phi (* node growth) strike
                                                (aref discounts penultimate) variance))))
                  (setf (aref values i)
                        (if early (max hold (intrinsic penultimate i)) hold))))))
          (dotimes (i (1+ n-steps))
            (setf (aref values i)
                  (if (jumps-at n-steps)
                      (max 0d0 (* phi (- (apply-dividend-jumps
                                          (* spot (aref powers (+ n-steps (- n-steps (* 2 i)))))
                                          (jumps-at n-steps))
                                         strike)))
                      (intrinsic n-steps i)))))
      (let ((v-u nil) (v-d nil) (v-uu nil) (v-ud nil) (v-dd nil))
        (iter (declare (declare-variables))
              (for (the fixnum step) from (if smooth (- n-steps 2) (1- n-steps)) downto 0)
              (for (the double-float p) = (aref probabilities step))
              (for (the double-float disc) = (aref discounts step))
              (for early = (exercise-p step))
              (if (jumps-at step)
                  ;; Roll back and exercise just after the dividend, cross it,
                  ;; then exercise just before it.
                  (progn
                    (dotimes (i (1+ step))
                      (setf (aref values i) (* disc (+ (* p (aref values i))
                                                       (* (- 1d0 p) (aref values (1+ i)))))))
                    (when early
                      (dotimes (i (1+ step))
                        (setf (aref values i) (max (aref values i) (intrinsic step i)))))
                    (dividend-interface values (nodes step) step (jumps-at step))
                    (when early
                      (dotimes (i (1+ step))
                        (setf (aref values i) (max (aref values i) (intrinsic step i))))))
                  (locally (declare (optimize (speed 3) (safety 0)))
                    (dotimes (i (1+ step))
                      (let ((hold (* disc (+ (* p (aref values i))
                                             (* (- 1d0 p) (aref values (1+ i)))))))
                        (setf (aref values i)
                              (if early (max hold (intrinsic step i)) hold))))))
              (case step
                (2 (setf v-uu (aref values 0) v-ud (aref values 1) v-dd (aref values 2)))
                (1 (setf v-u (aref values 0) v-d (aref values 1)))))
        (values (aref values 0) v-u v-d v-uu v-ud v-dd)))))

;;; A lattice needs only a mask per tree: EXERCISE-MASK is the entire
;;; difference between American and Bermudan here.
(defun binomial-price (engine market exercise phi strike vol tau)
  "Tree value for EXERCISE, extrapolated if ENGINE says so. Each tree, N and
N/2 steps, gets the mask and the dividend steps of its own grid."
  (flet ((tree (n)
           (multiple-value-bind (root jumps escrow) (tree-dividends market (process engine) tau n)
             (crr-value phi strike market vol tau n
                        (exercise-mask exercise market (uniform-time-grid tau n))
                        (smooth engine)
                        :root root :jumps jumps :escrow escrow))))
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

(defmethod check-discrete-dividends ((e binomial-engine) o market)
  (declare (ignore o market))
  nil)

(defmethod engine-candidates append ((designator (eql :binomial)) (x american-exercise) (p vanilla-payoff)
                                     (f path-independent) (process gbm))
  '((10 :binomial)))

(defmethod engine-candidates append ((designator (eql :binomial)) (x bermudan-exercise) (p vanilla-payoff)
                                     (f path-independent) (process gbm))
  '((10 :binomial)))
