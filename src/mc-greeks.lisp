;;;; mc-greeks.lisp --- pathwise and likelihood-ratio Monte Carlo Greeks
;;;;
;;;; (greeks option mc-engine market :method :pathwise) differentiates each
;;;; simulated path's value: delta from the path scaled by (1 +/- h), vega
;;;; from the path moved along its vol tangent d ln S_t / d sigma, the payoff
;;;; evaluated through its PATH-KERNEL on the moved states (central
;;;; differences with h = 1e-6 on the same path, exact for piecewise-linear
;;;; payoffs except within h of a kink). It needs a payoff that is Lipschitz
;;;; in the path (PATHWISE-DIFFERENTIABLE-P). :likelihood-ratio weights each
;;;; path's value by the score of the spot in the path density, which needs
;;;; no smoothness (digitals, barriers): the transition to the first grid
;;;; point, or to expiry for payoffs that see only the terminal observable.
;;;; Both are for GBM.

(in-package #:fincl)

(defgeneric pathwise-differentiable-p (payoff path-feature)
  (:documentation "True if the value of PAYOFF under PATH-FEATURE is a
Lipschitz function of the path, so pathwise Greeks are unbiased: vanilla
payoffs, European or on an Asian average or a lookback extremum.")
  (:method (payoff path-feature) (declare (ignore payoff path-feature)) nil)
  (:method ((p vanilla-payoff) (f path-independent)) t)
  (:method ((p vanilla-payoff) (f asian)) t)
  (:method ((p vanilla-payoff) (f lookback)) t))

(defun vol-tangent-variances (process market times)
  "For each grid interval, the derivative of its log-variance w(t1) - w(t0)
with respect to a parallel shift of the vol: 2 sigma dt for a constant vol,
else of the ATM-forward Black variance, w(t) = sigma_B(t)^2 t, so 2 (sqrt(w(t1) t1)
- sqrt(w(t0) t0))."
  (let ((vol (process-vol process)))
    (iter (with n = (1- (length times)))
          (for (t0 t1) in-intervals times)
          (collect-double (if vol
                              (* 2d0 vol (- t1 t0))
                              (flet ((root (time) (sqrt (* (atm-forward-variance market time) time))))
                                (* 2d0 (- (root t1) (root t0)))))
                          into v length n)
          (finally (return v)))))

(defun estimator-greeks (o e market method which)
  "Pathwise or likelihood-ratio Greeks of option O under the Monte Carlo
engine E (GBM): a plist over WHICH with :STANDARD-ERRORS."
  (let ((allowed (rest (assoc method *estimator-greeks*))))
    (unless allowed
      (signal-invalid-argument 'method method '(member :bump :pathwise :likelihood-ratio)
                               ":BUMP, :PATHWISE or :LIKELIHOOD-RATIO"))
    (a:when-let ((other (set-difference which allowed)))
      (signal-invalid-argument 'which which 'list
                               (format nil "Greeks among ~{~S~^ ~} for ~S" allowed method))))
  (let* ((view (if (typep market 'composite-market) (instrument-view market o) market))
         (x (option-exercise o)) (p (option-payoff o)) (f (option-path o))
         (tau (time-to-expiry x view))
         (process (-> e process (resolve-process view tau))))
    (unless (and (typep x 'european-exercise) (typep process 'gbm))
      (error 'unsupported-combination :exercise x :payoff p :path f :process process
                                      :engine e :market view))
    (when (and (eq method :pathwise) (not (pathwise-differentiable-p p f)))
      (error 'model-domain-error
             :engine e
             :reason (format nil "pathwise Greeks need a payoff Lipschitz in the path, which ~
~(~A~) under ~(~A~) is not (PATHWISE-DIFFERENTIABLE-P); use :LIKELIHOOD-RATIO."
                             (class-name (class-of p)) (class-name (class-of f)))))
    (let* ((grid (if (and (eq method :likelihood-ratio)
                          (equal (payoff-requirements p f) '(:terminal)))
                     ;; One step to expiry: the score of the terminal value.
                     (make-time-grid view :dates `(,(expiry x)))
                     (make-time-grid view :dates (instrument-grid-dates x p f) :n-steps (n-steps e))))
           (times (time-grid-times grid))
           (n (1- (length times)))
           (stepper (make-stepper process view times))
           (s0 (float (spot view) 1d0))
           (dw (iter (for (t0 t1) in-intervals times)
                     (collect-double (gbm-forward-variance process view t0 t1) into v length n)
                     (finally (return v))))
           (dw-prime (vol-tangent-variances process view times))
           (expiry-discount (discount-factor view tau))
           (payments (path-payment-dates p f))
           (pay-discounts (map 'double-vector (lambda (d) (/ (discount-factor view d) expiry-discount))
                               (if (eq payments :grid) times payments)))
           (h 1d-6)
           ;; Variants: (scale-kind sign); the state each kernel sees is
           ;; S (1 + sign h) for :spot, S exp(sign h g) for :vol, S for :base.
           (variants (if (eq method :pathwise)
                         '((:spot 1d0) (:spot -1d0) (:vol 1d0) (:vol -1d0))
                         '((:base 0d0))))
           (n-variants (length variants))
           (anti (antithetic e))
           (n-base (if anti (/ (total-paths e) 2) (total-paths e)))
           (keys (if (eq method :pathwise) '(:delta :vega) '(:delta)))
           (samples (map-into (make-array (length keys))
                              (lambda () (make-array n-base :element-type 'double-float))))
           (plan (make-sampling-plan e times 1 n-base))
           (size (path-accumulator-size p f)))
      (declare (type double-vector times dw dw-prime pay-discounts) (fixnum n n-base))
      (multiple-value-bind (init step final) (let ((*barrier-bridge* (barrier-bridge e)))
                                               (path-kernel p f process grid))
        (declare (type function init step final stepper))
        (flet ((run-chunk (range index)
                 (multiple-value-bind (g start-path) (make-normal-filler e index plan)
                   (declare (type function start-path))
                   (let ((path-normals (make-array n :element-type 'double-float))
                         (normal (make-array 1 :element-type 'double-float))
                         (state (make-array 1 :element-type 'double-float))
                         (moved (iter (repeat n-variants)
                                      (collect (make-array 1 :element-type 'double-float))))
                         (accs (iter (repeat n-variants)
                                     (collect (make-array size :element-type 'double-float))))
                         (sums (make-array n-variants :element-type 'double-float))
                         (alive (make-array n-variants :initial-element t)))
                     (labels ((pay-for (v)
                                (lambda (amount k)
                                  (declare (double-float amount) (fixnum k))
                                  (incf (aref sums v) (* amount (aref pay-discounts k)))))
                              (step-variants (j tangent)
                                (iter (for (kind sign) in variants)
                                      (for v from 0)
                                      (for acc in accs)
                                      (for vstate in moved)
                                      (when (svref alive v)
                                        (setf (aref vstate 0)
                                              (ecase kind
                                                (:spot (* (aref state 0) (+ 1d0 (* sign h))))
                                                (:vol (* (aref state 0) (exp (* sign h tangent))))
                                                (:base (aref state 0))))
                                        (unless (funcall step acc j vstate (pay-for v))
                                          (setf (svref alive v) nil)))))
                              (side (sign)
                                ;; The estimates of one path with normals SIGN x PATH-NORMALS.
                                (let ((tangent 0d0) (score 0d0))
                                  (setf (aref state 0) s0)
                                  (fill sums 0d0)
                                  (fill alive t)
                                  (mapc init accs)
                                  (step-variants 0 tangent)
                                  (dotimes (k n)
                                    (let ((z (* sign (aref path-normals k))))
                                      (setf (aref normal 0) z)
                                      (funcall stepper state normal k)
                                      (when (plusp (aref dw k))
                                        (incf tangent (* 0.5d0 (aref dw-prime k)
                                                         (- (/ z (sqrt (aref dw k))) 1d0))))
                                      (when (zerop k)
                                        (setf score (/ z (* s0 (sqrt (aref dw 0))))))
                                      (step-variants (1+ k) tangent)))
                                  (let ((values (iter (for v below n-variants)
                                                      (for acc in accs)
                                                      (for vstate in moved)
                                                      (collect (+ (aref sums v)
                                                                  (if (svref alive v)
                                                                      (funcall final acc vstate)
                                                                      0d0))))))
                                    (if (eq method :pathwise)
                                        (destructuring-bind (up down vol-up vol-down) values
                                          `(,(/ (- up down) (* 2d0 h s0))
                                            ,(/ (- vol-up vol-down) (* 2d0 h))))
                                        `(,(* (first values) score)))))))
                       (iter (for i from (car range) below (cdr range))
                             (funcall start-path i)
                             (dotimes (k n)
                               (fill-normals g normal)
                               (setf (aref path-normals k) (aref normal 0)))
                             (let ((base (side 1d0))
                                   (partner (and anti (side -1d0))))
                               (iter (for b in base)
                                     (for k from 0)
                                     (setf (aref (the double-vector (svref samples k)) i)
                                           (if anti (* 0.5d0 (+ b (nth k partner))) b))))))))))
          (with-pricing-kernel ()
            (lp:pmapc #'run-chunk (chunk-ranges n-base (n-chunks e))
                      (a:iota (length (chunk-ranges n-base (n-chunks e))))))
          (iter (for key in which)
                (multiple-value-bind (value se)
                    (summarize-samples e (svref samples (position key keys))
                                       :discount expiry-discount)
                  (appending `(,key ,value) into values)
                  (appending `(,key ,se) into errors))
                (finally (return `(,@values :standard-errors ,errors)))))))))
