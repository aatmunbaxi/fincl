;;;; processes/cev.lisp --- constant elasticity of variance
;;;;
;;;;   dS = (r - q) S dt + sigma S^beta dW
;;;;
;;;; beta = 1 is GBM; beta < 1 gives the leverage effect (local vol rises as
;;;; the spot falls), which produces a negatively skewed implied vol smile
;;;; with one parameter. beta = 0 is the (Gaussian) Bachelier model.
;;;;
;;;; Stepped in log space, so beta = 1 reproduces GBM exactly and S stays
;;;; positive. That last part is a modelling choice, not a free lunch: for
;;;; beta < 1 the true CEV process can reach zero in finite time (absorption
;;;; at bankruptcy), and log-Euler cannot represent that. Use an absorbing
;;;; Euler scheme or the exact non-central chi-squared sampler if the zero
;;;; boundary matters for the product being priced.

(in-package #:fincl)

(defun cev-default-sigma (process market horizon)
  "The sigma whose instantaneous vol at the spot is the market's Black vol:
vol * S0^(1 - beta)."
  (* (black-vol market (spot market) horizon)
     (expt (spot market) (- 1d0 (beta process)))))

(define-process cev (stochastic-process) ()
  ((beta 1d0 :bounds (0 nil) :doc "Elasticity: 1 is GBM, 0 is Bachelier.")
   (sigma nil :bounds ((0) nil) :market-default cev-default-sigma
              :doc "Scale parameter. NIL calibrates it to the market's Black
vol at the spot. Its units change with beta."))
  (:documentation "Constant-elasticity-of-variance dynamics."))

(defun cev-sigma (process market time)
  (or (sigma process) (cev-default-sigma process market time)))

(defmethod make-stepper ((p cev) market dt)
  (let* ((beta (beta p))
         (sigma (cev-sigma p market dt))
         (mu (* (- (zero-rate market dt) (dividend-yield market dt)) dt))
         (sqrt-dt (sqrt dt))
         (exponent (- beta 1d0)))
    (declare (double-float beta sigma mu sqrt-dt exponent))
    (lambda (state normals)
      (declare (type (simple-array double-float (*)) state normals)
               (optimize (speed 3) (safety 0)))
      (let* ((s (aref state 0))
             ;; Local volatility sigma * S^(beta-1).
             (local-vol (* sigma (expt s exponent))))
        (declare (double-float s local-vol))
        (setf (aref state 0)
              (* s (exp (+ (- mu (* 0.5d0 local-vol local-vol dt))
                           (* local-vol sqrt-dt (aref normals 0))))))))))
