;;;; processes/cev.lisp --- constant elasticity of variance
;;;;
;;;;   dS = (r - q) S dt + sigma S^beta dW
;;;;
;;;; beta = 1 is GBM; beta < 1 gives the leverage effect (local vol rises as
;;;; the spot falls), which produces a negatively skewed implied vol smile
;;;; with one parameter. beta = 0 is the (Gaussian) Bachelier model.
;;;;
;;;; For beta < 1 the process can reach zero in finite time, where it is
;;;; absorbed (bankruptcy); CEV-ENGINE prices Europeans in closed form under
;;;; that absorbing boundary. Two path schemes, chosen by the SCHEME slot:
;;;;
;;;; :LOG-EULER (the default) steps ln S with the local vol sigma S^(beta-1),
;;;; so beta = 1 reproduces GBM exactly and S stays positive. It cannot reach
;;;; zero, which biases deep out-of-the-money puts for beta < 1.
;;;;
;;;; :ABSORBING-EULER steps S itself, S + drift + sigma S^beta sqrt(dt) Z,
;;;; and absorbs a path at 0 the first time a step lands at or below 0.

(in-package #:fincl)

(defun cev-default-sigma (process market horizon)
  "sigma_ATM(T) S0^(1 - beta), with sigma_ATM(T) = sqrt(w(T)/T) from the
market's ATM-forward Black variance to HORIZON = T: the sigma whose
instantaneous vol at the spot matches the market's vol to expiry."
  (* (sqrt (/ (atm-forward-variance market horizon) horizon))
     (expt (spot market) (- 1d0 (beta process)))))

(define-process cev (stochastic-process) ()
  ((beta 1d0 :bounds (0 nil) :doc "Elasticity: 1 is GBM, 0 is Bachelier.")
   (sigma nil :bounds ((0) nil) :market-default cev-default-sigma
              :doc "Scale parameter; its units change with beta. NIL resolves to
sigma_ATM(T) S0^(1 - beta), T the expiry, sigma_ATM(T) the market's
ATM-forward Black vol to T.")
   (scheme :log-euler :parameter nil :type (member :log-euler :absorbing-euler)
           :doc "Path discretization: :LOG-EULER (positive, never absorbed) or
:ABSORBING-EULER (Euler on S, absorbed at 0)."))
  (:documentation "Constant-elasticity-of-variance dynamics."))

(defmethod make-stepper ((p cev) market times)
  (declare (type (simple-array double-float (*)) times))
  (let* ((p (resolve-process p market (aref times (1- (length times)))))
         (beta (beta p))
         (sigma (sigma p))
         (exponent (- beta 1d0))
         (dt (interval-lengths times))
         (sqrt-dt (map '(simple-array double-float (*)) #'sqrt dt))
         (mu (interval-log-growths market times)))
    (declare (double-float beta sigma exponent)
             (type (simple-array double-float (*)) dt sqrt-dt mu))
    (if (eq (scheme p) :absorbing-euler)
        (let ((growth (map '(simple-array double-float (*)) #'exp mu)))
          (declare (type (simple-array double-float (*)) growth))
          ;; The drift is the exact forward growth over the step, so with no
          ;; absorption E[S(t+dt) | S(t)] = S(t) F(t+dt)/F(t).
          (lambda (state normals j)
            (declare (type (simple-array double-float (*)) state normals)
                     (fixnum j)
                     (optimize (speed 3) (safety 0)))
            (let ((s (aref state 0)))
              (declare (double-float s))
              (when (plusp s)
                (let ((next (+ (* s (aref growth j))
                               (* sigma (expt s beta) (aref sqrt-dt j) (aref normals 0)))))
                  (declare (double-float next))
                  (setf (aref state 0) (max 0d0 next)))))))
        (cev-log-euler-stepper sigma exponent sqrt-dt mu))))

(defun cev-log-euler-stepper (sigma exponent sqrt-dt mu)
  (declare (double-float sigma exponent)
           (type (simple-array double-float (*)) sqrt-dt mu))
  (lambda (state normals j)
    (declare (type (simple-array double-float (*)) state normals)
             (fixnum j)
             (optimize (speed 3) (safety 0)))
    (let ((s (aref state 0)))
      (declare (double-float s))
      ;; A path that underflowed to 0 stays there (S^(beta-1) is infinite).
      (when (plusp s)
        (let* (;; Local volatility sigma * S^(beta-1), and its step deviation.
               (local-vol (* sigma (expt s exponent)))
               (deviation (* local-vol (aref sqrt-dt j))))
          (declare (double-float local-vol deviation))
          (setf (aref state 0)
                (if (> deviation 1d10)
                    ;; exp(-deviation^2 / 2 + ...) underflows: the step lands at 0.
                    0d0
                    (* s (exp (+ (- (aref mu j) (* 0.5d0 deviation deviation))
                                 (* deviation (aref normals 0))))))))))))
