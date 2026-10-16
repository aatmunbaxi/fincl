;;;; math/implied-normal.lisp --- Bachelier's normalised price and its inverse
;;;;
;;;; Peter Jäckel, "Implied Normal Volatility", 6 June 2017
;;;; (www.jaeckel.org/ImpliedNormalVolatility.pdf). An out-of-the-money
;;;; Bachelier price with x = -|F - K| / (sigma sqrt(T)) < 0 is
;;;;
;;;;   v = |F - K| (-Phi~(x)),   Phi~(x) := Phi(x) + phi(x) / x,
;;;;
;;;; so implying sigma is inverting the univariate Phi~ on x < 0. The inverse
;;;; is a rational guess in one of two branches plus one closed-form
;;;; third-order correction, accurate to about 1.2e-17 relative in x with no
;;;; iteration (the paper's figure 1).

(in-package #:fincl)

(defun phi-tilde (x)
  "Phi(X) + phi(X)/X for X < 0, without cancellation: Abramowitz and Stegun
26.2.12 summed from its second term below -10, and phi(X)/X (1 + X Y(X))
with Y = Phi/phi = sqrt(pi/2) erfcx(-X/sqrt 2) above.

  (phi-tilde -2.25d0) => -0.0018820392711...d0"
  (declare (double-float x))
  (if (<= x -10d0)
      ;; Phi(x) = phi(x)/|x| (1 - 1/x^2 + 3/x^4 - ...), and phi(x)/x cancels
      ;; the leading 1.
      (let* ((x2 (* x x)) (term (/ -1d0 x2)) (sum term))
        (declare (double-float x2 term sum))
        (iter (for k from 2 to 60)
              (for next = (* term (/ (- (* 2 k) 1) x2) -1d0))
              (while (and (< (abs next) (abs term)) (> (abs next) (* 1d-17 (abs sum)))))
              (setf term next) (incf sum next))
        (/ (* (lbr-norm-pdf x) sum) (- x)))
      (* (/ (lbr-norm-pdf x) x)
         (+ 1d0 (* x +sqrt-pi-over-two+ (cody-calerf (* (- +one-over-sqrt-two+) x) 2))))))

(defparameter *phi-tilde-branch-point* (phi-tilde -2.25d0)
  "Phi~(-9/4), about -0.001882039271: the boundary between the paper's two
initial-guess branches.")

(defun inverse-phi-tilde (target)
  "The X < 0 with (phi-tilde x) = TARGET < 0, by the paper's formulas
(2.1)-(2.7).

  (inverse-phi-tilde (phi-tilde -3d0)) => -3.0d0"
  (declare (double-float target))
  (let ((x-bar
          (if (< target *phi-tilde-branch-point*)
              ;; Far from the money: (2.1)-(2.3).
              (let* ((g (/ (- target 0.5d0)))
                     (g2 (* g g))
                     (xi-bar (/ (- 0.032114372355d0
                                   (* g2 (- 0.016969777977d0
                                            (* g2 (- 2.6207332461d-3 (* 9.6066952861d-5 g2))))))
                                (- 1d0 (* g2 (- 0.6635646938d0
                                                (* g2 (- 0.14528712196d0
                                                         (* 0.010472855461d0 g2)))))))))
                (* g (+ +one-over-sqrt-two-pi+ (* xi-bar g2))))
              ;; Near the money: (2.4)-(2.5).
              (let ((h (sqrt (- (log (- target))))))
                (/ (- 9.4883409779d0
                      (* h (- 9.6320903635d0 (* h (+ 0.58556997323d0 (* 2.1464093351d0 h))))))
                   (- 1d0 (* h (+ 0.65174820867d0
                                  (* h (+ 1.5120247828d0 (* 6.6437847132d-5 h)))))))))))
    (declare (double-float x-bar))
    ;; (2.6)-(2.7): one third-order correction.
    (let* ((q (/ (- (phi-tilde x-bar) target) (lbr-norm-pdf x-bar)))
           (x2 (* x-bar x-bar)))
      (declare (double-float q x2))
      (+ x-bar (/ (* 3 q x2 (- 2 (* q x-bar (+ 2 x2))))
                  (+ 6 (* q x-bar (+ -12 (* x-bar (+ (* 6 q)
                                                     (* x-bar (+ -6 (* q x-bar (+ 3 x2))))))))))))))

(defun normalised-bachelier (theta difference stddev)
  "The undiscounted Bachelier price of a call (THETA = 1) or put (THETA = -1)
with DIFFERENCE = F - K and STDDEV = sigma sqrt(T): the intrinsic value plus
|F - K| (-Phi~(-|F - K| / STDDEV)), accurate also far from the money."
  (declare (double-float theta difference stddev))
  (let ((intrinsic (max (* theta difference) 0d0))
        (distance (abs difference)))
    (cond ((<= stddev 0d0) intrinsic)
          ((zerop distance) (* stddev +one-over-sqrt-two-pi+))
          (t (+ intrinsic (* distance (- (phi-tilde (- (/ distance stddev))))))))))
