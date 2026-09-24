;;;; math.lisp --- normal distribution and rootfinding

(in-package #:fincl)

(defconstant +sqrt2+ (sqrt 2d0))

(declaim (inline norm-cdf norm-pdf))

(defun norm-cdf (x)
  "Standard normal CDF."
  (declare (double-float x))
  (* 0.5d0 (sf:erfc (/ (- x) +sqrt2+))))

(defun norm-pdf (x)
  (declare (double-float x))
  (* 0.3989422804014327d0 (exp (* -0.5d0 x x))))

(defun norm-quantile (u)
  "Inverse standard normal CDF, for inverse-transform sampling."
  (declare (double-float u))
  (* +sqrt2+ (sf:inverse-erf (- (* 2d0 u) 1d0))))

;;; --------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------

(define-condition pricing-error (error) ())

(define-condition convergence-failure (pricing-error)
  ((context :initarg :context :reader failure-context))
  (:report (lambda (c s)
             (format s "Numerical procedure failed to converge: ~A"
                     (failure-context c)))))

;;; --------------------------------------------------------------------
;;; Rootfinding
;;; --------------------------------------------------------------------

(defun solve-root (f lo hi &key (expand-hi nil) (epsilon 1d-12) (delta 1d-12))
  "Bracketed root of F on [LO, HI] via NU:ROOT-BISECTION. If EXPAND-HI, grow
HI until the bracket straddles a root. Signals CONVERGENCE-FAILURE with
USE-VALUE and RETRY-WITH-BRACKET restarts, so the caller picks the recovery
policy instead of the numerics deciding for it."
  (let ((flo (funcall f lo))
        (fhi (funcall f hi)))
    (when expand-hi
      (loop repeat 60
            while (plusp (* flo fhi))
            do (setf hi (* hi 2d0)
                     fhi (funcall f hi))))
    (if (plusp (* flo fhi))
        (restart-case
            (error 'convergence-failure
                   :context (format nil "no sign change on [~,6F, ~,6F]" lo hi))
          (use-value (v)
            :report "Supply a value to use instead."
            v)
          (retry-with-bracket (new-lo new-hi)
            :report "Retry with a different bracket."
            (solve-root f new-lo new-hi :epsilon epsilon :delta delta)))
        (nu:root-bisection f (nu:interval lo hi) :epsilon epsilon :delta delta))))
