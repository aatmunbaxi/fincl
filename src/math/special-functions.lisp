;;;; math/special-functions.lisp --- the normal distribution

(in-package #:fincl)

(defconstant +sqrt2+ (sqrt 2d0))

(declaim (inline norm-cdf norm-pdf))

(defun norm-cdf (x)
  "Standard normal CDF.

  (norm-cdf 1.96d0) => 0.9750021048517795d0"
  (declare (double-float x))
  (* 0.5d0 (sf:erfc (/ (- x) +sqrt2+))))

(defun norm-pdf (x)
  "Standard normal density.

  (norm-pdf 0d0) => 0.3989422804014327d0"
  (declare (double-float x))
  (* 0.3989422804014327d0 (exp (* -0.5d0 x x))))

(defun norm-quantile (u)
  "Inverse standard normal CDF, for inverse-transform sampling.

  (norm-quantile 0.975d0) => 1.959963984540054d0"
  (declare (double-float u))
  (* +sqrt2+ (sf:inverse-erf (- (* 2d0 u) 1d0))))
