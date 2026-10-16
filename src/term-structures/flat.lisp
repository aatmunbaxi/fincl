;;;; term-structures/flat.lisp --- constant rates and vols
;;;;
;;;; The expressions are the ones the flat Black-Scholes market used (exp(-r
;;;; h), vol * vol * h), so prices on a flat composite market are bit-identical
;;;; to it.

(in-package #:fincl)

(defclass flat-yield-curve (yield-curve)
  ((rate :initarg :rate :reader flat-rate :type double-float
         :initform (a:required-argument :rate)))
  (:documentation "A constant continuously compounded rate.

  (make-instance 'flat-yield-curve :rate 0.04d0)"))

(defclass flat-dividend-curve (dividend-curve)
  ((rate :initarg :rate :reader flat-rate :type double-float :initform 0d0))
  (:documentation "A constant continuously compounded dividend yield.

  (make-instance 'flat-dividend-curve :rate 0.015d0)"))

(defclass flat-black-vol (black-vol-structure)
  ((vol :initarg :vol :reader flat-vol :type double-float
        :initform (a:required-argument :vol)))
  (:documentation "One Black vol for every strike and horizon.

  (make-instance 'flat-black-vol :vol 0.2d0)"))

(defmethod initialize-instance :after ((c flat-yield-curve) &key)
  (check-input (slot-value c 'rate) real "a real rate")
  (setf (slot-value c 'rate) (float (slot-value c 'rate) 1d0)))

(defmethod initialize-instance :after ((c flat-dividend-curve) &key)
  (check-input (slot-value c 'rate) real "a real dividend yield")
  (setf (slot-value c 'rate) (float (slot-value c 'rate) 1d0)))

(defmethod initialize-instance :after ((s flat-black-vol) &key)
  (check-input (slot-value s 'vol) real "a real vol")
  (setf (slot-value s 'vol) (float (slot-value s 'vol) 1d0)))

(defmethod curve-discount ((c flat-yield-curve) (horizon real))
  (exp (- (* (flat-rate c) horizon))))
(defmethod curve-zero-rate ((c flat-yield-curve) horizon &optional (compounding :continuous)
                                                           (frequency :annual) day-counter)
  (if (and (eq compounding :continuous) (null day-counter) (realp horizon))
      (flat-rate c)
      (call-next-method)))

(defmethod curve-dividend-discount ((c flat-dividend-curve) horizon)
  (exp (- (* (flat-rate c) horizon))))
(defmethod curve-dividend-yield ((c flat-dividend-curve) horizon)
  (declare (ignore horizon))
  (flat-rate c))

(defmethod structure-black-variance ((s flat-black-vol) strike horizon)
  (declare (ignore strike))
  (* (flat-vol s) (flat-vol s) horizon))
(defmethod structure-black-vol ((s flat-black-vol) strike horizon)
  (declare (ignore strike horizon))
  (flat-vol s))

(defmethod bump-structure ((c flat-yield-curve) shift)
  (make-instance 'flat-yield-curve :rate (apply-shift (flat-rate c) shift)))
(defmethod bump-structure ((c flat-dividend-curve) shift)
  (make-instance 'flat-dividend-curve :rate (apply-shift (flat-rate c) shift)))
(defmethod bump-structure ((s flat-black-vol) shift)
  (make-instance 'flat-black-vol :vol (apply-shift (flat-vol s) shift)))

(defmethod print-object ((c flat-yield-curve) stream)
  (print-unreadable-object (c stream :type t) (format stream "r=~,4F" (flat-rate c))))
(defmethod print-object ((c flat-dividend-curve) stream)
  (print-unreadable-object (c stream :type t) (format stream "q=~,4F" (flat-rate c))))
(defmethod print-object ((s flat-black-vol) stream)
  (print-unreadable-object (s stream :type t) (format stream "vol=~,4F" (flat-vol s))))
