;;;; markets.lisp --- test-only markets with term structure
;;;;
;;;; PIECEWISE-MARKET has piecewise-constant forward rates, dividend yields
;;;; and vols. It implements only the real-horizon protocol methods, as the
;;;; market protocol requires, so every engine that works on it goes through
;;;; ZERO-RATE, DIVIDEND-YIELD, BLACK-VARIANCE and the defaults built on them.
;;;; Engines that silently assumed flat markets give wrong answers on it.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defclass piecewise-market (market)
  ((valuation-date :initarg :valuation-date :reader valuation-date)
   (day-counter :initarg :day-counter :initform +actual-365-fixed+
                :reader market-day-counter)
   (spot :initarg :spot :reader piecewise-spot)
   (knots :initarg :knots :reader piecewise-knots
          :documentation "Increasing interval ends in market time; the last
interval extends to infinity.")
   (rates :initarg :rates :reader piecewise-rates)
   (dividends :initarg :dividends :reader piecewise-dividends)
   (vols :initarg :vols :reader piecewise-vols))
  (:documentation "Forward rate, dividend yield and vol constant on each of
[0, k1), [k1, k2), ..., [kn, inf). Each value list has one more element than
KNOTS."))

(defun make-piecewise-market (&key (valuation-date *today*) (spot 100d0)
                                   (knots '(0.25d0 0.5d0 1d0))
                                   (rates '(0.02d0 0.03d0 0.045d0 0.05d0))
                                   (dividends '(0.01d0 0.015d0 0.02d0 0.02d0))
                                   (vols '(0.30d0 0.25d0 0.20d0 0.18d0)))
  (assert (= (1+ (length knots)) (length rates) (length dividends) (length vols)))
  (make-instance 'piecewise-market :valuation-date valuation-date :spot (float spot 1d0)
                                   :knots knots :rates rates :dividends dividends :vols vols))

(defun integrate-piecewise (knots values horizon &optional (f #'identity))
  "Integral over [0, HORIZON] of (F value) on each interval."
  (iter (for value in values)
        (for lo first 0d0 then hi)
        (for hi in (append knots (list most-positive-double-float)))
        (sum (* (funcall f value) (max 0d0 (- (min horizon hi) lo))))))

(defmethod spot ((m piecewise-market)) (piecewise-spot m))

(defmethod zero-rate ((m piecewise-market) (horizon real))
  (if (zerop horizon)
      (first (piecewise-rates m))
      (/ (integrate-piecewise (piecewise-knots m) (piecewise-rates m) horizon) horizon)))

(defmethod dividend-yield ((m piecewise-market) (horizon real))
  (if (zerop horizon)
      (first (piecewise-dividends m))
      (/ (integrate-piecewise (piecewise-knots m) (piecewise-dividends m) horizon) horizon)))

(defmethod black-variance ((m piecewise-market) strike (horizon real))
  (declare (ignore strike))
  (integrate-piecewise (piecewise-knots m) (piecewise-vols m) horizon (lambda (v) (* v v))))

(defmethod black-vol ((m piecewise-market) strike (horizon real))
  (if (zerop horizon)
      (first (piecewise-vols m))
      (sqrt (/ (black-variance m strike horizon) horizon))))

(defclass inverted-variance-market (piecewise-market) ()
  (:documentation "Total variance that falls after 0.5 years: calendar
arbitrage, which processes must refuse."))

(defmethod black-variance ((m inverted-variance-market) strike (horizon real))
  (declare (ignore strike))
  (if (<= horizon 0.5d0) (* 0.04d0 horizon) (- 0.02d0 (* 0.01d0 (- horizon 0.5d0)))))
