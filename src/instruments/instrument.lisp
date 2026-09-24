;;;; instruments/instrument.lisp --- shared across asset classes
;;;;
;;;; Everything here is meant to be reused by bonds, swaps and anything else
;;;; added later. Option-specific components live in options.lisp.

(in-package #:fincl)

(defclass instrument ()
  ()
  (:documentation "A contract. Carries terms only: no market data, no model.
Instruments are immutable and reusable across scenarios."))

;;; --------------------------------------------------------------------
;;; Exercise (shared: callable bonds and Bermudan swaptions need this too)
;;; --------------------------------------------------------------------

(defclass exercise ()
  ((expiry :initarg :expiry :reader expiry :type double-float
           :initform (a:required-argument :expiry)
           :documentation "Year fraction from the valuation date.")))

(defclass european-exercise (exercise) ())
(defclass american-exercise (exercise) ())

(defgeneric exercise-allowed-p (exercise time)
  (:documentation "Can the holder exercise at TIME? One generic function is
the entire difference between European, American and Bermudan in a lattice
or LSM engine.")
  (:method ((x european-exercise) time) (declare (ignore time)) nil)
  (:method ((x american-exercise) time) (declare (ignore time)) t))

;;; --------------------------------------------------------------------
;;; Path dependence
;;; --------------------------------------------------------------------

(defclass path-feature () ())
(defclass path-independent (path-feature) ())
;; Barrier, Asian and lookback subclasses go here; engines dispatch on them.

;;; --------------------------------------------------------------------
;;; Payoff protocol
;;; --------------------------------------------------------------------

(defclass payoff ()
  ((kind :initarg :kind :reader kind :type (member :call :put)
         :initform (a:required-argument :kind))))

(defgeneric payoff-value (payoff x)
  (:documentation "Dispatching payoff. Convenient; too slow to call per path."))

(defgeneric payoff-kernel (payoff)
  (:documentation "Dispatch once, return a typed (double-float ->
double-float) closure. Engines call this before entering their loops."))
