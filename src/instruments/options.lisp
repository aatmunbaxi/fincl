;;;; instruments/options.lisp --- options as component bundles
;;;;
;;;; An option is payoff x exercise x path feature. Anything that only
;;;; changes a number inside a method (call vs. put) is a slot; anything that
;;;; changes which method applies (exercise style, path dependence) is a
;;;; class.

(in-package #:fincl)

(defclass call-put-payoff (payoff)
  ((kind :initarg :kind :reader kind :type (member :call :put)
         :initform (a:required-argument :kind) :fact :kind))
  (:metaclass component-class)
  (:documentation "A payoff that comes as a call or a put."))

(declaim (inline phi))
(defun phi (payoff)
  "+1 for a call, -1 for a put."
  (if (eq (kind payoff) :call) 1d0 -1d0))

;;; DEFINE-PAYOFF formulas for call/put payoffs may use PHI.
(declare-payoff-variables call-put-payoff (phi phi double-float))

(defclass strike-payoff (call-put-payoff)
  ((strike :initarg :strike :reader strike :type double-float
           :initform (a:required-argument :strike) :fact :strike))
  (:metaclass component-class)
  (:documentation "A call or put with a strike: the data a struck payoff
needs, with no formula. Build new struck payoffs on this, not on
VANILLA-PAYOFF: engines with closed forms for VANILLA-PAYOFF would apply them
to a subclass and return the vanilla price.

  (define-payoff digital (strike-payoff) () (s)
    (if (plusp (* phi (- s strike))) 1d0 0d0))"))

(defmethod initialize-instance :after ((p strike-payoff) &key)
  (check-input (slot-value p 'strike) real "a real strike")
  (setf (slot-value p 'strike) (float (slot-value p 'strike) 1d0)))

;;; DEFINE-PAYOFF formulas for struck payoffs may use PHI and STRIKE.
(declare-payoff-variables strike-payoff (phi phi double-float) (strike strike double-float))

(defmethod validate-payoff ((p strike-payoff) market expiry)
  (let ((vol (black-vol market (strike p) expiry)))
    (unless (plusp vol)
      (error 'non-positive-volatility
             :volatility vol
             :where (format nil "at strike ~,2F to ~A" (strike p) expiry)))))

(define-payoff vanilla-payoff (strike-payoff) ()
  (s)
  "max(phi (S - K), 0). Engines with closed forms specialize on this class.

  (make-instance 'vanilla-payoff :kind :put :strike 40d0)"
  (max 0d0 (* phi (- s strike))))


;;; --------------------------------------------------------------------
;;; The option
;;; --------------------------------------------------------------------

(defclass option (instrument)
  ((payoff :initarg :payoff :reader option-payoff :type payoff
           :initform (a:required-argument :payoff))
   (exercise :initarg :exercise :reader option-exercise :type exercise
             :initform (a:required-argument :exercise))
   (path :initarg :path :reader option-path :type path-feature
         :initform (make-instance 'path-independent)))
  (:metaclass component-class))

(defmethod initialize-instance :after ((o option) &key)
  ;; SBCL does not check slot :TYPEs in MAKE-INSTANCE at default safety.
  ;; Options are built rarely, so check them once here.
  (check-input (slot-value o 'payoff) payoff)
  (check-input (slot-value o 'exercise) exercise)
  (check-input (slot-value o 'path) path-feature))

;;; Options are built with the option construction language
;;; (option-language.lisp): (option (put 40) (american) (expires #D"...")).
