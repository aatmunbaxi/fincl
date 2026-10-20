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
  (check-input (slot-value o 'path) path-feature)
  (check-path-feature (slot-value o 'path) (slot-value o 'payoff) (slot-value o 'exercise)))

(defgeneric check-path-feature (path payoff exercise)
  (:documentation "Signal an INVALID-INPUT if PATH cannot be combined with
PAYOFF and EXERCISE. Called when an option is made; the default accepts.")
  (:method (path payoff exercise) (declare (ignore path payoff exercise)) nil))

(defgeneric validate-path-feature (path market)
  (:documentation "Signal a PRICING-ERROR if PATH cannot be priced on MARKET.
Called once per PRICE; the default accepts.")
  (:method (path market) (declare (ignore path market)) nil))

(defgeneric payoff-mandatory-dates (payoff)
  (:documentation "The dates a payoff with its own path logic (DEFINE-PATH-PAYOFF)
needs on a time grid.")
  (:method ((p payoff)) '()))

(defgeneric instrument-mandatory-dates (instrument)
  (:documentation "Every date a path simulation of INSTRUMENT must hit
exactly: exercise dates, the expiry, observation and payment dates. Sorted,
without duplicates.

  (instrument-mandatory-dates (make-option (put 100) (bermudan :dates ds))) => DS"))

(defmethod instrument-mandatory-dates ((o option))
  (-<> (append (exercise-mandatory-dates (option-exercise o))
               (path-mandatory-dates (option-path o))
               (payoff-mandatory-dates (option-payoff o)))
       (sort <> #'date<)
       (remove-duplicates <> :test #'date=)))

;;; Options are built with the option construction language
;;; (option-language.lisp): (make-option (put 40) (american) (expires #D"...")).
