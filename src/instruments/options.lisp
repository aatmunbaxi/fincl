;;;; instruments/options.lisp --- options as component bundles
;;;;
;;;; An option is payoff x exercise x path feature. Anything that only
;;;; changes a number inside a method (call vs. put) is a slot; anything that
;;;; changes which method applies (exercise style, path dependence) is a
;;;; class.

(in-package #:fincl)

(defclass vanilla-payoff (payoff)
  ((strike :initarg :strike :reader strike :type double-float
           :initform (a:required-argument :strike))))

(declaim (inline phi))
(defun phi (payoff)
  "+1 for a call, -1 for a put."
  (if (eq (kind payoff) :call) 1d0 -1d0))

(defmethod payoff-value ((p vanilla-payoff) x)
  (max 0d0 (* (phi p) (- x (strike p)))))

(defmethod payoff-kernel ((p vanilla-payoff))
  (let ((k (strike p))
        (phi (phi p)))
    (declare (double-float k phi))
    (lambda (s)
      (declare (double-float s) (optimize (speed 3) (safety 0)))
      (max 0d0 (* phi (- s k))))))

;;; --------------------------------------------------------------------
;;; The option
;;; --------------------------------------------------------------------

(defclass option (instrument)
  ((payoff :initarg :payoff :reader option-payoff)
   (exercise :initarg :exercise :reader option-exercise)
   (path :initarg :path :reader option-path
         :initform (make-instance 'path-independent))))

(defmethod print-object ((o option) stream)
  (print-unreadable-object (o stream :type t)
    (let ((p (option-payoff o)) (e (option-exercise o)))
      (format stream "~(~A~) ~(~A~) K=~,2F T=~,3F"
              (class-name (class-of e)) (kind p) (strike p) (expiry e)))))

(defun make-vanilla-option (kind strike expiry &key (exercise :european))
  (make-instance
   'option
   :payoff (make-instance 'vanilla-payoff :kind kind :strike (float strike 1d0))
   :exercise (make-instance (ecase exercise
                              (:european 'european-exercise)
                              (:american 'american-exercise))
                            :expiry (float expiry 1d0))))

;;; A small spec DSL. Pattern matching is the readable way to grow this: each
;;; new path feature adds one clause instead of another pile of keyword
;;; arguments.
;;;
;;;   (make-option '(:american :put 40 1))
;;;   (make-option '(:call 100 0.5d0))       ; European by default
;;;
(defun make-option (spec)
  "Build an OPTION from a list spec. EMATCH gives an informative error on an
unrecognized spec, so typos fail at construction rather than at pricing."
  (ematch spec
    ((list (and style (or :european :american))
           (and kind (or :call :put))
           (guard strike (realp strike))
           (guard expiry (and (realp expiry) (plusp expiry))))
     (make-vanilla-option kind strike expiry :exercise style))
    ((list (and kind (or :call :put))
           (guard strike (realp strike))
           (guard expiry (and (realp expiry) (plusp expiry))))
     (make-vanilla-option kind strike expiry))))
