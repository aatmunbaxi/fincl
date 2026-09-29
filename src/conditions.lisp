;;;; conditions.lisp --- the root of fincl's condition hierarchy

(in-package #:fincl)

(define-condition pricing-error (error) ()
  (:documentation "Superclass of every error fincl signals, so a caller can
handle all of them with one clause."))
