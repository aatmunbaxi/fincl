;;;; syntax-literals.lisp --- compiled and loaded by SYNTAX-CHECKS
;;;;
;;;; Not an ASDF component: the test runs COMPILE-FILE on it so that the
;;;; literals go through MAKE-LOAD-FORM and back.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defparameter *fasl-date* #D"2027-06-15")
(defparameter *fasl-tenor* #T"3M")

(defun fasl-literals ()
  (list #D"2024-02-29" #T"10D" #T"-1Y"))
