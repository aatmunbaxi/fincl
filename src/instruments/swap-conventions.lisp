;;;; instruments/swap-conventions.lisp --- the terms of standard swaps, as data
;;;;
;;;; One table holds the terms that OIS-HELPER and SWAP-HELPER use as defaults. It also
;;;; holds the terms of the named conventions of the swap language (loaded after this
;;;; file, in rates-language.lisp). Thus a bootstrapped curve and a swap that is written
;;;; with the same convention agree by construction. A named convention extends one of the
;;;; two base conventions, which carry the defaults of the helpers.

(in-package #:fincl)

(defvar *swap-conventions* (make-hash-table :test 'eq)
  "Convention name -> property list of its terms. A :EXTENDS entry names the
convention whose terms it adds to or overrides.")

(defun swap-convention-terms (name &optional visited)
  "The terms of the swap convention NAME, with those of the convention it
extends below its own, as a property list. A convention that extends itself,
directly or through others, signals INVALID-ARGUMENT."
  (when (member name visited)
    (signal-invalid-argument 'name name 'keyword
                             (format nil "a convention that does not extend itself (chain ~{~S~^ -> ~})"
                                     (reverse (cons name visited)))))
  (let ((terms (or (gethash name *swap-conventions*)
                   (signal-invalid-argument 'name name 'keyword
                                            (format nil "one of ~{~S~^, ~}"
                                                    (a:hash-table-keys *swap-conventions*))))))
    (let ((base (getf terms :extends)))
      `(,@(a:remove-from-plist terms :extends)
        ,@(when base (swap-convention-terms base (cons name visited)))))))

(declaim (ftype (function (t t) (values t &optional)) swap-convention-term))
(defun swap-convention-term (name key)
  "The value of the term KEY of the swap convention NAME."
  (getf (swap-convention-terms name) key))

(defmacro define-swap-convention (name &body terms)
  "Define the swap convention NAME by its TERMS, a property list."
  `(setf (gethash ,name *swap-conventions*) (list ,@terms)))

;;; The defaults of the helpers.
(define-swap-convention :ois-defaults
  :settlement-days 2 :payment-lag 0 :payment-convention :following :payment-frequency :annual
  :roll-convention :modified-following)

(define-swap-convention :ibor-swap-defaults
  :fixed-frequency :annual :fixed-convention :modified-following
  :fixed-day-counter (make-instance 'thirty-360 :variant :bond-basis))

;;; The named conventions: the setups the bootstrap fixtures use.
(define-swap-convention :usd-sofr-ois
  :extends :ois-defaults :kind :ois :index (sofr) :payment-lag 2)

(define-swap-convention :eur-estr-ois
  :extends :ois-defaults :kind :ois :index (estr) :payment-lag 1)

(define-swap-convention :eur-euribor-6m-swap
  :extends :ibor-swap-defaults :kind :vanilla :index (euribor (make-tenor 6 :months))
  :fixed-convention :unadjusted)   ; as the bootstrap fixture; market swaps adjust the fixed dates.

(defparameter *named-swap-conventions* '(:usd-sofr-ois :eur-estr-ois :eur-euribor-6m-swap)
  "The conventions the swap language offers by name.")
