;;;; currency.lisp --- currencies and amounts of money
;;;;
;;;; A currency is a keyword registered with DEFINE-CURRENCY; the CURRENCY
;;;; type checks registration. MONEY is an amount in a currency, returned at
;;;; API boundaries (leg and bond values); engines work in raw doubles.
;;;; Arithmetic on amounts in different currencies signals CURRENCY-MISMATCH:
;;;; fincl has no FX conversion.

(in-package #:fincl)

(define-condition currency-mismatch (invalid-input)
  ((currencies :initarg :currencies :reader error-currencies)
   (operation :initarg :operation :initform nil :reader error-operation))
  (:report (lambda (c s)
             (format s "~@[~A: ~]amounts in ~{~S~^ and ~} cannot be combined without an FX rate."
                     (error-operation c) (error-currencies c))))
  (:documentation "Amounts or cash flows in different currencies were added,
compared or valued together."))

(defstruct (currency-info (:constructor make-currency-info (code name fraction-digits)))
  "What DEFINE-CURRENCY records about a currency."
  (code nil :type keyword :read-only t)
  (name "" :type string :read-only t)
  (fraction-digits 2 :type (integer 0 4) :read-only t))

(defvar *currencies* (make-hash-table :test 'eq)
  "Currency keyword -> CURRENCY-INFO.")

(defmacro define-currency (code &key name (fraction-digits 2))
  "Register the currency CODE, a keyword, with its NAME and the number of
FRACTION-DIGITS amounts are shown with.

  (define-currency :usd :name \"US Dollar\" :fraction-digits 2)"
  (check-type code keyword)
  `(progn (setf (gethash ,code *currencies*) (make-currency-info ,code ,name ,fraction-digits))
          ,code))

(defun currencyp (x)
  "True if X is a registered currency keyword."
  (and (keywordp x) (nth-value 1 (gethash x *currencies*))))

(deftype currency ()
  "A keyword registered with DEFINE-CURRENCY."
  '(satisfies currencyp))

(defun currency-name (currency)
  (currency-info-name (gethash currency *currencies*)))

(defun currency-fraction-digits (currency)
  (currency-info-fraction-digits (gethash currency *currencies*)))

(define-currency :usd :name "US Dollar")
(define-currency :eur :name "Euro")
(define-currency :gbp :name "British Pound")
(define-currency :jpy :name "Japanese Yen" :fraction-digits 0)
(define-currency :chf :name "Swiss Franc")
(define-currency :cad :name "Canadian Dollar")
(define-currency :aud :name "Australian Dollar")
(define-currency :sek :name "Swedish Krona")
(define-currency :nok :name "Norwegian Krone")
(define-currency :dkk :name "Danish Krone")
(define-currency :nzd :name "New Zealand Dollar")
(define-currency :hkd :name "Hong Kong Dollar")
(define-currency :cny :name "Chinese Yuan")

;;; --------------------------------------------------------------------
;;; Money
;;; --------------------------------------------------------------------

(defstruct (money (:constructor %make-money (amount currency)))
  "AMOUNT units of CURRENCY. Read-only."
  (amount 0d0 :type double-float :read-only t)
  (currency nil :type keyword :read-only t))

(defun make-money (amount currency)
  "AMOUNT (a real, coerced to double-float) in CURRENCY.

  (make-money 1000000 :usd)"
  (check-input amount real "a real amount")
  (check-input currency currency "a currency registered with DEFINE-CURRENCY")
  (%make-money (float amount 1d0) currency))

(defmethod print-object ((m money) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (m stream :type t)
        (format stream "~,vF ~A" (currency-fraction-digits (money-currency m))
                (money-amount m) (money-currency m)))))

(defun check-same-currency (operation amounts)
  (let ((currencies (->> amounts (mapcar #'money-currency) remove-duplicates)))
    (when (rest currencies)
      (error 'currency-mismatch :currencies currencies :operation operation))
    (first currencies)))

(defun money+ (amount &rest more)
  "The sum of amounts of money in one currency; signals CURRENCY-MISMATCH
otherwise.

  (money+ (make-money 1 :usd) (make-money 2 :usd)) => 3.00 USD"
  (let ((all (cons amount more)))
    (dolist (m all) (check-input m money "an amount of MONEY"))
    (%make-money (reduce #'+ all :key #'money-amount :initial-value 0d0)
                 (check-same-currency "money+" all))))

(defun money- (amount &rest more)
  "AMOUNT less the MORE amounts, or AMOUNT negated when MORE is empty; one
currency only (else CURRENCY-MISMATCH)."
  (let ((all (cons amount more)))
    (dolist (m all) (check-input m money "an amount of MONEY"))
    (let ((currency (check-same-currency "money-" all)))
      (%make-money (if more
                       (- (money-amount amount) (reduce #'+ more :key #'money-amount))
                       (- (money-amount amount)))
                   currency))))

(defun money* (amount factor)
  "AMOUNT scaled by the real FACTOR."
  (check-input amount money "an amount of MONEY")
  (check-input factor real "a real factor")
  (%make-money (* (money-amount amount) (float factor 1d0)) (money-currency amount)))
