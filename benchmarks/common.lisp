;;;; common.lisp --- the shared inputs, written twice (here and common.hpp)
;;;;
;;;; Every value here has a twin in common.hpp; the input echo of each
;;;; benchmark (CHECK-INPUTS) catches the two drifting apart.

(in-package #:fincl/benchmarks)
(named-readtables:in-readtable fincl:syntax)

(defparameter *today* #D"2026-09-24"
  "Valuation date. *EXPIRY*, 365 days later, is 1.0 in ACT/365F.")
(defparameter *expiry* #D"2027-09-24")

(defparameter *equity* '(:spot 100 :strike 100 :rate 0.05d0 :dividend 0.02d0 :vol 0.2d0)
  "The flat equity market and the strike of every option.")

(defparameter *heston* '(:v0 0.04d0 :kappa 2d0 :theta 0.04d0 :xi 0.3d0 :rho -0.7d0))

(defparameter *jumps* '(:intensity 0.5d0 :jump-mean -0.1d0 :jump-vol 0.15d0)
  "Merton's and Bates's lognormal jumps.")

(defparameter *variance-gamma* '(:sigma 0.2d0 :nu 0.2d0 :theta -0.14d0))

(defparameter *ssvi* '(:theta-per-year 0.04d0 :rho -0.6d0 :eta 1.2d0 :gamma 0.4d0)
  "An SSVI surface with ATM total variance theta(T) = 0.04 T.")

(defun equity (key) (getf *equity* key))

(defun equity-inputs (&rest more)
  "The equity market as an input plist, with MORE's keys added or replacing
the market's."
  (let ((base `(,@*equity* :expiry-days 365)))
    `(,@more ,@(apply #'a:remove-from-plist base
                      (iter (for (k nil) on more by #'cddr) (collect k))))))

(defun equity-market (&key (rate (equity :rate)) (dividend (equity :dividend)) (vol (equity :vol)))
  (make-bs-market :valuation-date *today* :spot (equity :spot) :rate rate :dividend dividend :vol vol))

(defun quarterly-dates ()
  "2026-12-24, 2027-03-24, 2027-06-24, 2027-09-24."
  (iter (for months in '(3 6 9 12))
        (collect (add-tenor *today* (make-tenor months :months)))))

(defun monthly-dates ()
  (iter (for months from 1 to 12)
        (collect (add-tenor *today* (make-tenor months :months)))))

(defun vanilla (kind &key (strike (equity :strike)) (exercise :european))
  "A KIND (:call or :put) on the shared strike, expiring at *EXPIRY*, with
EXERCISE :european, :american or :bermudan (quarterly)."
  (make-option-from-clauses
   `((,kind ,strike)
     ,@(ecase exercise
         (:european '())
         (:american '((:american)))
         (:bermudan `((:bermudan :dates ,(quarterly-dates)))))
     (:expires ,*expiry*))))

(defun ssvi-surface ()
  (destructuring-bind (&key theta-per-year rho eta gamma) *ssvi*
    (make-ssvi-surface '(1) `(,theta-per-year) :rho rho :eta eta :gamma gamma)))

(defun heston-inputs (&rest more)
  (apply #'equity-inputs (append *heston* more)))

(defun fourier (process &optional (n-terms 256))
  "A COS engine for PROCESS."
  (make-engine :cos :process process :n-terms n-terms))

(defun barrier-option (kind type level &key (rebate 0) monitoring)
  (make-option-from-clauses
   `((,kind ,(equity :strike))
     (:barrier ,type ,level :rebate ,rebate ,@(when monitoring `(:monitoring ,monitoring)))
     (:expires ,*expiry*))))

(defun priced (option engine market)
  "PRICE's value and standard error, without the engine."
  (multiple-value-bind (value std-error) (price option engine market)
    (values value std-error)))
