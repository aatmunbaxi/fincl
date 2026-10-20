;;;; calibration-models.lisp --- whole calibration jobs
;;;;
;;;; Each side generates its quotes from a known parameter set with its own
;;;; pricer (in the setup, untimed), then calibrates from the same start
;;;; point (timed). The value is the fit's implied-vol RMSE; the recovered
;;;; parameters, printed with the details, are compared to the generating
;;;; ones, since two optimizers need not stop at the same point.
;;;;
;;;;   benchmark               QuantLib                                       fincl
;;;;   heston-calibration      HestonModel + AnalyticHestonEngine, LM         calibrate, COS 256 terms
;;;;   sabr-smile-fit          SABRInterpolation (Hagan), beta fixed          fit-sabr :hagan, beta fixed
;;;;   svi-fit                 SviInterpolation (raw SVI)                     fit-svi
;;;;   hull-white-calibration  HullWhite + JamshidianSwaptionEngine, LM       calibrate-hull-white

(in-package #:fincl/benchmarks)
(named-readtables:in-readtable fincl:syntax)

(defparameter *heston-truth* '(:v0 0.05d0 :kappa 1.5d0 :theta 0.06d0 :xi 0.6d0 :rho -0.65d0)
  "The parameters the Heston quotes come from; the fit starts at *HESTON*.")

(defparameter *calibration-tenors* '("3M" "6M" "1Y" "18M" "2Y"))
(defparameter *calibration-strikes* '(80 90 95 100 105 110 120))

(defparameter *sabr-truth* '(:alpha 2d0 :beta 0.5d0 :rho -0.4d0 :nu 0.6d0))
(defparameter *smile-strikes* '(70 76 82 88 94 100 106 112 118 124 130))

(defparameter *svi-truth* '(:a 0.03d0 :b 0.1d0 :rho -0.4d0 :m 0.02d0 :sigma 0.15d0)
  "Raw SVI in total variance at one year, k = ln(K/F).")

(defparameter *hull-white-truth* '(:a 0.08d0 :sigma 0.009d0))

(defun out-of-the-money (strike date market)
  "The out-of-the-money European vanilla at STRIKE expiring on DATE."
  (make-option-from-clauses
   `((,(if (< strike (forward market date)) :put :call) ,strike) (:expires ,date))))

(defun heston-quotes (market)
  "Option quotes at *CALIBRATION-TENORS* x *CALIBRATION-STRIKES*, priced
under *HESTON-TRUTH* by COS with 1024 terms."
  (let ((engine (fourier (apply #'make-heston *heston-truth*) 1024)))
    (iter outer
          (for tenor in *calibration-tenors*)
          (for date = (add-tenor *today* (parse-tenor tenor)))
          (iter (for k in *calibration-strikes*)
                (for o = (out-of-the-money k date market))
                (in outer (collect (make-option-quote o :price (price o engine market))))))))

(defun parameter-details (process names)
  (iter (for name in names)
        (appending `(,name ,(process-parameter process name)))))

(define-benchmark "heston-calibration"
    (:group :calibration-models :title "Heston to 5 expiries x 7 strikes, implied-vol errors"
     :engine "calibrate (COS)"
     :inputs `(,@(equity-inputs) :expiries 5 :strikes 7 ,@*heston-truth*)
     :work '(:start (:v0 0.04 :kappa 2 :theta 0.04 :xi 0.3 :rho -0.7) :terms 256)
     :kind :method-error :tolerance 1d-4 :parallel t
     :notes "value: the RMSE of the implied-vol residuals at the fit. QuantLib
prices by Gauss-Laguerre (128 points), fincl by COS (256 terms); both minimize
implied-vol errors by Levenberg-Marquardt from *HESTON*")
    ((m (equity-market)) (quotes (heston-quotes m)))
  (multiple-value-bind (fitted report)
      (calibrate (apply #'make-heston *heston*) m quotes :engine '(:cos :n-terms 256))
    (values (getf report :rmse) nil
            `(,@(parameter-details fitted '(:v0 :kappa :theta :xi :rho))
              :iterations ,(getf report :iterations)))))

(defun smile-quotes (market vol-of-strike)
  "Quotes at *SMILE-STRIKES* for the one-year expiry, at the vols
VOL-OF-STRIKE gives."
  (mapcar (lambda (k) (make-option-quote (out-of-the-money k *expiry* market) :vol (funcall vol-of-strike k)))
          *smile-strikes*))

(define-benchmark "sabr-smile-fit"
    (:group :calibration-models :title "SABR to 11 strikes at one year, beta fixed at 0.5"
     :engine "fit-sabr (Hagan)"
     :inputs `(,@(equity-inputs) :strikes 11 ,@*sabr-truth*)
     :kind :method-error :tolerance 1d-4
     :notes "quotes from Hagan's formula (QuantLib's sabrVolatility and fincl's
sabr-volatility :hagan agree to the bit on the fixtures); QuantLib starts at
alpha 1, nu 0.3, rho 0 with no restarts, fincl from its own ATM guess")
    ((m (equity-market))
     (quotes (destructuring-bind (&key alpha beta rho nu) *sabr-truth*
               (let ((f (forward m *expiry*)))
                 (smile-quotes m (lambda (k) (sabr-volatility k f 1 alpha beta rho nu :hagan)))))))
  (multiple-value-bind (smile rmse) (fit-sabr quotes m :beta 0.5d0 :approximation :hagan)
    (values rmse nil `(:alpha ,(fincl::sabr-alpha smile) :rho ,(fincl::sabr-rho smile)
                       :nu ,(fincl::sabr-nu smile)))))

(define-benchmark "svi-fit"
    (:group :calibration-models :title "Raw SVI to 11 strikes at one year" :engine "fit-svi"
     :inputs `(,@(equity-inputs) :strikes 11 ,@*svi-truth*)
     :kind :method-error :tolerance 1d-4
     :notes "QuantLib: Levenberg-Marquardt on all five parameters from a = 0.04,
b = 0.1, sigma = 0.1, rho = 0, m = 0, no restarts; fincl: Zeletskii-De Marco's
reduction, then a Levenberg-Marquardt polish")
    ((m (equity-market))
     (quotes (destructuring-bind (&key a b rho ((:m center)) sigma) *svi-truth*
               (let ((f (forward m *expiry*)))
                 (smile-quotes m (lambda (k)
                                   (let ((x (- (log (/ k f)) center)))
                                     (sqrt (+ a (* b (+ (* rho x) (sqrt (+ (* x x) (* sigma sigma))))))))))))))
  (multiple-value-bind (slice rmse) (fit-svi quotes m)
    (values rmse nil `(:a ,(fincl::svi-a slice) :b ,(fincl::svi-b slice) :rho ,(fincl::svi-rho slice)
                       :m ,(fincl::svi-m slice) :sigma ,(fincl::svi-sigma slice)))))

(defun hull-white-quotes (market)
  "The five co-terminal payer swaptions at 2.7% into 2036-09-28, with the
Black vols of their Jamshidian prices under *HULL-WHITE-TRUTH*."
  (let ((truth (apply #'make-hull-white *hull-white-truth*)))
    (mapcar (lambda (reset)
              (let* ((s (make-swaption (euro-swap :payer 1 0.027d0 reset #D"2036-09-28") reset))
                     (target (price s (make-engine :jamshidian :process truth) market)))
                `(,s ,(solve-brent (lambda (vol)
                                     (- (price s (make-engine :black-rates :volatility vol) market) target))
                                   0.01d0 2d0 :tolerance 1d-15))))
            *hull-white-resets*)))

(define-benchmark "hull-white-calibration"
    (:group :calibration-models :title "Hull-White to 5 co-terminal swaptions" :engine "calibrate-hull-white"
     :inputs `(:swaptions 5 :fixed-rate 0.027d0 ,@*hull-white-truth*)
     :work '(:start (:a 0.2 :sigma 0.02))
     :kind :method-error :tolerance 1d-4
     :notes "value: the fitted mean reversion a (the quotes come from a = 0.08);
both minimize relative price errors, QuantLib's swaption helpers building their
own swaps from the exercise dates")
    ((m (rates-market :eur-discount :euribor-6m)) (quotes (hull-white-quotes m)))
  (multiple-value-bind (fitted errors iterations)
      (calibrate-hull-white (make-hull-white :a 0.2d0 :sigma 0.02d0) m quotes)
    (values (hull-white-a fitted) nil
            `(:sigma ,(process-parameter fitted :sigma)
              :max-relative-error ,(reduce #'max (map 'list #'abs errors))
              :iterations ,iterations))))
