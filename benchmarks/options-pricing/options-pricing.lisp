;;;; options-pricing.lisp --- vanillas by closed form, tree and Fourier
;;;;
;;;; Market: spot 100, rate 5%, dividend 2%, vol 20%, strike 100, one year,
;;;; flat curves, ACT/365F.
;;;;
;;;;   benchmark                    QuantLib                           fincl                     work
;;;;   european-call-black          AnalyticEuropeanEngine             :black-scholes
;;;;   american-put-baw             BaroneAdesiWhaleyApproximation...  :baw
;;;;   american-put-crr-1000        BinomialVanillaEngine<CRR>         :binomial, plain CRR      1000 steps
;;;;   american-put-bbsr            BinomialVanillaEngine<CRR>         :binomial default (BBSR)  1000 steps
;;;;   bermudan-put-quarterly       BinomialVanillaEngine<CRR>         :binomial, plain CRR      1000 steps
;;;;   bermudan-put-quarterly-cos   BinomialVanillaEngine<CRR>         :cos                      256 terms
;;;;   heston-european-analytic     AnalyticHestonEngine (Laguerre)    :cos                      128 points / 256 terms
;;;;   heston-european-cos          COSHestonEngine                    :cos                      256 terms, L = 12
;;;;   bates-european               BatesEngine (Laguerre)             :cos                      128 points / 256 terms
;;;;   merton-european              JumpDiffusionEngine                :merton-series
;;;;   vg-european                  VarianceGammaEngine                :cos                      256 terms
;;;;   cev-european                 AnalyticCEVEngine                  :cev (r = q)
;;;;   kou-european                 --                                 :cos                      256 terms

(in-package #:fincl/benchmarks)

(define-benchmark "european-call-black"
    (:group :options-pricing :title "European call, Black-Scholes" :engine "black-scholes"
     :inputs (equity-inputs :kind "call"))
    ((m (equity-market)) (o (vanilla :call)) (e (make-engine :black-scholes)))
  (priced o e m))

(define-benchmark "american-put-baw"
    (:group :options-pricing :title "American put, Barone-Adesi-Whaley" :engine "baw"
     :inputs (equity-inputs :kind "put" :exercise "american")
     :notes "the critical price is found to each library's own solver tolerance")
    ((m (equity-market)) (o (vanilla :put :exercise :american)) (e (make-engine :baw)))
  (priced o e m))

(define-benchmark "american-put-crr-1000"
    (:group :options-pricing :title "American put, plain CRR tree" :engine "binomial (no smoothing)"
     :inputs (equity-inputs :kind "put" :exercise "american") :work '(:steps 1000)
     :kind :method-error :tolerance 1d-2)
    ((m (equity-market)) (o (vanilla :put :exercise :american))
     (e (make-engine :binomial :n-steps 1000 :smooth nil :extrapolate nil)))
  (priced o e m))

(define-benchmark "american-put-bbsr"
    (:group :options-pricing :title "American put, fincl's default tree" :engine "binomial (BBSR)"
     :inputs (equity-inputs :kind "put" :exercise "american") :work '(:steps 1000)
     :kind :method-error :tolerance 1d-2
     :notes "fincl's default tree (Black-Scholes last step, Richardson over N and N/2)
against QuantLib's plain CRR at the same N")
    ((m (equity-market)) (o (vanilla :put :exercise :american))
     (e (make-engine :binomial :n-steps 1000)))
  (priced o e m))

(define-benchmark "bermudan-put-quarterly"
    (:group :options-pricing :title "Bermudan put, quarterly, plain CRR" :engine "binomial (no smoothing)"
     :inputs (equity-inputs :kind "put" :exercise "bermudan") :work '(:steps 1000)
     :kind :method-error :tolerance 1d-2
     :notes "exercise dates snap to the nearest tree step in both libraries")
    ((m (equity-market)) (o (vanilla :put :exercise :bermudan))
     (e (make-engine :binomial :n-steps 1000 :smooth nil :extrapolate nil)))
  (priced o e m))

(define-benchmark "bermudan-put-quarterly-cos"
    (:group :options-pricing :title "Bermudan put, quarterly, COS" :engine "cos"
     :inputs (equity-inputs :kind "put" :exercise "bermudan") :work '(:steps 1000 :terms 256)
     :kind :method-error :tolerance 1d-2
     :notes "fincl's COS backward recursion against QuantLib's 1000-step CRR tree")
    ((m (equity-market)) (o (vanilla :put :exercise :bermudan)) (e (fourier (make-gbm))))
  (priced o e m))

(define-benchmark "heston-european-analytic"
    (:group :options-pricing :title "Heston European call" :engine "cos"
     :inputs (heston-inputs :kind "call") :work '(:laguerre-points 128 :terms 256)
     :notes "QuantLib integrates the Heston characteristic function by Gauss-Laguerre;
fincl sums 256 cosine terms")
    ((m (equity-market)) (o (vanilla :call)) (e (fourier (apply #'make-heston *heston*))))
  (priced o e m))

(define-benchmark "heston-european-cos"
    (:group :options-pricing :title "Heston European call, COS on both sides" :engine "cos"
     :inputs (heston-inputs :kind "call") :work '(:terms 256 :truncation 12)
     :notes "both use Fang-Oosterlee's range c1 +- L sqrt(c2 + sqrt(c4)); QuantLib
prices the call directly, fincl the put and parity")
    ((m (equity-market)) (o (vanilla :call)) (e (fourier (apply #'make-heston *heston*))))
  (priced o e m))

(define-benchmark "bates-european"
    (:group :options-pricing :title "Bates European call" :engine "cos"
     :inputs (apply #'heston-inputs :kind "call" *jumps*) :work '(:laguerre-points 128 :terms 256))
    ((m (equity-market)) (o (vanilla :call))
     (e (fourier (apply #'make-bates (append *heston* *jumps*)))))
  (priced o e m))

(define-benchmark "merton-european"
    (:group :options-pricing :title "Merton jump-diffusion European call" :engine "merton-series"
     :inputs (apply #'equity-inputs :kind "call" *jumps*)
     :notes "fincl truncates the Poisson series below weight 1e-16, QuantLib at a
relative accuracy of 1e-12")
    ((m (equity-market)) (o (vanilla :call))
     (e (make-engine :merton-series :process (apply #'make-merton-jump :vol (equity :vol) *jumps*))))
  (priced o e m))

(define-benchmark "vg-european"
    (:group :options-pricing :title "Variance gamma European call" :engine "cos"
     :inputs (apply #'equity-inputs :kind "call" *variance-gamma*) :work '(:terms 256)
     :notes "QuantLib integrates the conditional Black price over the gamma density")
    ((m (equity-market)) (o (vanilla :call))
     (e (fourier (apply #'make-variance-gamma *variance-gamma*))))
  (priced o e m))

(define-benchmark "cev-european"
    (:group :options-pricing :title "CEV European call (r = q)" :engine "cev"
     :inputs (equity-inputs :kind "call" :rate 0.05d0 :dividend 0.05d0 :beta 0.5d0 :sigma 2d0)
     :notes "QuantLib's CEV is driftless in the forward and fincl's in the spot with
drift r - q; they coincide at r = q, so this row's market has q = 5%")
    ((m (equity-market :dividend 0.05d0)) (o (vanilla :call))
     (e (make-engine :cev :process (make-cev :beta 0.5d0 :sigma 2d0))))
  (priced o e m))

(define-benchmark "kou-european"
    (:group :options-pricing :title "Kou jump-diffusion European call" :engine "cos"
     :inputs (equity-inputs :kind "call") :work '(:terms 256) :kind :none)
    ((m (equity-market)) (o (vanilla :call)) (e (fourier (make-kou-jump :vol (equity :vol)))))
  (priced o e m))
