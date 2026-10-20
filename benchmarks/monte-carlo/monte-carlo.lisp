;;;; monte-carlo.lisp --- Monte Carlo engines, one thread, fixed seed
;;;;
;;;; 100,000 paths on both sides: fincl's N-PATHS counts paths, QuantLib's
;;;; antithetic samples count pairs, so QuantLib runs 50,000 samples.
;;;;
;;;;   benchmark                QuantLib                                   fincl                                   work
;;;;   mc-european-gbm          MCEuropeanEngine<PseudoRandom>             :mc, :counter rng                       1 step
;;;;   mc-european-gbm-sobol    MCEuropeanEngine<LowDiscrepancy>           :mc, :rng :sobol (16 randomizations)    1 step
;;;;   mc-european-heston       MCEuropeanHestonEngine (QE martingale)     :mc, QE martingale-corrected            50 steps
;;;;   mc-asian-arith-cv        MCDiscreteArithmeticAPEngine, CV           :mc, :control-variates (:geometric-asian)  12 fixings
;;;;   mc-barrier-down-out      MCBarrierEngine (bridge crossing)          :mc, Brownian bridge                    252 steps
;;;;   mc-american-lsm          MCAmericanEngine (LSM, monomials, deg 3)   :mc LSM, monomials, degree 3            50 steps

(in-package #:fincl/benchmarks)

(defun mc (&rest settings)
  (apply #'make-engine :mc :n-paths 100000 :seed 42 settings))

(define-benchmark "mc-european-gbm"
    (:group :monte-carlo :title "European call, GBM" :engine "mc (counter)"
     :inputs (equity-inputs :kind "call") :work '(:paths 100000 :steps 1)
     :kind :statistical :parallel t
     :reference (price o :black-scholes m))
    ((m (equity-market)) (o (vanilla :call)) (e (mc)))
  (priced o e m))

(define-benchmark "mc-european-gbm-sobol"
    (:group :monte-carlo :title "European call, GBM, Sobol" :engine "mc (sobol)"
     :inputs (equity-inputs :kind "call") :work '(:paths 100000 :steps 1)
     :kind :statistical :parallel t
     :reference (price o :black-scholes m)
     :notes "QuantLib's Sobol estimate has no valid standard error (its sample
standard deviation ignores the low discrepancy), so the verdict uses fincl's,
from 16 randomizations; neither side uses antithetics")
    ((m (equity-market)) (o (vanilla :call)) (e (mc :rng :sobol :antithetic nil)))
  (priced o e m))

(define-benchmark "mc-european-heston"
    (:group :monte-carlo :title "European call, Heston" :engine "mc (counter)"
     :inputs (heston-inputs :kind "call") :work '(:paths 100000 :steps 50)
     :kind :statistical :parallel t
     :reference (price o (fourier p 1024) m)
     :notes "both use Andersen's quadratic-exponential scheme with the martingale
correction")
    ((m (equity-market)) (o (vanilla :call)) (p (apply #'make-heston *heston*))
     (e (mc :process p :n-steps 50)))
  (priced o e m))

(define-benchmark "mc-asian-arith-cv"
    (:group :monte-carlo :title "Arithmetic average-price call, geometric control variate"
     :engine "mc (counter)"
     :inputs (equity-inputs :kind "call" :average "arithmetic" :fixings 12)
     :work '(:paths 100000 :steps 12)
     :kind :statistical :parallel t
     :notes "QuantLib corrects with a fixed coefficient of 1 on the geometric
Asian, fincl with the regression coefficient")
    ((m (equity-market))
     (o (make-option-from-clauses `((:call ,(equity :strike)) (:average :arithmetic :dates ,(monthly-dates))
                                    (:expires ,*expiry*))))
     ;; With fixing dates, one step means no steps between them: 12 in all.
     (e (mc :n-steps 1 :control-variates '(:geometric-asian))))
  (priced o e m))

(define-benchmark "mc-barrier-down-out"
    (:group :monte-carlo :title "Down-and-out call, continuous, Brownian-bridge crossing"
     :engine "mc (counter)"
     :inputs (equity-inputs :kind "call" :barrier-type "down-and-out" :barrier 90 :rebate 0)
     :work '(:paths 100000 :steps 252)
     :kind :statistical :parallel t
     :reference (price o :black-scholes m)
     :notes "QuantLib samples the crossing with a uniform per step, fincl weights
by the survival probability")
    ((m (equity-market)) (o (barrier-option :call :down-and-out 90)) (e (mc :n-steps 252)))
  (priced o e m))

(define-benchmark "mc-american-lsm"
    (:group :monte-carlo :title "American put, Longstaff-Schwartz" :engine "mc (LSM)"
     :inputs (equity-inputs :kind "put" :exercise "american")
     :work '(:paths 100000 :calibration-paths 100000 :steps 50 :degree 3)
     :kind :statistical :parallel t
     :reference (price o (make-engine :binomial :n-steps 10000) m)
     :notes "both fit the exercise rule on one path set and price on another (fincl
:lsm-valuation :independent), a low-biased estimate of the 50-date Bermudan")
    ((m (equity-market)) (o (vanilla :put :exercise :american))
     (e (mc :n-steps 50 :basis-degree 3 :basis :monomial :lsm-valuation :independent)))
  (priced o e m))
