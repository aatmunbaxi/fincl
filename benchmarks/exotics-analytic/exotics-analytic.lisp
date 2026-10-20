;;;; exotics-analytic.lisp --- closed forms for barriers and geometric Asians
;;;;
;;;; Market and strike as options-pricing; barriers continuous unless noted,
;;;; fixings monthly.
;;;;
;;;;   benchmark                     QuantLib                                          fincl
;;;;   barrier-down-out-continuous   AnalyticBarrierEngine                             :black-scholes
;;;;   barrier-up-in-rebate          AnalyticBarrierEngine                             :black-scholes
;;;;   barrier-discrete-monthly      --                                                :black-scholes (BGK shift)
;;;;   asian-geometric-discrete      AnalyticDiscreteGeometricAveragePriceAsianEngine  :black-scholes

(in-package #:fincl/benchmarks)

(define-benchmark "barrier-down-out-continuous"
    (:group :exotics-analytic :title "Down-and-out call, continuous" :engine "black-scholes"
     :inputs (equity-inputs :kind "call" :barrier-type "down-and-out" :barrier 90 :rebate 0))
    ((m (equity-market)) (o (barrier-option :call :down-and-out 90)) (e (make-engine :black-scholes)))
  (priced o e m))

(define-benchmark "barrier-up-in-rebate"
    (:group :exotics-analytic :title "Up-and-in call with a rebate, continuous" :engine "black-scholes"
     :inputs (equity-inputs :kind "call" :barrier-type "up-and-in" :barrier 120 :rebate 3)
     :notes "the rebate of a knock-in is paid at expiry if the barrier is never touched")
    ((m (equity-market)) (o (barrier-option :call :up-and-in 120 :rebate 3))
     (e (make-engine :black-scholes)))
  (priced o e m))

(define-benchmark "barrier-discrete-monthly"
    (:group :exotics-analytic :title "Down-and-out call, monthly monitoring" :engine "black-scholes"
     :inputs (equity-inputs :kind "call" :barrier-type "down-and-out" :barrier 90 :rebate 0)
     :kind :none
     :notes "Broadie-Glasserman-Kou's shift of the barrier; QuantLib has no analytic
discretely monitored barrier")
    ((m (equity-market)) (o (barrier-option :call :down-and-out 90 :monitoring (monthly-dates)))
     (e (make-engine :black-scholes)))
  (priced o e m))

(define-benchmark "asian-geometric-discrete"
    (:group :exotics-analytic :title "Geometric average-price call, monthly fixings"
     :engine "black-scholes"
     :inputs (equity-inputs :kind "call" :average "geometric" :fixings 12))
    ((m (equity-market))
     (o (make-option-from-clauses `((:call ,(equity :strike)) (:average :geometric :dates ,(monthly-dates))
                                    (:expires ,*expiry*))))
     (e (make-engine :black-scholes)))
  (priced o e m))
