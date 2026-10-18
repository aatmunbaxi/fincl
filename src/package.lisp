;;;; package.lisp

(defpackage #:fincl
  (:use #:cl #:iterate)
  ;; ITERATE exports a MINIMIZE clause; fincl's MINIMIZE is the optimizer.
  ;; Inside ITER forms, write the clause as ITER:MINIMIZE.
  (:shadow #:minimize)
  ;; Package-local nicknames keep NU.STATISTICS:MEAN and similar names from
  ;; colliding and keep call sites short. Supported natively by SBCL,
  ;; CCL, ECL, ABCL, Clasp and Allegro; elsewhere load
  ;; trivial-package-local-nicknames first.
  (:local-nicknames (#:a #:alexandria)
                    (#:nu #:num-utils)
                    (#:sf #:special-functions)
                    (#:sts #:nu.statistics)
                    (#:rs #:random-state)
                    (#:lp #:lparallel)
                    (#:c2mop #:closer-mop))
  (:import-from #:trivia #:match #:ematch #:guard)
  (:import-from #:arrow-macros #:-> #:->> #:cond->)
  (:export
   ;; math
   #:norm-cdf #:norm-pdf #:norm-quantile #:expm1 #:log1p #:erfcx
   #:bachelier-formula #:implied-black-vol #:implied-normal-vol #:implied-volatility
   #:price-out-of-bounds #:greeks #:bump-greeks
   #:solve-bisection #:solve-brent #:solve-newton
   #:richardson-extrapolate #:richardson-weights
   #:finite-difference-weights #:apply-stencil #:difference-quotient #:derivative
   #:default-step #:jacobian!
   #:double-vector #:solve-tridiagonal! #:solve-tridiagonal-cyclic! #:tridiagonal-multiply
   #:interpolator #:make-interpolator #:interpolator-derivative #:interpolator-second-derivative
   #:interpolator-knots #:interpolator-values #:interpolator-method #:extrapolation-error
   #:interpolator-2d #:make-interpolator-2d
   #:minimize #:use-best
   ;; construction kernel
   #:define-construction-language #:define-clause #:define-convention #:define-fact
   #:define-fact-rule #:define-slot-option
   #:publish #:fact #:require-fact #:fact-present-p #:fact-provenance #:with-scope
   #:with-fact-table #:apply-clauses #:make-from-facts #:real-to-double
   #:annotated-class #:component-class
   #:object-clauses #:print-clause-form #:round-trip-p
   #:construction-error #:undeclared-fact #:fact-type-error #:conflicting-clauses
   #:missing-fact #:fact-rule-cycle #:unknown-clause #:unknown-convention #:unused-fact
   #:clause-argument-error #:ambiguous-clause #:malformed-clause #:unknown-language
   ;; dates and tenors
   #:date #:datep #:make-date #:parse-date #:invalid-date #:date-serial
   #:date-year #:date-month #:date-day #:day-of-week
   #:date+ #:date- #:date< #:date<= #:date=
   #:leap-year-p #:days-in-month
   #:tenor #:make-tenor #:parse-tenor #:invalid-tenor
   #:tenorp #:tenor-n #:tenor-unit #:add-tenor
   ;; #D / #T literal syntax (a named readtable)
   #:syntax
   ;; calendars
   #:calendar #:null-calendar #:weekends-only #:joint-calendar
   #:joint-calendars #:joint-rule
   #:business-day-p #:holiday-p #:weekend-p #:adjust #:business-days-between
   #:count-business-days #:holiday-calendar #:calendar-holidays #:define-calendar
   #:define-observance #:easter #:nth-weekday #:clear-calendar-cache
   #:target #:+target+ #:united-states-settlement #:+united-states-settlement+
   #:nyse #:+nyse+ #:united-states-government-bond #:+united-states-government-bond+
   #:united-states-sofr #:+united-states-sofr+
   #:united-kingdom-settlement #:+united-kingdom-settlement+
   #:united-kingdom-exchange #:+united-kingdom-exchange+
   #:advance #:calendar-end-of-month #:calendar-end-of-month-p
   #:schedule #:make-schedule #:schedule-dates #:schedule-unadjusted-dates #:schedule-regular-p
   #:schedule-calendar #:schedule-tenor #:schedule-convention #:schedule-termination-convention
   #:schedule-rule #:schedule-end-of-month #:actual-actual-icma
   ;; day counters
   #:day-counter #:day-count #:year-fraction
   #:actual-360 #:actual-365-fixed #:thirty-360 #:thirty-360-variant
   #:actual-actual-isda #:business-252 #:day-counter-calendar
   #:+actual-360+ #:+actual-365-fixed+ #:+actual-actual-isda+
   ;; currencies and money
   #:currency #:define-currency #:currency-mismatch #:currency-name
   #:money #:make-money #:money-amount #:money-currency #:money+ #:money- #:money*
   ;; interest rates
   #:interest-rate #:make-interest-rate #:interest-rate-rate #:interest-rate-day-counter
   #:interest-rate-compounding #:interest-rate-frequency #:compounding #:frequency
   #:compound-factor #:discount-factor-of #:implied-rate #:equivalent-rate
   ;; market protocol
   #:market #:make-bs-market
   #:spot #:zero-rate #:dividend-yield #:black-vol
   #:discount-factor #:forward-factor #:forward #:black-variance
   #:valuation-date #:market-day-counter #:market-time #:derive-market
   ;; term structures and composite markets
   #:term-structure #:yield-curve #:dividend-curve #:black-vol-structure
   #:curve-discount #:curve-zero-rate #:curve-dividend-discount #:curve-dividend-yield
   ;; vol surfaces and calibration (task 17)
   #:smile #:smile-variance #:smile-vol #:smile-expiry #:make-interpolated-smile
   #:svi-slice #:make-svi-slice #:sabr-smile #:make-sabr-smile #:sabr-volatility
   #:moneyness-vol-structure #:surface-variance #:smile-surface #:make-smile-surface
   #:grid-vol-surface #:make-grid-vol-surface #:svi-surface #:make-svi-surface
   #:ssvi-surface #:make-ssvi-surface #:surface-smile
   #:butterfly-arbitrage-free-p #:calendar-arbitrage-free-p #:butterfly-arbitrage
   #:option-quote #:make-option-quote #:quote-set #:quote-set-quotes #:quote-instrument
   #:quote-weight #:quote-implied-vol #:quote-price #:quotes-vol-surface
   #:fit-svi #:fit-ssvi #:fit-sabr #:local-vol #:make-local-vol #:calibrate
   #:structure-black-variance #:structure-black-vol #:bump-structure #:shift #:apply-shift
   #:flat-yield-curve #:flat-dividend-curve #:flat-black-vol #:flat-rate #:flat-vol
   #:dated-curve #:interpolated-discount-curve #:interpolated-zero-curve
   #:interpolated-forward-curve #:curve-forward-rate #:curve-reference-date #:curve-day-counter
   #:curve-time #:curve-dates
   #:composite-market #:make-market #:market-underlyings #:market-view #:view-underlying
   #:view-discount-curve #:bump #:ambiguous-underlying #:missing-market-data
   #:unsupported-quanto
   #:instrument-underlying #:instrument-currency
   ;; indexes, cash flows and legs
   #:interest-rate-index #:ibor-index #:overnight-index #:euribor #:sofr #:estr #:sonia
   #:index-name #:index-tenor #:index-fixing-calendar #:index-fixing-days #:index-day-counter
   #:index-currency #:index-forecast-curve #:index-fixing #:index-value-date
   #:index-maturity-date #:index-fixing-date #:missing-fixing
   #:cashflow #:simple-cashflow #:coupon #:fixed-rate-coupon #:floating-rate-coupon
   #:ibor-coupon #:overnight-indexed-coupon #:payment-date #:cashflow-amount #:coupon-rate
   #:coupon-nominal #:accrual-start #:accrual-end #:accrual-period
   #:fixed-leg #:floating-leg #:leg-npv #:leg-bps #:accrued-amount #:market-fixings
   ;; rate helpers and bootstrapping
   #:rate-helper #:deposit-helper #:fra-helper #:futures-helper #:ois-helper #:swap-helper
   #:helper-quote #:helper-pillar-date #:helper-implied-quote #:helper-tenor
   #:bootstrap-curve #:piecewise-yield-curve #:curve-helpers #:bump-structure-quote
   ;; interest-rate instruments (task 16)
   #:rates-instrument #:bond #:fixed-rate-bond #:make-fixed-rate-bond #:bond-cashflows
   #:bond-face #:bond-schedule #:bond-coupon #:bond-redemption #:bond-settlement-days
   #:bond-maturity-date #:bond-settlement-date #:bond-notional
   #:bond-accrued-amount #:bond-dirty-price #:bond-clean-price #:bond-bps #:bond-npv
   #:bond-yield #:bond-price-from-yield #:bond-duration #:bond-convexity
   #:z-spread #:bond-price-from-z-spread
   #:swap #:vanilla-swap #:ois-swap #:make-vanilla-swap #:make-ois-swap #:swap-legs
   #:swap-type #:swap-nominal #:swap-fixed-rate #:swap-spread
   #:swap-npv #:swap-leg-npv #:swap-leg-bps #:fair-rate #:fair-spread
   #:forward-rate-agreement #:make-forward-rate-agreement #:fra-forward-rate #:fra-amount
   #:fra-npv
   #:cap-floor #:make-cap-floor #:cap-floor-type #:cap-floor-leg #:cap-rates #:floor-rates
   #:swaption #:make-swaption #:swaption-swap #:swaption-exercise #:swaption-settlement
   #:rates-vol-structure #:flat-rates-vol #:expiry-rates-vol #:rates-vol-variance
   ;; path simulation (task 19)
   #:payoff-requirements #:path-accumulator-size #:path-payment-dates #:path-kernel
   #:define-path-payoff #:path-payoff
   #:process-state-names #:state-index #:instrument-mandatory-dates
   #:rates-engine #:discounting-engine #:black-rates-engine #:bachelier-rates-engine
   #:cap-floor-optionlet-prices
   #:callable-fixed-rate-bond #:make-callable-fixed-rate-bond #:bond-call-schedule
   #:hull-white #:make-hull-white #:hull-white-a #:make-rate-curve #:hull-white-discount-bond
   #:hull-white-discount #:hull-white-bond-option
   #:jamshidian-swaption-engine #:hull-white-tree-engine #:calibrate-hull-white
   ;; instruments
   #:instrument #:payoff #:call-put-payoff #:strike-payoff #:vanilla-payoff #:kind #:strike
   #:validate-payoff #:validate-pricing-inputs
   #:exercise #:european-exercise #:american-exercise #:bermudan-exercise
   #:expiry #:exercise-dates #:exercise-allowed-p #:exercise-mask #:time-to-expiry
   #:resolve-expiry
   #:path-feature #:path-independent #:observed-path #:asian #:barrier #:lookback
   #:observation-dates #:asian-average #:strike-type #:barrier-direction #:barrier-knock
   #:barrier-level #:barrier-rebate #:barrier-type #:past-observation
   #:option #:option-payoff #:option-exercise #:option-path
   #:make-option #:make-option-from-clauses #:make-vanilla-option
   #:payoff-value #:payoff-kernel #:define-payoff #:declare-payoff-variables
   #:formula-payoff-superclass
   ;; processes
   #:stochastic-process #:process-factors #:process-state-size
   #:initial-state #:make-stepper #:bridge-variance #:terminal-sampler #:process-vol
   #:characteristic-function #:log-cumulants
   #:resolve-process #:increment-characteristic-function #:independent-increments
   #:calendar-arbitrage
   #:define-process #:parameterized-class #:parameterized #:invalid-parameter
   #:process-parameters #:process-parameter #:parameter-bounds #:parameter-vector #:with-parameter-vector
   #:gbm #:cev #:heston #:garch #:constructor-collision
   #:make-gbm #:make-cev #:make-heston #:make-garch #:make-merton-jump #:make-kou-jump
   #:make-variance-gamma #:make-nig #:make-cgmy #:make-bates
   #:exponential-levy #:levy-exponent #:levy-cumulants #:diffusion-variance
   #:no-exponential-moment #:simulatable-p #:jump-diffusion
   #:merton-jump #:kou-jump #:variance-gamma #:nig #:cgmy #:bates
   ;; engines
   #:engine #:analytic-engine #:black-scholes-engine
   #:barone-adesi-whaley-engine #:merton-series-engine #:cev-engine #:monte-carlo-engine
   #:cos-engine #:n-terms #:truncation #:richardson-dates
   #:binomial-engine #:smooth #:extrapolate
   #:process #:n-paths #:n-steps #:seed #:antithetic #:n-chunks
   #:basis-degree #:basis #:regressors #:process-regressors #:rng #:sampler
   #:price #:price-analytic #:price-mc #:price-cos #:price-binomial
   #:make-engine #:engine-candidates #:with-engines #:engine-fallback #:no-suitable-engine
   ;; conditions and restarts
   #:fincl-error #:invalid-input #:pricing-error
   #:invalid-argument #:check-input #:signal-invalid-argument
   #:invalid-exercise #:invalid-literal #:unknown-parameter
   #:unsupported-combination #:model-domain-error #:convergence-failure
   #:regression-failure #:incompatible-time-step #:option-expired #:rank-deficient
   #:invalid-tree #:non-positive-volatility #:non-stationary-process
   #:retry-with-bracket #:skip-exercise-date
   #:use-engine #:increase-steps #:use-process-period #:with-pricing-policy
   #:derive-engine
   ;; parallelism
   #:with-pricing-kernel))
