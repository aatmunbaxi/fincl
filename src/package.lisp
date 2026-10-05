;;;; package.lisp

(defpackage #:fincl
  (:use #:cl)
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
   ;; day counters
   #:day-counter #:day-count #:year-fraction
   #:actual-360 #:actual-365-fixed #:thirty-360 #:thirty-360-variant
   #:actual-actual-isda #:business-252 #:day-counter-calendar
   #:+actual-360+ #:+actual-365-fixed+ #:+actual-actual-isda+
   ;; market protocol
   #:market #:make-bs-market
   #:spot #:zero-rate #:dividend-yield #:black-vol
   #:discount-factor #:forward-factor #:forward #:black-variance
   #:valuation-date #:market-day-counter #:market-time #:derive-market
   ;; term structures and composite markets
   #:term-structure #:yield-curve #:dividend-curve #:black-vol-structure
   #:curve-discount #:curve-zero-rate #:curve-dividend-discount #:curve-dividend-yield
   #:structure-black-variance #:structure-black-vol #:bump-structure #:shift #:apply-shift
   #:flat-yield-curve #:flat-dividend-curve #:flat-black-vol #:flat-rate #:flat-vol
   #:composite-market #:make-market #:market-underlyings #:market-view #:view-underlying
   #:view-discount-curve #:bump #:ambiguous-underlying #:missing-market-data
   #:currency-mismatch
   #:instrument-underlying #:instrument-currency
   ;; instruments
   #:instrument #:payoff #:call-put-payoff #:strike-payoff #:vanilla-payoff #:kind #:strike
   #:validate-payoff #:validate-pricing-inputs
   #:exercise #:european-exercise #:american-exercise #:bermudan-exercise
   #:expiry #:exercise-dates #:exercise-allowed-p #:exercise-mask #:time-to-expiry
   #:resolve-expiry
   #:path-feature #:path-independent
   #:option #:option-payoff #:option-exercise #:option-path
   #:make-option #:make-vanilla-option
   #:payoff-value #:payoff-kernel #:define-payoff #:declare-payoff-variables
   #:formula-payoff-superclass
   ;; processes
   #:stochastic-process #:process-factors #:process-state-size
   #:initial-state #:make-stepper #:terminal-sampler #:process-vol
   #:characteristic-function #:log-cumulants
   #:resolve-process #:increment-characteristic-function #:independent-increments
   #:calendar-arbitrage
   #:define-process #:parameterized-class #:parameterized #:invalid-parameter
   #:process-parameters #:process-parameter #:parameter-bounds #:parameter-vector #:with-parameter-vector
   #:gbm #:cev #:heston #:garch
   ;; engines
   #:engine #:analytic-engine #:black-scholes-engine
   #:barone-adesi-whaley-engine #:monte-carlo-engine
   #:cos-engine #:n-terms #:truncation #:richardson-dates
   #:binomial-engine #:smooth #:extrapolate
   #:process #:n-paths #:n-steps #:seed #:antithetic #:n-chunks
   #:basis-degree #:basis #:regressors #:process-regressors #:rng #:sampler
   #:price #:price-analytic #:price-mc #:price-cos #:price-binomial
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
