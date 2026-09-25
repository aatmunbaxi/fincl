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
                    (#:lp #:lparallel))
  (:import-from #:trivia #:match #:ematch #:guard)
  (:export
   ;; math
   #:norm-cdf #:norm-pdf #:norm-quantile #:solve-root
   ;; dates and tenors
   #:date #:make-date #:parse-date #:invalid-date #:date-serial
   #:date-year #:date-month #:date-day #:day-of-week
   #:date+ #:date- #:date< #:date<= #:date=
   #:leap-year-p #:days-in-month
   #:tenor #:make-tenor #:parse-tenor #:invalid-tenor
   #:tenor-n #:tenor-unit #:add-tenor
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
   #:market #:black-scholes-market #:make-bs-market
   #:spot #:zero-rate #:dividend-yield #:black-vol
   #:discount-factor #:forward-factor #:forward #:black-variance
   #:valuation-date #:market-day-counter #:market-time #:derive-market
   ;; instruments
   #:instrument #:payoff #:vanilla-payoff #:kind #:strike
   #:exercise #:european-exercise #:american-exercise #:bermudan-exercise
   #:expiry #:exercise-dates #:exercise-allowed-p #:time-to-expiry
   #:resolve-expiry
   #:path-feature #:path-independent
   #:option #:option-payoff #:option-exercise #:option-path
   #:make-option #:make-vanilla-option
   #:payoff-value #:payoff-kernel
   ;; processes
   #:stochastic-process #:process-factors #:process-state-size
   #:initial-state #:make-stepper #:terminal-sampler #:process-vol
   #:gbm #:cev #:heston #:garch
   ;; engines
   #:engine #:analytic-engine #:black-scholes-engine
   #:barone-adesi-whaley-engine #:monte-carlo-engine
   #:process #:n-paths #:n-steps #:seed #:antithetic #:n-chunks
   #:basis-degree #:rng #:sampler
   #:price #:price-analytic #:price-mc
   ;; conditions and restarts
   #:pricing-error #:unsupported-combination #:convergence-failure
   #:regression-failure #:incompatible-time-step #:option-expired
   #:retry-with-bracket #:skip-exercise-date
   ;; parallelism
   #:with-pricing-kernel))
