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
   ;; market protocol
   #:market #:black-scholes-market #:make-bs-market
   #:spot #:zero-rate #:dividend-yield #:black-vol
   #:discount-factor #:forward-factor #:forward
   ;; instruments
   #:instrument #:payoff #:vanilla-payoff #:kind #:strike
   #:exercise #:european-exercise #:american-exercise #:expiry
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
   #:regression-failure #:incompatible-time-step
   #:retry-with-bracket #:skip-exercise-date
   ;; parallelism
   #:with-pricing-kernel))
