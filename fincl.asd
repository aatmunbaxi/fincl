;;;; fincl.asd

(defsystem "fincl"
  :description "Common Lisp quantitative finance library"
  :version "0.1.0"
  :depends-on ("alexandria"
               "num-utils"
               "special-functions"
               "statistics"
               "magicl"
               "random-state"
               "lparallel"
               "trivia"
               "bordeaux-threads"
               "arrow-macros"
               "iterate"
               "named-readtables"
               "closer-mop")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "iteration-helpers")
               (:module "math"
                :serial t
                :components ((:file "special-functions")
                             (:file "distributions")
                             (:file "random")
                             (:static-file "sobol-directions.sexp")
                             (:file "lets-be-rational")
                             (:file "implied-normal")
                             (:file "fourier")
                             (:file "extrapolation")
                             (:file "quadrature")
                             (:file "differentiation")
                             (:file "solvers")
                             (:file "linear-algebra")
                             (:file "interpolation")
                             (:file "optimize")))
               (:module "time"
                :serial t
                :components ((:file "date")
                             (:file "tenor")
                             (:file "calendar")
                             (:file "holidays")
                             (:file "calendars")
                             (:file "schedule")
                             (:file "day-counter")))
               ;; Defines the #D / #T readtable; every file after this one
               ;; may use it via NAMED-READTABLES:IN-READTABLE.
               (:file "syntax")
               (:file "currency")
               ;; The construction kernel: general, knows no instrument.
               (:module "construction"
                :serial t
                :components ((:file "slot-options")
                             (:file "facts")
                             (:file "fact-tables")
                             (:file "components")
                             (:file "clauses")
                             (:file "conventions")
                             (:file "rules")
                             (:file "languages")
                             (:file "printing")))
               (:module "term-structures"
                :serial t
                :components ((:file "interest-rate")
                             (:file "protocol")
                             (:file "flat")
                             (:file "dividends")
                             (:file "yield-curves")
                             (:file "rates-vol")
                             (:file "vol-surfaces")))
               (:file "market")
               (:module "instruments"
                :serial t
                :components ((:file "instrument")
                             (:file "options")
                             (:file "option-language")
                             (:file "path-features")
                             (:file "indexes")
                             (:file "cashflows")
                             (:file "rate-helpers")
                             (:file "rates")))
               (:module "processes"
                :serial t
                :components ((:file "parameters")
                             (:file "process")
                             (:file "gbm")
                             (:file "levy")
                             (:file "merton")
                             (:file "kou")
                             (:file "variance-gamma")
                             (:file "nig")
                             (:file "cgmy")
                             (:file "cev")
                             (:file "heston")
                             (:file "bates")
                             (:file "garch")
                             (:file "local-vol")
                             (:file "hull-white")))
               (:module "engines"
                :serial t
                :components ((:file "engine")
                             (:file "paths")
                             (:file "analytic")
                             (:file "monte-carlo")
                             (:file "path-payoffs")
                             (:file "binomial")
                             (:file "fd")
                             (:file "fd-heston")
                             (:file "cos")
                             (:file "selection")
                             (:file "discounting")
                             (:file "rates-analytic")
                             (:file "hull-white")))
               ;; Composite markets are priced through views; needs PRICE.
               (:file "pricing")
               (:file "implied-volatility")
               (:file "greeks")
               (:file "mc-greeks")
               (:file "calibration"))
  :in-order-to ((test-op (test-op "fincl/tests"))))

(defsystem "fincl/tests"
  :description "Tests for fincl"
  :depends-on ("fincl" "lparallel" "closer-mop" "magicl" "bordeaux-threads" "iterate")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "markets")
               (:file "construction")
               (:file "math")
               (:file "numerics")
               (:file "time")
               (:file "calendars")
               (:file "rates")
               (:file "syntax")
               (:file "market")
               (:file "instruments")
               (:file "analytic")
               (:file "cos")
               (:file "binomial")
               (:file "mc")
               (:file "processes")
               (:file "levy")
               (:file "selection")
               (:file "errors")
               (:file "implied-volatility")
               (:file "greeks")
               (:file "invariants")
               (:file "option-language")
               (:file "golden")
               (:file "rates-instruments")
               (:file "paths")
               (:file "surfaces")
               (:file "fd")
               (:file "dividends")
               (:file "readme")
               ;; The runners call the checks above; keep them last.
               (:file "tests"))
  ;; RUN-TESTS returns NIL when a check fails; make TEST-OP (and the shell
  ;; command around it) fail too.
  :perform (test-op (o c)
             (unless (symbol-call '#:fincl/tests '#:run-tests)
               (error "fincl/tests: RUN-TESTS reported failing checks."))))

(defsystem "fincl/benchmarks"
  :description "fincl against QuantLib: the same jobs priced by both, timed
under one protocol. Not part of fincl; run by hand in the .#benchmarks shell."
  :depends-on ("fincl" "lparallel" "iterate" "uiop")
  :pathname "benchmarks/"
  :serial t
  :components ((:file "package")
               (:file "harness")
               (:file "report")
               (:file "common")
               (:file "runner")
               (:file "calibration/calibration")
               (:file "options-pricing/options-pricing")
               (:file "exotics-analytic/exotics-analytic")
               (:file "monte-carlo/monte-carlo")
               (:file "pde/pde")
               (:file "rates/rates")
               (:file "calibration-models/calibration-models")))
