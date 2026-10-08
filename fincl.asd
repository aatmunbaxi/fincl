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
               "named-readtables"
               "closer-mop")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:module "math"
                :serial t
                :components ((:file "special-functions")
                             (:file "random")
                             (:file "lets-be-rational")
                             (:file "implied-normal")
                             (:file "fourier")
                             (:file "extrapolation")
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
                :components ((:file "protocol")
                             (:file "flat")))
               (:file "market")
               (:module "instruments"
                :serial t
                :components ((:file "instrument")
                             (:file "options")
                             (:file "option-language")))
               (:module "processes"
                :serial t
                :components ((:file "parameters")
                             (:file "process")
                             (:file "gbm")
                             (:file "cev")
                             (:file "heston")
                             (:file "garch")))
               (:module "engines"
                :serial t
                :components ((:file "engine")
                             (:file "analytic")
                             (:file "monte-carlo")
                             (:file "binomial")
                             (:file "cos")))
               ;; Composite markets are priced through views; needs PRICE.
               (:file "pricing")
               (:file "implied-volatility")
               (:file "greeks"))
  :in-order-to ((test-op (test-op "fincl/tests"))))

(defsystem "fincl/tests"
  :description "Tests for fincl"
  :depends-on ("fincl" "lparallel" "closer-mop" "magicl" "bordeaux-threads")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "markets")
               (:file "construction")
               (:file "math")
               (:file "numerics")
               (:file "time")
               (:file "calendars")
               (:file "syntax")
               (:file "market")
               (:file "instruments")
               (:file "analytic")
               (:file "cos")
               (:file "binomial")
               (:file "mc")
               (:file "processes")
               (:file "errors")
               (:file "implied-volatility")
               (:file "greeks")
               (:file "invariants")
               (:file "option-language")
               (:file "golden")
               ;; The runners call the checks above; keep them last.
               (:file "tests"))
  :perform (test-op (o c)
             (symbol-call '#:fincl/tests '#:run-tests)))
