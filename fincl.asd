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
               "named-readtables"
               "closer-mop")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:module "math"
                :serial t
                :components ((:file "special-functions")
                             (:file "fourier")
                             (:file "extrapolation")
                             (:file "differentiation")
                             (:file "solvers")
                             (:file "linear-algebra")))
               (:module "time"
                :serial t
                :components ((:file "date")
                             (:file "tenor")
                             (:file "calendar")
                             (:file "day-counter")))
               ;; Defines the #D / #T readtable; every file after this one
               ;; may use it via NAMED-READTABLES:IN-READTABLE.
               (:file "syntax")
               (:file "market")
               (:module "instruments"
                :serial t
                :components ((:file "instrument")
                             (:file "options")))
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
                             (:file "cos"))))
  :in-order-to ((test-op (test-op "fincl/tests"))))

(defsystem "fincl/tests"
  :description "Tests for fincl"
  :depends-on ("fincl" "lparallel" "closer-mop")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "time")
               (:file "syntax")
               (:file "market")
               (:file "instruments")
               (:file "analytic")
               (:file "cos")
               (:file "binomial")
               (:file "mc")
               (:file "processes")
               (:file "errors")
               (:file "invariants")
               (:file "golden")
               ;; The runners call the checks above; keep them last.
               (:file "tests"))
  :perform (test-op (o c)
             (symbol-call '#:fincl/tests '#:run-tests)))
