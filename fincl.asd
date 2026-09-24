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
               "trivia")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "market")
               (:module "instruments"
                :serial t
                :components ((:file "instrument")
                             (:file "options")))
               (:module "processes"
                :serial t
                :components ((:file "process")
                             (:file "gbm")
                             (:file "cev")
                             (:file "heston")
                             (:file "garch")))
               (:module "engines"
                :serial t
                :components ((:file "engine")
                             (:file "analytic")
                             (:file "monte-carlo"))))
  :in-order-to ((test-op (test-op "fincl/tests"))))

(defsystem "fincl/tests"
  :description "Tests for fincl"
  :depends-on ("fincl" "lparallel")
  :pathname "tests/"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (symbol-call '#:fincl/tests '#:run-tests)))
