(asdf:defsystem #:fincl
  :description "Finance in Common Lisp"
  :author "Aatmun Baxi"

  :depends-on (#:fincl/math
               #:fincl/stochastics
               #:fincl/options
               #:fincl/magicl-extensions)

  :components ((:file "src/package")))


(asdf:defsystem #:fincl/math
  :description "Math functions"
  :depends-on (#:magicl
               #:special-functions
               #:random-state)
  :pathname "src/math/"
  :serial t
  :components ((:file "stats")))


(asdf:defsystem #:fincl/stochastics
  :description "Stochastic processes"
  :depends-on (:lparallel
               :alexandria
               #:fincl/math)
  :pathname "src/stochastic/"
  :components
  ((:file "stochastic-process")
   (:file "geometric-brownian-motion")
   (:file "ornstein-uhlenbeck")
   (:file "sabr")))

(asdf:defsystem #:fincl/options
  :description "Fincl options definitions"
  :depends-on (#:fincl/stochastics
               #:trivia
               #:fincl/math)
  :pathname "src/option/"
  :components
  ((:file "american")
   (:file "european")))
