(asdf:defsystem #:fincl-plot
  :description "Visualization utilites"
  :depends-on (#:py4cl
               :magicl)
  :pathname "src/plot/"
  :components ((:file "package")
               (:file "plotting")))
