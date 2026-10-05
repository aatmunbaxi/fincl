;;;; fincl-viz.asd --- developer plotting aid; not part of fincl

(defsystem "fincl-viz"
  :description "Plot fincl simulations with matplotlib through py4cl."
  :depends-on ("fincl" "py4cl")
  :pathname "viz/"
  :components ((:file "viz")))
