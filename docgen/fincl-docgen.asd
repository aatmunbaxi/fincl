(defsystem "fincl/docgen"
  :description "Generates the fincl documentation with mgl-pax."
  :depends-on ("fincl" "mgl-pax" "mgl-pax/document")
  :pathname "."
  :components ((:file "docs")))
