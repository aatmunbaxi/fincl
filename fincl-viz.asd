;;;; fincl-viz.asd --- developer plotting aid; not part of fincl

(defsystem "fincl-viz"
  :description "Plot fincl simulations as Plotly figures written as JSON."
  :depends-on ("fincl" "com.inuoe.jzon")
  :pathname "viz/"
  :serial t
  :components ((:file "package")
               (:file "json")
               (:file "figure")
               (:file "output")
               (:static-file "render.py")
               (:file "viz")
               (:file "greeks")
               (:file "calibration"))
  :in-order-to ((test-op (test-op "fincl-viz/tests"))))

(defsystem "fincl-viz/tests"
  :description "Tests for fincl-viz; not part of fincl's tests."
  :depends-on ("fincl-viz")
  :pathname "viz/"
  :components ((:file "tests"))
  :perform (test-op (o c)
             (unless (uiop:symbol-call '#:fincl-viz/tests '#:run-viz-tests)
               (error "fincl-viz tests failed."))))
