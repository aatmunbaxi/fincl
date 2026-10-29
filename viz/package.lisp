;;;; viz/package.lisp --- the fincl-viz package

(defpackage #:fincl-viz
  (:use #:cl #:fincl #:iterate)
  (:shadowing-import-from #:fincl #:minimize)
  (:local-nicknames (#:a #:alexandria) (#:jzon #:com.inuoe.jzon))
  (:export
   ;; Output
   #:*display* #:*output-directory* #:*python-command* #:*renderer*
   #:figure-json #:write-figure #:emit-figure #:render-failure
   ;; Building figures
   #:*theme* #:theme-color #:theme-colorscale #:merge-spec #:configure-figure
   #:line-trace #:scatter-trace #:bar-trace #:error-trace #:surface-trace
   #:vline #:hline #:matrix-traces #:histogram-density
   #:panel #:panel-grid #:camera-eye #:define-plot
   ;; Simulation
   #:uniform-draws #:simulate #:state-component #:engine-paths #:instantaneous-vol
   ;; Plots
   #:plot-series #:plot-array #:plot-panels
   #:plot-paths #:plot-engine-paths #:plot-terminal #:plot-log-returns #:plot-convergence
   #:series-figure #:array-figure #:panels-figure
   #:paths-figure #:engine-paths-figure #:terminal-figure #:log-returns-figure
   #:convergence-figure))
