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
   #:%panel #:%panel-grid #:camera-eye #:define-plot
   ;; Plot: layers, panels, grids
   #:plot #:plot-figure #:panel #:grid #:layer #:make-layer #:layer-traces
   #:paths #:mean-line #:band #:statistic #:histogram #:default-layers
   #:reference-line
   #:with-theme #:*roles* #:*max-plotted-paths*
   #:missing-plot-data #:incompatible-layers #:unknown-role #:too-many-paths
   ;; Plots of a process
   #:plot-paths #:plot-terminal #:plot-log-returns
   #:paths-figure #:terminal-figure #:log-returns-figure))
