(in-package #:fincl-plot)

(py4cl:import-module "numpy" :as "np")
(py4cl:import-module "matplotlib.pyplot" :as "plt")
(py4cl:import-function "slice")

(defun plot-array (matrix &key
                            (title nil)
                            (xlabel nil)
                            (ylabel nil)
                            (show t))
  (loop :with x-np := (np:array (magicl:lisp-array matrix))
        :with n := (car (magicl:shape matrix))
        :for i :from 0
          :below n
        :finally (if show  (plt:show) nil)
        :do
           (plt:plot (py4cl:chain x-np ([]  i (slice 0 -1))))
        :when title :do (plt:title title)
          :when xlabel :do (plt:xlabel xlabel)
            :when ylabel :do (plt:ylabel ylabel)))
