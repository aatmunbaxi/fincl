(ql:quickload :magicl)
(ql:quickload :py4cl)
(ql:quickload :alexandria)
(ql:quickload :special-functions )
(ql:quickload :random-state)
(ql:quickload :trivia)
(ql:quickload :lparallel)
(py4cl:import-module "numpy" :as "np")
(py4cl:import-module "matplotlib.pyplot" :as "plt")
(py4cl:import-function "slice")


;; (setf op (make-instance 'european-option :K 110 :tte 1))
;; (setf gbm (make-instance 'GBM :sigma 0.2))
(defun gen-dists (dists per-dist)
  (magicl:map! #'fincl.utils:quantile
               (magicl:from-list
                (loop :with v := (random-state:make-generator :quasi nil )
                      :repeat (* dists per-dist)
                      :collect (coerce   (random-state:next-byte v) 'double-float))
                `(,dists ,per-dist)
                :type 'double-float)))


(defun plot-hist (array &key
                          (show t))
  (plt:hist (np:array array ) :bins 50)
  (when show
    (plt:show)))

(defun plot-array (matrix &key
                            (title nil)
                            (xlabel nil)
                            (ylabel nil)
                            (show t))
  (loop :with x-np := (np:array (magicl:lisp-array matrix))
        :with n := (car (magicl:shape matrix))
        :for i :from 0
          :below n
        :finally (when show (plt:show))
        :do
           (plt:plot (py4cl:chain x-np ([]  i (slice 0 -1))))
        :when title :do (plt:title title)
          :when xlabel :do (plt:xlabel xlabel)
            :when ylabel :do (plt:ylabel ylabel)))
