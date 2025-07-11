(defun plot-array (matrix &key
                            (title nil)
                            (xlabel nil)
                            (ylabel nil)
                            (noshow nil))
  (loop :with x-np := (np:array (magicl:lisp-array matrix))
        :with n := (car (magicl:shape matrix))
        :for i :from 0
          :below n
        :finally (if noshow nil (plt:show))
        :do
           (plt:plot (py4cl:chain x-np ([]  i (slice 0 -1))))
        :when title :do (plt:title title)
          :when xlabel :do (plt:xlabel xlabel)
            :when ylabel :do (plt:ylabel ylabel)))

(defun approx-gbm-option (sims steps)
  (mc-option-pricer
   (make-instance 'european-option :K 110 :tte 0.1 :callput 'call )
   100
   sims
   steps
   (make-instance 'GBM :sigma 0.2 :drift 0.00)))
