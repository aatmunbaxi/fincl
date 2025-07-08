(defclass ornstein-uhlenbeck (stochastic-process)
  ((mean-reversion
    :initarg mean-reversion
    :initform 0
    :accessor mean-reversion)
   (mean
    :accessor mean
    :initform (error "No long term mean provided.")
    :initarg :mean)
   (volatility
    :initarg :sigma
    :initform (error "No volatility provided.")
    :accessor volatility)))

(defmethod gen-paths ((proc ornstein-uhlenbeck)
                      init-value
                      time-length
                      num-paths
                      num-steps)
  (loop :with W := (magicl:random-normal `(,num-paths ,num-steps))
        :with s0 := (magicl:const init-value `(,num-paths 1) :type 'double-float)
        :with paths := `(,s0)
        :with dt := (/ time-length num-steps)
        :with variance := (magicl:const 0.2 `(,num-paths 1))
        :for i :from 0 :below num-steps
        :finally (return (magicl:hstack (reverse paths)))
        :do
           (magicl:.+ variance
                      (magicl:.*
                       (nu process)
                       (magicl:.* (sqrt dt)
                                  (magicl:.*
                                   variance
                                   (magicl:column Wprime i)))) variance)
           (push (magicl:.+ (magicl:.* (1+ (* dt (drift process))) (car paths))
                            (magicl:.*
                             (magicl:.* variance
                              (magicl:.^ (car paths) (beta process)))
                             (magicl:.* (sqrt dt) (magicl:column W i)))) paths)))
