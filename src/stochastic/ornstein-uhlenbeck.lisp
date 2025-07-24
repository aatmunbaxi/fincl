(defclass ornstein-uhlenbeck (stochastic-process)
  ((mean-reversion
    :initarg mean-reversion
    :initform 0
    :accessor mean-reversion)
   (mean
    :accessor long-term-mean
    :initform (error "No long term mean provided.")
    :initarg :mean)
   (volatility
    :initarg :sigma
    :initform (error "No volatility provided.")
    :accessor volatility)))

;; (defmethod gen-paths ((proc ornstein-uhlenbeck)
;;                       init-value
;;                       time-length
;;                       num-paths
;;                       num-steps)
;;   (with-threads-path-gen num-paths threads
;;     (logic .. here ..)))
