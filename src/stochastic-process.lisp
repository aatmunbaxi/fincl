(defclass stochastic-process ()
  ((drift
    :accessor drift
    :initarg :drift
    :initform 0)) )

(defmethod expected-value ((process stochastic-process) initial tests)
  (let* ((paths (gen-paths process initial 1 tests 1)))
    (/ (magicl:sum (magicl:column paths 1)) tests)))

(defgeneric gen-paths (process
                       init-value
                       time-length
                       num-paths
                       num-steps)
  (:documentation "Generate paths with PROCESS using Euler-Maruyama discretization scheme with optional drift."))
