(in-package #:fincl)

(deftype option-type () '(member :call :put))

(defclass option ()
  ((type
    :accessor callput
    :type option-type
    :initarg :callput
    :initform :call)
   (strike
    :accessor strike
    :initarg :K
    :initform (error "Must provide a strike"))
   (time-to-expiration
    :accessor tte
    :initarg :time-to-expiration
    :initform 1)))


(defgeneric mc-option-pricer (op spot sims steps process &key threads)
  (:documentation "Monte-Carlo pricer for options.")
  (:method ((op european-option) spot sims steps process &key (threads nil))
    (declare (type fixnum sims steps))
    (declare (type stochastic-process  process))
    (let* ((paths (gen-paths process spot (tte op) sims steps :threads threads)))
      (trivia:match (callput op)
        (:call
         (* (exp (* (- 0 *risk-free-rate*) (tte op)))
            (.mean
             (magicl:.max 0 (magicl:.- (magicl:column paths steps) (strike op))))))
        (:put
         (* (exp (* (- 0 *risk-free-rate*) (tte op)))
            (.mean
             (magicl:.max 0 (magicl:.- (strike op) (magicl:column paths steps))))))))))


(defgeneric binomial-option-pricer (op spot  trials up down))

(defgeneric analytic-option-pricer (op spot process)
  (:documentation "Analytic option pricer for option.")
  (:method ((op european-option) spot (process GBM))
    (let* ((tau (tte op))
           (sig (sigma process))
           (discount (exp (* (- 0 *risk-free-rate*) tau)))
           (d+ (*
                (/ 1 (* sig (sqrt tau)))
                (+ (log (/ spot (strike op))) (+ *risk-free-rate* (* (/ (expt sig 2) 2) tau)))))
           (d- (- d+ (* sig (sqrt tau)))))

      (trivia:match (callput op)
        (:call
         (- (* (cdf d+) spot) (* (cdf d-) (strike op) discount)))
        (:put
         (- (* (cdf (- 0  d-)) spot discount) (* (cdf (- 0 d+)) (strike op)))))))

  (:method (op spot process)
    (error "Analytic option pricing only available for European options with GBM process.")))
