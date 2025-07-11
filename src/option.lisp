(defclass option ()
  ((type
    :accessor callput
    :initarg :callput
    :initform 'call)
   (strike
    :accessor strike
    :initarg :K
    :initform (error "Must provide a strike"))
   (time-to-expiration
    :accessor tte
    :initarg :tte
    :initform 1)))
(deftype option-type () '(member 'call 'put))

(deftype pricing-engine () '(member 'analytic 'monte-carlo
                             'binomial-tree))
(defgeneric price (op spot engine process)
  (:documentation "Price an option."))

(defgeneric mc-option-pricer (op spot sims steps proc)
  (:documentation "Monte-Carlo pricer for option."))

(defgeneric binomial-option-pricer (op spot  trials up down))

(defgeneric analytic-option-pricer (op spot proc)
  (:documentation "Analytic option pricer for option."))
