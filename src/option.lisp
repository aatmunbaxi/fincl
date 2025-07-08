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


(defgeneric mc-pricer (op spot sims steps proc)
  (:documentation "Monte-Carlo pricer for option."))
