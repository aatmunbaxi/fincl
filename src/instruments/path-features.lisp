;;;; instruments/path-features.lisp --- Asian, barrier and lookback options
;;;;
;;;; A path feature is what a payoff depends on beside the terminal value.
;;;; It is a class, so engines dispatch on it, and a clause of the option
;;;; language:
;;;;
;;;;   (make-option (call 100) (average :arithmetic :dates ds) (expires d))
;;;;   (make-option (call 100) (average :geometric :every #T"1M" :from today) (expires d))
;;;;   (make-option (call 100) (barrier :down-and-out 90 :rebate 2) (expires d))
;;;;   (make-option (put 100) (barrier :up-and-in 120 :monitoring dates) (expires d))
;;;;   (make-option (call 100) (lookback :floating) (expires d))
;;;;
;;;; Observation dates are given (:DATES, :MONITORING) or generated EVERY
;;;; tenor back from the expiry, strictly after FROM, as Bermudan dates are.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Classes
;;; --------------------------------------------------------------------

(defclass observed-path (path-feature) ()
  (:metaclass component-class)
  (:documentation "A path feature that observes the observable on
OBSERVATION-DATES (or continuously, when they are NIL)."))

(defclass asian (observed-path)
  ((average :initarg :average :reader asian-average :type (member :arithmetic :geometric)
            :initform :arithmetic :fact :average-kind)
   (dates :initarg :dates :reader observation-dates :fact :observation-dates
          :initform (a:required-argument :dates))
   (strike-type :initarg :strike-type :reader strike-type :type (member :fixed :floating)
                :initform :fixed :fact :strike-type))
  (:metaclass component-class)
  (:documentation "The payoff is applied to the average of the observable on
DATES (arithmetic or geometric, equal weights). A :FIXED strike pays the
payoff of the average; a :FLOATING strike pays max(phi (S_T - A), 0)."))

(defclass barrier (observed-path)
  ((direction :initarg :direction :reader barrier-direction :type (member :up :down)
              :fact :barrier-direction :initform (a:required-argument :direction))
   (knock :initarg :knock :reader barrier-knock :type (member :in :out)
          :fact :barrier-knock :initform (a:required-argument :knock))
   (level :initarg :level :reader barrier-level :type double-float :fact :barrier-level
          :initform (a:required-argument :level))
   (rebate :initarg :rebate :reader barrier-rebate :type double-float :fact :rebate
           :initform 0d0)
   (dates :initarg :dates :reader observation-dates :initform nil :fact :observation-dates
          :documentation "Monitoring dates, or NIL for continuous monitoring."))
  (:metaclass component-class)
  (:documentation "A single barrier at LEVEL. A knock-out pays nothing once
the observable has touched it and pays REBATE when it does; a knock-in pays
only if it has, and REBATE at expiry otherwise. Monitored continuously, or on
DATES."))

(defclass lookback (observed-path)
  ((strike-type :initarg :strike-type :reader strike-type :type (member :fixed :floating)
                :initform :floating :fact :strike-type)
   (dates :initarg :dates :reader observation-dates :initform nil :fact :observation-dates
          :documentation "Observation dates, or NIL for every simulation step."))
  (:metaclass component-class)
  (:documentation "The extremum of the observable: a :FLOATING strike pays
phi (S_T - extremum), the minimum for a call and the maximum for a put; a
:FIXED strike pays the payoff of the maximum (call) or minimum (put)."))

(defun barrier-type (barrier)
  "The barrier's direction and knock as one keyword, e.g. :DOWN-AND-OUT."
  (a:format-symbol :keyword "~A-AND-~A" (barrier-direction barrier) (barrier-knock barrier)))

(defmethod initialize-instance :after ((b barrier) &key)
  (check-input (slot-value b 'direction) (member :up :down) ":UP or :DOWN")
  (check-input (slot-value b 'knock) (member :in :out) ":IN or :OUT")
  (check-input (slot-value b 'level) (real (0)) "a positive barrier level")
  (check-input (slot-value b 'rebate) (real 0) "a non-negative rebate")
  (setf (slot-value b 'level) (float (slot-value b 'level) 1d0)
        (slot-value b 'rebate) (float (slot-value b 'rebate) 1d0)))

(define-condition past-observation (pricing-error)
  ((path :initarg :path :reader error-path)
   (date :initarg :date :reader error-date)
   (valuation-date :initarg :valuation-date :reader error-valuation-date))
  (:report (lambda (c s)
             (format s "~A observes ~A, on or before the valuation date ~A; fixings ~
already observed are not supported. Price before the first observation date."
                     (class-name (class-of (error-path c))) (error-date c)
                     (error-valuation-date c))))
  (:documentation "A path feature has an observation date the market has
already passed."))

(defun floating-p (f) (eq (strike-type f) :floating))

(defmethod check-path-feature ((f observed-path) payoff exercise)
  (let ((expiry (expiry exercise)))
    (a:when-let ((late (find-if (lambda (d) (date< expiry d)) (observation-dates f))))
      (signal-invalid-argument 'dates late 'date
                               (format nil "an observation date on or before the expiry ~A" expiry))))
  (unless (typep exercise 'european-exercise)
    (signal-invalid-argument 'exercise exercise 'european-exercise
                             "European exercise: path features are European only"))
  (when (and (or (typep f 'lookback) (and (typep f 'asian) (floating-p f)))
             (not (typep payoff 'vanilla-payoff)))
    (signal-invalid-argument 'payoff payoff 'vanilla-payoff
                             (format nil "a call or put: a ~(~A~) uses only the payoff's kind"
                                     (if (typep f 'lookback) "lookback" "floating-strike Asian")))))

(defmethod validate-path-feature ((f observed-path) market)
  (let ((today (valuation-date market)))
    (a:when-let ((past (find-if (lambda (d) (date<= d today)) (observation-dates f))))
      (error 'past-observation :path f :date past :valuation-date today))))

(defmethod path-mandatory-dates ((f observed-path)) (observation-dates f))

;;; --------------------------------------------------------------------
;;; The option language
;;; --------------------------------------------------------------------

(define-fact :average-kind (member :arithmetic :geometric) "Arithmetic or geometric averaging.")
(define-fact :strike-type (member :fixed :floating) "A fixed strike, or one set by the path.")
(define-fact :barrier-direction (member :up :down) "Whether the barrier is above or below the spot.")
(define-fact :barrier-knock (member :in :out) "Whether touching the barrier starts or ends the option.")
(define-fact :barrier-level double-float "The barrier." :coerce #'real-to-double)
(define-fact :rebate double-float "Paid when a knock-out barrier is touched, or at expiry
when a knock-in barrier never is." :coerce #'real-to-double)
(define-fact :observation-dates (satisfies date-list-p) "Dates a path feature observes, sorted."
  :coerce #'sorted-dates)
(define-fact :observation-tenor tenor "Spacing of generated observation dates.")
(define-fact :observation-from date "Generated observation dates fall strictly after this date.")

(defun publish-observation-dates (clause dates every from)
  "Publish DATES, or the tenor EVERY and start FROM the rule generates them from."
  (cond ((and dates every)
         (error 'clause-argument-error
                :format-control "(~(~A~)) takes either dates or :EVERY, not both."
                :format-arguments `(,clause)))
        (dates (publish :observation-dates dates))
        (every
         (unless from
           (error 'clause-argument-error
                  :format-control "(~(~A~) ... :every ~S) needs :FROM, the date the observations start after."
                  :format-arguments `(,clause ,every)))
         (publish :observation-tenor every)
         (publish :observation-from from))))

(define-fact-rule option :observation-dates ((tenor :observation-tenor) (from :observation-from)
                                             (expiry :expiry))
  (bermudan-schedule-dates expiry tenor from nil :following nil))

(define-clause option average (kind &key dates every from (strike-type :fixed))
  (:publishes :path-kind :average-kind :strike-type :observation-dates :observation-tenor
              :observation-from)
  "An Asian option averaging the observable (KIND :ARITHMETIC or :GEOMETRIC)
on DATES, or on dates EVERY tenor back from the expiry, strictly after FROM.
STRIKE-TYPE is :FIXED (the payoff of the average) or :FLOATING."
  (publish :path-kind :asian)
  (publish :average-kind kind)
  (publish :strike-type strike-type)
  (unless (or dates every)
    (error 'clause-argument-error :format-control "(average ~S) needs :DATES or :EVERY and :FROM."
                                  :format-arguments `(,kind)))
  (publish-observation-dates 'average dates every from))

(define-clause option barrier (type level &key (rebate 0) (monitoring :continuous) every from)
  (:publishes :path-kind :barrier-direction :barrier-knock :barrier-level :rebate :observation-dates
              :observation-tenor :observation-from)
  "A barrier option: TYPE :DOWN-AND-OUT, :DOWN-AND-IN, :UP-AND-OUT or
:UP-AND-IN at LEVEL, with REBATE. MONITORING is :CONTINUOUS or a list of
dates; or monitor on dates EVERY tenor back from the expiry after FROM."
  (publish :path-kind :barrier)
  (destructuring-bind (direction knock)
      (or (cdr (assoc type '((:down-and-out :down :out) (:down-and-in :down :in)
                             (:up-and-out :up :out) (:up-and-in :up :in))))
          (error 'clause-argument-error
                 :format-control "(barrier ~S ...): the type is one of :DOWN-AND-OUT, ~
:DOWN-AND-IN, :UP-AND-OUT or :UP-AND-IN."
                 :format-arguments `(,type)))
    (publish :barrier-direction direction)
    (publish :barrier-knock knock))
  (publish :barrier-level level)
  (publish :rebate rebate)
  (publish-observation-dates 'barrier (unless (eq monitoring :continuous) monitoring) every from))

(define-clause option lookback (strike-type &key monitoring every from)
  (:publishes :path-kind :strike-type :observation-dates :observation-tenor :observation-from)
  "A lookback: STRIKE-TYPE :FLOATING or :FIXED, observed on every simulation
step, or on MONITORING dates, or on dates EVERY tenor back from the expiry."
  (publish :path-kind :lookback)
  (publish :strike-type strike-type)
  (publish-observation-dates 'lookback monitoring every from))

(defmethod assemble-path-feature ((kind (eql :asian))) (make-from-facts 'asian))
(defmethod assemble-path-feature ((kind (eql :barrier))) (make-from-facts 'barrier))
(defmethod assemble-path-feature ((kind (eql :lookback))) (make-from-facts 'lookback))

(defmethod option-component-clauses ((f asian))
  `((:average ,(asian-average f) :dates ,(observation-dates f) :strike-type ,(strike-type f))))

(defmethod option-component-clauses ((f barrier))
  `((:barrier ,(barrier-type f) ,(barrier-level f) :rebate ,(barrier-rebate f)
              :monitoring ,(or (observation-dates f) :continuous))))

(defmethod option-component-clauses ((f lookback))
  `((:lookback ,(strike-type f) ,@(when (observation-dates f) `(:monitoring ,(observation-dates f))))))
