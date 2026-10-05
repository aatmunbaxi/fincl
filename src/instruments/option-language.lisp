;;;; instruments/option-language.lisp --- building options from clauses
;;;;
;;;; The option construction language, on the general kernel in
;;;; src/construction/. This file is vocabulary and assembly only: facts,
;;;; clauses, rules, how an option is assembled from facts, and how one
;;;; prints back as clauses.
;;;;
;;;;   (option (call 100) (expires #D"2027-09-24"))                ; European
;;;;   (option (put k) (american) (expires #T"1Y" :from *today*))
;;;;   (option (put 95) (bermudan :dates (list d1 d2 d3)))         ; expiry = d3
;;;;   (option (put 95) (bermudan :every #T"3M" :from *today*) (expires d))
;;;;   (option (call 100) (capped-call :cap 30) (expires d))
;;;;
;;;; With no exercise clause an option is European, with no payoff clause its
;;;; payoff is vanilla, and with no path clause it is path-independent: the
;;;; option Black-Scholes prices. (call k) and (put k) give only the kind and
;;;; strike, so they also feed other struck payoffs, as above. The
;;;; expiry has no default.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Facts
;;; --------------------------------------------------------------------

(defun date-list-p (x) (and (listp x) x (every #'datep x)))
(defun sorted-dates (x) (if (date-list-p x) (sort (copy-list x) #'date<) x))
(defun payoff-class-name-p (x)
  (and (symbolp x) (find-class x nil) (subtypep x 'payoff)))

(define-fact :kind (member :call :put) "Call or put.")
(define-fact :strike double-float "The strike." :coerce #'real-to-double)
(define-fact :expiry date "The last exercise date.")
(define-fact :exercise-style (member :european :american :bermudan) "The exercise rights.")
(define-fact :exercise-dates (satisfies date-list-p) "Bermudan exercise dates, sorted."
  :coerce #'sorted-dates)
(define-fact :exercise-tenor tenor "Spacing of generated Bermudan dates.")
(define-fact :exercise-from date "Generated Bermudan dates fall strictly after this date.")
(define-fact :calendar calendar "The business-day calendar.")
(define-fact :roll-convention (member :unadjusted :following :modified-following
                                      :preceding :modified-preceding)
  "How a date that is not a business day moves (see ADJUST).")
(define-fact :end-of-month boolean "Whether month-end dates stay at month end.")
(define-fact :payoff-class (satisfies payoff-class-name-p) "The payoff's class name.")
(define-fact :payoff-initargs list "Initargs for the payoff, beside its :FACT slots.")
(define-fact :underlying keyword "The market underlying an instrument refers to.")
(define-fact :currency keyword "The currency an instrument pays in.")

;;; --------------------------------------------------------------------
;;; The language
;;; --------------------------------------------------------------------

(defun resolve-payoff-clause (name)
  "A clause for the DEFINE-PAYOFF class called NAME, if there is one:
(capped-call :kind :call :strike 100 :cap 30)."
  (let ((classes (loop for class being the hash-keys of *formula-payoffs*
                       when (string= (symbol-name class) name) collect class)))
    (when (rest classes)
      (error 'ambiguous-clause :name name :language 'option :candidates classes))
    (let ((class (first classes)))
      (when class
        (make-clause-definition name '(&rest initargs) 0 nil '(:payoff-class :payoff-initargs)
                                (format nil "The payoff ~(~A~), with its initargs." class)
                                (lambda (&rest initargs)
                                  (publish :payoff-class class)
                                  (when initargs (publish :payoff-initargs initargs))))))))

(define-construction-language option
  (:documentation "Build an option from clauses: a kind and strike or a
payoff, an expiry, and optionally exercise rights. With no payoff clause the
payoff is vanilla; with no exercise clause the option is European.")
  (:macro option)
  (:function make-option)
  (:defaults :exercise-style :european :payoff-class 'vanilla-payoff)
  (:assemble assemble-option)
  (:resolver resolve-payoff-clause (:payoff-class :payoff-initargs)
   "(class :initarg value ...) for a payoff class defined with DEFINE-PAYOFF"))

;;; --------------------------------------------------------------------
;;; Clauses
;;; --------------------------------------------------------------------

(define-clause option call (strike)
  (:publishes :kind :strike)
  "A call struck at STRIKE: vanilla unless a payoff clause names another
struck payoff."
  (publish :kind :call)
  (publish :strike strike))

(define-clause option put (strike)
  (:publishes :kind :strike)
  "A put struck at STRIKE: vanilla unless a payoff clause names another
struck payoff."
  (publish :kind :put)
  (publish :strike strike))

(define-clause option payoff (class &rest initargs)
  (:publishes :payoff-class :payoff-initargs)
  "The payoff class CLASS (a symbol), with INITARGS. Its :FACT slots are filled
from facts too."
  (publish :payoff-class class)
  (when initargs (publish :payoff-initargs initargs)))

(define-clause option underlying (name)
  (:publishes :underlying)
  "The market underlying NAME, a keyword: (underlying :spx). Without it the
option prices on the market's only underlying."
  (publish :underlying name))

(define-clause option currency (name)
  (:publishes :currency)
  "The currency NAME the option pays in, which selects the discount curve.
Without it, the underlying's currency."
  (publish :currency name))

(define-clause option european ()
  (:publishes :exercise-style)
  "Exercise on the expiry only (the default)."
  (publish :exercise-style :european))

(define-clause option american ()
  (:publishes :exercise-style)
  "Exercise on any date up to the expiry."
  (publish :exercise-style :american))

(define-clause option bermudan (&key dates every from calendar (roll-convention :following)
                                     end-of-month)
  (:publishes :exercise-style :exercise-dates :exercise-tenor :exercise-from :calendar
              :roll-convention :end-of-month)
  "Exercise on given DATES (the last is the expiry), or on dates EVERY tenor
back from the expiry, strictly after FROM, moved onto business days of
CALENDAR with ROLL-CONVENTION."
  (publish :exercise-style :bermudan)
  (cond ((and dates (not every))
         (publish :exercise-dates dates))
        ((and every (not dates))
         (unless from
           (error 'clause-argument-error
                  :format-control "(bermudan :every ~S) needs :FROM, the date the schedule starts after."
                  :format-arguments `(,every)))
         (publish :exercise-tenor every)
         (publish :exercise-from from)
         (when calendar
           (publish :calendar calendar)
           (publish :roll-convention roll-convention))
         (when end-of-month (publish :end-of-month t)))
        (t (error 'clause-argument-error
                  :format-control "(bermudan) takes either :DATES or :EVERY, not ~:[neither~;both~]."
                  :format-arguments `(,(and dates every))))))

(define-clause option expires (when &key from calendar (roll-convention :following) end-of-month)
  (:publishes :expiry)
  "The expiry: a date, or a tenor from FROM, moved onto a business day of
CALENDAR with ROLL-CONVENTION (see RESOLVE-EXPIRY)."
  (publish :expiry (resolve-expiry when :from from :calendar calendar
                                        :convention roll-convention :end-of-month end-of-month)))

;;; --------------------------------------------------------------------
;;; Rules
;;; --------------------------------------------------------------------

(defun roll-back-schedule (expiry tenor from calendar roll-convention end-of-month)
  "Dates EXPIRY, EXPIRY - TENOR, EXPIRY - 2 TENOR, ... strictly after FROM,
earliest first. Every date but the expiry moves onto a business day of
CALENDAR, if given, and stays strictly between FROM and EXPIRY."
  (let ((dates `(,expiry)))
    (loop for k from 1
          for unadjusted = (add-tenor expiry (make-tenor (- (* k (tenor-n tenor))) (tenor-unit tenor))
                                      :end-of-month end-of-month)
          while (date< from unadjusted)
          do (let ((date (if calendar (adjust calendar unadjusted roll-convention) unadjusted)))
               (when (and (date< from date) (date< date expiry))
                 (push date dates))))
    (remove-duplicates dates :test #'date=)))

(define-fact-rule option :exercise-dates ((tenor :exercise-tenor) (from :exercise-from)
                                          (expiry :expiry)
                                          &optional (calendar :calendar nil)
                                          (roll-convention :roll-convention :following)
                                          (end-of-month :end-of-month nil))
  (roll-back-schedule expiry tenor from calendar roll-convention end-of-month))

(define-fact-rule option :expiry ((dates :exercise-dates))
  (a:lastcar dates))

;;; --------------------------------------------------------------------
;;; Assembly
;;; --------------------------------------------------------------------

(defun check-bermudan-expiry ()
  "Explicit Bermudan dates and an explicit expiry must agree."
  (multiple-value-bind (dates-source dates-priority) (fact-provenance :exercise-dates)
    (multiple-value-bind (expiry-source expiry-priority) (fact-provenance :expiry)
      (when (and (eq dates-priority :explicit) (eq expiry-priority :explicit)
                 (not (date= (a:lastcar (fact :exercise-dates)) (fact :expiry))))
        (error 'conflicting-clauses :fact :expiry :earlier dates-source :later expiry-source)))))

(defun assemble-option ()
  (let* ((payoff (apply #'make-from-facts (require-fact :payoff-class)
                        (values (fact :payoff-initargs))))
         (exercise (ecase (require-fact :exercise-style)
                     (:european (make-from-facts 'european-exercise))
                     (:american (make-from-facts 'american-exercise))
                     (:bermudan (check-bermudan-expiry)
                                (make-from-facts 'bermudan-exercise)))))
    (make-from-facts 'option :payoff payoff :exercise exercise)))

(defun make-vanilla-option (kind strike expiry &key (exercise :european) from calendar
                                                    (roll-convention :following) end-of-month
                                                    underlying)
  "A vanilla option: (KIND STRIKE) with EXERCISE (:EUROPEAN or :AMERICAN) and
the expiry clause (expires EXPIRY :from FROM ...).

  (make-vanilla-option :put 40 #D\"2027-09-24\" :exercise :american)"
  (make-option `((,kind ,strike) (,exercise)
                 (:expires ,expiry :from ,from :calendar ,calendar
                           :roll-convention ,roll-convention :end-of-month ,end-of-month)
                 ,@(when underlying `((:underlying ,underlying))))))

;;; --------------------------------------------------------------------
;;; Back to clauses
;;; --------------------------------------------------------------------

(defun slot-initargs (object)
  "Initargs that rebuild OBJECT's bound slots, in slot order."
  (loop for slot in (c2mop:class-slots (class-of object))
        for initarg = (first (c2mop:slot-definition-initargs slot))
        for name = (c2mop:slot-definition-name slot)
        when (and initarg (slot-boundp object name))
          append `(,initarg ,(slot-value object name))))

(defgeneric option-component-clauses (component)
  (:documentation "The option-language clauses that rebuild COMPONENT.")
  (:method ((p payoff))
    (let ((class (class-name (class-of p))))
      (if (gethash class *formula-payoffs*)
          `((,(a:make-keyword class) ,@(slot-initargs p)))
          `((:payoff ,class ,@(slot-initargs p))))))
  (:method ((p strike-payoff))
    ;; (call k) or (put k), then the payoff's own clause with the rest.
    `((,(kind p) ,(strike p))
      ,@(unless (typep p 'vanilla-payoff)
          (destructuring-bind ((head . arguments)) (call-next-method)
            (if (eq head :payoff)
                `((:payoff ,(first arguments)
                           ,@(a:remove-from-plist (rest arguments) :kind :strike)))
                `((,head ,@(a:remove-from-plist arguments :kind :strike))))))))
  (:method ((x european-exercise)) `((:expires ,(expiry x))))
  (:method ((x american-exercise)) `((:american) (:expires ,(expiry x))))
  (:method ((x bermudan-exercise)) `((:bermudan :dates ,(exercise-dates x))))
  (:method ((f path-independent)) '()))

(defmethod object-clauses ((language (eql 'option)) (o option))
  `(,@(when (instrument-underlying o) `((:underlying ,(instrument-underlying o))))
    ,@(when (instrument-currency o) `((:currency ,(instrument-currency o))))
    ,@(option-component-clauses (option-payoff o))
    ,@(option-component-clauses (option-exercise o))
    ,@(option-component-clauses (option-path o))))

(defmethod print-object ((o option) stream)
  (print-clause-form 'option o stream))
