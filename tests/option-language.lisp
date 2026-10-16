;;;; option-language.lisp --- the option construction language
;;;;
;;;; The standard clauses, defaults, derived Bermudan schedules, diagnostics,
;;;; printing and round trip; and extensions defined here, outside fincl: a
;;;; clause for the hand-written DIGITAL-PAYOFF (instruments.lisp) and a
;;;; payoff with a new fact.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Extensions
;;; --------------------------------------------------------------------

;;; A clause for a payoff class that is not a DEFINE-PAYOFF formula.
(define-clause option digital (level)
  (:publishes :payoff-class :payoff-initargs)
  "Pays 1 above LEVEL."
  (publish :payoff-class 'digital-payoff)
  (publish :payoff-initargs `(:level ,(float level 1d0))))

;;; A new fact, a clause publishing it, and a payoff slot consuming it.
(define-fact :rebate double-float "Paid when the option expires worthless."
  :coerce #'real-to-double)

(define-clause option rebate (amount)
  (:publishes :rebate)
  "Pay AMOUNT if the option expires worthless."
  (publish :rebate amount))

(define-payoff rebated-call (strike-payoff)
    ((rebate double-float :fact :rebate :doc "Paid when out of the money."))
  (s)
  (let ((intrinsic (* phi (- s strike))))
    (if (plusp intrinsic) intrinsic rebate)))

;;; --------------------------------------------------------------------
;;; Checks
;;; --------------------------------------------------------------------

(defun expiry-of (option) (expiry (option-exercise option)))

(defun dates= (expected actual)
  (and (= (length expected) (length actual)) (every #'date= expected actual)))

(defun option-language-checks ()
  (format t "~&Option language: clauses and defaults~%")
  (let* ((d #D"2027-09-24")
         (k 95)
         (call (make-option (call 100) (expires d)))
         (aput (make-option (put k) (american) (expires d))))
    (check-that "(call 100) (expires d) is a European vanilla call"
                (and (typep (option-exercise call) 'european-exercise)
                     (typep (option-payoff call) 'vanilla-payoff)
                     (eq (kind (option-payoff call)) :call)
                     (= (strike (option-payoff call)) 100d0)
                     (date= (expiry-of call) d)
                     (typep (option-path call) 'path-independent)))
    (check-that "clause arguments are evaluated: (put k) (american)"
                (and (typep (option-exercise aput) 'american-exercise)
                     (= (strike (option-payoff aput)) 95d0)))
    (check-that "(european) is the default, and saying it changes nothing"
                (equalp (object-clauses 'option call)
                        (object-clauses 'option (make-option (european) (call 100) (expires d)))))
    (check-that "the macro and the data function build the same option"
                (equalp (object-clauses 'option aput)
                        (object-clauses 'option (make-option-from-clauses `((:put ,k) (:american) (:expires ,d))))))
    (check-that "make-vanilla-option is the same option"
                (equalp (object-clauses 'option aput)
                        (object-clauses 'option (make-vanilla-option :put 95 d :exercise :american))))
    (check-that "a define-payoff class is a clause: (capped-call :kind :call ...)"
                (let ((o (make-option (capped-call :kind :call :strike 100 :cap 30) (expires d))))
                  (and (typep (option-payoff o) 'capped-call)
                       (= 30d0 (payoff-value (option-payoff o) 200d0)))))
    (check-that "(call k) feeds another struck payoff: (call 100) (capped-call :cap 30)"
                (let ((o (make-option (call 100) (capped-call :cap 30) (expires d))))
                  (and (typep (option-payoff o) 'capped-call)
                       (= 100d0 (strike (option-payoff o)))
                       (equalp (object-clauses 'option o)
                               `((:call 100d0) (:capped-call :cap 30d0) (:expires ,d))))))
    (check-that "a struck payoff with its own kind and strike needs no (call k)"
                (equalp (object-clauses 'option (make-option (capped-call :kind :put :strike 90 :cap 5)
                                                        (expires d)))
                        `((:put 90d0) (:capped-call :cap 5d0) (:expires ,d))))
    (check-that "(payoff class initargs...) names any payoff class"
                (typep (option-payoff (make-option (payoff 'capped-call :kind :put :strike 90 :cap 5)
                                              (expires d)))
                       'capped-call))
    (check "a digital built from its clause prices at expiry"
           1d0 (price (make-option (digital 95) (expires *today*)) (make-instance 'black-scholes-engine)
                      (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
           0))

  (format t "~&Option language: Bermudan schedules~%")
  (let ((weekends (make-instance 'weekends-only))
        (d1 #D"2027-03-24") (d2 #D"2027-09-24"))
    (check-that "the expiry is the last explicit date, given in any order"
                (let ((o (make-option (put 95) (bermudan :dates (list d2 d1)))))
                  (and (date= (expiry-of o) d2)
                       (dates= (list d1 d2) (exercise-dates (option-exercise o))))))
    (check-that "an explicit expiry equal to the last date is accepted"
                (date= d2 (expiry-of (make-option (put 95) (bermudan :dates (list d1 d2)) (expires d2)))))
    ;; Back from Monday 2027-11-29 in steps of 3 months: 2027-08-29 is a
    ;; Sunday, 2027-05-29 a Saturday, 2027-02-28 and 2026-11-29 Sundays.
    (check-equal ":every rolls back from the expiry, strictly after :from"
                 (list #D"2026-11-29" #D"2027-02-28" #D"2027-05-29" #D"2027-08-29" #D"2027-11-29")
                 (exercise-dates (option-exercise
                                  (make-option (put 95) (bermudan :every #T"3M" :from *today*)
                                          (expires #D"2027-11-29"))))
                 :test #'dates=)
    (check-equal ":every with a calendar moves each date but the expiry onto a business day"
                 (list #D"2026-11-30" #D"2027-03-01" #D"2027-05-31" #D"2027-08-30" #D"2027-11-29")
                 (exercise-dates (option-exercise
                                  (make-option (put 95) (bermudan :every #T"3M" :from *today*
                                                             :calendar weekends)
                                          (expires #D"2027-11-29"))))
                 :test #'dates=)
    (check-equal ":every with a tenor expiry"
                 (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24")
                 (exercise-dates (option-exercise
                                  (make-option (put 95) (bermudan :every #T"3M" :from *today*)
                                          (expires #T"1Y" :from *today*))))
                 :test #'dates=))

  (format t "~&Option language: diagnostics~%")
  (let ((d #D"2027-09-24"))
    (let ((c (condition-of (lambda () (make-option-from-clauses `((:expires ,d)))))))
      (check-that "with no payoff clause the vanilla payoff asks for (call) or (put)"
                  (and (typep c 'missing-fact)
                       (every (lambda (s) (search s (princ-to-string c)))
                              '("VANILLA-PAYOFF" "(call)" "(put)")))))
    (let ((c (condition-of (lambda () (make-option (call 100))))))
      (check-that "a missing expiry names the exercise slot and (expires)"
                  (and (typep c 'missing-fact)
                       (search "(expires)" (princ-to-string c))
                       (search "expiry" (princ-to-string c)))))
    (let ((c (condition-of (lambda () (make-option (put 95) (bermudan :every #T"3M" :from *today*))))))
      (check-that ":every with no expiry reports the schedule/expiry cycle and how to break it"
                  (and (typep c 'fact-rule-cycle) (search "(expires)" (princ-to-string c)))))
    (check-error "two exercise styles conflict"
                 (make-option (call 100) (american) (bermudan :dates (list d)))
                 conflicting-clauses)
    (check-error "a call and a put conflict" (make-option (call 100) (put 100) (expires d))
                 conflicting-clauses)
    (check-error "two payoff clauses conflict"
                 (make-option (call 100) (capped-call :cap 5) (digital 95) (expires d))
                 conflicting-clauses)
    (check-that "a strike nothing reads warns: (call 100) with (digital 95)"
                (let ((warned nil))
                  (handler-bind ((unused-fact (lambda (w)
                                                (setf warned (search "(call 100)" (princ-to-string w)))
                                                (muffle-warning w))))
                    (make-option (call 100) (digital 95) (expires d)))
                  warned))
    (let ((c (condition-of (lambda ()
                             (make-option (put 95) (bermudan :dates (list #D"2027-03-24" d))
                                     (expires #D"2027-06-24"))))))
      (check-that "explicit Bermudan dates and a different expiry conflict, naming both"
                  (and (typep c 'conflicting-clauses)
                       (search "(bermudan" (princ-to-string c))
                       (search "(expires" (princ-to-string c)))))
    (check-error "(bermudan) needs :dates or :every" (make-option (put 95) (bermudan) (expires d))
                 clause-argument-error)
    (check-error "(bermudan :every ...) needs :from"
                 (make-option (put 95) (bermudan :every #T"3M") (expires d))
                 clause-argument-error)
    (check-that "a strike of the wrong type names its clause"
                (let ((c (condition-of (lambda () (make-option (call "100") (expires d))))))
                  (and (typep c 'fact-type-error) (search "(call \"100\")" (princ-to-string c)))))
    (check-that "an expiry must be a date or a tenor"
                (search "not a date" (error-message (lambda () (make-option (call 100) (expires 1))))))
    (check-that "an unknown clause is a macroexpansion error suggesting the nearest"
                (let ((c (condition-of (lambda ()
                                         (macroexpand-1 '(make-option (cal 100) (expires d)))))))
                  (and (typep c 'unknown-clause) (search "call?" (princ-to-string c)))))
    (check-error "an unknown clause through the data function"
                 (make-option-from-clauses '((:call 100) (:expiers #D"2027-09-24")))
                 unknown-clause)
    (check-that "a fact nothing reads warns: (rebate 5) on a vanilla call"
                (let ((warned nil))
                  (handler-bind ((unused-fact (lambda (w)
                                                (setf warned (search "(rebate 5)" (princ-to-string w)))
                                                (muffle-warning w))))
                    (make-option (call 100) (rebate 5) (expires d)))
                  warned)))

  (format t "~&Option language: extension from outside fincl~%")
  (let* ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (o (make-option (rebated-call :kind :call :strike 100) (rebate 5) (expires #D"2027-09-24"))))
    (check-that "(rebate 5) fills the payoff's :fact slot"
                (and (typep (option-payoff o) 'rebated-call)
                     (= 5d0 (payoff-value (option-payoff o) 90d0))
                     (= 20d0 (payoff-value (option-payoff o) 120d0))))
    (check-that "a rebated call missing its rebate names (rebate)"
                (search "(rebate)" (error-message
                                    (lambda () (make-option (rebated-call :kind :call :strike 100)
                                                       (expires #D"2027-09-24"))))))
    (multiple-value-bind (v se) (price o (mc :n-paths 50000) m)
      (check "a rebated call = vanilla + discounted rebate x P(S_T <= K), by Monte Carlo"
             (+ (price (make-option (call 100) (expires #D"2027-09-24"))
                       (make-instance 'black-scholes-engine) m)
                (* 5 (exp -0.05d0) (norm-cdf (- (- (/ (+ (log (/ 100 100d0)) 0.05d0) 0.2d0) 0.1d0)))))
             v (* 4 se))))

  (format t "~&Option language: printing~%")
  (let ((d #D"2027-09-24"))
    (dolist (o (list (make-option (call 100) (expires d))
                     (make-option (put 95) (american) (expires d))
                     (make-option (put 95) (bermudan :every #T"3M" :from *today*) (expires d))
                     (make-option (capped-call :kind :call :strike 100 :cap 30) (expires d))
                     (make-option (put 90) (capped-call :cap 30) (american) (expires d))
                     (make-option (rebated-call :kind :call :strike 100) (rebate 5) (expires d))
                     (make-option (digital 95) (expires d))))
      (check-that (format nil "~A round-trips" o) (round-trip-p 'option o))
      (check-that (format nil "~A reads back" o)
                  (equalp (object-clauses 'option o)
                          (object-clauses
                           'option
                           (eval (let ((*readtable* (named-readtables:find-readtable 'fincl:syntax))
                                       (*package* (find-package '#:fincl/tests)))
                                   (read-from-string
                                    (let ((*print-readably* t)) (prin1-to-string o)))))))))
    (check-equal "a Bermudan prints its dates and no separate expiry"
                 "#<OPTION (put 95.0d0) (bermudan :dates (#D\"2027-03-24\" #D\"2027-09-24\"))>"
                 (let ((*package* (find-package '#:fincl)))
                   (prin1-to-string (make-option (put 95) (bermudan :dates (list #D"2027-03-24" d)))))
                 :test #'string=)))

(defun option-round-trip-checks ()
  (format t "~&Option language: every invariant-grid option rebuilds from its clauses~%")
  (let ((market (invariant-market))
        (gbm (make-instance 'gbm))
        (mismatches 0)
        (count 0))
    (dolist (class (engine-classes))
      (let ((engine (and (has-test-instance-p class)
                         (test-engine-instance (class-name class) gbm))))
        (when engine
          (dolist (style *invariant-styles*)
            (dolist (kind '(:call :put))
              (dolist (strike *invariant-strikes*)
                (let* ((o (invariant-option style kind strike))
                       (rebuilt (make-option-from-clauses (object-clauses 'option o))))
                  (incf count)
                  (unless (and (round-trip-p 'option o)
                               (equal (price-or-status o engine market)
                                      (price-or-status rebuilt engine market)))
                    (incf mismatches)
                    (format t "~&    ~(~A~) ~A~%" (class-name class) o)))))))))
    (check-that (format nil "~D rebuilt options price = to the originals under every engine" count)
                (zerop mismatches))))
