;;;; instruments.lisp --- dates, expiries and exercise in pricing

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun dated-pricing-checks ()
  (let* ((bs (make-instance 'black-scholes-engine))
         (baw (make-instance 'barone-adesi-whaley-engine))
         (engines (list bs baw)))
    (format t "~&Convention invariance (ACT/365F vs rescaled ACT/360)~%")
    ;; Rates and variance rescaled by 360/365, so D, F and w coincide.
    (let* ((scale (/ 360d0 365d0))
           (m365 (make-bs-market :valuation-date *today* :spot 100
                                 :rate 0.05d0 :dividend 0.02d0 :vol 0.25d0))
           (m360 (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                 :spot 100 :rate (* 0.05d0 scale)
                                 :dividend (* 0.02d0 scale)
                                 :vol (* 0.25d0 (sqrt scale)))))
      (dolist (spec '((:call 100 #D"2027-09-24") (:put 95 #D"2027-03-10")
                      (:american :put 110 #D"2027-09-24")
                      (:american :call 90 #D"2028-01-15")))
        (let ((o (make-option spec)))
          (dolist (e engines)
            (unless (and (typep e 'black-scholes-engine)
                         (typep (option-exercise o) 'american-exercise))
              (check (format nil "~A ~(~A~)" o (class-name (class-of e)))
                     (price o e m365) (price o e m360) 1d-12))))))

    (format t "~&Aging~%")
    ;; Valued half a year later, the option is the same as a fresh one
    ;; with the same number of days left.
    (let* ((m0 (make-bs-market :valuation-date *today* :spot 100
                               :rate 0.05d0 :dividend 0.02d0 :vol 0.25d0))
           (later #D"2027-03-24")
           (aged (derive-market m0 :valuation-date later))
           (days-left (date- #D"2027-09-24" later)))
      (dolist (style '(:european :american))
        (let ((old (make-vanilla-option :put 105 #D"2027-09-24" :exercise style))
              (fresh (make-vanilla-option :put 105 (date+ *today* days-left)
                                          :exercise style)))
          (check (format nil "~(~A~) put aged to ~A" style later)
                 (price fresh (if (eq style :american) baw bs) m0)
                 (price old (if (eq style :american) baw bs) aged)
                 1d-12)))
      (check-that "derive-market leaves the original unchanged"
                  (date= (valuation-date m0) *today*)))

    (format t "~&Expiry~%")
    (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
      (dolist (e (list bs baw (mc :n-paths 1000)))
        (check (format nil "at expiry, ~(~A~) returns the payoff" (class-name (class-of e)))
               10d0
               (price (make-option `(:american :put 110 ,*today*)) e m)
               0))
      (let ((expired (make-option '(:put 110 #D"2026-09-23"))))
        (check-error "past expiry" (price expired bs m) option-expired)
        (check-that "option-expired report names both dates"
                    (let ((msg (error-message (lambda () (price expired bs m)))))
                      (and (search "2026-09-23" msg) (search "2026-09-24" msg))))))

    (format t "~&Expiries are dates~%")
    (check-error "bare-number expiry" (make-option '(:call 100 1)))
    (check-that "bare-number expiry error explains dates"
                (search "not a date" (error-message
                                      (lambda () (make-option '(:call 100 1))))))
    (check-error "bare-number expiry, make-vanilla-option"
                 (make-vanilla-option :call 100 0.5d0))

    (format t "~&Tenor expiries~%")
    (flet ((expiry-of (option) (expiry (option-exercise option))))
      (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
            (weekends (make-instance 'weekends-only)))
        (check-equal "1Y from 2026-09-24 is stored as a date" #D"2027-09-24"
                     (expiry-of (make-option '(:american :call 100 #T"1Y") :from *today*))
                     :test (lambda (e a) (and (typep a 'date) (date= e a))))
        (check-error "tenor without :from" (make-option '(:call 100 #T"1Y")))
        (check-that "tenor without :from error asks for a start date"
                    (search ":FROM" (error-message
                                     (lambda () (make-option '(:call 100 #T"1Y"))))))
        ;; 2026-09-26 is a Saturday, so 1Y later is Sunday 2027-09-26.
        (check-equal "unadjusted without a calendar" #D"2027-09-26"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"))
                     :test #'date=)
        (check-equal "calendar, following" #D"2027-09-27"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"
                                                                 :calendar weekends))
                     :test #'date=)
        (check-equal "calendar, preceding" #D"2027-09-24"
                     (expiry-of (make-option '(:call 100 #T"1Y") :from #D"2026-09-26"
                                                                 :calendar weekends
                                                                 :convention :preceding))
                     :test #'date=)
        (check-equal "end of month" #D"2027-03-31"
                     (expiry-of (make-option '(:call 100 #T"1M") :from #D"2027-02-28"
                                                                 :end-of-month t))
                     :test #'date=)
        (check-equal "make-vanilla-option with a tenor" #D"2027-03-24"
                     (expiry-of (make-vanilla-option :put 100 #T"6M" :from *today*
                                                     :exercise :american))
                     :test #'date=)
        (check "tenor-built option prices like the date-built one"
               (price (make-option '(:call 100 #D"2027-09-24")) bs m)
               (price (make-option '(:call 100 #T"1Y") :from *today*) bs m)
               0)
        (let ((o (make-option '(:call 100 #T"1Y") :from *today*)))
          (check-that "expiry does not move when the market ages"
                      (and (date= (expiry-of o) #D"2027-09-24")
                           (price o bs (derive-market m :valuation-date #D"2027-03-24"))
                           (date= (expiry-of o) #D"2027-09-24"))))
        (check-equal "a strike ladder from one tenor" 101
                     (length (loop :for strike :from 50 :upto 150
                                   :collect (make-option `(:american :call ,strike #T"1Y")
                                                         :from *today*))))))

    (format t "~&Exercise~%")
    (let ((b (make-instance 'bermudan-exercise
                            :dates (list #D"2027-09-24" #D"2027-03-24"))))
      (check-equal "bermudan expiry is the last date" #D"2027-09-24" (expiry b)
                   :test #'date=)
      (check-that "bermudan exercise-allowed-p"
                  (and (exercise-allowed-p b #D"2027-03-24")
                       (not (exercise-allowed-p b #D"2027-03-25"))))
      (check-that "european and american exercise-allowed-p"
                  (let ((eu (make-instance 'european-exercise :expiry #D"2027-09-24"))
                        (am (make-instance 'american-exercise :expiry #D"2027-09-24")))
                    (and (exercise-allowed-p eu #D"2027-09-24")
                         (not (exercise-allowed-p eu #D"2027-03-24"))
                         (exercise-allowed-p am #D"2027-03-24")
                         (not (exercise-allowed-p am #D"2027-09-25"))))))))

;;; --------------------------------------------------------------------
;;; The payoff protocol: nothing generic assumes a vanilla payoff
;;; --------------------------------------------------------------------

(defclass digital-payoff (payoff)
  ((level :initarg :level :reader digital-level))
  (:documentation "Pays 1 above LEVEL. Defined here, outside fincl, to show a
payoff with no kind and no strike works through the generic code."))

(defmethod payoff-value ((p digital-payoff) x)
  (if (> x (digital-level p)) 1d0 0d0))

(defmethod describe-payoff ((p digital-payoff) stream)
  (format stream "digital above ~,2F" (digital-level p)))

(defun payoff-protocol-checks ()
  (format t "~&Payoff protocol~%")
  (let* ((bs (make-instance 'black-scholes-engine))
         (m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (payoff (make-instance 'digital-payoff :level 95d0))
         (today (make-instance 'option :payoff payoff
                                       :exercise (make-instance 'european-exercise
                                                                :expiry *today*)))
         (later (make-instance 'option :payoff payoff
                                       :exercise (make-instance 'european-exercise
                                                                :expiry #D"2027-09-24"))))
    (check "a digital expiring today is worth its payoff" 1d0 (price today bs m) 0)
    (check-that "a digital option prints through describe-payoff"
                (search "digital above 95.00" (prin1-to-string later)))
    (check-error "a digital before expiry reaches the engine's dispatch"
                 (price later bs m) unsupported-combination)
    (check-equal "vanilla options print as before"
                 "#<OPTION american-exercise put K=40.00 #D\"2027-09-24\">"
                 (let ((*package* (find-package '#:fincl)))
                   (prin1-to-string (make-option '(:american :put 40 #D"2027-09-24"))))
                 :test #'string=)
    (check-that "a vanilla payoff is a call-put payoff"
                (typep (option-payoff (make-option '(:put 40 #D"2027-09-24")))
                       'call-put-payoff))
    (check-that "zero volatility is still refused for a vanilla"
                (search "Volatility must be positive"
                        (error-message
                         (lambda ()
                           (price (make-option '(:call 100 #D"2027-09-24")) bs
                                  (make-bs-market :valuation-date *today* :spot 100
                                                  :rate 0.05d0 :vol 0d0))))))
    (check-error "an option needs a payoff"
                 (make-instance 'option :exercise (make-instance 'european-exercise
                                                                 :expiry *today*)))
    (check-error "an option needs an exercise" (make-instance 'option :payoff payoff))
    (check-error "an option's payoff must be a payoff"
                 (make-instance 'option :payoff 3
                                        :exercise (make-instance 'european-exercise
                                                                 :expiry *today*))
                 type-error)))

;;; --------------------------------------------------------------------
;;; DEFINE-PAYOFF
;;; --------------------------------------------------------------------

;;; Inherits STRIKE and PHI from STRIKE-PAYOFF; only the cap is new. Not a
;;; subclass of VANILLA-PAYOFF, whose closed forms would misprice it.
(define-payoff capped-call (strike-payoff)
    ((cap double-float :doc "Maximum payout."))
  (s)
  (min cap (max 0d0 (* phi (- s strike)))))

(define-payoff cash-or-nothing (strike-payoff) ()
  (s)
  (if (plusp (* phi (- s strike))) 1d0 0d0))

(defun disassembly (function)
  (with-output-to-string (*standard-output*) (disassemble function)))

(defun calls-slot-readers-p (function)
  "True if FUNCTION's machine code calls KIND or STRIKE. SBCL annotates a
full call with the callee's name."
  (let ((code (disassembly function)))
    (or (search "STRIKE" code) (search "KIND" code))))

(defun define-payoff-checks ()
  (format t "~&DEFINE-PAYOFF~%")
  (let ((kernel (payoff-kernel (make-instance 'vanilla-payoff :kind :call :strike 100d0))))
    (check-that "the vanilla kernel makes no generic arithmetic call"
                (not (search "GENERIC" (disassembly kernel))))
    (check-that "the check sees a call to a slot reader when there is one"
                (calls-slot-readers-p (compile nil '(lambda (p) (strike p)))))
    (check-that "the vanilla kernel calls neither KIND nor STRIKE"
                (not (calls-slot-readers-p kernel))))
  (let* ((state (sb-ext:seed-random-state 7))
         (spots (loop repeat 1000 collect (random 250d0 state))))
    (dolist (kind '(:call :put))
      (let* ((p (make-instance 'capped-call :kind kind :strike 100d0 :cap 30d0))
             (kernel (payoff-kernel p))
             (phi (if (eq kind :call) 1d0 -1d0)))
        (check-that (format nil "capped ~(~A~): kernel = payoff-value = formula on 1000 spots" kind)
                    (every (lambda (s)
                             (= (funcall kernel s) (payoff-value p s)
                                (min 30d0 (max 0d0 (* phi (- s 100d0))))))
                           spots)))))
  (check-that "a capped call prices below the vanilla by Monte Carlo"
              (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
                    (capped (make-instance 'option
                                           :payoff (make-instance 'capped-call :kind :call
                                                                               :strike 100d0 :cap 10d0)
                                           :exercise (make-instance 'european-exercise
                                                                    :expiry #D"2027-09-24"))))
                (< (price capped (mc :n-paths 20000) m)
                   (price (make-option '(:call 100 #D"2027-09-24")) (mc :n-paths 20000) m))))
  (check-error "a define-payoff slot without an initform is required"
               (make-instance 'capped-call :kind :call :strike 100d0))
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
        (expiry (make-instance 'european-exercise :expiry #D"2027-09-24")))
    (check-error "a closed-form engine refuses a non-vanilla struck payoff"
                 (price (make-instance 'option :exercise expiry
                                               :payoff (make-instance 'capped-call :kind :call
                                                                                   :strike 100 :cap 1))
                        (make-instance 'black-scholes-engine) m)
                 unsupported-combination)
    (check-that "subclassing a payoff with its own formula warns at compile time"
                (handler-case
                    (progn (macroexpand-1 '(define-payoff bad (vanilla-payoff) () (s) s)) nil)
                  (formula-payoff-superclass () t)))
    ;; Cash-or-nothing: pays 1 if S_T is beyond the strike. Closed form
    ;; D N(phi d2) checks the generic Monte Carlo path.
    (dolist (kind '(:call :put))
      (let* ((phi (if (eq kind :call) 1 -1))
             (payoff (make-instance 'cash-or-nothing :kind kind :strike 110))
             (d2 (- (/ (+ (log (/ 100 110d0)) 0.05d0) 0.2d0) 0.1d0))
             (exact (* (exp -0.05d0) (norm-cdf (* phi d2)))))
        (multiple-value-bind (v se)
            (price (make-instance 'option :payoff payoff :exercise expiry) (mc :n-paths 100000) m)
          (check (format nil "Monte Carlo prices a user-defined digital ~(~A~)" kind)
                 exact v (* 4 se)))
        (multiple-value-bind (v se)
            (price (make-instance 'option :payoff payoff
                                          :exercise (make-instance 'american-exercise
                                                                   :expiry #D"2027-09-24"))
                   (mc :n-paths 50000 :n-steps 50) m)
          (check-that (format nil "LSM prices an American digital ~(~A~) above the European" kind)
                      (>= v (- exact (* 4 se)))))))
    (multiple-value-bind (v se)
        (price (make-instance 'option :payoff (make-instance 'digital-payoff :level 110d0)
                                      :exercise expiry)
               (mc :n-paths 100000) m)
      (check "a hand-written payoff with only PAYOFF-VALUE prices by Monte Carlo"
             (* (exp -0.05d0) (norm-cdf (- (/ (+ (log (/ 100 110d0)) 0.05d0) 0.2d0) 0.1d0)))
             v (* 4 se)))))

;;; --------------------------------------------------------------------
;;; The open spec syntax
;;; --------------------------------------------------------------------

;;; New syntax from outside fincl: (:digital level expiry).
(defmethod parse-option-spec ((head (eql :digital)) spec resolve-args)
  (destructuring-bind (&optional level expiry &rest more) (rest spec)
    (unless (and (realp level) expiry (null more))
      (error 'invalid-option-spec :spec spec :expected "(:digital level expiry)"))
    (make-instance 'option
                   :payoff (make-instance 'digital-payoff :level (float level 1d0))
                   :exercise (make-instance 'european-exercise
                                            :expiry (apply #'resolve-expiry expiry
                                                           resolve-args)))))

(defun option-spec-checks ()
  (format t "~&Option spec syntax~%")
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0)))
    (dolist (spec '((:call 100 #D"2027-09-24") (:put 95 #D"2027-03-10")
                    (:european :put 100 #D"2027-09-24") (:american :call 90 #D"2027-09-24")))
      (check-that (format nil "~S parses" spec) (typep (make-option spec) 'option)))
    (check-that "a head defined outside fincl is accepted, with tenor resolution"
                (let ((o (make-option '(:digital 95 #T"1Y") :from *today*)))
                  (and (typep (option-payoff o) 'digital-payoff)
                       (date= (expiry (option-exercise o)) #D"2027-09-24"))))
    (check "a digital built from a spec prices at expiry"
           1d0 (price (make-option `(:digital 95 ,*today*)) (make-instance 'black-scholes-engine) m)
           0)
    (check-error "an unknown head" (make-option '(:bogus 1 2)) invalid-option-spec)
    (check-that "the unknown-head report lists every head, including :DIGITAL"
                (let ((msg (error-message (lambda () (make-option '(:bogus 1 2))))))
                  (every (lambda (head) (search head msg))
                         '(":CALL" ":PUT" ":EUROPEAN" ":AMERICAN" ":DIGITAL"))))
    (check-that "a malformed spec with a known head names the expected shape"
                (search "(:american :call|:put strike expiry)"
                        (error-message (lambda () (make-option '(:american :call "100" #D"2027-09-24"))))))
    (check-error "an extra element" (make-option '(:call 100 #D"2027-09-24" :oops))
                 invalid-option-spec)
    (check-error "a spec that is not a list" (make-option :call) invalid-option-spec)))

;;; --------------------------------------------------------------------
;;; Exercise masks
;;; --------------------------------------------------------------------

(defun exercise-mask-checks ()
  (format t "~&Exercise masks~%")
  (let ((m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
        (grid (fincl::uniform-time-grid 1d0 4)))          ; 0, 0.25, 0.5, 0.75, 1
    (check-that "a uniform grid ends at tau exactly"
                (= 1d0 (aref (fincl::uniform-time-grid 1d0 3) 3)))
    (check-equal "European: expiry only" #*00001
                 (exercise-mask (make-instance 'european-exercise :expiry #D"2027-09-24") m grid))
    (check-equal "American: every step" #*11111
                 (exercise-mask (make-instance 'american-exercise :expiry #D"2027-09-24") m grid))
    ;; Today (index 0), 2027-03-25 (t = 0.4986, nearest index 2), expiry
    ;; (index 4); a date in the past is dropped.
    (check-equal "Bermudan: today, a snapped date, expiry; past date dropped" #*10101
                 (exercise-mask (make-instance 'bermudan-exercise
                                               :dates (list #D"2026-06-01" *today*
                                                            #D"2027-03-25" #D"2027-09-24"))
                                m grid))
    (check-equal "Bermudan: dates beyond the grid clamp to its last index" #*00001
                 (exercise-mask (make-instance 'bermudan-exercise
                                               :dates (list #D"2027-03-25" #D"2027-09-24"))
                                m (fincl::uniform-time-grid 0.5d0 4)))))
