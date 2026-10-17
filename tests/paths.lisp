;;;; paths.lisp --- time grids, the path-kernel protocol, path features,
;;;; path payoffs, control variates, QMC and MC Greeks (task 19)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun paths-market (&key (vol 0.2d0))
  (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol vol))

;;; --------------------------------------------------------------------
;;; Phase 1: grids and the protocol
;;; --------------------------------------------------------------------

;;; A GBM that counts its stepper's calls.
(defclass counting-gbm (gbm) () (:metaclass parameterized-class))
(defvar *stepper-calls* (list 0)
  "A cell counting stepper calls; incremented atomically, from every worker.")
(defun stepper-calls () (car *stepper-calls*))
(defun reset-stepper-calls () (setf (car *stepper-calls*) 0))
(defmethod make-stepper :around ((p counting-gbm) market times)
  (declare (ignore market times))
  (let ((stepper (call-next-method)))
    (lambda (state normals j)
      (sb-ext:atomic-incf (car *stepper-calls*))
      (funcall stepper state normals j))))

;;; A test payoff that pays 1 on each of its dates and stops after the second.
(defclass two-coupons (payoff)
  ((dates :initarg :dates :reader coupon-dates))
  (:metaclass component-class))
(defmethod payoff-requirements ((p two-coupons) (f path-independent)) '(:cashflows))
(defmethod path-payment-dates ((p two-coupons) (f path-independent)) (coupon-dates p))
(defmethod fincl::payoff-mandatory-dates ((p two-coupons)) (coupon-dates p))
(defmethod path-accumulator-size ((p two-coupons) (f path-independent)) 1)
(defmethod path-kernel ((p two-coupons) (f path-independent) process grid)
  (declare (ignore process))
  (multiple-value-bind (mask positions) (fincl::grid-date-mask grid (coupon-dates p))
    (values (lambda (acc) (setf (aref acc 0) 0d0))
            (lambda (acc j state pay)
              (declare (ignore state))
              (if (= 1 (sbit mask j))
                  (progn (funcall pay 1d0 (aref positions j))
                         (< (incf (aref acc 0)) 2d0))
                  t))
            (lambda (acc state) (declare (ignore acc state)) 100d0))))

;;; A call that declares it needs the path, so GBM steps instead of sampling.
(define-payoff stepped-call (strike-payoff) () (s) (max 0d0 (* phi (- s strike))))
(defmethod payoff-requirements ((p stepped-call) (f path-independent)) '(:path))

;;; A GBM whose pathwise discount factors are the market's: the D13 hook.
(defclass curve-discounting-gbm (gbm) () (:metaclass parameterized-class))
(defmethod fincl::path-discount-kernel ((p curve-discounting-gbm) market grid)
  (let ((times (fincl::time-grid-times grid)))
    (lambda (state j)
      (declare (ignore state))
      (discount-factor market (aref times j)))))

(defun grid-checks ()
  (format t "~&Time grids and the path-kernel protocol~%")
  (let* ((m (paths-market))
         (dates (list #D"2026-10-09" #D"2027-01-15" #D"2027-01-18" #D"2027-06-30" #D"2027-09-24"))
         (grid (fincl::make-time-grid m :dates dates :max-dt 0.05d0))
         (times (fincl::time-grid-times grid)))
    (check-that "every mandatory date is a grid time, exactly"
                (every (lambda (d) (find (market-time m d) times :test #'=)) dates))
    (check-that "no step exceeds max-dt"
                (iter (for i from 1 below (length times))
                      (always (<= (- (aref times i) (aref times (1- i))) (+ 0.05d0 1d-12)))))
    (check-that "dates on or before the valuation date are dropped"
                (= 5 (length (fincl::time-grid-dates
                              (fincl::make-time-grid m :dates (cons *today* dates) :max-dt 0.05d0))))))
  (let ((m (paths-market)))
    (check-that "expiry and n-steps alone give the uniform grid, bit for bit"
                (equalp (fincl::time-grid-times (fincl::make-time-grid m :dates (list #D"2027-09-24")
                                                                         :n-steps 50))
                        (fincl::uniform-time-grid (market-time m #D"2027-09-24") 50))))
  ;; Named state components.
  (check-that "state names resolve: GBM :spot, Heston :spot/:variance, GARCH :conditional-variance"
              (and (= 0 (state-index (make-gbm) :spot))
                   (= 1 (state-index (make-heston) :variance))
                   (= 1 (state-index (make-garch) :conditional-variance))
                   (= 0 (state-index (make-cev) '(:spot)))))
  (check-error "an unknown state name" (state-index (make-heston) :volatility) invalid-argument)
  (check-that "... and the report lists the process's names"
              (search ":VARIANCE" (or (error-message (lambda () (state-index (make-heston) :vol))) "")))
  ;; The terminal sampler stays in use for terminal payoffs.
  (let ((m (paths-market))
        (call (make-option (call 100) (expires #D"2027-09-24"))))
    (reset-stepper-calls)
    (price call (make-engine :mc :process (make-instance 'counting-gbm) :n-paths 2000) m)
    (check "a European under GBM never steps (the terminal sampler)" 0 (stepper-calls) 0)
    (reset-stepper-calls)
    (progn
      (price (make-option (payoff 'two-coupons :dates (list #D"2027-03-24" #D"2027-09-24"))
                          (expires #D"2027-09-24"))
             (make-engine :mc :process (make-instance 'counting-gbm) :n-paths 100 :n-steps 10
                          :antithetic nil)
             m)
      (check-that (format nil "a cash-flow payoff steps its paths (~D calls)" (stepper-calls))
                  (plusp (stepper-calls)))))
  ;; Deterministic cash flows: two of three coupons, then a stop.
  (let* ((m (paths-market))
         (dates (list #D"2027-01-15" #D"2027-05-14" #D"2027-09-24"))
         (option (make-option (payoff 'two-coupons :dates dates) (expires #D"2027-09-24")))
         (exact (+ (discount-factor m (first dates)) (discount-factor m (second dates)))))
    (multiple-value-bind (v se) (price option (make-engine :mc :n-paths 1000 :n-steps 12) m)
      (check-that (format nil "pays 1 on two of three dates then stops: ~,15F vs ~,15F, SE ~A"
                          v exact se)
                  (and (< (abs (- v exact)) (* 4 double-float-epsilon exact)) (zerop se))))
    (check "... the same through a pathwise discount kernel equal to the curve" exact
           (price option (make-engine :mc :process (make-instance 'curve-discounting-gbm)
                                          :n-paths 1000 :n-steps 12)
                  m)
           1d-14)
    (check-that "processes discount with the market's curve by default (no kernel)"
                (null (fincl::path-discount-kernel (make-heston) m
                                                   (fincl::make-time-grid m :dates dates)))))
  ;; Memory does not grow with the number of steps: no path matrix, and the
  ;; stepped loop conses nothing per step (GBM's stepper does not either).
  (let* ((m (paths-market))
         (call (make-option (call 100) (stepped-call) (expires #D"2027-09-24")))
         (bytes (mapcar (lambda (n)
                          (sb-ext:gc :full t)
                          (let ((start (sb-ext:get-bytes-consed)))
                            (price call (make-engine :mc :n-paths 20000 :n-steps n) m)
                            (- (sb-ext:get-bytes-consed) start)))
                        '(25 100 400))))
    (check-that (format nil "stepped European (GBM) bytes consed flat in n-steps: ~{~:D~^, ~} at 25, ~
100, 400 steps" bytes)
                (< (third bytes) (* 2 (first bytes))))))

;;; --------------------------------------------------------------------
;;; Phase 2: Asians, barriers and lookbacks
;;; --------------------------------------------------------------------

(defun expiring (days &optional (from *today*))
  (add-tenor from (make-tenor days :days)))

(defun within-se (label value reference se &optional (slack 0d0))
  "Check |VALUE - REFERENCE| <= 3 SE + SLACK, reporting the distance in SEs."
  (check-that (format nil "~A: ~,5F vs ~,5F (~,2F SE)" label value reference
                      (/ (abs (- value reference)) se))
              (<= (abs (- value reference)) (+ (* 3 se) slack))))

;;; Haug (1998) p. 72, through QuantLib's barrieroption.cpp: S = 100,
;;; q = 4%, r = 8%, T = 0.5 (180 days, ACT/360), rebate 3, vol 25%.
(defparameter *haug-barriers*
  '((:down-and-out 95 :call 90 9.0246) (:down-and-out 95 :call 100 6.7924)
    (:down-and-out 95 :call 110 4.8759) (:down-and-out 100 :call 90 3.0)
    (:up-and-out 105 :call 90 2.6789) (:up-and-out 105 :call 100 2.3580)
    (:up-and-out 105 :call 110 2.3453)
    (:down-and-in 95 :call 90 7.7627) (:down-and-in 95 :call 100 4.0109)
    (:down-and-in 95 :call 110 2.0576) (:down-and-in 100 :call 90 13.8333)
    (:up-and-in 105 :call 90 14.1112) (:up-and-in 105 :call 100 8.4482)
    (:up-and-in 105 :call 110 4.5910)
    (:down-and-out 95 :put 90 2.2798) (:down-and-out 95 :put 100 2.2947)
    (:down-and-out 95 :put 110 2.6252) (:down-and-out 100 :put 90 3.0)
    (:up-and-out 105 :put 90 3.7760) (:up-and-out 105 :put 100 5.4932)
    (:up-and-out 105 :put 110 7.5187)
    (:down-and-in 95 :put 90 2.9586) (:down-and-in 95 :put 100 6.5677)
    (:down-and-in 95 :put 110 11.9752) (:down-and-in 100 :put 90 2.2845)
    (:up-and-in 105 :put 90 1.4653) (:up-and-in 105 :put 100 3.3721)
    (:up-and-in 105 :put 110 7.0846)))

(defun act-360-market (&key (spot 100) rate dividend vol)
  (make-market :valuation-date *today* :day-counter (make-instance 'fincl::actual-360)
               :underlyings `((:x :spot ,spot :dividend ,dividend :vol ,vol :currency :usd))
               :curves `((:usd :rate ,rate))))

(defun analytic-path-checks ()
  (format t "~&Analytic barriers and geometric Asians~%")
  (let ((m (act-360-market :rate 0.08d0 :dividend 0.04d0 :vol 0.25d0)))
    (check-that "Reiner-Rubinstein reproduces Haug's 28 values to 1e-4 (spot on the barrier included)"
                (every (lambda (row)
                         (destructuring-bind (type h kind k expected) row
                           (< (abs (- (price (make-option-from-clauses
                                              `((,kind ,k) (:barrier ,type ,h :rebate 3)
                                                (:expires ,(expiring 180))))
                                             :black-scholes m)
                                      expected))
                              5d-5)))
                       *haug-barriers*)))
  ;; Clewlow and Strickland, via QuantLib's asianoptions.cpp: ten fixings
  ;; 36 days apart, ACT/360, r = 6%, q = 3%, vol 20%.
  (let ((m (act-360-market :rate 0.06d0 :dividend 0.03d0 :vol 0.2d0))
        (dates (iter (for j from 1 to 10) (collect (expiring (* 36 j))))))
    (check "discrete geometric Asian = QuantLib's 5.3425606635" 5.3425606635d0
           (price (make-option (call 100) (average :geometric :dates dates) (expires (expiring 360)))
                  :black-scholes m)
           1d-10))
  (let ((m (paths-market)))
    (check-that "a knock-out with the spot past the barrier is its rebate"
                (= 2d0 (price (make-option (call 100) (barrier :up-and-out 95 :rebate 2)
                                           (expires (expiring 365)))
                              :black-scholes m)))
    (check "... and a knock-in is the European"
           (price (make-option (call 100) (expires (expiring 365))) :black-scholes m)
           (price (make-option (call 100) (barrier :up-and-in 95) (expires (expiring 365)))
                  :black-scholes m)
           1d-14)
    (check-error "an arithmetic Asian has no closed form"
                 (price (make-option (call 100) (average :arithmetic :every #T"1M" :from *today*)
                                     (expires (expiring 365)))
                        :black-scholes m)
                 unsupported-combination)
    (check-error "a lookback has no closed form here"
                 (price (make-option (call 100) (lookback :floating) (expires (expiring 365)))
                        :black-scholes m)
                 unsupported-combination)))

(defun mc-path-checks ()
  (format t "~&Monte Carlo Asians, barriers and lookbacks~%")
  (let* ((m (paths-market))
         (expiry (expiring 365))
         (monthly (iter (for j from 1 to 12)
                        (collect (add-tenor *today* (make-tenor j :months)))))
         (mc (make-engine :mc :n-paths 200000 :n-steps 50)))
    ;; Geometric Asian.
    (let ((o (make-option (call 100) (average :geometric :dates monthly) (expires expiry))))
      (multiple-value-bind (v se) (price o mc m)
        (within-se "geometric Asian MC vs closed form" v (price o :black-scholes m) se)))
    (let ((arithmetic (make-option (call 100) (average :arithmetic :dates monthly) (expires expiry))))
      (check-that "arithmetic Asian >= geometric (AM-GM, same paths)"
                  (>= (price arithmetic mc m)
                      (price (make-option (call 100) (average :geometric :dates monthly)
                                          (expires expiry))
                             mc m)))
      (multiple-value-bind (v se) (price (make-option (put 100) (average :arithmetic :dates monthly
                                                                         :strike-type :floating)
                                                      (expires expiry))
                                         mc m)
        (check-that (format nil "floating-strike Asian put prices: ~,4F (SE ~,4F)" v se)
                    (and (plusp v) (plusp se)))))
    ;; Continuous barrier: the bridge removes the grid's bias.
    (let ((out (make-option (call 100) (barrier :down-and-out 90) (expires expiry))))
      (multiple-value-bind (v se) (price out mc m)
        (within-se "continuous down-and-out, bridge, 50 steps, vs Reiner-Rubinstein" v
                   (price out :black-scholes m) se))
      (multiple-value-bind (v se) (price out (derive-engine mc :barrier-bridge nil) m)
        (let ((exact (price out :black-scholes m)))
          (check-that (format nil "... without the bridge it is biased high: ~,5F vs ~,5F (~,1F SE)"
                              v exact (/ (- v exact) se))
                      (> (- v exact) (* 5 se))))))
    (dolist (case '((:down-and-out 90 :call 2) (:down-and-in 90 :call 2) (:up-and-out 120 :put 2)
                    (:up-and-in 120 :put 2) (:up-and-out 120 :call 0) (:down-and-in 85 :put 0)))
      (destructuring-bind (type h kind rebate) case
        (let ((o (make-option-from-clauses `((,kind 100) (:barrier ,type ,h :rebate ,rebate)
                                             (:expires ,expiry)))))
          (multiple-value-bind (v se) (price o mc m)
            (within-se (format nil "~(~A~) ~D ~(~A~), rebate ~D, bridge" type h kind rebate) v
                       (price o :black-scholes m) se)))))
    ;; Daily monitoring against Broadie-Glasserman-Kou.
    (let* ((days (iter (for d in-dates (expiring 1) below (expiring 366)) (collect d)))
           (o (make-option (call 100) (barrier :down-and-out 90 :monitoring days) (expires expiry))))
      (multiple-value-bind (v se) (price o (make-engine :mc :n-paths 200000) m)
        (within-se "daily down-and-out vs BGK-corrected Reiner-Rubinstein" v
                   (price o :black-scholes m) se 1d-3)))
    ;; In + out = vanilla.
    (let ((in (make-option (put 100) (barrier :down-and-in 85 :monitoring monthly) (expires expiry)))
          (out (make-option (put 100) (barrier :down-and-out 85 :monitoring monthly) (expires expiry)))
          (vanilla (price (make-option (put 100) (expires expiry)) :black-scholes m)))
      (multiple-value-bind (vi sei) (price in mc m)
        (multiple-value-bind (vo seo) (price out mc m)
          (within-se "monthly down-and-in + down-and-out put = European" (+ vi vo) vanilla
                     (sqrt (+ (* sei sei) (* seo seo)))))))
    ;; Other diffusions use the bridge; jump processes refuse it.
    (let ((out (make-option (call 100) (barrier :down-and-out 90) (expires expiry))))
      (multiple-value-bind (v se) (price out (make-engine :mc :process (make-heston :xi 1d-8)
                                                          :n-paths 100000)
                                         m)
        (within-se "Heston with xi -> 0 reduces to Reiner-Rubinstein (bridge on v dt)" v
                   (price out :black-scholes m) se))
      (multiple-value-bind (v se) (price out (make-engine :mc :process (make-cev) :n-paths 100000) m)
        (within-se "CEV with beta = 1 reduces to Reiner-Rubinstein" v (price out :black-scholes m) se))
      (check-error "a continuous barrier under jumps"
                   (price out (make-engine :mc :process (make-merton-jump)) m)
                   model-domain-error)
      (check-that "... and prices with :barrier-bridge nil"
                  (plusp (price out (make-engine :mc :process (make-merton-jump) :n-paths 2000
                                                     :barrier-bridge nil)
                                m)))
      (check-that "automatic selection prices a barrier in closed form"
                  (typep (nth-value 2 (price out :auto m)) 'black-scholes-engine)))
    ;; Lookbacks: the floating call pays at least the European call.
    (let ((european (price (make-option (call 100) (expires expiry)) :black-scholes m)))
      (multiple-value-bind (v se) (price (make-option (call 100) (lookback :floating) (expires expiry)) mc m)
        (check-that (format nil "floating lookback call ~,4F (SE ~,4F) >= European ~,4F" v se european)
                    (> v european)))
      (multiple-value-bind (v se) (price (make-option (call 100) (lookback :fixed) (expires expiry)) mc m)
        (check-that (format nil "fixed lookback call ~,4F (SE ~,4F) >= European" v se)
                    (> v european))))))

(defun path-feature-language-checks ()
  (format t "~&Path features in the option language~%")
  (let ((dates (list #D"2027-03-24" #D"2027-09-24")))
    (dolist (o (list (make-option (call 100) (average :arithmetic :dates dates) (expires #D"2027-09-24"))
                     (make-option (put 90) (average :geometric :every #T"3M" :from *today*
                                                    :strike-type :floating)
                                  (expires #D"2027-09-24"))
                     (make-option (call 100) (barrier :down-and-out 90 :rebate 2) (expires #D"2027-09-24"))
                     (make-option (put 100) (barrier :up-and-in 120 :monitoring dates)
                                  (expires #D"2027-09-24"))
                     (make-option (call 100) (lookback :floating) (expires #D"2027-09-24"))))
      (check-that (format nil "~A round-trips" o) (round-trip-p 'option o)))
    (check-that "(average ... :every #T\"3M\" :from today) generates quarterly dates"
                (equalp (observation-dates
                         (option-path (make-option (call 100) (average :arithmetic :every #T"3M"
                                                                         :from *today*)
                                                   (expires #D"2027-09-24"))))
                        (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24")))
    (check-error "an observation date after the expiry"
                 (make-option (call 100) (average :arithmetic :dates (list #D"2028-01-01"))
                              (expires #D"2027-09-24"))
                 invalid-argument)
    (check-error "a floating-strike Asian on a non-vanilla payoff"
                 (make-option (call 100) (capped-call :cap 30)
                              (average :arithmetic :dates dates :strike-type :floating)
                              (expires #D"2027-09-24"))
                 invalid-argument)
    (check-error "an American barrier"
                 (make-option (call 100) (american) (barrier :down-and-out 90) (expires #D"2027-09-24"))
                 invalid-argument)
    (check-error "an unknown barrier type"
                 (make-option (call 100) (barrier :sideways 90) (expires #D"2027-09-24"))
                 construction-error)
    (check-error "an observation on or before the valuation date"
                 (price (make-option (call 100) (average :arithmetic :dates (list *today* #D"2027-09-24"))
                                     (expires #D"2027-09-24"))
                        :mc (paths-market))
                 fincl::past-observation)))

;;; --------------------------------------------------------------------
;;; Phase 3: DEFINE-PATH-PAYOFF
;;; --------------------------------------------------------------------

;;; Called at the first observation at or above CALL-LEVEL x INITIAL, paying
;;; the notional and one coupon per observation so far; otherwise the
;;; notional at expiry, less the loss below PROTECTION x INITIAL.
(define-path-payoff test-autocallable (payoff)
    ((notional double-float) (initial double-float) (call-level double-float)
     (coupon double-float) (protection double-float) (observations list))
  (s)
  (:on observations (i)
    (when (>= s (* call-level initial))
      (pay (+ notional (* coupon (+ i 1))))
      (stop)))
  (:final (if (>= s (* protection initial)) notional (* notional (/ s initial)))))

;;; The same contract, but the path is stepped to expiry after the call.
(define-path-payoff test-autocallable-no-stop (payoff)
    ((notional double-float) (initial double-float) (call-level double-float)
     (coupon double-float) (protection double-float) (observations list))
  (s)
  (:accumulate (called 0d0))
  (:on observations (i)
    (when (and (zerop called) (>= s (* call-level initial)))
      (pay (+ notional (* coupon (+ i 1))))
      (setf called 1d0)))
  (:final (cond ((plusp called) 0d0)
                ((>= s (* protection initial)) notional)
                (t (* notional (/ s initial))))))

;;; Sum of the capped and floored returns between resets.
(define-path-payoff test-cliquet (payoff)
    ((local-cap double-float) (local-floor double-float) (resets list))
  (s)
  (:accumulate (total 0d0) (previous 0d0))
  (:start (setf previous s))
  (:on resets ()
    (incf total (min local-cap (max local-floor (- (/ s previous) 1d0))))
    (setf previous s))
  (:final (max 0d0 total)))

;;; A call on the Heston variance at expiry.
(define-path-payoff test-variance-call (payoff)
    ((level double-float))
  (s)
  (:state v :variance)
  (:final (max 0d0 (- v level))))

(defun quarterly-dates (&optional (n 4))
  "N observation dates 91 days apart; the last is the expiry."
  (iter (for k from 1 to n) (collect (expiring (* 91 k)))))

(defun autocallable (class dates)
  (make-option-from-clauses `((:payoff ,class :notional 100 :initial 100 :call-level 1.05d0
                                        :coupon 2 :protection 0.8d0 :observations ,dates)
                              (:expires ,(a:lastcar dates)))))

(defun brute-force-price (engine market dates path-value)
  "The mean over antithetic pairs of (funcall PATH-VALUE spots discounts) on
the paths of SIMULATE-PATHS, whose N-STEPS-step uniform grid is DATES' grid:
SPOTS[k] is the spot on grid point k, DISCOUNTS[k] the discount factor to it."
  (let* ((tau (market-time market (a:lastcar dates)))
         (paths (fincl::simulate-paths engine market tau))
         (n (fincl::path-matrix-n-paths paths))
         (n-times (fincl::path-matrix-n-times paths))
         (data (fincl::path-matrix-data paths))
         (discounts (cons 1d0 (mapcar (lambda (d) (discount-factor market d)) dates)))
         (values (iter (for i below n)
                       (collect (funcall path-value
                                         (iter (for k below n-times) (collect (aref data (+ i (* k n)))))
                                         discounts))))
         (half (floor n 2)))
    (/ (iter (for v in values) (for w in (nthcdr half values)) (for i below half)
             (sum (* 0.5d0 (+ v w))))
       half)))

(defun path-payoff-checks ()
  (format t "~&DEFINE-PATH-PAYOFF~%")
  (let* ((m (paths-market))
         (dates (quarterly-dates))
         (engine (make-engine :mc :n-paths 20000 :n-steps 4))
         (option (autocallable 'test-autocallable dates)))
    (let ((reference (brute-force-price
                      engine m dates
                      (lambda (spots discounts)
                        (or (iter (for k from 1 to 4)
                                  (when (>= (nth k spots) (* 1.05d0 100d0))
                                    (return (* (nth k discounts) (+ 100d0 (* 2d0 k))))))
                            (let ((s (nth 4 spots)))
                              (* (nth 4 discounts)
                                 (if (>= s (* 0.8d0 100d0)) 100d0 (* 100d0 (/ s 100d0)))))))))
          (v (price option engine m)))
      (check-that (format nil "the autocallable equals a brute force on SIMULATE-PATHS (same seed): ~
~,12F vs ~,12F, relative ~,1E" v reference (relative-difference reference v))
                  (< (relative-difference reference v) 1d-12)))
    (check "... and so does the version that never stops"
           (price option engine m)
           (price (autocallable 'test-autocallable-no-stop dates) engine m)
           1d-12)
    (let ((resets (quarterly-dates))
          (cap 0.05d0) (floor -0.03d0))
      (check "the cliquet equals a brute force on SIMULATE-PATHS"
             (brute-force-price engine m resets
                                (lambda (spots discounts)
                                  (* (nth 4 discounts)
                                     (max 0d0 (iter (for (a b) on spots) (while b)
                                                    (sum (min cap (max floor (- (/ b a) 1d0)))))))))
             (price (make-option (test-cliquet :local-cap cap :local-floor floor :resets resets)
                                 (expires (a:lastcar resets)))
                    engine m)
             1d-12)))
  ;; A payoff on latent state: with xi = 0 the Heston variance is
  ;; deterministic, v_T = theta + (v0 - theta) e^(-kappa T).
  (let* ((m (paths-market))
         (expiry (expiring 365))
         (tau (market-time m expiry))
         (heston '(:v0 0.06d0 :theta 0.03d0 :kappa 1.5d0 :xi 0d0 :rho -0.5d0)))
    (dolist (scheme '(:quadratic-exponential :full-truncation))
      (multiple-value-bind (v se)
          (price (make-option (test-variance-call :level 0.035d0) (expires expiry))
                 (make-engine :mc :process (apply #'make-instance 'heston :scheme scheme heston)
                              :n-paths 1000
                              :n-steps 365)
                 m)
        (check (format nil "a call on the Heston variance (:state v :variance), xi = 0, ~(~A~), SE ~A"
                       scheme se)
               (* (discount-factor m expiry)
                  (max 0d0 (- (+ 0.03d0 (* (- 0.06d0 0.03d0) (exp (* -1.5d0 tau)))) 0.035d0)))
               v
               (if (eq scheme :full-truncation) 1d-4 1d-12)))))
  ;; Early termination: weekly observations, so most paths are called early.
  (let* ((m (paths-market))
         (weekly (iter (for k from 1 to 52) (collect (expiring (* 7 k)))))
         (engine (make-engine :mc :process (make-instance 'counting-gbm) :n-paths 20000)))
    (flet ((run (class)
             (reset-stepper-calls)
             (let ((start (get-internal-real-time)))
               (dotimes (i 3) (price (autocallable class weekly) engine m))
               (values (/ (stepper-calls) 3 20000d0)
                       (/ (- (get-internal-real-time) start) internal-time-units-per-second 3d0)))))
      (multiple-value-bind (steps seconds) (run 'test-autocallable)
        (multiple-value-bind (all-steps all-seconds) (run 'test-autocallable-no-stop)
          (check-that (format nil "stopping saves steps: ~,1F steps per path vs ~,1F; ~,3Fs vs ~
~,3Fs, ~,1Fx faster"
                              steps all-steps seconds all-seconds (/ all-seconds seconds))
                      (< steps (* 0.6 all-steps)))))))
  ;; The generated closures are typed: no generic arithmetic, no slot readers.
  (let* ((m (paths-market))
         (grid (fincl::make-time-grid m :dates (quarterly-dates)))
         (payoff (option-payoff (autocallable 'test-autocallable (quarterly-dates)))))
    (multiple-value-bind (init step final) (path-kernel payoff (make-instance 'path-independent)
                                                        (make-gbm) grid)
      (check-that "the autocallable's closures make no generic arithmetic call"
                  (notany (lambda (f) (search "GENERIC" (disassembly f))) (list init step final)))
      (check-that "... and call no slot reader"
                  (notany (lambda (f) (or (search "NOTIONAL" (disassembly f))
                                          (search "CALL-LEVEL" (disassembly f))))
                          (list init step final)))))
  (let* ((cliquet (make-option (test-cliquet :local-cap 0.05 :local-floor -0.03 :resets (quarterly-dates))
                               (expires (expiring 364))))
         (step (nth-value 1 (path-kernel (option-payoff cliquet) (make-instance 'path-independent)
                                         (make-gbm)
                                         (fincl::make-time-grid (paths-market)
                                                                :dates (quarterly-dates))))))
    (check-that "the cliquet's step closure makes no generic arithmetic call"
                (not (search "GENERIC" (disassembly step)))))
  ;; Requirements and the language.
  (let ((option (autocallable 'test-autocallable (quarterly-dates))))
    (check-that "requirements: the autocallable (:path :cashflows), the variance call (:path :state)"
                (and (equal '(:path :cashflows)
                            (payoff-requirements (option-payoff option) (option-path option)))
                     (equal '(:path :state)
                            (payoff-requirements (make-instance 'test-variance-call :level 1)
                                                 (make-instance 'path-independent)))))
    (check-that "an autocallable is a clause and round-trips"
                (round-trip-p 'option option))
    (check-that "... built from its clause"
                (typep (option-payoff (make-option (test-autocallable :notional 100 :initial 100
                                                                      :call-level 1 :coupon 1
                                                                      :protection 0.8
                                                                      :observations (quarterly-dates))
                                                   (expires (expiring 364))))
                       'test-autocallable))
    (dolist (engine '(:black-scholes :cos :binomial))
      (check-error (format nil "~(~A~) refuses a path payoff" engine)
                   (price option engine (paths-market))
                   unsupported-combination))
    (check-error "a path payoff with a barrier"
                 (price (make-option-from-clauses
                         `((:payoff test-autocallable :notional 100 :initial 100 :call-level 1.05
                                    :coupon 2 :protection 0.8 :observations ,(quarterly-dates))
                           (:barrier :down-and-out 50)
                           (:expires ,(expiring 364))))
                        :mc (paths-market))
                 unsupported-combination)
    (check-error "an American path payoff"
                 (make-option-from-clauses
                  `((:payoff test-autocallable :notional 100 :initial 100 :call-level 1.05
                             :coupon 2 :protection 0.8 :observations ,(quarterly-dates))
                    (:american) (:expires ,(expiring 364))))
                 invalid-argument)
    (check-error "PAY outside an :ON clause"
                 (macroexpand-1 '(define-path-payoff bad-payoff (payoff) ((x double-float)) (s)
                                  (:every (pay 1d0))
                                  (:final 0d0)))
                 invalid-argument)))

;;; --------------------------------------------------------------------
;;; Phase 4: control variates and randomized QMC
;;; --------------------------------------------------------------------

(defun sobol-reference-checks ()
  (format t "~&Sobol' points against QuantLib (Joe-Kuo D6)~%")
  (let* ((fixture (with-open-file (in (asdf:system-relative-pathname :fincl "tests/fixtures/sobol.sexp"))
                    (read in)))
         (dims (getf fixture :dimensions))
         (n (getf fixture :points))
         (p (1- (expt 2 61)))
         (hashes (make-array dims :initial-element 0))
         (rows '()))
    (multiple-value-bind (directions shift) (fincl::sobol-tables dims)
      (let ((point (make-array dims :element-type '(unsigned-byte 64))))
        ;; QuantLib's draw k is point k + 1: it skips the origin.
        (fincl::sobol-point! point directions shift 1)
        (iter (for k from 1 to n)
              (when (> k 1) (fincl::sobol-next! point directions (1- k)))
              (let ((row (map 'list (lambda (x) (ash x -32)) point)))
                (when (<= k 16) (push row rows))
                (iter (for x in row) (for d from 0)
                      (setf (aref hashes d) (mod (+ (* (aref hashes d) 1000003) x) p)))))))
    (check-that (format nil "all ~D dimensions of the first ~D points match (per-dimension hashes)" dims n)
                (every #'= hashes (getf fixture :hashes)))
    (check-that "... and the first 16 points, every dimension, integer for integer"
                (equal (reverse rows) (getf fixture :first-points)))
    (check-that "the points jump-started at index k equal the Gray-code walk"
                (multiple-value-bind (directions shift) (fincl::sobol-tables 7 :key 42 :randomization 3)
                  (let ((walked (make-array 7 :element-type '(unsigned-byte 64)))
                        (direct (make-array 7 :element-type '(unsigned-byte 64))))
                    (fincl::sobol-point! walked directions shift 0)
                    (iter (for k from 1 below 300)
                          (fincl::sobol-next! walked directions (1- k))
                          (always (equalp walked (fincl::sobol-point! direct directions shift k)))))))
    (check-that "a scrambled dimension is still a (0, m, 1)-net: one point in each 1/256 interval"
                (multiple-value-bind (directions shift) (fincl::sobol-tables 3 :key 7 :randomization 0)
                  (let ((point (make-array 3 :element-type '(unsigned-byte 64))))
                    (every (lambda (d)
                             (let ((cells (make-array 256 :initial-element 0)))
                               (dotimes (k 256)
                                 (fincl::sobol-point! point directions shift k)
                                 (incf (aref cells (ash (aref point d) -56))))
                               (every (lambda (c) (= c 1)) cells)))
                           '(0 1 2)))))
    (check-error "more dimensions than the bundled 1111"
                 (fincl::sobol-tables 1112)
                 invalid-argument)))

(defun log-log-slope (ns ses)
  "Least-squares slope of log SE against log N."
  (let* ((xs (mapcar #'log ns)) (ys (mapcar #'log ses))
         (mx (/ (reduce #'+ xs) (length xs))) (my (/ (reduce #'+ ys) (length ys))))
    (/ (iter (for x in xs) (for y in ys) (sum (* (- x mx) (- y my))))
       (iter (for x in xs) (sum (expt (- x mx) 2))))))

(defun variance-reduction-checks ()
  (format t "~&Control variates and randomized QMC~%")
  (let* ((m (paths-market))
         (expiry (expiring 365))
         (asian (make-option (call 100) (average :arithmetic :every #T"1M" :from *today*) (expires expiry)))
         (call (make-option (call 100) (expires expiry))))
    ;; The geometric Asian as control variate for the arithmetic one.
    (multiple-value-bind (v se) (price asian (make-engine :mc :n-paths 100000) m)
      (multiple-value-bind (v-cv se-cv)
          (price asian (make-engine :mc :n-paths 100000 :control-variates '(:geometric-asian)) m)
        (check-that (format nil "geometric-Asian control variate: SE ~,6F -> ~,6F (~,1Fx)" se se-cv
                            (/ se se-cv))
                    (> (/ se se-cv) 10))
        (within-se "... and the price is within 3 SE of the plain estimate" v-cv v se)))
    (multiple-value-bind (v se) (price (make-option (call 100) (barrier :down-and-out 90) (expires expiry))
                                       (make-engine :mc :n-paths 100000 :control-variates '(:european))
                                       m)
      (within-se "a down-and-out with the European as control variate vs Reiner-Rubinstein" v
                 (price (make-option (call 100) (barrier :down-and-out 90) (expires expiry))
                        :black-scholes m)
                 se))
    (let ((heston (make-heston :xi 0.5d0 :rho -0.7d0)))
      (multiple-value-bind (v se) (price asian (make-engine :mc :process heston :n-paths 100000) m)
        (multiple-value-bind (v-cv se-cv)
            (price asian (make-engine :mc :process heston :n-paths 100000 :control-variates '(:european)) m)
          (check-that (format nil "under Heston the European variate is priced by COS: SE ~,5F -> ~,5F, ~
~,5F vs ~,5F" se se-cv v-cv v)
                      (and (< se-cv se) (<= (abs (- v v-cv)) (* 3 se)))))))
    (check-error "a geometric-Asian variate for a barrier option"
                 (price (make-option (call 100) (barrier :down-and-out 90) (expires expiry))
                        (make-engine :mc :n-paths 1000 :control-variates '(:geometric-asian)) m)
                 invalid-argument)
    ;; Randomized QMC: the standard error falls nearly as 1/N.
    (let ((ns '(2048 4096 8192 16384 32768 65536)))
      (dolist (case `(("European" ,call) ("Asian" ,asian)))
        (destructuring-bind (label option) case
          (flet ((ses (rng)
                   (mapcar (lambda (n) (nth-value 1 (price option (make-engine :mc :rng rng :n-paths n) m)))
                           ns)))
            (let ((qmc (log-log-slope ns (ses :sobol)))
                  (mc (log-log-slope ns (ses :counter))))
              (check-that (format nil "~A: SE vs N log-log slope ~,2F under :sobol (plain MC ~,2F)"
                                  label qmc mc)
                          (<= qmc -0.8)))))))
    (multiple-value-bind (v se) (price asian (make-engine :mc :rng :sobol :n-paths 65536
                                                         :control-variates '(:geometric-asian))
                                       m)
      (within-se "RQMC with a control variate agrees with the plain estimate" v
                 (price asian (make-engine :mc :n-paths 100000) m)
                 (sqrt (+ (* se se) (expt (nth-value 1 (price asian (make-engine :mc :n-paths 100000) m)) 2)))))
    (check-that "RQMC prices a Heston American (LSM on bridged Sobol' paths)"
                (multiple-value-bind (v se)
                    (price (make-option (put 100) (american) (expires expiry))
                           (make-engine :mc :rng :sobol :process (make-heston) :n-paths 8192 :n-steps 25) m)
                  (and (plusp v) (plusp se))))
    ;; Thread and chunk invariance.
    (flet ((prices ()
             (list (price asian (make-engine :mc :rng :sobol :n-paths 8192) m)
                   (price asian (make-engine :mc :n-paths 8192 :control-variates '(:geometric-asian :european)) m)
                   (price call (make-engine :mc :rng :sobol :n-paths 8192 :n-chunks 7) m)
                   (price (make-option (call 100) (barrier :up-and-out 120) (expires expiry))
                          (make-engine :mc :rng :sobol :n-paths 8192) m))))
      (let ((one (let ((lp:*kernel* (lp:make-kernel 1)))
                   (unwind-protect (prices) (lp:end-kernel :wait t))))
            (four (let ((lp:*kernel* (lp:make-kernel 4)))
                    (unwind-protect (prices) (lp:end-kernel :wait t))))
            (rechunked (list (price asian (make-engine :mc :rng :sobol :n-paths 8192 :n-chunks 3) m))))
        (check-that (format nil "RQMC and control-variate prices are identical with 1 and 4 workers: ~A" one)
                    (every #'= one four))
        (check-that "... and with a different N-CHUNKS"
                    (= (first one) (first rechunked)))))))

;;; --------------------------------------------------------------------
;;; Phase 5: Bermudan LSM and Monte Carlo Greeks
;;; --------------------------------------------------------------------

(define-payoff test-digital (strike-payoff) () (s) (if (plusp (* phi (- s strike))) 1d0 0d0))

(defun lsm-and-greek-checks ()
  (format t "~&Bermudan LSM and Monte Carlo Greeks~%")
  (let* ((m (paths-market))
         (expiry (expiring 365))
         (bermudan (make-option (put 100) (bermudan :every #T"3M" :from *today*)
                                (expires (add-tenor *today* (make-tenor 1 :years)))))
         (cos (price bermudan :cos m)))
    (multiple-value-bind (v se) (price bermudan (make-engine :mc :n-paths 100000) m)
      (within-se "Bermudan LSM (quarterly dates on the grid) vs COS" v cos se 0.01d0)
      (multiple-value-bind (vi sei)
          (price bermudan (make-engine :mc :n-paths 100000 :lsm-valuation :independent) m)
        (check-that (format nil ":independent valuation ~,5F (SE ~,5F) <= in-sample ~,5F within noise, ~
<= COS ~,5F + 3 SE" vi sei v cos)
                    (and (<= vi (+ v (* 3 (sqrt (+ (* se se) (* sei sei))))))
                         (<= vi (+ cos (* 3 sei)))))))
    (check-that "Bermudan dates are grid points: the exercise mask has exactly one 1 per date"
                (let* ((grid (fincl::make-time-grid m :dates (exercise-dates (option-exercise bermudan))
                                                     :n-steps 50))
                       (mask (exercise-mask (option-exercise bermudan) m (fincl::time-grid-times grid))))
                  (and (= 4 (count 1 mask))
                       (every (lambda (d) (= 1 (sbit mask (fincl::grid-index-of grid d))))
                              (exercise-dates (option-exercise bermudan))))))
    ;; Pathwise and likelihood-ratio Greeks.
    (let* ((call (make-option (call 100) (expires expiry)))
           (exact (greeks call :black-scholes m :which '(:delta :vega)))
           (e (make-engine :mc :n-paths 100000))
           (pathwise (greeks call e m :method :pathwise))
           (errors (getf pathwise :standard-errors)))
      (dolist (key '(:delta :vega))
        (within-se (format nil "pathwise ~(~A~) of a European call vs Black-Scholes" key)
                   (getf pathwise key) (getf exact key) (getf errors key)))
      (let* ((lr (greeks call e m :method :likelihood-ratio)))
        (within-se "likelihood-ratio delta of the same call" (getf lr :delta) (getf exact :delta)
                   (getf (getf lr :standard-errors) :delta))))
    (let* ((digital (make-option (call 100) (test-digital) (expires expiry)))
           (tau (market-time m expiry))
           (sd (* 0.2d0 (sqrt tau)))
           (d2 (- (/ (log (/ (forward m expiry) 100d0)) sd) (* 0.5d0 sd)))
           (exact (/ (* (discount-factor m expiry) (fincl::norm-pdf d2)) (* 100d0 sd)))
           (lr (greeks digital (make-engine :mc :n-paths 100000) m :method :likelihood-ratio)))
      (within-se "likelihood-ratio delta of a digital vs D phi(d2) / (S sigma sqrt T)"
                 (getf lr :delta) exact (getf (getf lr :standard-errors) :delta))
      (check-error "pathwise Greeks refuse a digital"
                   (greeks digital (make-engine :mc :n-paths 1000) m :method :pathwise)
                   model-domain-error))
    (let* ((asian (make-option (call 100) (average :arithmetic :every #T"1M" :from *today*)
                               (expires expiry)))
           (e (make-engine :mc :n-paths 400000 :seed 1))
           (pathwise (greeks asian e m :method :pathwise :which '(:delta)))
           (bumped (greeks asian e m :which '(:delta))))
      (within-se "pathwise delta of an arithmetic Asian vs bump delta"
                 (getf pathwise :delta) (getf bumped :delta)
                 (sqrt (+ (expt (getf (getf pathwise :standard-errors) :delta) 2)
                          (expt (getf (getf bumped :standard-errors) :delta) 2)))))
    (let ((barrier (make-option (call 100) (barrier :down-and-out 90) (expires expiry))))
      (multiple-value-bind (lr) (greeks barrier (make-engine :mc :n-paths 100000) m
                                        :method :likelihood-ratio)
        (let ((bumped (greeks barrier :black-scholes m :which '(:delta))))
          (within-se "likelihood-ratio delta of a continuous down-and-out vs the closed form's"
                     (getf lr :delta) (getf bumped :delta) (getf (getf lr :standard-errors) :delta)))))
    (check-error "pathwise Greeks under Heston"
                 (greeks (make-option (call 100) (expires expiry))
                         (make-engine :mc :process (make-heston) :n-paths 1000) m :method :pathwise)
                 unsupported-combination)
    (check-error "a Greek the estimator does not give"
                 (greeks (make-option (call 100) (expires expiry)) (make-engine :mc :n-paths 1000) m
                         :method :likelihood-ratio :which '(:vega))
                 invalid-argument)))

(defun path-checks ()
  (grid-checks)
  (lsm-and-greek-checks)
  (sobol-reference-checks)
  (variance-reduction-checks)
  (path-payoff-checks)
  (path-feature-language-checks)
  (analytic-path-checks)
  (mc-path-checks))

(defun run-path-tests ()
  "Run only the task 19 checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (path-checks))
    (format t "~&~[All path checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
