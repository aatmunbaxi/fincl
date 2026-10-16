;;;; selection.lisp --- process constructors, engine designators, automatic
;;;; selection, the option constructor's name (task 25)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun ls-market ()
  "Longstaff and Schwartz's put market: S = 36, r = 6%, vol 20%."
  (make-bs-market :valuation-date *today* :spot 36 :rate 0.06d0 :vol 0.2d0))

(defun ls-put () (make-option (put 40) (american) (expires #D"2027-09-24")))

(defun engine-key (engine) (second (fincl::engine-form engine)))

;;; --------------------------------------------------------------------
;;; Phase 1: constructors, designators, printing, the option rename
;;; --------------------------------------------------------------------

(defun constructor-checks ()
  (format t "~&Process constructors~%")
  (check-that "every concrete process has MAKE-<class>"
              (every #'fboundp '(make-gbm make-cev make-heston make-garch make-merton-jump
                                 make-kou-jump make-variance-gamma make-nig make-cgmy make-bates)))
  (check-that "the abstract JUMP-DIFFUSION has none" (not (fboundp 'fincl::make-jump-diffusion)))
  (check-that "a constructor builds the process it names, defaults intact"
              (let ((h (make-heston :xi 0.5d0)))
                (and (typep h 'heston) (= 0.5d0 (process-parameter h :xi))
                     (= 2d0 (process-parameter h :kappa)))))
  (check-that "a subclass's constructor takes its superclass's parameters"
              (= 0.2d0 (process-parameter (make-bates :xi 0.2d0 :intensity 1) :xi)))
  (check-that "a test process gets its constructor too" (typep (make-toy-base :a 2) 'toy-base))
  (check-error "a constructor that would replace another function"
               (eval '(progn (defun make-collider () :mine)
                             (define-process collider (stochastic-process) () ())))
               constructor-collision)
  (check-that "redefining a process keeps its constructor"
              (progn (eval '(define-process redefined (stochastic-process) () ()))
                     (eval '(define-process redefined (stochastic-process) () ()))
                     (typep (funcall 'make-redefined) 'redefined))))

(defun option-name-checks ()
  (format t "~&The option constructor~%")
  (let ((d #D"2027-09-24"))
    (check-that "MAKE-OPTION builds what MAKE-OPTION-FROM-CLAUSES builds from data"
                (equal (object-clauses 'option (make-option (put 95) (american) (expires d)))
                       (object-clauses 'option (make-option-from-clauses
                                                `((:put 95) (:american) (:expires ,d))))))
    (let ((o (make-option (call 100) (expires d))))
      (check-that "options print readably as (make-option ...)"
                  (search "make-option (:call 100"
                          (string-downcase (let ((*print-readably* t)) (prin1-to-string o)))))
      (check-that "and the printed form round-trips" (round-trip-p 'option o)))
    (let ((message (error-message (lambda () (eval '(make-option '((:call 100) (:expires #D"2027-09-24"))))))))
      (check-that "an old-style call signals MALFORMED-CLAUSE naming MAKE-OPTION-FROM-CLAUSES"
                  (and message (search "make-option-from-clauses" message))))))

(defun designator-checks ()
  (format t "~&Engine designators~%")
  (let* ((m (ls-market)) (put (ls-put))
         (call (make-option (call 40) (expires #D"2027-09-24"))))
    (check-that "a keyword names one shared engine"
                (eq (fincl::ensure-engine :black-scholes) (fincl::ensure-engine :black-scholes)))
    (check-that "MAKE-ENGINE equals MAKE-INSTANCE with the same settings, both values"
                (every (lambda (case)
                         (destructuring-bind (option key class &rest settings) case
                           (equal (subseq (multiple-value-list (price option (apply #'make-engine key settings) m)) 0 2)
                                  (subseq (multiple-value-list (price option (apply #'make-instance class settings) m)) 0 2))))
                       `((,call :black-scholes black-scholes-engine)
                         (,put :baw barone-adesi-whaley-engine)
                         (,put :binomial binomial-engine :n-steps 501)
                         (,put :cos cos-engine :n-terms 128)
                         (,put :mc monte-carlo-engine :n-paths 4000 :n-steps 20)
                         (,call :merton-series merton-series-engine :process ,(make-merton-jump :intensity 1))
                         (,call :cev cev-engine :process ,(make-cev :beta 0.7d0)))))
    (check-that "PRICE returns the engine that priced"
                (typep (nth-value 2 (price put :binomial m)) 'binomial-engine))
    (check "a list designator is data" (price put (make-engine :cos :n-terms 512) m)
           (price put '(:cos :n-terms 512) m) 0)
    (check-error "an unknown designator" (make-engine :nope) invalid-argument)
    (check-error "a quoted, unevaluated process form" (price put '(:cos :process (make-heston)) m)
                 invalid-argument)
    (check-error "a process an analytic engine cannot model"
                 (make-engine :black-scholes :process (make-heston)) invalid-argument)
    (check-error "an unknown setting" (make-engine :binomial :n-stepz 3) invalid-argument)
    (check-that "every engine prints as a MAKE-ENGINE form that rebuilds it, with and without settings"
                (every (lambda (engine)
                         (let ((form (fincl::engine-form engine)))
                           (and (search "(make-engine" (prin1-to-string engine))
                                (equal form (fincl::engine-form (eval form))))))
                       (list (make-engine :black-scholes) (make-engine :baw)
                             (make-engine :binomial) (make-engine :binomial :n-steps 4000 :smooth nil)
                             (make-engine :cos :process (make-heston :xi 0.5d0) :n-terms 512)
                             (make-engine :mc :n-paths 1000 :antithetic nil)
                             (make-engine :merton-series :process (make-merton-jump :intensity 2))
                             (make-engine :cev :process (make-cev :beta 0.3d0)))))))

;;; --------------------------------------------------------------------
;;; Phase 2: automatic selection, overrides, Greeks
;;; --------------------------------------------------------------------

(defun auto-price (option designator market)
  "PRICE's three values with ENGINE-FALLBACK muffled."
  (handler-bind ((engine-fallback #'muffle-warning))
    (price option designator market)))

(defun selection-checks ()
  (format t "~&Automatic engine selection~%")
  (let* ((m (ls-market)) (put (ls-put))
         (tree (price put :binomial m)))
    (dolist (designator (list :auto (make-gbm)))
      (multiple-value-bind (v se engine) (price put designator m)
        (declare (ignore se))
        (check-that (format nil "~S selects the binomial tree and agrees with it" (if (keywordp designator) designator 'make-gbm))
                    (and (eq (engine-key engine) :binomial) (= v tree)))))
    (dolist (process (list (make-merton-jump :intensity 0) (make-kou-jump :intensity 0)
                           (make-variance-gamma :nu 1d-8 :theta 0d0)))
      (multiple-value-bind (v se engine) (price put process m)
        (declare (ignore se))
        (check-that (format nil "~(~A~), no jumps: COS, within 1e-4 of the tree" (class-name (class-of process)))
                    (and (eq (engine-key engine) :cos) (< (abs (- v tree)) 1d-4)))))
    (let ((warned nil))
      (multiple-value-bind (v se engine)
          (handler-bind ((engine-fallback (lambda (c) (setf warned t) (muffle-warning c))))
            (price put (make-cev :beta 0.5d0) m))
        (declare (ignore v se))
        (check-that "CEV American falls back to Monte Carlo with ENGINE-FALLBACK"
                    (and warned (eq (engine-key engine) :mc)))))
    (check-error "(:fallback nil) refuses the fallback"
                 (with-engines ((:fallback nil)) (price put (make-cev :beta 0.5d0) m))
                 unsupported-combination)
    ;; Overrides: by style, by process class, by both; inner shadows outer.
    (check-that "an override by exercise style"
                (with-engines ((:american (make-engine :cos :richardson-dates 8)))
                  (eq (engine-key (nth-value 2 (price put :auto m))) :cos)))
    (check-that "an override by process class"
                (with-engines ((merton-jump :mc))
                  (eq (engine-key (nth-value 2 (auto-price (make-option (call 40) (expires #D"2027-09-24"))
                                                           (make-merton-jump) m)))
                      :mc)))
    (check-that "an override by style and class applies only to both"
                (with-engines (((:american gbm) :baw))
                  (and (eq (engine-key (nth-value 2 (price put :auto m))) :baw)
                       (eq (engine-key (nth-value 2 (price (make-option (call 40) (expires #D"2027-09-24"))
                                                         :auto m)))
                           :black-scholes))))
    (check-that "an inner override shadows an outer one"
                (with-engines ((:american :baw))
                  (with-engines ((:american :cos))
                    (eq (engine-key (nth-value 2 (price put :auto m))) :cos))))
    ;; The engine chosen prices exactly as itself.
    (check-that "an automatic price is = to its engine's own price"
                (every (lambda (case)
                         (destructuring-bind (option process) case
                           (multiple-value-bind (v se engine) (auto-price option process m)
                             (declare (ignore se))
                             (= v (price option engine m)))))
                       `((,put ,(make-gbm)) (,put ,(make-kou-jump))
                         (,(make-option (call 40) (expires #D"2027-09-24")) ,(make-heston))
                         (,(make-option (call 40) (expires #D"2027-09-24")) ,(make-cev :beta 0.6d0)))))
    ;; Greeks and implied vol select once.
    (let* ((call (make-option (call 40) (expires #D"2027-09-24")))
           (m (make-market :valuation-date *today*
                           :underlyings '((:x :spot 36 :vol 0.2d0)) :curves '((:usd :rate 0.06d0))))
           (fincl::*selections* 0))
      (greeks call (make-heston :xi 0.4d0) m :which '(:delta :gamma :vega :rho))
      (check "GREEKS under a process selects an engine once" 1 fincl::*selections* 0)
      (check "IMPLIED-VOLATILITY with :auto" (implied-volatility (ls-put) 4.48668d0 :binomial m)
             (implied-volatility (ls-put) 4.48668d0 :auto m) 1d-12))))

;;; --------------------------------------------------------------------
;;; Phase 3: degenerate latent state in LSM
;;; --------------------------------------------------------------------

(defun degenerate-lsm-checks ()
  (format t "~&LSM with a latent state that never moves~%")
  (let* ((m (ls-market)) (put (ls-put))
         (gbm (multiple-value-list (price put (make-engine :mc :n-paths 100000) m))))
    (dolist (process (list (make-heston :v0 0.04d0 :theta 0.04d0 :xi 0d0)
                           (make-bates :intensity 0 :v0 0.04d0 :theta 0.04d0 :xi 0d0)
                           (make-garch :period 1/50 :alpha 0d0 :beta 0d0 :gamma 0d0
                                       :omega (* 0.04d0 1/50) :h0 (* 0.04d0 1/50))))
      (multiple-value-bind (v se) (price put (make-engine :mc :n-paths 100000 :process process) m)
        (check-that (format nil "~(~A~): LSM within 3 SE of GBM's (~,5F vs ~,5F, SE ~,5F)"
                            (class-name (class-of process)) v (first gbm) se)
                    (< (abs (- v (first gbm))) (* 3 (sqrt (+ (* se se) (expt (second gbm) 2))))))))))

(defun selection-test-checks ()
  (constructor-checks)
  (option-name-checks)
  (designator-checks)
  (selection-checks)
  (degenerate-lsm-checks))

(defun run-selection-tests ()
  "Run only the checks of this file. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (selection-test-checks))
    (format t "~&~[All selection checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
