;;;; fd.lisp --- the finite-difference engine (task 20)

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun fd-market (&key (rate 0.05d0) (dividend 0d0) (vol 0.2d0))
  (make-bs-market :valuation-date *today* :spot 100 :rate rate :dividend dividend :vol vol))

(defun one-year () (add-tenor *today* (make-tenor 1 :years)))

;;; --------------------------------------------------------------------
;;; Phase 1: GBM
;;; --------------------------------------------------------------------

(defun fd-gbm-checks ()
  (format t "~&Finite differences under GBM~%")
  (let* ((m (fd-market))
         (call (make-option (call 100) (expires (one-year))))
         (exact (price call :black-scholes m)))
    (check "European call at 200x200 vs Black-Scholes" exact (price call :fd m) 1d-4)
    (let ((errors (mapcar (lambda (n) (- (price call (make-engine :fd :n-space n :n-time n) m) exact))
                          '(100 200 400 800))))
      (format t "~&  info errors at n = 100, 200, 400, 800: ~{~,2,,,,,'eE~^ ~}~%" errors)
      (check-that (format nil "second order: error ratios on doubling ~{~,2F~^ ~} in [3.5, 4.5]"
                          (mapcar #'/ errors (rest errors)))
                  (every (lambda (r) (<= 3.5d0 r 4.5d0)) (mapcar #'/ errors (rest errors)))))
    (let ((grid (greeks call :fd m :which '(:delta :gamma :theta)))
          (closed (greeks call :black-scholes m :which '(:delta :gamma :theta))))
      (dolist (key '(:delta :gamma :theta))
        (check (format nil "grid ~(~A~) vs Black-Scholes" key) (getf closed key) (getf grid key) 1d-4))))
  (let* ((m (fd-market))
         (put (make-option (put 100) (american) (expires (one-year))))
         (bermudan (make-option (put 100) (bermudan :every #T"3M" :from *today*) (expires (one-year)))))
    (check "American put at 800x800 vs BBSR binomial at 20,000 steps"
           (price put (make-engine :binomial :n-steps 20000) m)
           (price put (make-engine :fd :n-space 800 :n-time 800) m) 1d-4)
    (check "Bermudan put (quarterly) at 1600x1600 vs COS"
           (price bermudan (make-engine :cos :n-terms 2048) m)
           (price bermudan (make-engine :fd :n-space 1600 :n-time 1600) m) 1d-5))
  (let* ((m (make-piecewise-market))
         (call (make-option (call 100) (expires #D"2027-09-24"))))
    (check "piecewise market at 400x400 vs the integrated Black price"
           (price call :black-scholes m)
           (price call (make-engine :fd :n-space 400 :n-time 400) m) 1d-4))
  ;; Barriers: a knock-out is the grid truncated at the barrier with the
  ;; rebate on it; a knock-in two coupled layers.
  (let ((m (fd-market :dividend 0.02d0 :vol 0.25d0))
        (engine (make-engine :fd :n-space 400 :n-time 400)))
    (dolist (case '((:down-and-out 90 :call 2 1d-4) (:down-and-in 90 :call 2 2d-4)
                    (:up-and-out 120 :put 2 2d-4) (:up-and-in 120 :put 0 2d-4)))
      (destructuring-bind (type level kind rebate tolerance) case
        (let ((o (make-option-from-clauses `((,kind 100) (:barrier ,type ,level :rebate ,rebate)
                                             (:expires ,(one-year))))))
          (check (format nil "continuous ~(~A~) ~D ~(~A~), rebate ~D, vs Reiner-Rubinstein" type level kind rebate)
                 (price o :black-scholes m) (price o engine m) tolerance))))
    ;; Monthly monitoring, the expiry included: the up barrier knocks out
    ;; payoffs in the money at expiry.
    (let ((monthly (iter (for k from 1 to 12) (collect (add-tenor *today* (make-tenor k :months)))))
          (vanilla (price (make-option (call 100) (expires (one-year))) :black-scholes m)))
      (dolist (barrier '((:down-and-out :down-and-in 90) (:up-and-out :up-and-in 120)))
        (destructuring-bind (out-type in-type level) barrier
          (let ((out (make-option-from-clauses `((:call 100) (:barrier ,out-type ,level :monitoring ,monthly)
                                                 (:expires ,(one-year)))))
                (in (make-option-from-clauses `((:call 100) (:barrier ,in-type ,level :monitoring ,monthly)
                                                (:expires ,(one-year))))))
            (check (format nil "monthly ~(~A~) + ~(~A~) = European" in-type out-type)
                   vanilla (+ (price out engine m) (price in engine m)) 1d-3)
            (with-pricing-kernel ()
              (multiple-value-bind (v se) (price out (make-engine :mc :n-paths 200000) m)
                (within-se (format nil "monthly ~(~A~) vs Monte Carlo" out-type) (price out engine m) v se))))))))
  (let ((m (fd-market)))
    (check-that "automatic selection prices an American under CEV and local vol by FD"
                (and (typep (nth-value 2 (price (make-option (put 100) (american) (expires (one-year)))
                                                (make-cev :beta 0.7d0) m))
                            'fd-engine)
                     (typep (nth-value 2 (price (make-option (put 100) (american) (expires (one-year)))
                                                (make-local-vol) m))
                            'fd-engine)))
    (check-error "a path payoff" (price (make-option (call 100) (average :arithmetic :every #T"1M" :from *today*)
                                                     (expires (one-year)))
                                        :fd m)
                 unsupported-combination)
    (check-error "a process with jumps" (price (make-option (put 100) (expires (one-year)))
                                               (make-engine :fd :process (make-merton-jump)) m)
                 unsupported-combination)))

;;; --------------------------------------------------------------------
;;; Phase 2: CEV and local vol
;;; --------------------------------------------------------------------

(defun fd-cev-local-vol-checks ()
  (format t "~&Finite differences under CEV and local vol~%")
  (let ((m (fd-market :dividend 0.01d0 :vol 0.25d0)))
    (check-that "CEV European at 400x400 within 1e-4 of Schroder's formula (beta 0.5, 0.8; K 80, 100, 120)"
                (iter outer
                      (for beta in '(0.5d0 0.8d0))
                      (iter (for k in '(80 100 120))
                            (for o = (make-option (call k) (expires (one-year))))
                            (for cev = (make-cev :beta beta))
                            (in outer (always (< (abs (- (price o (make-engine :fd :process cev :n-space 400 :n-time 400) m)
                                                         (price o (make-engine :cev :process cev) m)))
                                                 1d-4)))))))
  (let* ((ssvi (make-ssvi-surface '(0.25 0.5 1 2) '(0.0121 0.0225 0.04 0.072) :rho -0.6 :eta 1.2 :gamma 0.4))
         (m (make-bs-market :valuation-date *today* :spot 100 :rate 0.04d0 :dividend 0.01d0 :vol ssvi))
         (engine (make-engine :fd :process (make-local-vol) :n-space 800 :n-time 800))
         (worst 0d0))
    (check-that "local vol at 800x800 reprices 15 vanillas of an SSVI surface within 5e-4"
                (iter outer
                      (for days in '(91 182 365))
                      (iter (for k in '(80 90 100 110 120))
                            (for o = (make-option-from-clauses
                                      `((,(if (< k 100) :put :call) ,k) (:expires ,(add-tenor *today* (make-tenor days :days))))))
                            (for error = (abs (- (price o engine m) (price o :black-scholes m))))
                            (setf worst (max worst error))
                            (in outer (always (< error 5d-4))))))
    (format t "~&  info worst repricing error ~,2,,,,,'eE~%" worst)
    (let* ((expiry (one-year))
           (days (iter (for d in-dates (add-tenor *today* (make-tenor 1 :days)) to expiry) (collect d))))
      (with-pricing-kernel ()
        (multiple-value-bind (v se)
            (price (make-option (put 100) (bermudan :dates days))
                   (make-engine :mc :process (make-local-vol) :n-paths 100000 :n-steps 365) m)
          (within-se "local-vol American put (FD) vs LSM on daily dates"
                     (price (make-option (put 100) (american) (expires expiry))
                            (make-engine :fd :process (make-local-vol) :n-space 400 :n-time 400) m)
                     v se 0.01d0))))))

;;; --------------------------------------------------------------------
;;; Phase 3: Heston ADI
;;; --------------------------------------------------------------------

(defun fd-heston-checks ()
  (format t "~&Heston by Hundsdorfer-Verwer ADI~%")
  (let ((m (fd-market :rate 0.04d0 :dividend 0.01d0)))
    (dolist (params '((:v0 0.04d0 :theta 0.04d0 :kappa 2d0 :xi 0.3d0 :rho -0.7d0)
                      (:v0 0.0625d0 :theta 0.16d0 :kappa 5d0 :xi 0.9d0 :rho 0.1d0)
                      ;; 2 kappa theta = 0.18 < xi^2 = 0.2025: violates Feller's condition.
                      (:v0 0.09d0 :theta 0.09d0 :kappa 1d0 :xi 0.45d0 :rho -0.5d0)))
      (let* ((heston (apply #'make-heston params))
             (errors (mapcar (lambda (k)
                               (let ((o (make-option (put k) (expires (one-year)))))
                                 (- (price o (make-engine :fd :process heston :n-space 200 :n-variance 100
                                                              :n-time 100)
                                           m)
                                    (price o (make-engine :cos :process heston :n-terms 2048) m))))
                             '(80 100 120))))
        (check-that (format nil "~A at 200x100x100: puts K 80, 100, 120 within 1e-3 of COS (~{~,1,,,,,'eE~^ ~})"
                            params errors)
                    (every (lambda (e) (< (abs e) 1d-3)) errors)))))
  ;; Clarke and Parrott's American puts: K = 10, T = 0.25, r = 0.1, kappa 5,
  ;; theta 0.16, xi 0.9, rho 0.1, v0 0.0625; references as in mc.lisp.
  (let ((heston (make-heston :v0 0.0625d0 :kappa 5d0 :theta 0.16d0 :xi 0.9d0 :rho 0.1d0))
        (seconds 0d0))
    (iter (for (spot reference) in '((8 2.0000d0) (9 1.1076d0) (10 0.5199d0) (11 0.2135d0) (12 0.0820d0)))
          (let ((m (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                   :spot spot :rate 0.1d0 :vol 0.25d0))
                (put (make-option (put 10) (american) (expires (date+ *today* 90))))
                (start (get-internal-real-time)))
            (check (format nil "Clarke-Parrott American put, S = ~D, at 100x50x100" spot)
                   reference
                   (price put (make-engine :fd :process heston :n-space 100 :n-variance 50 :n-time 100) m)
                   1d-3)
            (setf seconds (max seconds (/ (- (get-internal-real-time) start) internal-time-units-per-second 1d0)))))
    (check-that (format nil "runtime at 100x50x100: ~,2F s per price (under 2 s)" seconds)
                (< seconds 2d0))
    (check-that "automatic selection prices a Heston American by FD"
                (typep (nth-value 2 (price (make-option (put 100) (american) (expires (one-year)))
                                           heston (fd-market)))
                       'fd-engine))
    (check-error "Bates (jumps) is refused"
                 (price (make-option (put 100) (expires (one-year)))
                        (make-engine :fd :process (make-bates)) (fd-market))
                 unsupported-combination)))

(defun fd-checks ()
  (fd-gbm-checks)
  (fd-cev-local-vol-checks)
  (fd-heston-checks))

(defun run-fd-tests ()
  "Run only the task 20 checks. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (fd-checks))
    (format t "~&~[All FD checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
