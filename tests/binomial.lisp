;;;; binomial.lisp --- the Cox-Ross-Rubinstein binomial engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun binomial-checks ()
  (let* ((bs (make-instance 'black-scholes-engine))
         (cos (make-instance 'cos-engine))
         (m (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                            :dividend 0.03d0 :vol 0.25d0))
         (ls (make-bs-market :valuation-date *today* :spot 36 :rate 0.06d0 :vol 0.2d0))
         (aput (make-option '(:american :put 40 #D"2027-09-24")))
         (acall (make-option '(:american :call 100 #D"2027-09-24"))))

    (format t "~&Binomial: American under GBM~%")
    (check "American put = COS" (price aput cos ls) (price aput (binomial 8000) ls) 2d-5)
    (check "American put, Longstaff-Schwartz benchmark 4.478" 4.478
           (price aput (binomial 2000) ls) 0.01)
    (check "American call with dividends = COS"
           (price acall cos m) (price acall (binomial 8000) m) 2d-5)
    (let ((no-div (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.25d0)))
      (check "American call without dividends = European"
             (price (make-option '(:call 100 #D"2027-09-24")) bs no-div)
             (price acall (binomial 8000) no-div)
             1d-4))
    ;; Smoothing the last step removes the odd-even oscillation of the
    ;; plain tree, which dominates its error at these N.
    (let ((exact (price aput cos ls)))
      (dolist (n '(500 1000))
        (check-that (format nil "BBS tree beats plain CRR at N = ~D" n)
                    (< (abs (- exact (price aput (binomial n :extrapolate nil) ls)))
                       (abs (- exact (price aput (binomial n :smooth nil :extrapolate nil)
                                            ls))))))
      (check-that "BBSR is exactly 2 v(N) - v(N/2)"
                  (= (price aput (binomial 1000) ls)
                     (- (* 2 (price aput (binomial 1000 :extrapolate nil) ls))
                        (price aput (binomial 500 :extrapolate nil) ls))))
      (check-that "error falls with N (250, 8000)"
                  (< (abs (- exact (price aput (binomial 8000) ls)))
                     (abs (- exact (price aput (binomial 250) ls))))))

    (format t "~&Binomial: Bermudan under GBM~%")
    (let* ((payoff (make-instance 'vanilla-payoff :kind :put :strike 100d0))
           (quarterly (make-instance 'option
                                     :payoff payoff
                                     :exercise (make-instance
                                                'bermudan-exercise
                                                :dates (list #D"2026-12-24" #D"2027-03-24"
                                                             #D"2027-06-24" #D"2027-09-24")))))
      ;; 7300 steps over 365 days puts every exercise date on the grid, for
      ;; both the N and N/2 trees.
      (check "quarterly Bermudan = COS"
             (price quarterly cos m) (price quarterly (binomial 7300) m) 1d-4)
      (check-that "Bermudan error falls with N (730, 7300)"
                  (< (abs (- (price quarterly cos m)
                             (price quarterly (binomial 7300 :extrapolate nil) m)))
                     (abs (- (price quarterly cos m)
                             (price quarterly (binomial 730 :extrapolate nil) m)))))
      (check "single-date Bermudan = European"
             (price (make-option '(:put 100 #D"2027-09-24")) bs m)
             (price (make-instance 'option
                                   :payoff payoff
                                   :exercise (make-instance 'bermudan-exercise
                                                            :dates (list #D"2027-09-24")))
                    (binomial 3650) m)
             1d-4))

    (format t "~&Binomial: conventions, term structure and errors~%")
    (let* ((scale (/ 360d0 365d0))
           (m360 (make-bs-market :valuation-date *today* :day-counter +actual-360+
                                 :spot 100 :rate (* 0.05d0 scale)
                                 :dividend (* 0.03d0 scale) :vol (* 0.25d0 (sqrt scale)))))
      (check "American put: ACT/365F = rescaled ACT/360"
             (price (make-option '(:american :put 100 #D"2027-06-18")) (binomial 2000) m)
             (price (make-option '(:american :put 100 #D"2027-06-18")) (binomial 2000) m360)
             1d-10))
    (check-error "European under GBM is left to Black-Scholes"
                 (price (make-option '(:put 100 #D"2027-09-24")) (binomial 1000) m)
                 unsupported-combination)
    (check-error "American under Heston"
                 (price aput (binomial 1000 :process (make-instance 'heston)) ls)
                 unsupported-combination)
    (check-error "too few steps for the drift"
                 (price aput (binomial 2 :extrapolate nil)
                        (make-bs-market :valuation-date *today* :spot 36 :rate 3
                                        :vol 0.05d0))
                 invalid-tree)))

(defun run-binomial-tests ()
  "Run only the binomial engine checks. Returns T when all pass."
  (let ((*failures* 0))
    (binomial-checks)
    (format t "~&~[All binomial checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
