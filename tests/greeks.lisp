;;;; greeks.lisp --- sensitivities from every engine

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun greek-market (&key (spot 100) (rate 0.05d0) (dividend 0.02d0) (vol 0.2d0))
  (make-bs-market :valuation-date *today* :spot spot :rate rate :dividend dividend :vol vol))

(defun bs-reference-greeks (kind strike &key (spot 100d0) (rate 0.05d0) (dividend 0.02d0)
                                             (vol 0.2d0))
  "Ridders' derivatives of the closed-form price in each input: the reference
for every Greek method."
  (let ((option (make-option `((,kind ,strike) (:expires #D"2027-09-24"))))
        (bs (make-instance 'black-scholes-engine)))
    (flet ((price-with (&key (s spot) (r rate) (q dividend) (v vol))
             (nth-value 0 (price option bs (greek-market :spot s :rate r :dividend q :vol v)))))
      `(:delta ,(derivative (lambda (s) (price-with :s s)) spot)
        :gamma ,(derivative (lambda (s) (price-with :s s)) spot :order 2)
        :vega ,(derivative (lambda (v) (price-with :v v)) vol :h0 0.05d0)
        :volga ,(derivative (lambda (v) (price-with :v v)) vol :order 2 :h0 0.05d0)
        :rho ,(derivative (lambda (r) (price-with :r r)) rate :h0 0.05d0)
        :dividend-rho ,(derivative (lambda (q) (price-with :q q)) dividend :h0 0.05d0)
        :vanna ,(derivative (lambda (v) (derivative (lambda (s) (price-with :s s :v v)) spot))
                            vol :h0 0.05d0)))))

(defun greeks-agree-p (expected actual tolerance
                       &key (keys (loop for k in expected by #'cddr collect k)) overrides)
  "Every key of EXPECTED within TOLERANCE x max(1, |expected|) in ACTUAL, or
the key's own tolerance in the plist OVERRIDES; prints the offenders."
  (let ((ok t))
    (dolist (key keys ok)
      (let ((e (getf expected key)) (a (getf actual key))
            (tolerance (getf overrides key tolerance)))
        (unless (<= (abs (- a e)) (* tolerance (max 1d0 (abs e))))
          (setf ok nil)
          (format t "~&    ~(~A~): expected ~,10F, got ~,10F~%" key e a))))))

(defun greeks-checks ()
  (format t "~&Greeks~%")
  (let* ((m (greek-market))
         (bs (make-instance 'black-scholes-engine))
         (d #D"2027-09-24"))
    (dolist (kind '(:call :put))
      (let* ((o (make-option `((,kind 105) (:expires ,d))))
             (closed (greeks o bs m))
             (bumped (bump-greeks o bs m))
             (reference (bs-reference-greeks kind 105d0)))
        (check-that (format nil "BS ~(~A~): closed form = Ridders' derivatives of the price to 1e-8" kind)
                    (greeks-agree-p reference closed 1d-8))
        ;; A second difference in the vol with h = 1e-4 has an error floor
        ;; of about 1e-6 absolute: BLACK-FORMULA's ~1e-14 rounding over h^2.
        ;; Volga crosses zero near K = 105 (-0.23), so its floor shows there.
        (check-that (format nil "BS ~(~A~): closed form = bump Greeks to 1e-6 relative ~
(volga to 5e-6 absolute, near its zero)" kind)
                    (greeks-agree-p closed bumped 1d-6 :overrides '(:volga 5d-6)))))
    ;; Theta: one calendar day of aging; compare with the instantaneous rate.
    (let* ((call (option (call 100) (expires d)))
           (theta (getf (greeks call bs m :which '(:theta)) :theta))
           (s 100d0) (k 100d0) (r 0.05d0) (q 0.02d0) (vol 0.2d0) (tau 1d0)
           (d1 (/ (+ (log (/ s k)) (* (+ (- r q) (* 0.5d0 vol vol)) tau)) (* vol (sqrt tau))))
           (d2 (- d1 (* vol (sqrt tau))))
           (rate-per-year (+ (- (/ (* s (exp (- (* q tau))) (norm-pdf d1) vol) (* 2 (sqrt tau))))
                             (- (* r k (exp (- (* r tau))) (norm-cdf d2)))
                             (* q s (exp (- (* q tau))) (norm-cdf d1)))))
      (check "1y ATM call theta (one day of aging) = bump theta" theta
             (getf (bump-greeks call bs m :which '(:theta)) :theta) 1d-12)
      ;; One day of aging differs from the instantaneous rate x one day by
      ;; about theta' dt^2 / 2.
      (check (format nil "... is negative and within 1e-5 of the instantaneous rate x 1/365 ~
(gap ~,1E)" (abs (- theta (/ rate-per-year 365))))
             (/ rate-per-year 365) theta 1d-5)
      (check-that "theta is negative" (minusp theta)))

    (format t "~&Greeks from a binomial tree~%")
    (let* ((no-dividend (greek-market :dividend 0))
           (acall (option (call 105) (american) (expires d)))
           (european (option (call 105) (expires d)))
           (tree (binomial 2000))
           (from-tree (greeks acall tree no-dividend))
           (closed (greeks european bs no-dividend)))
      (check-that "American call without dividends (= European): tree Greeks = BS to 1e-4"
                  (greeks-agree-p closed from-tree 1d-4
                                  :keys '(:delta :gamma :vega :theta :rho :dividend-rho)))
      (check-that "the tree's default Greeks leave out vanna and volga"
                  (not (or (getf from-tree :vanna) (getf from-tree :volga)))))
    (let* ((aput (option (put 105) (american) (expires d)))
           (tree (binomial 2000)))
      (check "American put: node delta = bumped-tree delta to 1e-4"
             (getf (bump-greeks aput tree m :which '(:delta)) :delta)
             (getf (greeks aput tree m :which '(:delta)) :delta)
             1d-4))

    (format t "~&Greeks by Monte Carlo~%")
    (let* ((call (option (call 100) (expires d)))
           (result (greeks call (mc :n-paths 200000) m :which '(:delta :vega)))
           (delta (getf result :delta))
           (se (getf (getf result :standard-errors) :delta))
           (exact (getf (greeks call bs m :which '(:delta)) :delta)))
      (check-that (format nil "European delta at 200k paths: SE ~,5F < 2e-3" se) (< se 2d-3))
      (check (format nil "... within 3 SE of BS") exact delta (* 3 se)))))
