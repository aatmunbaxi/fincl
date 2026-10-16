;;;; market.lisp --- the market protocol

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun market-checks ()
  "Protocol functions on a flat market."
  (with-standard-fixtures
    (format t "~&Market protocol~%")
    (check "market time to 2027-09-24" 1d0 (market-time mkt #D"2027-09-24") 0)
    (check "discount factor" (exp -0.05d0) (discount-factor mkt 1d0) 1d-14)
    (check "discount factor, date" (exp -0.05d0)
           (discount-factor mkt #D"2027-09-24") 1d-14)
    (check "forward" (* 100 (exp 0.05d0)) (forward mkt 1d0) 1d-12)
    (check "forward, date" (* 100 (exp 0.05d0))
           (forward mkt #D"2027-09-24") 1d-12)
    (check "black vol" 0.2d0 (black-vol mkt 100d0 1d0) 1d-15)
    (check "black variance, date" 0.04d0
           (black-variance mkt 100d0 #D"2027-09-24") 1d-15)))

;;; --------------------------------------------------------------------
;;; Composite markets, views and bumps (task 11)
;;; --------------------------------------------------------------------

(defun two-index-market ()
  "SPX and NDX in USD on one OIS curve, and a EUR curve for EUR-paying
instruments."
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :currency :usd :dividend 0.015d0 :vol 0.22d0)
                              (:ndx :spot 150 :currency :usd :vol 0.3d0))
               :curves '((:usd-ois :rate 0.04d0) (:eur-estr :rate 0.02d0))
               :discounting '((:usd . :usd-ois) (:eur . :eur-estr))))

(defun market-engines ()
  `(,(make-instance 'black-scholes-engine) ,(make-instance 'barone-adesi-whaley-engine)
    ,(binomial 500) ,(make-instance 'cos-engine) ,(mc :n-paths 4000 :n-steps 20)))

(defun composite-market-checks ()
  (format t "~&Composite markets and views~%")
  (let* ((m (two-index-market))
         (spx-alone (make-bs-market :valuation-date *today* :spot 100 :rate 0.04d0
                                    :dividend 0.015d0 :vol 0.22d0))
         (ndx-alone (make-bs-market :valuation-date *today* :spot 150 :rate 0.04d0 :vol 0.3d0))
         (d #D"2027-09-24"))
    ;; Each underlying of one market prices exactly as its own market.
    (iter (for (name alone strike) in `((:spx ,spx-alone 100) (:ndx ,ndx-alone 150)))
          (dolist (style '(:european :american))
            (dolist (engine (market-engines))
              (let ((named (make-option-from-clauses `((:call ,strike) (:underlying ,name) (,style)
                                          (:expires ,d))))
                    (plain (make-option-from-clauses `((:call ,strike) (,style) (:expires ,d)))))
                (handler-case
                    (check (format nil "~(~A~) ~(~A~) call, ~(~A~): two-underlying market = ~
its own market" name style (class-name (class-of engine)))
                           (price plain engine alone) (price named engine m) 0)
                  (unsupported-combination () nil))))))
    (let ((view (market-view m :underlying :ndx)))
      (check-that "a view answers the single-underlying protocol"
                  (and (= 150d0 (spot view))
                       (= 0.04d0 (zero-rate view 1d0))
                       (= 0d0 (dividend-yield view 1d0))
                       (= 0.3d0 (black-vol view 100d0 1d0))
                       (= (exp -0.04d0) (discount-factor view d))
                       (date= *today* (valuation-date view))))
      (check-error "derive-market on a view" (derive-market view :valuation-date d)
                   invalid-argument))
    (let ((view (market-view m :underlying :spx :discount-curve :eur-estr)))
      (check-that "a view discounting on another curve keeps the underlying's forward"
                  (and (= (exp -0.02d0) (discount-factor view 1d0))
                       (= (forward spx-alone 1d0) (forward view 1d0))))
      (check "pricing on that view: Black with the forward and the other discount factor"
             (fincl::black-formula 1d0 (forward spx-alone 1d0) 100d0 (exp -0.02d0)
                                   (black-variance spx-alone 100d0 1d0))
             (price (make-option (call 100) (expires d)) (make-instance 'black-scholes-engine) view)
             0))
    (check-error "an option paying in another currency than its underlying's (a quanto)"
                 (price (make-option (call 100) (underlying :spx) (currency :eur) (expires d))
                        (make-instance 'black-scholes-engine) m)
                 unsupported-quanto)
    (check "an option naming its underlying's own currency"
           (price (make-option (call 100) (expires d)) (make-instance 'black-scholes-engine) spx-alone)
           (price (make-option (call 100) (underlying :spx) (currency :usd) (expires d))
                  (make-instance 'black-scholes-engine) m)
           0)
    (check-that "aging copies the market and leaves the original unchanged"
                (let ((aged (derive-market m :valuation-date #D"2027-03-24")))
                  (and (date= #D"2027-03-24" (valuation-date aged))
                       (date= *today* (valuation-date m))
                       (= 150d0 (spot (market-view aged :underlying :ndx))))))
    (check-that "the market prints its underlyings and curves"
                (let ((printed (prin1-to-string m)))
                  (every (lambda (s) (search s printed))
                         '(":SPX" ":NDX" ":USD-OIS" ":EUR-ESTR" "vol=0.2200" "r=0.0400"))))
    (check-error "a non-positive spot"
                 (make-market :valuation-date *today* :underlyings '((:x :spot -1))
                              :curves '((:c :rate 0)))
                 invalid-argument)
    (check-error "discounting on a curve the market does not hold"
                 (make-market :valuation-date *today* :underlyings '((:x :spot 1))
                              :curves '((:c :rate 0)) :discounting '((:default . :nope)))
                 invalid-argument)
    (check-error "a view of a market with no vol for the underlying, asked for a vol"
                 (black-vol (make-market :valuation-date *today* :underlyings '((:x :spot 1))
                                         :curves '((:c :rate 0)))
                            1d0 1d0)
                 missing-market-data)

    (format t "~&Instruments name their underlyings~%")
    (let ((bs (make-instance 'black-scholes-engine)))
      (check-error "no underlying on a two-underlying market"
                   (price (make-option (call 100) (expires d)) bs m)
                   ambiguous-underlying)
      (let ((c (handler-case (price (make-option (call 100) (underlying :dax) (expires d)) bs m)
                 (error (c) c))))
        (check-that "an unknown underlying names the known ones"
                    (and (typep c 'missing-market-data)
                         (search ":SPX, :NDX" (princ-to-string c)))))
      (check-that "SPX and NDX options from one market"
                  (and (= (price (make-option (call 100) (expires d)) bs spx-alone)
                          (price (make-option (call 100) (underlying :spx) (expires d)) bs m))
                       (= (price (make-option (put 140) (expires d)) bs ndx-alone)
                          (price (make-option (put 140) (underlying :ndx) (expires d)) bs m))))
      (check-that "the underlying clause prints and round-trips"
                  (let ((o (make-option (call 100) (underlying :spx) (expires d))))
                    (and (search "(underlying :spx)" (prin1-to-string o))
                         (round-trip-p 'option o))))
      (check-that "make-vanilla-option takes :underlying"
                  (eq :ndx (instrument-underlying (make-vanilla-option :call 1 d :underlying :ndx)))))

    (format t "~&Bumps~%")
    (let* ((bs (make-instance 'black-scholes-engine))
           (spx-call (make-option (call 100) (underlying :spx) (expires d)))
           (ndx-call (make-option (call 150) (underlying :ndx) (expires d)))
           (up (bump m '(:spot :spx) '(:relative 1/100))))
      (check-that "a relative spot bump moves only that underlying's spot"
                  (and (= 101d0 (spot (market-view up :underlying :spx)))
                       (= 150d0 (spot (market-view up :underlying :ndx)))
                       (= 100d0 (spot (market-view m :underlying :spx)))))
      (check "an NDX option is unchanged by an SPX bump"
             (price ndx-call bs m) (price ndx-call bs up) 0)
      ;; Central differences: the O(h^2) error is below 1e-9 here.
      (let* ((h 1d-4) (s 100d0) (q 0.015d0) (r 0.04d0) (vol 0.22d0) (k 100d0)
             (d1 (/ (+ (log (/ s k)) (* (+ (- r q) (* 0.5d0 vol vol)) 1d0)) vol))
             (d2 (- d1 vol)))
        (check "delta from spot bumps = e^-qT N(d1)"
               (* (exp (- q)) (norm-cdf d1))
               (/ (- (price spx-call bs (bump m '(:spot :spx) `(:relative ,h)))
                     (price spx-call bs (bump m '(:spot :spx) `(:relative ,(- h)))))
                  (* 2 h s))
               1d-6)
        (check "1bp curve bumps: (P(+1bp) - P(-1bp)) / 2 = rho x 1e-4"
               (* k (exp (- r)) (norm-cdf d2) 1d-4)
               (/ (- (price spx-call bs (bump m '(:curve :usd-ois) '(:absolute 1/10000)))
                     (price spx-call bs (bump m '(:curve :usd-ois) '(:absolute -1/10000))))
                  2)
               1d-8))
      (check "an absolute vol bump" 0.23d0
             (black-vol (market-view (bump m '(:vol :spx) '(:absolute 1/100)) :underlying :spx)
                        100d0 1d0)
             1d-15)
      (check "a dividend bump" 0.025d0
             (dividend-yield (market-view (bump m '(:dividend :spx) '(:absolute 1/100))
                                          :underlying :spx)
                             1d0)
             1d-15)
      (check-error "bumping an underlying the market lacks" (bump m '(:spot :dax) '(:relative 0))
                   missing-market-data)
      (check-error "an unknown factor" (bump m '(:rho :spx) '(:relative 0)) invalid-argument)
      (check-error "a malformed shift" (bump m '(:spot :spx) '(:percent 1)) invalid-argument)
      (check-error "bumping a view" (bump (market-view m :underlying :spx) '(:spot :spx)
                                          '(:relative 0))
                   invalid-argument))))
