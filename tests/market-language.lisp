;;;; market-language.lisp --- the market construction language
;;;;
;;;; Markets built from clauses against markets built from keywords. The
;;;; shapes below are the distinct ways MAKE-MARKET is called in tests/ (one
;;;; underlying or several, discrete dividends or not, a flat vol or a vol
;;;; structure, a curve by rate or by structure, fixings or not, a day
;;;; counter or not). Each has a clause twin written by hand; a converter
;;;; from the keyword shape to clauses is checked against the twins and then
;;;; run over every shape.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun specs-to-clauses (&key valuation-date (day-counter nil day-counter-p)
                              underlyings curves discounting fixings)
  "The clause data (for MAKE-MARKET-FROM-CLAUSES) that build what
MAKE-MARKET-FROM-SPECS builds from the same keyword arguments."
  `((:valued-on ,valuation-date)
    ,@(when day-counter-p `((:day-counter ,day-counter)))
    ,@(iter (for entry in underlyings)
            (destructuring-bind (name &key spot currency dividend dividend-schedule vol) entry
              (collect `(:underlying ,name
                                     ,@(when spot `((:spot ,spot)))
                                     ,@(when currency `((:currency ,currency)))
                                     ,@(when dividend `((:dividend ,dividend)))
                                     ,@(when dividend-schedule `((:dividends ,dividend-schedule)))
                                     ,@(when vol `((:vol ,vol)))))))
    ,@(iter (for entry in curves)
            (destructuring-bind (id &key rate curve) entry
              (collect `(:curve ,id ,(if curve `(:structure ,curve) `(:rate ,rate))))))
    ,@(iter (for (currency . id) in discounting)
            (collect `(:discounting ,currency ,id)))
    ,@(iter (for (id . pairs) in fixings)
            (collect `(:fixings ,id ,pairs)))))

(defun market-shapes ()
  "A list of (name specs twin): the keyword arguments of one MAKE-MARKET call
shape found in tests/ and its clause twin, written by hand."
  (let ((zero-curve (make-interpolated-zero-curve
                                   :dates (list *today* #D"2027-09-24" #D"2031-09-24")
                                   :zero-rates '(0.04d0 0.041d0 0.043d0)))
        (discount-curve (make-interpolated-discount-curve
                                       :dates (list *today* #D"2027-09-24")
                                       :discounts '(1d0 0.95d0)))
        (grid (make-grid-vol-surface :expiries '(0.5d0 1d0 2d0)
                                     :log-moneyness '(-0.2d0 0d0 0.2d0)
                                     :vols '((0.25d0 0.2d0 0.21d0) (0.24d0 0.2d0 0.205d0)
                                       (0.23d0 0.2d0 0.2d0))))
        (ssvi (make-ssvi-surface '(0.25 0.5 1 2) '(0.0121 0.0225 0.04 0.072)
                                 :rho -0.6 :eta 1.2 :gamma 0.4)))
    `(("two underlyings, two currencies, two curves, explicit discounting (market.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:spx :spot 100 :currency :usd :dividend 0.015d0 :vol 0.22d0)
                      (:ndx :spot 150 :currency :usd :vol 0.3d0))
        :curves ((:usd-ois :rate 0.04d0) (:eur-estr :rate 0.02d0))
        :discounting ((:usd . :usd-ois) (:eur . :eur-estr)))
       ((:valued-on ,*today*)
        (:underlying :spx (:spot 100) (:currency :usd) (:dividend 0.015d0) (:vol 0.22d0))
        (:underlying :ndx (:spot 150) (:currency :usd) (:vol 0.3d0))
        (:curve :usd-ois (:rate 0.04d0))
        (:curve :eur-estr (:rate 0.02d0))
        (:discounting :usd :usd-ois)
        (:discounting :eur :eur-estr)))
      ("one underlying with no vol, one curve at rate 0 (market.lisp)"
       (:valuation-date ,*today* :underlyings ((:x :spot 1)) :curves ((:c :rate 0)))
       ((:valued-on ,*today*) (:underlying :x (:spot 1)) (:curve :c (:rate 0))))
      ("discrete dividends as strings, no currency (dividends.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:x :spot 100 :vol 0.2d0
                       :dividend-schedule (("2027-01-15" 1.5d0) ("2027-07-15" 2d0))))
        :curves ((:c :rate 0.05d0)))
       ((:valued-on ,*today*)
        (:underlying :x (:spot 100)
                     (:dividends (("2027-01-15" 1.5d0) ("2027-07-15" 2d0))) (:vol 0.2d0))
        (:curve :c (:rate 0.05d0))))
      ("a curve object under a sofr id (portfolio.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:spx :spot 5000 :currency :usd :vol 0.2d0))
        :curves ((:sofr :curve ,zero-curve)))
       ((:valued-on ,*today*)
        (:underlying :spx (:spot 5000) (:currency :usd) (:vol 0.2d0))
        (:curve :sofr (:structure ,zero-curve))))
      ("mixed cash and proportional dividends, a vol grid and an SSVI surface (portfolio.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:spx :spot 5000 :currency :usd :dividend 0.01d0
                       :dividend-schedule (("2027-01-15" 20d0)
                                           ("2027-07-15" 0.01d0 :kind :proportional))
                       :vol ,grid)
                      (:ndx :spot 18000 :currency :usd :vol ,ssvi))
        :curves ((:usd-ois :curve ,zero-curve))
        :discounting ((:usd . :usd-ois))
        :fixings ((:sofr ("2026-09-23" 0.0405d0))))
       ((:valued-on ,*today*)
        (:underlying :spx (:spot 5000) (:currency :usd) (:dividend 0.01d0)
                     (:dividends (("2027-01-15" 20d0) ("2027-07-15" 0.01d0 :kind :proportional)))
                     (:vol ,grid))
        (:underlying :ndx (:spot 18000) (:currency :usd) (:vol ,ssvi))
        (:curve :usd-ois (:structure ,zero-curve))
        (:discounting :usd :usd-ois)
        (:fixings :sofr (("2026-09-23" 0.0405d0)))))
      ("no underlyings, curves by object, several discounting entries, fixings (rates.lisp)"
       (:valuation-date #D"2026-09-24"
        :curves ((:estr :curve ,discount-curve) (:sofr :curve ,zero-curve))
        :discounting ((:eur . :estr) (:usd . :sofr))
        :fixings ((:sofr (#D"2026-09-22" 0.0431d0) ("2026-09-23" 0.0433d0))
                  (:estr (#D"2026-09-23" 0.0192d0))))
       ((:valued-on #D"2026-09-24")
        (:curve :estr (:structure ,discount-curve))
        (:curve :sofr (:structure ,zero-curve))
        (:discounting :eur :estr)
        (:discounting :usd :sofr)
        (:fixings :sofr ((#D"2026-09-22" 0.0431d0) ("2026-09-23" 0.0433d0)))
        (:fixings :estr ((#D"2026-09-23" 0.0192d0)))))
      ("one curve only, no underlyings, no discounting (rates.lisp)"
       (:valuation-date ,*today* :curves ((:sofr :curve ,zero-curve)))
       ((:valued-on ,*today*) (:curve :sofr (:structure ,zero-curve))))
      ("a day counter (paths.lisp)"
       (:valuation-date ,*today* :day-counter ,(make-instance 'fincl::actual-360)
        :underlyings ((:x :spot 100 :dividend 0.04d0 :vol 0.25d0 :currency :usd))
        :curves ((:usd :rate 0.08d0)))
       ((:valued-on ,*today*) (:day-counter ,(make-instance 'fincl::actual-360))
        (:underlying :x (:spot 100) (:currency :usd) (:dividend 0.04d0) (:vol 0.25d0))
        (:curve :usd (:rate 0.08d0))))
      ("three underlyings, one curve, default discounting (simulation.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:aapl :spot 100 :currency :usd :vol 0.3d0 :dividend 0.01d0)
                      (:msft :spot 50 :currency :usd :vol 0.2d0)
                      (:ibm :spot 10 :currency :usd :vol 0.25d0))
        :curves ((:usd :rate 0.03d0)))
       ((:valued-on ,*today*)
        (:underlying :aapl (:spot 100) (:currency :usd) (:dividend 0.01d0) (:vol 0.3d0))
        (:underlying :msft (:spot 50) (:currency :usd) (:vol 0.2d0))
        (:underlying :ibm (:spot 10) (:currency :usd) (:vol 0.25d0))
        (:curve :usd (:rate 0.03d0))))
      ("a negative continuous dividend and zero rate (simulation-books.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:eq :spot 100 :currency :usd :dividend -0.03d0 :vol 0.16d0))
        :curves ((:usd :rate 0d0)) :discounting ((:usd . :usd)))
       ((:valued-on ,*today*)
        (:underlying :eq (:spot 100) (:currency :usd) (:dividend -0.03d0) (:vol 0.16d0))
        (:curve :usd (:rate 0d0))
        (:discounting :usd :usd)))
      ("a flat dividend curve object and an SSVI vol (measure.lisp)"
       (:valuation-date ,*today*
        :underlyings ((:x :spot 100 :dividend ,(make-instance 'fincl::flat-dividend-curve :rate 0.01d0)
                       :currency :usd :vol ,ssvi))
        :curves ((:usd :rate 0.05d0)))
       ((:valued-on ,*today*)
        (:underlying :x (:spot 100) (:currency :usd)
                     (:dividend ,(make-instance 'fincl::flat-dividend-curve :rate 0.01d0))
                     (:vol ,ssvi))
        (:curve :usd (:rate 0.05d0))))
      ("a dividend schedule object and a flat vol structure object"
       (:valuation-date ,*today*
        :underlyings ((:x :spot 100 :vol ,(make-instance 'fincl::flat-black-vol :vol 0.2d0)
                       :dividend-schedule ,(make-dividend-schedule '(("2027-01-15" 1d0)))))
        :curves ((:usd :rate 0.05d0)))
       ((:valued-on ,*today*)
        (:underlying :x (:spot 100)
                     (:dividends ,(make-dividend-schedule '(("2027-01-15" 1d0))))
                     (:vol ,(make-instance 'fincl::flat-black-vol :vol 0.2d0)))
        (:curve :usd (:rate 0.05d0)))))))

(defun quoted-plist (plist)
  "PLIST with every value quoted, so it can be spliced into a form."
  (iter (for (key value) in-plist plist)
        (collect key)
        (collect `(quote ,value))))

(defun market-probe-price (market)
  "The Black-Scholes price of an at-the-money call on the first underlying of
MARKET, by name, that has a vol, or NIL if there is none."
  (let ((u (find-if (lambda (u) (gethash u (fincl::market-vol-structures market)))
                    (sort (copy-list (fincl::market-underlyings market)) #'string<))))
    (when u
      (price (make-option (call (fincl::underlying-spot market u)) (expires #D"2027-09-24")
                          (underlying u))
             :black-scholes market))))

(define-checks market-language-checks
  (format t "~&Market language~%")
  (dolist (shape (market-shapes))
    (destructuring-bind (name specs twin) shape
      (let ((by-specs (apply #'fincl::make-market-from-specs specs))
            (by-twin (make-market-from-clauses twin))
            (converted (apply #'specs-to-clauses specs)))
        (check-that (format nil "the converter gives the hand-written twin: ~A" name)
                    (construction-equal-p converted twin))
        (check-that (format nil "keyword and clause markets are equal slot for slot: ~A" name)
                    (and (construction-equal-p by-specs by-twin)
                         (construction-equal-p by-specs (make-market-from-clauses converted))))
        (check-that (format nil "the keyword shape of the macro builds the same market: ~A" name)
                    (construction-equal-p by-specs (eval `(make-market ,@(quoted-plist specs)))))
        (let ((p (market-probe-price by-specs)))
          (when p
            (check-that (format nil "prices are = on both: ~A" name)
                        (= p (market-probe-price by-twin))))))))
  ;; The macro written as clauses, with arguments evaluated.
  (let ((spot 5000) (curve (make-interpolated-zero-curve
                                          :dates (list *today* #D"2027-09-24")
                                          :zero-rates '(0.04d0 0.041d0))))
    (check-that "the macro takes clauses with evaluated arguments"
                (construction-equal-p
                 (make-market (valued-on *today*)
                              (underlying :spx (spot spot) (vol (* 2 0.09d0)))
                              (curve :c (structure curve)))
                 (fincl::make-market-from-specs :valuation-date *today*
                                                :underlyings `((:spx :spot ,spot :vol ,(* 2 0.09d0)))
                                                :curves `((:c :curve ,curve))))))
  (let ((m (make-market (valued-on *today*) (underlying :x (spot 100) (vol 0.2d0))
                        (curve :c (rate 0.05d0)) (curve :d (rate 0.04d0))
                        (discounting :usd :c))))
    (check-that "a market holding two curves and a discounting clause has them"
                (and (eq :c (gethash :usd (fincl::market-discount-curve-for m)))
                     (= 2 (hash-table-count (fincl::market-yield-curves m)))))
    (check-that "with one curve and no discounting clause every currency discounts on it"
                (eq :c (gethash :default
                                (fincl::market-discount-curve-for
                                 (make-market (valued-on *today*) (underlying :x (spot 100))
                                              (curve :c (rate 0.05d0))))))))
  ;; Scoped conflicts and requirements.
  (check-error "two spots for one underlying conflict"
               (make-market (valued-on *today*) (underlying :x (spot 100) (spot 101))
                            (curve :c (rate 0.05d0)))
               conflicting-clauses)
  (check-error "the currency set twice in one underlying conflicts"
               (make-market (valued-on *today*) (underlying :x (spot 100) (currency :usd) (currency :eur))
                            (curve :c (rate 0.05d0)))
               conflicting-clauses)
  (check-error "an underlying named twice conflicts"
               (make-market (valued-on *today*) (underlying :x (spot 100)) (underlying :x (spot 101))
                            (curve :c (rate 0.05d0)))
               conflicting-clauses)
  (check-error "a curve given by rate and by structure conflicts"
               (make-market (valued-on *today*) (curve :c (rate 0.05d0) (structure (make-instance 'fincl::flat-yield-curve :rate 0.04d0))))
               conflicting-clauses)
  (check-error "a valuation date set twice conflicts"
               (make-market (valued-on *today*) (valued-on *today*) (curve :c (rate 0.05d0)))
               conflicting-clauses)
  (check-that "the same currency in two different underlyings does not conflict"
              (make-market (valued-on *today*)
                           (underlying :x (spot 100) (currency :usd))
                           (underlying :y (spot 100) (currency :usd))
                           (curve :c (rate 0.05d0))))
  (check-error "no valuation date" (make-market (curve :c (rate 0.05d0))) missing-fact)
  (check-error "an underlying without a spot" (make-market (valued-on *today*) (underlying :x (vol 0.2d0))) missing-fact)
  (check-error "a curve with neither rate nor structure" (make-market (valued-on *today*) (curve :c)) missing-fact)
  (check-error "a negative spot is the same error as in the keyword shape"
               (make-market (valued-on *today*) (underlying :x (spot -1)) (curve :c (rate 0)))
               invalid-argument)
  (check-error "discounting on a curve the market does not hold"
               (make-market (valued-on *today*) (underlying :x (spot 1)) (curve :c (rate 0))
                            (discounting :default :nope))
               invalid-argument)
  (check-error "an unknown clause" (macroexpand-1 '(make-market (valued-on *today*) (spoot 1))) unknown-clause)
  (check-error "clauses passed as quoted data to the macro"
               (macroexpand-1 '(make-market '((:valued-on d)))) malformed-clause)
  ;; Printing, round trip and snapshots.
  (let ((m (make-market (valued-on *today*)
                        (underlying :x (spot 100) (currency :usd) (dividend 0.01d0) (vol 0.2d0)
                                    (dividends '(("2027-01-15" 1d0))))
                        (underlying :y (spot 50) (currency :usd))
                        (curve :usd (rate 0.03d0))
                        (discounting :usd :usd)
                        (fixings :sofr '((#D"2026-09-23" 0.0405d0))))))
    (check-that "a market prints as its clauses"
                (let ((text (princ-to-string m)))
                  (and (search "(valued-on #D\"2026-09-24\")" text)
                       (search "(underlying :x (spot 100.0d0)" text)
                       (search "(curve :usd (rate 0.03d0))" text))))
    (check-that "a market with writable parts round-trips through its clauses"
                (round-trip-p 'market m :test #'construction-equal-p))
    (check-that "a market with writable parts prints readably as a form that rebuilds it"
                (let ((form (let ((*package* (find-package '#:fincl/tests)))
                              (with-output-to-string (s)
                                (let ((*print-readably* t))
                                  (fincl::print-clause-form 'market m s))))))
                  (construction-equal-p m (eval (let ((*readtable* (named-readtables:find-readtable 'fincl:syntax))
                                                      (*package* (find-package '#:fincl/tests)))
                                                  (read-from-string form))))))
    (check-that "a market with a bootstrapped or non-writable curve still prints, with #<...>"
                (search "#<" (princ-to-string
                              (make-market (valued-on *today*)
                                           (curve :c (structure (make-interpolated-zero-curve
                                                                               :dates (list *today* #D"2027-09-24")
                                                                               :zero-rates '(0.04d0 0.041d0)))))))))
  (let* ((old (with-open-file (s (asdf:system-relative-pathname "fincl" "tests/fixtures/market-snapshot-v1.sexp"))
                (read-snapshot s)))
         (text (with-output-to-string (s) (write-snapshot old s)))
         (new (with-input-from-string (s text) (read-snapshot s))))
    (check-that "a snapshot written before markets had clauses still reads" (typep old 'fincl::composite-market))
    (check-that "a market is written as make-market-from-clauses data"
                (and (search "MAKE-MARKET-FROM-CLAUSES" text) (not (search "MAKE-MARKET " text))))
    (check-that "the clause snapshot reads back to an equal market and equal prices"
                (and (construction-equal-p old new)
                     (= (market-probe-price old) (market-probe-price new))))
    (check-that "the old snapshot has the markets the file describes"
                (equal '(:spx :aapl) (fincl::market-underlyings old)))))
