;;;; constructors.lisp --- keyword constructors of leaf objects
;;;;
;;;; Required keywords, the constructors of curves and flat structures, the
;;;; dividend constructor, and the validation of a scenario's bumps.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun missing-keyword-name (thunk)
  "The place named by the INVALID-ARGUMENT THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (invalid-argument (c) (string-downcase (princ-to-string (fincl::invalid-argument-place c))))))

(define-checks constructor-checks
  (let ((schedule (make-schedule :effective #D"2027-01-15" :termination #D"2028-01-15" :tenor #T"6M"))
        (dates (list #D"2026-09-24" #D"2027-09-24")))
    (check-equal "a missing required keyword is named" "rate"
                 (missing-keyword-name (lambda () (fixed-leg :schedule schedule :notional 1d6
                                                             :day-counter +actual-360+))))
    (check-that "the report names the constructor"
                (search "FIXED-LEG"
                        (string-upcase
                         (handler-case (fixed-leg :schedule schedule :notional 1d6 :day-counter +actual-360+)
                           (invalid-argument (c) (princ-to-string c))))))
    (check-equal "a required keyword whose value would be NIL is still required" "tenor"
                 (missing-keyword-name (lambda () (make-schedule :effective #D"2027-01-15"
                                                                 :termination #D"2028-01-15"))))
    (check-equal "the smile names its missing knots" "total-variances"
                 (missing-keyword-name (lambda () (make-interpolated-smile :expiry 1
                                                                           :log-moneyness '(-0.1 0 0.1)))))
    (check-equal "a curve names its missing dates" "dates"
                 (missing-keyword-name (lambda () (make-interpolated-zero-curve :zero-rates '(0.04d0)))))
    (check-error "positional schedules are gone" (make-schedule #D"2027-01-15" #D"2028-01-15" #T"6M"))
    (let ((new (make-interpolated-zero-curve :dates dates :zero-rates '(0.04d0 0.041d0)))
          (old (make-instance 'interpolated-zero-curve :dates dates :zero-rates '(0.04d0 0.041d0))))
      (check-that "the constructor builds what make-instance builds"
                  (= (curve-discount new 0.8d0) (curve-discount old 0.8d0))))
    (check-that "a flat vol, curve and dividend curve by keyword"
                (and (= (flat-vol (make-flat-black-vol :vol 0.2d0)) 0.2d0)
                     (= (flat-rate (make-flat-yield-curve :rate 0.03d0)) 0.03d0)
                     (= (flat-rate (make-flat-dividend-curve :rate 0.01d0)) 0.01d0)))
    (check-that "rates vols by keyword"
                (and (= (fincl::rates-vol (make-flat-rates-vol :vol 0.3d0)) 0.3d0)
                     (= (length (fincl::rates-vol-vols (make-expiry-rates-vol :dates dates :vols '(0.3d0 0.2d0)))) 2))))
  (check-that "a dividend made by make-dividend and by a legacy entry give equal schedules"
              (let ((a (make-dividend-schedule
                        (list (make-dividend "2027-02-10" 0.82d0 :pay-date #D"2027-03-01")
                              (make-dividend #D"2027-08-10" 0.05d0 :kind :proportional))))
                    (b (make-dividend-schedule '(("2027-02-10" 0.82d0 :pay-date #D"2027-03-01")
                                                 (#D"2027-08-10" 0.05d0 :kind :proportional)))))
                (equalp (fincl::schedule-entries a) (fincl::schedule-entries b))))
  (check-error "make-dividend checks the pay date" (make-dividend #D"2027-02-10" 1d0 :pay-date #D"2027-01-01")
               invalid-argument)
  (check-error "make-dividend checks the amount" (make-dividend #D"2027-02-10" 1.5d0 :kind :proportional)
               invalid-argument)
  (check-that "a snapshot of a dividend schedule reads back to the same entries"
              (let* ((s (make-dividend-schedule (list (make-dividend #D"2027-02-10" 0.82d0))))
                     (text (with-output-to-string (o) (write-snapshot s o)))
                     (back (with-input-from-string (i text) (read-snapshot i))))
                (and (search "MAKE-DIVIDEND " text)
                     (equalp (fincl::schedule-entries s) (fincl::schedule-entries back)))))
  (check-that "a snapshot of an interpolated smile writes keywords and reads back"
              (let* ((s (make-interpolated-smile :expiry 1 :log-moneyness '(-0.2d0 0d0 0.2d0)
                                                 :total-variances '(0.05d0 0.04d0 0.042d0)))
                     (text (with-output-to-string (o) (write-snapshot s o)))
                     (back (with-input-from-string (i text) (read-snapshot i))))
                (and (search ":LOG-MONEYNESS" text)
                     (= (smile-variance s 0.1d0) (smile-variance back 0.1d0)))))
  (check-that "a snapshot of an interpolated curve writes its constructor"
              (let* ((c (make-interpolated-discount-curve :dates (list #D"2026-09-24" #D"2027-09-24")
                                                          :discounts '(1d0 0.96d0)))
                     (text (with-output-to-string (o) (write-snapshot c o)))
                     (back (with-input-from-string (i text) (read-snapshot i))))
                (and (search "MAKE-INTERPOLATED-DISCOUNT-CURVE" text)
                     (= (curve-discount c 0.5d0) (curve-discount back 0.5d0)))))
  (check-that "a make-instance curve form from an older snapshot still reads"
              (typep (with-input-from-string
                         (i "(make-instance 'interpolated-discount-curve :dates (quote (#D\"2026-09-24\" #D\"2027-09-24\")) :discounts (quote (1d0 0.96d0)))")
                       (read-snapshot i))
                     'fincl::interpolated-discount-curve))
  (check-that "a valid scenario is accepted"
              (typep (make-scenario "s" :bumps '(((:spot :spx) (:relative -1/10))))
                     'fincl::scenario))
  (check-error "a scenario's bump without a shift is refused" (make-scenario "s" :bumps '(((:spot :spx))))
               invalid-argument)
  (check-error "a scenario's shift of an unknown kind is refused"
               (make-scenario "s" :bumps '(((:spot :spx) (:multiply 2)))) invalid-argument)
  (check-error "a scenario's factor that is not a keyword form is refused"
               (make-scenario "s" :bumps '((spot (:relative 1/10)))) invalid-argument))
