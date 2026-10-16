;;;; instruments/rate-helpers.lisp --- rate helpers and curve bootstrapping
;;;;
;;;; A rate helper is a quoted instrument (deposit, FRA, future, OIS, swap)
;;;; with a pillar date and a function giving the quote it implies on a
;;;; curve. BOOTSTRAP-CURVE finds the curve on which every helper reprices
;;;; its quote: pillar by pillar with Brent's method, repeating whole passes
;;;; for interpolations that are not local (monotone-convex) until no node
;;;; moves by more than the accuracy. Instruments and dates follow QuantLib's
;;;; DepositRateHelper, FraRateHelper, FuturesRateHelper, OISRateHelper and
;;;; SwapRateHelper (through MakeOIS and MakeVanillaSwap), and the traits
;;;; and node at the reference date follow its PiecewiseYieldCurve, so the
;;;; curves agree with QuantLib's (tests/fixtures/bootstrap.sexp).

(in-package #:fincl)

(defclass rate-helper ()
  ((quote :initarg :quote :reader helper-quote :type double-float)
   (valuation-date :initarg :valuation-date :reader helper-valuation-date :type date)
   (pillar-date :reader helper-pillar-date :type date
                :documentation "The curve node this helper determines.")
   (tenor :initarg :tenor :reader helper-tenor :initform nil
          :documentation "The tenor that names the helper (BUMP's (:quote id tenor)).")
   (arguments :initarg :arguments :reader helper-arguments
              :documentation "The constructor call, to rebuild with another quote."))
  (:documentation "A quoted instrument that pins one node of a bootstrapped
curve."))

(defgeneric helper-implied-quote (helper curve &optional discount-curve)
  (:documentation "The quote HELPER implies when CURVE forecasts and
DISCOUNT-CURVE (default CURVE) discounts. Both are dated curves measured
from the helper's valuation date."))

(defgeneric helper-shifted-quote (helper shift)
  (:documentation "HELPER's quote after moving its rate by SHIFT ((:relative
x) or (:absolute x)): the rate itself for rate quotes; 100 minus the shifted
rate for a futures price.")
  (:method ((h rate-helper) shift)
    (apply-shift (helper-quote h) shift)))

(defun helper-with-quote (helper quote)
  "A copy of HELPER quoting QUOTE."
  (destructuring-bind (constructor &rest arguments) (helper-arguments helper)
    (apply constructor quote (rest arguments))))

(defun index-with-forecast-curve (index curve-id)
  "A copy of INDEX forecast by the market curve CURVE-ID."
  (apply #'make-instance (class-of index)
         :forecast-curve curve-id
         (iter (for slot in (c2mop:class-slots (class-of index)))
               (for initarg = (first (c2mop:slot-definition-initargs slot)))
               (when (and initarg (not (eq initarg :forecast-curve))
                          (slot-boundp index (c2mop:slot-definition-name slot)))
                 (appending `(,initarg ,(slot-value index (c2mop:slot-definition-name slot))))))))

(defun simple-forward (curve start end day-counter)
  "(D(start)/D(end) - 1) / year fraction, on CURVE's dates."
  (/ (- (/ (curve-discount curve start) (curve-discount curve end)) 1d0)
     (year-fraction day-counter start end)))

(defmethod print-object ((h rate-helper) stream)
  (print-unreadable-object (h stream :type t)
    (format stream "~@[~A ~]~,6F pillar ~A" (helper-tenor h) (helper-quote h)
            (helper-pillar-date h))))

;;; --------------------------------------------------------------------
;;; Deposits, FRAs, futures
;;; --------------------------------------------------------------------

(defclass deposit-helper (rate-helper)
  ((index :initarg :index :reader helper-index)
   (start :reader helper-start)
   (maturity :reader helper-maturity))
  (:documentation "A deposit at INDEX's conventions from spot for its tenor;
the quote is the simple rate."))

(defun deposit-helper (quote index &key valuation-date)
  "A deposit quoting QUOTE for INDEX's tenor from spot.

  (deposit-helper 0.0245d0 (euribor #T\"6M\") :valuation-date today)"
  (check-input quote real "a real rate")
  (check-input index ibor-index "an Ibor index")
  (check-input valuation-date date "the valuation date")
  (let* ((h (make-instance 'deposit-helper :quote (float quote 1d0) :valuation-date valuation-date
                                           :index index :tenor (index-tenor index)
                                           :arguments `(deposit-helper ,quote ,index
                                                                       :valuation-date ,valuation-date)))
         (start (index-value-date index (adjust (index-fixing-calendar index) valuation-date)))
         (maturity (index-maturity-date index start)))
    (setf (slot-value h 'start) start
          (slot-value h 'maturity) maturity
          (slot-value h 'pillar-date) maturity)
    h))

(defmethod helper-implied-quote ((h deposit-helper) curve &optional discount-curve)
  (declare (ignore discount-curve))
  (simple-forward curve (helper-start h) (helper-maturity h) (index-day-counter (helper-index h))))

(defclass fra-helper (deposit-helper) ()
  (:documentation "A forward rate agreement on INDEX starting a number of
months after spot; the quote is the index's forward rate."))

(defun fra-helper (quote months-to-start index &key valuation-date)
  "An FRA starting MONTHS-TO-START months after spot on INDEX (a 3x9 on a 6M
index has MONTHS-TO-START 3), quoting QUOTE. Its pillar is the index
maturity from the start, QuantLib's default.

  (fra-helper 0.0251d0 3 (euribor #T\"6M\") :valuation-date today)"
  (check-input quote real "a real rate")
  (check-input months-to-start (integer 0) "a whole number of months")
  (check-input index ibor-index "an Ibor index")
  (check-input valuation-date date "the valuation date")
  (let* ((calendar (index-fixing-calendar index))
         (spot (advance calendar (adjust calendar valuation-date)
                        (make-tenor (index-fixing-days index) :days)))
         (start (advance calendar spot (make-tenor months-to-start :months)
                         :convention (index-convention index) :end-of-month (index-end-of-month index)))
         (maturity (index-maturity-date index start))
         (h (make-instance 'fra-helper :quote (float quote 1d0) :valuation-date valuation-date
                                       :index index :tenor (make-tenor months-to-start :months)
                                       :arguments `(fra-helper ,quote ,months-to-start ,index
                                                               :valuation-date ,valuation-date))))
    (setf (slot-value h 'start) start
          (slot-value h 'maturity) maturity
          (slot-value h 'pillar-date) maturity)
    h))

(defclass futures-helper (deposit-helper)
  ((convexity :initarg :convexity :reader helper-convexity :type double-float))
  (:documentation "An interest-rate future: the quote is the price, 100 times
(1 - forward rate - CONVEXITY), with the convexity adjustment given."))

(defun futures-helper (price start index &key (convexity 0d0) valuation-date)
  "A future on INDEX's rate from the date START (an IMM date, say) to the
index maturity from it, quoting PRICE.

  (futures-helper 97.52d0 #D\"2026-12-16\" (euribor #T\"3M\") :convexity 0.0002d0
                  :valuation-date today)"
  (check-input price real "a real price")
  (check-input start date "the start date of the underlying deposit")
  (check-input index ibor-index "an Ibor index")
  (check-input valuation-date date "the valuation date")
  (let ((h (make-instance 'futures-helper :quote (float price 1d0) :valuation-date valuation-date
                                          :index index :convexity (float convexity 1d0)
                                          :arguments `(futures-helper ,price ,start ,index
                                                                      :convexity ,convexity
                                                                      :valuation-date ,valuation-date))))
    (setf (slot-value h 'start) start
          (slot-value h 'maturity) (index-maturity-date index start)
          (slot-value h 'pillar-date) (helper-maturity h))
    h))

(defmethod helper-implied-quote ((h futures-helper) curve &optional discount-curve)
  (declare (ignore discount-curve))
  (* 100 (- 1d0 (+ (call-next-method) (helper-convexity h)))))

(defmethod helper-shifted-quote ((h futures-helper) shift)
  (* 100 (- 1d0 (apply-shift (- 1d0 (/ (helper-quote h) 100)) shift))))

;;; --------------------------------------------------------------------
;;; Swaps
;;; --------------------------------------------------------------------

(defclass swap-rate-helper (rate-helper)
  ((fixed-leg :reader helper-fixed-leg)
   (floating-leg :reader helper-floating-leg)
   (projection :initarg :projection :reader helper-projection-index))
  (:documentation "A par swap: the quote is the fixed rate at which the fixed
leg's value equals the floating leg's."))

(defun swap-helper-market (helper curve discount-curve)
  (make-market :valuation-date (helper-valuation-date helper)
               :curves `((:projection :curve ,curve) (:discount :curve ,(or discount-curve curve)))))

(defmethod helper-implied-quote ((h swap-rate-helper) curve &optional discount-curve)
  (let ((m (swap-helper-market h curve discount-curve)))
    (/ (money-amount (leg-npv (helper-floating-leg h) m :discount-curve :discount))
       (* 1d4 (money-amount (leg-bps (helper-fixed-leg h) m :discount-curve :discount))))))

(defun last-payment-date (leg)
  (iter (for cf in leg) (finding (payment-date cf) maximizing (date-serial (payment-date cf)))))

(defclass ois-helper (swap-rate-helper) ()
  (:documentation "An overnight indexed swap: fixed against daily compounded
overnight coupons, both legs on the same schedule (MakeOIS's)."))

(defun ois-helper (quote tenor index &key valuation-date (settlement-days 2) (payment-lag 0)
                                         (payment-convention :following) (payment-frequency :annual)
                                         (convention :modified-following) (end-of-month nil eom-p))
  "An OIS of TENOR from SETTLEMENT-DAYS business days after the valuation
date on the overnight INDEX, quoting the fixed rate QUOTE (fixed day counter
the index's). Both legs pay every PAYMENT-FREQUENCY (a single period for
shorter swaps), PAYMENT-LAG business days after each period. END-OF-MONTH
defaults to whether the start is its month's last business day. The pillar
is the last date the swap depends on: its maturity, its last payment, or the
end of its last fixing's deposit.

  (ois-helper 0.0372d0 #T\"5Y\" (sofr) :valuation-date today :payment-lag 2)"
  (check-input quote real "a real rate")
  (check-input tenor tenor "a tenor")
  (check-input index overnight-index "an overnight index")
  (check-input valuation-date date "the valuation date")
  (let* ((calendar (index-fixing-calendar index))
         (spot (advance calendar (adjust calendar valuation-date) (make-tenor settlement-days :days)))
         (start (adjust calendar spot :following))
         (eom (if eom-p end-of-month (calendar-end-of-month-p calendar start)))
         (end (let ((end (add-tenor start tenor)))
                (if (and eom (member (tenor-unit tenor) '(:months :years))
                         (calendar-end-of-month-p calendar start))
                    (calendar-end-of-month calendar end)
                    end)))
         (schedule (make-schedule start end (make-tenor (/ 12 (frequency-periods payment-frequency))
                                                        :months)
                                  :calendar calendar :convention convention
                                  :termination-convention convention :rule :backward
                                  :end-of-month eom))
         (projection (index-with-forecast-curve index :projection))
         (fixed (fixed-leg schedule 1 0d0 (index-day-counter index) :currency (index-currency index)
                                          :payment-lag payment-lag :payment-convention payment-convention))
         (floating (floating-leg schedule 1 projection :payment-lag payment-lag
                                                       :payment-convention payment-convention))
         (last-fixing (a:last-elt (overnight-fixing-dates (a:lastcar floating))))
         (fixing-end (index-maturity-date index (index-value-date index last-fixing)))
         (maturity (accrual-end (a:lastcar floating)))
         (h (make-instance 'ois-helper
                           :quote (float quote 1d0) :valuation-date valuation-date :tenor tenor
                           :projection projection
                           :arguments `(ois-helper ,quote ,tenor ,index :valuation-date ,valuation-date
                                                   :settlement-days ,settlement-days
                                                   :payment-lag ,payment-lag
                                                   :payment-convention ,payment-convention
                                                   :payment-frequency ,payment-frequency
                                                   :convention ,convention
                                                   ,@(when eom-p `(:end-of-month ,end-of-month))))))
    (setf (slot-value h 'fixed-leg) fixed
          (slot-value h 'floating-leg) floating
          (slot-value h 'pillar-date)
          (iter (for d in `(,maturity ,(last-payment-date fixed) ,(last-payment-date floating)
                            ,fixing-end))
                (finding d maximizing (date-serial d))))
    h))

(defclass swap-helper (swap-rate-helper) ()
  (:documentation "A vanilla swap: a fixed leg against an Ibor leg on the
index's tenor, par coupons (MakeVanillaSwap's)."))

(defun swap-helper (quote tenor index &key valuation-date (fixed-frequency :annual)
                                          (fixed-convention :modified-following)
                                          (fixed-day-counter (make-instance 'thirty-360 :variant :bond-basis))
                                          calendar)
  "A vanilla swap of TENOR from spot on the Ibor INDEX, quoting the fixed
rate QUOTE paid every FIXED-FREQUENCY under FIXED-DAY-COUNTER, dates
adjusted by FIXED-CONVENTION on CALENDAR (default the index's). The pillar
is the later of the maturity and the end of the last coupon's forecast.

  (swap-helper 0.0268d0 #T\"10Y\" (euribor #T\"6M\") :valuation-date today)"
  (check-input quote real "a real rate")
  (check-input tenor tenor "a tenor")
  (check-input index ibor-index "an Ibor index")
  (check-input valuation-date date "the valuation date")
  (let* ((calendar (or calendar (index-fixing-calendar index)))
         (start (index-value-date index (adjust (index-fixing-calendar index) valuation-date)))
         (end (add-tenor start tenor))
         (fixed-schedule (make-schedule start end (make-tenor (/ 12 (frequency-periods fixed-frequency))
                                                              :months)
                                        :calendar calendar :convention fixed-convention
                                        :termination-convention fixed-convention :rule :backward))
         (float-schedule (make-schedule start end (index-tenor index)
                                        :calendar calendar :convention (index-convention index)
                                        :termination-convention (index-convention index)
                                        :rule :backward))
         (projection (index-with-forecast-curve index :projection))
         (payment-convention (index-convention index))
         (fixed (fixed-leg fixed-schedule 1 0d0 fixed-day-counter :currency (index-currency index)
                                                              :payment-convention payment-convention))
         (floating (floating-leg float-schedule 1 projection :payment-convention payment-convention))
         (maturity (iter (for cf in (append fixed floating))
                         (finding (accrual-end cf) maximizing (date-serial (accrual-end cf)))))
         (h (make-instance 'swap-helper
                           :quote (float quote 1d0) :valuation-date valuation-date :tenor tenor
                           :projection projection
                           :arguments `(swap-helper ,quote ,tenor ,index :valuation-date ,valuation-date
                                                    :fixed-frequency ,fixed-frequency
                                                    :fixed-convention ,fixed-convention
                                                    :fixed-day-counter ,fixed-day-counter
                                                    :calendar ,calendar))))
    (setf (slot-value h 'fixed-leg) fixed
          (slot-value h 'floating-leg) floating
          (slot-value h 'pillar-date)
          (let ((fixing-end (fixing-end-date (a:lastcar floating))))
            (if (date< maturity fixing-end) fixing-end maturity)))
    h))

;;; --------------------------------------------------------------------
;;; Bootstrapping
;;; --------------------------------------------------------------------

(defclass piecewise-yield-curve (dated-curve)
  ((helpers :initarg :helpers :reader curve-helpers
            :documentation "The helpers, by pillar date.")
   (interpolation :initarg :interpolation :reader curve-interpolation)
   (discount-curve :initarg :discount-curve :reader curve-exogenous-discount
                   :documentation "The curve the helpers discount on, or NIL for this one.")
   (accuracy :initarg :accuracy :reader curve-accuracy)
   (nodes :initarg :nodes :reader curve-nodes
          :documentation "The interpolated curve through the solved nodes."))
  (:documentation "A curve bootstrapped from rate helpers: each helper
reprices its quote. Discount factors come from the interpolated curve
through the nodes. Built by BOOTSTRAP-CURVE."))

(defmethod curve-discount ((c piecewise-yield-curve) (x real))
  (curve-discount (curve-nodes c) x))

(defmethod print-object ((c piecewise-yield-curve) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A, ~D helpers, ~(~A~)" (curve-reference-date c) (length (curve-helpers c))
            (curve-interpolation c))))

(defparameter *bootstrap-max-rate* 1d0
  "The bootstrap searches each node for forward rates within this (QuantLib's maxRate).")

(defun node-curve (interpolation dates data day-counter)
  "The interpolated curve through DATES and DATA: discount factors for
:LOG-LINEAR, period forwards for :MONOTONE-CONVEX and :BACKWARD-FLAT."
  (ecase interpolation
    (:log-linear (make-instance 'interpolated-discount-curve :dates dates :discounts data
                                                             :day-counter day-counter
                                                             :interpolation :log-linear))
    ((:monotone-convex :backward-flat)
     (make-instance 'interpolated-forward-curve :dates dates :forwards data :day-counter day-counter
                                                :interpolation interpolation))))

(defun bootstrap-curve (helpers &key (interpolation :log-linear) discount-curve
                                     (day-counter +actual-365-fixed+) (accuracy 1d-12)
                                     (max-passes 100))
  "The curve on which every helper of HELPERS reprices its quote, from their
common valuation date under DAY-COUNTER. INTERPOLATION is :LOG-LINEAR
(discount factors; each node needs one pass), or :MONOTONE-CONVEX or
:BACKWARD-FLAT (instantaneous forwards; monotone-convex is not local, so
passes repeat until no node moves by more than ACCURACY). DISCOUNT-CURVE, if
given, discounts the helpers' cash flows while the bootstrapped curve only
forecasts: the second stage of a multi-curve build (OIS first, then the
Ibor curve on it).

  (bootstrap-curve (list (ois-helper 0.037d0 #T\"1Y\" (sofr) :valuation-date today) ...))"
  (check-input helpers (and list (not null)) "a non-empty list of rate helpers")
  (let* ((helpers (-> helpers copy-list (sort #'date< :key #'helper-pillar-date)))
         (today (helper-valuation-date (first helpers)))
         (dates (coerce (cons today (mapcar #'helper-pillar-date helpers)) 'simple-vector))
         (n (length helpers))
         (forward (not (eq interpolation :log-linear)))
         (data (make-array (1+ n) :element-type 'double-float
                                  :initial-element (if forward 0.05d0 1d0)))
         (times (map 'double-vector (lambda (d) (year-fraction day-counter today d)) dates)))
    (unless (every (lambda (h) (date= (helper-valuation-date h) today)) helpers)
      (signal-invalid-argument 'helpers helpers 'list "helpers sharing one valuation date"))
    (iter (for i from 1 to n)
          (unless (date< (svref dates (1- i)) (svref dates i))
            (signal-invalid-argument 'helpers helpers 'list
                                     (format nil "helpers with distinct pillar dates after ~A ~
(two pillars fall on ~A)" today (svref dates i)))))
    (when (and forward (= n 1) (eq interpolation :monotone-convex))
      (signal-invalid-argument 'helpers helpers 'list "at least two helpers for a monotone-convex curve"))
    (flet ((curve-through (k)
             ;; Nodes 0..K.
             (node-curve interpolation (subseq dates 0 (1+ k)) (subseq data 0 (1+ k)) day-counter))
           (set-node (i x)
             (setf (aref data i) x)
             (when (and forward (= i 1)) (setf (aref data 0) x))))
      (iter (for pass from 1)
            (for previous = (copy-seq data))
            (iter (for i from 1 to n)
                  (for helper = (nth (1- i) helpers))
                  (let* ((dt (- (aref times i) (aref times (1- i))))
                         (lo (if forward (- *bootstrap-max-rate*)
                                 (* (aref data (1- i)) (exp (- (* *bootstrap-max-rate* dt))))))
                         (hi (if forward *bootstrap-max-rate*
                                 (* (aref data (1- i)) (exp (* *bootstrap-max-rate* dt)))))
                         (k (if (= pass 1) i n)))
                    (set-node i (solve-brent (lambda (x)
                                               (set-node i x)
                                               (- (helper-implied-quote helper (curve-through k)
                                                                        discount-curve)
                                                  (helper-quote helper)))
                                             lo hi :tolerance 1d-16))))
            (until (or (not (eq interpolation :monotone-convex))
                       (and (> pass 1)
                            (<= (iter (for i from 1 to n)
                                      (maximize (abs (- (aref data i) (aref previous i)))))
                                accuracy))))
            (when (>= pass max-passes)
              (error 'convergence-failure
                     :context (format nil "the bootstrap did not converge in ~D passes" max-passes))))
      (make-instance 'piecewise-yield-curve
                     :reference-date today :day-counter day-counter :helpers helpers
                     :interpolation interpolation :discount-curve discount-curve :accuracy accuracy
                     :nodes (curve-through n)))))

(defun rebootstrap (curve helpers)
  "CURVE's bootstrap repeated with HELPERS."
  (bootstrap-curve helpers :interpolation (curve-interpolation curve)
                           :discount-curve (curve-exogenous-discount curve)
                           :day-counter (curve-day-counter curve)
                           :accuracy (curve-accuracy curve)))

(defmethod bump-structure ((c piecewise-yield-curve) shift)
  ;; Every quote moves: a parallel move of the par rates.
  (rebootstrap c (mapcar (lambda (h) (helper-with-quote h (helper-shifted-quote h shift)))
                         (curve-helpers c))))

(defmethod bump-structure-quote ((c piecewise-yield-curve) selector shift)
  (let ((target (etypecase selector
                  (integer (nth selector (curve-helpers c)))
                  (tenor (find selector (curve-helpers c) :key #'helper-tenor :test #'equalp)))))
    (unless target
      (signal-invalid-argument 'selector selector '(or integer tenor)
                               (format nil "the index or tenor of one of the curve's helpers (~{~A~^, ~})"
                                       (mapcar #'helper-tenor (curve-helpers c)))))
    (rebootstrap c (mapcar (lambda (h) (if (eq h target)
                                           (helper-with-quote h (helper-shifted-quote h shift))
                                           h))
                           (curve-helpers c)))))
