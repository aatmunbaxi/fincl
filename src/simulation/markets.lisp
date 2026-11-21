;;;; markets.lisp --- markets, discounting and fixings along a path
;;;;
;;;; Builds the market at an event date from the legs' states, discount factors
;;;; along a path and the index fixings a path carries.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defgeneric market-at-state (market simulation date states)
  (:documentation "Return MARKET as of DATE, built from the simulated states. STATES is a vector of the state
vectors of the legs, in the order of (SIMULATION-UNDERLYINGS SIMULATION). For a composite
market, the default ages MARKET to DATE in one copy of its tables. It sets the target of
each leg to STRUCTURE-AT-STATE of its state (a spot leg: element 0). Then it sets each
:DERIVE target to the value of its function. It keeps every other component (curves,
dividend schedules, vol structures, fixings). To build markets in another way, define a
method on your market class, or pass :MARKET-FROM-STATE to SIMULATE (a function with the
same arguments).

  (market-at-state m plan #D\"2027-03-24\" states)")
  (:method ((m composite-market) simulation date states)
    (let ((new (copy-composite-market m :valuation-date date)))
      (iter (for leg in-vector (simulation-plan-legs simulation))
            (for state in-vector states)
            (for target = (simulation-leg-target leg))
            (set-market-component new target
                                  (apply #'structure-at-state (simulation-leg-process leg) target
                                         (simulation-leg-initial leg) new date state
                                         (simulation-leg-options leg))
                                  date))
      (iter (for (target . function) in (simulation-plan-derive simulation))
            (set-market-component new target (funcall function new date states) date))
      new))
  (:method ((m market) simulation date states)
    (declare (ignore simulation date states))
    (signal-invalid-argument 'market m 'composite-market
                             "a composite market, a MARKET-AT-STATE method for this market class, or :MARKET-FROM-STATE")))

(defun simulation-date (plan k)
  "The date of event K of PLAN (0: the valuation date)."
  (if (zerop k)
      (valuation-date (simulation-plan-market plan))
      (aref (simulation-plan-event-dates plan) (1- k))))

(defun simulation-market (plan k states &optional fixings)
  "The market at event K (0: the valuation date) for STATES, carrying the
path's FIXINGS (a fixing buffer, see PATH-FIXINGS!) of the dates before K."
  (let ((date (simulation-date plan k)))
    (a:if-let ((from-state (simulation-plan-market-from-state plan)))
      (funcall from-state (simulation-plan-market plan) plan date states)
      (let ((market (market-at-state (simulation-plan-market plan) plan date states)))
        (when (and fixings (plusp k))
          (add-path-fixings plan market k fixings))
        market))))

;;; --------------------------------------------------------------------
;;; Discounting and fixings along a path
;;; --------------------------------------------------------------------

(defgeneric state-discount (process state)
  (:documentation "The discount from the valuation date along a path where
PROCESS's state is STATE, exp(-int r ds), or NIL if PROCESS does not drive
a curve: the default reads a state component named :INTEGRAL.")
  (:method ((p stochastic-process) state)
    (a:when-let ((i (position :integral (process-state-names p))))
      (exp (- (aref state i)))))
  (:method ((p physical-process) state)
    (state-discount (risk-neutral-process p) state)))

(defun default-discount-target (market)
  "(:curve id) for the discount curve of the composite MARKET's only currency."
  (let ((currencies (sorted-keys (market-discount-curve-for market))))
    (unless (= 1 (length currencies))
      (signal-invalid-argument 'target nil 'list
                               (format nil "a curve to discount on, (path-discount '(:curve id)): the market ~
discounts ~:[no currency~;~:*~{~S~^, ~}~]" currencies)))
    `(:curve ,(gethash (first currencies) (market-discount-curve-for market)))))

(defun simulation-path-discount (plan k states target)
  "PATH-DISCOUNT in a clause: the discount from the valuation date to event K
on the curve TARGET ((:curve id), default the only currency's discount
curve), from the leg driving it if any, else from the initial market."
  (let ((market (simulation-plan-market plan)))
    (cond ((zerop k) 1d0)
          ((and (null target) (not (typep market 'composite-market)))
           (discount-factor market (simulation-date plan k)))
          (t
           (let* ((target (if target (normalize-target target) (default-discount-target market)))
                  (legs (simulation-plan-legs plan))
                  (i (position target legs :key #'simulation-leg-target :test #'equal)))
             (unless (eq (first target) :curve)
               (signal-invalid-argument 'target target 'list "a curve to discount on, (:curve id)"))
             (if i
                 (or (state-discount (simulation-leg-process (aref legs i)) (svref states i))
                     (signal-invalid-argument 'target target 'list
                                              (format nil "a curve whose leg's process discounts along the path ~
(~(~A~) has no :INTEGRAL state)" (class-name (class-of (simulation-leg-process (aref legs i)))))))
                 (market-curve-discount market (second target) (simulation-date plan k))))))))

(defun make-fixing-buffer (plan)
  "A buffer for one path's fixings of PLAN's :FIX indexes, one per (index,
event), or NIL without :FIX."
  (let ((n (length (simulation-plan-fix plan))))
    (when (plusp n)
      (make-array (* n (1+ (length (simulation-plan-event-dates plan))))
                  :element-type 'double-float :initial-element 0d0))))

(defun fixing-market (plan k states)
  "A market on event K's date holding only the forecast curves of PLAN's :FIX
indexes, as the leg driving each (or the :DERIVE form, or the initial
market) gives it on this path."
  (let* ((initial (simulation-plan-market plan))
         (date (simulation-date plan k))
         (curves (make-hash-table :test 'equal))
         (full nil))
    (iter (for index in-vector (simulation-plan-fix plan))
          (for id = (index-forecast-curve index))
          (for target = `(:curve ,id))
          (unless (gethash id curves)
            (setf (gethash id curves)
                  (let ((i (position target (simulation-plan-legs plan) :key #'simulation-leg-target :test #'equal)))
                    (cond (i (let ((leg (aref (simulation-plan-legs plan) i)))
                               (apply #'structure-at-state (simulation-leg-process leg) target
                                      (simulation-leg-initial leg) initial date (svref states i)
                                      (simulation-leg-options leg))))
                          ((assoc target (simulation-plan-derive plan) :test #'equal)
                           (market-component (or full (setf full (market-at-state initial plan date states)))
                                             target))
                          (t (find-yield-curve initial id)))))))
    (make-instance 'composite-market
                   :valuation-date date :day-counter (market-day-counter initial) :underlyings '()
                   :spots (make-hash-table :test 'equal) :yield-curves curves
                   :dividend-curves (make-hash-table :test 'equal)
                   :vol-structures (make-hash-table :test 'equal)
                   :underlying-currency (make-hash-table :test 'equal)
                   :discount-curve-for (make-hash-table :test 'equal))))

(defun path-fixings! (plan k states buffer)
  "Store in BUFFER each :FIX index's fixing on event K's date (NaN on a day
the index does not fix), from that date's forecast curve on this path."
  (when buffer
    (let* ((date (simulation-date plan k))
           (fix (simulation-plan-fix plan))
           (market nil))
      (iter (for index in-vector fix)
            (for i from 0)
            (setf (aref buffer (+ i (* k (length fix))))
                  (if (business-day-p (index-fixing-calendar index) date)
                      (if (zerop k)
                          (index-fixing index (simulation-plan-market plan) date)
                          (index-fixing index (or market (setf market (fixing-market plan k states))) date))
                      (quiet-nan)))))))

(defun add-path-fixings (plan market k buffer)
  "Add to MARKET, built for event K and not yet shared, the path's fixings in
BUFFER on the dates before K, on top of the initial market's."
  (let ((fix (simulation-plan-fix plan))
        (tables (market-fixings market)))
    (iter (for index in-vector fix)
          (for i from 0)
          (for id = (index-name index))
          (let ((table (make-hash-table :test 'equalp)))
            (a:when-let ((old (gethash id tables)))
              (maphash (lambda (d r) (setf (gethash d table) r)) old))
            (iter (for j below k)
                  (for rate = (aref buffer (+ i (* j (length fix)))))
                  (unless (nan-p rate)
                    (setf (gethash (simulation-date plan j) table) rate)))
            (setf (gethash id tables) table)))))
