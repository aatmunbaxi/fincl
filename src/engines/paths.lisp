;;;; engines/paths.lisp --- time grids and the path-kernel protocol
;;;;
;;;; A path simulation steps a process over a TIME-GRID that contains every
;;;; date the contract depends on (MAKE-TIME-GRID), and shows each state to a
;;;; payoff's PATH-KERNEL: three typed closures over a per-path accumulator,
;;;; INIT, STEP (which can record cash flows and stop the path) and FINAL.
;;;; PAYOFF-REQUIREMENTS tells the engine what a payoff needs, so a payoff
;;;; that needs only the terminal observable keeps the terminal-sampler fast
;;;; path. Every payoff has a path kernel: the default for path-independent
;;;; payoffs wraps PAYOFF-KERNEL.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Time grids
;;; --------------------------------------------------------------------

(defstruct (time-grid (:constructor %make-time-grid))
  "Times from 0 (the valuation date) in market time, with the grid index of
each mandatory date. Read-only."
  (market nil :read-only t)
  (times nil :type double-vector :read-only t)
  (dates nil :type simple-vector :read-only t)    ; mandatory dates, sorted
  (indices nil :type (simple-array fixnum (*)) :read-only t))

(defun make-time-grid (market &key dates n-steps max-dt)
  "A grid from MARKET's valuation date to the last of DATES containing every
date of DATES after the valuation date exactly. Between consecutive dates it
fills evenly so that no step exceeds MAX-DT (default: the span over N-STEPS;
with neither, each gap is one step). With DATES the expiry alone and N-STEPS
given, it is the uniform grid of N-STEPS steps.

  (make-time-grid m :dates (list d1 d2 expiry) :n-steps 50)"
  (check-input dates (and list (not null)) "a non-empty list of dates")
  (let* ((today (valuation-date market))
         (dates (-<> (copy-list dates)
                     (remove-if (lambda (d) (date<= d today)) <>)
                     (sort <> #'date<)
                     (remove-duplicates <> :test #'date=))))
    (unless dates
      (signal-invalid-argument 'dates dates 'list (format nil "dates after the valuation date ~A" today)))
    (let* ((mandatory (mapcar (lambda (d) (float (market-time market d) 1d0)) dates))
           (horizon (a:lastcar mandatory))
           (max-dt (cond (max-dt (check-input max-dt (real (0)) "a positive time step")
                                 (float max-dt 1d0))
                         (n-steps (check-input n-steps (integer 1) "a positive number of steps")
                                  (/ horizon n-steps))
                         (t nil)))
           (times (list 0d0))
           (indices '())
           (begin 0d0))
      (dolist (end mandatory)
        (let* ((span (- end begin))
               (k (if max-dt (max 1 (ceiling (- (/ span max-dt) 1d-9))) 1))
               (dt (/ span k)))
          (iter (for i from 1 below k) (push (+ begin (* i dt)) times))
          (push end times)
          (push (1- (length times)) indices)
          (setf begin end)))
      (%make-time-grid :market market
                       :times (coerce (nreverse times) 'double-vector)
                       :dates (coerce dates 'simple-vector)
                       :indices (coerce (nreverse indices) '(simple-array fixnum (*)))))))

(defun grid-index-of (grid date)
  "The grid index of the mandatory DATE, or NIL if it is not on GRID."
  (let ((position (position date (time-grid-dates grid) :test #'date=)))
    (and position (aref (time-grid-indices grid) position))))

(defun grid-date-mask (grid dates)
  "A bit vector over GRID's indices with 1 at each of DATES (mandatory dates
on the grid), and a fixnum vector giving, at those indices, the position of
the date in DATES: (values mask positions)."
  (let* ((n (length (time-grid-times grid)))
         (mask (make-array n :element-type 'bit :initial-element 0))
         (positions (make-array n :element-type 'fixnum :initial-element -1)))
    (iter (for date in dates)
          (for k from 0)
          (let ((index (grid-index-of grid date)))
            (when index
              (setf (sbit mask index) 1
                    (aref positions index) k))))
    (values mask positions)))

;;; --------------------------------------------------------------------
;;; The path-kernel protocol
;;; --------------------------------------------------------------------

(defgeneric payoff-requirements (payoff path-feature)
  (:documentation "What PAYOFF under PATH-FEATURE needs from a simulation, a
list of keywords: :TERMINAL (the observable at expiry only), :PATH
(observations on grid dates), :STATE (latent state components), :CASHFLOWS
(pays before expiry, or stops early). An engine uses the terminal sampler
only for (:TERMINAL).")
  (:method ((p payoff) (f path-independent)) '(:terminal)))

(defgeneric path-accumulator-size (payoff path-feature)
  (:documentation "Length of the per-path accumulator PATH-KERNEL's closures
share.")
  (:method ((p payoff) (f path-feature)) 0))

(defgeneric path-payment-dates (payoff path-feature)
  (:documentation "The dates on which a path kernel's PAY records cash flows,
in the order of PAY's payment index, or :GRID: the payment index is a grid
index and the cash flow is paid at that grid time (a rebate paid at a barrier
hit). The engine discounts each to its date.")
  (:method ((p payoff) (f path-feature)) '()))

(defgeneric path-kernel (payoff path-feature process grid)
  (:documentation "(values init step final): typed closures over a per-path
accumulator ACC, a (SIMPLE-ARRAY DOUBLE-FLOAT (*)) of PATH-ACCUMULATOR-SIZE.

  (init acc)                resets ACC at the start of a path;
  (step acc j state pay)    sees the full STATE at grid index J (0 included)
                            and returns true to continue, NIL to stop the
                            path; (funcall pay amount k) records AMOUNT paid
                            on payment date K (PATH-PAYMENT-DATES);
  (final acc state)         the amount paid at expiry, if not stopped.

PROCESS is resolved; read latent components through STATE-INDEX here, once.
GRID is the simulation's TIME-GRID. The default for path-independent payoffs
wraps PAYOFF-KERNEL: STEP does nothing and FINAL reads the observable.")
  (:method ((p payoff) (f path-independent) process grid)
    (declare (ignore process grid))
    (let ((kernel (payoff-kernel p)))
      (declare (type (function (double-float) double-float) kernel))
      (values (lambda (acc) (declare (ignore acc)) nil)
              (lambda (acc j state pay) (declare (ignore acc j state pay)) t)
              (lambda (acc state)
                (declare (ignore acc) (type (simple-array double-float (*)) state))
                (funcall kernel (aref state 0)))))))

(defun instrument-grid-dates (exercise payoff path)
  "Every date a simulation of the option made of EXERCISE, PAYOFF and PATH
must hit: its mandatory dates and its payment dates."
  (let ((payments (path-payment-dates payoff path)))
    (append (exercise-mandatory-dates exercise) (path-mandatory-dates path)
            (payoff-mandatory-dates payoff) (if (listp payments) payments '()))))

;;; --------------------------------------------------------------------
;;; Path features: Asian, barrier and lookback kernels
;;; --------------------------------------------------------------------

(defvar *barrier-bridge* t
  "Whether continuously monitored barriers use the Brownian-bridge crossing
probability between grid points (BRIDGE-VARIANCE). Bound by the Monte Carlo
engine from its BARRIER-BRIDGE slot; NIL monitors on grid points only.")

(defun observation-mask (grid dates)
  "GRID-DATE-MASK of DATES, or every grid index (0 included) when DATES is NIL."
  (if dates
      (values (grid-date-mask grid dates))
      (make-array (length (time-grid-times grid)) :element-type 'bit :initial-element 1)))

(defun knock-out-rebate-p (f)
  (and (eq (barrier-knock f) :out) (plusp (barrier-rebate f))))

(defmethod payoff-requirements ((p payoff) (f observed-path))
  '(:path))

(defmethod payoff-requirements ((p payoff) (f barrier))
  (if (knock-out-rebate-p f) '(:path :cashflows) '(:path)))

(defmethod path-payment-dates ((p payoff) (f barrier))
  (if (knock-out-rebate-p f) :grid '()))

(defmethod path-accumulator-size ((p payoff) (f asian)) 1)
(defmethod path-accumulator-size ((p payoff) (f lookback)) 1)
(defmethod path-accumulator-size ((p payoff) (f barrier)) 3)

;;; ACC[0] is the running sum of the observations, or of their logs.
(defmethod path-kernel ((p payoff) (f asian) process grid)
  (declare (ignore process))
  (let ((mask (observation-mask grid (observation-dates f)))
        (n (float (length (observation-dates f)) 1d0))
        (geometric (eq (asian-average f) :geometric))
        (floating (floating-p f))
        (phi (if (typep p 'call-put-payoff) (phi p) 0d0))
        (kernel (payoff-kernel p)))
    (declare (type simple-bit-vector mask) (double-float n phi)
             (type (function (double-float) double-float) kernel))
    (values (lambda (acc)
              (declare (type (simple-array double-float (*)) acc))
              (setf (aref acc 0) 0d0))
            (lambda (acc j state pay)
              (declare (ignore pay) (fixnum j) (type (simple-array double-float (*)) acc state))
              (when (= 1 (sbit mask j))
                (let ((s (aref state 0)))
                  (incf (aref acc 0) (if geometric (log s) s))))
              t)
            (lambda (acc state)
              (declare (type (simple-array double-float (*)) acc state))
              (let ((average (if geometric (exp (/ (aref acc 0) n)) (/ (aref acc 0) n))))
                (if floating
                    (max 0d0 (* phi (- (aref state 0) average)))
                    (funcall kernel average)))))))

;;; ACC[0] is the running extremum: the maximum when a call is fixed or a
;;; put floating, else the minimum.
(defmethod path-kernel ((p payoff) (f lookback) process grid)
  (declare (ignore process))
  (let* ((mask (observation-mask grid (observation-dates f)))
         (phi (phi p))
         (floating (floating-p f))
         (maximum (if floating (minusp phi) (plusp phi)))
         (kernel (payoff-kernel p)))
    (declare (type simple-bit-vector mask) (double-float phi)
             (type (function (double-float) double-float) kernel))
    (values (lambda (acc)
              (declare (type (simple-array double-float (*)) acc))
              (setf (aref acc 0) (if maximum 0d0 most-positive-double-float)))
            (lambda (acc j state pay)
              (declare (ignore pay) (fixnum j) (type (simple-array double-float (*)) acc state))
              (when (= 1 (sbit mask j))
                (let ((s (aref state 0)))
                  (setf (aref acc 0) (if maximum (max s (aref acc 0)) (min s (aref acc 0))))))
              t)
            (lambda (acc state)
              (declare (type (simple-array double-float (*)) acc state))
              (let ((extremum (aref acc 0)))
                (if floating
                    (max 0d0 (* phi (- (aref state 0) extremum)))
                    (funcall kernel extremum)))))))

;;; ACC[0] is the probability the path has not touched the barrier: 0 or 1
;;; under discrete monitoring; under continuous monitoring with the bridge,
;;; the product of the bridge survival probabilities of the steps so far.
;;; ACC[1] is ln(S/H) at the previous grid point and ACC[2] the variance of
;;; ln S over the step that follows it. A knock-out pays its rebate at the
;;; grid time the barrier is touched (the end of the step it is crossed in)
;;; and stops once it has surely been touched; a knock-in pays the payoff
;;; with the probability of a touch, and the rebate otherwise.
(defmethod path-kernel ((p payoff) (f barrier) process grid)
  (let* ((market (time-grid-market grid))
         (times (time-grid-times grid))
         (n (1- (length times)))
         (level (barrier-level f))
         (up (eq (barrier-direction f) :up))
         (out (eq (barrier-knock f) :out))
         (rebate (barrier-rebate f))
         (continuous (null (observation-dates f)))
         (mask (observation-mask grid (observation-dates f)))
         (bridge (when (and continuous *barrier-bridge*)
                   (bridge-variance process market times)))
         (kernel (payoff-kernel p)))
    (declare (fixnum n) (double-float level rebate) (type simple-bit-vector mask)
             (type (function (double-float) double-float) kernel))
    (values (lambda (acc)
              (declare (type (simple-array double-float (*)) acc))
              (setf (aref acc 0) 1d0))
            (lambda (acc j state pay)
              (declare (fixnum j) (type (simple-array double-float (*)) acc state)
                       (type function pay))
              (let* ((s (aref state 0))
                     ;; ln(S/H); a path absorbed at 0 is far below any barrier.
                     (x (if (plusp s) (log (/ s level)) -1d150))
                     (touch (cond ((zerop (sbit mask j)) 0d0)
                                  ((if up (>= s level) (<= s level)) 1d0)
                                  ((and bridge (plusp j) (plusp (aref acc 2)))
                                   (exp (/ (* -2d0 (aref acc 1) x) (aref acc 2))))
                                  (t 0d0))))
                (declare (double-float s x touch))
                (when (plusp touch)
                  (let ((survival (aref acc 0)))
                    (when (and out (plusp rebate))
                      (funcall pay (* rebate survival touch) j))
                    (setf (aref acc 0) (* survival (- 1d0 touch)))))
                (when (and bridge (< j n))
                  (setf (aref acc 1) x)
                  (funcall (the function bridge) state j acc 2))
                (not (and out (zerop (aref acc 0))))))
            (lambda (acc state)
              (declare (type (simple-array double-float (*)) acc state))
              (let ((survival (aref acc 0))
                    (value (funcall kernel (aref state 0))))
                (if out
                    (* survival value)
                    (+ (* (- 1d0 survival) value) (* survival rebate))))))))
