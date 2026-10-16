;;;; invariants.lisp --- model-free no-arbitrage relations for every engine
;;;;
;;;; The sweep finds every concrete engine class through the MOP and prices a
;;;; grid of options under a few processes. It then checks relations that
;;;; hold for any arbitrage-free model: price bounds, put-call parity,
;;;; monotonicity and convexity in the strike, and American >= Bermudan >=
;;;; European. A new engine or process is covered by adding one
;;;; TEST-ENGINE-INSTANCE method, or one entry in INVARIANT-PROCESSES.
;;;; Combinations an engine does not support are recorded in the coverage
;;;; matrix, not treated as failures.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; Engines under test
;;; --------------------------------------------------------------------

(defgeneric test-engine-instance (class-name process)
  (:documentation "An instance of the engine class CLASS-NAME with
test-sized settings, using PROCESS where the engine takes one. NIL when the
engine does not take a process and PROCESS is not the one its model fixes
(an analytic engine is GBM by construction).

  (test-engine-instance 'monte-carlo-engine (make-instance 'gbm))"))

(defun gbm-p (process) (eq (class-name (class-of process)) 'gbm))

(macrolet ((process-free (class)
             `(defmethod test-engine-instance ((c (eql ',class)) process)
                (when (gbm-p process) (make-instance ',class)))))
  (process-free black-scholes-engine)
  (process-free barone-adesi-whaley-engine))

(defmethod test-engine-instance ((c (eql 'merton-series-engine)) process)
  (when (typep process 'merton-jump) (make-instance c :process process)))

(defmethod test-engine-instance ((c (eql 'cev-engine)) process)
  (when (typep process 'cev) (make-instance c :process process)))

(defmethod test-engine-instance ((c (eql 'monte-carlo-engine)) process)
  (make-instance 'monte-carlo-engine :process process :n-paths 20000 :n-steps 50))

(defmethod test-engine-instance ((c (eql 'binomial-engine)) process)
  (make-instance 'binomial-engine :process process :n-steps 500))

(defmethod test-engine-instance ((c (eql 'cos-engine)) process)
  (make-instance 'cos-engine :process process))

(defun engine-classes ()
  "Every subclass of ENGINE, found by walking the class graph."
  (labels ((walk (class)
             (cons class (mapcan #'walk (closer-mop:class-direct-subclasses class)))))
    (remove-duplicates (rest (walk (find-class 'engine))))))

(defun has-test-instance-p (class)
  (find-method #'test-engine-instance '()
               (list `(eql ,(class-name class)) (find-class t)) nil))

(defun concrete-engine-class-p (class)
  "A leaf of the engine hierarchy, or an inner class with a test instance."
  (or (null (closer-mop:class-direct-subclasses class))
      (has-test-instance-p class)))

;;; --------------------------------------------------------------------
;;; Tolerances
;;; --------------------------------------------------------------------

(defgeneric invariant-tolerance (engine price-magnitude &optional standard-error)
  (:documentation "Absolute tolerance for an invariant on prices from
ENGINE. STANDARD-ERROR is the combined standard error of the prices the
invariant involves, for Monte Carlo engines.")
  (:method ((e analytic-engine) magnitude &optional se)
    (declare (ignore magnitude se))
    1d-8)
  (:method ((e cos-engine) magnitude &optional se)
    (declare (ignore magnitude se))
    1d-4)
  (:method ((e binomial-engine) magnitude &optional se)
    (declare (ignore magnitude se))
    1d-3)
  (:method ((e monte-carlo-engine) magnitude &optional se)
    (declare (ignore magnitude))
    (+ (* 4 (or se 0d0)) 1d-10))
  ;; Automatic selection: Monte Carlo when there is a standard error, else
  ;; the loosest deterministic engine's (the tree's).
  (:method ((e stochastic-process) magnitude &optional se)
    (declare (ignore magnitude))
    (if se (+ (* 4 se) 1d-10) 1d-3)))

;;; --------------------------------------------------------------------
;;; The grid
;;; --------------------------------------------------------------------

(defparameter *invariant-strikes* '(80 90 100 110 120)
  "Equally spaced, so interior strikes have butterflies of width 10.")

(defparameter *invariant-styles* '(:european :american :bermudan))

(defparameter *invariant-expiry* #D"2027-09-24")

(defparameter *invariant-bermudan-dates*
  (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24"))

(defun invariant-market (&key (rate 0.05d0))
  "A dividend yield above zero, so American calls carry an early-exercise
premium."
  (make-bs-market :valuation-date *today* :spot 100 :rate rate
                  :dividend 0.03d0 :vol 0.25d0))

(defun invariant-processes ()
  "(label process) pairs."
  (list (list "gbm" (make-instance 'gbm))
        (list "cev" (make-instance 'cev :beta 0.7d0))
        (list "heston" (make-instance 'heston :xi 0.5d0 :rho -0.7d0))
        (list "merton" (make-instance 'merton-jump))
        (list "kou" (make-instance 'kou-jump))))

(defun invariant-option (style kind strike)
  (make-option-from-clauses `((,kind ,strike)
                 ,@(if (eq style :bermudan)
                       `((:bermudan :dates ,*invariant-bermudan-dates*))
                       `((,style) (:expires ,*invariant-expiry*))))))

(defun price-or-status (option engine market)
  "(:ok price se), :unsupported, or (:error message)."
  (handler-case (multiple-value-bind (v se) (price option engine market)
                  (list :ok v se))
    (unsupported-combination () :unsupported)
    (error (c) (list :error (princ-to-string c)))))

(defun price-grid (engine market)
  "Hash table from (style kind strike) to PRICE-OR-STATUS."
  (let ((grid (make-hash-table :test 'equal)))
    (dolist (style *invariant-styles* grid)
      (dolist (kind '(:call :put))
        (dolist (strike *invariant-strikes*)
          (setf (gethash (list style kind strike) grid)
                (price-or-status (invariant-option style kind strike) engine market)))))))

;;; --------------------------------------------------------------------
;;; The invariants
;;; --------------------------------------------------------------------

(defvar *invariant-count* 0)
(defvar *cell-failed*)

(defun ok-result-p (r) (and (consp r) (eq (first r) :ok)))

(defun invariant (engine label terms bound description)
  "Check that sum c_i v_i >= BOUND, up to ENGINE's tolerance. TERMS is a
list of (coefficient result), results as from PRICE-OR-STATUS; the check is
skipped unless all of them priced."
  (when (every (lambda (term) (ok-result-p (second term))) terms)
    (incf *invariant-count*)
    (let* ((value (iter (for (c r) in terms) (sum (* c (second r)))))
           (ses (iter (for (c r) in terms)
                      (when (third r) (collect (* c (third r))))))
           (se (and ses (sqrt (iter (for x in ses) (sum (* x x))))))
           (scale (if se 1 (iter (for (c) in terms) (sum (abs c)))))
           (tol (* scale (invariant-tolerance engine (abs value) se))))
      (unless (>= value (- bound tol))
        (setf *cell-failed* t)
        (incf *failures*)
        (format t "~&  FAIL ~A: ~A~%       ~,8F < ~,8F (tolerance ~,2E)~%"
                label description value bound tol)))))

(defun check-invariants (engine grid market style label)
  (let* ((s (spot market))
         (d (discount-factor market *invariant-expiry*))
         (e-q (* (forward market *invariant-expiry*) d (/ s)))) ; exp(-qT)
    (flet ((at (kind strike &optional (st style)) (gethash (list st kind strike) grid)))
      (dolist (kind '(:call :put))
        (let ((phi (if (eq kind :call) 1 -1))
              (tag (format nil "~(~A ~A~)" style kind)))
          (dolist (k *invariant-strikes*)
            (let ((v (at kind k))
                  (where (format nil "~A K=~D" tag k))
                  ;; European lower bound: max(0, phi (S e^-qT - K D)).
                  (euro-floor (max 0 (* phi (- (* s e-q) (* k d))))))
              (invariant engine label `((1 ,v)) 0 (format nil "~A: price >= 0" where))
              ;; Upper bounds: the European call pays at most S_T, the
              ;; European put at most K; early exercise can collect them
              ;; now, so the American bounds are S and K.
              (invariant engine label `((-1 ,v))
                         (- (if (eq style :european)
                                (if (= phi 1) (* s e-q) (* k d))
                                (if (= phi 1) s k)))
                         (format nil "~A: below its upper bound" where))
              (invariant engine label `((1 ,v)) euro-floor
                         (format nil "~A: above the European lower bound" where))
              (when (eq style :american)
                (invariant engine label `((1 ,v)) (max 0 (* phi (- s k)))
                           (format nil "~A: above intrinsic" where)))
              (when (and (eq style :european) (= phi 1))
                (let ((put (at :put k)))
                  (invariant engine label `((1 ,v) (-1 ,put)) (- (* s e-q) (* k d))
                             (format nil "~A: C - P >= D (F - K)" where))
                  (invariant engine label `((-1 ,v) (1 ,put)) (- (* k d) (* s e-q))
                             (format nil "~A: C - P <= D (F - K)" where))))
              ;; American >= Bermudan >= European, or American >= European
              ;; when the engine has no Bermudan method.
              (when (eq style :american)
                (let ((bermudan (at kind k :bermudan)) (european (at kind k :european)))
                  (if (ok-result-p bermudan)
                      (invariant engine label `((1 ,v) (-1 ,bermudan)) 0
                                 (format nil "~A: American >= Bermudan" where))
                      (invariant engine label `((1 ,v) (-1 ,european)) 0
                                 (format nil "~A: American >= European" where)))))
              (when (eq style :bermudan)
                (invariant engine label `((1 ,v) (-1 ,(at kind k :european))) 0
                           (format nil "~A: Bermudan >= European" where)))))
          ;; Monotone in strike: calls fall and puts rise as K increases.
          (iter (for (k1 k2) on *invariant-strikes*) (while k2)
                (invariant engine label `((,phi ,(at kind k1)) (,(- phi) ,(at kind k2))) 0
                           (format nil "~A: monotone in strike, K=~D..~D" tag k1 k2)))
          ;; Convex in strike: butterflies are worth at least zero.
          (iter (for (k1 k2 k3) on *invariant-strikes*) (while k3)
                (invariant engine label
                           `((1 ,(at kind k1)) (-2 ,(at kind k2)) (1 ,(at kind k3))) 0
                           (format nil "~A: convex in strike at K=~D" tag k2))))))))

;;; --------------------------------------------------------------------
;;; The sweep and its coverage matrix
;;; --------------------------------------------------------------------

(defun cell-status (grid style failed)
  (let ((results (iter (for kind in '(:call :put))
                       (appending (iter (for k in *invariant-strikes*)
                                        (collect (gethash (list style kind k) grid)))))))
    (cond ((some (lambda (r) (and (consp r) (eq (first r) :error))) results) "ERROR")
          (failed "FAIL")
          ((every (lambda (r) (eq r :unsupported)) results) "--")
          ((some (lambda (r) (eq r :unsupported)) results) "part")
          (t "ok"))))

(defun print-coverage (rows processes)
  (let ((columns (iter (for style in *invariant-styles*)
                       (appending (iter (for (label) in processes)
                                        (collect (format nil "~(~A~)/~A"
                                                         (subseq (string style) 0 3) label)))))))
    (format t "~&~%  Coverage (--: unsupported, n/a: engine has no such process)~%")
    (format t "  ~28A~{ ~10@A~}~%" "" columns)
    (iter (for (name . cells) in rows)
          (format t "  ~28A~{ ~10@A~}~%" (string-downcase name) cells))))

(defun invariant-markets ()
  "(label market) pairs: a flat market with a dividend yield, the same at zero
and negative rates, and a market with rate, dividend and vol term structure."
  (list (list "flat" (invariant-market))
        (list "flat r=0" (invariant-market :rate 0))
        (list "flat r=-1%" (invariant-market :rate -0.01d0))
        (list "piecewise" (make-piecewise-market))))

(defun greek-or-status (option engine market)
  "Delta, gamma and vega as PRICE-OR-STATUS results (:ok value se), or
:unsupported."
  (handler-case
      (let* ((g (greeks option engine market :which '(:delta :gamma :vega)))
             (ses (getf g :standard-errors)))
        (iter (for key in '(:delta :gamma :vega))
              (appending `(,key (:ok ,(getf g key) ,(getf ses key))))))
    (unsupported-combination () :unsupported)
    (error (c) `(:error ,(princ-to-string c)))))

(defun check-greek-invariants (engine market label)
  "For an at-the-money call of each style: 0 <= delta <= e^-qT (European) or
1 (American), gamma >= 0, vega >= 0, up to ENGINE's tolerance."
  (let ((e-q (* (forward market *invariant-expiry*) (discount-factor market *invariant-expiry*)
                (/ (spot market)))))
    (dolist (style '(:european :american))
      (let ((g (greek-or-status (invariant-option style :call 100) engine market))
            (where (format nil "~(~A~) call K=100" style)))
        (cond ((eq g :unsupported))
              ((eq (first g) :error)
               (setf *cell-failed* t)
               (incf *failures*)
               (format t "~&  FAIL ~A ~A Greeks: ~A~%" label where (second g)))
              (t
               (let ((delta (getf g :delta)))
                 (invariant engine label `((1 ,delta)) 0 (format nil "~A: delta >= 0" where))
                 (invariant engine label `((-1 ,delta)) (- (if (eq style :european) e-q 1))
                            (format nil "~A: delta <= ~:[1~;e^-qT~]" where (eq style :european)))
                 (invariant engine label `((1 ,(getf g :gamma))) 0 (format nil "~A: gamma >= 0" where))
                 (invariant engine label `((1 ,(getf g :vega))) 0 (format nil "~A: vega >= 0" where)))))))))

(defun invariant-sweep (market market-label processes)
  "Check every tested engine on MARKET; return the coverage rows."
  (let ((rows '()))
    (dolist (class (engine-classes) rows)
      (let ((name (class-name class)))
        (when (and (concrete-engine-class-p class) (has-test-instance-p class))
          (let ((cells (make-hash-table :test 'equal)))
            (iter (for (label process) in processes)
                  (for engine = (test-engine-instance name process))
                  (for where = (format nil "~(~A~)/~A/~A" name label market-label))
                  (if (null engine)
                      (dolist (style *invariant-styles*)
                        (setf (gethash (list style label) cells) "n/a"))
                      (let ((grid (price-grid engine market)))
                        (maphash (lambda (key r)
                                   (when (and (consp r) (eq (first r) :error))
                                     (incf *failures*)
                                     (format t "~&  FAIL ~A ~S: ~A~%" where key (second r))))
                                 grid)
                        (dolist (style *invariant-styles*)
                          (let ((*cell-failed* nil))
                            (check-invariants engine grid market style where)
                            (setf (gethash (list style label) cells)
                                  (cell-status grid style *cell-failed*))))
                        ;; Bump Greeks need named factors: composite markets only.
                        (when (typep market 'composite-market)
                          (let ((*cell-failed* nil))
                            (check-greek-invariants engine market where))))))
            (push (cons name (iter (for style in *invariant-styles*)
                                   (appending (iter (for (label) in processes)
                                                    (collect (gethash (list style label) cells))))))
                  rows)))))))

(defun auto-sweep (market market-label processes)
  "The coverage row of automatic selection: every cell priced through
(price option process market), Monte Carlo fallbacks at 10,000 paths.
A cell automatic selection cannot price is a failure."
  (let ((cells (make-hash-table :test 'equal)))
    (handler-bind ((engine-fallback #'muffle-warning))
      (iter (for (label process) in processes)
            (for where = (format nil "auto/~A/~A" label market-label))
            (let ((grid (with-engines ((:fallback '(:mc :n-paths 10000 :n-steps 50)))
                          (price-grid process market))))
              (maphash (lambda (key r)
                         (when (or (eq r :unsupported) (and (consp r) (eq (first r) :error)))
                           (incf *failures*)
                           (format t "~&  FAIL ~A ~S: ~A~%" where key
                                   (if (consp r) (second r) "no engine prices it"))))
                       grid)
              (dolist (style *invariant-styles*)
                (let ((*cell-failed* nil))
                  (check-invariants process grid market style where)
                  (setf (gethash (list style label) cells) (cell-status grid style *cell-failed*)))))))
    (cons 'auto (iter (for style in *invariant-styles*)
                      (appending (iter (for (label) in processes)
                                       (collect (gethash (list style label) cells))))))))

(defun invariant-checks ()
  (format t "~&Invariants: no-arbitrage relations over every engine~%")
  (let ((processes (invariant-processes))
        (*invariant-count* 0))
    (dolist (class (engine-classes))
      (when (and (concrete-engine-class-p class) (not (has-test-instance-p class)))
        (incf *failures*)
        (format t "~&  FAIL ~(~A~) is untested: define a TEST-ENGINE-INSTANCE method~%"
                (class-name class))))
    (iter (for (label market) in (invariant-markets))
          (format t "~&  ~A market~%" label)
          (print-coverage (append (sort (invariant-sweep market label processes) #'string< :key #'car)
                                  (list (auto-sweep market label processes)))
                          processes))
    (format t "~&  ~D invariants checked~%" *invariant-count*)))

(defun run-invariant-tests ()
  "Run only the invariant sweep. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (invariant-checks))
    (format t "~&~[All invariant checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
