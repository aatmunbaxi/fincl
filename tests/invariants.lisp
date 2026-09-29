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
    (+ (* 4 (or se 0d0)) 1d-10)))

;;; --------------------------------------------------------------------
;;; The grid
;;; --------------------------------------------------------------------

(defparameter *invariant-strikes* '(80 90 100 110 120)
  "Equally spaced, so interior strikes have butterflies of width 10.")

(defparameter *invariant-styles* '(:european :american :bermudan))

(defparameter *invariant-expiry* #D"2027-09-24")

(defparameter *invariant-bermudan-dates*
  (list #D"2026-12-24" #D"2027-03-24" #D"2027-06-24" #D"2027-09-24"))

(defun invariant-market ()
  "A dividend yield above zero, so American calls carry an early-exercise
premium."
  (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                  :dividend 0.03d0 :vol 0.25d0))

(defun invariant-processes ()
  "(label process) pairs."
  (list (list "gbm" (make-instance 'gbm))
        (list "cev" (make-instance 'cev :beta 0.7d0))
        (list "heston" (make-instance 'heston :xi 0.5d0 :rho -0.7d0))))

(defun invariant-option (style kind strike)
  (if (eq style :bermudan)
      (make-instance 'option
                     :payoff (make-instance 'vanilla-payoff :kind kind
                                                            :strike (float strike 1d0))
                     :exercise (make-instance 'bermudan-exercise
                                              :dates *invariant-bermudan-dates*))
      (make-option (list style kind strike *invariant-expiry*))))

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
    (let* ((value (loop for (c r) in terms sum (* c (second r))))
           (ses (loop for (c r) in terms
                      when (third r) collect (* c (third r))))
           (se (and ses (sqrt (loop for x in ses sum (* x x)))))
           (scale (if se 1 (loop for (c) in terms sum (abs c))))
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
          (loop for (k1 k2) on *invariant-strikes* while k2
                do (invariant engine label `((,phi ,(at kind k1)) (,(- phi) ,(at kind k2))) 0
                              (format nil "~A: monotone in strike, K=~D..~D" tag k1 k2)))
          ;; Convex in strike: butterflies are worth at least zero.
          (loop for (k1 k2 k3) on *invariant-strikes* while k3
                do (invariant engine label
                              `((1 ,(at kind k1)) (-2 ,(at kind k2)) (1 ,(at kind k3))) 0
                              (format nil "~A: convex in strike at K=~D" tag k2))))))))

;;; --------------------------------------------------------------------
;;; The sweep and its coverage matrix
;;; --------------------------------------------------------------------

(defun cell-status (grid style failed)
  (let ((results (loop for kind in '(:call :put)
                       append (loop for k in *invariant-strikes*
                                    collect (gethash (list style kind k) grid)))))
    (cond ((some (lambda (r) (and (consp r) (eq (first r) :error))) results) "ERROR")
          (failed "FAIL")
          ((every (lambda (r) (eq r :unsupported)) results) "--")
          ((some (lambda (r) (eq r :unsupported)) results) "part")
          (t "ok"))))

(defun print-coverage (rows processes)
  (let ((columns (loop for style in *invariant-styles*
                       append (loop for (label) in processes
                                    collect (format nil "~(~A~)/~A"
                                                    (subseq (string style) 0 3) label)))))
    (format t "~&~%  Coverage (--: unsupported, n/a: engine has no such process)~%")
    (format t "  ~28A~{ ~10@A~}~%" "" columns)
    (loop for (name . cells) in rows
          do (format t "  ~28A~{ ~10@A~}~%" (string-downcase name) cells))))

(defun invariant-checks ()
  (format t "~&Invariants: no-arbitrage relations over every engine~%")
  (let ((market (invariant-market))
        (processes (invariant-processes))
        (*invariant-count* 0)
        (rows '()))
    (dolist (class (engine-classes))
      (let ((name (class-name class)))
        (cond
          ((not (concrete-engine-class-p class)))
          ((not (has-test-instance-p class))
           (incf *failures*)
           (format t "~&  FAIL ~(~A~) is untested: define a TEST-ENGINE-INSTANCE method~%"
                   name))
          (t
           (let ((cells (make-hash-table :test 'equal)))
             (loop for (label process) in processes
                   for engine = (test-engine-instance name process)
                   do (if (null engine)
                          (dolist (style *invariant-styles*)
                            (setf (gethash (list style label) cells) "n/a"))
                          (let ((grid (price-grid engine market)))
                            (maphash (lambda (key r)
                                       (when (and (consp r) (eq (first r) :error))
                                         (incf *failures*)
                                         (format t "~&  FAIL ~(~A~)/~A ~S: ~A~%"
                                                 name label key (second r))))
                                     grid)
                            (dolist (style *invariant-styles*)
                              (let ((*cell-failed* nil))
                                (check-invariants engine grid market style
                                                  (format nil "~(~A~)/~A" name label))
                                (setf (gethash (list style label) cells)
                                      (cell-status grid style *cell-failed*)))))))
             (push (cons name (loop for style in *invariant-styles*
                                    append (loop for (label) in processes
                                                 collect (gethash (list style label) cells))))
                   rows))))))
    (print-coverage (sort rows #'string< :key #'car) processes)
    (format t "~&  ~D invariants checked~%" *invariant-count*)))

(defun run-invariant-tests ()
  "Run only the invariant sweep. Returns T when all pass."
  (let ((*failures* 0))
    (with-pricing-kernel ()
      (invariant-checks))
    (format t "~&~[All invariant checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
