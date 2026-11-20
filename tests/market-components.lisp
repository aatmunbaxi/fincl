;;;; market-components.lisp --- kinds of market component defined from outside
;;;;
;;;; The package below uses only fincl's exported symbols, as user code
;;;; would. It defines an FX rate (a real), a hazard curve (a structure with
;;;; its own bump and snapshot form), a sub-clause of an underlying, and a
;;;; kind whose values have no registered snapshot constructor. The checks
;;;; build markets with them, bump, print, snapshot and simulate, and test
;;;; the registry's refusals. A reader walk over src/ checks that no
;;;; function branches on a component kind.

(defpackage #:fincl-outside-kinds
  (:use #:cl)
  (:export #:hazard-curve #:make-hazard-curve #:hazard-intensity #:make-recovery))

(in-package #:fincl-outside-kinds)
(named-readtables:in-readtable fincl:syntax)

(fincl:define-market-component :fx
  :type (real (0)) :description "a positive FX rate" :what "FX rate"
  :clause (fx (pair rate))
  :documentation "The FX rate of PAIR, a keyword: units of the quote currency per unit of the base.")

(defclass hazard-curve ()
  ((intensity :initarg :intensity :reader hazard-intensity))
  (:documentation "A flat default intensity."))

(defun make-hazard-curve (&key intensity)
  (make-instance 'hazard-curve :intensity intensity))

(defmethod fincl:bump-structure ((c hazard-curve) shift)
  (make-hazard-curve :intensity (fincl:apply-shift (hazard-intensity c) shift)))

(defmethod fincl:snapshot-form ((c hazard-curve))
  `(make-hazard-curve :intensity ,(hazard-intensity c)))

(fincl:define-market-component :hazard
  :type hazard-curve :description "a HAZARD-CURVE"
  :clause (hazard (issuer curve))
  :snapshot-constructors (make-hazard-curve)
  :documentation "The hazard curve of ISSUER, a keyword.")

(defclass recovery ()
  ((rate :initarg :rate :reader recovery-rate))
  (:documentation "A recovery rate, whose snapshot constructor no kind registers."))

(defun make-recovery (&key rate)
  (make-instance 'recovery :rate rate))

(defmethod fincl:snapshot-form ((r recovery))
  `(make-recovery :rate ,(recovery-rate r)))

(fincl:define-market-component :recovery
  :type recovery :description "a RECOVERY"
  :clause (recovery (issuer value))
  :documentation "A recovery rate without a registered snapshot constructor.")

(fincl:define-market-component :beta
  :type real :description "a real beta" :coerce (lambda (x) (float x 1d0))
  :within (underlying beta (value))
  :documentation "The beta of an underlying to its index.")

(fincl:define-market-component :redefinable
  :type real :clause (redefinable (key value))
  :documentation "A kind another package defines again.")

(defparameter *market*
  (fincl:make-market (valued-on #D"2026-09-24")
                     (underlying :spx (spot 5000) (currency :usd) (vol 0.2d0) (beta 1.1d0))
                     (curve :usd (rate 0.04d0))
                     (fx :eurusd 1.08d0)
                     (fx :usdjpy 150)
                     (hazard :acme (make-hazard-curve :intensity 0.02d0))))

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

;;; --------------------------------------------------------------------
;;; No kind branches in src/
;;; --------------------------------------------------------------------

(defparameter *kind-branch-keys*
  '(:spot :vol :dividend :dividend-schedule :curve :discounting :fixings
    :spots :yield-curves :dividend-curves :dividend-schedules :vol-structures)
  "Component kinds, and the table names the market once branched on.")

(defparameter *kind-branch-exceptions*
  '((estimator-greeks "keys are the tangents of a pathwise estimator, not market kinds")
    (state-coordinate "keys are process state names"))
  "Top-level definitions whose CASE on these keywords is not about market components.")

(defun kind-branches (form)
  "The CASE, ECASE and CCASE forms in FORM keyed on a kind, EQL method
specializers on one, and MEMBER types of two or more."
  (let ((found '()))
    (labels ((keys (clauses)
               (iter (for clause in clauses)
                     (when (consp clause)
                       (let ((k (first clause)))
                         (appending (if (listp k) (copy-list k) (list k)))))))
             (walk (f)
               (when (consp f)
                 (cond ((and (member (first f) '(case ecase ccase)) (listp (cddr f))
                             (intersection (keys (cddr f)) *kind-branch-keys*))
                        (push f found))
                       ((and (eq (first f) 'eql) (consp (rest f)) (member (second f) *kind-branch-keys*))
                        (push f found))
                       ((and (eq (first f) 'member) (listp (rest f)) (every #'atom (rest f))
                             (<= 2 (length (intersection (rest f) *kind-branch-keys*))))
                        (push f found)))
                 (iter (for tail on f)
                       (walk (car tail))
                       (while (consp (cdr tail)))))))
      (walk form))
    found))

(defun source-kind-branches ()
  "((file definition form) ...) for each kind branch in src/ outside the exceptions."
  (let ((results '()))
    (dolist (file (directory (asdf:system-relative-pathname "fincl" "src/**/*.lisp")) results)
      (with-open-file (s file)
        (let ((*package* (find-package '#:fincl))
              (*readtable* (named-readtables:find-readtable 'fincl:syntax)))
          (iter (for form = (read s nil s))
                (until (eq form s))
                (when (and (consp form) (eq (first form) 'in-package))
                  (setf *package* (find-package (second form))))
                (let ((name (and (consp form) (consp (rest form)) (second form))))
                  (unless (and (symbolp name)
                               (member (symbol-name name) *kind-branch-exceptions*
                                       :key (lambda (e) (symbol-name (first e))) :test #'string=))
                    (dolist (branch (kind-branches form))
                      (push `(,(enough-namestring file (asdf:system-source-directory "fincl")) ,name ,branch)
                            results))))))))))

;;; --------------------------------------------------------------------
;;; Checks
;;; --------------------------------------------------------------------

(defun report-of (thunk)
  "The report of the error THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (error (c) (princ-to-string c))))

(defun eval-in-tests (form)
  "FORM evaluated with this package current, as a definition in it would be."
  (let ((*package* (find-package '#:fincl/tests)))
    (eval form)))

(defun component-rates-market (vol)
  "The bootstrap fixture's curves with the rates vol VOL under :EUR-CAP."
  (let ((curves (bootstrap-fixture-curves)))
    (fincl::make-market-from-specs :valuation-date #D"2026-09-24"
                                   :curves (mapcar (lambda (c) `(,(car c) :curve ,(cdr c))) curves)
                                   :discounting '((:usd . :sofr) (:eur . :estr))
                                   :components `((:rates-vol (:eur-cap ,vol))))))

(define-checks market-component-checks
  (format t "~&Market component kinds~%")
  (check-that "the walk finds an ECASE, an EQL specializer and a MEMBER type on kinds"
              (= 3 (length (kind-branches '(progn (ecase kind (:spot 1) ((:vol :curve) 2))
                                                  (defmethod f ((k (eql :curve))))
                                                  (check-input k (member :spot :vol)))))))
  (let ((branches (source-kind-branches)))
    (check-that (format nil "no function in src/ branches on a component kind~@[: ~S~]" branches)
                (null branches)))
  ;; An FX rate, a hazard curve and a beta from another package.
  (let ((m fincl-outside-kinds::*market*))
    (check-that "an outside kind is read as (kind key)"
                (and (= 1.08d0 (market-component m '(:fx :eurusd)))
                     (= 150 (market-component m '(:fx :usdjpy)))
                     (= 0.02d0 (fincl-outside-kinds:hazard-intensity (market-component m '(:hazard :acme))))
                     (= 1.1d0 (market-component m '(:beta :spx)))))
    (check "bumping an FX rate by 1% moves it by 1%"
           (* 1.08d0 1.01d0)
           (market-component (bump m '(:fx :eurusd) '(:relative 1/100)) '(:fx :eurusd))
           1d-15)
    (check-that "a bump leaves the original market and the other kinds alone"
                (let ((b (bump m '(:fx :eurusd) '(:relative 1/100))))
                  (and (= 1.08d0 (market-component m '(:fx :eurusd)))
                       (= 150 (market-component b '(:fx :usdjpy)))
                       (= 5000 (market-component b :spx)))))
    (check "a hazard curve bumps through its BUMP-STRUCTURE method"
           0.0201d0
           (fincl-outside-kinds:hazard-intensity
            (market-component (bump m '(:hazard :acme) '(:absolute 1/10000)) '(:hazard :acme)))
           1d-15)
    (check-that "(:spot :eurusd) is a spot, not an FX rate: the report lists the market's spots"
                (handler-case (progn (bump m '(:spot :eurusd) '(:relative 1/100)) nil)
                  (missing-market-data (c) (and (search ":SPX" (princ-to-string c))
                                                (not (search ":USDJPY" (princ-to-string c)))))))
    (check-error "an FX rate the market lacks" (market-component m '(:fx :gbpusd)) missing-market-data)
    (check-that "the market prints the outside clauses after the built-in ones"
                (let ((text (princ-to-string m)))
                  (and (search "(underlying :spx (spot 5000.0d0) (currency :usd) (vol 0.2d0) (beta 1.1d0))" text)
                       (search "(fx :eurusd 1.08d0) (fx :usdjpy 150)" text)
                       (< (search "(curve :usd" text) (search "(fx :eurusd" text) (search "(hazard :acme" text)))))
    (check-that "a market with outside kinds round-trips through its clauses"
                (round-trip-p 'market m :test #'construction-equal-p))
    (let* ((text (with-output-to-string (s) (write-snapshot m s)))
           (back (with-input-from-string (s text) (read-snapshot s))))
      (check-that "a snapshot of outside kinds reads back to an equal market"
                  (construction-equal-p m back))
      (check-that "the snapshot calls the hazard curve's registered constructor"
                  (search "MAKE-HAZARD-CURVE" text)))
    (check-that "a value whose constructor no kind registered is refused at write time, naming the kind"
                (a:when-let ((s (report-of
                                 (lambda ()
                                   (write-snapshot
                                    (make-market (valued-on *today*) (curve :usd (rate 0.04d0))
                                                 (recovery :acme (fincl-outside-kinds:make-recovery :rate 0.4d0)))
                                    (make-broadcast-stream))))))
                  (and (search ":RECOVERY" s) (search ":SNAPSHOT-CONSTRUCTORS" s))))
    (check-that "an FX rate has no view to simulate on: the report lists the kinds that have one"
                (a:when-let ((s (report-of (lambda () (fincl::target-view :fx m :eurusd)))))
                  (let ((kinds (subseq s (or (search "stepped on:" s) (length s)))))
                    (and (search ":CURVE" kinds) (not (search ":FX" kinds)))))))
  ;; The keyword shape takes outside kinds as :COMPONENTS.
  (let ((m (fincl::make-market-from-specs :valuation-date *today* :curves '((:usd :rate 0.04d0))
                                          :components '((:fx (:eurusd 1.08d0) (:gbpusd 1.27d0))))))
    (check-that ":components holds entries of an outside kind"
                (= 1.27d0 (market-component m '(:fx :gbpusd))))
    (check-that "clauses and :components build equal markets"
                (construction-equal-p m (make-market (valued-on *today*) (curve :usd (rate 0.04d0))
                                                     (fx :eurusd 1.08d0) (fx :gbpusd 1.27d0)))))
  (check-that "an unregistered kind in :components lists the registered kinds"
              (a:when-let ((s (report-of (lambda ()
                                           (fincl::make-market-from-specs :valuation-date *today*
                                                                          :components '((:nope (:a 1))))))))
                (and (search ":FX" s) (search ":SPOT" s))))
  (check-error "a value of the wrong type in :components"
               (fincl::make-market-from-specs :valuation-date *today* :components '((:fx (:eurusd -1))))
               invalid-argument)
  (check-error "a built-in kind in :components is refused"
               (fincl::make-market-from-specs :valuation-date *today* :components '((:spot (:x 1))))
               invalid-argument)
  (check-error "a clause argument of the wrong type"
               (make-market (valued-on *today*) (fx :eurusd "1.08")) fact-type-error)
  (check-error "an outside kind's clause has its arity checked at expansion"
               (macroexpand-1 '(make-market (valued-on *today*) (fx :eurusd))) clause-argument-error)
  ;; Sub-clauses belong to their parent.
  (check-error "a curve's part is not a sub-clause of an underlying"
               (macroexpand-1 '(make-market (valued-on *today*) (underlying :x (spot 1) (rate 0.04d0))))
               clause-argument-error)
  (check-error "an underlying's member is not a part of a curve"
               (macroexpand-1 '(make-market (valued-on *today*) (curve :c (spot 1))))
               clause-argument-error)
  (check-error "the data function checks sub-clauses too"
               (make-market-from-clauses `((:valued-on ,*today*) (:underlying :x (:spot 1) (:fx :a 1))))
               clause-argument-error)
  ;; Redefinition and ownership.
  (check-that "redefining a kind from another package warns, with the clause warning"
              (let ((kinds '()))
                (handler-bind ((warning (lambda (w)
                                          (push (type-of w) kinds)
                                          (muffle-warning w))))
                  (eval-in-tests '(define-market-component :redefinable
                          :type integer :clause (redefinable (key value)))))
                (and (member 'market-component-redefined kinds) (member 'clause-redefined kinds))))
  (check-that "the new definition wins"
              (handler-case
                  (progn (make-market (valued-on *today*) (redefinable :k 1/2)) nil)
                (invalid-input () t)))
  (check-error "a built-in kind is sealed"
               (eval-in-tests '(define-market-component :spot :type real :within (underlying spot (price))))
               invalid-argument)
  (check-error "a clause name another kind owns is refused"
               (eval-in-tests '(define-market-component :thing :type real :clause (rate (key value))))
               invalid-argument)
  (check-error "a market clause that is no kind's is refused"
               (eval-in-tests '(define-market-component :thing :type real :clause (valued-on (key value))))
               invalid-argument)
  (check-error "a fact another language defined with another type is refused"
               (eval-in-tests '(define-market-component :thing :type real :clause (thing (key value)) :fact :expiry))
               invalid-argument)
  ;; Rates vols in the market.
  (let* ((leg (floating-leg (make-schedule #D"2026-09-28" #D"2031-09-29" #T"6M" :calendar +target+)
                            1d6 (euribor #T"6M")))
         (cap (make-cap-floor (cap 0.022d0) (on-leg leg)))
         (m (component-rates-market 0.25d0))
         (by-id (make-engine :black-rates :volatility :eur-cap))
         (literal (make-engine :black-rates :volatility 0.25d0)))
    (check-that "an engine reading the market's rates vol prices as one holding it"
                (= (price cap literal m) (price cap by-id m)))
    (check-that "bumping the market's rates vol moves the price of an engine reading it"
                (= (price cap (make-engine :black-rates :volatility 0.26d0) m)
                   (price cap by-id (bump m '(:rates-vol :eur-cap) '(:absolute 1/100)))))
    (check-that "an engine's own vol is a literal the bump does not reach"
                (= (price cap literal m) (price cap literal (bump m '(:rates-vol :eur-cap) '(:absolute 1/100)))))
    (check-that "an expiry rates vol bumps every vol"
                (equal (mapcar (lambda (v) (+ v 1/100)) '(0.3d0 0.28d0))
                       (fincl::rates-vol-vols
                        (market-component (bump (component-rates-market
                                                 (make-instance 'expiry-rates-vol
                                                                :dates (list #D"2027-03-24" #D"2027-09-24")
                                                                :vols '(0.3d0 0.28d0)))
                                                '(:rates-vol :eur-cap) '(:absolute 1/100))
                                          '(:rates-vol :eur-cap)))))
    (check-error "a rates vol id the market lacks"
                 (price cap (make-engine :black-rates :volatility :usd-cap) m) missing-market-data)
    (check-error "a volatility that is neither an id, a structure nor a real"
                 (make-engine :black-rates :volatility "0.2") invalid-argument)
    (let* ((flat (make-market (valued-on *today*) (curve :usd (rate 0.04d0)) (rates-vol :cap 0.2d0)))
           (back (with-input-from-string (s (with-output-to-string (s) (write-snapshot flat s)))
                   (read-snapshot s))))
      (check-that "a market's rates vol snapshots and reads back"
                  (construction-equal-p flat back)))))
