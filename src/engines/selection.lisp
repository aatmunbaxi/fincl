;;;; engines/selection.lisp --- engine designators and automatic selection
;;;;
;;;; Wherever an engine is expected (PRICE, GREEKS, IMPLIED-VOLATILITY), a
;;;; designator may stand in for it:
;;;;
;;;;   an engine                        itself
;;;;   :cos, :binomial, ...             the engine class with that designator,
;;;;                                    default settings (one shared instance)
;;;;   (:cos :n-terms 512)              settings as data, values not evaluated
;;;;   a process, e.g. (make-heston)    price under that model with the first
;;;;                                    engine that can (ENGINE-CANDIDATES)
;;;;   :auto                            the same, for GBM on the market's vols
;;;;
;;;; In code, settings go through MAKE-ENGINE, whose arguments are evaluated:
;;;; (make-engine :cos :process (make-heston) :n-terms 512).

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Making engines from designators
;;; --------------------------------------------------------------------

(defun engine-designators ()
  "The engine designator keywords, sorted."
  (sorted-keys *engine-designators*))

(defun designated-class (key)
  (let ((name (gethash key *engine-designators*)))
    (unless name
      (signal-invalid-argument 'engine key 'keyword
                               (format nil "an engine designator: ~{~S~^, ~}" (engine-designators))))
    (find-class name)))

(defun class-initargs (class)
  (iter (for slot in (c2mop:class-slots (c2mop:ensure-finalized class)))
        (appending (copy-list (c2mop:slot-definition-initargs slot)))))

(defun class-models-process-p (class process)
  (some (lambda (model) (typep process model)) (engine-class-models class)))

(defun make-engine (key &rest settings)
  "The engine designated by KEY (:black-scholes, :baw, :binomial, :cos, :mc,
:merton-series, :cev, or any class's (:designator ...)) with SETTINGS, its
initargs, evaluated. :PROCESS goes to engines that take one; an engine whose
model is fixed (Black-Scholes, BAW) accepts a process only of its model.

  (make-engine :binomial :n-steps 4000)
  (make-engine :cos :process (make-heston :xi 0.5d0) :n-terms 512)"
  (check-input key keyword "an engine designator keyword")
  (let* ((class (designated-class key))
         (initargs (class-initargs class))
         (process (getf settings :process)))
    (when (and process (not (typep process 'stochastic-process)))
      (signal-invalid-argument :process process 'stochastic-process
                               "a process object: evaluate its constructor, (make-heston ...), not its form"))
    (when (and process (not (member :process initargs)))
      (unless (class-models-process-p class process)
        (signal-invalid-argument :process process 'stochastic-process
                                 (format nil "a process ~S models (~{~(~A~)~^, ~})"
                                         (class-name class) (engine-class-models class))))
      (setf settings (a:remove-from-plist settings :process)))
    (iter (for (initarg nil) in-plist settings)
          (unless (member initarg initargs)
            (signal-invalid-argument 'settings initarg 'keyword
                                     (format nil "a setting of ~(~A~): ~{~S~^, ~}" (class-name class)
                                             (remove-duplicates initargs)))))
    (apply #'make-instance class settings)))

(defvar *shared-engines* (make-hash-table :test 'eq :synchronized t)
  "Designator keyword -> the engine with default settings. Engines are
immutable, so one instance serves every caller.")

(defun ensure-engine (designator &optional process)
  "The engine DESIGNATOR names, with PROCESS for engines that take one: an
engine itself; a keyword (one shared instance per class without PROCESS); or
a list (keyword initarg value ...) as data, whose values are not evaluated.

  (ensure-engine :binomial) => the shared default binomial engine"
  (etypecase designator
    (engine designator)
    (keyword
     (when (member designator '(:auto))
       (signal-invalid-argument 'designator designator 'keyword
                                "an engine keyword: :AUTO selects an engine through PRICE"))
     (if process
         (make-engine designator :process process)
         (or (gethash designator *shared-engines*)
             (setf (gethash designator *shared-engines*) (make-engine designator)))))
    (cons
     (destructuring-bind (key &rest settings) designator
       (apply #'make-engine key
              (if (and process (not (getf settings :process)))
                  `(,@settings :process ,process)
                  settings))))))

;;; --------------------------------------------------------------------
;;; Engines and processes as forms
;;; --------------------------------------------------------------------

(defun non-default-initargs (object slots)
  "(initarg value ...) for the SLOTS of OBJECT whose value differs from the
slot's initform."
  (iter (for slot in slots)
        (for initarg = (first (c2mop:slot-definition-initargs slot)))
        (for name = (c2mop:slot-definition-name slot))
        (for initfunction = (c2mop:slot-definition-initfunction slot))
        (when (and initarg (slot-boundp object name))
          (appending (let ((value (slot-value object name))
                           (default (and initfunction (funcall initfunction))))
                       (unless (and initfunction
                                    (if (typep value 'stochastic-process)
                                        (and (typep default 'stochastic-process)
                                             (equal (process-form value) (process-form default)))
                                        (equalp value default)))
                         `(,initarg ,value)))))))

(defun process-form (process)
  "A form that evaluates to a process equal to PROCESS: its constructor with
the settings that differ from the defaults.

  (process-form (make-heston :xi 0.5d0)) => (MAKE-HESTON :XI 0.5d0)"
  (let* ((class (class-of process))
         (constructor (constructor-name (class-name class))))
    `(,@(if (eq (get constructor 'process-constructor) (class-name class))
            `(,constructor)
            `(make-instance ',(class-name class)))
      ,@(non-default-initargs process (managed-slots class)))))

(defun engine-form (engine)
  "A form that evaluates to an engine equal to ENGINE: (make-engine key
setting ...), with the settings that differ from the defaults and a process
as its PROCESS-FORM. NIL for an engine class without a designator."
  (let ((key (engine-class-designator (class-of engine))))
    (when key
      `(make-engine ,key
                    ,@(iter (for (initarg value) in-plist (non-default-initargs
                                                            engine (c2mop:class-slots (class-of engine))))
                            (appending `(,initarg ,(if (typep value 'stochastic-process)
                                                       (process-form value)
                                                       value))))))))

(defmethod print-object ((e engine) stream)
  ;; Readably as the MAKE-ENGINE form, as options print as MAKE-OPTION;
  ;; otherwise #<ENGINE form>.
  (let ((form (engine-form e)))
    (cond ((null form) (call-next-method))
          (*print-readably* (prin1 form stream))
          (t (let ((*package* (find-package '#:fincl)) (*print-case* :downcase))
               (format stream "#<ENGINE ~S>" form))))))

;;; --------------------------------------------------------------------
;;; Automatic selection
;;; --------------------------------------------------------------------

(define-condition engine-fallback (warning)
  ((instrument :initarg :instrument :reader fallback-instrument)
   (process :initarg :process :reader fallback-process)
   (declined :initarg :declined :reader fallback-declined))
  (:report (lambda (c s)
             (format s "No faster engine prices ~A under ~A~@[ (~{~S~^, ~} declined)~]; ~
falling back to Monte Carlo."
                     (fallback-instrument c) (fallback-process c)
                     (mapcar #'engine-designator-key (fallback-declined c)))))
  (:documentation "Automatic selection is about to price by Monte Carlo
because every faster candidate declined. MUFFLE-WARNING silences it;
(with-engines ((:fallback nil)) ...) refuses the fallback instead."))

(define-condition no-suitable-engine (unsupported-combination)
  ((tried :initarg :tried :reader engines-tried))
  (:report (lambda (c s)
             (format s "No engine prices ~A / ~A / ~A under ~A: tried ~:[none~;~:*~{~S~^, ~}~]."
                     (class-name (class-of (error-exercise c)))
                     (class-name (class-of (error-payoff c)))
                     (class-name (class-of (error-path c)))
                     (error-process c)
                     (mapcar #'engine-designator-key (engines-tried c)))))
  (:documentation "Automatic selection found no candidate that prices the
instrument under the process."))

(defun engine-designator-key (designator)
  (etypecase designator
    (keyword designator)
    (cons (first designator))
    (engine (or (engine-class-designator (class-of designator)) (class-name (class-of designator))))))

(defvar *engine-preferences* '()
  "Overrides bound by WITH-ENGINES, innermost first: (selector designator).")

(defvar *engine-fallback-allowed* t
  "Whether automatic selection may fall back to Monte Carlo: T, NIL, or a
designator to fall back to instead of the default Monte Carlo engine.")

(defconstant +fallback-priority+ 90
  "Candidates at or above this priority are slow fallbacks (Monte Carlo).")

(defmacro with-engines (clauses &body body)
  "Run BODY with engine preferences for automatic selection. Each clause is
(selector designator), the designator evaluated; the selector is an exercise
style (:european, :american, :bermudan), a process class name, or (style
class). Matching overrides are tried before the candidates, innermost
binding first. The clause (:fallback nil) forbids falling back to Monte
Carlo; (:fallback designator) falls back to that engine instead (a list
designator gets the process: '(:mc :n-paths 20000)).

  (with-engines ((:american (make-engine :cos :richardson-dates 8))
                 (merton-jump :cos)
                 (:fallback nil))
    (price aput (make-kou-jump) m))"
  (let ((fallback (find :fallback clauses :key #'first))
        (overrides (remove :fallback clauses :key #'first)))
    `(let ((*engine-preferences*
             (append (list ,@(iter (for (selector designator) in overrides)
                                   (collect `(list ',selector ,designator))))
                     *engine-preferences*))
           (*engine-fallback-allowed* ,(if fallback (second fallback) '*engine-fallback-allowed*)))
       ,@body)))

(defun exercise-style (exercise)
  (etypecase exercise
    (american-exercise :american)
    (bermudan-exercise :bermudan)
    (european-exercise :european)))

(defun selector-matches-p (selector exercise process)
  (etypecase selector
    (keyword (eq selector (exercise-style exercise)))
    (symbol (typep process selector))
    (cons (destructuring-bind (style class) selector
            (and (eq style (exercise-style exercise)) (typep process class))))))

(defun engine-choices (option process)
  "(priority designator) pairs to try for OPTION under PROCESS: matching
overrides (priority 0), then ENGINE-CANDIDATES by priority."
  (let ((exercise (option-exercise option)))
    `(,@(iter (for (selector designator) in *engine-preferences*)
              (when (selector-matches-p selector exercise process)
                (collect `(0 ,designator))))
      ,@(-> (iter (for key in (engine-designators))
                  (appending (engine-candidates key exercise (option-payoff option)
                                                (option-path option) process)))
            (stable-sort #'< :key #'first)))))

(defvar *selections* 0
  "Automatic selections made, for tests.")

(defun price-with-model (option process market)
  "Price OPTION under PROCESS with the first engine that can: (values npv
standard-error engine)."
  (incf *selections*)
  (let ((declined '()) (warned nil))
    (iter (for (priority designator) in (engine-choices option process))
          (for fallback = (>= priority +fallback-priority+))
          (unless (and fallback (not *engine-fallback-allowed*))
            (let ((engine (ensure-engine (if (and fallback (not (eq *engine-fallback-allowed* t)))
                                             *engine-fallback-allowed*
                                             designator)
                                         process)))
              (when (and fallback (not warned))
                (setf warned t)
                (warn 'engine-fallback :instrument option :process process
                                       :declined (reverse declined)))
              (handler-case
                  (multiple-value-bind (npv se) (price option engine market)
                    (return-from price-with-model (values npv se engine)))
                ((or unsupported-combination model-domain-error) ()
                  (push designator declined))))))
    (error 'no-suitable-engine
           :exercise (option-exercise option) :payoff (option-payoff option)
           :path (option-path option) :process process :engine nil :market market
           :tried (reverse declined))))

(defmethod price ((o option) (process stochastic-process) market)
  (price-with-model o process market))

(defmethod price ((o option) (designator symbol) market)
  (if (eq designator :auto)
      (price-with-model o (make-gbm) market)
      (price o (ensure-engine designator) market)))

(defmethod price ((o option) (designator cons) market)
  (price o (ensure-engine designator) market))

(defun resolve-designator (instrument designator market)
  "The engine DESIGNATOR stands for when pricing INSTRUMENT in MARKET:
automatic selection runs once, on MARKET."
  (typecase designator
    (engine designator)
    ((or stochastic-process (eql :auto)) (nth-value 2 (price instrument designator market)))
    (t (ensure-engine designator))))
