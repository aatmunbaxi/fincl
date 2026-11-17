;;;; processes/measure.lisp --- physical-measure processes from risk-neutral ones
;;;;
;;;; Every process is specified under the risk-neutral measure Q. By
;;;; Girsanov's theorem a state X^i driven by a Brownian motion W^i with
;;;; diffusion coefficient sigma_i has, under the physical measure P,
;;;;
;;;;   dW^Q = dW^P + lambda_i dt,   so   mu^P_i = mu^Q_i + sigma_i lambda_i,
;;;;
;;;; lambda_i the market price of risk of W^i. Diffusion coefficients and
;;;; correlations are unchanged; jump parts change through their intensity
;;;; and size law. CHANGE-MEASURE builds a PHYSICAL-PROCESS from a Q process
;;;; and premia; it steps under P (MEASURE-CHANGE-DYNAMICS gives a stepping
;;;; process with P parameters and a drift change of the observable, which
;;;; OBSERVABLE-DRIFT-STEPPER adds after each step) and answers every
;;;; valuation question with the Q process. No engine prices with it.

(in-package #:fincl)

;;; --------------------------------------------------------------------
;;; Conditions
;;; --------------------------------------------------------------------

(define-condition invalid-measure-change (invalid-input)
  ((process :initarg :process :reader measure-change-process)
   (reason :initarg :reason :reader measure-change-reason))
  (:report (lambda (c s)
             (format s "Cannot change the measure of ~A: ~A."
                     (measure-change-process c) (measure-change-reason c))))
  (:documentation "Arguments to CHANGE-MEASURE that do not describe an
equivalent physical measure: an unknown or repeated state, a state without
a Brownian driver, a premium form the process cannot honour, a parameter
fixed by the change of measure, or a process already physical."))

(define-condition risk-neutral-process-required (pricing-error)
  ((process :initarg :process :reader required-process)
   (operation :initarg :operation :reader required-operation))
  (:report (lambda (c s)
             (format s "~A needs a risk-neutral process, but was given ~A. Prices are ~
expectations under the risk-neutral measure; use (risk-neutral-process p) for the ~
process the physical one was built from."
                     (required-operation c) (required-process c))))
  (:documentation "A physical process given where prices are computed: an
engine, automatic selection or calibration."))

(defun require-risk-neutral (process operation)
  "Signal RISK-NEUTRAL-PROCESS-REQUIRED if PROCESS is a PHYSICAL-PROCESS."
  (when (typep process 'physical-process)
    (error 'risk-neutral-process-required :process process :operation operation)))

;;; --------------------------------------------------------------------
;;; State coordinates and diffusion coefficients
;;; --------------------------------------------------------------------

(defgeneric state-coordinate (process name)
  (:documentation "The coordinate in which a drift change of PROCESS's state
NAME is added: :LOG (X <- X exp(int delta dt)), :LEVEL (X <- X + int delta
dt), or NIL when the state has no Brownian driver of its own and takes no
price of risk (the default).")
  (:method ((p stochastic-process) name)
    (declare (ignore name))
    nil))

(defgeneric state-diffusion (process market name times)
  (:documentation "The diffusion coefficient sigma of PROCESS's state NAME in
its STATE-COORDINATE, per unit market time: (values diffusion sigmas).
DIFFUSION is a closure (lambda (j time state)) giving sigma at TIME in grid
interval J of TIMES on a path whose state is STATE; SIGMAS is a
DOUBLE-VECTOR of sigma per interval when sigma does not depend on the state,
else NIL. PROCESS is resolved; methods hoist per-interval terms."))

(defgeneric premium-form-problem (process name kind spec)
  (:documentation "NIL if PROCESS can honour the premium SPEC of KIND
(:PRICE-OF-RISK or :DRIFT-CHANGE) on its state NAME, else a string naming
the forms it accepts there. The default accepts every form.")
  (:method ((p stochastic-process) name kind spec)
    (declare (ignore name kind spec))
    nil))

(defun constant-diffusion (sigmas)
  "STATE-DIFFUSION's values for the per-interval SIGMAS."
  (declare (type double-vector sigmas))
  (values (lambda (j time state)
            (declare (ignore time state) (fixnum j))
            (aref sigmas j))
          sigmas))

(defun interval-sigmas (variances dt)
  "sqrt(VARIANCES[j] / DT[j]), 0 on an empty interval."
  (declare (type double-vector variances dt))
  (map 'double-vector (lambda (v h) (if (plusp h) (sqrt (/ (max v 0d0) h)) 0d0)) variances dt))

;;; --------------------------------------------------------------------
;;; Premia
;;; --------------------------------------------------------------------

(defun price-of-risk-spec-p (spec)
  (or (realp spec) (functionp spec)
      (and (consp spec) (eq (first spec) :scaled) (consp (rest spec))
           (realp (second spec)) (null (cddr spec)))))

(defun normalize-spec (spec)
  "SPEC with its reals as double-floats."
  (cond ((realp spec) (float spec 1d0))
        ((consp spec) `(:scaled ,(float (second spec) 1d0)))
        (t spec)))

(defun normalize-premia (process prices-of-risk drift-changes)
  "((name kind spec) ...) for the premia, KIND :PRICE-OF-RISK or
:DRIFT-CHANGE, after checking every name and form."
  (let ((names (process-state-names process))
        (seen '()))
    (flet ((refuse (format &rest args)
             (error 'invalid-measure-change :process process
                                            :reason (apply #'format nil format args))))
      (iter (for (kind list valid accepted)
                 in '((:price-of-risk prices-of-risk price-of-risk-spec-p
                       "a real lambda, (:scaled c) or a function (lambda (time state)) returning lambda")
                      (:drift-change drift-changes nil
                       "a real drift change or a function (lambda (time state)) returning one")))
            (for given = (if (eq list 'prices-of-risk) prices-of-risk drift-changes))
            (unless (listp given)
              (refuse "~(~A~) must be a list ((state-name spec) ...), not ~S" list given))
            (appending
             (iter (for entry in given)
                   (unless (and (consp entry) (consp (rest entry)) (null (cddr entry)))
                     (refuse "~(~A~) entries are (state-name spec), not ~S" list entry))
                   (for (name spec) = entry)
                   (unless (member name names)
                     (refuse "~S is not a state of ~(~A~); its states are ~{~S~^, ~}"
                             name (class-name (class-of process)) names))
                   (when (member name seen)
                     (refuse "the state ~S is given a premium twice; give it once, either as a price ~
of risk or as a drift change" name))
                   (push name seen)
                   (unless (state-coordinate process name)
                     (refuse "the state ~S has no Brownian driver of its own in ~(~A~), so it ~
takes no price of risk~@[; give its physical law through :parameters (~{~(:~A~)~^, ~})~]"
                             name (class-name (class-of process))
                             (measure-dependent-parameters process)))
                   (unless (if valid (funcall valid spec) (or (realp spec) (functionp spec)))
                     (refuse "the ~(~A~) of ~S is ~S, but must be ~A" kind name spec accepted))
                   (a:when-let ((accepted (premium-form-problem process name kind spec)))
                     (refuse "~(~A~) does not take ~:[a function~;~:*~S~] as the ~(~A~) of ~S; it ~
accepts ~A" (class-name (class-of process)) (unless (functionp spec) spec) kind name accepted))
                   (collect `(,name ,kind ,(normalize-spec spec)))))))))

(defun normalize-measure-parameters (process parameters)
  "PARAMETERS, a plist of physical values, with keys as the slot names and
values as double-floats, after checking that each is a measure-dependent
parameter of PROCESS within its bounds."
  (let ((dependent (measure-dependent-parameters process)))
    (flet ((refuse (format &rest args)
             (error 'invalid-measure-change :process process
                                            :reason (apply #'format nil format args))))
      (unless (and (listp parameters) (evenp (length parameters)))
        (refuse ":parameters must be a plist (:name value ...), not ~S" parameters))
      (iter (with seen = '())
            (for (key value) in-plist parameters)
            (for name = (find (string key) (process-parameters process) :key #'string :test #'string=))
            (cond ((null name)
                   (refuse "~S is not a parameter of ~(~A~); its measure-dependent parameters are ~
~:[none~;~:*~{~(:~A~)~^, ~}~]" key (class-name (class-of process)) dependent))
                  ((member name seen)
                   (refuse "the parameter ~(:~A~) is given twice" name))
                  ((not (member name dependent))
                   (refuse "~(:~A~) is fixed by the change of measure (diffusion coefficients, ~
correlations and initial states are the same under both measures); the measure-dependent ~
parameters of ~(~A~) are ~:[none~;~:*~{~(:~A~)~^, ~}~]"
                           name (class-name (class-of process)) dependent))
                  ((not (realp value))
                   (refuse "the physical ~(~A~) is ~S, not a real number" name value))
                  ((not (within-bounds-p value (slot-bounds* (find-managed-slot process name))))
                   (refuse "the physical ~(~A~) = ~S is outside ~A" name value
                           (format-bounds (slot-bounds* (find-managed-slot process name))))))
            (push name seen)
            (appending `(,name ,(float value 1d0)))))))

;;; --------------------------------------------------------------------
;;; The physical process
;;; --------------------------------------------------------------------

(defclass physical-process (stochastic-process)
  ((risk-neutral-process :initarg :risk-neutral-process :reader risk-neutral-process
                         :documentation "The risk-neutral process the physical one was
built from, resolved once the physical process is.")
   (prices-of-risk :initarg :prices-of-risk :initform '() :reader physical-prices-of-risk)
   (drift-changes :initarg :drift-changes :initform '() :reader physical-drift-changes)
   (parameters :initarg :parameters :initform '() :reader physical-parameters)
   (measure-parameters :initform '() :reader physical-measure-parameters
                       :documentation "PARAMETERS as (slot-name double ...), checked.")
   (premia :initform '() :reader physical-premia
           :documentation "((name kind spec) ...), checked and normalized.")
   (stepping-process :initarg :stepping-process :initform nil :reader stepping-process
                     :documentation "After RESOLVE-PROCESS: the process of the risk-neutral
class with physical parameters whose stepper the physical stepper wraps.")
   (observable-drift :initarg :observable-drift :initform nil :reader observable-drift
                     :documentation "After RESOLVE-PROCESS: (kind spec) of the drift change
OBSERVABLE-DRIFT-STEPPER adds to state 0, or NIL."))
  (:documentation "A process under the physical measure P, built by
CHANGE-MEASURE from a risk-neutral process and its prices of risk. It steps
under P wherever a process is stepped (SIMULATE legs) and answers every
valuation question (the date's market, curves, discounting) with
RISK-NEUTRAL-PROCESS. Engines and calibration refuse it."))

(defmethod initialize-instance :after ((p physical-process) &key)
  (let ((q (risk-neutral-process p)))
    (unless (typep q 'stochastic-process)
      (signal-invalid-argument 'process q 'stochastic-process "a risk-neutral stochastic process"))
    (when (typep q 'physical-process)
      (error 'invalid-measure-change
             :process q
             :reason "it is already physical; change the measure of its risk-neutral process, (risk-neutral-process p)"))
    (unless (steppable-p q)
      (error 'invalid-measure-change
             :process q :reason (format nil "~(~A~) has no stepper, so it cannot be simulated under any measure"
                                        (class-name (class-of q)))))
    (setf (slot-value p 'premia)
          (normalize-premia q (physical-prices-of-risk p) (physical-drift-changes p))
          (slot-value p 'measure-parameters)
          (normalize-measure-parameters q (physical-parameters p)))))

(declaim (ftype (function (t &key (:prices-of-risk list) (:drift-changes list) (:parameters list))
                    (values physical-process &optional))
          change-measure))
(defun change-measure (process &key prices-of-risk drift-changes parameters)
  "A PHYSICAL-PROCESS stepping PROCESS's dynamics under the physical measure.
PROCESS is a risk-neutral process with a stepper. Premia name the state they
apply to, from PROCESS-STATE-NAMES; states not named carry none.

  PRICES-OF-RISK  ((state spec) ...), lambda for the Brownian motion driving
                  the state: a real (constant lambda); (:scaled c), lambda =
                  c sigma, a drift change of c sigma^2 (proportional to
                  variance); or a function (lambda (time state)) returning
                  lambda, TIME in market time, STATE the state vector (read
                  only).
  DRIFT-CHANGES   ((state spec) ...), the drift change delta = sigma lambda
                  itself, in the state's coordinate: a real or a function
                  (lambda (time state)). For the observable, delta is the
                  excess continuously compounded growth over the risk-neutral
                  carry: E^P[S(t+h)] = E^Q[S(t+h)] exp(delta h) for constant
                  delta.
  PARAMETERS      a plist of physical values of PROCESS's measure-dependent
                  parameters (jump intensities and size laws).

Latent states take only premia that keep their scheme exact, as physical
parameters of the stepping process: Heston's :VARIANCE (:SCALED c) gives
kappa^P = kappa - c xi^2 and theta^P = kappa theta / kappa^P, a constant
drift change d gives theta^P = (kappa theta + d) / kappa; NGARCH's :SPOT
takes a real lambda per period (Duan's model, gamma^P = gamma - lambda);
Hull-White's :SHORT-RATE takes any form, functions being called with STATE
NIL (a function of time only), as a drift sigma lambda(t) inside its exact
step. Heston variance premia keep the measures equivalent only when the
Feller condition 2 kappa theta > xi^2 holds under both; a violation signals
the warning FELLER-CONDITION-VIOLATED, once per image. Variance gamma and
NIG take physical parameters only, plus the drift that keeps their drift
beside the jumps unchanged.

A drift change of the observable is added after each step of the
risk-neutral scheme, exactly when delta is constant over a step, by the
trapezoid rule on the states at the step's two ends otherwise. Discrete
dividends are applied once, after it. With no premia the result steps as
PROCESS on the same normals. Defaults left to the market (NIL parameters)
are resolved, and the premia converted, by RESOLVE-PROCESS. Bad arguments
signal INVALID-MEASURE-CHANGE.

  (change-measure (make-gbm) :drift-changes '((:spot 0.03d0)))
  (change-measure (make-merton-jump) :prices-of-risk '((:spot 0.2d0))
                                     :parameters '(:intensity 1d0))"
  (make-instance 'physical-process :risk-neutral-process process
                                   :prices-of-risk prices-of-risk
                                   :drift-changes drift-changes
                                   :parameters parameters))

(defmethod parameter-source ((p physical-process)) (risk-neutral-process p))

(defun physical-form (p)
  "(change-measure q-form :prices-of-risk ... ) for P; function specs appear
as the functions."
  `(change-measure ,(process-form (risk-neutral-process p))
                   ,@(a:when-let ((x (physical-prices-of-risk p))) `(:prices-of-risk (quote ,x)))
                   ,@(a:when-let ((x (physical-drift-changes p))) `(:drift-changes (quote ,x)))
                   ,@(a:when-let ((x (physical-parameters p))) `(:parameters (quote ,x)))))

(defun function-premium-state (p)
  "The first state of P whose premium is a function, or NIL."
  (first (find-if (lambda (premium) (functionp (third premium))) (physical-premia p))))

(defmethod print-object ((p physical-process) stream)
  (let ((*package* (find-package '#:fincl)) (*print-pretty* nil))
    (if (and *print-readably* (null (function-premium-state p)))
        (prin1 (physical-form p) stream)
        (let ((*print-case* :downcase))
          (format stream "#<PHYSICAL-PROCESS ~S>" (physical-form p))))))

(defun premium-on (premia name)
  "(values kind spec) of the premium on state NAME in PREMIA's :PREMIA, or NIL."
  (a:when-let ((premium (find name (getf premia :premia) :key #'first)))
    (values (second premium) (third premium))))

(defun premia-without (premia &rest names)
  "PREMIA with the premia on NAMES removed and its parameters kept."
  `(:premia ,(remove-if (lambda (premium) (member (first premium) names)) (getf premia :premia))
    :parameters ,(getf premia :parameters)))

(defun premia-with-parameters (premia &rest parameters)
  "PREMIA with the plist PARAMETERS (slot name value ...) added before its
parameters."
  `(:premia ,(getf premia :premia)
    :parameters (,@parameters ,@(getf premia :parameters))))

(defun refuse-measure-change (process format &rest args)
  (error 'invalid-measure-change :process process :reason (apply #'format nil format args)))

;;; --------------------------------------------------------------------
;;; Resolution: the stepping dynamics
;;; --------------------------------------------------------------------

(defun process-with-parameters (process parameters)
  "A new process of PROCESS's class with the slots in the plist PARAMETERS
(slot name -> value) replaced and every other slot copied; PROCESS itself
when PARAMETERS is empty. The new process is validated."
  (if (null parameters)
      process
      (let ((initargs '()))
        (dolist (slot (c2mop:class-slots (class-of process)))
          (let ((name (c2mop:slot-definition-name slot))
                (initarg (first (c2mop:slot-definition-initargs slot))))
            (when (and initarg (slot-boundp process name))
              (push initarg initargs)
              (push (getf parameters name (slot-value process name)) initargs))))
        (apply #'make-instance (class-of process) (nreverse initargs)))))

(defgeneric measure-change-dynamics (q-process premia market horizon)
  (:documentation "The physical dynamics of the resolved Q-PROCESS: (values
stepping-process observable-drift). PREMIA is (:premia ((name kind spec)
...) :parameters plist), checked by CHANGE-MEASURE. STEPPING-PROCESS is a
process of Q-PROCESS's class with the physical parameters; OBSERVABLE-DRIFT
is (kind spec) for state 0, added by OBSERVABLE-DRIFT-STEPPER, or NIL: a
premium as given, or (:INCREMENT f), f (lambda (j state)) the drift change's
integral over interval J from the state at its start. The
default takes the parameters as given, passes a premium on state 0 through
and refuses one on a latent state.")
  (:method ((q stochastic-process) premia market horizon)
    (declare (ignore market horizon))
    (let ((observable (first (process-state-names q)))
          (drift nil))
      (dolist (premium (getf premia :premia))
        (destructuring-bind (name kind spec) premium
          (if (eq name observable)
              (setf drift `(,kind ,spec))
              (error 'invalid-measure-change
                     :process q
                     :reason (format nil "~(~A~) takes no premium on its latent state ~S~@[; give its ~
physical law through :parameters (~{~(:~A~)~^, ~})~]"
                                     (class-name (class-of q)) name (measure-dependent-parameters q))))))
      (values (process-with-parameters q (getf premia :parameters)) drift))))

(defun physical-resolved-p (p)
  (not (null (stepping-process p))))

(defmethod resolve-process ((p physical-process) market horizon)
  (if (physical-resolved-p p)
      p
      (let ((q (resolve-process (risk-neutral-process p) market horizon)))
        (multiple-value-bind (stepping drift)
            (measure-change-dynamics q `(:premia ,(physical-premia p) :parameters ,(physical-measure-parameters p))
                                     market horizon)
          (make-instance 'physical-process :risk-neutral-process q
                                           :prices-of-risk (physical-prices-of-risk p)
                                           :drift-changes (physical-drift-changes p)
                                           :parameters (physical-parameters p)
                                           :stepping-process stepping
                                           :observable-drift drift)))))

;;; --------------------------------------------------------------------
;;; Stepping under P
;;; --------------------------------------------------------------------

(defun drift-rate-function (kind spec diffusion)
  "(lambda (j time state)) -> delta, the drift change at TIME in interval J,
for the premium (KIND SPEC) and the STATE-DIFFUSION closure DIFFUSION."
  (flet ((user (f) (lambda (time state) (float (funcall (the function f) time state) 1d0))))
    (ecase kind
      (:drift-change
       (if (functionp spec)
           (let ((f (user spec))) (lambda (j time state) (declare (ignore j)) (funcall f time state)))
           (lambda (j time state) (declare (ignore j time state)) spec)))
      (:price-of-risk
       (cond ((realp spec)
              (lambda (j time state) (* spec (the double-float (funcall diffusion j time state)))))
             ((functionp spec)
              (let ((f (user spec)))
                (lambda (j time state)
                  (* (the double-float (funcall diffusion j time state))
                     (the double-float (funcall f time state))))))
             (t
              (let ((c (second spec)))
                (lambda (j time state)
                  (let ((sigma (funcall diffusion j time state)))
                    (declare (double-float sigma))
                    (* c sigma sigma))))))))))

(defun drift-integrals (kind spec sigmas dt)
  "The integral of the drift change over each interval, a DOUBLE-VECTOR, when
it does not depend on the state; else NIL. SIGMAS is STATE-DIFFUSION's
per-interval vector or NIL."
  (declare (type double-vector dt))
  (cond ((functionp spec) nil)
        ((eq kind :drift-change) (map 'double-vector (lambda (h) (* spec h)) dt))
        ((null sigmas) nil)
        ((realp spec) (map 'double-vector (lambda (s h) (* spec s h)) sigmas dt))
        (t (let ((c (second spec)))
             (map 'double-vector (lambda (s h) (* c s s h)) sigmas dt)))))

(defun observable-drift-stepper (stepper process drift market times)
  "STEPPER, a MAKE-STEPPER closure of PROCESS over TIMES, followed on each
interval by the drift change DRIFT, (kind spec), of state 0 in its
STATE-COORDINATE (:LOG when it has none): exact when the drift is constant over the interval, by the
trapezoid rule on the states at its two ends otherwise. STEPPER itself when
DRIFT is NIL."
  (declare (type double-vector times) (function stepper))
  (if (null drift)
      stepper
      (destructuring-bind (kind spec) drift
        (when (eq kind :increment)
          (return-from observable-drift-stepper (increment-drift-stepper stepper process spec)))
        (let* ((name (first (process-state-names process)))
               ;; NIL: a drift the change of measure itself sets on a
               ;; pure-jump observable, a growth rate.
               (coordinate (or (state-coordinate process name) :log))
               (dt (interval-lengths times)))
          (declare (type double-vector dt))
          (multiple-value-bind (diffusion sigmas)
              (if (and (eq kind :drift-change) (realp spec))
                  (values nil nil)
                  (state-diffusion process market name times))
            (let ((integrals (drift-integrals kind spec sigmas dt)))
              (if integrals
                  (let ((growth (if (eq coordinate :log) (map 'double-vector #'exp integrals) integrals))
                        (log-p (eq coordinate :log)))
                    (declare (type double-vector growth))
                    (lambda (state normals j)
                      (declare (type double-vector state) (fixnum j))
                      (funcall stepper state normals j)
                      (if log-p
                          (setf (aref state 0) (* (aref state 0) (aref growth j)))
                          (incf (aref state 0) (aref growth j)))
                      nil))
                  (let ((rate (drift-rate-function kind spec diffusion))
                        (log-p (eq coordinate :log)))
                    (declare (function rate))
                    (lambda (state normals j)
                      (declare (type double-vector state) (fixnum j))
                      (let ((d0 (funcall rate j (aref times j) state)))
                        (declare (double-float d0))
                        (funcall stepper state normals j)
                        (let* ((d1 (funcall rate j (aref times (1+ j)) state))
                               (g (* 0.5d0 (aref dt j) (+ d0 (the double-float d1)))))
                          (declare (double-float g))
                          (if log-p
                              (setf (aref state 0) (* (aref state 0) (exp g)))
                              (incf (aref state 0) g))))
                      nil)))))))))

(defun increment-drift-stepper (stepper process increment)
  "STEPPER followed on each interval by the drift change (INCREMENT j state),
its integral over interval J from the state at the interval's start, added
in state 0's STATE-COORDINATE."
  (declare (function stepper increment))
  (let ((log-p (member (state-coordinate process (first (process-state-names process))) '(:log nil))))
    (lambda (state normals j)
      (declare (type double-vector state) (fixnum j))
      (let ((g (funcall increment j state)))
        (declare (double-float g))
        (funcall stepper state normals j)
        (if log-p
            (setf (aref state 0) (* (aref state 0) (exp g)))
            (incf (aref state 0) g)))
      nil)))

(defmethod make-stepper ((p physical-process) market times)
  (declare (type double-vector times))
  (let* ((p (resolve-process p market (aref times (1- (length times)))))
         (stepping (stepping-process p)))
    (observable-drift-stepper (let ((*apply-discrete-dividends* nil))
                                (make-stepper stepping market times))
                              stepping (observable-drift p) market times)))

;;; --------------------------------------------------------------------
;;; The rest of the protocol: the risk-neutral process's answers
;;; --------------------------------------------------------------------

(macrolet ((delegate (name lambda-list)
             (let ((args (remove-if (lambda (x) (member x lambda-list-keywords))
                                    (rest lambda-list))))
               `(defmethod ,name ((p physical-process) ,@(rest lambda-list))
                  (,name (risk-neutral-process p) ,@args)))))
  (delegate process-state-size (p))
  (delegate process-state-names (p))
  (delegate process-factors (p))
  (delegate process-regressors (p))
  (delegate spot-loadings (p))
  (delegate steppable-p (p))
  (delegate initial-state (p market))
  (delegate process-targets (p))
  (delegate dividend-model (p))
  (delegate state-discount-bond (p state t0 t1)))

(defmethod structure-at-state ((p physical-process) target initial market date state
                               &rest keys &key &allow-other-keys)
  (apply #'structure-at-state (risk-neutral-process p) target initial market date state keys))

(defmethod simulatable-p ((p physical-process)) nil)

(defmethod characteristic-function ((p physical-process) market horizon)
  (declare (ignore market horizon))
  nil)

(defmethod increment-characteristic-function ((p physical-process) market t0 t1)
  (declare (ignore market t0 t1))
  nil)

(defmethod log-cumulants ((p physical-process) market horizon)
  (declare (ignore market horizon))
  nil)

(defmethod terminal-sampler ((p physical-process) market time)
  (declare (ignore market time))
  nil)

(defmethod bridge-variance ((p physical-process) market times)
  (declare (ignore market times))
  nil)

(defmethod path-discount-kernel ((p physical-process) market grid)
  (declare (ignore market grid))
  nil)
