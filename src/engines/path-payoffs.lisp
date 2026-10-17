;;;; engines/path-payoffs.lisp --- DEFINE-PATH-PAYOFF
;;;;
;;;; A path payoff carries its own path logic: autocallables, cliquets,
;;;; payoffs on latent state. DEFINE-PATH-PAYOFF writes its class and its
;;;; PATH-KERNEL from declarative clauses, as DEFINE-PAYOFF writes a terminal
;;;; payoff's PAYOFF-VALUE and PAYOFF-KERNEL from one formula.

(in-package #:fincl)

(defclass path-payoff (payoff) ()
  (:metaclass component-class)
  (:documentation "A payoff defined by DEFINE-PATH-PAYOFF: its PATH-KERNEL
holds all of its path logic, so it is European, combines only with
PATH-INDEPENDENT, and has no closed form."))

(defmethod check-path-feature ((f path-independent) (p path-payoff) exercise)
  (unless (typep exercise 'european-exercise)
    (signal-invalid-argument 'exercise exercise 'european-exercise
                             (format nil "European exercise: ~(~A~) is a path payoff"
                                     (class-name (class-of p))))))

(defmethod validate-payoff ((p path-payoff) market expiry)
  (declare (ignore expiry))
  (let ((today (valuation-date market)))
    (a:when-let ((past (find-if (lambda (d) (date<= d today)) (payoff-mandatory-dates p))))
      (error 'past-observation :path p :date past :valuation-date today))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *path-payoff-clauses* '(:accumulate :start :on :every :state :final))

  (defun path-payoff-clause-error (name clause reason)
    (signal-invalid-argument 'clause clause 'list
                             (format nil "a DEFINE-PATH-PAYOFF ~(~A~) clause: ~A" name reason)))

  (defun mentions-p (tree name)
    "True if TREE contains a symbol named NAME."
    (cond ((symbolp tree) (and tree (string= (symbol-name tree) name)))
          ((consp tree) (or (mentions-p (car tree) name) (mentions-p (cdr tree) name)))
          (t nil))))

(defmacro define-path-payoff (name superclasses slots (observable) &body clauses)
  "Define the payoff class NAME (a PATH-PAYOFF) and its PATH-KERNEL from
CLAUSES. SLOTS are as in DEFINE-PAYOFF; a slot holding dates has type LIST.
Every clause may use the slots and the variables SUPERCLASSES declare (PHI,
STRIKE) as typed local variables, OBSERVABLE (the observable at the current
grid point, a double-float), and the latent state variables of :STATE.

  (:accumulate (var init) ...)   per-path double-float variables, set to
                                 INIT at the start of each path
  (:state var name)              VAR is the state component NAME
                                 (STATE-INDEX), e.g. (:state v :variance)
  (:start form ...)              at grid index 0 (the valuation date)
  (:on date-slot (index) form ...)
                                 at each date of DATE-SLOT; INDEX is the
                                 date's position in the slot
  (:every form ...)              at every grid index
  (:final form)                  the amount paid at expiry

In :ON clauses, (pay amount) pays AMOUNT on the current date and (pay amount
i) on date I of the clause's slot. Clauses run in the order written; (stop)
in any of them ends the path, which then pays nothing at expiry.

  (define-path-payoff autocallable (payoff)
      ((notional double-float) (initial double-float) (barrier double-float)
       (coupon double-float) (observations list))
    (s)
    (:on observations (i)
      (when (>= s (* barrier initial))
        (pay (+ notional coupon))
        (stop)))
    (:final (min notional (* notional (/ s initial)))))"
  (dolist (super superclasses)
    (when (gethash super *formula-payoffs*)
      (error 'formula-payoff-superclass :payoff name :superclass super)))
  (let* ((documentation (when (stringp (first clauses)) (first clauses)))
         (clauses (if documentation (rest clauses) clauses))
         (own (iter (for (slot type . options) in slots)
                    (collect `(,slot ,(getf options :reader slot) ,type))))
         (variables (append (inherited-payoff-variables superclasses) own))
         (payoff (gensym "PAYOFF")) (process (gensym "PROCESS")) (grid (gensym "GRID"))
         (acc (gensym "ACC")) (j (gensym "J")) (state (gensym "STATE")) (pay (gensym "PAY"))
         (step-block (gensym "STEP"))
         ;; PAY and STOP are matched by name, like PHI, so clauses written in
         ;; any package can use them.
         (pay-macro (intern "PAY")) (stop-macro (intern "STOP"))
         (accumulators (iter (for clause in clauses)
                             (when (eq (first clause) :accumulate) (appending (rest clause)))))
         (states (iter (for clause in clauses)
                       (when (eq (first clause) :state)
                         (unless (= 3 (length clause))
                           (path-payoff-clause-error name clause "(:state var name)"))
                         (collect `(,(second clause) ,(third clause) ,(gensym "INDEX"))))))
         (ons (iter (for clause in clauses)
                    (when (eq (first clause) :on)
                      (destructuring-bind (slot (&optional index) &rest forms) (rest clause)
                        (unless (find slot slots :key #'first)
                          (path-payoff-clause-error name clause
                                                    (format nil "~S is not a slot of ~S" slot name)))
                        (collect `(,slot ,(or index (gensym "I")) ,forms
                                   ,(gensym "MASK") ,(gensym "POSITIONS") ,(gensym "OFFSET")))))))
         (finals (remove :final clauses :key #'first :test-not #'eq))
         (final (second (first finals)))
         (pays (mentions-p clauses "PAY"))
         (requirements `(:path ,@(when states '(:state))
                               ,@(when (or pays (mentions-p clauses "STOP")) '(:cashflows))))
         (date-readers (mapcar (lambda (on)
                                 (getf (cddr (find (first on) slots :key #'first)) :reader (first on)))
                               ons)))
    (dolist (clause clauses)
      (unless (and (consp clause) (member (first clause) *path-payoff-clauses*))
        (path-payoff-clause-error name clause (format nil "one of ~{~S~^, ~}" *path-payoff-clauses*))))
    (dolist (clause clauses)
      (when (and (not (eq (first clause) :on)) (mentions-p clause "PAY"))
        (path-payoff-clause-error name clause "PAY is available only in :ON clauses")))
    (unless (= 1 (length finals))
      (path-payoff-clause-error name (first finals) "exactly one (:final form)"))
    (flet ((bindings ()
             (iter (for (variable reader) in variables)
                   (collect `(,variable (,reader ,payoff)))))
           (declarations ()
             (cons `(ignorable ,@(mapcar #'first variables))
                   (iter (for (variable nil type) in variables)
                         (collect `(type ,type ,variable)))))
           ;; The accumulators, the observable and the state variables.
           (with-path-variables (body)
             `(symbol-macrolet ,(iter (for (var) in accumulators)
                                      (for k from 0)
                                      (collect `(,var (aref ,acc ,k))))
                (let ((,observable (aref ,state 0))
                      ,@(iter (for (var nil index) in states)
                              (collect `(,var (aref ,state ,index)))))
                  (declare (double-float ,observable ,@(mapcar #'first states))
                           (ignorable ,observable ,@(mapcar #'first states)))
                  ,@body))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (gethash ',name *payoff-variables*) ',variables
                 (gethash ',name *formula-payoffs*) t))
         ,@(payoff-class-forms name `(path-payoff ,@superclasses) slots documentation)
         (defmethod payoff-requirements ((,payoff ,name) (f path-independent)) ',requirements)
         (defmethod path-accumulator-size ((,payoff ,name) (f path-independent))
           ,(length accumulators))
         (defmethod payoff-mandatory-dates ((,payoff ,name))
           (append ,@(mapcar (lambda (reader) `(,reader ,payoff)) date-readers)))
         (defmethod path-payment-dates ((,payoff ,name) (f path-independent))
           ,(if pays `(append ,@(mapcar (lambda (reader) `(,reader ,payoff)) date-readers)) ''()))
         (defmethod path-kernel ((,payoff ,name) (f path-independent) ,process ,grid)
           (let (,@(bindings))
             (declare ,@(declarations))
             (let* (,@(iter (for (nil nil index) in states)
                            (for (nil state-name) in states)
                            (collect `(,index (state-index ,process ',state-name))))
                    ,@(iter (for (nil nil nil mask positions offset) in ons)
                            (for reader in date-readers)
                            (for k from 0)
                            ;; Payment index = position among all :ON dates.
                            (collect `(,offset (+ ,@(mapcar (lambda (r) `(length (,r ,payoff)))
                                                            (subseq date-readers 0 k)))))
                            (collect `(,mask (nth-value 0 (grid-date-mask ,grid (,reader ,payoff)))))
                            (collect `(,positions (nth-value 1 (grid-date-mask ,grid (,reader ,payoff)))))))
               (declare (ignorable ,process)
                        ,@(iter (for (nil nil index) in states) (collect `(fixnum ,index)))
                        ,@(iter (for (nil nil nil mask positions offset) in ons)
                                (collect `(type simple-bit-vector ,mask))
                                (collect `(type (simple-array fixnum (*)) ,positions))
                                (collect `(fixnum ,offset))))
               (values
                (lambda (,acc)
                  (declare (type (simple-array double-float (*)) ,acc) (ignorable ,acc))
                  ,@(iter (for (nil init) in accumulators)
                          (for k from 0)
                          (collect `(setf (aref ,acc ,k) (float ,init 1d0))))
                  nil)
                (lambda (,acc ,j ,state ,pay)
                  (declare (type (simple-array double-float (*)) ,acc ,state) (fixnum ,j)
                           (type function ,pay) (ignorable ,acc ,pay)
                           (optimize (speed 3) (safety 0)))
                  (block ,step-block
                    (macrolet ((,stop-macro () '(return-from ,step-block nil))
                               (,pay-macro (&rest arguments)
                                 (path-payoff-clause-error ',name (cons 'pay arguments)
                                                           "PAY is available only in :ON clauses")))
                      ,(with-path-variables
                        (iter (with pending = ons)
                              (for clause in clauses)
                              (case (first clause)
                                (:start (collect `(when (zerop ,j) ,@(rest clause))))
                                (:every (collect `(progn ,@(rest clause))))
                                (:on
                                 (destructuring-bind (slot index forms mask positions offset)
                                     (pop pending)
                                   (declare (ignore slot))
                                   (collect
                                       `(when (= 1 (sbit ,mask ,j))
                                          (let ((,index (aref ,positions ,j)))
                                            (declare (fixnum ,index) (ignorable ,index))
                                            (macrolet ((,pay-macro (amount &optional (date ',index))
                                                         `(funcall ,',pay (float ,amount 1d0)
                                                                   (+ ,',offset ,date))))
                                              ,@forms)))))))))
                      t)))
                (lambda (,acc ,state)
                  (declare (type (simple-array double-float (*)) ,acc ,state) (ignorable ,acc)
                           (optimize (speed 3) (safety 0)))
                  ,(with-path-variables `((float ,final 1d0))))))))
         ',name))))

;;; Path payoffs carry their own path logic.
(defmethod price-mc ((x european-exercise) (p path-payoff) (f observed-path)
                     (e monte-carlo-engine) market)
  (error 'unsupported-combination :exercise x :payoff p :path f :process (process e)
                                  :engine e :market market))

(macrolet ((european-only (exercise)
             `(defmethod price-mc ((x ,exercise) (p path-payoff) (f path-independent)
                                   (e monte-carlo-engine) market)
                (error 'unsupported-combination :exercise x :payoff p :path f :process (process e)
                                                :engine e :market market))))
  (european-only american-exercise)
  (european-only bermudan-exercise))
