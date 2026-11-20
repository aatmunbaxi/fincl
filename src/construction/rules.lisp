;;;; construction/rules.lisp --- facts derived from other facts
;;;;
;;;; A rule computes a fact that no clause gave, on demand, from other facts:
;;;; Bermudan exercise dates from a tenor, a start and the expiry; an expiry
;;;; from the last exercise date. Rules run lazily, only when the fact is read
;;;; and absent, and the result is recorded with the rule as its provenance.
;;;;
;;;; Rules may be mutually recursive (dates from expiry, expiry from dates):
;;;; a fact being derived is treated as absent if a derivation reaches it
;;;; again. If that leaves the wanted fact underivable, REQUIRE-FACT reports
;;;; the cycle (FACT-RULE-CYCLE) with the clauses that would break it.

(in-package #:fincl)

(defstruct (fact-rule (:constructor make-fact-rule (fact required optional function)))
  (fact nil :read-only t)
  (required '() :read-only t)                ; facts
  (optional '() :read-only t)                ; (fact default-thunk)
  (function nil :read-only t))

(defmacro define-fact-rule (language fact inputs &body body)
  "Define how LANGUAGE derives FACT when no clause gave it. INPUTS is a list
of (variable fact) pairs, then optionally &OPTIONAL and (variable fact
default) triples; BODY computes the value from them. Required inputs are
read in order, and the first missing one leaves the fact absent, so list the
inputs least likely to be derivable first.

  (define-fact-rule option :expiry ((dates :exercise-dates))
    (a:lastcar dates))"
  (let* ((split (position '&optional inputs))
         (required (subseq inputs 0 split))
         (optional (and split (subseq inputs (1+ split)))))
    `(setf (gethash ,fact (construction-language-rules (ensure-language ',language)))
           (make-fact-rule ,fact ',(mapcar #'second required)
                           (list ,@(mapcar (lambda (o)
                                             `(list ,(second o) (lambda () ,(third o))))
                                           optional))
                           (lambda (,@(mapcar #'first required) ,@(mapcar #'first optional))
                             ,@body)))))

(defvar *deriving* '() "Facts whose rules are running, innermost first.")

(defun find-fact-rule (language fact)
  "The rule of LANGUAGE for FACT: its own, else one it shares, looked up now in
the language it shares it from; NIL if there is none."
  (or (gethash fact (construction-language-rules language))
      (iter (for (source facts) in (construction-language-shared-rules language))
            (when (member fact facts)
              (return (a:when-let ((source-language (gethash source *construction-languages*)))
                        (find-fact-rule source-language fact)))))))

(defun derive-fact (fact)
  "Derive FACT by its rule in the current language and scope. Returns
(values value t), or (values nil nil missing-inputs)."
  (let ((rule (and *language* (find-fact-rule *language* fact))))
    (cond ((null rule) (values nil nil '()))
          ((member fact *deriving*)
           (setf *cycles* `(,@*cycles* ,@(subseq *deriving* 0 (1+ (position fact *deriving*)))))
           (values nil nil '()))
          (t
           (let ((*deriving* `(,fact ,@*deriving*))
                 (missing '())
                 (arguments '()))
             ;; Stop at the first missing input: later inputs may only be
             ;; derivable through this very rule, which would report a
             ;; cycle where the honest answer is the first missing input.
             (dolist (input (fact-rule-required rule))
               (multiple-value-bind (value found) (fact input)
                 (if found
                     (setf arguments `(,@arguments ,value))
                     (progn (setf missing `(,input)) (return)))))
             (if missing
                 (values nil nil missing)
                 (let* ((optional (iter (for (input default) in (fact-rule-optional rule))
                                        (collect (multiple-value-bind (value found) (fact input)
                                                   (if found value (funcall default))))))
                        (provenance (make-provenance `(:rule ,fact)))
                        (value (check-fact-value fact (apply (fact-rule-function rule)
                                                             `(,@arguments ,@optional))
                                                 provenance)))
                   (record-fact fact value :derived provenance)
                   (setf (fact-entry-read (find-fact-entry fact)) t)
                   (values value t))))))))
