;;;; construction/conventions.lisp --- named clause bundles
;;;;
;;;; A convention names the clauses that market practice implies, e.g. every
;;;; term of a standard USD SOFR swap. Each language has a (convention name)
;;;; clause that runs a bundle at convention priority, so the user's own
;;;; clauses override it whatever their order, while two conventions setting
;;;; the same fact conflict.

(in-package #:fincl)

(defmacro define-convention (language name &body clauses)
  "Define the convention NAME (a keyword) of LANGUAGE as CLAUSES, written as
in the language's macro. Clause heads are checked now; arguments are
evaluated when the convention is defined.

  (define-convention swap :usd-sofr-ois
    (calendar +us-settlement+)
    (leg :pay (frequency :annual))
    (leg :receive (floating :sofr)))"
  `(setf (gethash ,(clause-key name)
                  (construction-language-conventions (ensure-language ',language)))
         (list ,@(mapcar (lambda (form) (expand-clause-form language form)) clauses))))

(defun apply-convention (name)
  "Run the clauses of convention NAME of the current language at convention
priority."
  (let* ((conventions (construction-language-conventions *language*))
         (clauses (or (gethash (clause-key name) conventions)
                      (error 'unknown-convention
                             :name name :language (construction-language-name *language*)
                             :known (mapcar #'a:make-keyword (sorted-keys conventions))))))
    (let ((*priority* :convention))
      (apply-clauses clauses))))

(defun register-convention-clause (language)
  "Give LANGUAGE its (convention name) clause."
  (register-clause language
                   (make-clause-definition "CONVENTION" '(name) 1 nil '()
                                           "Apply the named convention bundle."
                                           #'apply-convention)))
