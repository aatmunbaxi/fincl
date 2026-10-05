;;;; syntax.lisp --- #D"2027-06-15" and #T"3M" literals
;;;;
;;;; The literals live in the named readtable FINCL:SYNTAX; the global
;;;; readtable is never modified. A file that uses them starts with
;;;;
;;;;   (in-package #:fincl)
;;;;   (named-readtables:in-readtable fincl:syntax)
;;;;
;;;; Each literal reads one string and calls PARSE-DATE or PARSE-TENOR, so an
;;;; invalid literal is an error at read time. DATE and TENOR define
;;;; MAKE-LOAD-FORM, so literals survive COMPILE-FILE.

(in-package #:fincl)

;;; The readtable must exist at compile time, so later files in the same
;;; build can IN-READTABLE it.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition invalid-literal (invalid-input reader-error)
    ((subchar :initarg :subchar :reader invalid-literal-subchar)
     (problem :initarg :problem :reader invalid-literal-problem))
    (:report (lambda (c s)
               (format s "Invalid #~C literal: ~A"
                       (invalid-literal-subchar c) (invalid-literal-problem c))))
    (:documentation "A malformed #D or #T literal. Also a READER-ERROR, so the
reader reports the stream position."))

  (defun literal-error (stream subchar problem)
    (error 'invalid-literal :stream stream :subchar subchar :problem problem))

  (defun read-literal-string (stream subchar arg)
    (when arg
      (literal-error stream subchar
                     (format nil "#~D~C takes no numeric argument; write #~C\"...\"."
                             arg subchar subchar)))
    (let ((string (read stream t nil t)))
      (unless (or *read-suppress* (stringp string))
        (literal-error stream subchar
                       (format nil "#~C must be followed by a string, not ~S." subchar string)))
      string))

  (defun parse-literal (stream subchar parser string)
    "PARSER applied to STRING, with its INVALID-INPUT re-signalled as an
INVALID-LITERAL at STREAM's position."
    (handler-case (funcall parser string)
      (invalid-input (c) (literal-error stream subchar (princ-to-string c)))))

  (defun read-date-literal (stream subchar arg)
    "#D\"2027-06-15\" => (parse-date \"2027-06-15\")"
    (let ((string (read-literal-string stream subchar arg)))
      (unless *read-suppress* (parse-literal stream subchar #'parse-date string))))

  (defun read-tenor-literal (stream subchar arg)
    "#T\"3M\" => (parse-tenor \"3M\")"
    (let ((string (read-literal-string stream subchar arg)))
      (unless *read-suppress* (parse-literal stream subchar #'parse-tenor string))))

  ;; Equivalent to (DEFREADTABLE SYNTAX (:MERGE :STANDARD) ...), built
  ;; without DEFREADTABLE: merging walks the source readtable with
  ;; named-readtables' SBCL iterator, which fails an internal assertion on
  ;; SBCL 2.6 (named-readtables 2023-10-21). COPY-READTABLE NIL is the
  ;; standard readtable, and registering by name is all IN-READTABLE needs.
  (let ((readtable (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\D #'read-date-literal readtable)
    (set-dispatch-macro-character #\# #\T #'read-tenor-literal readtable)
    (named-readtables:unregister-readtable 'syntax)
    (named-readtables:register-readtable 'syntax readtable)))
