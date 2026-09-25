;;;; syntax.lisp --- #D / #T literals

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defvar *fasl-date*)
(defvar *fasl-tenor*)
(declaim (ftype function fasl-literals))

(defun tenor-equal-p (a b)
  (and (= (tenor-n a) (tenor-n b)) (eq (tenor-unit a) (tenor-unit b))))

(defun read-with-syntax (string)
  (let ((*readtable* (named-readtables:find-readtable 'fincl:syntax)))
    (read-from-string string)))

(defun check-fasl-round-trip ()
  (let ((source (asdf:system-relative-pathname "fincl" "tests/syntax-literals.lisp")))
    (makunbound '*fasl-date*)
    (makunbound '*fasl-tenor*)
    (fmakunbound 'fasl-literals)
    (uiop:with-temporary-file (:pathname fasl
                               :type (pathname-type (compile-file-pathname "x.lisp")))
      (multiple-value-bind (output warnings-p failure-p)
          (let ((*error-output* (make-broadcast-stream)))
            (compile-file source :output-file fasl :verbose nil :print nil))
        (declare (ignore warnings-p))
        (check-that "literal file compiles to a fasl" (and output (not failure-p)))
        (when output
          (load output)
          (check-that "fasl date literal"
                      (date= *fasl-date* (make-date 2027 6 15)))
          (check-that "fasl tenor literal"
                      (tenor-equal-p *fasl-tenor* (make-tenor 3 :months)))
          (check-that "fasl literals inside a function"
                      (destructuring-bind (date days years) (fasl-literals)
                        (and (date= date (make-date 2024 2 29))
                             (tenor-equal-p days (make-tenor 10 :days))
                             (tenor-equal-p years (make-tenor -1 :years))))))))))

(defun syntax-checks ()
  (format t "~&Reader syntax~%")
  (check-equal "#D reads a date" (make-date 2027 6 15) #D"2027-06-15" :test #'date=)
  (check-equal "#T reads a tenor" (make-tenor 3 :months) #T"3M" :test #'tenor-equal-p)
  (check-that "#d and #t are the same dispatch characters"
              (and (date= (read-with-syntax "#d\"2027-06-15\"") #D"2027-06-15")
                   (tenor-equal-p (read-with-syntax "#t\"2W\"") #T"2W")))
  (check-equal "printed date reads back" #D"2024-02-29"
               (read-with-syntax (prin1-to-string #D"2024-02-29")) :test #'date=)
  (check-equal "printed tenor reads back" #T"-1M"
               (read-with-syntax (prin1-to-string #T"-1M")) :test #'tenor-equal-p)
  (check-error "invalid #D literal at read time"
               (read-with-syntax "#D\"2027-02-30\"") invalid-date)
  (check-error "invalid #T literal at read time"
               (read-with-syntax "#T\"3X\"") invalid-tenor)
  (check-error "#D without a string" (read-with-syntax "#D 20270615"))
  (check-that "#+nil skips an invalid literal"
              (equal '(:after) (read-with-syntax "(#+nil #D\"2027-02-30\" :after)")))
  (check-that "global readtable has no #D or #T"
              (and (null (get-dispatch-macro-character #\# #\D *readtable*))
                   (null (get-dispatch-macro-character #\# #\T *readtable*))
                   (null (get-dispatch-macro-character #\# #\D (copy-readtable nil)))))
  (check-that "plain functions work without the syntax"
              (date= (parse-date "2027-06-15") (make-date 2027 6 15)))
  (check-fasl-round-trip))

(defun run-syntax-tests ()
  "Run only the reader-syntax checks. Returns T when all pass."
  (let ((*failures* 0))
    (syntax-checks)
    (format t "~&~[All syntax checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
