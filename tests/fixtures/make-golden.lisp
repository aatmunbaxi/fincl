;;;; make-golden.lisp --- regenerate tests/fixtures/golden.sexp
;;;;
;;;; Run by hand, never from the test suite, and only when a numerical change
;;;; is intended:
;;;;
;;;;   nix develop -c sbcl --non-interactive --load tests/fixtures/make-golden.lisp
;;;;
;;;; Overwrites the fixture, dropping any hand-added :tolerance entries.

(ql:quickload :fincl/tests :silent t)
(format t "~&Wrote ~D golden entries to ~A~%"
        (funcall (find-symbol "WRITE-GOLDEN-FIXTURE" "FINCL/TESTS"))
        (funcall (find-symbol "GOLDEN-PATH" "FINCL/TESTS")))
