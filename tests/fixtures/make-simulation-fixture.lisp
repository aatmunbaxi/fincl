;;;; make-simulation-fixture.lisp --- write tests/fixtures/simulation-spot-only.sexp
;;;;
;;;; Written once, from the code before task 32, and never regenerated: the
;;;; task requires spot-only SIMULATE runs to stay = to it.
;;;;
;;;;   nix develop -c sbcl --non-interactive --load tests/fixtures/make-simulation-fixture.lisp

(ql:quickload :fincl/tests :silent t)
(format t "~&Wrote ~D spot-only runs to ~A~%"
        (funcall (find-symbol "WRITE-SPOT-ONLY-FIXTURE" "FINCL/TESTS"))
        (funcall (find-symbol "SPOT-ONLY-PATH" "FINCL/TESTS")))
