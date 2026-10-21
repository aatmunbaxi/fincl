;;;; package.lisp --- fincl against QuantLib (task 27)
;;;;
;;;; Each group directory holds GROUP.lisp, whose DEFINE-BENCHMARK forms say
;;;; what fincl computes, and GROUP.cpp, a standalone QuantLib program that
;;;; runs the same-named benchmarks and prints one s-expression per benchmark.
;;;; RUN-BENCHMARKS compiles and runs the C++ side, runs the Lisp side under
;;;; the same protocol (harness.lisp), and prints the joined report.

(defpackage #:fincl/benchmarks
  (:use #:cl #:fincl #:iterate)
  (:shadowing-import-from #:fincl #:minimize)
  (:import-from #:arrow-macros #:-> #:->>)
  (:local-nicknames (#:a #:alexandria) (#:lp #:lparallel))
  (:export #:run-benchmarks #:replay-results))
