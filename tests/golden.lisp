;;;; golden.lisp --- exact outputs of the current engines
;;;;
;;;; Each case prices one fixed contract with fixed engine settings and seeds.
;;;; tests/fixtures/golden.sexp records what the code returned when the
;;;; fixture was generated; RUN-GOLDEN-TESTS demands the same doubles, with
;;;; =, under several worker counts. Internal refactors must pass unchanged.
;;;; A task that is allowed to move a result by round-off adds
;;;; ":tolerance x" to that entry by hand, with a comment saying why.
;;;;
;;;; Regenerate only on purpose (a deliberate numerical change):
;;;;
;;;;   nix develop -c sbcl --non-interactive --load tests/fixtures/make-golden.lisp

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun golden-path ()
  (asdf:system-relative-pathname "fincl" "tests/fixtures/golden.sexp"))

(defun golden-cases ()
  "(name thunk) pairs; each thunk returns (values price standard-error)."
  (let* ((flat (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0 :vol 0.2d0))
         (div (make-bs-market :valuation-date *today* :spot 100 :rate 0.05d0
                              :dividend 0.03d0 :vol 0.25d0))
         (ls (make-bs-market :valuation-date *today* :spot 36 :rate 0.06d0 :vol 0.2d0))
         (heston (make-instance 'heston :xi 0.5d0 :rho -0.7d0))
         (cev (make-instance 'cev :beta 0.7d0))
         (call (make-option '(:call 100 #D"2027-09-24")))
         (aput (make-option '(:american :put 40 #D"2027-09-24")))
         (aput-110 (make-option '(:american :put 110 #D"2027-09-24")))
         (acall-90 (make-option '(:american :call 90 #D"2027-09-24")))
         (quarterly (make-instance 'option
                                   :payoff (make-instance 'vanilla-payoff :kind :put
                                                                          :strike 100d0)
                                   :exercise (make-instance
                                              'bermudan-exercise
                                              :dates (list #D"2026-12-24" #D"2027-03-24"
                                                           #D"2027-06-24" #D"2027-09-24")))))
    (macrolet ((case-list (&rest cases)
                 `(list ,@(loop for (name form) in cases
                                collect `(list ,name (lambda () ,form))))))
      (case-list
       ;; Monte Carlo: fixed seeds and chunking; the answers do not depend
       ;; on the worker count.
       ("mc european gbm, antithetic" (price call (mc :n-paths 50000) flat))
       ("mc european gbm, no antithetic" (price call (mc :n-paths 50000 :antithetic nil) flat))
       ("mc european gbm, inverse transform"
        (price call (mc :n-paths 50000 :sampler :inverse-transform) flat))
       ("mc european gbm, pcg stream" (price call (mc :n-paths 50000 :rng :pcg) flat))
       ("mc european heston, stepped"
        (price call (mc :n-paths 20000 :n-steps 50 :process heston) flat))
       ("mc european cev, stepped" (price call (mc :n-paths 20000 :n-steps 50 :process cev) flat))
       ("mc american gbm, lsm" (price aput (mc :n-paths 20000 :n-steps 50) ls))
       ("mc american heston, lsm" (price aput (mc :n-paths 20000 :n-steps 50 :process heston) ls))
       ;; Lattice.
       ("binomial american put, bbsr" (price aput-110 (binomial 730) div))
       ("binomial american put, bbs" (price aput-110 (binomial 730 :extrapolate nil) div))
       ("binomial american put, crr"
        (price aput-110 (binomial 730 :smooth nil :extrapolate nil) div))
       ("binomial american call, bbsr" (price acall-90 (binomial 730) div))
       ("binomial bermudan put, bbsr" (price quarterly (binomial 730) div))
       ("binomial bermudan put, crr"
        (price quarterly (binomial 730 :smooth nil :extrapolate nil) div))
       ;; Fourier-cosine.
       ("cos european gbm" (price call (make-instance 'cos-engine) flat))
       ("cos european heston" (price call (make-instance 'cos-engine :process heston) flat))
       ("cos bermudan gbm put" (price quarterly (make-instance 'cos-engine) div))
       ("cos american gbm put" (price aput-110 (make-instance 'cos-engine) div))
       ("cos american gbm call" (price acall-90 (make-instance 'cos-engine) div))
       ;; Closed forms.
       ("black-scholes european" (price call (make-instance 'black-scholes-engine) flat))
       ("baw american put" (price aput-110 (make-instance 'barone-adesi-whaley-engine) div))))))

(defun evaluate-golden-cases ()
  "(name price standard-error) for every case, in the current kernel."
  (loop for (name thunk) in (golden-cases)
        collect (multiple-value-bind (v se) (funcall thunk) (list name v se))))

(defun write-golden-fixture (&key (path (golden-path)) (workers 4))
  "Evaluate every case and overwrite PATH. Hand-added tolerances are lost."
  (let ((entries (let ((lp:*kernel* (lp:make-kernel workers)))
                   (unwind-protect (evaluate-golden-cases)
                     (lp:end-kernel :wait t)))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (format s ";;;; golden.sexp --- generated by tests/fixtures/make-golden.lisp~%~
;;;; Entries: (name price standard-error [:tolerance x]). Add a :tolerance by~%~
;;;; hand only when a task allows a round-off change, with a comment saying why.~%~%")
        (dolist (e entries)
          (prin1 e s)
          (terpri s))))
    (length entries)))

(defun read-golden-fixture ()
  (with-open-file (s (golden-path))
    (with-standard-io-syntax
      (loop for entry = (read s nil s)
            until (eq entry s)
            collect entry))))

(defun golden-match-p (expected actual tolerance)
  (cond ((and (null expected) (null actual)) t)
        ((or (null expected) (null actual)) nil)
        (tolerance (<= (abs (- expected actual)) (* tolerance (max 1d0 (abs expected)))))
        (t (= expected actual))))

(defun golden-checks (&optional label)
  "Compare every case against the fixture in the current kernel."
  (format t "~&Golden values~@[ (~A)~]~%" label)
  (let ((fixture (read-golden-fixture))
        (actual (evaluate-golden-cases)))
    (loop for (name price se . options) in fixture
          for tolerance = (getf options :tolerance)
          for found = (find name actual :key #'first :test #'string=)
          do (cond ((null found)
                    (incf *failures*)
                    (format t "~&  FAIL ~A: in the fixture but no longer a case~%" name))
                   ((and (golden-match-p price (second found) tolerance)
                         (golden-match-p se (third found) tolerance))
                    (format t "~&  ok   ~A~:[~; (tolerance ~:*~,1E)~]~%" name tolerance))
                   (t
                    (incf *failures*)
                    (format t "~&  FAIL ~A: expected ~S ~S, got ~S ~S~%"
                            name price se (second found) (third found)))))
    (dolist (a actual)
      (unless (find (first a) fixture :key #'first :test #'string=)
        (incf *failures*)
        (format t "~&  FAIL ~A: not in the fixture; regenerate it~%" (first a))))))

(defun run-golden-tests (&key (workers '(1 2 8)))
  "Check the golden values under each worker count in WORKERS. Returns T when
all pass."
  (let ((*failures* 0))
    (dolist (n workers)
      (let ((lp:*kernel* (lp:make-kernel n)))
        (unwind-protect (golden-checks (format nil "~D worker~:P" n))
          (lp:end-kernel :wait t))))
    (format t "~&~[All golden checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
