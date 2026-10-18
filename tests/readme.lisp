;;;; readme.lisp --- README.org's session, run and checked
;;;;
;;;; RUN-README-CHECK evaluates the #+begin_src lisp blocks of README.org in
;;;; order, as a new user would type them, then checks what they computed.
;;;; It is not part of RUN-TESTS.

(in-package #:fincl/tests)
(named-readtables:in-readtable fincl:syntax)

(defun readme-blocks ()
  "The source of each #+begin_src lisp block of README.org, in order."
  (with-open-file (in (asdf:system-relative-pathname "fincl" "README.org"))
    (iter (with lines = '())
          (with inside = nil)
          (for line = (read-line in nil))
          (while line)
          (for trimmed = (string-trim " " line))
          (cond ((string-equal trimmed "#+begin_src lisp") (setf lines '() inside t))
                ((and inside (string-equal trimmed "#+end_src"))
                 (setf inside nil)
                 (collect (format nil "~{~A~%~}" (reverse lines))))
                (inside (push (org-unescape line) lines))))))

(defun org-unescape (line)
  "LINE with Org's escape removed: a comma before a leading * or #+ inside a
source block."
  (let ((start (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line) (length line))))
    (if (and (< (1+ start) (length line))
             (char= (char line start) #\,)
             (or (char= (char line (1+ start)) #\*)
                 (and (< (+ start 2) (length line)) (string= "#+" line :start2 (1+ start) :end2 (+ start 3)))))
        (concatenate 'string (subseq line 0 start) (subseq line (1+ start)))
        line)))

(defun evaluate-readme ()
  "Evaluate README.org's blocks in order, each form as the REPL would, with
*PACKAGE*, *READTABLE* and the lparallel kernel the session sets bound here.
Returns ((form value ...) ...); an error is returned as (form :error c)."
  (let ((*package* (find-package '#:cl-user))
        (*readtable* *readtable*)
        (lp:*kernel* nil)
        (results '()))
    (unwind-protect
         (handler-bind ((engine-fallback #'muffle-warning))
           (dolist (block (readme-blocks))
             (with-input-from-string (in block)
               (iter (for form = (read in nil in))
                     (until (eq form in))
                     (push (handler-case `(,form ,@(multiple-value-list (eval form)))
                             (error (c) `(,form :error ,c)))
                           results)))))
      (when lp:*kernel* (lp:end-kernel :wait t)))
    (nreverse results)))

(defun readme-checks ()
  (format t "~&README.org session~%")
  (let* ((results (evaluate-readme))
         (failed (remove-if-not (lambda (r) (eq (second r) :error)) results)))
    (check-that (format nil "all ~D forms evaluate~{~%    ~S~}" (length results)
                        (mapcar (lambda (r) (format nil "~S: ~A" (first r) (third r))) failed))
                (null failed))
    (flet ((session (name) (symbol-value (find-symbol name '#:fincl-session))))
      (let* ((market (session "*MARKET*"))
             (call (session "*EURO-CALL*"))
             (put (session "*AMER-PUT*"))
             (binary (session "*BINARY*"))
             ;; S 100, K 110, r 4.25%, q 1.2%, vol 20%, T = 1 (365 days, ACT/365F)
             (f (* 100d0 (exp (- 0.0425d0 0.012d0))))
             (d1 (/ (+ (log (/ f 110d0)) 0.02d0) 0.2d0))
             (d2 (- d1 0.2d0))
             (df (exp -0.0425d0)))
        (check "the Black-Scholes call equals the formula written out"
               (* df (- (* f (norm-cdf d1)) (* 110d0 (norm-cdf d2))))
               (price call :black-scholes market) 1d-10)
        (check-that "automatic selection: Black-Scholes for the European, the tree for the American"
                    (and (typep (nth-value 2 (price call :auto market)) 'black-scholes-engine)
                         (typep (nth-value 2 (price put :auto market)) 'binomial-engine)))
        (check "a GBM vol of 30% by COS = Black-Scholes on the market bumped to 30%"
               (price call :black-scholes (bump market '(:vol :foo) '(:absolute 0.1d0)))
               (price call (make-engine :cos :process (make-gbm :vol 0.3d0)) market) 1d-8)
        (multiple-value-bind (v se) (price binary (make-engine :mc :n-paths 100000) market)
          (check "the binary call by Monte Carlo, within 4 SE of D N(d2)"
                 (* df (norm-cdf d2)) v (* 4 se)))
        (check-that "the session's last form shows Black-Scholes declining the binary"
                    (stringp (second (a:lastcar results))))))))

(defun run-readme-check ()
  "Run README.org's session and check its results. Returns T when all pass."
  (let ((*failures* 0))
    (readme-checks)
    (format t "~&~[README session checks passed~:;~:*~D failure(s)~]~%" *failures*)
    (zerop *failures*)))
