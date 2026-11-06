;;;; platform.lisp --- the portable branches of src/platform.lisp, called directly
;;;;
;;;; Each portable branch is a function named %PORTABLE-NAME, so it runs on SBCL
;;;; too, where the public name chooses the SBCL branch.

(in-package #:fincl/tests)

(defun ulp-distance (a b)
  "The distance between the finite doubles A and B in units of the last place of the larger."
  (let ((m (max (abs a) (abs b))))
    (if (zerop m)
        0
        (let ((ulp (scale-float 1d0 (max -1074 (- (nth-value 1 (decode-float m)) 53)))))
          (/ (abs (- a b)) ulp)))))

(defparameter *hypot-magnitudes*
  `(0d0 5d-324 2.2250738585072014d-308 1d-300 1d-160 1d-10 0.3d0 1d0 1.7d0 3d0 1d10 1d150 1d300))

(defun read-source-defun (name relative-path)
  "The DEFUN form NAME read from the fincl source file RELATIVE-PATH."
  (let ((*package* (find-package '#:fincl)))
    (with-open-file (in (asdf:system-relative-pathname :fincl relative-path))
      (iter (for form = (read in nil in))
            (until (eq form in))
            (when (and (consp form) (eq (first form) 'defun) (eq (second form) name))
              (return form))))))

(defun portable-variant (name relative-path)
  "A function compiled from NAME's source with its platform calls replaced by their
portable branches, named PORTABLE-NAME."
  (let ((form (read-source-defun name relative-path))
        (new (intern (format nil "PORTABLE-~A" name) '#:fincl)))
    (compile new
             `(lambda ,@(cddr (sublis `((libm-hypot . %portable-hypot)
                                        (libm-log1p . %portable-log1p)
                                        (libm-log . log)
                                        (,name . ,new))
                                      form))))
    new))

(defun relative-difference (a b)
  (/ (abs (- a b)) (max (abs b) least-positive-normalized-double-float)))

(defun platform-checks ()
  (format t "~&Platform~%")
  (with-quiet-float-traps
    (let ((nan (fincl::make-nan)) (inf (positive-infinity)))
      (check-that "the NaN predicates: true for NaN"
                  (and (fincl::nan-p nan) (fincl::%portable-nan-p nan)))
      (check-that "the NaN predicates: false for infinities, zeros and denormals"
                  (notany (lambda (x) (or (fincl::nan-p x) (fincl::%portable-nan-p x)))
                          `(,inf ,(- inf) 0d0 -0d0 5d-324 -5d-324 1d300 -1d0)))
      (check-that "the infinity predicates: true for both infinities, false elsewhere"
                  (and (fincl::infinity-p inf) (fincl::infinity-p (- inf))
                       (fincl::%portable-infinity-p inf) (fincl::%portable-infinity-p (- inf))
                       (notany (lambda (x) (or (fincl::infinity-p x) (fincl::%portable-infinity-p x)))
                               `(,nan 0d0 -0d0 5d-324 ,most-positive-double-float -1d300))))
      (check-that "make-nan and the portable make-nan (traps masked) give NaNs"
                  (and (fincl::nan-p (fincl::make-nan)) (fincl::nan-p (fincl::%portable-make-nan))))
      ;; Portable hypot against the C library's.
      (let ((worst 0) (bad '()))
        (dolist (x *hypot-magnitudes*)
          (dolist (y *hypot-magnitudes*)
            (dolist (sx '(1d0 -1d0))
              (dolist (sy '(1d0 -1d0))
                (let* ((a (* sx x)) (b (* sy y))
                       (want (fincl::libm-hypot a b)) (got (fincl::%portable-hypot a b)))
                  (cond ((and (fincl::infinity-p want) (fincl::infinity-p got)))
                        (t (let ((d (ulp-distance want got)))
                             (setf worst (max worst d))
                             (when (> d 2) (push (list a b want got) bad)))))))))
        (check-that (format nil "portable hypot within 2 ulp of the C library's over ~D pairs (worst ~,2F ulp)"
                            (* 4 (expt (length *hypot-magnitudes*) 2)) worst)
                    (null bad))))
      (check-that "portable hypot: signed zeros, infinities and NaNs"
                  (and (= 0d0 (fincl::%portable-hypot 0d0 -0d0))
                       (fincl::infinity-p (fincl::%portable-hypot inf 1d0))
                       (fincl::infinity-p (fincl::%portable-hypot -3d0 (- inf)))
                       (fincl::nan-p (fincl::%portable-hypot inf nan))
                       (fincl::nan-p (fincl::%portable-hypot nan 1d0))
                       (fincl::nan-p (fincl::%portable-hypot 1d0 nan))))
      (check-equal "portable log1p near zero keeps its digits" 9.9999999995d-11
                   (fincl::%portable-log1p 1d-10) :test (lambda (a b) (< (relative-difference b a) 1d-15)))))
  ;; complex-sqrt and complex-log built on the portable branches.
  (let ((psqrt (portable-variant 'fincl::complex-sqrt "src/math/special-functions.lisp"))
        (plog (portable-variant 'fincl::complex-log "src/math/special-functions.lisp"))
        (zs (iter outer (for r in '(1d-160 1d-100 1d-3 0.5d0 0.999d0 1d0 1.001d0 2d0 1d3 1d100 1d160))
                  (iter (for theta in '(0d0 1d-8 0.7d0 1.5707963267948966d0 3d0 3.141592653589793d0))
                        (dolist (s '(1d0 -1d0))
                          (in outer (collect (complex (* r (cos theta)) (* s r (sin theta))))))))))
    (let ((worst-sqrt 0d0) (worst-log 0d0))
      (dolist (z (append zs '(#c(-4d0 0d0) #c(-4d0 -0d0) #c(0d0 1d0) #c(1d0 0d0))))
        (let ((s (funcall psqrt z)) (l (funcall plog z)))
          (setf worst-sqrt (max worst-sqrt (relative-difference s (sqrt z))))
          (unless (= z 1d0)
            ;; log is near zero at 1; compare by the larger of the parts there.
            (setf worst-log (max worst-log (/ (abs (- l (log z))) (max (abs (log z)) 1d-3)))))))
      (check-that (format nil "complex-sqrt on the portable hypot agrees with SQRT (worst ~,1E relative)" worst-sqrt)
                  (< worst-sqrt 1d-15))
      (check-that (format nil "complex-log on the portable branches agrees with LOG (worst ~,1E)" worst-log)
                  (< worst-log 1d-15))
      (check-that "portable complex-sqrt keeps the sign of a zero imaginary part"
                  (and (= (imagpart (funcall psqrt #c(-4d0 0d0))) 2d0)
                       (= (imagpart (funcall psqrt #c(-4d0 -0d0))) -2d0)))))
  ;; Special variables.
  (check-that "special-variable-p and the portable branch: true for a special, false for a function"
              (and (fincl::special-variable-p '*read-base*) (fincl::%portable-special-variable-p '*read-base*)
                   (not (fincl::special-variable-p 'car)) (not (fincl::%portable-special-variable-p 'car))))
  ;; Worker count.
  (check-that "the portable processor count is a positive integer"
              (let ((n (fincl::%portable-cpu-count))) (and (integerp n) (plusp n))))
  (let ((fincl::*cpu-count* nil) (fincl::*cpu-count-source* nil)
        (fincl::*fallbacks* (make-hash-table :test 'eq))
        (original (fdefinition 'fincl::%portable-cpu-count))
        (warned 0))
    (unwind-protect
         (progn
           (setf (fdefinition 'fincl::%portable-cpu-count) (lambda () nil))
           (handler-bind ((portability-fallback (lambda (c) (incf warned) (muffle-warning c))))
             (check-equal "an unreadable processor count falls back to 4" 4 (fincl::cpu-count))
             (fincl::cpu-count))
           (check-equal "the fallback warns once" 1 warned)
           (check-equal "the source of the count is recorded" :default fincl::*cpu-count-source*))
      (setf (fdefinition 'fincl::%portable-cpu-count) original)))
  ;; Seeds.
  (check-error "the portable seeded random state signals seed-unsupported"
               (fincl::%portable-seeded-random-state 7) seed-unsupported)
  (check-that "the report of seed-unsupported lists the portable designators"
              (let ((text (handler-case (fincl::%portable-seeded-random-state 7)
                            (seed-unsupported (c) (princ-to-string c)))))
                (and (search ":counter" text) (search "(:pcg n)" text) (search ":mersenne-twister-64" text))))
  (check-that "an implementation with seedable native generators repeats its stream"
              (let ((a (fincl::make-uniform-fn :native 5)) (b (fincl::make-uniform-fn :native 5)))
                (equal (list (funcall a) (funcall a)) (list (funcall b) (funcall b)))))
  ;; Barrier and fallback records.
  (check-that "the publication barrier returns"
              (progn (fincl::memory-publication-barrier) t))
  (let ((fincl::*fallbacks* (make-hash-table :test 'eq))
        (warnings '()))
    (handler-bind ((portability-fallback (lambda (c) (push c warnings) (muffle-warning c))))
      (fincl::note-fallback :demo-real :real "a stand-in")
      (check-equal "a real fallback is silent by default" 0 (length warnings))
      (let ((fincl::*portability-warnings* t))
        (fincl::note-fallback :demo-real-loud :real "another stand-in"))
      (check-equal "a real fallback warns under *portability-warnings*" 1 (length warnings))
      (fincl::note-fallback :demo-degraded :degraded "slower")
      (fincl::note-fallback :demo-degraded :degraded "slower")
      (check-equal "a degraded fallback warns once per feature" 2 (length warnings)))
    (let ((report (with-output-to-string (s) (portability-report s))))
      (check-that "the report lists an active fallback with its kind and reason"
                  (and (search "demo-degraded [degraded]: slower" report)
                       (search "demo-real [real]: a stand-in" report)))
      (check-that "the report names the implementation" (search (lisp-implementation-type) report))))
  (let ((fincl::*fallbacks* (make-hash-table :test 'eq))
        (fincl::*optional-backends* '()))
    (check-that "the report with nothing active says so"
                (let ((report (with-output-to-string (s) (portability-report s))))
                  (and (search "Fallbacks in use: none" report) (search "Optional backends: none" report))))
    (fincl::register-optional-backend :demo "a test backend")
    (check-that "a registered optional backend is listed"
                (search "demo: a test backend" (with-output-to-string (s) (portability-report s)))))
  (check-that "src/ names no implementation extension outside platform.lisp"
              (null (implementation-references))))

(defun code-characters (text)
  "TEXT with comments and string contents blanked, so a search sees code only."
  (let ((out (copy-seq text)) (in-string nil) (in-comment nil) (escaped nil))
    (dotimes (i (length text) out)
      (let ((c (char text i)))
        (cond (in-comment (if (char= c #\Newline) (setf in-comment nil) (setf (char out i) #\Space)))
              (in-string (cond (escaped (setf escaped nil (char out i) #\Space))
                               ((char= c #\\) (setf escaped t (char out i) #\Space))
                               ((char= c #\") (setf in-string nil))
                               (t (setf (char out i) (if (char= c #\Newline) c #\Space)))))
              ((char= c #\")
               (setf in-string t))
              ((and (char= c #\;) (not (and (> i 0) (char= (char text (1- i)) #\\))))
               (setf in-comment t (char out i) #\Space)))))))

(defun implementation-references ()
  "(file . needle) for each file of src/ other than platform.lisp whose code
contains SB-, #+SBCL, #-SBCL or :SYNCHRONIZED."
  (let ((root (asdf:system-relative-pathname :fincl "src/")))
    (iter outer (for file in (directory (merge-pathnames "**/*.lisp" root)))
          (unless (string= (file-namestring file) "platform.lisp")
            (let ((code (string-downcase (code-characters (a:read-file-into-string file)))))
              (dolist (needle '("sb-" "#+sbcl" "#-sbcl" ":synchronized"))
                (when (search needle code)
                  (in outer (collect (cons (file-namestring file) needle))))))))))
