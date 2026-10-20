;;;; report.lisp --- verdicts and the printed tables
;;;;
;;;; A ROW joins one benchmark's two sides: (:benchmark b :quantlib plist
;;;; :fincl plist :parallel plist :workers n), the plists as MEASURE (Lisp)
;;;; and the C++ program return them, or NIL for a side that did not run.

(in-package #:fincl/benchmarks)

;;; --------------------------------------------------------------------
;;; Verdicts
;;; --------------------------------------------------------------------

(defun side-value (side) (and side (getf side :value)))
(defun side-error (side) (and side (getf side :std-error)))

(defun row-verdict (row)
  "(values verdict difference z): VERDICT one of :AGREE, :WITHIN, :DIFFER,
:NONE (no QuantLib value) or :ERROR; DIFFERENCE fincl minus QuantLib; Z the
difference in combined standard errors (:STATISTICAL only)."
  (destructuring-bind (&key benchmark quantlib fincl &allow-other-keys) row
    (let ((ql (side-value quantlib)) (fv (side-value fincl)))
      (cond ((or (getf fincl :error) (getf quantlib :error)) (values :error nil nil))
            ((or (null ql) (null fv) (eq (benchmark-kind benchmark) :none)) (values :none nil nil))
            (t
             (let ((d (- fv ql))
                   (tolerance (benchmark-tolerance benchmark)))
               (ecase (benchmark-kind benchmark)
                 (:deterministic
                  (values (if (<= (abs d) (or tolerance (* 1d-8 (max 1d0 (abs ql))))) :agree :differ)
                          d nil))
                 (:method-error
                  (values (if (<= (abs d) tolerance) :within :differ) d nil))
                 (:statistical
                  (let* ((se (sqrt (+ (expt (or (side-error quantlib) 0d0) 2)
                                      (expt (or (side-error fincl) 0d0) 2))))
                         (z (if (plusp se) (/ d se) 0d0)))
                    (values (if (<= (abs z) 3d0) :agree :differ) d z))))))))))

(defun verdict-label (verdict benchmark &key org)
  (ecase verdict
    (:agree "agree")
    (:within (format nil "within ~A" (format-short (benchmark-tolerance benchmark))))
    (:differ (if org "*DIFFER*" "DIFFER"))
    (:none "--")
    (:error (if org "*ERROR*" "ERROR"))))

;;; --------------------------------------------------------------------
;;; Number formats
;;; --------------------------------------------------------------------

(defun format-value (x &optional (digits 8))
  "X to DIGITS significant digits; -- for NIL."
  (cond ((null x) "--")
        ((zerop x) "0")
        (t (let ((e (floor (log (abs x) 10))))
             (if (<= -3 e 6)
                 (format nil "~,vF" (max 0 (- digits 1 e)) x)
                 (format nil "~,v,2,,,,'eE" (1- digits) x))))))

(defun format-short (x)
  "X to two significant digits."
  (cond ((null x) "--")
        ((zerop x) "0")
        ((<= 1d-2 (abs x) 1d3) (format-value x 2))
        (t (format nil "~,1,,,,,'eE" x))))

(defun format-seconds (s)
  "S seconds in a scaled unit, three significant digits."
  (if (null s)
      "--"
      (multiple-value-bind (v unit)
          (cond ((< s 1d-6) (values (* s 1d9) "ns"))
                ((< s 1d-3) (values (* s 1d6) "us"))
                ((< s 1d0) (values (* s 1d3) "ms"))
                (t (values s "s")))
        (format nil "~A ~A"
                (cond ((< v 10) (format nil "~,2F" v))
                      ((< v 100) (format nil "~,1F" v))
                      (t (format nil "~D" (round v))))
                unit))))

(defun format-bytes (b)
  (cond ((null b) "--")
        ((< b 1024) (format nil "~D B" b))
        ((< b (* 1024 1024)) (format nil "~,1F KB" (/ b 1024)))
        (t (format nil "~,1F MB" (/ b 1048576)))))

(defun format-time-cell (side)
  "Median [minimum] and the median absolute deviation in percent."
  (if (and side (getf side :median))
      (destructuring-bind (&key median min mad &allow-other-keys) side
        (format nil "~A [~A] ±~D%" (format-seconds median) (format-seconds min)
                (round (* 100 mad) median)))
      "--"))

(defun time-ratio (fincl quantlib)
  (and fincl quantlib (getf fincl :median) (getf quantlib :median)
       (/ (getf fincl :median) (getf quantlib :median))))

(defun format-ratio (r) (if r (format nil "~,2Fx" r) "--"))

(defun cost-to-se (side &optional (target 1d-3))
  "Seconds to reach a standard error of TARGET: t (se / target)^2."
  (let ((se (side-error side)) (median (and side (getf side :median))))
    (and se median (* median (expt (/ se target) 2)))))

;;; --------------------------------------------------------------------
;;; Tables
;;; --------------------------------------------------------------------

(defun print-table (headers rows stream &key org)
  "HEADERS and ROWS (lists of strings) as fixed-width columns, the first
left-aligned and the rest right-aligned, or as an org table."
  (let ((widths (apply #'mapcar (lambda (&rest cells) (reduce #'max cells :key #'length))
                       headers rows)))
    (flet ((line (cells)
             (if org
                 (format stream "~&|~{ ~A |~}~%"
                         (mapcar (lambda (c w) (format nil "~vA" w c)) cells widths))
                 (format stream "~& ~{~A~^  ~}~%"
                         (iter (for c in cells) (for w in widths) (for first initially t then nil)
                               (collect (if first (format nil "~vA" w c) (format nil "~v@A" w c))))))))
      (line headers)
      (when org
        (format stream "~&|~{~A~^+~}|~%"
                (mapcar (lambda (w) (make-string (+ w 2) :initial-element #\-)) widths)))
      (mapc #'line rows))))

(defun statistical-group-p (rows)
  (some (lambda (row) (eq (benchmark-kind (getf row :benchmark)) :statistical)) rows))

(defun value-cell (side statistical)
  (cond ((null side) "--")
        ((getf side :error) "ERROR")
        ((and statistical (side-error side))
         (format nil "~A ± ~A" (format-value (side-value side) 6) (format-value (side-error side) 2)))
        (t (format-value (side-value side)))))

(defun row-cells (row statistical org)
  "The cells of ROW's line, and of its N-worker line if any."
  (destructuring-bind (&key benchmark quantlib fincl parallel workers) row
    (multiple-value-bind (verdict d z) (row-verdict row)
      (let ((main `(,(benchmark-id benchmark)
                    ,(value-cell quantlib statistical)
                    ,(value-cell fincl statistical)
                    ,(cond ((null d) "--")
                           (z (format nil "~,2Fσ" z))
                           (t (format-short d)))
                    ,(verdict-label verdict benchmark :org org)
                    ,(format-time-cell quantlib)
                    ,(format-time-cell fincl)
                    ,(format-ratio (time-ratio fincl quantlib))
                    ,@(when statistical
                        `(,(format nil "~A / ~A" (format-seconds (cost-to-se quantlib))
                                   (format-seconds (cost-to-se fincl)))))
                    ,(format-bytes (getf fincl :consed))
                    ,(format-seconds (getf quantlib :setup-seconds))
                    ,(format-seconds (getf fincl :setup-seconds)))))
        `(,main
          ,@(when parallel
              `((,(format nil "  [fincl, ~D workers]" workers) "" ,(value-cell parallel statistical)
                 "" "" "" ,(format-time-cell parallel) ,(format-ratio (time-ratio parallel quantlib))
                 ,@(when statistical `(,(format nil "-- / ~A" (format-seconds (cost-to-se parallel)))))
                 ,(format-bytes (getf parallel :consed)) "" ,(format-seconds (getf parallel :setup-seconds))))))))))

(defun geometric-mean (xs)
  (and xs (exp (/ (reduce #'+ xs :key #'log) (length xs)))))

(defun one-line (string)
  "STRING with its runs of whitespace collapsed to single spaces."
  (format nil "~{~A~^ ~}" (uiop:split-string (substitute #\Space #\Newline string)
                                             :separator '(#\Space))))

(defun print-group (group rows stream &key org workers)
  (let* ((statistical (statistical-group-p rows))
         (title (format nil "~(~A~)~:[~; (1 worker; extra rows at ~D)~]" group
                        (some (lambda (r) (getf r :parallel)) rows) workers)))
    (if org
        (format stream "~&~%** ~A~%" title)
        (format stream "~&~%== ~A ~A~%" title (make-string (max 3 (- 78 (length title))) :initial-element #\=)))
    (print-table `("benchmark" "QuantLib value" "fincl value" ,(if statistical "diff (se)" "diff")
                   "verdict" "QuantLib median [min] ±MAD" "fincl median [min] ±MAD" "ratio"
                   ,@(when statistical '("cost to 1e-3 se (QL / fincl)"))
                   "fincl consed" "QL setup" "fincl setup")
                 (mapcan (lambda (row) (row-cells row statistical org)) rows)
                 stream :org org)
    (let ((ratios (iter (for row in rows)
                        (for r = (time-ratio (getf row :fincl) (getf row :quantlib)))
                        (when r (collect r))))
          (verdicts (mapcar #'row-verdict rows)))
      (format stream "~&~% geometric mean of the ratios: ~A~@[ (~A)~]; ~D DIFFER, ~D without QuantLib, ~D ERROR~%"
              (format-ratio (geometric-mean ratios))
              (a:when-let ((g (geometric-mean ratios)))
                (cond ((> g 1.05d0) "fincl slower") ((< g 0.95d0) "fincl faster") (t "even")))
              (count :differ verdicts) (count :none verdicts) (count :error verdicts)))
    (dolist (row rows)
      (destructuring-bind (&key benchmark quantlib fincl &allow-other-keys) row
        (let ((id (benchmark-id benchmark)))
          (a:when-let ((notes (benchmark-notes benchmark)))
            (format stream "~& ~A: ~A~%" id (one-line notes)))
          (a:when-let ((reference (getf (getf fincl :details) :reference)))
            (format stream "~&   ~A against the reference ~A: QuantLib ~A, fincl ~A~%"
                    id (format-value reference)
                    (if (side-value quantlib) (format-short (- (side-value quantlib) reference)) "--")
                    (if (side-value fincl) (format-short (- (side-value fincl) reference)) "--")))
          (print-list-differences id (getf quantlib :details) (getf fincl :details) stream)
          (dolist (side `(("QuantLib" ,quantlib) ("fincl" ,(and fincl `(:details ,(a:remove-from-plist (getf fincl :details) :reference)
                                                                       ,@fincl)))))
            (destructuring-bind (name plist) side
              (a:when-let ((details (iter (for (k v) on (getf plist :details) by #'cddr)
                                          (unless (number-list-p v) (appending `(,k ,v))))))
                (format stream "~&   ~A ~A: ~A~%" id name (format-details details)))
              (a:when-let ((message (getf plist :error)))
                (format stream "~&   ~A ~A ERROR: ~A~%" id name (one-line message))))))))
    (when (eq group :calibration)
      (print-protocol-check rows stream))))

(defun number-list-p (x) (and (consp x) (every #'realp x)))

(defun format-details (plist)
  "PLIST with numbers to eight digits and lists of numbers as their count."
  (format nil "~{~(~A~) ~A~^, ~}"
          (iter (for (k v) on plist by #'cddr)
                (collect k)
                (collect (cond ((number-list-p v) (format nil "[~D values]" (length v)))
                               ((and (realp v) (= v (round v)) (< (abs v) 1d6)) (format nil "~D" (round v)))
                               ((realp v) (format-value v))
                               (t (format nil "~(~A~)" v)))))))

(defun print-list-differences (id quantlib fincl stream)
  "For each key whose value is a list of numbers on both sides, the largest
difference between them."
  (iter (for (k v) on quantlib by #'cddr)
        (for w = (getf fincl k))
        (when (and (number-list-p v) (number-list-p w) (= (length v) (length w)))
          (format stream "~&   ~A ~(~A~): largest |fincl - QuantLib| ~A over ~D values~%"
                  id k (format-short (reduce #'max (mapcar (lambda (a b) (abs (- a b))) v w)))
                  (length v)))))

(defun print-protocol-check (rows stream)
  "Whether both sides timed the 50 ms sleep within 2%."
  (a:when-let ((row (find "sleep-50ms" rows :test #'string= :key (lambda (r) (benchmark-id (getf r :benchmark))))))
    (flet ((within (side)
             (let ((m (and side (getf side :median))))
               (cond ((null m) "--")
                     ((<= (abs (- m 0.05d0)) 0.001d0) (format nil "~A (within 2%)" (format-seconds m)))
                     (t (format nil "~A (NOT within 2%)" (format-seconds m)))))))
      (format stream "~& protocol: the 50 ms sleep times as QuantLib ~A, fincl ~A~%"
              (within (getf row :quantlib)) (within (getf row :fincl))))))

(defun print-discrepancies (rows stream &key org)
  (if org
      (format stream "~&~%** discrepancies~%")
      (format stream "~&~%== discrepancies ~A~%" (make-string 62 :initial-element #\=)))
  (let ((bad (remove-if-not (lambda (row) (member (row-verdict row) '(:differ :error))) rows)))
    (if (null bad)
        (format stream "~& (none)~%")
        (dolist (row bad)
          (destructuring-bind (&key benchmark quantlib fincl &allow-other-keys) row
            (multiple-value-bind (verdict d z) (row-verdict row)
              (format stream "~& ~A: ~A; QuantLib ~A, fincl ~A~@[, difference ~A~]~@[ (~,2F se)~]~@[, tolerance ~A~]~@[~%   ~A~]~%"
                      (benchmark-id benchmark) (verdict-label verdict benchmark :org org)
                      (value-cell quantlib t) (value-cell fincl t)
                      (and d (format-short d)) z
                      (and (benchmark-tolerance benchmark) (format-short (benchmark-tolerance benchmark)))
                      (a:when-let ((n (benchmark-notes benchmark))) (one-line n)))))))))
