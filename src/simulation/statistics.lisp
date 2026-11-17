;;;; statistics.lisp --- streamed statistics and stored values
;;;;
;;;; Per-date running statistics (Welford, merged in chunk order) and the store of
;;;; kept paths and quantile tables.

(in-package #:fincl)
(named-readtables:in-readtable fincl:syntax)

(defstruct (date-statistics (:constructor make-date-statistics
                                (n &aux (count (make-array n :element-type 'fixnum :initial-element 0))
                                        (mean (make-array n :element-type 'double-float :initial-element 0d0))
                                        (m2 (make-array n :element-type 'double-float :initial-element 0d0))
                                        (minimum (make-array n :element-type 'double-float
                                                               :initial-element most-positive-double-float))
                                        (maximum (make-array n :element-type 'double-float
                                                               :initial-element most-negative-double-float)))))
  "Count, mean, sum of squared deviations, min and max of one recorded name
at each simulation date, updated one value at a time (Welford)."
  (count nil :type (simple-array fixnum (*)))
  (mean nil :type double-vector)
  (m2 nil :type double-vector)
  (minimum nil :type double-vector)
  (maximum nil :type double-vector))

(defun add-statistic (s k x)
  (declare (type date-statistics s) (fixnum k) (double-float x))
  (let* ((n (1+ (aref (date-statistics-count s) k)))
         (d (- x (aref (date-statistics-mean s) k))))
    (setf (aref (date-statistics-count s) k) n)
    (incf (aref (date-statistics-mean s) k) (/ d n))
    (incf (aref (date-statistics-m2 s) k) (* d (- x (aref (date-statistics-mean s) k))))
    (setf (aref (date-statistics-minimum s) k) (min x (aref (date-statistics-minimum s) k))
          (aref (date-statistics-maximum s) k) (max x (aref (date-statistics-maximum s) k)))))

(defun merge-statistics (a b)
  "A's statistics with B's merged in (Chan et al.), date by date; A is updated."
  (dotimes (k (length (date-statistics-count a)) a)
    (let ((na (aref (date-statistics-count a) k)) (nb (aref (date-statistics-count b) k)))
      (when (plusp nb)
        (let* ((n (+ na nb))
               (d (- (aref (date-statistics-mean b) k) (aref (date-statistics-mean a) k))))
          (setf (aref (date-statistics-mean a) k) (+ (aref (date-statistics-mean a) k) (* d (/ nb n)))
                (aref (date-statistics-m2 a) k) (+ (aref (date-statistics-m2 a) k)
                                                   (aref (date-statistics-m2 b) k)
                                                   (* d d (/ (* na nb) n)))
                (aref (date-statistics-count a) k) n
                (aref (date-statistics-minimum a) k) (min (aref (date-statistics-minimum a) k)
                                                          (aref (date-statistics-minimum b) k))
                (aref (date-statistics-maximum a) k) (max (aref (date-statistics-maximum a) k)
                                                          (aref (date-statistics-maximum b) k))))))))

(declaim (inline quiet-nan))
(defun quiet-nan () (make-nan))

(defstruct (simulation-store (:constructor %make-simulation-store))
  "What a run keeps beyond its outputs (:KEEP), shared by the chunks: every
array is allocated before the paths run and each path writes only its own
slots, except the quantile tables, made on first use under LOCK."
  (n-total 0 :type fixnum)
  (n-dates 0 :type fixnum)
  (quantiles nil)                 ; T to keep recorded values per (path, date)
  (quantile-tables (make-hash-table :test 'equal))
  (lock (bt:make-lock "simulation store"))
  (kept-paths 0 :type fixnum)     ; the first K paths' states and held values
  (kept-states nil)               ; path -> date -> vector of leg state copies
  (held nil)                      ; path -> date -> plist of held values
  (records nil)                   ; path -> name -> double vector over dates (NaN: nothing)
  (all-states nil))               ; T: every path's states (into KEPT-STATES)

(defun parse-keep (keep)
  "(values quantiles paths states) from the :KEEP designator(s)."
  (let ((items (if (or (keywordp keep) (and (consp keep) (eq (first keep) :paths))) `(,keep) keep))
        (quantiles nil) (paths 0) (states nil))
    (dolist (item items)
      (match item
        (:quantiles (setf quantiles t))
        (:states (setf states t))
        ((list :paths (guard k (and (integerp k) (>= k 0)))) (setf paths k))
        (_ (signal-invalid-argument 'keep keep 'list
                                    ":QUANTILES, :STATES, (:PATHS k), or a list of them"))))
    (values quantiles paths states)))

(defun keep-form (keep)
  "The form building the :KEEP designator at run time: the keywords are
literal, K in (:PATHS k) is evaluated.

  (keep-form '(:quantiles (:paths n))) => (list :quantiles (list :paths n))

A variable (or NIL) is evaluated as it stands."
  (flet ((item (i)
           (if (and (consp i) (eq (first i) :paths) (= 2 (length i)))
               `(list :paths ,(second i))
               `',i)))
    (cond ((or (keywordp keep) (and (consp keep) (eq (first keep) :paths))) (item keep))
          ((consp keep) `(list ,@(mapcar #'item keep)))
          (t keep))))

(defun make-simulation-store (plan keep)
  (multiple-value-bind (quantiles paths states) (parse-keep keep)
    (let* ((n-total (simulation-n-total plan))
           (n-dates (1+ (length (simulation-plan-event-dates plan))))
           (kept (if states n-total (min paths n-total))))
      (when states
        (let ((doubles (* n-total n-dates
                          (iter (for l in-vector (simulation-plan-legs plan))
                                (sum (process-state-size (simulation-leg-process l)))))))
          (when (> doubles 100000000)
            (warn 'simple-style-warning
                  :format-control "SIMULATE :KEEP :STATES stores ~:D doubles (n-paths x dates x state size)."
                  :format-arguments `(,doubles)))))
      (%make-simulation-store
       :n-total n-total :n-dates n-dates :quantiles quantiles
       :kept-paths kept :all-states states
       :kept-states (when (plusp kept) (make-array kept :initial-element nil))
       :held (when (plusp kept) (make-array kept :initial-element nil))
       :records (when (plusp kept) (make-array kept :initial-element nil))))))

(defun store-path-date (store path k states held)
  "Keep PATH's STATES (copied) and HELD values at date K if PATH is kept."
  (declare (fixnum path k))
  (when (< path (simulation-store-kept-paths store))
    (let ((dates (or (svref (simulation-store-kept-states store) path)
                     (setf (svref (simulation-store-kept-states store) path)
                           (make-array (simulation-store-n-dates store) :initial-element nil)))))
      (setf (svref dates k) (map 'simple-vector #'copy-seq states)))
    (when held
      (let ((dates (or (svref (simulation-store-held store) path)
                       (setf (svref (simulation-store-held store) path)
                             (make-array (simulation-store-n-dates store) :initial-element nil)))))
        (setf (svref dates k) held)))))

(defun keeping-path-p (store path)
  (< path (simulation-store-kept-paths store)))

(defun record-value (statistics store name path k value)
  "Add VALUE to the statistic NAME at date K, in the chunk's STATISTICS."
  (let ((x (if (realp value)
               (float value 1d0)
               (signal-invalid-argument 'value value 'real
                                        (format nil "a real to record under ~S" name)))))
    (add-statistic (or (gethash name statistics)
                       (setf (gethash name statistics)
                             (make-date-statistics (simulation-store-n-dates store))))
                   k x)
    (when (simulation-store-quantiles store)
      (let ((table (or (gethash name (simulation-store-quantile-tables store))
                       (bt:with-lock-held ((simulation-store-lock store))
                         (or (gethash name (simulation-store-quantile-tables store))
                             (setf (gethash name (simulation-store-quantile-tables store))
                                   (make-array (* (simulation-store-n-total store)
                                                  (simulation-store-n-dates store))
                                               :element-type 'double-float :initial-element (quiet-nan))))))))
        (setf (aref table (+ path (* k (simulation-store-n-total store)))) x)))
    (when (keeping-path-p store path)
      ;; Only PATH's worker writes its table, so no lock.
      (let ((names (or (svref (simulation-store-records store) path)
                       (setf (svref (simulation-store-records store) path) (make-hash-table :test 'equal)))))
        (setf (aref (the double-vector
                         (or (gethash name names)
                             (setf (gethash name names)
                                   (make-array (simulation-store-n-dates store)
                                               :element-type 'double-float :initial-element (quiet-nan)))))
                    k)
              x)))
    value))
