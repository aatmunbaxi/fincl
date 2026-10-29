;;;; viz/json.lisp --- Plotly figures written as JSON
;;;;
;;;; A figure is Lisp data, (:data traces :layout plist :config plist), and
;;;; this file, the only one that calls jzon, fixes how it reads as JSON:
;;;;
;;;;   - a non-empty plist whose keys are all keywords or strings is an object;
;;;;     keyword keys are lower-cased with - turned into _ (:error-y ->
;;;;     "error_y"), string keys pass through unchanged;
;;;;   - any other list and any vector is an array (write an array of an even
;;;;     number of keywords as a vector); a 2-D array is an array of its rows;
;;;;     #() is the empty array;
;;;;   - integers are integers; other reals are written as doubles, NaN and
;;;;     the infinities as null; T and :TRUE are true, NIL and :FALSE false,
;;;;     :NULL null; any other keyword is its lower-case name (:lines+markers ->
;;;;     "lines+markers"); strings are strings.
;;;;
;;;; Anything else signals INVALID-ARGUMENT naming where in the figure it is.
;;;; Keys are written in the order given and floats in their shortest form, so
;;;; writing a figure twice gives the same bytes.

(in-package #:fincl-viz)

(defun json-object-p (x)
  "True if X is a non-empty plist whose keys are all keywords or strings."
  (and (consp x)
       (iter (for tail initially x then (cddr tail))
             (until (null tail))
             (always (and (consp tail) (consp (cdr tail))
                          (typep (car tail) '(or keyword string)))))))

(defun json-key (key)
  (etypecase key
    (string key)
    (keyword (substitute #\_ #\- (string-downcase (symbol-name key))))))

(defun json-place (path)
  "PATH, innermost first and rooted at a key, as figure.data[0].x[3]."
  (destructuring-bind (root &rest segments) (reverse path)
    (format nil "~A~{~A~}" root
            (mapcar (lambda (segment)
                      (etypecase segment
                        (integer (format nil "[~D]" segment))
                        (string (format nil ".~A" segment))))
                    segments))))

(defun write-json-double (x)
  (jzon:write-value* (if (or (sb-ext:float-nan-p x) (sb-ext:float-infinity-p x)) 'null x)))

(defun write-json-elements (elements path)
  "Write the sequence ELEMENTS as a JSON array."
  (jzon:begin-array*)
  (iter (for x in-sequence elements)
        (for i from 0)
        (write-json-value x (cons i path)))
  (jzon:end-array*))

(defun write-json-value (value path)
  "Write VALUE through jzon's current writer; PATH locates it for errors."
  (cond ((json-object-p value)
         (jzon:begin-object*)
         (iter (for (key x) in-plist value)
               (let ((name (json-key key)))
                 (jzon:write-key* name)
                 (write-json-value x (cons name path))))
         (jzon:end-object*))
        ((member value '(t :true)) (jzon:write-value* t))
        ((member value '(nil :false)) (jzon:write-value* nil))
        ((eq value :null) (jzon:write-value* 'null))
        ((keywordp value) (jzon:write-value* (string-downcase (symbol-name value))))
        ((stringp value) (jzon:write-value* value))
        ((integerp value) (jzon:write-value* value))
        ((realp value) (write-json-double (float value 1d0)))
        ((and (arrayp value) (= (array-rank value) 2))
         (jzon:begin-array*)
         (dotimes (i (array-dimension value 0))
           (write-json-elements (make-array (array-dimension value 1)
                                            :element-type (array-element-type value)
                                            :displaced-to value
                                            :displaced-index-offset
                                            (* i (array-dimension value 1)))
                                (cons i path)))
         (jzon:end-array*))
        ((or (vectorp value) (a:proper-list-p value))
         (write-json-elements value path))
        (t (signal-invalid-argument
            (json-place path) value
            '(or list vector (array * (* *)) real string symbol)
            "a plist, a list, a vector, a 2-D array, a real, a string, T, NIL or a keyword"))))

(defun figure-json (figure)
  "The JSON text of FIGURE, a plist (:data traces :layout plist :config plist).

  (figure-json '(:data ((:type :scatter :x #(0 1 2) :y #(1 4 9)))
                 :layout (:title (:text \"squares\"))))"
  (unless (json-object-p figure)
    (signal-invalid-argument 'figure figure 'list "a plist (:data traces :layout plist ...)"))
  (with-output-to-string (s)
    (jzon:with-writer* (:stream s)
      (write-json-value figure '("figure")))))

(defun write-figure (figure path)
  "Write FIGURE's JSON to PATH, replacing any file there; returns PATH."
  (let ((text (figure-json figure)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string text s))
    path))
