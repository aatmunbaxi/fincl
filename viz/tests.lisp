;;;; viz/tests.lisp --- checks for fincl-viz; not part of fincl's run-tests
;;;;
;;;;   (asdf:test-system :fincl-viz)   or   (fincl-viz/tests:run-viz-tests)
;;;;
;;;; The renderer checks run viz/render.py, so they need the dev shell's Python.

(defpackage #:fincl-viz/tests
  (:use #:cl #:fincl #:fincl-viz #:iterate)
  (:shadowing-import-from #:fincl #:minimize)
  (:local-nicknames (#:a #:alexandria))
  (:export #:run-viz-tests))

(in-package #:fincl-viz/tests)

(defvar *failures* 0)

(defmacro check-that (name form)
  `(if ,form
       (format t "~&  ok   ~A~%" ,name)
       (progn (incf *failures*) (format t "~&  FAIL ~A~%" ,name))))

(defmacro check-equal (name expected actual)
  (let ((e (gensym)) (a (gensym)))
    `(let ((,e ,expected) (,a ,actual))
       (if (equal ,e ,a)
           (format t "~&  ok   ~A (~A)~%" ,name ,a)
           (progn (incf *failures*)
                  (format t "~&  FAIL ~A: expected ~A, got ~A~%" ,name ,e ,a))))))

(defmacro check-error (name form &optional (type 'error))
  `(if (handler-case (progn ,form nil) (,type () t))
       (format t "~&  ok   ~A signals ~A~%" ,name ',type)
       (progn (incf *failures*)
              (format t "~&  FAIL ~A: no ~A signalled~%" ,name ',type))))

(defun json (value)
  "The JSON of VALUE written as the value of a one-key figure, unwrapped."
  (let ((text (figure-json `(:v ,value))))
    (subseq text 5 (1- (length text)))))

(defun doubles (&rest xs)
  (make-array (length xs) :element-type 'double-float
                          :initial-contents (mapcar (lambda (x) (float x 1d0)) xs)))

(defun json-checks ()
  (format t "~&JSON~%")
  ;; Objects and arrays.
  (check-equal "plist is an object" "{\"a\":1,\"b\":\"x\"}" (json '(:a 1 :b "x")))
  (check-equal "nested objects" "{\"line\":{\"width\":2}}" (json '(:line (:width 2))))
  (check-equal "list of numbers is an array" "[1,2,3]" (json '(1 2 3)))
  (check-equal "odd-length keyword list is an array" "[\"a\",1,\"b\"]" (json '(:a 1 :b)))
  (check-equal "non-keyword key makes an array" "[\"a\",1,2,3]" (json '(:a 1 2 3)))
  (check-equal "even keyword vector is an array" "[\"lines\",\"markers\"]" (json #(:lines :markers)))
  (check-equal "list of plists" "[{\"a\":1},{\"a\":2}]" (json '((:a 1) (:a 2))))
  (check-equal "empty vector" "[]" (json #()))
  ;; Keys.
  (check-equal "hyphen to underscore" "{\"error_y\":{\"array\":[1]}}" (json '(:error-y (:array (1)))))
  (check-equal "digits kept" "{\"xaxis2\":1}" (json '(:xaxis2 1)))
  (check-equal "string key unchanged" "{\"fooBar-baz\":1}" (json '("fooBar-baz" 1)))
  ;; Numbers.
  (check-equal "double vector" "[1.5,0.25]" (json (doubles 1.5 0.25)))
  (check-equal "rational as double" "0.25" (json 1/4))
  (check-equal "integer stays integer" "7" (json 7))
  (check-equal "single float as double" (json (float 0.5f0 1d0)) (json 0.5f0))
  (check-equal "NaN, infinities as null" "[null,null,null,1.0]"
               (json (sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
                       (let ((inf sb-ext:double-float-positive-infinity))
                         (doubles (- inf inf) inf (- inf) 1)))))
  (check-equal "2-D array by rows" "[[1,2,3],[4,5,6]]" (json #2A((1 2 3) (4 5 6))))
  (check-equal "2-D double array by rows" "[[1.0,2.0],[3.0,4.5]]"
               (json (make-array '(2 2) :element-type 'double-float
                                        :initial-contents '((1d0 2d0) (3d0 4.5d0)))))
  ;; Constants and keywords.
  (check-equal "t and :true" "[true,true]" (json '(t :true)))
  (check-equal "nil and :false" "{\"a\":false,\"b\":false}" (json '(:a nil :b :false)))
  (check-equal ":null" "null" (json :null))
  (check-equal "keyword value, no underscore" "\"lines+markers\"" (json :lines+markers))
  (check-equal "hyphenated keyword value kept" "\"top-left\"" (json :top-left))
  (check-equal "string" "\"a \\\"b\\\"\"" (json "a \"b\""))
  ;; Refusals name their place.
  (check-error "rank 3 refused" (json (make-array '(1 1 1) :initial-element 0)) invalid-argument)
  (check-error "figure must be a plist" (figure-json '(1 2)) invalid-argument)
  (check-equal "refused object's place"
               "figure.data[0].x[1]"
               (handler-case (figure-json `(:data ((:x (1 ,#p"/tmp/x")))))
                 (invalid-argument (c) (fincl::invalid-argument-place c))))
  ;; Writing twice gives the same bytes.
  (let* ((figure `(:data ((:type :scatter :x ,(doubles 0.1d0 0.2d0 0.3d0) :y (1 2 3)))
                   :layout (:title (:text "twice"))))
         (path-1 (write-figure figure #p"/tmp/fincl-viz-tests/a.json"))
         (path-2 (write-figure figure #p"/tmp/fincl-viz-tests/b.json")))
    (check-that "rewrites are byte-identical"
                (equalp (a:read-file-into-byte-vector path-1)
                        (a:read-file-into-byte-vector path-2)))
    (check-equal "file holds figure-json" (figure-json figure)
                 (a:read-file-into-string path-1 :external-format :utf-8))))

(defparameter *test-figure*
  `(:data ((:type :scatter :mode :lines :name "line" :x ,(doubles 0 1 2) :y ,(doubles 1 4 9))
           (:type :bar :name "bar" :x #("a" "b") :y #(3 5) :xaxis "x2" :yaxis "y2")
           (:type :surface :z ,(make-array '(2 3) :element-type 'double-float
                                                  :initial-contents '((1d0 2d0 3d0) (2d0 3d0 5d0)))
            :scene "scene"))
    :layout (:width 900 :height 400
             :xaxis (:domain (0 0.3)) :yaxis (:domain (0 1))
             :xaxis2 (:domain (0.35 0.6) :anchor "y2") :yaxis2 (:domain (0 1) :anchor "x2")
             :scene (:domain (:x (0.65 1) :y (0 1)))
             :title (:text "fincl-viz renderer check"))
    :config (:displaylogo nil))
  "A line, a bar on a second pair of axes, and a 3-D surface.")

(defun renderer-checks ()
  (format t "~&Renderer~%")
  (let ((*output-directory* #p"/tmp/fincl-viz-tests/"))
    (let ((json (emit-figure *test-figure* "renderer" :display :json)))
      (check-equal ":json writes only the JSON" "json" (pathname-type json)))
    (let ((html (emit-figure *test-figure* "renderer" :display :html)))
      (check-that "HTML written" (search "plotly" (a:read-file-into-string html))))
    (let ((png (emit-figure *test-figure* "renderer" :display :png)))
      (check-that "PNG signature"
                  (equalp (subseq (a:read-file-into-byte-vector png) 0 4) #(137 80 78 71))))
    (let ((svg (emit-figure *test-figure* "renderer" :display :svg)))
      (check-that "SVG written" (search "<svg" (a:read-file-into-string svg))))
    (check-that "misspelt attribute reports Plotly's message"
                (handler-case (progn (emit-figure '(:data ((:type :scatter :linex 3))) "bad"
                                                  :display :html)
                                     nil)
                  (render-failure (c)
                    (search "Invalid property" (fincl-viz::render-failure-error-output c)))))))

(defun trace-named (figure name)
  (find name (getf figure :data) :key (lambda (trace) (getf trace :name)) :test #'equal))

(defun traces-on (figure xref)
  (remove xref (getf figure :data) :key (lambda (trace) (getf trace :xaxis)) :test-not #'equal))

(defun coordinates (sequence) (map 'list (lambda (x) (float x 1d0)) sequence))

(defun same-values-p (a b) (equal (coordinates a) (coordinates b)))

(defun merge-checks ()
  (format t "~&MERGE-SPEC~%")
  (check-equal "nested merge, override wins"
               '(:line (:color "red" :width 2) :opacity 0.5)
               (merge-spec '(:line (:color "red" :width 1)) '(:line (:width 2) :opacity 0.5)))
  (check-equal "arrays replaced, not merged" '(:x (4 5)) (merge-spec '(:x (1 2 3)) '(:x (4 5))))
  (check-equal "NIL value overrides" '(:showlegend nil) (merge-spec '(:showlegend t) '(:showlegend nil)))
  (check-equal "keys matched by JSON name" '(:error-y (:width 3)) (merge-spec '(:error-y (:width 1)) '("error_y" (:width 3))))
  (check-equal "NIL override keeps base" '(:a 1) (merge-spec '(:a 1) nil)))

(defun rectangles-overlap-p (a b)
  (destructuring-bind ((ax0 ax1) (ay0 ay1)) a
    (destructuring-bind ((bx0 bx1) (by0 by1)) b
      (and (< ax0 bx1) (< bx0 ax1) (< ay0 by1) (< by0 ay1)))))

(defun grid-checks ()
  (format t "~&PANEL-GRID~%")
  (let* ((line (fincl-viz::line-trace #(0 1) #(0 1)))
         (figure (panel-grid (iter (for i below 6) (collect (panel (list line (vline 0.5)))))
                             :columns 3 :title "grid"))
         (layout (getf figure :layout))
         (cells (iter (for k from 1 to 6)
                      (let ((suffix (if (= k 1) "" (princ-to-string k))))
                        (collect `(,(getf (getf layout (a:make-keyword (format nil "XAXIS~A" suffix))) :domain)
                                   ,(getf (getf layout (a:make-keyword (format nil "YAXIS~A" suffix))) :domain)))))))
    (check-that "six pairs of axes" (every (lambda (c) (and (first c) (second c))) cells))
    (check-that "anchors pair x with y"
                (equal "y3" (getf (getf layout :xaxis3) :anchor)))
    (check-that "domains inside [0, 1]"
                (every (lambda (c) (every (lambda (d) (and (<= 0 (first d)) (< (first d) (second d)) (<= (second d) 1))) c)) cells))
    (check-that "domains do not overlap"
                (iter outer (for (a . rest) on cells)
                      (iter (for b in rest)
                            (in outer (never (rectangles-overlap-p a b))))))
    (check-that "each trace on its panel's axes"
                (every (lambda (trace) (equal (subseq (getf trace :xaxis) 1) (subseq (getf trace :yaxis) 1)))
                       (getf figure :data)))
    (check-equal "one trace per panel (unnamed vlines add none)" 6 (length (getf figure :data)))
    (check-equal "vlines become shapes" 6 (length (getf layout :shapes)))
    (check-equal "a shape spans its panel's y domain" "y4 domain" (getf (aref (getf layout :shapes) 3) :yref))
    (check-equal "panel legends" "legend6" (getf (aref (getf figure :data) 5) :legend)))
  (let* ((surface (surface-trace #(0 1) #(0 1) #2A((0 1) (1 2))))
         (line (fincl-viz::line-trace #(0 1) #(0 1)))
         (figure (panel-grid `(,@(iter (repeat 3) (collect (panel (list surface))))
                               ,@(iter (repeat 3) (collect (panel (list line)))))
                             :columns 3))
         (layout (getf figure :layout))
         (data (getf figure :data)))
    (check-that "three scenes" (and (getf layout :scene) (getf layout :scene2) (getf layout :scene3)))
    (check-that "three pairs of axes, numbered from 1" (and (getf layout :xaxis) (getf layout :xaxis3) (not (getf layout :xaxis4))))
    (check-equal "surfaces on scenes" '("scene" "scene2" "scene3")
                 (map 'list (lambda (trace) (getf trace :scene)) (subseq data 0 3)))
    (check-equal "lines on axes" '("x" "x2" "x3")
                 (map 'list (lambda (trace) (getf trace :xaxis)) (subseq data 3)))
    (check-that "colorbar right of its scene, left of the next"
                (let ((x (getf (getf (aref data 0) :colorbar) :x))
                      (scene-1 (getf (getf (getf layout :scene) :domain) :x))
                      (scene-2 (getf (getf (getf layout :scene2) :domain) :x)))
                  (< (second scene-1) x (first scene-2))))))

(defparameter *viz-market*
  (make-bs-market :valuation-date (make-date 2026 9 24) :spot 100 :rate 0.05d0 :vol 0.2d0))

(defun simulation-plot-checks ()
  (format t "~&Simulation plots~%")
  (let ((m *viz-market*) (heston (make-heston)))
    (multiple-value-bind (times states) (simulate heston m :n-paths 5 :n-steps 20)
      (let* ((figure (paths-figure heston m :n-paths 5 :n-steps 20))
             (ys (state-component states 0))
             (paths (remove "sample mean" (remove "forward" (getf figure :data) :key (lambda (tr) (getf tr :name)) :test #'equal)
                            :key (lambda (tr) (getf tr :name)) :test #'equal)))
        (check-equal "a line per path" 5 (length paths))
        (check-that "path lines are the simulated states"
                    (iter (for trace in-vector paths) (for i from 0)
                          (always (and (equalp times (getf trace :x))
                                       (equalp (fincl-viz::matrix-column ys i) (getf trace :y))))))
        (check-that "mean is ROW-MEANS" (equalp (fincl-viz::row-means ys) (getf (trace-named figure "sample mean") :y)))
        (check-that "forward is FORWARD-CURVE" (equalp (fincl-viz::forward-curve m times) (getf (trace-named figure "forward") :y)))
        (check-equal "one legend entry for the paths" 0
                     (count-if (lambda (tr) (getf tr :showlegend)) paths)))
      (let ((figure (paths-figure heston m :n-paths 5 :n-steps 20 :component 1 :kind :scatter)))
        (check-that "variance as dots, no forward"
                    (and (not (trace-named figure "forward"))
                         (eq :markers (getf (aref (getf figure :data) 0) :mode))
                         (= 105 (length (getf (aref (getf figure :data) 0) :y)))))))
    (multiple-value-bind (figure mean forward) (terminal-figure (make-gbm) m :n-paths 2000 :n-steps 1 :bins 40)
      (let* ((bars (trace-named figure "simulated"))
             (centers (getf bars :x)) (heights (getf bars :y)) (width (getf bars :width))
             (sample (fincl-viz::terminal-sample (make-gbm) m 1d0 1 2000 42 :counter)))
        (check-equal "BINS bins" 40 (length heights))
        (check-that "heights integrate to 1" (< (abs (- 1 (* width (reduce #'+ heights)))) 1d-12))
        (check-that "bins span [min, max]"
                    (and (< (abs (- (aref centers 0) (+ (reduce #'min sample) (/ width 2)))) 1d-9)
                         (< (abs (- (aref centers 39) (- (reduce #'max sample) (/ width 2)))) 1d-9)))
        (check-that "forward marked" (= 1 (length (getf (getf figure :layout) :shapes))))
        (check-that "mean near forward" (< (abs (- mean forward)) 1))))
    (let ((option (make-option (call 100) (expires (make-date 2027 9 24)))))
      (multiple-value-bind (figure results) (convergence-figure option m :path-counts '(1000 4000) :n-steps 5
                                                                         :reference 10.45d0)
        (let ((trace (trace-named figure "MC, 2 SE")))
          (check-that "same prices as PRICE"
                      (equal results
                             (mapcar (lambda (n) (multiple-value-bind (v se)
                                                     (price option (make-engine :mc :process (make-gbm) :n-paths n :n-steps 5 :seed 42) m)
                                                   `(,n ,v ,se)))
                                     '(1000 4000))))
          (check-that "error bars are 2 SE"
                      (equal (getf (getf trace :error-y) :array) (mapcar (lambda (r) (* 2 (third r))) results)))
          (check-equal "log axis" "log" (getf (getf (getf figure :layout) :xaxis) :type))
          (check-that "reference line" (trace-named figure "reference")))))
    (let ((figure (log-returns-figure (make-garch) m)))
      (check-that "two panels sharing time"
                  (and (getf (getf figure :layout) :xaxis2)
                       (equal "x" (getf (getf (getf figure :layout) :xaxis2) :matches)))))))

(defun configuration-checks ()
  (format t "~&Configuration~%")
  (let* ((figure (panel-grid (list (panel (list (fincl-viz::line-trace #(0 1) #(0 1) :name "a")
                                                (fincl-viz::line-trace #(0 1) #(1 0) :name "b"))))))
         (base (configure-figure figure)))
    (check-that "no options, no change"
                (and (equalp (getf figure :data) (getf base :data))
                     (equal (getf figure :layout) (getf base :layout))))
    (let ((c (configure-figure figure :layout '(:yaxis (:type "log")))))
      (check-equal ":layout merged" "log" (getf (getf (getf c :layout) :yaxis) :type))
      (check-that ":layout keeps the rest"
                  (equal (getf (getf (getf figure :layout) :yaxis) :domain)
                         (getf (getf (getf c :layout) :yaxis) :domain)))
      (check-that ":layout leaves traces" (equalp (getf base :data) (getf c :data))))
    (check-equal ":config" '(:scrollzoom t) (getf (configure-figure figure :config '(:scrollzoom t)) :config))
    (let* ((c (configure-figure figure :style '(("b" :line (:color "green")))))
           (data (getf c :data)))
      (check-equal ":style by name" "green" (getf (getf (aref data 1) :line) :color))
      (check-that ":style leaves other traces" (equal (aref data 0) (aref (getf base :data) 0))))
    (check-that ":style :all"
                (every (lambda (tr) (= 0.3 (getf tr :opacity)))
                       (getf (configure-figure figure :style '((:all :opacity 0.3))) :data)))
    (check-equal ":title" "T" (getf (getf (getf (configure-figure figure :title "T") :layout) :title) :text)))
  (let ((*output-directory* #p"/tmp/fincl-viz-tests/"))
    (check-equal ":display and :name" "json"
                 (pathname-type (plot-series '(0 1) '(("a" (0 1))) :name "config" :display :json)))
    (let ((json (a:read-file-into-string #p"/tmp/fincl-viz-tests/config.json")))
      (check-that "theme under the figure" (search "\"colorway\"" json)))))

(defun calibration-checks ()
  (format t "~&Calibration figure~%")
  (let* ((market (fincl-viz::calibration-market))
         (truth (fincl-viz::model-surface fincl-viz::*true-model* market))
         (guess (fincl-viz::model-surface fincl-viz::*initial-guess* market))
         (figure (fincl-viz::calibration-stage-figure "stage" truth guess "Guess" market
                                                      fincl-viz::*initial-guess*))
         (data (getf figure :data))
         (layout (getf figure :layout))
         (surfaces (remove :surface data :key (lambda (tr) (getf tr :type)) :test-not #'eq)))
    (check-that "three scenes, three 2-D panels"
                (and (= 3 (length surfaces)) (getf layout :scene3) (getf layout :xaxis3) (not (getf layout :xaxis4))))
    (check-that "truth and model share their range"
                (and (= (getf (aref surfaces 0) :cmin) (getf (aref surfaces 1) :cmin))
                     (= (getf (aref surfaces 0) :cmax) (getf (aref surfaces 1) :cmax))))
    (check-that "error surface symmetric about 0"
                (= (getf (aref surfaces 2) :cmin) (- (getf (aref surfaces 2) :cmax))))
    (check-that "error surface is model minus truth, in percent"
                (< (abs (- (first (first (getf (aref surfaces 2) :z)))
                           (* 100 (- (first (first guess)) (first (first truth))))))
                   1d-9))
    (check-equal "camera" (camera-eye 25 -125) (getf (getf (getf layout :scene) :camera) :eye))
    (check-equal "smiles" 6 (count-if (lambda (tr) (getf tr :xaxis)) data))))

(defun pricing-checks ()
  (format t "~&Pricing figures~%")
  (let* ((figure (fincl-viz::greeks-vs-spot-figure))
         (m (fincl-viz::demo-market))
         (spots (fincl-viz::grid 60 140))
         (markets (fincl-viz::scenario-markets m '(:spot :spx) spots 100))
         (call (second (first (fincl-viz::demo-options)))))
    (check-that "six panels" (getf (getf figure :layout) :xaxis6))
    (check-that "delta series is GREEKS' delta"
                (same-values-p (fincl-viz::greek-on call :black-scholes markets :delta)
                               (getf (find "European call" (traces-on figure "x") :key (lambda (tr) (getf tr :name)) :test #'equal) :y))))
  (let ((figure (fincl-viz::smiles-figure)))
    (check-equal "two panels of four smiles" '(4 4)
                 (list (length (traces-on figure "x")) (length (traces-on figure "x2"))))))

(defparameter *all-plots*
  `(("series" ,(lambda (&rest k) (apply #'plot-series '(0 1 2) '(("a" (0 1 4)) ("b" (0 2 3))) k)))
    ("array" ,(lambda (&rest k) (apply #'plot-array (fincl-viz::random-uniforms 50) :kind :bar k)))
    ("panels" ,(lambda (&rest k) (apply #'plot-panels '((:xs (0 1) :series (("a" (0 1))) :title "A")
                                                        (:xs (0 1) :series (("b" (1 0))) :title "B"))
                                        k)))
    ("paths" ,(lambda (&rest k) (apply #'plot-paths (make-heston) *viz-market* :n-steps 50 k)))
    ("engine-paths" ,(lambda (&rest k) (apply #'plot-engine-paths
                                              (make-engine :mc :process (make-garch) :n-paths 20 :n-steps 252)
                                              *viz-market* k)))
    ("terminal" ,(lambda (&rest k) (apply #'plot-terminal (make-heston :rho -0.8d0) *viz-market* :n-steps 50 k)))
    ("log-returns" ,(lambda (&rest k) (apply #'plot-log-returns (make-garch) *viz-market* k)))
    ("convergence" ,(lambda (&rest k) (let ((o (make-option (call 100) (expires (make-date 2027 9 24)))))
                                        (apply #'plot-convergence o *viz-market* :path-counts '(1000 4000 16000)
                                               :reference (price o :black-scholes *viz-market*) k))))
    ("price-vs-inputs" ,(lambda (&rest k) (apply #'fincl-viz::plot-price-vs-inputs k)))
    ("greeks-vs-spot" ,(lambda (&rest k) (apply #'fincl-viz::plot-greeks-vs-spot k)))
    ("greeks-vs-expiry" ,(lambda (&rest k) (apply #'fincl-viz::plot-greeks-vs-expiry k)))
    ("smiles" ,(lambda (&rest k) (apply #'fincl-viz::plot-smiles k)))
    ("call-price-surface" ,(lambda (&rest k) (apply #'fincl-viz::plot-call-price-surface :vol 0.3d0
                                                    :layout '(:height 700) k)))
    ("calibration-stage"
     ,(lambda (&rest k)
        (let ((market (fincl-viz::calibration-market)))
          (apply #'fincl-viz::plot-calibration-stage "Initial guess"
                 (fincl-viz::model-surface fincl-viz::*true-model* market)
                 (fincl-viz::model-surface fincl-viz::*initial-guess* market)
                 "Initial guess" market fincl-viz::*initial-guess* k)))))
  "(name function): FUNCTION calls a PLOT- function with its keyword arguments.")

(defun all-plots-checks (&key (displays '(:json :html :png)) (directory #p"/tmp/fincl-viz-tests/plots/"))
  (format t "~&Every plot~%")
  (let ((*output-directory* directory))
    (dolist (entry *all-plots*)
      (destructuring-bind (name function) entry
        (dolist (display displays)
          (let ((path (funcall function :name name :display display)))
            (check-that (format nil "~A as ~(~A~)" name display)
                        (and (probe-file path)
                             (string-equal (pathname-type path) (string-downcase display))))))))))

(defun run-viz-tests ()
  "Run every fincl-viz check; returns T when all pass."
  (setf *failures* 0)
  (json-checks)
  (renderer-checks)
  (merge-checks)
  (grid-checks)
  (configuration-checks)
  (simulation-plot-checks)
  (pricing-checks)
  (calibration-checks)
  (all-plots-checks)
  (format t "~&~:[~D failure~:P~;All fincl-viz checks passed~*~]~%" (zerop *failures*) *failures*)
  (zerop *failures*))
