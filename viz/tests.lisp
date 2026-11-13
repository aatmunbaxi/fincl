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
  #+sbcl (check-equal "NaN, infinities as null" "[null,null,null,1.0]"
                      (json (let ((inf sb-ext:double-float-positive-infinity))
                              (doubles (fincl::make-nan) inf (- inf) 1))))
  #-sbcl (format t "~&  skip NaN, infinities as null: no infinity constant outside SBCL~%")
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
         (figure (%panel-grid (iter (for i below 6) (collect (%panel (list line (vline 0.5)))))
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
         (figure (%panel-grid `(,@(iter (repeat 3) (collect (%panel (list surface))))
                               ,@(iter (repeat 3) (collect (%panel (list line)))))
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
    (let* ((figure (paths-figure heston m :n-paths 5 :n-dates 20))
           (lines (remove-if (lambda (tr) (member (getf tr :name) '("sample mean" "forward") :test #'equal))
                             (getf figure :data))))
      (check-equal "a line per path" 5 (length lines))
      (check-equal "a point per date, and the valuation date" 21 (length (getf (aref lines 0) :y)))
      (check-that "the forward is the market's"
                  (let ((fwd (trace-named figure "forward")))
                    (< (abs (- (aref (getf fwd :y) 19) (forward m (make-date 2027 9 24)))) 1d-9)))
      (check-equal "one legend entry for the paths" 1
                   (count-if (lambda (tr) (getf tr :showlegend)) lines))
      (check-that "variance has no forward"
                  (not (trace-named (paths-figure heston m :n-paths 5 :n-dates 20 :component :variance)
                                    "forward"))))
    (multiple-value-bind (figure mean forward) (terminal-figure (make-gbm) m :n-paths 2000 :bins 40)
      (let* ((bars (trace-named figure "simulated"))
             (centers (getf bars :x)) (heights (getf bars :y)) (width (getf bars :width)))
        (check-equal "BINS bins" 40 (length heights))
        (check-that "heights integrate to 1" (< (abs (- 1 (* width (reduce #'+ heights)))) 1d-12))
        (check-that "bins are equally spaced" (< (abs (- (- (aref centers 1) (aref centers 0)) width)) 1d-9))
        (check-that "forward marked" (= 1 (length (getf (getf figure :layout) :shapes))))
        (check-that "mean near forward" (< (abs (- mean forward)) 1))))
    (let ((figure (log-returns-figure heston m)))
      (check-that "two panels sharing time"
                  (and (getf (getf figure :layout) :xaxis2)
                       (equal "x" (getf (getf (getf figure :layout) :xaxis2) :matches)))))))

(defun plot-dates ()
  (list (make-date 2026 12 24) (make-date 2027 3 24) (make-date 2027 6 24) (make-date 2027 9 24)))

(defun sample-result (&key (keep '(:quantiles (:paths 6))) (n-paths 400))
  "A GBM run over four dates: the spot recorded as :SPOT, outputs :SPOT and :PNL."
  (let ((keep keep))
    (simulate ((make-gbm) *viz-market* (plot-dates) :n-paths n-paths :keep keep)
      (:state s :spot)
      (:every (date) (record :spot s))
      (:final (:spot s) (:pnl (- s 100))))))

(defun trace-names (figure)
  (map 'list (lambda (trace) (getf trace :name)) (getf figure :data)))

(defun same-doubles (a b)
  "True if the double vectors A and B are equal, NaNs (no data) matching."
  (and (= (length a) (length b))
       (every (lambda (x y) (or (and (fincl::nan-p x) (fincl::nan-p y)) (= x y))) a b)))

(defun layer-checks ()
  (format t "~&Layers and PLOT~%")
  (let* ((r (sample-result))
         (dates (fincl::simulation-dates r)))
    ;; An overlay is one panel with each layer's traces.
    (let* ((figure (plot-figure (paths r :n 4) (mean-line r :spot)))
           (data (getf figure :data)))
      (check-equal "overlay: 4 path traces and a mean line" 5 (length data))
      (check-that "all on the one pair of axes"
                  (every (lambda (trace) (equal "x" (getf trace :xaxis))) data))
      (check-that "path traces are the stored states, by date"
                  (and (equalp (getf (aref data 1) :y) (simulation-path-state r 1 :spot))
                       (equalp (getf (aref data 1) :x) (map 'vector #'fincl-viz::date-label dates))))
      (check-that "the mean is the recorded mean"
                  (same-doubles (getf (aref data 4) :y) (simulation-statistic r :spot :mean)))
      (check-equal "one legend entry for the paths" '(t nil nil nil)
                   (map 'list (lambda (trace) (getf trace :showlegend)) (subseq data 0 4))))
    ;; Bands.
    (let ((band-traces (getf (plot-figure (band r :spot :quantiles '(0.1d0 0.9d0))) :data)))
      (check-equal "a band is two traces, the upper filled to the lower" '("tonexty")
                   (remove nil (map 'list (lambda (trace) (getf trace :fill)) band-traces)))
      (check-that "band edges are the quantiles"
                  (and (same-doubles (getf (aref band-traces 0) :y) (simulation-statistic r :spot :quantile 0.1d0))
                       (same-doubles (getf (aref band-traces 1) :y) (simulation-statistic r :spot :quantile 0.9d0)))))
    ;; Histograms and markers.
    (let* ((figure (plot-figure (histogram r :pnl :bins 20 :var 0.95d0 :es 0.95d0)))
           (bars (trace-named figure "pnl")))
      (check-equal "bins" 20 (length (getf bars :y)))
      (check-equal "VaR and ES markers are shapes" 2 (length (getf (getf figure :layout) :shapes)))
      (check-that "the VaR marker is the 5% quantile of the P&L"
                  (< (abs (- (getf (aref (getf (getf figure :layout) :shapes) 0) :x0)
                             (simulation-quantile r :pnl 0.05d0)))
                     1d-9)))
    ;; Grids.
    (let ((figure (plot-figure (grid (panel (paths r :n 3)) (panel (statistic r :spot :statistic :std-dev))
                                     (panel (histogram r :spot))))))
      (check-that "three panels: three pairs of axes"
                  (and (getf (getf figure :layout) :xaxis3) (not (getf (getf figure :layout) :xaxis4)))))
    (check-equal "shared-x links the later panels' x axes" "x"
                 (getf (getf (getf (plot-figure (grid (panel (mean-line r :spot)) (panel (band r :spot))
                                                      :shared-x t))
                                   :layout)
                             :xaxis2)
                       :matches))
    (check-that "(plot r) draws the paths, the recorded name and the outputs"
                (let ((figure (plot-figure r)))
                  (and (getf (getf figure :layout) :xaxis4) (not (getf (getf figure :layout) :xaxis5)))))
    ;; Errors.
    (check-error "a time layer and a value layer in one panel"
                 (panel (mean-line r :spot) (histogram r :pnl)) incompatible-layers)
    (check-that "its report names both layers"
                (let ((text (handler-case (panel (mean-line r :spot) (histogram r :pnl))
                              (incompatible-layers (c) (princ-to-string c)))))
                  (and (search "mean of spot" text) (search "histogram" text :test (function char-equal)))))
    (let ((bare (sample-result :keep nil)))
      (check-error "paths of a result that kept none" (paths bare :n 3) missing-plot-data)
      (check-that "the report names the addition"
                  (search "This result kept no paths: add :keep (:paths 3) to the SIMULATE call that made it."
                          (handler-case (paths bare :n 3) (missing-plot-data (c) (princ-to-string c)))))
      (check-error "a band without quantiles" (band bare :spot) missing-plot-data)
      (check-error "a name that was not recorded" (mean-line r :nope) missing-plot-data)
      (check-error "an output that does not exist" (histogram r :nope) missing-plot-data)
      (check-that "a result with only outputs and records draws histograms and means"
                  (getf (plot-figure bare) :data)))
    (check-error "more paths than the limit" (paths r :n 31) too-many-paths)
    (let ((many (sample-result :keep '(:paths 40))))
      (check-error "a result that kept 40 paths needs :n" (paths many) too-many-paths)
      (check-equal "with :n 30 it draws 30" 30
                   (length (getf (plot-figure (paths many :n 30)) :data))))
    (check-error "an object with no layers" (plot-figure 42) invalid-argument)))

(defun first-trace (figure)
  (aref (getf figure :data) 0))

(defun path-colors (figure)
  "The line colours of FIGURE's path traces, without repeats."
  (remove-duplicates (map 'list #'line-color (remove "paths" (getf figure :data)
                                                      :key (lambda (trace) (getf trace :name))
                                                      :test-not #'equal))
                     :test #'equal))

(defun line-color (trace)
  (getf (getf trace :line) :color))

(defun style-checks ()
  (format t "~&Roles and styles~%")
  (let* ((r (sample-result))
         (truth (theme-color :truth)) (model (theme-color :model)) (secondary (theme-color :secondary)))
    (flet ((colors (figure) (map 'list #'line-color (getf figure :data))))
      (check-equal "the theme's role colour by default" `(,truth)
                   (remove-duplicates (colors (plot-figure (paths r :n 2))) :test #'equal))
      (check-equal "a role override in :style applies in every panel" `(,model)
                   (path-colors (plot-figure (grid (panel (paths r :n 2)) (panel (paths r :n 2) (mean-line r :spot)))
                                             :style '(:paths (:color :model)))))
      (check-equal "the other role is untouched" (theme-color :mean)
                   (line-color (trace-named (plot-figure (paths r :n 2) (mean-line r :spot)
                                                         :style '(:paths (:color :model)))
                                            "mean of spot")))
      (check-equal "a raw CSS colour works" "hotpink"
                   (line-color (first-trace (plot-figure (paths r :n 1) :style '(:paths (:color "hotpink"))))))
      (check-equal "with-theme changes the role for every plot inside" `(,secondary)
                   (remove-duplicates
                    (colors (with-theme '(:roles (:paths (:color :secondary))) (plot-figure (paths r :n 2))))
                    :test #'equal))
      (check-equal "and nothing outside it" `(,truth)
                   (progn (with-theme '(:roles (:paths (:color :secondary))) nil)
                          (remove-duplicates (colors (plot-figure (paths r :n 2))) :test #'equal)))
      (check-equal ":style beats with-theme" `(,model)
                   (remove-duplicates
                    (colors (with-theme '(:roles (:paths (:color :secondary)))
                              (plot-figure (paths r :n 2) :style '(:paths (:color :model)))))
                    :test #'equal))
      (check-equal "a panel's :style beats the plot's" `(,secondary)
                   (remove-duplicates
                    (colors (plot-figure (panel (paths r :n 2) :style '(:paths (:color :secondary)))
                                         :style '(:paths (:color :model))))
                    :test #'equal))
      (check-equal "a panel's :style leaves other panels alone" `(,model ,truth)
                   (colors (plot-figure (grid (panel (paths r :n 1) :style '(:paths (:color :model)))
                                              (panel (paths r :n 1))))))
      (check-equal "the layer's keyword beats :style" "black"
                   (line-color (first-trace (plot-figure (paths r :n 1 :color "black")
                                                         :style '(:paths (:color :model))))))
      (check-equal "a layer name in :style beats its role" "green"
                   (line-color (first-trace (plot-figure (paths r :n 1 :name "fan")
                                                         :style '(:paths (:color :model) "fan" (:color "green"))))))
      (check-equal "width and opacity from the role" '(0.8 0.6)
                   (let ((trace (first-trace (plot-figure (paths r :n 1)))))
                     `(,(getf (getf trace :line) :width) ,(getf trace :opacity))))
      (check-equal "the layer's :raw is the last word" 0.1
                   (getf (first-trace (plot-figure (paths r :n 1 :raw '(:opacity 0.1))
                                                   :style '(:paths (:opacity 0.9))))
                         :opacity))
      (check-that "a band's fill takes the role's colour and opacity"
                  (search "rgba(42,120,214,0.5" 
                          (getf (aref (getf (plot-figure (band r :spot) :style '(:band (:opacity 0.5))) :data) 1)
                                :fillcolor))))
    (check-error "an unknown role in :style" (plot-figure (paths r :n 1) :style '(:path (:color :model))) unknown-role)
    (check-error "an unknown role in with-theme" (with-theme '(:roles (:fan (:color :model))) 1) unknown-role)
    (check-error "an unknown colour name" (plot-figure (paths r :n 1) :style '(:paths (:color :grey))) invalid-argument)
    (check-error "a style that is not a plist" (plot-figure (paths r :n 1) :style '(1 2 3)) invalid-argument)
    (check-error "make-layer with an unknown role" (make-layer :fan :time (lambda (style) style)) unknown-role)))

(defun path-color-checks ()
  (format t "~&Path colours~%")
  (let* ((r (sample-result))
         (palette '("#2a78d6" "#eb6834" "#2f9e6e"))
         (truth (theme-color :truth)))
    (check-equal "colours cycle through the list by path index"
                 '("#2a78d6" "#eb6834" "#2f9e6e" "#2a78d6" "#eb6834")
                 (mapcar #'line-color (coerce (getf (plot-figure (paths r :n 5 :colors palette)) :data) 'list)))
    (check-equal "path i has the same colour in every panel that uses the list"
                 '("#2a78d6" "#eb6834" "#2a78d6" "#eb6834")
                 (mapcar #'line-color
                         (coerce (getf (plot-figure (grid (panel (paths r :n 2 :colors palette))
                                                          (panel (paths r :n 2 :colors palette :dash "dot"))))
                                       :data)
                                 'list)))
    (check-equal "theme names resolve in a palette" `(,truth ,(theme-color :model))
                 (mapcar #'line-color
                         (coerce (getf (plot-figure (paths r :n 2 :colors '(:truth :model))) :data) 'list)))
    (check-equal "the palette overrides the style's colour" `(,(first palette))
                 (remove-duplicates
                  (mapcar #'line-color
                          (coerce (getf (plot-figure (paths r :n 1 :colors palette)
                                                     :style '(:paths (:color :model)))
                                        :data)
                                  'list))
                  :test #'equal))
    (check-equal "the other look keywords still apply to every path" '(0.4 0.4)
                 (mapcar (lambda (trace) (getf trace :opacity))
                         (coerce (getf (plot-figure (paths r :n 2 :colors palette :opacity 0.4)) :data) 'list)))
    (check-error "colours with a colour" (paths r :n 2 :color "black" :colors palette) invalid-argument)
    (check-error "a colour that is not a list" (paths r :n 2 :colors "black") invalid-argument)))

(defun plottable-checks ()
  (format t "~&Other plottables and the marginal layout~%")
  (let ((r (sample-result)))
    ;; Marginal.
    (let* ((figure (plot-figure (grid (panel (paths r :n 3))
                                      (panel (histogram r :spot :horizontal t :var 0.95d0))
                                      :marginal t)))
           (layout (getf figure :layout))
           (bars (trace-named figure "spot")))
      (check-equal "marginal: the histogram's bars are horizontal" "h" (getf bars :orientation))
      (check-equal "its y axis follows the first panel's" "y" (getf (getf layout :yaxis2) :matches))
      (check-that "and the right panel is narrow"
                  (let ((d1 (getf (getf layout :xaxis) :domain)) (d2 (getf (getf layout :xaxis2) :domain)))
                    (< (- (second d2) (first d2)) (- (second d1) (first d1)))))
      (check-equal "its VaR marker is a horizontal line" "y2"
                   (getf (aref (getf layout :shapes) 0) :yref)))
    (check-error "a marginal grid has two panels"
                 (grid (panel (paths r :n 1)) :marginal t) invalid-argument)
    (check-that "a reference line shares a panel with either kind of axis"
                (and (panel (mean-line r :spot) (reference-line 100))
                     (panel (histogram r :pnl) (reference-line 0 :axis :x))))))

(defun configuration-checks ()
  (format t "~&Configuration~%")
  (let* ((figure (%panel-grid (list (%panel (list (fincl-viz::line-trace #(0 1) #(0 1) :name "a")
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
                 (pathname-type (plot-paths (make-heston) *viz-market* :n-paths 2 :n-dates 5 :name "config" :display :json)))
    (let ((json (a:read-file-into-string #p"/tmp/fincl-viz-tests/config.json")))
      (check-that "theme under the figure" (search "\"colorway\"" json)))))

(defparameter *all-plots*
  `(("paths" ,(lambda (&rest k) (apply #'plot-paths (make-heston) *viz-market* k)))
    ("terminal" ,(lambda (&rest k) (apply #'plot-terminal (make-heston :rho -0.8d0) *viz-market* k)))
    ("log-returns" ,(lambda (&rest k) (apply #'plot-log-returns (make-heston) *viz-market* k)))
    ("layers-default" ,(lambda (&rest k) (apply #'plot (sample-result) k)))
    ("layers-marginal" ,(lambda (&rest k)
                          (let ((r (sample-result)))
                            (apply #'plot (grid (panel (paths r :n 6) (band r :spot))
                                                (panel (histogram r :spot :horizontal t))
                                                :marginal t)
                                   k)))))
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

(defun recorded-paths-checks ()
  (format t "~&Paths of recorded and held values~%")
  (flet ((run (keep)
           (simulate ((make-gbm) *viz-market* (plot-dates) :n-paths 200 :keep keep)
             (:accumulate (nav 1d0))
             (:hold (held-nav 1d0))
             (:state s :spot)
             (:every (date)
               (setf nav (* nav 1.01d0) held-nav nav)
               (record :balance (* nav s))))))
    (let* ((r (run '(:paths 5)))
           (by-record (plot-figure (paths r :record :balance :n 5)))
           (by-held (plot-figure (paths r :held :held-nav :n 5))))
      (check-equal "(paths r :record :balance) draws a trace per path" 5 (length (getf by-record :data)))
      (check-that "its values are the path's records"
                  (same-doubles (coerce (getf (first-trace by-record) :y) 'fincl:double-vector)
                                (simulation-path-record r 0 :balance)))
      (check-that "(paths r :held :held-nav) draws the held values"
                  (= 1.01d0 (aref (getf (first-trace by-held) :y) 1)))
      (check-that "both render"
                  (let ((*output-directory* #p"/tmp/fincl-viz-tests/plots/"))
                    (and (probe-file (plot (paths r :record :balance) :name "recorded-paths" :display :json))
                         (probe-file (plot (paths r :held :held-nav) :name "held-paths" :display :json)))))
      (check-error "both :record and :held" (paths r :record :balance :held :held-nav) invalid-argument))
    (let ((bare (run nil)))
      (check-error "recorded paths of a result that kept none" (paths bare :record :balance :n 3)
                   missing-plot-data)
      (check-that "the report names the :KEEP to add"
                  (search "add :keep (:paths 3)"
                          (handler-case (paths bare :record :balance :n 3)
                            (missing-plot-data (c) (princ-to-string c))))))))

(defun run-viz-tests ()
  "Run every fincl-viz check; returns T when all pass."
  (setf *failures* 0)
  (recorded-paths-checks)
  (json-checks)
  (renderer-checks)
  (merge-checks)
  (grid-checks)
  (configuration-checks)
  (layer-checks)
  (style-checks)
  (path-color-checks)
  (plottable-checks)
  (simulation-plot-checks)
  (all-plots-checks)
  (format t "~&~:[~D failure~:P~;All fincl-viz checks passed~*~]~%" (zerop *failures*) *failures*)
  (zerop *failures*))
