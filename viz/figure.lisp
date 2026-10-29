;;;; viz/figure.lisp --- building Plotly figures from traces and panels
;;;;
;;;; Nothing here knows about fincl. A trace is a plist of Plotly trace
;;;; attributes; a panel is the traces of one pair of axes or one 3-D scene;
;;;; PANEL-GRID lays panels out as one figure (:data traces :layout plist).
;;;; Configuration is MERGE-SPEC throughout: *THEME* under every figure, the
;;;; plot's own layout over it, and the user's :LAYOUT, :CONFIG and :STYLE on
;;;; top (DEFINE-PLOT).

(in-package #:fincl-viz)

;;; --------------------------------------------------------------------
;;; Merging
;;; --------------------------------------------------------------------

(defun spec-get (plist key)
  "The value of KEY in PLIST, keys compared by their JSON names (:error-y and
\"error_y\" are one key); second value T if KEY is present."
  (iter (for (k v) in-plist plist)
        (when (string= (json-key k) (json-key key))
          (return (values v t)))
        (finally (return (values nil nil)))))

(defun merge-spec (base override)
  "BASE with OVERRIDE merged over it. Where both are plists (JSON objects, see
viz/json.lisp) their keys are merged recursively, BASE's first and OVERRIDE's
new keys after; any other value of OVERRIDE, arrays and NIL included, replaces
BASE's. A NIL OVERRIDE as a whole leaves BASE.

  (merge-spec '(:line (:color \"red\" :width 1)) '(:line (:width 2) :opacity 0.5))
  => (:LINE (:COLOR \"red\" :WIDTH 2) :OPACITY 0.5)"
  (if (and (json-object-p base) (json-object-p override))
      (append (iter (for (k v) in-plist base)
                    (multiple-value-bind (w present) (spec-get override k)
                      (appending `(,k ,(cond ((not present) v)
                                             ((and (json-object-p v) (json-object-p w))
                                              (merge-spec v w))
                                             (t w))))))
              (iter (for (k v) in-plist override)
                    (unless (nth-value 1 (spec-get base k))
                      (appending `(,k ,v)))))
      (or override base)))

;;; --------------------------------------------------------------------
;;; Theme
;;; --------------------------------------------------------------------

(defparameter *theme*
  '(:layout (:width 1000 :height 600
             :font (:family "Helvetica, Arial, sans-serif" :size 13 :color "#2b2b2b")
             :paper-bgcolor "white" :plot-bgcolor "white"
             :colorway #("#2a78d6" "#eb6834" "#2f9e6e" "#8a5cd1" "#d6a32a"
                         "#e34948" "#3fb5c4" "#7a6f63")
             :title (:x 0.5 :xanchor "center" :font (:size 16))
             :legend (:bgcolor "rgba(255,255,255,0.75)")
             :template (:layout (:xaxis (:gridcolor "#e6e6e6" :linecolor "#8c8c8c" :showline t
                                         :zeroline nil :ticks "outside" :automargin t)
                                 :yaxis (:gridcolor "#e6e6e6" :linecolor "#8c8c8c" :showline t
                                         :zeroline nil :ticks "outside" :automargin t))))
    :config (:displaylogo nil)
    :colors (:truth "#2a78d6" :model "#eb6834" :mean "#1f1f1f" :forward "#e34948"
             :reference "#e34948" :secondary "#8c8c8c")
    :colorscales (:sequential ((0 "#cde2fb") (0.2 "#86b6ef") (0.4 "#3987e5")
                               (0.6 "#256abf") (0.8 "#184f95") (1 "#0d366b"))
                  :diverging ((0 "#2a78d6") (0.5 "#f0efec") (1 "#e34948"))))
  "Presentation shared by every figure. :LAYOUT is merged under each figure's
layout and :CONFIG under its config; :COLORS and :COLORSCALES are the named
colours and Plotly colorscales plots refer to through THEME-COLOR and
THEME-COLORSCALE. Restyle every plot with one SETF, e.g.

  (setf (getf (getf *theme* :colors) :truth) \"black\")")

(defun theme-entry (section key)
  (or (getf (getf *theme* section) key)
      (signal-invalid-argument 'key key 'keyword
                               (format nil "one of ~{~S~^, ~}"
                                       (iter (for (k nil) in-plist (getf *theme* section))
                                             (collect k))))))

(defun theme-color (key)
  "The colour *THEME* names KEY, e.g. (theme-color :truth)."
  (theme-entry :colors key))

(defun theme-colorscale (key)
  "The Plotly colorscale *THEME* names KEY: :SEQUENTIAL or :DIVERGING."
  (theme-entry :colorscales key))

(defun themed (figure)
  "FIGURE over *THEME*'s layout and config."
  (destructuring-bind (&key data layout config) figure
    `(:data ,data
      :layout ,(merge-spec (getf *theme* :layout) layout)
      :config ,(merge-spec (getf *theme* :config) config))))

;;; --------------------------------------------------------------------
;;; Traces
;;; --------------------------------------------------------------------

(defun legend-entry (name)
  "A named trace is in the legend; an unnamed one is not."
  (if name `(:name ,name) '(:showlegend nil)))

(defun line-trace (xs ys &key name style)
  "A line through the points (XS, YS). STYLE, a plist of Plotly scatter
attributes, is merged over the defaults.

  (line-trace #(0 1 2) #(1 4 9) :name \"squares\" :style '(:line (:dash \"dash\")))"
  (merge-spec `(:type :scatter :mode :lines :x ,xs :y ,ys ,@(legend-entry name)) style))

(defun scatter-trace (xs ys &key name style)
  "A dot at each point (XS, YS)."
  (merge-spec `(:type :scatter :mode :markers :x ,xs :y ,ys :marker (:size 4)
                ,@(legend-entry name))
              style))

(defun bar-trace (xs ys &key name style)
  "A bar of height YS at each of XS."
  (merge-spec `(:type :bar :x ,xs :y ,ys ,@(legend-entry name)) style))

(defun error-trace (xs ys errors &key name style)
  "Markers at (XS, YS) joined by a line, with symmetric error bars of
half-length ERRORS."
  (merge-spec `(:type :scatter :mode :lines+markers :x ,xs :y ,ys
                :error-y (:type :data :array ,errors :visible t :width 4)
                ,@(legend-entry name))
              style))

(defun surface-trace (xs ys z &key name style)
  "A 3-D surface over the grid XS x YS: Z has a row per element of YS and a
column per element of XS (a list of lists or a 2-D array)."
  (merge-spec `(:type :surface :x ,xs :y ,ys :z ,z ,@(when name `(:name ,name))) style))

(defun vline (x &key name style)
  "A vertical line at X across its panel's height, drawn as a layout shape
by PANEL-GRID; with NAME it also has a legend entry. STYLE may hold :LINE.

  (vline forward :name \"forward\" :style '(:line (:dash \"dot\")))"
  (merge-spec `(:type :vline :x ,x ,@(when name `(:name ,name))) style))

(defun hline (y &key name style)
  "A horizontal line at Y across its panel's width (see VLINE)."
  (merge-spec `(:type :hline :y ,y ,@(when name `(:name ,name))) style))

(defun matrix-column (matrix j)
  (let ((column (make-array (array-dimension matrix 0)
                            :element-type (array-element-type matrix))))
    (dotimes (i (length column) column)
      (setf (aref column i) (aref matrix i j)))))

(defun matrix-traces (xs matrix &key name style)
  "A line per column of MATRIX (a row per element of XS), all in one legend
group: with NAME the group has a single legend entry, the first line's."
  (iter (for j below (array-dimension matrix 1))
        (collect (merge-spec (line-trace xs (matrix-column matrix j) :name name)
                             (merge-spec `(:legendgroup ,(or name "matrix")
                                           :showlegend ,(and name (zerop j) t))
                                         style)))))

(defun histogram-density (xs bins)
  "BINS equal-width bins over [min, max] of the sample XS: (values centers
heights width), as double vectors and a double, heights normalised so that
the sum of height x width is 1."
  (let* ((n (length xs))
         (lo (float (reduce #'min xs) 1d0))
         (hi (float (reduce #'max xs) 1d0))
         (width (if (< lo hi) (/ (- hi lo) bins) 1d0))
         (counts (make-array bins :initial-element 0)))
    (map nil (lambda (x)
               (incf (aref counts (min (1- bins) (floor (- x lo) width)))))
         xs)
    (values (iter (for k below bins)
                  (fincl::collect-double (+ lo (* (+ k 1/2) width)) into centers length bins)
                  (finally (return centers)))
            (map '(vector double-float) (lambda (c) (/ c (* n width))) counts)
            width)))

;;; --------------------------------------------------------------------
;;; Panels and grids
;;; --------------------------------------------------------------------

(defun axis-title (label)
  (when label `(:title (:text ,label))))

(defun panel (traces &key title xlabel ylabel zlabel xaxis yaxis scene)
  "The traces of one pair of axes, or of one 3-D scene when a trace is 3-D
(a :SURFACE). XLABEL, YLABEL and ZLABEL title the axes; XAXIS, YAXIS and
SCENE are Plotly axis and scene attributes merged over them.

  (panel (list (line-trace xs ys :name \"delta\")) :title \"Delta\" :xlabel \"spot\")"
  `(:traces ,traces :title ,title
    :xaxis ,(merge-spec (axis-title xlabel) xaxis)
    :yaxis ,(merge-spec (axis-title ylabel) yaxis)
    :scene ,(merge-spec `(,@(when xlabel `(:xaxis ,(axis-title xlabel)))
                          ,@(when ylabel `(:yaxis ,(axis-title ylabel)))
                          ,@(when zlabel `(:zaxis ,(axis-title zlabel))))
                        scene)))

(defun panel-3d-p (panel)
  (some (lambda (trace) (member (getf trace :type) '(:surface :scatter3d :mesh3d)))
        (getf panel :traces)))

(defun numbered (prefix k)
  "PREFIX for the first axis, legend or scene, PREFIX followed by K after it."
  (if (= k 1) prefix (format nil "~A~D" prefix k)))

(defun layout-key (name)
  (a:make-keyword (string-upcase name)))

(defun line-shape (trace xref yref)
  "The layout shape of a VLINE or HLINE pseudo-trace on axes XREF, YREF."
  (let ((line (getf trace :line)))
    (ecase (getf trace :type)
      (:vline `(:type "line" :xref ,xref :x0 ,(getf trace :x) :x1 ,(getf trace :x)
                :yref ,(format nil "~A domain" yref) :y0 0 :y1 1 ,@(when line `(:line ,line))))
      (:hline `(:type "line" :xref ,(format nil "~A domain" xref) :x0 0 :x1 1
                :yref ,yref :y0 ,(getf trace :y) :y1 ,(getf trace :y) ,@(when line `(:line ,line)))))))

(defun panel-grid (panels &key (columns (min 2 (length panels))) title
                               (cell-width (if (rest panels) 560 1000))
                               (cell-height (if (rest panels) 400 600)))
  "One figure of PANELS (see PANEL) in a grid of COLUMNS, filled by rows;
each cell is CELL-WIDTH x CELL-HEIGHT pixels. A 2-D panel gets its own axes
(x, y; x2, y2; ...) and, when there are several panels, its own legend in its
top-left corner; a 3-D panel gets its own scene, with each surface's colorbar
beside it. VLINE and HLINE pseudo-traces become layout shapes. A single
panel's title is the figure's when TITLE is NIL.

  (panel-grid (list (panel (list (line-trace xs deltas)) :title \"Delta\")
                    (panel (list (line-trace xs gammas)) :title \"Gamma\"))
              :columns 2 :title \"Greeks\")"
  (let* ((n (length panels))
         (single (= n 1))
         (rows (ceiling n columns))
         (title (or title (and single (getf (first panels) :title))))
         (hgap (if single 0 (/ 90 (* columns cell-width))))
         (vgap (if single 0 (/ 110 (* rows cell-height))))
         (colorbar-room (/ 90 (* columns cell-width)))
         (w (/ (- 1 (* (1- columns) hgap)) columns))
         (h (/ (- 1 (* (1- rows) vgap)) rows))
         (n-2d 0) (n-3d 0)
         (data '()) (layout '()) (shapes '()) (annotations '()))
    (iter (for panel in panels)
          (for i from 0)
          (let* ((x0 (* (mod i columns) (+ w hgap))) (x1 (+ x0 w))
                 (y1 (- 1 (* (floor i columns) (+ h vgap)))) (y0 (- y1 h)))
            (if (panel-3d-p panel)
                (let* ((scene (numbered "scene" (incf n-3d)))
                       (scene-x1 (- x1 colorbar-room))
                       (placement `(:scene ,scene)))
                  (setf layout (append layout
                                       `(,(layout-key scene)
                                         ,(merge-spec `(:domain (:x (,x0 ,scene-x1) :y (,y0 ,y1)))
                                                      (getf panel :scene)))))
                  (dolist (trace (getf panel :traces))
                    (push (merge-spec (if (eq (getf trace :type) :surface)
                                          `(,@placement
                                            :colorbar (:x ,(+ scene-x1 0.01) :xanchor "left"
                                                       :y ,(/ (+ y0 y1) 2) :yanchor "middle"
                                                       :len ,(* 0.75 h) :thickness 14))
                                          placement)
                                      trace)
                          data)))
                (let* ((k (incf n-2d))
                       (xref (numbered "x" k)) (yref (numbered "y" k))
                       (legend (numbered "legend" k)))
                  (setf layout (append layout
                                       `(,(layout-key (numbered "xaxis" k))
                                         ,(merge-spec `(:domain (,x0 ,x1) :anchor ,yref)
                                                      (getf panel :xaxis))
                                         ,(layout-key (numbered "yaxis" k))
                                         ,(merge-spec `(:domain (,y0 ,y1) :anchor ,xref)
                                                      (getf panel :yaxis)))
                                       (unless single
                                         `(,(layout-key legend)
                                           (:x ,(+ x0 0.01) :y ,(- y1 0.01)
                                            :xanchor "left" :yanchor "top"
                                            :bgcolor "rgba(255,255,255,0.75)" :font (:size 11))))))
                  (let ((placement `(:xaxis ,xref :yaxis ,yref ,@(unless single `(:legend ,legend)))))
                    (dolist (trace (getf panel :traces))
                      (if (member (getf trace :type) '(:vline :hline))
                          (progn
                            (push (line-shape trace xref yref) shapes)
                            (when (getf trace :name)
                              (push (merge-spec `(:type :scatter :mode :lines :x #(:null) :y #(:null)
                                                  :name ,(getf trace :name)
                                                  ,@(when (getf trace :line) `(:line ,(getf trace :line))))
                                                placement)
                                    data)))
                          (push (merge-spec placement trace) data))))))
            (when (and (getf panel :title) (not single))
              (push `(:text ,(getf panel :title) :x ,(/ (+ x0 x1) 2) :y ,y1
                      :xref "paper" :yref "paper" :xanchor "center" :yanchor "bottom"
                      :yshift 4 :showarrow nil :font (:size 14))
                    annotations))))
    `(:data ,(coerce (nreverse data) 'vector)
      :layout (:width ,(* columns cell-width)
               :height ,(+ (* rows cell-height) (if title 60 0))
               ,@(when title `(:title (:text ,title)))
               :margin (:t ,(if title 90 40) :l 70 :r 40 :b 60)
               ,@layout
               :shapes ,(coerce (nreverse shapes) 'vector)
               :annotations ,(coerce (nreverse annotations) 'vector)))))

(defun camera-eye (elevation azimuth &optional (distance 2))
  "Plotly's scene.camera.eye for matplotlib's view_init(ELEVATION, AZIMUTH),
in degrees, at DISTANCE from the centre.

  (panel traces :scene `(:camera (:eye ,(camera-eye 25 -125))))"
  (let ((e (* elevation (/ pi 180))) (a (* azimuth (/ pi 180))))
    `(:x ,(* distance (cos e) (cos a))
      :y ,(* distance (cos e) (sin a))
      :z ,(* distance (sin e)))))

;;; --------------------------------------------------------------------
;;; Configuration and plot definitions
;;; --------------------------------------------------------------------

(defun configure-figure (figure &key layout config style title)
  "FIGURE with LAYOUT and CONFIG merged over its own, TITLE as its title, and
STYLE, an alist ((trace-name . plist) ...), merged into the traces of each
name; the name :ALL styles every trace.

  (configure-figure fig :style '((\"forward\" :line (:color \"green\"))))"
  (destructuring-bind (&key data ((:layout figure-layout)) ((:config figure-config))) figure
    `(:data ,(map 'vector
                  (lambda (trace)
                    (reduce (lambda (trace entry)
                              (destructuring-bind (name . plist) entry
                                (if (or (eq name :all) (equal name (getf trace :name)))
                                    (merge-spec trace plist)
                                    trace)))
                            style :initial-value trace))
                  data)
      :layout ,(merge-spec (merge-spec figure-layout layout)
                           (when title `(:title (:text ,title))))
      :config ,(merge-spec figure-config config))))

(defmacro define-plot (name lambda-list &body body)
  "Define NAME-FIGURE with LAMBDA-LIST (required and &key parameters), whose
BODY returns a figure and possibly further values, and PLOT-NAME, which takes
the same arguments plus these keys, emits the figure with EMIT-FIGURE and
returns the file's pathname followed by BODY's further values:

  :layout plist  merged over the figure's layout
  :config plist  merged over its Plotly config
  :style alist   ((trace-name . plist) ...) merged into the traces of each
                 name; the name :all styles every trace
  :title string  the figure's title
  :name string   the file name, by default NAME in lower case
  :display       *DISPLAY* for this call

  (define-plot squares (n)
    \"The squares of 0 .. N-1.\"
    (let ((xs (alexandria:iota n)))
      (panel-grid (list (panel (list (line-trace xs (mapcar (lambda (x) (* x x)) xs)
                                                 :name \"x^2\")))))))
  (plot-squares 10 :layout '(:yaxis (:type \"log\")) :display :html)"
  (multiple-value-bind (required optional rest) (a:parse-ordinary-lambda-list lambda-list)
    (when (or optional rest)
      (signal-invalid-argument 'lambda-list lambda-list 'list
                               "required and &key parameters only"))
    (let ((figure-function (a:symbolicate name '#:-figure))
          (plot-function (a:symbolicate '#:plot- name))
          (doc (and (stringp (first body)) (rest body) (first body))))
      (a:with-gensyms (arguments layout config style title file display results)
        `(progn
           (defun ,figure-function ,lambda-list ,@body)
           (defun ,plot-function (,@required &rest ,arguments
                                  &key ((:layout ,layout)) ((:config ,config))
                                       ((:style ,style)) ((:title ,title))
                                       ((:name ,file) ,(string-downcase (symbol-name name)))
                                       ((:display ,display) *display*)
                                  &allow-other-keys)
             ,(format nil "~@[~A~%~%~]Emits ~(~A~)'s figure; DEFINE-PLOT gives the ~
keys :LAYOUT :CONFIG :STYLE :TITLE :NAME :DISPLAY."
                      doc figure-function)
             (let ((,results (multiple-value-list
                              (apply #',figure-function ,@required
                                     (a:remove-from-plist ,arguments :layout :config :style
                                                          :title :name :display)))))
               (values-list
                (cons (emit-figure (configure-figure (first ,results)
                                                     :layout ,layout :config ,config
                                                     :style ,style :title ,title)
                                   ,file :display ,display)
                      (rest ,results))))))))))
