;;;; viz/plot.lisp --- PLOT, one entry point built from layers
;;;;
;;;; A layer is a small value meaning one thing (the paths of a simulation,
;;;; a recorded mean, a quantile band, a histogram of an output). PANEL puts
;;;; layers on one pair of axes, GRID lays panels out, and PLOT draws what it
;;;; is given:
;;;;
;;;;   (plot (paths r :n 30) (mean-line r :spot))             ; one panel
;;;;   (plot (grid (panel (paths r) (band r :spot))
;;;;               (panel (histogram r :pnl :var 0.99)))
;;;;         :style '(:paths (:color :secondary) :band (:opacity 0.3)))
;;;;   (plot r)                                               ; DEFAULT-LAYERS
;;;;
;;;; Layers read a SIMULATION-RESULT through its accessors and never simulate.
;;;; A layer's look comes from its role: *THEME*'s :ROLES entry, then WITH-THEME,
;;;; then the :STYLE of PLOT and of PANEL (keyed by role or by layer name), then
;;;; the layer's own keywords; the layer's :RAW (Plotly trace attributes,
;;;; MERGE-SPEC) comes last.

(in-package #:fincl-viz)

;;; --------------------------------------------------------------------
;;; Errors
;;; --------------------------------------------------------------------

(define-condition missing-plot-data (invalid-input)
  ((layer :initarg :layer :reader missing-plot-data-layer)
   (remedy :initarg :remedy :reader missing-plot-data-remedy))
  (:report (lambda (c s)
             (format s "~A cannot be drawn: ~A"
                     (missing-plot-data-layer c) (missing-plot-data-remedy c))))
  (:documentation "A layer needs data the object did not keep. The report says
what to add to the call that made it."))

(define-condition incompatible-layers (invalid-input)
  ((first-layer :initarg :first-layer :reader incompatible-layers-first)
   (second-layer :initarg :second-layer :reader incompatible-layers-second))
  (:report (lambda (c s)
             (let ((a (incompatible-layers-first c)) (b (incompatible-layers-second c)))
               (format s "~A (x axis: ~(~A~)) and ~A (x axis: ~(~A~)) cannot share a panel; ~
put them in separate panels of a GRID."
                       a (layer-x-kind a) b (layer-x-kind b)))))
  (:documentation "Layers on one panel with different kinds of x axis."))

(define-condition unknown-role (invalid-input)
  ((role :initarg :role :reader unknown-role-role))
  (:report (lambda (c s)
             (format s "~S is not a layer role; the roles are ~{~S~^, ~}."
                     (unknown-role-role c) *roles*)))
  (:documentation "A role outside *ROLES*."))

(define-condition too-many-paths (invalid-input)
  ((requested :initarg :requested :reader too-many-paths-requested)
   (limit :initarg :limit :reader too-many-paths-limit))
  (:report (lambda (c s)
             (format s "~D paths cannot be drawn at once (at most ~D): pass :N ~D or fewer to PATHS, ~
and keep no more than that with :KEEP (:PATHS k)."
                     (too-many-paths-requested c) (too-many-paths-limit c)
                     (too-many-paths-limit c))))
  (:documentation "A paths layer asked to draw more than *MAX-PLOTTED-PATHS* paths."))

;;; --------------------------------------------------------------------
;;; Roles and styles
;;; --------------------------------------------------------------------

(defparameter *roles*
  '(:paths :mean :band :statistic :histogram :reference :marker)
  "The roles a layer can have; *THEME*'s :ROLES gives each its look.")

(defparameter *max-plotted-paths* 30
  "The most paths one PATHS layer draws.")

(defun check-role (role)
  (unless (member role *roles*)
    (error 'unknown-role :role role))
  role)

(defun check-style (style)
  "STYLE, a plist keyed by role (a keyword of *ROLES*) or layer name (a
string), each value a plist of :COLOR :WIDTH :DASH :OPACITY."
  (unless (or (null style) (json-object-p style))
    (signal-invalid-argument 'style style 'list
                             "a plist keyed by role or layer name, e.g. (:paths (:color :secondary))"))
  (iter (for (key value) in-plist style)
        (when (keywordp key) (check-role key))
        (unless (or (null value) (json-object-p value))
          (signal-invalid-argument 'style value 'list
                                   (format nil "a plist of :color :width :dash :opacity under ~S" key))))
  style)

(defmacro with-theme (overrides &body body)
  "Run BODY with *THEME* merged under OVERRIDES, a plist as *THEME* (only the
entries to change); :ROLES keys must be roles.

  (with-theme '(:roles (:paths (:color \"black\")) :layout (:width 600))
    (plot r))"
  `(call-with-theme ,overrides (lambda () ,@body)))

(defun call-with-theme (overrides thunk)
  (check-style (getf overrides :roles))
  (let ((*theme* (merge-spec *theme* overrides)))
    (funcall thunk)))

(defvar *style-scopes* '()
  "The :STYLE plists in force, outermost (PLOT) first.")

(defun style-entry (style key)
  (iter (for (k v) in-plist style)
        (when (equal k key) (return v))))

(defun resolve-color (color)
  (if (keywordp color) (theme-color color) color))

(defun resolved-attributes (layer role)
  "ROLE's look for LAYER as a plist of :COLOR (a CSS string) :WIDTH :DASH
:OPACITY: the theme's, then each scope's entry for the role and, for the
layer's own role, its name, then the layer's keywords."
  (let ((own (eq role (layer-role layer)))
        (attributes (getf (getf *theme* :roles) role)))
    (dolist (scope *style-scopes*)
      (setf attributes (merge-spec attributes (style-entry scope role)))
      (when (and own (layer-name layer))
        (setf attributes (merge-spec attributes (style-entry scope (layer-name layer))))))
    (when own
      (setf attributes (merge-spec attributes (layer-attributes layer))))
    (let ((color (resolve-color (getf attributes :color))))
      (merge-spec attributes `(:color ,color)))))

;;; --------------------------------------------------------------------
;;; Layers
;;; --------------------------------------------------------------------

(defstruct (layer (:constructor %make-layer))
  "One thing to draw. ROLE is in *ROLES*; X-KIND is :TIME (an axis of dates),
:VALUE (numbers), :NONE (categories) or :ANY (a reference line, which suits
any axis); DATA-NEEDS lists what the layer asked of its
object (see CHECK-NEEDS); TRACES-FN takes a function (lambda (&optional role))
returning the resolved look of ROLE (the layer's own by default) and returns
the layer's traces. ATTRIBUTES are the layer's own :COLOR :WIDTH :DASH
:OPACITY, RAW Plotly attributes merged into each trace."
  (role :paths :type keyword :read-only t)
  (x-kind :time :type (member :time :value :none :any) :read-only t)
  (data-needs '() :type list :read-only t)
  (traces-fn nil :type function :read-only t)
  (name nil :type (or null string) :read-only t)
  (attributes '() :type list :read-only t)
  (raw '() :type list :read-only t)
  (xlabel nil :read-only t)
  (ylabel nil :read-only t))

(defmethod print-object ((l layer) stream)
  (print-unreadable-object (l stream :type t)
    (format stream "~(~S~)~@[ ~S~]" (layer-role l) (layer-name l))))

(defun make-layer (role x-kind traces-fn &key data-needs name attributes raw xlabel ylabel)
  "A layer of ROLE whose traces TRACES-FN makes (see LAYER); the way to add a
layer kind.

  (make-layer :reference :time
              (lambda (style)
                (let ((look (funcall style)))
                  `(,(hline 100 :style `(:line (:color ,(getf look :color)))))))
              :name \"strike\")"
  (check-role role)
  (%make-layer :role role :x-kind x-kind :traces-fn traces-fn :data-needs data-needs
               :name name :attributes attributes :raw raw :xlabel xlabel :ylabel ylabel))

(defun layer-traces (layer)
  "LAYER's traces, with each trace's Plotly :RAW attributes merged in."
  (mapcar (lambda (trace) (merge-spec trace (layer-raw layer)))
          (funcall (layer-traces-fn layer)
                   (lambda (&optional (role (layer-role layer)))
                     (resolved-attributes layer role)))))

(defun color-with-opacity (color opacity)
  "COLOR, a #rrggbb string, as an rgba() string of OPACITY; any other colour
as it is (Plotly's own trace opacity is then the only fade)."
  (if (and (stringp color) (= 7 (length color)) (char= #\# (char color 0)))
      (format nil "rgba(~D,~D,~D,~F)"
              (parse-integer color :start 1 :end 3 :radix 16)
              (parse-integer color :start 3 :end 5 :radix 16)
              (parse-integer color :start 5 :end 7 :radix 16)
              opacity)
      color))

(defun line-attributes (look &key (opacity t))
  "Plotly trace attributes of a line from the resolved LOOK."
  `(:line (:color ,(getf look :color) :width ,(or (getf look :width) 1)
           ,@(when (getf look :dash) `(:dash ,(getf look :dash))))
    ,@(when (and opacity (getf look :opacity)) `(:opacity ,(getf look :opacity)))))

;;; Needs of a SIMULATION-RESULT

(defun missing (layer remedy &rest arguments)
  (error 'missing-plot-data :layer layer :remedy (apply #'format nil remedy arguments)))

(defun check-needs (result layer-label needs)
  "Signal MISSING-PLOT-DATA, naming the addition to the SIMULATE call, unless
RESULT has everything in NEEDS: (:paths k), (:record name), (:quantiles name)
or (:output name)."
  (dolist (need needs)
    (destructuring-bind (kind &optional argument) need
      (ecase kind
        (:paths
         (let ((kept (simulation-kept-paths result)))
           (when (< kept argument)
             (missing layer-label "This result kept ~[no paths~:;~:*only ~D paths~]: add :keep (:paths ~D) ~
to the SIMULATE call that made it."
                      kept argument))))
        (:record
         (unless (member argument (simulation-recorded-names result) :test #'equal)
           (missing layer-label "This result recorded no ~S: add (record ~S value) to a clause of the SIMULATE ~
call that made it. It recorded ~:[nothing~;~:*~{~S~^, ~}~]."
                    argument argument (simulation-recorded-names result))))
        (:quantiles
         (unless (simulation-quantiles-p result argument)
           (missing layer-label "This result kept no values of ~S for quantiles: add :keep :quantiles to the ~
SIMULATE call that made it." argument)))
        (:output
         (unless (member argument (simulation-output-names result))
           (missing layer-label "This result has no output ~S: its outputs are ~:[none~;~:*~{~S~^, ~}~] ~
(the :FINAL clause of the SIMULATE call that made it)."
                    argument (simulation-output-names result))))))))

(defun date-label (date)
  (format nil "~4,'0D-~2,'0D-~2,'0D" (fincl::date-year date) (fincl::date-month date)
          (fincl::date-day date)))

(defun result-dates (result)
  (map 'vector #'date-label (simulation-dates result)))

(defun label-of (name)
  (string-downcase (princ-to-string name)))

(defmacro with-needs ((result label needs) &body body)
  "BODY after checking NEEDS of RESULT; LABEL names the layer in the report."
  `(progn (check-needs ,result ,label ,needs) ,@body))

;;; Constructors

(defun held-series (result path name)
  "The :HOLD variable NAME of stored PATH as a DOUBLE-VECTOR over the
simulation dates, NaN where the path had stopped; a value that is not a real
on a date the path ran signals."
  (map 'fincl:double-vector
       (lambda (plist)
         ;; A date the path ran stores a plist of every held variable.
         (let ((value (getf plist name)))
           (cond ((null plist) (fincl::quiet-nan))
                 ((realp value) (float value 1d0))
                 (t (fincl::signal-invalid-argument
                     'held value 'real
                     (format nil "a real held value of ~S on every date stored path ~D ran" name path))))))
       (fincl::kept-dates result path :held)))

(defun paths (result &key n state record held underlying name color width dash opacity raw)
  "The stored paths of RESULT (:KEEP (:PATHS k)) at the simulation dates,
for the first N paths, all of them by default (at most *MAX-PLOTTED-PATHS*):
state component STATE (of UNDERLYING, with several; default :SPOT), or the
values the path recorded under RECORD (see SIMULATION-PATH-RECORD), or the
:HOLD variable HELD (a keyword; reals on every date the path ran). At most
one of STATE, RECORD and HELD. Role :PATHS.

  (plot (paths r :n 20))
  (plot (paths r :record '(:book :value)))"
  (when (< 1 (count-if-not #'null `(,state ,record ,held)))
    (fincl::signal-invalid-argument 'paths `(:state ,state :record ,record :held ,held) 'list
                                    "at most one of :STATE, :RECORD and :HELD"))
  (let* ((kept (simulation-kept-paths result))
         (label (format nil "PATHS layer of ~S" result))
         (n (or n (and (plusp kept) kept) *max-plotted-paths*))
         (needs `((:paths ,n) ,@(when record `((:record ,record))))))
    (when (> n *max-plotted-paths*)
      (error 'too-many-paths :requested n :limit *max-plotted-paths*))
    (check-needs result label needs)
    (let ((dates (result-dates result))
          (name (or name "paths")))
      (flet ((series (i)
               (cond (record (simulation-path-record result i record))
                     (held (held-series result i held))
                     (t (simulation-path-state result i (or state :spot) underlying)))))
        (make-layer :paths :time
                    (lambda (style)
                      (let ((look (funcall style)))
                        (iter (for i below n)
                              (collect (line-trace dates (series i)
                                                   :name name
                                                   :style `(,@(line-attributes look)
                                                            :legendgroup ,name
                                                            :showlegend ,(zerop i)))))))
                    :data-needs needs :name name :ylabel (label-of (or record held state :spot))
                    :attributes (remove-nil-pairs `(:color ,color :width ,width :dash ,dash :opacity ,opacity))
                    :raw raw)))))

(defun remove-nil-pairs (plist)
  (iter (for (k v) in-plist plist)
        (when v (appending `(,k ,v)))))

(defun statistic (result name &key (statistic :mean) p (role :statistic) label color width dash
                                   opacity raw)
  "STATISTIC (:MEAN, :STD-DEV, :MIN, :MAX, :COUNT, or :QUANTILE with P, which
needs :KEEP :QUANTILES) of the values RESULT recorded under NAME with RECORD,
at each simulation date. Role :STATISTIC.

  (plot (statistic r :hedge-value :statistic :std-dev))"
  (let ((layer-label (format nil "STATISTIC layer ~S of ~S" name result))
        (needs `((:record ,name) ,@(when (eq statistic :quantile) `((:quantiles ,name))))))
    (check-role role)
    (check-needs result layer-label needs)
    (let ((dates (result-dates result))
          (values (simulation-statistic result name statistic p))
          (label (or label (format nil "~(~A~) of ~(~A~)" (if (eq statistic :quantile)
                                                              (format nil "~D% quantile" (round (* 100 p)))
                                                              statistic)
                                   (label-of name)))))
      (make-layer role :time
                  (lambda (style)
                    `(,(line-trace dates values :name label
                                                :style (line-attributes (funcall style)))))
                  :data-needs needs :name label :ylabel (label-of name)
                  :attributes (remove-nil-pairs `(:color ,color :width ,width :dash ,dash :opacity ,opacity))
                  :raw raw))))

(defun mean-line (result name &key label color width dash opacity raw)
  "The mean of the values RESULT recorded under NAME, by date: a STATISTIC
layer of role :MEAN.

  (plot (band r :spot) (mean-line r :spot))"
  (statistic result name :statistic :mean :role :mean :label label :color color :width width
                         :dash dash :opacity opacity :raw raw))

(defun band (result name &key (quantiles '(0.05d0 0.95d0)) label color opacity raw)
  "The area between two quantiles of the values RESULT recorded under NAME, by
date (needs :KEEP :QUANTILES). Role :BAND.

  (plot (band r :spot :quantiles '(0.25d0 0.75d0)))"
  (destructuring-bind (lo hi) quantiles
    (check-input lo (real 0 1) "a probability")
    (check-input hi (real 0 1) "a probability")
    (let ((layer-label (format nil "BAND layer ~S of ~S" name result))
          (needs `((:record ,name) (:quantiles ,name)))
          (label (or label (format nil "~D-~D% band" (round (* 100 lo)) (round (* 100 hi))))))
      (check-needs result layer-label needs)
      (let ((dates (result-dates result))
            (lower (simulation-statistic result name :quantile lo))
            (upper (simulation-statistic result name :quantile hi)))
        (make-layer :band :time
                    (lambda (style)
                      (let* ((look (funcall style))
                             (color (getf look :color))
                             (opacity (or (getf look :opacity) 0.2)))
                        `(,(line-trace dates lower :name label
                                                   :style `(:line (:width 0) :legendgroup ,label
                                                            :showlegend nil :hoverinfo "skip"))
                          ,(line-trace dates upper :name label
                                                   :style `(:line (:width 0) :fill "tonexty"
                                                            :fillcolor ,(color-with-opacity color opacity)
                                                            ,@(when (equal color (color-with-opacity color opacity))
                                                                `(:opacity ,opacity))
                                                            :legendgroup ,label)))))
                    :data-needs needs :name label :ylabel (label-of name)
                    :attributes (remove-nil-pairs `(:color ,color :opacity ,opacity))
                    :raw raw)))))

(defun histogram (result output &key (bins 50) var es horizontal label color opacity raw)
  "The density histogram of RESULT's per-path OUTPUT (a :FINAL name), BINS bins.
VAR and ES, confidence levels such as 0.99, add :MARKER lines at the value
at which the loss -OUTPUT reaches its value at risk and its expected
shortfall, so OUTPUT should be a P&L. HORIZONTAL draws the bars sideways, for
the right-hand panel of a :MARGINAL grid. Role :HISTOGRAM; the x axis is :VALUE.

  (plot (histogram r :hedged :var 0.99 :es 0.99))"
  (let ((layer-label (format nil "HISTOGRAM layer ~S of ~S" output result))
        (needs `((:output ,output))))
    (check-needs result layer-label needs)
    (let ((values (simulation-values result output))
          (label (or label (label-of output))))
      (make-histogram-layer values label bins :var var :es es :horizontal horizontal
                                              :needs needs :xlabel (label-of output)
                                              :color color :opacity opacity :raw raw))))

(defun make-histogram-layer (values label bins &key var es var-value es-value horizontal needs xlabel
                                                    color opacity raw)
  "A :HISTOGRAM layer of the sample VALUES, with :MARKER lines at VAR-VALUE and
ES-VALUE, or where the loss -VALUES reaches the VaR and ES at levels VAR and ES."
  (multiple-value-bind (centers heights width) (histogram-density values bins)
    (make-layer :histogram (if horizontal :none :value)
                (lambda (style)
                  (let* ((look (funcall style)) (marker (funcall style :marker))
                         (bars (bar-trace (if horizontal heights centers) (if horizontal centers heights)
                                          :name label
                                          :style `(:width ,width :opacity ,(or (getf look :opacity) 0.6)
                                                   ,@(when horizontal '(:orientation "h"))
                                                   :marker (:color ,(getf look :color) :line (:width 0)))))
                         (cutoffs
                           `(,@(when (or var var-value)
                                 `((,(format nil "VaR ~D%" (round (* 100 var)))
                                    ,(or var-value (fincl::sample-quantile values (- 1 var))))))
                             ,@(when (or es es-value)
                                 `((,(format nil "ES ~D%" (round (* 100 es)))
                                    ,(or es-value
                                         (let* ((cutoff (fincl::sample-quantile values (- 1 es)))
                                                (tail (remove-if-not (lambda (x) (<= x cutoff)) values)))
                                           (/ (reduce #'+ tail) (max 1 (length tail)))))))))))
                    `(,bars
                      ,@(mapcar (lambda (cutoff)
                                  (destructuring-bind (name at) cutoff
                                    (funcall (if horizontal #'hline #'vline) at
                                             :name name :style (line-attributes marker :opacity nil))))
                                cutoffs))))
                :data-needs needs :name label :xlabel xlabel :ylabel (if horizontal nil "density")
                :attributes (remove-nil-pairs `(:color ,color :opacity ,opacity))
                :raw raw)))

;;; Layers of other objects

(defun reference-line (value &key (axis :y) name color width dash raw)
  "A line across the panel at VALUE on the y axis, or on the x axis with AXIS
:X: a forward, a strike, an analytic price. Role :REFERENCE; it suits any
kind of x axis.

  (plot (histogram r :spot) (reference-line (forward m #D\"2027-09-24\") :axis :x :name \"forward\"))"
  (check-input axis (member :x :y))
  (make-layer :reference :any
              (lambda (style)
                `(,(funcall (if (eq axis :x) #'vline #'hline) value :name name
                            :style (line-attributes (funcall style) :opacity nil))))
              :name name
              :attributes (remove-nil-pairs `(:color ,color :width ,width :dash ,dash))
              :raw raw))

;;; --------------------------------------------------------------------
;;; Panels and grids
;;; --------------------------------------------------------------------

(defstruct (plot-panel (:constructor %make-plot-panel))
  "The layers of one pair of axes (see PANEL)."
  (layers '() :type list :read-only t)
  (title nil :read-only t)
  (xlabel nil :read-only t)
  (ylabel nil :read-only t)
  (style '() :read-only t)
  (xaxis '() :read-only t)
  (yaxis '() :read-only t))

(defstruct (plot-grid (:constructor %make-plot-grid))
  "Panels laid out in rows (see GRID)."
  (panels '() :type list :read-only t)
  (columns nil :read-only t)
  (shared-x nil :read-only t)
  (marginal nil :read-only t)
  (title nil :read-only t))

(defun split-arguments (arguments)
  "(values positional keys): ARGUMENTS up to its first keyword, and the rest."
  (let ((split (or (position-if #'keywordp arguments) (length arguments))))
    (values (subseq arguments 0 split) (subseq arguments split))))

(defun flatten-layers (things)
  (iter (for thing in things)
        (etypecase thing
          (layer (collect thing))
          (list (appending (flatten-layers thing))))))

(defun check-compatible (layers)
  "Signal INCOMPATIBLE-LAYERS if two of LAYERS differ in x-kind."
  (let* ((layers (remove :any layers :key #'layer-x-kind))
         (first (first layers)))
    (dolist (layer (rest layers))
      (unless (eq (layer-x-kind layer) (layer-x-kind first))
        (error 'incompatible-layers :first-layer first :second-layer layer)))))

(defun panel (&rest arguments)
  "A panel of layers (or lists of layers), then keys: :TITLE, :XLABEL,
:YLABEL (by default the layers' own), :STYLE (as PLOT's, for this panel
only) and :XAXIS, :YAXIS (Plotly axis attributes). The layers share one pair
of axes, so they need the same kind of x axis (INCOMPATIBLE-LAYERS).

  (panel (paths r :n 20) (mean-line r :spot) :title \"Spot\" :ylabel \"S\")"
  (multiple-value-bind (things keys) (split-arguments arguments)
    (destructuring-bind (&key title xlabel ylabel style xaxis yaxis) keys
      (let ((layers (flatten-layers things)))
        (when (null layers)
          (signal-invalid-argument 'layers things 'list "at least one layer"))
        (check-compatible layers)
        (check-style style)
        (%make-plot-panel :layers layers :title title
                          :xlabel (or xlabel (some #'layer-xlabel layers))
                          :ylabel (or ylabel (some #'layer-ylabel layers))
                          :style style :xaxis xaxis :yaxis yaxis)))))

(defun grid (&rest arguments)
  "A grid of panels (or layers, each a panel of its own), then keys:
:COLUMNS (2 by default), :SHARED-X (T: the panels zoom together, which
needs them to have the same kind of x axis), :MARGINAL (T: two panels, the
second a narrow one on the first's y axis, which shows the distribution of
what the first draws) and :TITLE.

  (grid (panel (paths r)) (panel (histogram r :spot :horizontal t)) :marginal t)"
  (multiple-value-bind (things keys) (split-arguments arguments)
    (destructuring-bind (&key columns shared-x marginal title) keys
      (let ((panels (mapcar (lambda (thing)
                              (etypecase thing
                                (plot-panel thing)
                                (layer (panel thing))))
                            (iter (for thing in things)
                                  (if (listp thing)
                                      (appending thing)
                                      (collect thing))))))
        (when (null panels)
          (signal-invalid-argument 'panels things 'list "at least one panel"))
        (when (and marginal (/= 2 (length panels)))
          (signal-invalid-argument 'panels panels 'list "two panels for a :MARGINAL grid"))
        (when shared-x
          (check-compatible (mapcar (lambda (p) (first (plot-panel-layers p))) panels)))
        (%make-plot-grid :panels panels :columns columns :shared-x shared-x
                         :marginal marginal :title title)))))

;;; --------------------------------------------------------------------
;;; PLOT
;;; --------------------------------------------------------------------

(defgeneric default-layers (object)
  (:documentation "The GRID PLOT draws for OBJECT, a fincl object. A new
plottable object type adds a method.

  (defmethod default-layers ((c my-curve))
    (grid (panel (my-curve-layer c))))")
  (:method (object)
    (signal-invalid-argument 'object object t
                             "a layer, a panel, a grid, or an object with a DEFAULT-LAYERS method")))

(defmethod default-layers ((result simulation-result))
  "One panel for the stored paths, one per recorded name (its mean, and a
5-95% band if its quantiles were kept) and one histogram per output."
  (let ((kept (simulation-kept-paths result))
        (panels '()))
    (when (plusp kept)
      (push (panel (paths result :n (min kept *max-plotted-paths*)) :title "paths") panels))
    (dolist (name (simulation-recorded-names result))
      (push (panel (when (simulation-quantiles-p result name) (band result name))
                   (mean-line result name)
                   :title (label-of name))
            panels))
    (dolist (output (simulation-output-names result))
      (push (panel (histogram result output) :title (label-of output)) panels))
    (when (null panels)
      (error 'missing-plot-data
             :layer (format nil "The default plot of ~S" result)
             :remedy "this result kept nothing to draw: add :keep (:paths 20) or (record name value) ~
to the SIMULATE call that made it, or a :FINAL clause."))
    (%make-plot-grid :panels (nreverse panels))))

(defun as-grid (thing)
  "THING (a grid, a panel, a layer or list of layers, a plottable object) as a grid."
  (typecase thing
    (plot-grid thing)
    (plot-panel (%make-plot-grid :panels `(,thing)))
    (layer (%make-plot-grid :panels `(,(panel thing))))
    (cons (%make-plot-grid :panels `(,(apply #'panel thing))))
    (t (let ((grid (default-layers thing)))
         (if (plot-grid-p grid)
             grid
             (signal-invalid-argument 'default-layers grid 'plot-grid "a GRID"))))))

(defun panel-figure-panel (panel)
  "The %PANEL of PANEL's layers' traces, under PANEL's style scope."
  (let ((*style-scopes* `(,@*style-scopes* ,(plot-panel-style panel)))
        (layers (plot-panel-layers panel)))
    (%panel (mapcan #'layer-traces layers)
            :title (plot-panel-title panel)
            :xlabel (plot-panel-xlabel panel)
            :ylabel (plot-panel-ylabel panel)
            :xaxis (plot-panel-xaxis panel)
            :yaxis (plot-panel-yaxis panel))))

(defun grid-figure (grid)
  (let* ((panels (iter (for panel in (plot-grid-panels grid))
                       (for i from 0)
                       (let ((figure-panel (panel-figure-panel panel)))
                         (when (and (plot-grid-shared-x grid) (plusp i))
                           (setf (getf figure-panel :xaxis)
                                 (merge-spec (getf figure-panel :xaxis) '(:matches "x"))))
                         (when (plot-grid-marginal grid)
                           (if (zerop i)
                               (setf (getf figure-panel :xaxis)
                                     (merge-spec '(:domain (0 0.8)) (getf figure-panel :xaxis)))
                               (setf (getf figure-panel :xaxis)
                                     (merge-spec '(:domain (0.82 1)) (getf figure-panel :xaxis))
                                     (getf figure-panel :yaxis)
                                     (merge-spec '(:matches "y" :showticklabels nil)
                                                 (getf figure-panel :yaxis)))))
                         (collect figure-panel)))))
    (apply #'%panel-grid panels
           `(,@(cond ((plot-grid-marginal grid) '(:columns 2))
                     ((plot-grid-columns grid) `(:columns ,(plot-grid-columns grid))))
             :title ,(plot-grid-title grid)))))

(defun plot-figure (&rest arguments)
  "The figure PLOT draws, with no files: ARGUMENTS as for PLOT without the
output keys.

  (plot-figure (paths r) (mean-line r :spot) :title \"GBM\")"
  (multiple-value-bind (things keys) (split-arguments arguments)
    (destructuring-bind (&key style title layout config) keys
      (check-style style)
      (let* ((grid (as-grid (if (rest things) things (first things))))
             (*style-scopes* `(,@*style-scopes* ,style)))
        (configure-figure (grid-figure grid) :layout layout :config config :title title)))))

(defun plot (&rest arguments)
  "Draw layers, a panel, a grid or a plottable object, then keys:
  :STYLE   a plist keyed by role or layer name, each a plist of :COLOR
           (a theme colour or CSS string) :WIDTH :DASH :OPACITY, over the
           theme and under the layers' own keywords
  :TITLE :LAYOUT :CONFIG :NAME :DISPLAY   as for DEFINE-PLOT
Several layers share one panel. Returns the pathname of the file written
(see EMIT-FIGURE).

  (plot (paths r :n 30) (mean-line r :spot))
  (plot (grid (panel (paths r)) (panel (histogram r :hedged :var 0.99)))
        :style '(:paths (:color :secondary)) :name \"fan\")
  (plot r)"
  (multiple-value-bind (things keys) (split-arguments arguments)
    (destructuring-bind (&key style title layout config (name "plot") (display *display*)) keys
      (emit-figure (apply #'plot-figure
                          `(,@things :style ,style :title ,title :layout ,layout :config ,config))
                   name :display display))))
