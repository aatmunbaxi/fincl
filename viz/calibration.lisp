;;;; viz/calibration.lisp --- a model's implied-vol surface before and after
;;;; calibration, against the surface it is calibrated to
;;;;
;;;; A "true" Heston model stands in for the market (the setup of
;;;; example.lisp's section 10). Its implied-vol surface is plotted against
;;;; that of an uncalibrated Heston guess; the guess is then calibrated to
;;;; the true model's option prices with CALIBRATE, and the two surfaces are
;;;; plotted again. Written as a user of fincl: quotes from MAKE-OPTION-QUOTE,
;;;; prices from PRICE, vols from IMPLIED-VOLATILITY.
;;;;
;;;;   (ql:quickload :fincl-viz)
;;;;   (fincl-viz::calibration-demo)   ; writes two PNGs under *OUTPUT-DIRECTORY*

(in-package #:fincl-viz)
(named-readtables:in-readtable fincl:syntax)

(defparameter *calibration-today* #D"2026-09-24")

(defun calibration-market ()
  "Spot 100, flat 4% rate and 1.5% dividend yield (the Heston prices do not
read the 22% vol)."
  (make-bs-market :valuation-date *calibration-today*
                  :spot 100 :rate 0.04d0 :dividend 0.015d0 :vol 0.22d0))

(defparameter *true-model*
  (make-heston :v0 0.04d0 :theta 0.05d0 :kappa 1.5d0 :xi 0.6d0 :rho -0.7d0)
  "The model whose prices play the market.")

(defparameter *initial-guess*
  (make-heston :v0 0.03d0 :theta 0.03d0 :kappa 3d0 :xi 0.2d0 :rho 0d0)
  "An uncalibrated Heston: low vol of vol and no correlation, so a nearly
flat, symmetric smile.")

(defparameter *surface-strikes* (grid 70 130 25))
(defparameter *surface-months* '(1 2 3 6 9 12 18 24))
(defparameter *smile-months* '(1 6 24)
  "Expiries shown as smiles under the surfaces.")

(defun months-out (months)
  (add-tenor *calibration-today* (make-tenor months :months)))

(defun otm-option (strike expiry)
  "The out-of-the-money option at STRIKE: a put below 100, a call above."
  (if (< strike 100)
      (make-option (put strike) (expires expiry))
      (make-option (call strike) (expires expiry))))

(defun model-smile (process market months strikes)
  "Black implied vols of PROCESS's COS prices across STRIKES, MONTHS out."
  (let ((engine (make-engine :cos :process process))
        (expiry (months-out months)))
    (mapcar (lambda (k)
              (let ((option (otm-option k expiry)))
                (implied-volatility option (price option engine market) :black-scholes market)))
            strikes)))

(defun model-surface (process market)
  "Implied vols over *SURFACE-MONTHS* x *SURFACE-STRIKES*, one list per expiry."
  (mapcar (lambda (months) (model-smile process market months *surface-strikes*))
          *surface-months*))

(defun calibration-quotes (market)
  "Option quotes priced by *TRUE-MODEL*: seven strikes at five expiries."
  (quote-set
   (mapcan (lambda (months)
             (mapcar (lambda (strike)
                       (let ((option (otm-option strike (months-out months))))
                         (make-option-quote
                          option :price (price option (make-engine :cos :process *true-model*) market))))
                     '(70 80 90 100 110 120 130)))
           '(1 3 6 12 24))))

;;; --------------------------------------------------------------------
;;; Drawing
;;; --------------------------------------------------------------------

(defparameter *truth-color* "#2a78d6" "Categorical slot 1 (blue): the true model.")
(defparameter *model-color* "#eb6834" "Categorical slot 2 (orange): the fitted model.")

(defun define-surface-helpers ()
  "Python helpers for 3-D surface panels: a single-hue blue ramp for vols
and a blue-gray-red diverging map, centred on 0, for errors."
  (py4cl:python-exec "
from matplotlib.colors import LinearSegmentedColormap
_fincl_seq = LinearSegmentedColormap.from_list(
    'fincl_blue', ['#cde2fb', '#86b6ef', '#3987e5', '#256abf', '#184f95', '#0d366b'])
_fincl_div = LinearSegmentedColormap.from_list('fincl_div', ['#2a78d6', '#f0efec', '#e34948'])
def _fincl_surface(rows, cols, index, xs, ys, z, title, kind, zlim, zlabel):
    ax = plt.subplot(rows, cols, index, projection='3d')
    X, Y = np.meshgrid(np.array(xs, dtype=float), np.array(ys, dtype=float))
    Z = np.array(z, dtype=float)
    if kind == 'error':
        m = max(float(np.abs(Z).max()), 1e-3)
        surface = ax.plot_surface(X, Y, Z, cmap=_fincl_div, vmin=-m, vmax=m,
                                  linewidth=0, antialiased=True)
        ax.set_zlim(-m, m)
    else:
        surface = ax.plot_surface(X, Y, Z, cmap=_fincl_seq, vmin=zlim[0], vmax=zlim[1],
                                  linewidth=0, antialiased=True)
        ax.set_zlim(zlim[0], zlim[1])
    ax.set_xlabel('strike', fontsize=9)
    ax.set_ylabel('years to expiry', fontsize=9)
    ax.set_zlabel(zlabel, fontsize=9)
    ax.tick_params(labelsize=8)
    ax.set_title(title, fontsize=11)
    ax.view_init(elev=25, azim=-125)
    plt.colorbar(surface, ax=ax, shrink=0.55, pad=0.12)
"))

(defun surface-panel (index z title &key (kind "vol") zlim (zlabel "implied vol (%)"))
  "Draw Z (rows per expiry, in percent) as 3-D panel INDEX of a 2 x 3 grid."
  (py4cl:python-call "_fincl_surface" 2 3 index *surface-strikes*
                     (mapcar (lambda (m) (/ m 12d0)) *surface-months*)
                     z title kind (or zlim '(0 1)) zlabel))

(defun percent (surface) (mapcar (lambda (row) (mapcar (lambda (v) (* 100 v)) row)) surface))

(defun plot-calibration-stage (name title truth model model-label market model-process)
  "Truth and MODEL surfaces (lists of vol rows) side by side with their
difference, over smiles of both at *SMILE-MONTHS*."
  (begin-figure :size '(19 11))
  (define-surface-helpers)
  (let* ((truth% (percent truth))
         (model% (percent model))
         (all (append (alexandria:flatten truth%) (alexandria:flatten model%)))
         (zlim `(,(floor (reduce #'min all)) ,(ceiling (reduce #'max all)))))
    (surface-panel 1 truth% "Ground truth (Heston)" :zlim zlim)
    (surface-panel 2 model% model-label :zlim zlim)
    (surface-panel 3 (mapcar (lambda (m tr) (mapcar #'- m tr)) model% truth%)
                   (format nil "~A minus truth" model-label)
                   :kind "error" :zlabel "vol points"))
  (alexandria:doplist (index months (alexandria:mappend #'list '(4 5 6) *smile-months*))
        (plt "subplot" 2 3 index)
        (draw-series *surface-strikes*
                     `(("ground truth" ,(mapcar (lambda (v) (* 100 v))
                                                (model-smile *true-model* market months *surface-strikes*))
                        :style (:color ,*truth-color* :linewidth 2))
                       (,model-label ,(mapcar (lambda (v) (* 100 v))
                                              (model-smile model-process market months *surface-strikes*))
                        :style (:color ,*model-color* :linewidth 2 :linestyle "--"))))
        (plt "title" (format nil "~D-month smile" months) :fontsize 11)
        (plt "xlabel" "strike")
        (plt "ylabel" "implied vol (%)")
        (plt "grid" :alpha 0.3)
        (plt "legend" :fontsize 9))
  (plt "suptitle" title :fontsize 13)
  (plt "tight_layout" :rect '(0 0 1 0.95))
  (finish-figure name))

(defun process-summary (process market)
  (format nil "~{~(~A~) ~,3F~^, ~}"
          (alexandria:mappend (lambda (name) `(,name ,(process-parameter process name market)))
                              '(:v0 :theta :kappa :xi :rho))))

(defun calibration-demo ()
  "Plot the true model's surface against the initial guess's, calibrate the
guess to the true model's prices, and plot again. Returns the two PNG
pathnames and CALIBRATE's report."
  (with-pricing-kernel ()
    (let* ((market (calibration-market))
           (truth (model-surface *true-model* market))
           (before (plot-calibration-stage
                    "calibration-before"
                    (format nil "Before calibration. Truth: ~A. Guess: ~A"
                            (process-summary *true-model* market)
                            (process-summary *initial-guess* market))
                    truth (model-surface *initial-guess* market) "Initial guess" market
                    *initial-guess*)))
      (multiple-value-bind (fitted report)
          (calibrate *initial-guess* market (calibration-quotes market) :engine :cos)
        (let ((after (plot-calibration-stage
                      "calibration-after"
                      (format nil "After calibration to 35 quotes (vol RMSE ~,1,,,,,'eE). Fitted: ~A"
                              (getf report :rmse) (process-summary fitted market))
                      truth (model-surface fitted market) "Calibrated" market fitted)))
          (values `(,before ,after) report))))))
