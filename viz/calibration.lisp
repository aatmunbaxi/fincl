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
;;;;   (fincl-viz::calibration-demo)   ; writes two figures under *OUTPUT-DIRECTORY*

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

(defun percent (surface) (mapcar (lambda (row) (mapcar (lambda (v) (* 100 v)) row)) surface))

(defun surface-panel (z title &key (colorscale :sequential) zlim (zlabel "implied vol (%)"))
  "Z (a row per expiry of *SURFACE-MONTHS*, in percent) over *SURFACE-STRIKES*
as a 3-D panel coloured by the theme's COLORSCALE over ZLIM, (lo hi)."
  (destructuring-bind (lo hi) zlim
    (panel (list (surface-trace *surface-strikes* (mapcar (lambda (m) (/ m 12d0)) *surface-months*) z
                                :style `(:colorscale ,(theme-colorscale colorscale)
                                         :cmin ,lo :cmax ,hi)))
           :title title :xlabel "strike" :ylabel "years to expiry" :zlabel zlabel
           :scene `(:zaxis (:range (,lo ,hi)) :camera (:eye ,(camera-eye 25 -125))))))

(defun smile-panel (months model-label market model-process)
  "The truth's and MODEL-PROCESS's smiles MONTHS out, in percent."
  (flet ((smile% (process)
           (mapcar (lambda (v) (* 100 v))
                   (model-smile process market months *surface-strikes*))))
    (panel (list (line-trace *surface-strikes* (smile% *true-model*) :name "ground truth"
                             :style `(:line (:color ,(theme-color :truth) :width 2)))
                 (line-trace *surface-strikes* (smile% model-process) :name model-label
                             :style `(:line (:color ,(theme-color :model) :width 2 :dash "dash"))))
           :title (format nil "~D-month smile" months)
           :xlabel "strike" :ylabel "implied vol (%)")))

(define-plot calibration-stage (title truth model model-label market model-process)
  "Truth and MODEL surfaces (lists of vol rows over *SURFACE-MONTHS* x
*SURFACE-STRIKES*) side by side with their difference, over smiles of both at
*SMILE-MONTHS*. Truth and model share one vol range; the difference is
coloured symmetrically about 0.

  (plot-calibration-stage \"Initial guess\" truth guess \"Guess\" market *initial-guess*
                          :name \"calibration-before\")"
  (let* ((truth% (percent truth))
         (model% (percent model))
         (error% (mapcar (lambda (m tr) (mapcar #'- m tr)) model% truth%))
         (all (append (a:flatten truth%) (a:flatten model%)))
         (zlim `(,(floor (reduce #'min all)) ,(ceiling (reduce #'max all))))
         (half-range (max 1d-3 (reduce #'max (mapcar #'abs (a:flatten error%))))))
    (panel-grid
     `(,(surface-panel truth% "Ground truth (Heston)" :zlim zlim)
       ,(surface-panel model% model-label :zlim zlim)
       ,(surface-panel error% (format nil "~A minus truth" model-label)
                       :colorscale :diverging :zlim `(,(- half-range) ,half-range)
                       :zlabel "vol points")
       ,@(mapcar (lambda (months) (smile-panel months model-label market model-process))
                 *smile-months*))
     :columns 3 :cell-width 600 :cell-height 480 :title title)))

(defun process-summary (process market)
  (format nil "~{~(~A~) ~,3F~^, ~}"
          (alexandria:mappend (lambda (name) `(,name ,(process-parameter process name market)))
                              '(:v0 :theta :kappa :xi :rho))))

(defun calibration-demo ()
  "Plot the true model's surface against the initial guess's, calibrate the
guess to the true model's prices, and plot again. Returns the two figures'
pathnames and CALIBRATE's report."
  (with-pricing-kernel ()
    (let* ((market (calibration-market))
           (truth (model-surface *true-model* market))
           (before (plot-calibration-stage
                    (format nil "Before calibration. Truth: ~A. Guess: ~A"
                            (process-summary *true-model* market)
                            (process-summary *initial-guess* market))
                    truth (model-surface *initial-guess* market) "Initial guess" market
                    *initial-guess*
                    :name "calibration-before")))
      (multiple-value-bind (fitted report)
          (calibrate *initial-guess* market (calibration-quotes market) :engine :cos)
        (let ((after (plot-calibration-stage
                      (format nil "After calibration to 35 quotes (vol RMSE ~,1,,,,,'eE). Fitted: ~A"
                              (getf report :rmse) (process-summary fitted market))
                      truth (model-surface fitted market) "Calibrated" market fitted
                      :name "calibration-after")))
          (values `(,before ,after) report))))))
