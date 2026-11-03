;;;; viz/README-fig-generation.lisp --- the figures of README.org's showcase
;;;;
;;;; The figures that show paths, or quantities along them, are drawn from a
;;;; FINCL:SIMULATE run through PLOT's layers (viz/plot.lisp). The rest (price
;;;; against inputs, volatility smiles, calibration surfaces) are not
;;;; simulations; they are written as figures of traces (%PANEL, %PANEL-GRID)
;;;; in the same theme. The Plotly JSON and PNG of each go to
;;;; viz/images/README-figures/.
;;;;
;;;;   nix develop -c sbcl --non-interactive --load viz/README-fig-generation.lisp
;;;;
;;;; or, from a REPL with fincl-viz loaded, (readme-figures:generate-all).
;;;; Needs python with plotly and kaleido (see viz/render.py).

(ql:quickload '(:fincl :fincl-viz) :silent t)

(defpackage #:readme-figures
  (:use #:cl #:fincl #:fincl-viz #:iterate)
  (:shadowing-import-from #:fincl #:minimize)
  (:export #:generate-all))

(in-package #:readme-figures)
(named-readtables:in-readtable fincl:syntax)

(defparameter *today* #D"2026-10-16")

(defparameter *images*
  (let ((override (uiop:getenv "README_FIGURES_DIR")))
    (if (and override (plusp (length override)))
        (uiop:ensure-directory-pathname override)
        (merge-pathnames "images/README-figures/"
                         (or *load-truename* (merge-pathnames "viz/" (uiop:getcwd))))))
  "Where the figures go: viz/images/README-figures/, or $README_FIGURES_DIR.")

(defun market ()
  "SPX at 100, 20% vol, 1% dividend yield, a flat 4% USD curve."
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :vol 0.2d0 :dividend 0.01d0 :currency :usd))
               :curves '((:usd :rate 0.04d0))
               :discounting '((:usd . :usd))))

(defun after (days)
  (add-tenor *today* (make-tenor days :days)))

(defun daily-dates (n) (iter (for k from 1 to n) (collect (after k))))
(defun weekly-dates (weeks) (iter (for k from 1 to weeks) (collect (after (* 7 k)))))

;;; --------------------------------------------------------------------
;;; Stochastic processes: twenty paths, their mean, and the forward
;;; --------------------------------------------------------------------

(defun process-figure (name process title &key (component :spot) (forward t))
  "Twenty paths of PROCESS over one year (plot-paths: the market forward is
drawn for the spot, and the sample mean should follow it)."
  (let ((*display* :png) (*output-directory* *images*))
    (plot-paths process (market) :horizon 1 :n-dates 100 :n-paths 20 :seed 42
                                 :component component :forward forward
                                 :name name :title title)))

(defun processes ()
  (process-figure "gbm-sim" (make-gbm) "Geometric Brownian motion: 20 paths")
  (process-figure "heston-sim"
                  (make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0 :xi 0.5d0 :rho -0.7d0)
                  "Heston: 20 paths")
  (process-figure "cev-sim" (make-cev :beta 0.5d0 :sigma 2d0) "CEV, beta = 1/2: 20 paths")
  ;; GARCH steps in whole model periods, so its period is one calendar day.
  (let ((*display* :png) (*output-directory* *images*)
        (dates (daily-dates 365))
        (garch (make-garch :period (/ 1d0 365) :alpha 0.05d0 :beta 0.9d0 :gamma 0.5d0)))
    (let ((r (simulate (garch (market) dates :n-paths 20 :seed 42 :keep (:paths 20))
               (:state s :spot)
               (:every (date) (record :spot s)))))
      (plot (paths r :n 20) (mean-line r :spot :label "sample mean")
            :name "garch-sim" :title "NGARCH: 20 paths" ))))

;;; --------------------------------------------------------------------
;;; Option prices and Greeks along simulated paths
;;; --------------------------------------------------------------------
;;; One-year options struck at 100 are valued on the market of a simulated
;;; date: the spot differs per path, so the prices and Greeks trace out the
;;; functions of the spot the figure is about.

(defun cloud (xs ys name color)
  "A layer of dots at (XS, YS), one per path, in COLOR."
  (make-layer :marker :value
              (lambda (style)
                (declare (ignore style))
                `(,(scatter-trace xs ys :name name
                                        :style `(:marker (:color ,color :opacity 0.5 :size 4)))))
              :name name))

(defparameter *one-year* (after 365))
(defparameter *call* (make-option (call 100) (expires *one-year*)))
(defparameter *put* (make-option (put 100) (expires *one-year*)))

(defun greeks-at-180-days ()
  "Per path, the spot after 6 months and the one-year call's and put's price
and Greeks on that day's market."
  (flet ((keys (option market)
           (greeks option :black-scholes market
                          :which '(:delta :gamma :vega :theta :rho :dividend-rho))))
    (simulate ((make-gbm) (market) (list (after 180)) :n-paths 1500 :seed 7)
      (:state s :spot)
      (:final (:spot s)
              (:call-price (price *call* :black-scholes market))
              (:put-price (price *put* :black-scholes market))
              (:call-delta (getf (keys *call* market) :delta))
              (:put-delta (getf (keys *put* market) :delta))
              (:gamma (getf (keys *call* market) :gamma))
              (:vega (getf (keys *call* market) :vega))
              (:theta (getf (keys *call* market) :theta))
              (:rho (getf (keys *call* market) :rho))
              (:dividend-rho (getf (keys *call* market) :dividend-rho))))))

(defun price-and-greeks-vs-spot ()
  (let* ((r (greeks-at-180-days))
         (xs (simulation-values r :spot))
         (dots (lambda (name label &optional (color "#c0392b"))
                 (cloud xs (simulation-values r name) label color)))
         (*display* :png) (*output-directory* *images*))
    (plot (grid (panel (funcall dots :call-delta "call") (funcall dots :put-delta "put" "#2874a6")
                       :title "Delta" :xlabel "spot" :ylabel "dV/dS")
                (panel (funcall dots :gamma "call") :title "Gamma" :xlabel "spot" :ylabel "d2V/dS2")
                (panel (funcall dots :vega "call") :title "Vega" :xlabel "spot" :ylabel "dV/dvol")
                (panel (funcall dots :theta "call") :title "Theta (per day)" :xlabel "spot")
                (panel (funcall dots :rho "call") :title "Rho" :xlabel "spot")
                (panel (funcall dots :dividend-rho "call") :title "Dividend rho" :xlabel "spot")
                :columns 3 :title "One-year call struck at 100, valued on 1,500 simulated spots after 6 months")
          :name "greeks-vs-spot")
    (plot (panel (funcall dots :call-price "European call")
                 (funcall dots :put-price "European put" "#2874a6")
                 :title "Price" :xlabel "spot after 6 months" :ylabel "price")
          :name "price-vs-spot-simulated"
          :title "One-year options struck at 100, valued on 1,500 simulated spots after 6 months")))

(defun greeks-vs-expiry-simulated ()
  "An at-the-money call's Greeks, valued on each simulated week's market and
averaged over the paths: the expected Greek at each date, not the Greek at a
fixed spot."
  (let* ((*display* :png) (*output-directory* *images*)
         (call *call*)
         (r (simulate ((make-gbm) (market) (weekly-dates 51) :n-paths 2000 :seed 7 :keep :quantiles)
              (:every (date market)
                (let ((g (greeks call :black-scholes market :which '(:delta :gamma :vega :theta))))
                  (record :delta (getf g :delta))
                  (record :gamma (getf g :gamma))
                  (record :vega (getf g :vega))
                  (record :theta (getf g :theta)))))))
    (plot (grid (panel (band r :delta) (mean-line r :delta) :title "Delta")
                (panel (band r :gamma) (mean-line r :gamma) :title "Gamma")
                (panel (band r :vega) (mean-line r :vega) :title "Vega")
                (panel (band r :theta) (mean-line r :theta) :title "Theta (per day)")
                :columns 2 :shared-x t
                :title "One-year call struck at 100: Greeks on simulated markets (mean and 5-95% band over 2,000 paths)")
          :name "greeks-vs-expiry-simulated")))

(defun greeks-vs-expiry ()
  "The Greeks of an at-the-money call at spot 100 against time to expiry."
  (let* ((m (market))
         (days (mapcar #'round (linspace 7 730 41)))
         (years (mapcar (lambda (d) (/ d 365d0)) days))
         (calls (mapcar (lambda (d) (make-option (call 100) (expires (after d)))) days)))
    (flet ((greek (key)
             (mapcar (lambda (o) (getf (greeks o :black-scholes m :which `(,key)) key)) calls)))
      (emit
       (%panel-grid
        (mapcar (lambda (spec)
                  (destructuring-bind (key title) spec
                    (lines-panel title "years to expiry" title years `(("ATM call" ,(greek key))))))
                '((:delta "Delta") (:gamma "Gamma") (:vega "Vega") (:theta "Theta (per day)")))
        :columns 2 :cell-width 560 :cell-height 400
        :title "At-the-money call: Greeks against time to expiry")
       "greeks-vs-expiry"))))

;;; --------------------------------------------------------------------
;;; Monte Carlo convergence
;;; --------------------------------------------------------------------

(defparameter *path-counts* '(1000 10000 100000 1000000))

(defun convergence-layer (estimates name)
  "A line with two-standard-error bars through ESTIMATES, a list of
(n-paths mean standard-error)."
  (make-layer :statistic :value
              (lambda (style)
                `(,(error-trace (mapcar #'first estimates) (mapcar #'second estimates)
                                (mapcar (lambda (e) (* 2 (third e))) estimates)
                                :name name
                                :style `(:line (:color ,(getf (funcall style) :color))))))
              :name name))

(defun convergence-figure (name title estimates reference reference-label)
  (let ((*display* :png) (*output-directory* *images*))
    (plot (panel (convergence-layer estimates "Monte Carlo, 2 SE")
                 (reference-line reference :name reference-label :dash "dash")
                 :xlabel "paths" :ylabel "price"
                 :xaxis '(:type "log" :tickvals (1000 10000 100000 1000000)
                          :ticktext #("1e3" "1e4" "1e5" "1e6") :minor (:showgrid nil :ticks "")))
          :name name :title title)))

(defun discounted-estimate (r output df)
  (multiple-value-bind (m se) (simulation-summary r (simulation-values r output))
    `(,(* df m) ,(* df se))))

(defun european-convergence ()
  (let* ((m (market)) (df (discount-factor m *one-year*)))
    (convergence-figure
     "euro-convergence" "ATM one-year European call"
     (mapcar (lambda (n)
               (let ((r (simulate ((make-gbm) m (list *one-year*) :n-paths n :seed 11 :antithetic t)
                          (:state s :spot)
                          (:final (:payoff (max 0d0 (- s 100d0)))))))
                 `(,n ,@(discounted-estimate r :payoff df))))
             *path-counts*)
     (price *call* :black-scholes m) "Black-Scholes")))

(defun asian-convergence ()
  "A discrete geometric Asian call (average of four quarterly dates) by path logic,
against its closed form."
  (let* ((m (market))
         (dates (iter (for k from 1 to 4) (collect (after (* k 91)))))
         (expiry (car (last dates)))
         (asian (make-option (call 100) (average :geometric :dates dates) (expires expiry))))
    (convergence-figure
     "asian-convergence" "ATM geometric Asian call, four quarterly fixings"
     (mapcar (lambda (n)
               (let ((r (simulate ((make-gbm) m dates :n-paths n :seed 11 :antithetic t)
                          (:accumulate (log-sum 0d0))
                          (:state s :spot)
                          (:every (date) (incf log-sum (log s)))
                          (:final (:payoff (max 0d0 (- (exp (/ log-sum 4d0)) 100d0)))))))
                 `(,n ,@(discounted-estimate r :payoff (discount-factor m expiry)))))
             *path-counts*)
     (price asian :black-scholes m) "closed form")))

(defun american-convergence ()
  "An American put priced by Longstaff-Schwartz Monte Carlo against the
binomial tree (the simulator has no exercise decisions of its own)."
  (let* ((m (market))
         (american (make-option (put 100) (american) (expires *one-year*)))
         (counts '(1000 10000 100000)))
    (convergence-figure
     "american-convergence" "ATM one-year American put (least-squares Monte Carlo)"
     (mapcar (lambda (n)
               (multiple-value-bind (p se)
                   (price american (make-engine :mc :n-paths n :n-steps 50 :seed 11 :antithetic t) m)
                 `(,n ,p ,se)))
             counts)
     (price american :binomial m) "binomial tree")))


;;; --------------------------------------------------------------------
;;; Not simulations: figures of traces, in the same theme
;;; --------------------------------------------------------------------

(defun linspace (from to n)
  (iter (for i below n) (collect (+ from (* i (/ (- to from) (1- n)))))))

(defun emit (figure name)
  (let ((*display* :png) (*output-directory* *images*))
    (emit-figure figure name)))

(defparameter *series-colors*
  '("#2a78d6" "#eb6834" "#2f9e6e" "#8a5cd1")
  "The colours of a panel's series in order, the same in every panel.")

(defun lines-panel (title xlabel ylabel xs series)
  "A panel of one line per (label ys) of SERIES against XS, series i in
colour *SERIES-COLORS* i."
  (%panel (mapcar (lambda (entry color)
                    (line-trace xs (second entry) :name (first entry)
                                                  :style `(:line (:color ,color))))
                  series *series-colors*)
          :title title :xlabel xlabel :ylabel ylabel))

(defun scenario-markets (market factor values base)
  "MARKET with FACTOR (as in BUMP) moved from BASE to each of VALUES."
  (mapcar (lambda (v) (bump market factor `(:absolute ,(- v base)))) values))

(defparameter *price-options*
  `(("European call" ,*call* :black-scholes)
    ("European put" ,*put* :black-scholes)
    ("American put" ,(make-option (put 100) (american) (expires *one-year*)) :binomial)))

(defun price-series (markets)
  "One (label prices) per option of *PRICE-OPTIONS* over MARKETS."
  (mapcar (lambda (entry)
            (destructuring-bind (label option engine) entry
              `(,label ,(mapcar (lambda (m) (price option engine m)) markets))))
          *price-options*))

(defun price-vs-inputs ()
  "Price against spot, volatility, the interest rate and time to expiry."
  (let* ((m (market))
         (spots (linspace 60 140 41))
         (vols (linspace 0.05d0 0.6d0 41))
         (rates (linspace -0.01d0 0.10d0 41))
         (days (mapcar #'round (linspace 7 730 41))))
    (emit
     (%panel-grid
      `(,(lines-panel "Price vs spot" "spot" "price" spots
                      (price-series (scenario-markets m '(:spot :spx) spots 100)))
        ,(lines-panel "Price vs volatility" "volatility" "price" vols
                      (price-series (scenario-markets m '(:vol :spx) vols 0.2d0)))
        ,(lines-panel "Price vs interest rate" "rate" "price" rates
                      (price-series (scenario-markets m '(:curve :usd) rates 0.04d0)))
        ,(lines-panel "Price vs time to expiry" "years to expiry" "price"
                      (mapcar (lambda (d) (/ d 365d0)) days)
                      (mapcar (lambda (entry)
                                (destructuring-bind (label option engine) entry
                                  (let ((make (if (eq engine :binomial)
                                                  (lambda (d) (make-option (put 100) (american) (expires (after d))))
                                                  (let ((kind (if (search "call" label) :call :put)))
                                                    (lambda (d) (if (eq kind :call)
                                                                    (make-option (call 100) (expires (after d)))
                                                                    (make-option (put 100) (expires (after d)))))))))
                                    (declare (ignore option))
                                    `(,label ,(mapcar (lambda (d) (price (funcall make d) engine m)) days)))))
                              *price-options*)))
      :columns 2 :cell-width 560 :cell-height 400
      :title "One-year options struck at 100 (spot 100, vol 20%, r 4%, q 1%)")
     "price-vs-inputs")))

(defun otm-option (strike expiry today)
  "The out-of-the-money option at STRIKE: a put below 100, a call above."
  (declare (ignore today))
  (if (< strike 100)
      (make-option (put strike) (expires expiry))
      (make-option (call strike) (expires expiry))))

(defun smile (process market strikes expiry)
  "Black implied vols (in percent) of PROCESS's COS prices across STRIKES."
  (let ((engine (make-engine :cos :process process)))
    (mapcar (lambda (k)
              (let ((option (otm-option k expiry nil)))
                (* 100 (implied-volatility option (price option engine market) :black-scholes market))))
            strikes)))

(defun volatility-smiles ()
  "Implied-vol smiles of Heston (stochastic vol, negative correlation) and
Merton (jumps) prices, by expiry, against the flat Black-Scholes vol."
  (let* ((m (market))
         (strikes (linspace 70 130 31))
         (expiries '((30 "1M") (91 "3M") (365 "1Y") (730 "2Y")))
         (models `(("Heston: v0 = theta = 4%, kappa 1.5, xi 0.5, rho -0.7"
                    ,(make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0 :xi 0.5d0 :rho -0.7d0))
                   ("Merton: 0.5 jumps a year, mean -10%, sd 15%"
                    ,(make-merton-jump :intensity 0.5d0 :jump-mean -0.1d0 :jump-vol 0.15d0)))))
    (emit
     (%panel-grid
      (mapcar (lambda (model)
                (destructuring-bind (title process) model
                  (%panel `(,@(mapcar (lambda (e color)
                                        (line-trace strikes (smile process m strikes (after (first e)))
                                                    :name (second e)
                                                    :style `(:line (:color ,color))))
                                      expiries *series-colors*)
                            ,(hline 20 :name "Black-Scholes"
                                       :style `(:line (:color ,(theme-color :reference) :dash "dash" :width 1.5))))
                          :title title :xlabel "strike" :ylabel "implied vol (%)")))
              models)
      :columns 2 :cell-width 560 :cell-height 440 :title "Implied-volatility smiles by expiry")
     "volatility-smiles")))

;;; Calibration: the true model's surface against a guess's, before and after
;;; CALIBRATE fits the guess to the true model's option prices.

(defparameter *cal-today* #D"2026-09-24")
(defparameter *cal-strikes* (linspace 70 130 25))
(defparameter *cal-months* '(1 2 3 6 9 12 18 24))
(defparameter *cal-smile-months* '(1 6 24) "Expiries drawn as smiles under the surfaces.")

(defparameter *true-model*
  (make-heston :v0 0.04d0 :theta 0.05d0 :kappa 1.5d0 :xi 0.6d0 :rho -0.7d0)
  "The model whose prices play the market.")

(defparameter *initial-guess*
  (make-heston :v0 0.03d0 :theta 0.03d0 :kappa 3d0 :xi 0.2d0 :rho 0d0)
  "Low vol of vol and no correlation: a nearly flat, symmetric smile.")

(defun cal-market ()
  (make-bs-market :valuation-date *cal-today* :spot 100 :rate 0.04d0 :dividend 0.015d0 :vol 0.22d0))

(defun months-out (months) (add-tenor *cal-today* (make-tenor months :months)))

(defun model-smile (process market months)
  (smile process market *cal-strikes* (months-out months)))

(defun model-surface (process market)
  "Implied vols (percent), a row per expiry of *CAL-MONTHS* over *CAL-STRIKES*."
  (mapcar (lambda (months) (model-smile process market months)) *cal-months*))

(defun cal-quotes (market)
  "Seven strikes at five expiries, priced by *TRUE-MODEL*."
  (quote-set
   (mapcan (lambda (months)
             (mapcar (lambda (strike)
                       (let ((option (otm-option strike (months-out months) nil)))
                         (make-option-quote
                          option :price (price option (make-engine :cos :process *true-model*) market))))
                     '(70 80 90 100 110 120 130)))
           '(1 3 6 12 24))))

(defun surface-panel (z title &key (colorscale :sequential) zlim (zlabel "implied vol (%)"))
  (destructuring-bind (lo hi) zlim
    (%panel (list (surface-trace *cal-strikes* (mapcar (lambda (m) (/ m 12d0)) *cal-months*) z
                                 :style `(:colorscale ,(theme-colorscale colorscale)
                                          :cmin ,lo :cmax ,hi)))
            :title title :xlabel "strike" :ylabel "years to expiry" :zlabel zlabel
            :scene `(:zaxis (:range (,lo ,hi)) :camera (:eye ,(camera-eye 25 -125 2.5))))))

(defun smile-panel (truth-smile model-smile months model-label)
  (%panel (list (line-trace *cal-strikes* truth-smile :name "ground truth"
                            :style `(:line (:color ,(theme-color :truth) :width 2)))
                (line-trace *cal-strikes* model-smile :name model-label
                            :style `(:line (:color ,(theme-color :model) :width 2 :dash "dash"))))
          :title (format nil "~D-month smile" months) :xlabel "strike" :ylabel "implied vol (%)"))

(defun calibration-stage (name title truth model model-label market model-process)
  "TRUTH and MODEL surfaces beside their difference, over smiles of both."
  (let* ((error% (mapcar (lambda (m tr) (mapcar #'- m tr)) model truth))
         (all (alexandria:flatten (append truth model)))
         (zlim `(,(floor (reduce #'min all)) ,(ceiling (reduce #'max all))))
         (half (max 1d-3 (reduce #'max (mapcar #'abs (alexandria:flatten error%))))))
    (emit
     (%panel-grid
      `(,(surface-panel truth "Ground truth (Heston)" :zlim zlim)
        ,(surface-panel model model-label :zlim zlim)
        ,(surface-panel error% (format nil "~A minus truth" model-label)
                        :colorscale :diverging :zlim `(,(- half) ,half) :zlabel "vol points")
        ,@(mapcar (lambda (months)
                    (smile-panel (model-smile *true-model* market months)
                                 (model-smile model-process market months) months model-label))
                  *cal-smile-months*))
      :columns 3 :cell-width 640 :cell-height 560 :title title)
     name)))

(defun process-summary (process market)
  (format nil "~{~(~A~) ~,3F~^, ~}"
          (alexandria:mappend (lambda (key) `(,key ,(process-parameter process key market)))
                              '(:v0 :theta :kappa :xi :rho))))

(defun calibration-figures ()
  (let* ((market (cal-market))
         (truth (model-surface *true-model* market)))
    (calibration-stage "calibration-before"
                       (format nil "Before calibration. Truth: ~A. Guess: ~A"
                               (process-summary *true-model* market)
                               (process-summary *initial-guess* market))
                       truth (model-surface *initial-guess* market) "Initial guess" market *initial-guess*)
    (multiple-value-bind (fitted report)
        (calibrate *initial-guess* market (cal-quotes market) :engine :cos)
      (calibration-stage "calibration-after"
                         (format nil "After calibration to 35 quotes (vol RMSE ~,1,,,,,'eE). Fitted: ~A"
                                 (getf report :rmse) (process-summary fitted market))
                         truth (model-surface fitted market) "Calibrated" market fitted))))

(defun generate-all ()
  (with-pricing-kernel ()
    (processes)
    (price-and-greeks-vs-spot)
    (greeks-vs-expiry-simulated)
    (greeks-vs-expiry)
    (european-convergence)
    (asian-convergence)
    (american-convergence)
    (price-vs-inputs)
    (volatility-smiles)
    (calibration-figures)))

(generate-all)
