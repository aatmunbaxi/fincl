;;;; viz/greeks.lisp --- an option's price and Greeks against its inputs,
;;;; and the volatility smiles stochastic models produce
;;;;
;;;; Written as a user of fincl would: one composite market, options from
;;;; MAKE-OPTION, scenarios from BUMP, prices from PRICE, sensitivities from
;;;; GREEKS and smiles from IMPLIED-VOLATILITY. Nothing here reaches into
;;;; fincl's internals.
;;;;
;;;;   (ql:quickload :fincl-viz)
;;;;   (fincl-viz::greeks-demo)   ; writes the figures under *OUTPUT-DIRECTORY*

(in-package #:fincl-viz)
(named-readtables:in-readtable fincl:syntax)

(defparameter *today* #D"2026-10-16")

(defun demo-market ()
  "SPX at 100 with a 20% vol and a 1% dividend yield, discounted on a flat
4% USD curve."
  (make-market :valuation-date *today*
               :underlyings '((:spx :spot 100 :vol 0.2d0 :dividend 0.01d0 :currency :usd))
               :curves '((:usd :rate 0.04d0))))

(defun expiring-in (days)
  "The expiry DAYS calendar days after *TODAY*."
  (add-tenor *today* (make-tenor days :days)))

(defun demo-options ()
  "One-year at-the-money options: (label option engine)."
  (let ((expiry (expiring-in 365)))
    `(("European call" ,(make-option (call 100) (expires expiry)) :black-scholes)
      ("European put" ,(make-option (put 100) (expires expiry)) :black-scholes)
      ("American put" ,(make-option (put 100) (american) (expires expiry)) :binomial))))

(defun grid (from to &optional (n 41))
  "N evenly spaced points from FROM to TO."
  (mapcar (lambda (i) (+ from (* i (/ (- to from) (1- n))))) (alexandria:iota n)))

;;; --------------------------------------------------------------------
;;; Scenarios: the same option repriced on bumped markets
;;; --------------------------------------------------------------------

(defun scenario-markets (market factor values base)
  "MARKET with FACTOR (as in BUMP) moved from BASE to each of VALUES."
  (mapcar (lambda (v) (bump market factor `(:absolute ,(- v base)))) values))

(defun prices-on (option engine markets)
  (mapcar (lambda (m) (price option engine m)) markets))

(defun greek-on (option engine markets key)
  (mapcar (lambda (m) (getf (greeks option engine m :which `(,key)) key)) markets))

(defun option-panel (title xlabel xs ys-for &key (ylabel "price"))
  "A panel spec (see SERIES-PANEL) with one series per demo option: YS-FOR
maps (option engine) to the values over XS."
  `(:title ,title :xlabel ,xlabel :ylabel ,ylabel :xs ,xs
    :series ,(mapcar (lambda (o)
                       (destructuring-bind (label option engine) o
                         `(,label ,(funcall ys-for option engine))))
                     (demo-options))))

(define-plot price-vs-inputs ()
  "Price against spot, volatility, the interest rate and time to expiry."
  (let* ((m (demo-market))
         (spots (grid 60 140))
         (vols (grid 0.05d0 0.6d0))
         (rates (grid -0.01d0 0.10d0))
         (days (mapcar #'round (grid 7 730))))
    (panel-grid
     (mapcar
      #'series-panel
      `(,(option-panel "Price vs spot" "spot" spots
                       (lambda (o e) (prices-on o e (scenario-markets m '(:spot :spx) spots 100))))
        ,(option-panel "Price vs volatility" "volatility" vols
                       (lambda (o e) (prices-on o e (scenario-markets m '(:vol :spx) vols 0.2d0))))
        ,(option-panel "Price vs interest rate" "rate" rates
                       (lambda (o e) (prices-on o e (scenario-markets m '(:curve :usd) rates 0.04d0))))
        ;; The same contract with every expiry from a week to two years.
        (:title "Price vs time to expiry" :xlabel "years to expiry" :ylabel "price"
         :xs ,(mapcar (lambda (d) (/ d 365d0)) days)
         :series (("European call" ,(mapcar (lambda (d)
                                               (price (make-option (call 100) (expires (expiring-in d)))
                                                      :black-scholes m))
                                             days))
                  ("European put" ,(mapcar (lambda (d)
                                              (price (make-option (put 100) (expires (expiring-in d)))
                                                     :black-scholes m))
                                            days))
                  ("American put" ,(mapcar (lambda (d)
                                              (price (make-option (put 100) (american) (expires (expiring-in d)))
                                                     :binomial m))
                                            days))))))
     :title "One-year options struck at 100 (spot 100, vol 20%, r 4%, q 1%)")))

(define-plot greeks-vs-spot ()
  "Delta, gamma, vega, theta, rho and dividend rho across spot."
  (let* ((m (demo-market))
         (spots (grid 60 140))
         (markets (scenario-markets m '(:spot :spx) spots 100)))
    (panel-grid
     (mapcar (lambda (spec)
               (destructuring-bind (key title ylabel) spec
                 (series-panel
                  (option-panel title "spot" spots (lambda (o e) (greek-on o e markets key))
                                :ylabel ylabel))))
             '((:delta "Delta" "dV/dS")
               (:gamma "Gamma" "d2V/dS2")
               (:vega "Vega" "dV/dvol (per unit vol)")
               (:theta "Theta" "price change per day")
               (:rho "Rho" "dV/dr (per unit rate)")
               (:dividend-rho "Dividend rho" "dV/dq (per unit yield)")))
     :title "Greeks of one-year options struck at 100"
     :columns 3)))

(define-plot greeks-vs-expiry ()
  "How an at-the-money call's Greeks evolve as expiry approaches."
  (let* ((m (demo-market))
         (days (mapcar #'round (grid 7 730)))
         (years (mapcar (lambda (d) (/ d 365d0)) days))
         (calls (mapcar (lambda (d) (make-option (call 100) (expires (expiring-in d)))) days)))
    (flet ((greek (key)
             (mapcar (lambda (o) (getf (greeks o :black-scholes m :which `(,key)) key)) calls)))
      (panel-grid
       (mapcar (lambda (spec)
                 (destructuring-bind (key title) spec
                   (series-panel `(:title ,title :xlabel "years to expiry" :xs ,years
                                   :series (("ATM call" ,(greek key)))))))
               '((:delta "Delta") (:gamma "Gamma") (:vega "Vega") (:theta "Theta (per day)")))
       :title "At-the-money call: Greeks against time to expiry"))))

;;; --------------------------------------------------------------------
;;; Volatility smiles
;;; --------------------------------------------------------------------

(defun smile (engine market strikes days)
  "Black implied vols of the out-of-the-money options (puts below the
forward, calls above) priced with ENGINE, expiring in DAYS."
  (let ((forward (forward market (expiring-in days))))
    (mapcar (lambda (k)
              (let ((option (if (< k forward)
                                (make-option (put k) (expires (expiring-in days)))
                                (make-option (call k) (expires (expiring-in days))))))
                (implied-volatility option (price option engine market) :black-scholes market)))
            strikes)))

(define-plot smiles ()
  "Implied-vol smiles of Heston (stochastic vol, negative correlation) and
Merton (jumps) prices, by expiry, against the flat Black-Scholes vol."
  (let* ((m (demo-market))
         (strikes (grid 70 130 31))
         (expiries '((30 "1M") (91 "3M") (365 "1Y") (730 "2Y")))
         (heston (make-engine :cos :process (make-heston :v0 0.04d0 :theta 0.04d0 :kappa 1.5d0
                                                         :xi 0.5d0 :rho -0.7d0)))
         (merton (make-engine :cos :process (make-merton-jump :intensity 0.5d0 :jump-mean -0.1d0
                                                              :jump-vol 0.15d0))))
    (flet ((smiles (engine)
             (mapcar (lambda (e) (destructuring-bind (days label) e
                                   `(,label ,(smile engine m strikes days))))
                     expiries)))
      (panel-grid
       (mapcar #'series-panel
               `((:title "Heston: v0 = theta = 4%, kappa 1.5, xi 0.5, rho -0.7"
                  :xlabel "strike" :ylabel "Black implied vol" :xs ,strikes
                  :series ,(smiles heston))
                 (:title "Merton jumps: 0.5 a year, mean -10%, vol 15%"
                  :xlabel "strike" :ylabel "Black implied vol" :xs ,strikes
                  :series ,(smiles merton))))
       :title "Volatility smiles from model prices (spot 100; flat Black-Scholes vol would be a line)"))))

(defun greeks-demo ()
  "Write every plot of this file; returns their pathnames."
  (list (plot-price-vs-inputs)
        (plot-greeks-vs-spot)
        (plot-greeks-vs-expiry)
        (plot-smiles)))

;;; --------------------------------------------------------------------
;;; Extending fincl-viz: a plot built only from its exported functions
;;; --------------------------------------------------------------------

(define-plot call-price-surface (&key (strike 100) (vol 0.2d0) (rate 0.04d0)
                                      (spots (grid 60 140 33)) (days (grid 7 730 25)))
  "Black-Scholes price of a European call struck at STRIKE over spot x time
to expiry, on flat markets with VOL and RATE.

  (fincl-viz::plot-call-price-surface :vol 0.3d0 :layout '(:height 700))"
  (let* ((days (mapcar #'round days))
         (prices (mapcar (lambda (d)
                           (let ((option (make-option (call strike) (expires (expiring-in d)))))
                             (mapcar (lambda (s)
                                       (price option :black-scholes
                                              (make-bs-market :valuation-date *today* :spot s
                                                              :rate rate :vol vol)))
                                     spots)))
                         days)))
    (panel-grid
     (list (panel (list (surface-trace spots (mapcar (lambda (d) (/ d 365d0)) days) prices
                                       :style `(:colorscale ,(theme-colorscale :sequential))))
                  :xlabel "spot" :ylabel "years to expiry" :zlabel "price"
                  :scene `(:camera (:eye ,(camera-eye 25 -125)))))
     :title (format nil "Call struck at ~A: vol ~,0F%, rate ~,1F%"
                    strike (* 100 vol) (* 100 rate)))))
