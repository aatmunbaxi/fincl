;;;; docgen/docs.lisp
(defpackage #:fincl/docgen
  (:use #:cl #:mgl-pax))

(in-package #:fincl/docgen)

(defsection @fincl-manual (:title "fincl")
  "fincl is a Common Lisp library for quantitative finance."
  (@pricing section)
  (@processes section))

(defsection @pricing (:title "Pricing")
  "Price an instrument with an engine under a market."
  (fincl:price function)
  (fincl:make-engine function)
  (fincl:make-option macro))
(defsection @processes (:title "Stochastic processes")
  "Generally speaking, a process is a stochastic differential equation. For the
purposes of financial modeling, these are typically **drift-diffusion** processes of
the form
$$dS_t = \\mu(X_t, t) + \\sigma(X_t,t)dW_t,$$
where $\\mu(X_t,t)$ is called the **drift**, and $\\sigma(X_t,t)$ is the **diffusion**.
$W_t$ is a standard Brownian motion.

Other useful SDE are the Lévy processes, which adds a jump term to drift-diffusion
models dictated by a Poisson process. All processes forms are given under the risk-netural
measure, where $r$ is the risk-free rate, $q$ is an equity dividend."
  (@diffusions section)
  (@levy-procs section))

(defsection @diffusions (:title "Diffusions")
  (@gbm section)
  (@heston section))
(defsection @levy-procs (:title "Lévy processes")
  "Lévy processes add jump terms to drift-diffusion models, accounting
for random jumps seen in the price movement of various instruments. Throughout,
$dN_t$ will denote a Poisson process with rate $\\lambda$."
  (@kou section))

(defsection @kou (:title "Kou Jump Diffusion")
  "Kou's jump-diffusion process dictated by 
$$\\frac{dS_t}{S_t} = (r-q-\\lambda)\\, dt + \\sigma \\,dW_t + (e^{Y_t} - 1)\\,dN_t.$$
See [S.G. Kou, \"A jump-diffusion model for option pricing\"](https://www.columbia.edu/~sk75/MagSci02.pdf)"
  (fincl:kou-jump class))


(defsection @gbm (:title "Geometric Brownian motion")
  "dS = (r - q) S dt + sigma S dW. Exact in log space, so Monte Carlo
terminal samples have no discretization error.

$$dS = (r - q) S \\,dt + \\sigma S\\,dW$$"
  (fincl:gbm class)
  (fincl:make-gbm function))
(defsection @stochastic-volatility (:title "Stochastic Volatility Processes")
  (@heston section))
(defsection @heston (:title "Heston")
  "A two-factor model with stochastic variance. The default scheme is
Andersen's quadratic-exponential method."
  (fincl:heston class)
  (fincl:make-heston function)
  (fincl:make-stepper function))
