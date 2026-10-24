;;;; docgen/docs.lisp
(defpackage #:fincl/docgen
  (:use #:cl #:mgl-pax))

(in-package #:fincl/docgen)
; * Entry point
(defsection @fincl-manual (:title "fincl")
  "fincl is a Common Lisp library for quantitative finance.

**This documentation and the library is a work in progress. API is subject to change
without notice.**"
  (@getting-started section)
  (@options section)
  (@engines section)
  (@processes section))
; * Overview, outline, explanation
(defsection @getting-started (:title "Getting Started")
  "Acquire the dependencies for `fincl` and the library itself and load it.
Assuming you have registered the package locally to quicklisp, you can load
the library

```cl
(ql:quickload :fincl)
(defpackage #:fun-in-fincl (:use #:cl #:fincl))
(in-package #:fun-in-fincl)

;;; Literals for dates and tenors
(named-readtables:in-readtable fincl:syntax)
```

The closest thing to a \"Hello, World!\" of a quantitative finance library
is pricing a European option with the Black-Scholes formula.

```cl
(defconstant +today+ #D\"2026-10-01\")
(defparameter *market*
  (make-bs-market :valuation-date +today+
                  :spot 100 :rate 0.04d0
                  :dividend 0.01d0
                  :vol 0.2d0))
(defparameter *option*
  (make-option (call 110) (expires #T\"1Y\" :from +today+)))
(price *option* (make-engine :black-scholes) *market*)
;; ==>  5.240727867335534d0
;;      NIL
;;     #<ENGINE (make-engine :black-scholes)> 
```")

; * Options
(defsection @options (:title "Options")
  "An option is contract giving the holder the right, but not the obligation,
to exercise a purchase or sale of an underlying asset at an agreed-upon price. Options have finite
lifetime, and carry various exercise rights. e.g. European options can be exercised on the expiration date,
while American options can be exercised at any point before the expiration.

Besides exercise rights, the primary difference of option types is how their payoff is
assessed. The major supported option payoff schemes are presented in this section.
The user may write their
own option payoff schemes (see TODO).

Throughout, $K$ denotes the strike price of the option. $t\\in [0, \\\\T]$
denotes a time between the inception of the option $0$ and the expiration
date $T$ (measured in years), and $S_t$ denotes the price of the underlying
asset at time $t$. $\\phi$ will $\\pm 1$ for calls/puts, respectively."
  (@vanilla section)
  (@asian section)
  (@barrier section)
  (@lookback section)
  (@make-option section))

(defsection @vanilla (:title "Vanilla Options")
  "$$\\max(\\phi (S_{T} - K) , 0).$$

Exercise is European unless the option says otherwise (see `make-option`)."
  (fincl:vanilla-payoff class))

(defsection @asian (:title "Asian Options")
  "With $A$ the average of $S_t$ over the observation dates $t_1,\\dots,t_n$
(arithmetic $\\frac{1}{n}\\sum_i S _{t_i}$ or geometric $(\\prod_i S _{t_i})^{1/n}$):

- fixed strike: $\\max(\\phi (A - K), 0)$
- floating strike: $\\max(\\phi (S_T - A), 0)$

European exercise only."
  (fincl:asian class))

(defsection @barrier (:title "Barrier Options")
  "The vanilla payoff, conditional on the path touching a level $H$ before
$T$, monitored continuously or on a list of dates. With rebate $R$:

- knock-out: the payoff if $H$ is never touched; $R$, paid at the touch, otherwise
- knock-in: the payoff if $H$ is touched; $R$, paid at $T$, otherwise

The type is one of `:down-and-out`, `:down-and-in`, `:up-and-out`,
`:up-and-in`. European exercise only."
  (fincl:barrier class))

(defsection @lookback (:title "Lookback Options")
  "With $m = \\min_t S_t$ and $M = \\max_t S_t$ over the observation dates
(every simulation step by default):

- floating strike: call $S_T - m$, put $M - S_T$
- fixed strike: call $\\max(M - K, 0)$, put $\\max(K - m, 0)$

European exercise only."
  (fincl:lookback class))

(defsection @make-option (:title "make-option")
  "`(make-option clause...)` builds an option from clauses, which may come in
any order. Arguments of a clause are evaluated.

- payoff: `(call K)` or `(put K)`; one is required
- exercise: `(european)` (default), `(american)`, or
  `(bermudan :dates dates)` / `(bermudan :every tenor :from date)`
- `(expires date)`, or `(expires tenor :from date)`; required, except that
  `bermudan :dates` takes the last date
- path feature, optional: `(average :arithmetic :dates dates)` (or
  `:geometric`, `:every tenor :from date`, `:strike-type :floating`),
  `(barrier :down-and-out level :rebate R :monitoring dates)` (continuous
  by default), `(lookback :floating)` (or `:fixed`)
- `(underlying name)` and `(currency name)`, when the market has several

```cl
(make-option (put 95) (american) (expires #T\"1Y\" :from +today+))
(make-option (call 100) (barrier :up-and-out 130 :rebate 2) (expires #D\"2027-06-15\"))
(make-option (call 100) (average :arithmetic :every #T\"1M\" :from +today+)
             (expires #T\"1Y\" :from +today+))
```

`make-option-from-clauses` takes the same clauses as a list, for building
options from data."
  (fincl:make-option macro)
  (fincl:make-option-from-clauses function))

; * Engines
(defsection @engines (:title "Pricing engines")
  "`(price instrument engine market)` returns `(values npv standard-error engine)`.
An instrument is an instance of an instrument: an option, bond, a swap.
An engine is an instance, a keyword designator, a list `(:cos :n-terms 512)`,
a process (priced by the first engine that can), or `:auto`, which tries the
engines below in order of speed and accuracy and falls back to Monte Carlo with
a warning. Build one with `make-engine`, which evaluates its arguments.

```cl
(make-engine :cos :process (make-heston) :n-terms 512)
(price option :auto market)
```

Designators:

- `:black-scholes`: closed forms; European, barriers, geometric Asians
- `:baw`: American
- `:merton-series`: European, Merton jump diffusion
- `:cev`: European, CEV with $\\beta\\le 1$
- `:binomial`: American, Bermudan, GBM
- `:cos`: European, Bermudan, American; any process with a characteristic function
- `:fd`: European, Bermudan, American, barriers; GBM, CEV, local vol, Heston
- `:mc`: any exercise and process that can be simulated"
  (fincl:make-engine function)
  (@black-scholes section)
  (@baw section)
  (@merton-series section)
  (@cev-engine section)
  (@binomial section)
  (@cos section)
  (@fd section)
  (@monte-carlo section))

(defsection @black-scholes (:title "Black-Scholes")
  "Closed forms in the forward, the discount factor and the total variance
to expiry, so they hold under term structures of rates, dividends and vol.
European vanillas; continuous barriers (Reiner-Rubinstein) and discrete
monitoring through the continuity correction of Broadie, Glasserman and Kou;
discrete geometric Asians.

See F. Black and M. Scholes, The pricing of options and corporate liabilities,
_Journal of Political Economy 81_ (1973); E. Reiner and M. Rubinstein, Breaking
down the barriers, _Risk 4_ (1991); M. Broadie, P. Glasserman and S. Kou, A
continuity correction for discrete barrier options, _Mathematical Finance 7_
(1997); A. Kemna and A. Vorst, A pricing method for options based on average
asset values, _Journal of Banking and Finance 14_ (1990)."
  (fincl:black-scholes-engine class))

(defsection @baw (:title "Barone-Adesi-Whaley")
  "Quadratic approximation of the American premium over the European price.
Returns the European price where early exercise is never optimal.

See G. Barone-Adesi and R. E. Whaley, Efficient analytic approximation of
American option values, _Journal of Finance 42_ (1987)."
  (fincl:barone-adesi-whaley-engine class))

(defsection @merton-series (:title "Merton series")
  "European vanillas under Merton jump diffusion as a Poisson-weighted sum of
Black-Scholes prices conditional on the number of jumps.

See R. C. Merton, Option pricing when underlying stock returns are
discontinuous, _Journal of Financial Economics 3_ (1976)."
  (fincl:merton-series-engine class))

(defsection @cev-engine (:title "CEV formula")
  "European vanillas under CEV with $\\beta\\le 1$ and absorption at zero, in
terms of the noncentral $\\chi^2$ distribution.

See R. Schroder, Computing the constant elasticity of variance option pricing
formula, _Journal of Finance 44_ (1989)."
  (fincl:cev-engine class))

(defsection @binomial (:title "Binomial tree")
  "Cox-Ross-Rubinstein tree for GBM. By default the last step is valued with
the Black-Scholes formula to remove the odd-even oscillation (BBS), and the
price is $2v(N)-v(N/2)$ (BBSR). Bermudan dates snap to the nearest step.

- `n-steps`: steps in the tree
- `smooth`: Black-Scholes on the last step
- `extrapolate`: Richardson extrapolation

See J. C. Cox, S. A. Ross and M. Rubinstein, Option pricing: a simplified
approach, _Journal of Financial Economics 7_ (1979); M. Broadie and J. Detemple,
American option valuation: new bounds, approximations, and a comparison of
existing methods, _Review of Financial Studies 9_ (1996)."
  (fincl:binomial-engine class))

(defsection @cos (:title "COS")
  "Fourier-cosine expansion of the density of the log-return, with coefficients
from the characteristic function. Europeans need only the characteristic
function; Bermudans and Americans use the backward recursion, which needs
state-independent log-increments, with the American price extrapolated from
Bermudans of $M, 2M, 4M, 8M$ dates.

- `n-terms`: terms in the expansion
- `truncation`: half-width $L$ of the truncation range, in cumulant scale
- `richardson-dates`: $M$

See F. Fang and C. W. Oosterlee, A novel pricing method for European options
based on Fourier-cosine series expansions, _SIAM Journal on Scientific Computing 31_ (2008), and Pricing early-exercise and discrete barrier options
by Fourier-cosine series expansions, _Numerische Mathematik 114_ (2009)."
  (fincl:cos-engine class))

(defsection @fd (:title "Finite differences")
  "Crank-Nicolson on a log-spot grid concentrated at the strike, with Rannacher
start-up after each payoff discontinuity. Americans and Bermudans by
Ikonen-Toivanen; barriers by truncating the grid or by events on the
monitoring dates. Heston by Hundsdorfer-Verwer ADI on a (log-spot, variance)
grid. Jump processes are refused.

- `n-space`, `n-variance`, `n-time`: grid sizes
- `theta`: time-stepping weight, $\\frac{1}{2}$ is Crank-Nicolson
- `rannacher-steps`: implicit substeps after a discontinuity
- `concentration`, `width`: grid concentration and half-width in standard deviations

See R. Rannacher, Finite element solution of diffusion problems with irregular
data, _Numerische Mathematik 43_ (1984); S. Ikonen and J. Toivanen, Operator
splitting methods for American option pricing, _Applied Mathematics Letters 17_ (2004); K. J. in 't Hout and S. Foulon, ADI finite difference schemes for
option pricing in the Heston model with correlation, _International Journal of Numerical Analysis and Modeling 7_ (2010)."
  (fincl:fd-engine class))

(defsection @monte-carlo (:title "Monte Carlo")
  "Simulates the paths of any process that defines a stepper. Terminal-only
payoffs sample the terminal value exactly where the process allows; the
rest step through a time grid containing every observation date. Americans
and Bermudans by least-squares regression on the continuation value.
Barriers monitored continuously are corrected with the Brownian bridge.
Prices do not depend on the number of threads or chunks.

- `process`, `n-paths`, `n-steps`, `seed`
- `rng`: `:counter` (counter-based Philox), `:sobol` (randomized
  quasi-Monte Carlo) or a `random-state` generator
- `antithetic`: antithetic pairs
- `control-variates`: `:european`, `:geometric-asian`

See P. Glasserman, _Monte Carlo Methods in Financial Engineering_ (2004);
F. A. Longstaff and E. S. Schwartz, Valuing American options by simulation: a
simple least-squares approach, _Review of Financial Studies 14_ (2001);
J. K. Salmon, M. A. Moraes, R. O. Dror and D. E. Shaw, Parallel random
numbers: as easy as 1, 2, 3, _Proceedings of SC11_ (2011); S. Joe and F. Y. Kuo,
Constructing Sobol sequences with better two-dimensional projections, _SIAM Journal on Scientific Computing 30_ (2008)."
  (fincl:monte-carlo-engine class))

; * Pricing
(defsection @pricing (:title "Pricing")
  "Price an instrument with an engine under a market."
  
  (fincl:make-engine function))

; * Actual documentation with commentary
; ** Processes
(defsection @processes (:title "Stochastic processes")
  "A process is a stochastic differential equation. For the
purposes of financial modeling, these are typically **drift-diffusion** processes or **Lévy processes**.
However, `fincl` allows you to define your own (see TODO). 

A general drift-diffusion process has the form
$$dS_t = \\mu(X_t, t) + \\sigma(X_t,t)dW_t,$$
where $\\mu(X_t,t)$ is called the **drift**, and $\\sigma(X_t,t)$ is the **diffusion**.
$W_t$ is a standard Brownian motion.

The Lévy processes add a jump term to drift-diffusion models dictated
by a Poisson process. All processes forms are given under the risk-netural
measure, where $r$ is the risk-free rate, $q$ is an equity dividend."
  (@diffusions section)
  (@levy-procs section))
; *** Drift-diffusion models
(defsection @diffusions (:title "Drift-diffusion processes")
  (@gbm section)
  (@cev section)
  (@local-vol section)
  (@heston section)
  (@garch section)
  (@hull-white section))
(defsection @gbm (:title "Geometric Brownian motion")
  "The classical drift-diffusion model of constant volatility.
$$dS_t = (r - q) S_t \\,dt + \\sigma S_t\\,dW_t.$$"
  (fincl:gbm class)
  (fincl:make-gbm function))

(defsection @cev (:title "Constant elasticity of variance")
  "$$dS_t = (r-q)\\,S_t\\,dt + \\sigma S_t^{\\beta}\\,dW_t$$

- $\\beta$: elasticity of the diffusion in $S$
- $\\sigma$: scale of the diffusion

See J. C. Cox, The constant elasticity of variance option pricing model (1975)."
  (fincl:cev class)
  (fincl:make-cev function))

(defsection @local-vol (:title "Dupire local volatility")
  "$$\\frac{dS_t}{S_t} = (r-q)\\,dt + \\sigma_{\\mathrm{loc}}(S_t,t)\\,dW_t$$

where $\\sigma_{\\mathrm{loc}}$ is the local volatility of an implied total variance surface
$w(k,T)$, $k =\\ln (\\\\K / \\\\F (\\\\T) )$, in Gatheral's form of Dupire's formula,

$$\\sigma_{\\mathrm{loc}}^2 =\\frac{\\partial _T w}{1 - \\frac{k}{w}\\partial _k w + \\frac{1}{4}\\left(-\\frac{1}{4} - \\frac{1}{w} + \\frac{k^2}{w^2}\\right)(\\partial _k w)^2+ \\frac{1}{2}\\partial _{kk} w}.$$

The surface is the market's unless one is given."
  (fincl:local-vol class)
  (fincl:make-local-vol function))

(defsection @heston (:title "Heston")
  ""
  (fincl:heston class)
  (fincl:make-heston function)
  (fincl:make-stepper function))

(defsection @garch (:title "NGARCH")
  "$$\\ln S_t = \\ln S _{t-1} + (r-q)\\Delta - \\frac{1}{2} h _t + \\sqrt{h _t}\\,z _t,
\\qquad h _{t+1} = \\omega + \\beta h _t + \\alpha\\, h _t\\,(z _t-\\gamma )^2$$

with $z_t$ standard normal and period $\\Delta$.

- $\\omega$: intercept of the variance
- $\\beta$: persistence of the variance
- $\\alpha$: sensitivity of the variance to shocks
- $\\gamma$: leverage, raising the next variance more after negative shocks

See J.-C. Duan, The GARCH option pricing model, _Mathematical Finance 5_ (1995)."
  (fincl:garch class)
  (fincl:make-garch function))

(defsection @hull-white (:title "Hull-White")
  "$$dr_t = \\left(\\theta(t) - a\\,r_t\\right)dt + \\sigma\\,dW_t$$

- $a$: speed of mean reversion
- $\\sigma$: volatility of the short rate
- $\\theta(t)$: fitted to the initial yield curve

See J. Hull and A. White, Pricing interest-rate derivative securities, _Review of Financial Studies 3_ (1990)."
  (fincl:hull-white class)
  (fincl:make-hull-white function))

; *** Lévy processes
(defsection @levy-procs (:title "Lévy processes")
  "Lévy processes add jump terms to drift-diffusion models, accounting
for random jumps seen in the price movement of various instruments. Throughout,
$dN_t$ will denote a Poisson process with rate $\\lambda$."
  (@merton section)
  (@kou section)
  (@variance-gamma section)
  (@nig section)
  (@cgmy section)
  (@bates section))

(defsection @merton (:title "Merton Jump Diffusion")
  "$$\\frac{dS_t}{S_t} = (r-q-\\lambda k)\\,dt + \\sigma\\,dW_t + (e^{Y}-1)\\,dN_t,
\\qquad Y \\sim \\mathcal{N}(\\mu_J, \\sigma_J^2),$$

with $k = e^{\\mu_J + \\sigma_J^2/2} - 1$.

- $\\sigma$: volatility of the diffusion
- $\\lambda$: jump intensity
- $\\mu_J$, $\\sigma_J$: mean and standard deviation of the log jump $Y$

See R. C. Merton, Option pricing when underlying stock returns are discontinuous,
_Journal of Financial Economics 3_ (1976)."
  (fincl:merton-jump class)
  (fincl:make-merton-jump function))

(defsection @kou (:title "Kou Jump Diffusion")
  "Kou's jump-diffusion process dictated by 
$$\\frac{dS_t}{S_t} = (r-q-\\lambda)\\, dt + \\sigma \\,dW_t + (e^{Y_t} - 1)\\,dN_t.$$
See S.G. Kou, \"A jump-diffusion model for option pricing\""
  (fincl:kou-jump class))

(defsection @variance-gamma (:title "Variance Gamma")
  "$$S_t = S_0\\exp\\left((r-q+\\omega)\\,t + \\theta \\, G_t + \\sigma \\, W_{G_t}\\right),
\\qquad \\omega = \\frac1\\nu\\ln\\left(1 - \\theta\\nu - \\tfrac12\\sigma^2\\nu\\right),$$

where $G$ is a gamma process with $\\mathbb{E}[G_t] = t$ and $\\operatorname{Var}[G_t] = \\nu t$.

- $\\theta$: drift of the Brownian motion, setting the skewness
- $\\sigma$: volatility of the Brownian motion
- $\\nu$: variance rate of the gamma clock, setting the kurtosis

See D. B. Madan, P. P. Carr and E. C. Chang, The variance gamma process and option
pricing, _European Finance Review 2_ (1998)."
  (fincl:variance-gamma class)
  (fincl:make-variance-gamma function))

(defsection @nig (:title "Normal Inverse Gaussian")
  "$$S_t = S_0\\exp\\left((r-q+\\omega)\\,t + \\beta V_t + W_{V_t}\\right),
\\qquad \\omega = \\delta\\left(\\sqrt{\\alpha^2-(\\beta+1)^2} - \\gamma\\right),$$

where $V$ is an inverse Gaussian process with $\\mathbb{E}[V_t] = \\delta t/\\gamma$ and
$\\gamma = \\sqrt{\\alpha^2-\\beta^2}$.

- $\\alpha$: heaviness of the tails, lighter as it grows
- $\\beta$: asymmetry, with $|\\beta| < \\alpha$
- $\\delta$: scale

See O. E. Barndorff-Nielsen, Processes of normal inverse Gaussian type, _Finance and Stochastics 2_ (1998)."
  (fincl:nig class)
  (fincl:make-nig function))

(defsection @cgmy (:title "CGMY")
  "$$S_t = S_0\\exp\\left((r-q+\\omega)\\,t + L_t\\right),
\\qquad \\omega = -C\\,\\Gamma(-Y)\\left((M-1)^Y - M^Y + (G+1)^Y - G^Y\\right),$$

where $L$ is a pure-jump process with Lévy density

$$\\nu(x) = C\\,\\frac{e^{-G|x|}}{|x|^{1+Y}}\\ (x<0), \\qquad
\\nu(x) = C\\,\\frac{e^{-Mx}}{x^{1+Y}}\\ (x>0).$$

- $C$: overall activity
- $G$, $M$: exponential decay of the left and right tails, with $M > 1$
- $Y < 2$: fine structure of the jumps

See P. Carr, H. Geman, D. B. Madan and M. Yor, The fine structure of asset returns,
_Journal of Business 75_ (2002)."
  (fincl:cgmy class)
  (fincl:make-cgmy function))

(defsection @bates (:title "Bates")
  "$$\\frac{dS_t}{S_t} = (r-q-\\lambda k)\\,dt + \\sqrt{v_t}\\,dW^1_t + (e^{Y}-1)\\,dN_t,
\\qquad dv_t = \\kappa(\\theta - v_t)\\,dt + \\xi\\sqrt{v_t}\\,dW^2_t,$$

with $\\mathrm{corr}(dW^1_t, dW^2_t) = \\rho$, $Y \\sim \\mathcal{N}(\\mu_J, \\sigma_J^2)$ and
$k = e^{\\mu_J + \\sigma_J^2/2} - 1$.

- $v_0$, $\\theta$: initial and long-run variance
- $\\kappa$: speed of mean reversion of the variance
- $\\xi$: volatility of the variance
- $\\rho$: correlation of the two Brownian motions
- $\\lambda$, $\\mu_J$, $\\sigma_J$: jump intensity, and mean and standard deviation of the log jump $Y$

See D. S. Bates, Jumps and stochastic volatility: exchange rate processes implicit in
Deutsche mark options, _Review of Financial Studies 9_ (1996)."
  (fincl:bates class)
  (fincl:make-bates function))







;;; The sidebar can be hidden with a button. The style and script go into every
;;; page's head; the button is added once the page has loaded.
(defparameter *sidebar-head* "<style>
  html.toc-collapsed #toc { display: none; }
  #toc-toggle { position: fixed; top: 0.5em; left: 0.5em; z-index: 10;
                font: inherit; cursor: pointer; }
</style>
<script>
  document.addEventListener('DOMContentLoaded', function () {
    var button = document.createElement('button');
    button.id = 'toc-toggle';
    button.textContent = 'Contents';
    button.setAttribute('aria-expanded', 'true');
    button.addEventListener('click', function () {
      var collapsed = document.documentElement.classList.toggle('toc-collapsed');
      button.setAttribute('aria-expanded', String(!collapsed));
    });
    document.body.appendChild(button);
  });
</script>"
  "Markup added to the head of every generated page: a style and a script that
add a button hiding the sidebar.")

(defun build-manual (&key (target-dir #p"docs/api/"))
  "Generate the HTML manual into TARGET-DIR with the collapsible sidebar."
  (let ((*document-html-head* *sidebar-head*))
    (update-asdf-system-html-docs @fincl-manual :fincl :target-dir target-dir)))
