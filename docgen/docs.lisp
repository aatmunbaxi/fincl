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
  (@markets section)
  (@options section)
  (@engines section)
  (@simulation section)
  (@processes section)
  (@portfolios section)
  (@extending section))
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
  (make-market (valued-on +today+)
               (underlying :spx (spot 100) (dividend 0.01d0) (vol 0.2d0))
               (curve :usd-ois (rate 0.04d0))))
(defparameter *option*
  (make-option (call 110) (expires #T\"1Y\" :from +today+)))
(price *option* (make-engine :black-scholes) *market*)
;; ==>  5.240727867335534d0
;;      NIL
;;     #<ENGINE (make-engine :black-scholes)> 
```")

; * Markets
(defsection @markets (:title "Dates and markets")
  "Dates and tenors are read with `#D\"2026-10-01\"` and `#T\"3M\"` in the
`fincl:syntax` readtable; `date+` and `date<` move and compare dates.

A market holds the spots, curves, dividends and volatilities that engines
read. It has a valuation date, there is no hidden \"today\", and it is
immutable: a scenario is a new market (see `bump`). `make-market` builds one
from clauses, with several underlyings and curves; `make-bs-market` builds the
common case of one underlying, one currency and one curve, with flat inputs.
The `dividends` clause of an underlying gives discrete cash and proportional
dividends. Its argument is a quoted list of
`(ex-date amount [:kind :cash|:proportional] [:pay-date date])`, or a
`dividend-schedule`. The clause `(fixings index entries)` gives past index fixings.

```cl
(make-market (valued-on #D\"2026-10-01\")
             (underlying :spx (spot 5000) (currency :usd)
                         (dividend 0.015d0) (vol 0.18d0)
                         (dividends '((\"2027-02-10\" 0.82d0))))
             (curve :usd-ois (rate 0.04d0))
             (discounting :usd :usd-ois))
```

`(documentation 'make-market 'function)` lists every clause."
  (fincl:date class)
  (fincl:tenor class)
  (fincl:date+ function)
  (fincl:date< function)
  (fincl:market class)
  (fincl:make-market macro)
  (fincl:make-market-from-clauses function)
  (fincl:make-bs-market function)
  (fincl:valuation-date generic-function)
  (fincl:dividend-schedule class)
  (fincl:make-dividend-schedule function)
  (fincl:bump function))

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
  (fincl:option class)
  (fincl:option-payoff function)
  (fincl:payoff class)
  (fincl:payoff-value generic-function)
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
  (@pricing section)
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

; * Simulation
(defsection @simulation (:title "Simulation")
  "`simulate` is a construct that allows one to simulate paths of a process
or processes, and evaluates lisp forms on a list of days in the simulation.
Paths come from the process protocol that the Monte Carlo engine uses. The
lisp forms evaluated and tracked on these days can be `:state` variables
from the process, or symbols declared in `:start` and `:hold`. This allows trading
and hedging strategies to be simulated, where one can write trackers for
PnL, max drawdown, etc. Because the maximum resolution of the simulation
is daily actions, these simulations cannot model complex intraday statistics
and strategies. 


```cl
(simulate (process market dates &key n-paths seed rng antithetic n-steps max-dt
                                     correlation keep market-from-state)
  (:accumulate (var init) ...)          ; per-path doubles
  (:hold (var init) ...)                ; per-path objects
  (:portfolio var [init] &key record engine cash cash-curve collect)  ; a per-path book, one clause each
  (:state var name [underlying])        ; a state component, e.g. :variance
  (:start [([market] &key time)] form ...)          ; at the valuation date
  (:every (date [market] &key time dt) form ...)    ; at each of DATES
  (:on date-list (i [market] &key time dt) form ...) ; at the dates of a list
  (:final (name form) ...))             ; named per-path outputs
```

Within `:start`, `:every` and `:on` bodies, `(record name value)` adds the
real VALUE to the statistic NAME (a keyword) at the current date, and returns
VALUE; `(stop)` ends the path, after which its later dates run no clauses and
record nothing (`:final` still runs); `(previous var)` is a `:state`
variable's value at the previous date. All are matched by name in any
package. `time` and `dt` bind the date's market time and its step from the
previous date.
MARKET is the date's market within the simulation, holding its curves,
surfaces and dividends; it is built lazily, only doing so if the forms use it.

**Recording.** A statistic is over everything recorded under NAME at a date,
so it summarizes the paths at that date. Recording at the same date more than
once under one name pools the values, and a date where nothing was recorded
holds NaN (a count of 0). `record` in `:start` goes to the valuation date.
Recorded values are streamed into summarizing statistics, each (name,
date) keeps the count, mean, standard deviation, minimum and maximum, and
`:keep :quantiles` adds quantiles. Read the results with
`(simulation-statistic result name statistic)`, which gives a vector over
`simulation-dates`, and list the names with `simulation-recorded-names`. Use
`record` for a quantity followed through time, such as a hedging balance or a
spot, and a `:final` output for the per-path quantity at the end such as
profit-loss, since `:final` keeps one value per path. In a `:portfolio`
clause the key `:record` lists `:value` and Greek keys to record each date under
the names
`(var-keyword key)`, e.g. `(:book :delta)` for the book `book` and the key
`:delta`. Several `:portfolio` clauses run several books on the same paths,
so their outputs are paired; without `:final` each book's value is an output
named by its variable.

**Outputs.** Each `:final` entry is a number per path for each path. All outputs
come from the same paths, so a quantity computed from several (a difference, a
ratio, an indicator) is a paired estimate on common random numbers:
`simulation-summary` gives its mean and standard error, with antithetic pairs
averaged first, and the standard error of a difference $X-Y$ is
$\\sqrt{(\\sigma_X^2+\\sigma_Y^2-2\\rho\\sigma_X\\sigma_Y)/n}$, small when the
outputs are correlated. For example, the following code simulates a
delta-hedging strategy against a short call position, comparing `:hedged`
strategy against an `:unhedged` strategy. At each date, the underlying is traded
to neutralize the net delta exposure of the holdings.

```cl
(simulate ((make-gbm) *market* *dates* :n-paths 20000)
  (:accumulate (shares 0d0) (cash 0d0) (premium 0d0))
  (:state s :spot)
  (:start (market)
    ;; Sell the call option to open the position
    (setf premium (price call :black-scholes market))
    (incf cash premium))
  (:every (date market)
    (when (date< date expiry)
      (let ((delta (getf (greeks call :black-scholes market :which '(:delta)) :delta)))
        ;; Neutralize delta exposure
        (decf cash (* (- delta shares) s))
        (setf shares delta))))
  ;; Return final balances
  (:final (:hedged (+ cash (* shares s) (- (payoff-value (option-payoff call) s))))
          (:unhedged (- premium (payoff-value (option-payoff call) s)))))
```

**Collection.** `record` keeps count, mean, variance, min and max per name and
date, merged in chunk order; `:keep :quantiles` keeps the values for
quantiles, `:keep (:paths k)` the first $k$ paths' states, held objects
(whose markets `path-markets` rebuilds) and recorded values
(`simulation-path-record`), `:keep :states` every path's.

**Several underlyings.** PROCESS may be a list of `(underlying process)` pairs,
each stepped on its own view. `:correlation` `((u1 u2 rho) ...)` correlates
the Brownian motions $W^{(1)}, W^{(2)}$ driving the two spots: with
`spot-loadings` $a, b$ ($W^{(1)}=\\sum_k a_k Z^{(1)}_k$, unit norm), factors
$i$ and $j$ are correlated at $\\rho\\,a_i b_j$, so
$\\operatorname{corr}(W^{(1)}, W^{(2)})=\\rho$ whatever the processes' own
factors (Heston's variance shock included). A square matrix over every factor
gives full control.

```cl
(simulate (((:aapl (make-heston)) (:msft (make-gbm))) market dates
           :correlation '((:aapl :msft 0.6d0)) :keep (:paths 20))
  (:state a :spot :aapl)
  (:state m :spot :msft)
  (:every (date) (record :basket (+ (* 2 a) m))))
```

Clauses must not start parallel work: paths already run in parallel, and
pricing inside a clause is serial.

See P. Glasserman, _Monte Carlo Methods in Financial Engineering_ (2004);
D. Bertsimas, L. Kogan and A. W. Lo, When is time continuous?, _Journal of
Financial Economics 55_ (2000)."
  (fincl:simulate macro)
  (fincl:map-simulated-paths function)
  (fincl:market-at-state generic-function)
  (fincl:simulation-underlyings function)
  (fincl:spot-loadings generic-function)
  (fincl:simulation-state-names function)
  (fincl:simulation-result class)
  (fincl:simulation-output-names function)
  (fincl:simulation-values function)
  (fincl:simulation-mean function)
  (fincl:simulation-standard-error function)
  (fincl:simulation-quantile function)
  (fincl:simulation-summary function)
  (fincl:simulation-statistic function)
  (fincl:simulation-recorded-names function)
  (fincl:simulation-dates function)
  (fincl:simulation-held function)
  (fincl:simulation-path-record function)
  (fincl:path-markets function)
  (@simulated-components section)
  (@books-with-cash section))

(defsection @simulated-components (:title "Simulated market components")
  "By default only the spots move along a path: on each date the market is
rebuilt with the simulated spots, and its curves, vol surfaces, dividends and
fixings stay as they were on day 0. A simulation can also rebuild those
other parts on each date, either from a process or from a formula.

Two words are used below. The *world* is what determines the market on each
date: the processes stepped along the paths and the `:derive` forms. The
*desk* is the clause code: what is held, how it is priced, when it is
hedged, and which model it prices with. The desk reads the world's markets;
nothing the desk does changes the paths.

**What a run measures.** Every process here is risk-neutral: the average
discounted price of anything the world prices stays equal to today's price.
A strategy whose prices agree with the world therefore has an average
discounted P&L of 0. A run measures the spread of the P&L (hedging error),
the cost of a decision rule (when to exercise, when to call), exposure and
its tails, and what the desk gains or loses when its prices differ from the
world's. For example, a 2-year at-the-money straddle marked by
Black-Scholes on a 20% surface, in a Heston world with vol of vol 0.5, is
worth 22.51 to the desk and 19.75 in the world, and its average mark falls
from the first to the second over its life.

**Targets.** A component is named in the factor syntax of `bump`:
`(:spot u)`, `(:vol u)`, `(:dividend u)`, `(:dividend-schedule u)` and
`(:curve id)`; a bare keyword `u` means `(:spot u)`. `market-component`
reads one from a market.

**Legs.** PROCESS may be a list of legs `(target process &rest options)`,
each stepped on its own; the options go to `structure-at-state`, which turns
the leg's state on a date into its component. `process-targets` lists the
kinds a process can drive. `:correlation` and `(:state var name leg)` name a
leg by its target or, for a spot, its underlying.

```cl
(simulate ((((:spot :spx) (make-heston))
            ((:curve :usd-ois) (make-hull-white :a 0.03d0 :sigma 0.008d0) :knots tenors))
           market dates :correlation (((:spot :spx) (:curve :usd-ois) 0.2d0)))
  ...)
```

**Formulas.** `(:derive target ([market [date]]) form ...)` sets a component
on every date from the date, the market built so far and the `:state`
variables. Its forms see nothing else of the path (no `:accumulate` or
`:hold` variable, no `record`), so a component depends only on the date and
the simulated states, and `path-markets` rebuilds it exactly.

```cl
(:derive (:curve :usd-libor-3m) (market)      ; a basis over a simulated curve
  (bump-structure (market-component market '(:curve :usd-ois)) '(:absolute 0.0015d0)))
(:derive (:vol :spx) (market date)            ; a surface per date from a table
  (gethash date surfaces))
```

**Hull-White curves.** A Hull-White leg simulates the short rate
$r = x + \\alpha(t)$, $\\alpha(t) = f(0,t) + \\frac{\\sigma^2}{2a^2}(1-e^{-at})^2$,
and its integral exactly: over a step $\\Delta$,
$x_{t+\\Delta} = x_t e^{-a\\Delta} + \\varepsilon_1$ and
$\\int_t^{t+\\Delta} x\\,ds = x_t B(\\Delta) + \\varepsilon_2$, $B(\\Delta) = (1-e^{-a\\Delta})/a$,
with $(\\varepsilon_1,\\varepsilon_2)$ Gaussian,
$\\operatorname{Var}\\varepsilon_1 = \\frac{\\sigma^2}{2a}(1-e^{-2a\\Delta})$,
$\\operatorname{Var}\\varepsilon_2 = \\frac{\\sigma^2}{a^2}\\big(\\Delta - 2B(\\Delta) + \\frac{1-e^{-2a\\Delta}}{2a}\\big)$,
$\\operatorname{Cov} = \\frac{\\sigma^2}{2}B(\\Delta)^2$. The curve on a date is
the model's own bond price function $\\tau \\mapsto P(t,t+\\tau\\mid r_t)$
(`state-discount-bond`), exact at every maturity and evaluated only when a
price asks for it; with `:knots` (tenors) it is those bond prices
log-linear in discount factors between the knots, as before task 33, and only
that curve has a `snapshot-form`. Every rates instrument prices on either
unchanged. `market-curve-discount` reads a curve of a market with several
underlyings. `(path-discount [curve])` in a clause is
$\\exp(-\\int_0^t r\\,ds)$ on the path when a leg simulates that curve, and
today's discount factor otherwise, so its average is today's discount
factor. `:fix (list index ...)` records each index's fixing on each date from
that date's simulated curve, and later markets on the path carry them;
pricing needs every fixing date among DATES (`fixing-dates`).

**Derived surfaces.** `rescale-vol-structure` keeps a surface's smile and
sets its at-the-money level to a function $a(\\tau)$:
$w'(k,\\tau) = w(k,\\tau)\\,a(\\tau)/w(0,\\tau)$ in total variance at
log-moneyness $k$. Driven by a simulated Heston variance $v$,
$a(\\tau) = \\theta\\tau + (v-\\theta)\\frac{1-e^{-\\kappa\\tau}}{\\kappa}$, the
expected variance to $\\tau$, so a shock to $v$ fades over longer maturities
as the model says. This level is the variance-swap level, a little above
Heston's own implied variance when the vol of vol is not 0; with vol of vol 0
the two agree.

```cl
(simulate (heston market dates)
  (:state v :variance)
  (:derive (:vol :spx) (market)
    (rescale-vol-structure (market-component market '(:vol :spx))
                           (lambda (tau) (+ (* theta tau) (* (- v theta) (/ (- 1 (exp (- (* kappa tau)))) kappa))))))
  (:every (date market)
    (record :straddle (+ (price call :black-scholes market) (price put :black-scholes market)))))
```

**The desk's model.** The library never refits a model. A desk that refits
keeps its model in `:hold` and calls `calibrate` on the date's market, for
example to `(surface-quotes market :spx options)`, the options quoted at the
market's vols. Calibration inside a clause is serial, so results do not
depend on the worker count. One warm-started Heston refit to six quotes
with COS takes about 0.3 s, so a weekly refit over a year at 1,000 paths
takes about four hours on one worker.

```cl
(:hold (desk heston-0))
(:every (date market)
  (setf desk (handler-bind ((convergence-failure #'use-best))
               (calibrate desk market (surface-quotes market :spx options) :engine :cos)))
  (record :mark (price straddle (make-engine :cos :process desk) market)))
```

**Writing a component model.** A process drives a non-spot component when
it lists the kind in `process-targets`, steps with `make-stepper` and
defines `structure-at-state`; a curve process that names a state component
`:integral` (the integral of its short rate) is discounted by
`path-discount`. A `:derive` form can also move a component by an
independent factor, such as a random level scaling every implied vol by
$e^x$. Then the average surface rises (with $\\operatorname{Var} x = 0.04$
after a year, every vol averages 2% above today's), the stock's own
volatility does not follow the surface, a shock moves all maturities
alike, and a parametric surface (SSVI) changes shape as well as level. The
library ships only components computed from the world's own state.

**Pitfalls.**

- *Rates move together.* Hull-White has one source of randomness, so every
  maturity of the curve moves in the same direction: over a month the 2-year
  and 10-year zero rates move with correlation 1.00. A trade that profits
  when the curve steepens shows almost no P&L, and hedging a 7-year bond with
  2- and 10-year ones, rebalanced 24 times over 19 months, leaves a residual
  of about 0.01% of its value.
  Neither says anything about the real trade.
- *The stock does not feel the rates.* The stock is stepped on today's rates,
  not on the rates simulated beside it. Uncorrelated with them, its average
  discounted price stays at today's spot; correlated, it does not: at
  correlation 0.5 with a Hull-White rate ($\\sigma = 1\\%$) the average
  discounted price of a 100 stock after 5 years is 98.8.
- *A derived surface keeps today's shape.* `rescale-vol-structure` moves the
  level of the surface and keeps today's smile: if the 1-year 90-strike vol is
  1.7 points above the at-the-money 20%, it is 2.5 points above an
  at-the-money 30%. A strategy that depends on the skew changing is not
  captured.
- *A refit changes the desk, not the world.* Refitting a model in a clause
  changes the prices and hedges the clause sees. The stock's paths are the
  same, bit for bit, with or without the refit.

**Glossary.**

- *World*: the legs and `:derive` forms, which determine the market on each
  date.
- *Desk*: the clause code, which prices, hedges and refits on those markets.
- *Target*: the name of one market component, such as `(:curve :usd-ois)`.
- *Derived component*: a component set on each date by a `:derive` form.

See J. Hull and A. White, Pricing interest-rate-derivative securities, _The
Review of Financial Studies 3_ (1990); P. Glasserman, _Monte Carlo Methods in
Financial Engineering_ (2004), section 3.3; R. Ahmad and P. Wilmott, Which
free lunch would you like today, sir?, _Wilmott Magazine_ (2005)."
  (fincl:market-component function)
  (fincl:process-targets generic-function)
  (fincl:structure-at-state generic-function)
  (fincl:fixing-dates function)
  (fincl:state-discount-bond generic-function)
  (fincl:market-curve-discount function)
  (fincl:rescale-vol-structure function)
  (fincl:surface-quotes function))

(defsection @books-with-cash (:title "Books with cash")
  "A `:portfolio` given `:cash x` (or holding a `cash-account` position in
its INIT) is self-financing: cash is a position of `x` units of a
`cash-account`, priced at 1 under any engine, so the book's value, Greeks,
snapshots and P&L explain include it. Negative cash is borrowing at the
same rate; there is no credit limit, transaction cost or margin.

**On each date** $t_k$, $k \\ge 1$, of a path still running, in order:

1. *Interest.* The balance is multiplied by $G_k = D_{k-1}/D_k$, $D$ the
   path's discount on the curve of the cash (`:cash-curve id`, default the
   currency's discount curve): $e^{\\int_{t_{k-1}}^{t_k} r\\,ds}$ when a leg
   simulates that curve, so the discounted balance is constant between
   trades, and $P(0,t_{k-1})/P(0,t_k)$ on the initial curve otherwise.
2. *Income.* Each position pays its quantity times its income over
   $(t_{k-1}, t_k]$ into cash, for the kinds in `:collect` (default `:all`):
   - a holding, its discrete dividends going ex in the interval, on the
     ex-date: a cash dividend $D$ its price drop $D\\,P(t_{pay})/P(t_{ex})$,
     so the drop and the cash cancel and no receivable is carried; a
     proportional one $\\delta$ times the spot expected just before the drop
     (`:discrete-dividend`); and its continuous yield to first order,
     $S_{k-1}(e^{q\\Delta t}-1)$ (`:continuous-dividend`);
   - a bond, its coupons (`:coupon`) and redemption (`:redemption`); a swap
     or FRA, its signed flows (`:coupon`), on their payment dates;
   - an option, its payoff at expiry (`:expiry`), which must be one of the
     dates (early exercise is clause code: `trade` it to 0, `deposit` the
     intrinsic value);
   - cash, caps, floors and swaptions nothing.

   A flow paid at $t_p < t_k$ is grown to $t_k$ by $P(0,t_p)/P(0,t_k)$
   without a leg on the cash's curve, exactly, and credited at face with
   one: an error of about the amount times $r(t_k - t_p)$, at most 0.33% of
   the flow for monthly dates at 4%. Then the bonds, swaps and FRAs whose
   last flow is paid and the options that expired leave the book.
3. The `:every` and `:on` clauses, in order. `(trade book instrument &key by
   to engine)` buys or sells in BOOK at the date's price (ENGINE, else the
   position's engine, else the book's `:engine`, with `price-portfolio`'s
   fallbacks), paid from its cash, and returns the cash paid; a position sold
   to 0 leaves the book, and a price that fails signals. `(deposit book x)`
   and `(withdraw book x)` move cash in and out. `adjust-position` stays a free change of quantity.
4. The book's `:record` keys, `:value`, `:cash` and Greeks.

At `:start` only steps 3 and 4 run; after `stop` nothing accrues.

*A negative dividend yield as a drift.* A user giving an equity $q < 0$ to
make it grow faster than the rate is, in a book, debited $|q| S\\,\\Delta t$ a
period as a continuous dividend: `:collect '(:discrete-dividend :coupon
:redemption :expiry)` leaves it out, and the yield then acts only on the
spot's drift.

A retirement drawdown, 60% in an equity and 40% in a 6-year par bond rolled
monthly, 5% a year withdrawn, the bond on the exact Hull-White curve:

```cl
(simulate ((((:curve :usd) (make-hull-white :a 0.05d0 :sigma 0.01d0)) (:eq (make-gbm)))
           market dates :n-paths 20000 :keep (:paths 10)
           :correlation ((:eq (:curve :usd) 0.2d0)))
  (:portfolio book (make-portfolio \"60/40\") :cash 1 :engine :discounting
              :collect '(:discrete-dividend :coupon :redemption) :record (:value))
  (:hold (bond nil))
  (:every (date market)
    (withdraw book (/ 0.05d0 12))
    (when bond (trade book bond :to 0))               ; sell last month's bond
    (let ((v (price book :discounting market)))       ; cash and the equity
      (if (<= v 0)
          (stop)
          (progn
            (setf bond (par-bond date 6 (par-rate market 6)))   ; user code
            (trade book (make-holding :underlying :eq) :to (/ (* 0.6d0 v) (market-component market '(:spot :eq))))
            (trade book bond :to (/ (* 0.4d0 v) (price bond :discounting market)))))))
  (:final (:ruined (if (<= (price book :discounting market) 0) 1d0 0d0))))
```

A book with bonds prices with `:discounting`: holdings and cash ignore the
engine, and `:auto` is refused for rates instruments. `(paths result :record
'(:book :value))` in `fincl-viz` draws the book's value on the kept paths."
  (fincl:cash-account class)
  (fincl:make-cash-account function))

; * Plotting simulations
(defsection @plotting (:title "Plotting simulations")
  "`fincl-viz` is an optional, developer-oriented companion system that draws
the result of a `simulate` call. It is not part of `fincl`. Its interface is less settled
than the library's and may change without notice. It draws only what
`simulate` produces; it does not plot prices, vol surfaces or curves.

**Prerequisites.** Building a figure needs only Lisp: `(ql:quickload
:fincl-viz)` adds the system and its dependency `jzon`.
A figure is a plist `(:data traces :layout plist :config plist)`
holding [Plotly](https://plotly.com/) attributes, and `plot` writes it as a
JSON file (by default `/tmp/fincl-viz/NAME.json`, see `*output-directory*`).
Turning that file into a picture is done by `viz/render.py`, which needs
Python 3 with the `plotly` package and, for static images (PNG, SVG), the
`kaleido` package. `*display*` chooses the output: `:png` (the default),
`:svg`, `:html`, `:json` (no rendering, no Python needed) or `:browser`.
The repository's Nix development shell (`nix develop`) provides Python,
plotly and kaleido; outside it you must install them yourself, and the
fincl-viz test suite is the way to check that your set-up renders. Kaleido
versions differ in how they find a browser engine, so image export may need
adjustment on your machine. `plot-figure` returns the figure without writing
any file.

**Using.** Plot layers of a `simulation-result`. Layers read the result
through its accessors and never simulate, so the `simulate` call must have
kept what a layer draws: `:keep (:paths k)` for paths (at most 30 are drawn),
`:keep :quantiles` for bands and quantile lines, `(record name value)` for
statistics by date, and a `:final` output for distributions of final values. A layer whose data
is missing signals `missing-plot-data`, which names the clause to add.

```cl
(ql:quickload :fincl-viz)

(defparameter *r*
  (simulate ((make-gbm) *mkd* *dates* :n-paths 5000
                                     :keep (:quantiles (:paths 30)))
    (:state s :spot)
    (:every (date) (record :spot s))
    (:final (:pnl (- s 100)))))

(fincl-viz:plot (fincl-viz:paths *r* :n 30)        ; a fan of kept paths
                (fincl-viz:mean-line *r* :spot))   ; with the recorded mean
(fincl-viz:plot (fincl-viz:histogram *r* :pnl :var 0.99d0 :es 0.99d0))
(fincl-viz:plot *r*)                               ; a default layout
```

The layers are `paths`, `mean-line`, `statistic`, `band`, `histogram` and
`reference-line`. Layers listed in one `plot` call share a panel and must
agree on whether the horizontal axis is dates or numbers (`incompatible-layers`
otherwise); `panel` and `grid` arrange several panels, with `:shared-x` for a
common date axis and `:marginal` for a histogram beside a path fan. Each layer
has a role (`:paths`, `:mean`, `:band`, `:statistic`, `:histogram`,
`:reference`, `:marker`), and `:style` or `with-theme` restyles every layer of
a role at once, so a role looks the same in every panel.")

; * Pricing
(defsection @pricing (:title "Pricing")
  "Price an instrument with an engine under a market. `price` returns the
value, its standard error (NIL for deterministic engines) and the engine
used; `greeks` returns a plist of sensitivities. An engine that cannot price
an instrument signals `unsupported-combination`, or `model-domain-error` in a
parameter regime its method does not cover; automatic selection moves on to
the next engine. `unsupported-quanto` is signalled for an instrument in a
currency other than its underlying's, and `invalid-argument` for a malformed
argument."
  (fincl:instrument class)
  (fincl:engine class)
  (fincl:make-engine function)
  (fincl:price generic-function)
  (fincl:greeks function)
  (fincl:unsupported-combination condition)
  (fincl:model-domain-error condition)
  (fincl:unsupported-quanto condition)
  (fincl:invalid-argument condition))

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
  (@levy-procs section)
  (@change-of-measure section))
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

; *** Change of measure
(defsection @change-of-measure (:title "Change of measure")
  "Every process is stated under the risk-neutral measure $Q$. A process under
the physical measure $P$, for scenarios rather than prices, comes from one by
Girsanov's theorem: a state $X^i$ driven by $W^i$ with diffusion coefficient
$\\sigma_i$ has

$$dW^{i,Q}_t = dW^{i,P}_t + \\lambda_i(t, X_t)\\,dt, \\qquad \\mu^P_i = \\mu^Q_i + \\sigma_i\\lambda_i,$$

$\\lambda_i$ the market price of risk of $W^i$. Diffusion coefficients and
correlations are the same under both measures; jump parts change through their
intensity and size law.

Premia are named by state (`:spot`, `:variance`, `:short-rate`), given as

- a price of risk $\\lambda$: a real, `(:scaled c)` for $\\lambda = c\\,\\sigma$ (a drift
  change $c\\,\\sigma^2$, proportional to variance), or a function of time and state;
- a drift change $\\delta = \\sigma\\lambda$, in the state's coordinate (for the spot, the
  excess growth rate: $E^P[S_{t+h}] = E^Q[S_{t+h}]\\,e^{\\delta h}$ for constant $\\delta$);
- physical values of measure-dependent parameters.

| Process | Premium | Under $P$ |
|---|---|---|
| GBM, CEV, local vol | spot: any form | drift added after each step; exact when $\\delta$ is constant over a step, trapezoid rule otherwise |
| Merton, Kou | spot; jump parameters | as GBM; the jump law compensated at its $P$ intensity |
| Heston, Bates | spot: any form; variance: `(:scaled c)` or a constant $d$; $\\kappa$, $\\theta$ | $\\kappa^P = \\kappa - c\\,\\xi^2$, $\\theta^P = (\\kappa\\theta + d)/\\kappa^P$ |
| NGARCH | spot: real $\\lambda$ per period | $\\gamma^P = \\gamma - \\lambda$, mean $\\lambda\\sqrt{h_t}$ |
| Hull-White | short rate: any form, of time only; $a$ | drift $\\sigma\\lambda(t)$ inside the exact step |
| variance gamma, NIG | $\\sigma, \\theta$; $\\alpha, \\beta$ | physical Lévy measure, drift beside the jumps unchanged |

The measures stay equivalent: states without a Brownian driver take no
price of risk, and variance gamma's $\\nu$ and NIG's $\\delta$, which set the
small jumps, are fixed (K. Sato, _Lévy Processes and Infinitely Divisible
Distributions_, Theorem 33.1). A Heston variance premium keeps the measures
equivalent only if the Feller condition $2\\kappa\\theta > \\xi^2$ holds under both
(P. Cheridito, D. Filipović and R. L. Kimmel, Market price of risk specifications
for affine models, _Journal of Financial Economics 83_ (2007)); a violation
signals `feller-condition-violated`. The variance premium is Heston's,
proportional to variance (S. L. Heston, _Review of Financial Studies 6_ (1993));
the NGARCH form is Duan's (_Mathematical Finance 5_ (1995)).

In SIMULATE a physical process is a leg's process: its state evolves under
$P$, and each date's market is built from that state with the risk-neutral
process, so prices on every date are $Q$ valuations of a $P$ scenario.
Engines and calibration refuse a physical process."
  (fincl:change-measure function)
  (fincl:physical-process class)
  (fincl:risk-neutral-process generic-function)
  (fincl:simulation-measure function)
  (fincl:invalid-measure-change condition)
  (fincl:risk-neutral-process-required condition)
  (fincl:feller-condition-violated condition))

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







; * Portfolios and risk
(defsection @portfolios (:title "Portfolios and risk")
  "A portfolio is a tree of positions, each a signed quantity $q_i$ of an
instrument, valued on one market $M$ as
$$V(M)=\\sum_i q_i\\,V_i(M).$$
Pricing is `price` on every position, in parallel and under a recovery
policy: a book is priced even when one of its positions cannot be. Risk is
revaluation on scenarios, which are other markets (`bump`, aging), so every
instrument and engine that prices on a market takes part with no further
method. Results do not depend on the number of workers.

```cl
(defparameter *market*
  (make-market (valued-on #D\"2026-10-01\")
               (underlying :spx (spot 5000) (currency :usd)
                           (dividend 0.015d0) (vol 0.18d0))
               (curve :usd-ois (rate 0.04d0))))
(defparameter *book*
  (make-portfolio \"book\"
    (make-position (make-option (call 5000) (expires #T\"1Y\" :from #D\"2026-10-01\"))
                   :quantity 10 :tags '(:desk :a))
    (make-position (make-option (put 4500) (american) (expires #T\"6M\" :from #D\"2026-10-01\"))
                   :quantity -5 :tags '(:desk :b))
    (make-position (make-holding) :quantity -3 :tags '(:desk :a))))
(price *book* :auto *market*)
;; ==>  -11158.493465283193d0
;;      NIL
;;      NIL
```"
  (@positions section)
  (@portfolio-pricing section)
  (@scenarios section)
  (@var section)
  (@sensitivity-ladders section)
  (@snapshots section))

(defsection @positions (:title "Positions and portfolios")
  "A position is $q$ units of an instrument: long when $q>0$, short when
$q<0$. It may name its own engine, a designator; otherwise the engine given
to the pricing call prices it (`:auto` by default). It also carries a plist
of tags. A portfolio is a named tree of positions and
portfolios. Both are immutable; `position-instrument`, `position-quantity`,
`position-engine` and `position-tags` read a position, `portfolio-name` and
`portfolio-members` a portfolio. `holding` is the underlying itself, worth
its spot, with delta 1 and every other Greek 0, so that a book can hold its
hedge; borrow and financing are not modelled.

```cl
(portfolio-by-tag *book* :desk)
;; ==>  ((:A . #<PORTFOLIO \"book :DESK=:A\", 2 positions>)
;;       (:B . #<PORTFOLIO \"book :DESK=:B\", 1 position>))
```"
  (fincl:holding class)
  (fincl:make-holding function)
  (fincl:portfolio-position class)
  (fincl:position-instrument function)
  (fincl:position-quantity function)
  (fincl:position-engine function)
  (fincl:position-tags function)
  (fincl:make-position function)
  (fincl:derive-position function)
  (fincl:portfolio class)
  (fincl:portfolio-name function)
  (fincl:portfolio-members function)
  (fincl:make-portfolio function)
  (fincl:portfolio-positions function)
  (fincl:portfolio-long function)
  (fincl:portfolio-short function)
  (fincl:portfolio-by-tag function)
  (fincl:same-instrument-p generic-function)
  (fincl:adjust-position function))

(defsection @portfolio-pricing (:title "Pricing a portfolio")
  "`price-portfolio` returns a `portfolio-result` with one `position-result`
per position, in the order of `portfolio-positions`: the unit price, its
standard error, the engine that priced it and the condition it recovered
from, read by `position-result-unit-price`, `position-result-standard-error`,
`position-result-engine` and `position-result-condition`. A position that
cannot be priced is recorded with its condition and nothing is signalled;
`portfolio-value` returns the value, its standard error (Monte Carlo
positions assumed independent) and the unpriced positions.

When an engine declines a position (`unsupported-combination` or
`model-domain-error`), the engines in `applicable-engines` are tried in turn.
That function finds the engines whose pricing methods accept the instrument
by inspecting the method tables through the metaobject protocol, restricted
to those that model the process, and orders them by `engine-preference`:
Black-Scholes 10, Roll-Geske-Whaley 11, other closed forms 12, COS 20,
finite differences 30, the tree 40, Monte Carlo 90. An engine listed may
still decline in a regime its method does not cover.

```cl
(price-portfolio *book* *market*)
;; ==>  #<PORTFOLIO-RESULT \"book\" -11158.4935>
(applicable-engines (make-option (put 4500) (american) (expires #D\"2027-04-01\"))
                    *market* :process (make-heston))
```

`price` and `greeks` on a portfolio are the quantity-weighted sums over its
positions; `:method :bump` makes `greeks` bump every position."
  (fincl:price-portfolio function)
  (fincl:portfolio-result class)
  (fincl:position-result class)
  (fincl:position-result-unit-price function)
  (fincl:position-result-standard-error function)
  (fincl:position-result-engine function)
  (fincl:position-result-condition function)
  (fincl:portfolio-value function)
  (fincl:position-result-value function)
  (fincl:applicable-engines generic-function)
  (fincl:select-engine function)
  (fincl:engine-preference generic-function)
  (fincl:*default-engines* variable)
  (fincl:price (method () (fincl:portfolio t t)))
  (fincl:greeks (method () (fincl:portfolio t t))))

(defsection @scenarios (:title "Scenarios and revaluation")
  "A scenario is a list of risk-factor bumps, as `bump` takes them, and an
aging in days or to a date; applied to a composite market it gives another
market. `revalue` prices every position on each scenario's market with the
engine it got on the base market $M_0$, so automatic selection runs once, and
returns the P&L array
$$\\Pi_{ij}=q_i\\left(V_i(M_j)-V_i(M_0)\\right),$$
positions by scenarios; `scenario-pnl` sums its columns to the portfolio's
$\\Pi_j$. A position that cannot be priced contributes 0 and is listed.
The builders `parallel-shift-scenarios`, `spot-vol-grid` and
`historical-scenarios` make the usual scenario sets: shifts of one factor,
the grid of relative spot and absolute vol shifts, and one scenario per
observation of a history of factor changes. `scenario-name`, `scenario-bumps`
and `scenario-age` read a scenario.

```cl
(let ((grid (spot-vol-grid :spx '(-1/10 0 1/10) '(-5/100 0 5/100))))
  (scenario-pnl (revalue *book* *market* grid)))
;; ==>  #(-2067.9972298808498d0 -1577.8793581143627d0 -1041.389844616886d0
;;        -769.2592592460352d0 0.0d0 720.3974515347319d0 1427.080644420651d0
;;        2119.837472639283d0 2852.767617368155d0)
```"
  (fincl:scenario class)
  (fincl:scenario-name function)
  (fincl:scenario-bumps function)
  (fincl:scenario-age function)
  (fincl:make-scenario function)
  (fincl:apply-scenario function)
  (fincl:parallel-shift-scenarios function)
  (fincl:spot-vol-grid function)
  (fincl:historical-scenarios function)
  (fincl:revalue function)
  (fincl:scenario-pnl function))

(defsection @var (:title "Value at risk")
  "With losses $\\ell = - \\Pi $ over $n$ scenarios sorted
$\\ell_1 \\geq \\ell_2 \\geq \\ldots $ and $ k =\\lceil n(1-c)\\rceil$, the
historical value at risk and expected shortfall at confidence $c$ are
$$\\mathrm{VaR}_c = \\ell_k,\\qquad
\\mathrm{E S}_c = \\frac{1}{k}\\sum_{m = 1}^{k}\\ell_m,$$
the $k$-th largest loss and the mean of the $k$ largest, the sample forms of
the loss quantile and of the tail expectation. The sample is the P&L of a
revaluation on `historical-scenarios` or on any scenario set.

The delta-normal VaR is $z_c\\sqrt{s^\\top C s}$ with $z_c$ the normal
quantile, $C$ the covariance of the factor changes over the horizon and
$s_f=\\partial V/\\partial x_f$ the portfolio's sensitivity to each factor, by
central differences on bumped markets (`factor-sensitivities`); it is exact
for a portfolio linear in jointly normal factors. `repair-positive-semidefinite`
raises the negative eigenvalues of a covariance or correlation estimate to
zero, which gives the nearest positive-semidefinite matrix in the Frobenius
norm.

```cl
;; RETURNS: 500 daily SPX returns
(let* ((scenarios (historical-scenarios *market* `(((:spot :spx) :relative ,returns))))
       (pnl (scenario-pnl (revalue *book* *market* scenarios))))
  (values (historical-var pnl 0.99d0) (expected-shortfall pnl 0.99d0)))
(delta-normal-var *book* *market* '(((:spot :spx) :relative))
                  #2A((0.0004d0)) 0.99d0)
```

See C. Acerbi and D. Tasche, On the coherence of expected shortfall, _Journal
of Banking and Finance 26_ (2002); R. T. Rockafellar and S. Uryasev,
Conditional value-at-risk for general loss distributions, _Journal of Banking
and Finance 26_ (2002); J. P. Morgan and Reuters, _RiskMetrics Technical
Document_, 4th ed. (1996); N. J. Higham, Computing a nearest symmetric
positive semidefinite matrix, _Linear Algebra and its Applications 103_ (1988)."
  (fincl:historical-var function)
  (fincl:expected-shortfall function)
  (fincl:factor-sensitivities function)
  (fincl:delta-normal-var function)
  (fincl:repair-positive-semidefinite function))

(defsection @sensitivity-ladders (:title "Ladders and P&L explain")
  "`curve-delta-ladder` gives the portfolio's sensitivity to each input quote
of a bootstrapped curve, with the curve rebootstrapped on every bump; the sum
is the parallel-shift sensitivity to first order. `vega-ladder` gives vega by
expiry bucket.

`explain-pnl` attributes the P&L between two markets to the Greeks at the
first, position by position:
$$\\Delta V\\approx\\delta\\,\\Delta S+\\tfrac12\\gamma\\,\\Delta S^2
+\\nu\\,\\Delta\\sigma+\\text{vanna}\\,\\Delta S\\,\\Delta\\sigma
+\\tfrac12\\text{volga}\\,\\Delta\\sigma^2+\\theta\\,\\tau
+\\rho\\,\\Delta r+\\rho_q\\,\\Delta q,$$
with $\\Delta\\sigma$ the move of the Black vol at the position's strike and
expiry and $\\tau$ the calendar days between the valuation dates. It returns
each term, their sum `:explained`, the full revaluation `:actual` and the
`:unexplained` remainder: third-order terms, rate convexity and cross terms
with rates.

```cl
(explain-pnl *book* *market* (bump *market* '(:spot :spx) '(:relative 1/100)))
;; ==>  (:DELTA 181.67 :GAMMA 2.862 ... :EXPLAINED 184.53
;;       :ACTUAL 184.56 :UNEXPLAINED 0.0287)
(vega-ladder *book* *market* (list #T\"3M\" #T\"1Y\"))
```"
  (fincl:curve-delta-ladder function)
  (fincl:vega-ladder function)
  (fincl:explain-pnl function))

(defsection @snapshots (:title "Snapshots")
  "A snapshot is the constructor form that rebuilds a market, a portfolio or
one of their parts: composite markets with their flat, interpolated and
dividend-schedule term structures, smile, grid, SVI and SSVI surfaces and day
counters, options, holdings, positions and portfolios. Bootstrapped curves are
not supported. `write-snapshot` prints the form with dates as `#D` literals;
`read-snapshot` reads it with `*read-eval*` off and evaluates it with an
evaluator that calls only the snapshot constructors, so reading one runs no
other code. A rebuilt object is made from the same numbers, so its prices are
`=` to the original's. A composite market prints readably as its snapshot form.

```cl
(with-open-file (s \"book.sexp\" :direction :output :if-exists :supersede)
  (write-snapshot *book* s))
(with-open-file (s \"book.sexp\") (read-snapshot s))
```"
  (fincl:snapshot-form generic-function)
  (fincl:write-snapshot function)
  (fincl:read-snapshot function))

; * Extending fincl
(defsection @extending (:title "Extending fincl")
  "New currencies, calendars, processes and payoffs are defined with `define-`
macros, which write classes, constructors and the methods the engines call.
The dice game below uses all of them; each subsection defines one piece
and the last assembles them into a toy option contract pricing.

A fair die is rolled once a week. Players are paid in casino chips, and two contracts are
written on it:

- **Jackpot:** pays the face of the last roll, plus a bonus if it shows the
  top face. With a bonus of 10 on a six-sided die the expected payoff is
  $3.5 + 10/6$.
- **Bust game:** after three weekly rolls it pays the running total, unless a
  1 was rolled along the way, in which case the game ends and pays nothing.
  The expected payoff is $(5/6)^3 \\cdot 12 \\approx 6.944$, where 12 is the
  expected total of three rolls that are all at least 2.

Start with

```cl
(ql:quickload :fincl)
(defpackage #:dice (:use #:cl #:fincl))
(in-package #:dice)
(named-readtables:in-readtable fincl:syntax)
```"
  (@define-currency section)
  (@define-calendar section)
  (@define-process section)
  (@define-payoff section)
  (@define-path-payoff section)
  (@define-clause section)
  (@define-market-component section)
  (@dice-example section))

(defsection @define-currency (:title "define-currency")
  "Register a currency keyword with its name and the number of decimal places
amounts are shown with. Options and markets refer to it by keyword.

```cl
(define-currency :chip :name \"Casino chip\" :fraction-digits 0)
```"
  (fincl:define-currency macro))

(defsection @define-calendar (:title "define-calendar")
  "Define a holiday calendar from rules: `(:fixed month day)`,
`(:nth-weekday n weekday month)`, `(:easter offset)`, `(:dates \"YYYY-MM-DD\"...)`
and `(:when predicate)`, each with `:observe`, `:from`, `:to` and `:except`.
The macro makes the class and the singleton `+name+`, which any function
taking a calendar accepts, e.g. `(expires #T\"2M\" :from d :calendar +casino+)`.
`define-observance` adds a rule for holidays that fall on a weekend.

Our casino will be closed on weekends as well as Christmas
and Dec 26, observing this holiday on the previous weekday if it happens
to fall on a weekend.

```cl
(define-observance :previous-weekday (date)
  (case (day-of-week date)
    (6 (date+ date -1))
    (7 (date+ date -2))
    (t date)))

(define-calendar casino (weekends-only)
  \"Closed on Christmas Day, and on the 26th, observed on the previous weekday.\"
  (:fixed 12 25)
  (:fixed 12 26 :observe :previous-weekday))
```"
  (fincl:define-calendar macro)
  (fincl:weekends-only class)
  (fincl:define-observance macro)
  (fincl:day-of-week function))

(defsection @define-process (:title "define-process")
  "Define a custom process, its constructor `make-name` and its parameters,
which are validated, bounded and enumerable for calibration. The
dynamics are ordinary methods:

- `initial-state`: the state vector at time zero. Element 0 is the observable
  that payoffs see; the others are latent.
- `make-stepper`: returns `(lambda (state normals j))`, which advances the
  `state` vector in place from `times[j]` to `times[j+1]` using standard normals,
  and returns NIL. Do expensive work outside the lambda.
- `process-state-names`: names for the state elements, which path payoffs
  refer to.

The constructor takes the parameters as keywords. `process-parameter` reads one.
Our process will model a fair die with `faces` faces. The state of the process
will consist of 2 values: the value of the most recent roll, and a running
total of all previous rolls.

```cl
(define-process fair-die (stochastic-process) (:state-size 2)
  ((faces 6d0 :bounds (2 nil) :doc \"Number of faces.\"))
  (:documentation \"A die rolled once per step. Element 0 is the latest roll,
element 1 the running total.\"))

(defmethod initial-state ((p fair-die) market)
  (declare (ignore market))
  (make-array 2 :element-type 'double-float :initial-element 0d0))

(defmethod process-state-names ((p fair-die)) '(:spot :total))

(defmethod make-stepper ((p fair-die) market times)
  (declare (ignore market times))
  (let ((faces (process-parameter p :faces)))
    (declare (type double-float faces))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals) (ignore j))
      ;; convert standard normal to discrete uniform U(1,FACES)
      (let ((roll (+ 1d0 (min (- faces 1d0)
                              (ffloor (* faces (norm-cdf (aref normals 0))))))))
        (setf (aref state 0) roll
              (aref state 1) (+ (aref state 1) roll)))
      nil)))
```

A parameter given as `(name nil :market-default fn)` is taken from the market
when left unset; `(name default :parameter nil)` is a setting, not a model
parameter."
  (fincl:define-process macro)
  (fincl:stochastic-process class)
  (fincl:independent-increments class)
  (fincl:process-parameters function)
  (fincl:process-parameter function)
  (fincl:parameter-vector function)
  (fincl:process-factors generic-function)
  (fincl:process-state-size generic-function)
  (fincl:resolve-process generic-function)
  (fincl:constructor-collision condition)
  (fincl:norm-cdf function))

(defsection @define-payoff (:title "define-payoff")
  "Define a payoff class from a formula of the observable at expiry. Slots are
`(name type &key initform doc fact)`; one without an `initform` is a required
initarg. The class becomes a clause of `make-option` taking its slots as
keywords. Calls and puts with a strike inherit from `strike-payoff`,
which supplies `phi` and `strike` to the formula.

```cl
(define-payoff jackpot (payoff)
    ((top double-float :initform 6d0 :doc \"The face that wins the bonus.\")
     (bonus double-float :fact :bonus :doc \"Paid on top of the face.\"))
  (s)
  (+ s (if (>= s top) bonus 0d0)))
```

Monte Carlo prices any such payoff. Closed-form engines only know the built-in
ones, so `:auto` falls back to simulation for new payoffs."
  (fincl:define-payoff macro)
  (fincl:call-put-payoff class)
  (fincl:strike-payoff class)
  (fincl:payoff-kernel generic-function)
  (fincl:formula-payoff-superclass condition))

(defsection @define-path-payoff (:title "define-path-payoff")
  "Define a payoff that depends on the path, from clauses run on each path in
the order written. In them `s` is the observable at the current grid point.

- `(:accumulate (var init)...)`: variables kept along a path
- `(:state var name)`: `var` is the process state element called `name`
- `(:start form...)`: at the valuation date
- `(:on slot (i) form...)`: on each date in the date-list slot
- `(:every form...)`: at every grid point
- `(:final form)`: the amount paid at expiry

`(pay amount)` inside `:on` pays on that date, and `(stop)` ends the path,
which then pays nothing at expiry. The observation dates become points of the
simulation grid. Path payoffs are European and priced by Monte Carlo.

The `bust-game` will refer to the `fair-die` state `:total`, which
keeps the running total of die rolls. When the die is rolled on a date
in `rolls`, check if the die rolls `1`, the contract goes bust.

```cl
(define-path-payoff bust-game (payoff) ((rolls list))
  (s)
  (:state total :total)
  (:on rolls (i)
    (when (= s 1d0) (stop)))
  (:final total))
```"
  (fincl:define-path-payoff macro)
  (fincl:path-payoff class)
  (fincl:path-kernel generic-function)
  (fincl:state-index function))

(defsection @define-clause (:title "define-fact and define-clause")
  "`make-option` is built from clauses that set named facts. `define-fact` declares
a fact; `define-clause` adds a clause setting it with `publish`, and a payoff
slot with `:fact` reads it:

```cl
(define-fact :bonus double-float \"Paid on top of the top face.\"
  :coerce #'real-to-double)

(define-clause option bonus (amount)
  (:publishes :bonus)
  \"Pay AMOUNT on a roll of the top face.\"
  (publish :bonus amount))
```

so `(make-option (jackpot) (bonus 10) ...)` fills the `bonus` slot of `jackpot`.
A clause lists the facts it may set in `(:publishes ...)`."
  (fincl:define-fact macro)
  (fincl:define-clause macro)
  (fincl:apply-clauses function)
  (fincl:with-scope macro)
  (fincl:make-from-facts function)
  (fincl:fact function))

(defsection @define-market-component (:title "define-market-component")
  "A composite market holds kinds of data under keyword keys: spots, vol
structures and dividends by underlying, yield curves by id, rates vols by id.
`define-market-component` adds a kind in one form: its value type, its
`make-market` clause, and its place in `market-component`, `bump`, printing
and snapshots. The casino quotes chips in dollars:

```cl
(define-market-component :fx
  :type (real (0)) :description \"a positive FX rate\"
  :clause (fx (pair rate))
  :documentation \"The FX rate of PAIR: units of the quote currency per unit of the base.\")

(defparameter *casino*
  (make-market (valued-on #D\"2026-10-01\") (curve :chip (rate 0)) (fx :chipusd 0.25d0)))
(market-component *casino* '(:fx :chipusd))                ; => 0.25d0
(bump *casino* '(:fx :chipusd) '(:relative 1/100))          ; chips 1% dearer
```

`:within (underlying name (value))` makes the clause a sub-clause of
`underlying`, keyed by the underlying. A value that is not a real is moved by
its `bump-structure` method, and its `snapshot-form` may call only the
functions and classes listed in `:snapshot-constructors` and
`:snapshot-classes`. `:view` names the market a process steps on when a
simulation drives the kind; without it the kind is not simulated. Redefining
a kind from another package signals the warning
`market-component-redefined`; the kinds fincl defines cannot be redefined."
  (fincl:define-market-component macro)
  (fincl:market-component function)
  (fincl:market-component-redefined condition))

(defsection @dice-example (:title "A dice game")
  "Putting the pieces together. Let us simulate our games played
at our casino and price the contracts we'll write on them.

```cl
(define-currency :chip :name \"Casino chip\" :fraction-digits 0)

(define-observance :previous-weekday (date)
  (case (day-of-week date)
    (6 (date+ date -1))
    (7 (date+ date -2))
    (t date)))

(define-calendar casino (weekends-only)
  \"Closed on Christmas Day, and on the 26th, observed on the previous weekday.\"
  (:fixed 12 25)
  (:fixed 12 26 :observe :previous-weekday))

(define-process fair-die (stochastic-process) (:state-size 2)
  ((faces 6d0 :bounds (2 nil) :doc \"Number of faces.\"))
  (:documentation \"A die rolled once per step. Element 0 is the latest roll,
element 1 the running total.\"))

(defmethod initial-state ((p fair-die) market)
  (declare (ignore market))
  (make-array 2 :element-type 'double-float :initial-element 0d0))

(defmethod process-state-names ((p fair-die)) '(:spot :total))

(defmethod make-stepper ((p fair-die) market times)
  (declare (ignore market times))
  (let ((faces (process-parameter p :faces)))
    (declare (type double-float faces))
    (lambda (state normals j)
      (declare (type (simple-array double-float (*)) state normals) (ignore j))
      ;; Phi(Z) is uniform on (0, 1), so floor(faces U) + 1 is uniform on 1..faces.
      (let ((roll (+ 1d0 (min (- faces 1d0)
                              (ffloor (* faces (norm-cdf (aref normals 0))))))))
        (setf (aref state 0) roll
              (aref state 1) (+ (aref state 1) roll)))
      nil)))

(define-fact :bonus double-float \"Paid on top of the top face.\"
  :coerce #'real-to-double)

(define-clause option bonus (amount)
  (:publishes :bonus)
  \"Pay AMOUNT on a roll of the top face.\"
  (publish :bonus amount))

(define-payoff jackpot (payoff)
    ((top double-float :initform 6d0 :doc \"The face that wins the bonus.\")
     (bonus double-float :fact :bonus :doc \"Paid on top of the face.\"))
  (s)
  (+ s (if (>= s top) bonus 0d0)))

(define-path-payoff bust-game (payoff) ((rolls list))
  (s)
  (:state total :total)
  (:on rolls (i)
    (when (= s 1d0) (stop)))
  (:final total))
```

Now, pricing two contracts on the die:

```cl
(defparameter *today* #D\"2026-10-26\")
(defparameter *market*
  (make-market (valued-on *today*)
               (underlying :die (spot 1) (currency :chip) (vol 0.2d0))
               (curve :chip-curve (rate 0d0))
               (discounting :chip :chip-curve)))
(defparameter *engine*
  (make-engine :mc :process (make-fair-die) :n-paths 200000 :n-steps 3))

;; Pays the last roll, plus 10 chips on a 6: expected 3.5 + 10/6.
(price (make-option (jackpot) (bonus 10) (currency :chip)
                    (expires #T\"2M\" :from *today* :calendar +casino+))
       *engine* *market*)
;; ==> 5.17...

;; Weekly rolls; pays the total after three rolls unless a 1 shows up.
(price (make-option (bust-game :rolls (list #D\"2026-11-02\" #D\"2026-11-09\" #D\"2026-11-16\"))
                    (underlying :die) (expires #D\"2026-11-16\"))
       *engine* *market*)
;; ==> 6.96...  (exactly (5/6)^3 * 12 = 6.944)
```")


;;; The sidebar can be hidden with a button. The style and script go into every
;;; page's head; the button is added once the page has loaded.
(defparameter *sidebar-head* "<style>
  #toc .toc-hidden { display: none; }
  #toc .toc-h3 { padding-left: calc(10px + 1.2em + 12px); }
  #toc .toc-h4 { padding-left: calc(10px + 1.2em + 24px); }
  #toc .toc-section-toggle { background: none; border: none; padding: 0;
                             width: 1.2em; font: inherit; color: inherit;
                             cursor: pointer; }
</style>
<script>
  document.addEventListener('DOMContentLoaded', function () {
    var groups = [], group = null;
    document.querySelectorAll('#page-toc li').forEach(function (li) {
      if (li.classList.contains('toc-h2')) {
        group = { button: document.createElement('button'), items: [] };
        group.button.type = 'button';
        group.button.className = 'toc-section-toggle';
        li.insertBefore(group.button, li.firstChild);
        groups.push(group);
      } else if (li.classList.contains('toc-h1')) {
        group = null;
      } else if (group) {
        group.items.push(li);
      }
    });
    function setOpen(g, open) {
      g.button.textContent = open ? '\\u25BE' : '\\u25B8';
      g.button.setAttribute('aria-expanded', String(open));
      g.items.forEach(function (li) { li.classList.toggle('toc-hidden', !open); });
    }
    groups.forEach(function (g) {
      if (g.items.length === 0) { g.button.style.visibility = 'hidden'; return; }
      var here = g.items.some(function (li) {
        var a = li.querySelector('a');
        return a && a.getAttribute('href') === decodeURI(location.hash);
      });
      setOpen(g, here);
      g.button.addEventListener('click', function () {
        setOpen(g, g.button.getAttribute('aria-expanded') !== 'true');
      });
    });
  });
</script>"
  "Markup added to the head of every generated page: a style and a script that
put a button before each top-level section of the sidebar, which shows or
hides its subsections. Sections start collapsed, except the one holding the
page's anchor.")

(defun build-manual (&key (target-dir #p"docs/api/"))
  "Generate the HTML manual into TARGET-DIR, with collapsible sidebar sections."
  (let ((*document-html-head* *sidebar-head*))
    (update-asdf-system-html-docs @fincl-manual :fincl :target-dir target-dir)))
