________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`exponential_smoothing`
=======================

This library forecasts a single time series using simple exponential
smoothing, Holt linear-trend smoothing, or additive or multiplicative
Holt-Winters seasonal smoothing. Choose a method to match the level,
trend, and seasonality in your data. The object implements the
`forecaster_protocol` protocol and reuses dataset validation, diagnostics,
export, and lifecycle support from the `time_series_protocols` library.

Datasets are objects implementing `time_series_dataset_protocol`.
Seasonal models require a frequency of at least `2`. By default,
the dataset's `frequency/1` predicate supplies it; the `frequency/1`
learning option can override this choice. You can supply fixed
smoothing factors or let the library fit automatic factors using one
of the optimizer strategies described below.


API documentation
-----------------

Open the [../../apis/library_index.html#exponential-smoothing](../../apis/library_index.html#exponential-smoothing)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(exponential_smoothing(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(exponential_smoothing(tester)).


Models
------

The `model/1` option selects one of seven methods:

- `simple`: simple exponential smoothing with a level component.
- `holt`: Holt smoothing with level and linear-trend components.
- `holt_damped`: Holt smoothing with a damped linear trend.
- `holt_winters_additive`: Holt-Winters smoothing with additive seasonal
  factors.
- `holt_winters_multiplicative`: Holt-Winters smoothing with
  multiplicative seasonal factors. All training observations must be
  strictly positive.
- `holt_winters_additive_damped`: additive Holt-Winters smoothing with a
  damped trend.
- `holt_winters_multiplicative_damped`: multiplicative Holt-Winters
  smoothing with a damped trend. All training observations must be
  strictly positive.

Alternatively, supply the `model(auto)` option to compare candidate methods
using AICc or holdout validation. The default method is `simple`.

Simple smoothing requires at least two observations. Holt smoothing
requires at least three. Seasonal smoothing requires at least two full
cycles plus one observation, i.e. `2*Frequency + 1` observations.

Holt initialization uses the second observation as the initial level and
the difference between the first two observations as the initial trend.
Seasonal initialization uses the first two complete cycles. The initial
trend is the difference between the two cycle means divided by the
frequency. For additive smoothing, the initial level is aligned with the
end of the second cycle. For a cycle position `p`, the offset
`(p - (Frequency + 1)/2) * Trend` is removed from both cycle observations
before their additive seasonal deviations are averaged. For multiplicative
smoothing, the initial level is the unadjusted second-cycle mean and
corresponding ratios to the two cycle means are averaged.


Options
-------

The `learn/3` predicate accepts the following learning options:

- `model(Method)` selects the smoothing method (default: `simple`).
- `alpha(Value)` sets the level smoothing factor (default: `auto`).
- `beta(Value)` sets the trend smoothing factor (default: `auto`).
- `gamma(Value)` sets the seasonal smoothing factor (default: `auto`).
- `phi(Value)` sets the damping factor (default: `auto`). It
  must satisfy `0.0 < Value =< 1.0`.
- `initialization(Strategy)` selects an initial-state strategy: `two_cycles`,
  `regression`, or `optimized` (default: `two_cycles`). `optimized` jointly
  fits the initial level, trend, and seasonal factors with any automatic
  smoothing parameters.
- `initial_cycles(Count)` sets the number of complete cycles used by seasonal
  regression initialization (default: `2`; minimum: `2`).
- `optimizer(Strategy)` selects an automatic-fitting search strategy:
  `nelder_mead`, `multi_start(Starts)`, or `differential_evolution`
  (default: `nelder_mead`). See "Optimizer strategies" below.
- `optimizer_options(Options)` supplies sub-options for bounded Nelder-Mead
  fitting (default: `[]`). Supported sub-options are
  `max_iterations/1`, `tol_x/1`, `tol_f/1`, `initial_step/1`, and
  `adaptive/1`. The `nelder_mead` strategy uses them directly, and the
  `multi_start(Starts)` strategy uses them for each start. With the
  `differential_evolution` strategy, they control refinement when the
  `polish(true)` sub-option is enabled.
- `de_options(Options)` supplies sub-options for Differential Evolution
  fitting, only relevant when `optimizer(differential_evolution)` is
  selected (default: `[]`). Supported sub-options are `seed/1` (default:
  `42`), `population_size/1`, `max_generations/1`,
  `crossover_probability/1`, `differential_weight/1`, `strategy/1` (one
  of `rand/1/bin`, `rand/1/exp`, `best/1/bin`, or
  `current-to-best/1/bin`), and `polish/1` (default: `true`).
- `transformation(Transformation)` selects a pre-fitting transformation,
  one of `none`, `log`, or `box_cox(Lambda)` (default: `none`). `Lambda`
  is either a finite number or `auto`. A numeric `Lambda` with
  `abs(Lambda) =< 1.0e-6` is treated as `log` for numerical stability.
  Training observations must be strictly positive when a transformation
  other than `none` is selected.
- `box_cox_bounds(Lower, Upper)` sets a finite open search interval for
  `transformation(box_cox(auto))`, with `Lower < Upper` (default:
  `box_cox_bounds(-1.0, 2.0)`). An explicit occurrence is rejected unless
  automatic Box-Cox transformation is selected.
- `bias_adjustment(Adjustment)` selects inverse-transform bias correction,
  either `none` or `delta` (default: `none`). `delta` is rejected when
  the `transformation/1` option is `none`.
- `retain_residuals(Boolean)`, when `true`, retains the one-step
  training residuals (on the fitted, possibly transformed, scale) in
  the forecaster diagnostics for use by the `forecast_interval/5` predicate
  (default: `false`).
- `missing_policy(Policy)` selects `error` or `skip_update` (default:
  `error`). The default preserves the ordinary numeric-series validation
  and rejects unbound observations. See "Missing observations" below.
- `selection_criterion(Criterion)` selects `aicc` (the default)
  or `validation(Size)` for automatic model selection. The latter reserves
  a positive number of final observations for holdout scoring.
- `candidate_models(Methods)` supplies a non-empty list of
  methods to compare with `model(auto)`, or `default` (the default value).
  The default candidate set includes seasonal methods when a usable
  frequency is available.
- `frequency(Frequency)` selects `dataset` (the default), an
  explicit integer of at least `2`, or `auto` for a seasonal model.
- `frequency_candidates(Candidates)` supplies the periods to
  consider with `frequency(auto)`, as a list or an inclusive `Min-Max`
  range. Candidate periods must be at least `2`. Its default is `none`;
  supply candidates when requesting automatic frequency selection.

A smoothing factor can be `auto` or a number in the closed interval
`[0.0, 1.0]`. Automatic factors are optimized by minimizing the
one-step-ahead sum of squared errors. Explicit factors remain fixed,
allowing partially constrained models. When all relevant factors are
explicit and initialization is not optimized, fitting bypasses the optimizer.

The `beta/1` option is relevant only to Holt and Holt-Winters models. The
`gamma/1` option is relevant only to Holt-Winters models. Explicit numeric
values for an irrelevant factor are rejected instead of being silently ignored.


Optimizer strategies
---------------------

Automatic factors are fitted by minimizing the one-step-ahead sum of
squared errors over the `[0.0, 1.0]` box of automatic parameters. The
`optimizer/1` option selects the search strategy. When all relevant factors
are fixed and initialization is not optimized, fitting bypasses the
optimizer. The `convergence(fixed_parameters)` diagnostic term records
this case, with zero iterations and evaluations. THe following strategies
are supported:

- `nelder_mead` (default): a single bounded Nelder-Mead run from the
  deterministic initial point described under "Notes" below.
- `multi_start(Starts)`: `Starts` (a positive integer) bounded
  Nelder-Mead runs, each using the `optimizer_options/1` option. The first
  start is the same deterministic initial point used by `nelder_mead`; the
  remaining `Starts - 1` points are a deterministic Halton low-discrepancy
  sequence (bases `2, 3, 5, ...`, one per automatic parameter) mapped onto
  the parameter box, so the same options always produce the same starts.
  The run with the lowest sum of squared errors is selected; exact ties
  are broken by the lexicographically smallest parameter point (standard
  order of terms). The `iterations/1` and `evaluations/1` diagnostic terms
  report totals across all starts. The `convergence/1` diagnostic reflects
  only the winning start's termination status against the `max_iterations/1`
  sub-option supplied through `optimizer_options/1`.
- `differential_evolution`: a single run of the `differential_evolution`
  library's `rand/1/bin`-family metaheuristic over the same bounded
  objective, using a dedicated `fast_random(xoshiro128pp)` generator
  instance whose seed is saved before the run and restored afterward, so
  fitting neither depends on nor mutates any global random state. The
  `seed/1` sub-option of the `de_options/1` option (default `42`) makes runs
  deterministic. When the `polish/1` sub-option is `true` (the default), the
  DE result is refined by a further bounded Nelder-Mead run using the
  `optimizer_options/1` option, starting from the DE best point. The
  `iterations/1` and `evaluations/1` diagnostics report the sum of the
  DE generations/evaluations and, when polishing ran, the polishing
  iterations/evaluations. The `convergence/1` diagnostic reflects whether
  the DE generation cap (the `max_generations/1` sub-option, default `100`)
  was reached, independently of any subsequent polishing.

None of the three strategies guarantees the global minimum of the training
objective; the `multi_start(Starts)` and `differential_evolution` strategies
only reduce, but do not eliminate, the risk of a poor local optimum found
from a single start.

With `initialization(optimized)`, optimization is also required when all
smoothing parameters are explicit. Initial level and trend bounds are finite
and derived from the observed minimum, maximum, and range. Seasonal models
use `Frequency-1` identifiable coordinates: additive coordinates decode to
factors normalized to exact zero mean, while multiplicative log-ratio
coordinates decode to strictly positive factors normalized to mean one. The
learned forecaster keeps the canonical state and smoothing-parameter shapes;
initial-state coordinates are not persisted as a second representation.
Model and frequency selection count these fitted initial-state coordinates in
the AICc parameter total. An automatically fitted Box-Cox lambda is also
counted as one fitted parameter.


Missing observations
--------------------

Missing observations are opt-in. With `missing_policy(error)`, the default,
all observations must be numbers and the existing validation errors are
preserved. With `missing_policy(skip_update)`, unbound variables represent
missing observations, uniformly with the other time-series libraries. Dataset
facts can use anonymous variables, for example `observation(3, _).`. Learning
and updates do not bind missing observations, and learned forecasters remain
ground. Observation indices remain a regular, gap-free, 1-based sequence;
missing positions are not removed from the time axis.

Atoms such as `missing` or `na` are not missing observations and are rejected.
There is no configurable missing-value marker or `missing_value/1` option.

At a missing observation, simple smoothing retains its level. Holt models
advance the level by the current trend. Damped models advance by the damped
trend and decay the trend. Seasonal models also rotate the seasonal queue,
retaining the factor for that phase. No residual is recorded, no error is
scored, and no correction based on an observed value is applied.
Transformations are applied only to known observations. Automatic parameter
fitting and model or frequency selection evaluate the same missing-aware
objective.

Initialization never imputes a missing value. Nonseasonal two-point
initialization uses the first required known observations and their original
indices. Regression initialization fits only known values against their
original indices. A seasonal initialization window must contain at least one
known value in every seasonal phase and enough known values to estimate its
level and trend. Otherwise learning raises
`domain_error(insufficient_seasonal_phase_observations, Phase)` or
`domain_error(insufficient_known_observations, Context)`. At least one known
observation must remain to score after initialization.


Immutable online updates
------------------------

The `update/3` predicate applies one new observation to a learned forecaster
and returns a new forecaster term; the input forecaster is never modified.
Updates reuse the fitted parameters and the exact per-observation equations
used by batch fitting, including transformed and damped states and rotation of
the current seasonal queue. Parameters are not refitted.

With `missing_policy(skip_update)`, pass an anonymous or unbound variable to
the `update/3` predicate for a missing observation. With `missing_policy(error)`,
an unbound update observation raises an instantiation error.

The original effective training options remain unchanged. Each update
increments the `training_series_length/1` and `update_count/1` diagnostic
values. Known observations
also increment `observed_count/1` and `scored_count/1`, add their one-step
squared error to `sum_squared_error/1`, and recompute
`mean_squared_error/1`. A missing observation also increments `missing_count/1`,
but does not change the observed count, scored count, or error totals. Its
position still counts toward the elapsed length and update count, and trend
prediction and seasonal phase advance exactly as during batch fitting.

Residual retention follows the original `retain_residuals/1` learning option.
When enabled, each known online residual is appended chronologically to the
retained training residuals; skipped missing observations append nothing.
When disabled, diagnostics continue to store `residuals(none)`.


Forecaster representation
-------------------------

A learned forecaster uses the following term representation:

	exponential_smoothing_forecaster(Method, State, Parameters, Diagnostics)

The `State` argument uses one of the following terms:

- `level(Level)` for simple exponential smoothing.
- `holt(Level, Trend)` for Holt smoothing.
- `holt_damped(Level, Trend, Phi)` for damped Holt smoothing.
- `holt_winters(Level, Trend, Frequency, SeasonalQueue)` for seasonal
  smoothing. The queue begins at the seasonal position used by the next
  forecast.
- `holt_winters_damped(Level, Trend, Phi, Frequency, SeasonalQueue)` for
  damped seasonal smoothing.

When the `transformation/1` option value is not `none`, the state is wrapped
in the following term:

	transformed(Transformation, InnerState, residual_variance(Variance))

where `InnerState` is one of the method-specific states above, fitted on
the transformed series, and `Variance` is the transformed-scale mean
squared one-step error. The `none` transformation keeps the unwrapped
state shapes above unchanged.

`Parameters` contains the fitted numeric factors in canonical order:
`[Alpha]`, `[Alpha, Beta]`, `[Alpha, Beta, Phi]`,
`[Alpha, Beta, Gamma]`, or `[Alpha, Beta, Gamma, Phi]`.

Damped forecasts accumulate powers of `Phi` over the forecast horizon.
Setting `Phi` to `1.0` is equivalent to the corresponding undamped model;
values below `1.0` make the projected trend approach a finite limit.

The `Diagnostics` argument is a list of diagnostic terms. It includes
the common model, training-series length, and
options metadata plus the selected method, seasonal frequency when
applicable, fitted parameters, sum and mean squared one-step errors,
the selected strategy in an `optimizer(Optimizer)` diagnostic term,
`update_count(Count)`, convergence status,
optimizer iterations, objective evaluations, and
`observed_count(ObservedCount)`, `missing_count(MissingCount)`,
`scored_count(ScoredCount)`, and the retained residuals metadata term
`residuals(Residuals)`. `Residuals`
is the chronologically ordered list of one-step fitting residuals when
`retain_residuals(true)` was in effect, or the atom `none` otherwise.
The mean squared error denominator and retained residual list length are both
`ScoredCount`; initialization observations and missing observations are not
scored. Forecaster validation requires the persisted counts to agree with the
training length and error statistics.
The convergence status is `fixed_parameters`, `converged`, or
`maximum_iterations`; see "Optimizer strategies" above for how iterations,
evaluations, and convergence are aggregated for the `multi_start(Starts)` and
`differential_evolution`.

Forecaster validation checks the representation, method/state pairing,
finite numeric state and diagnostic values, parameter ranges, effective
model and factor options, seasonal frequency, convergence consistency with
the configured optimizer strategy and iteration limit, and consistency
between sum and mean squared errors. When the state is a transformation wrapper, validation
also checks that the wrapper transformation matches the effective
`transformation/1` option, that the residual variance is a finite
non-negative number, and that the inner state is valid for the method.
Validation also checks that the retained residuals metadata term matches
the effective `retain_residuals/1` option: `none` when the option is
`false`, or a list of finite numbers whose length equals the one-step
error count implied by the method, frequency, and initialization strategy
when the option is `true`.
Diagnostics are serialized metadata and are not
recomputed from the original training series; manually constructed terms
can therefore contain internally consistent but inaccurate error
statistics.


Transformations
---------------

When the `transformation/1` option value is `log` or `box_cox(Lambda)`, the
training series is transformed before fitting and forecasts are produced
on the transformed scale and then inverted:

- `log`: forward `log(Value)`, inverse `exp(Value)`.
- `box_cox(Lambda)` with `abs(Lambda) > 1.0e-6`: forward
  `(Value**Lambda - 1) / Lambda`, inverse `(Lambda*Value + 1) ** (1/Lambda)`.
  The inverse raises `domain_error(box_cox_inverse_domain, Base)` when
  `Base = Lambda*Value + 1` is not strictly positive.
- `box_cox(Lambda)` with `abs(Lambda) =< 1.0e-6` uses the `log` formulas,
  avoiding numerical instability from dividing by a near-zero `Lambda`.

With `transformation(box_cox(auto))`, lambda is selected deterministically
within the interval supplied by the `box_cox_bounds/2` option, using 24
iterations of golden-section search. Each lambda evaluation transforms the
series and performs a complete fit using the configured smoothing optimizer,
initialization strategy, model selection, and frequency selection. The
minimized profile-likelihood criterion uses
`N*log(SSE/E) - 2*(Lambda-1)*sum(log(Y))`, where `N` is the number of known
observations and `E` is the number of scored one-step errors, including the
Box-Cox Jacobian over known observations. Missing observations under
`missing_policy(skip_update)` are excluded from the transform and Jacobian;
every known observation must be strictly positive.

The learned state and the `options/1` diagnostic term always store the
selected numeric `box_cox(Lambda)` term; `auto` is never persisted. Forecasts,
bias adjustment, residual intervals, immutable updates, validation, and export
therefore use the same fixed selected lambda. Model and frequency AICc scores
count automatic lambda as a fitted parameter. If any required transformed fit
is infeasible, learning propagates the error; it does not fall back to an
untransformed or fixed-lambda fit.

The `sum_squared_error/1` and `mean_squared_error/1` diagnostic term arguments
are always computed and reported on the transformed scale.

When the `bias_adjustment/1` option value is `delta`, the inverse-transform
forecast is corrected using the stored transformed-scale residual variance `V`:

- `log`: `Forecast is exp(Value) * exp(V / 2)`.
- `box_cox(Lambda)` with `abs(Lambda) > 1.0e-6`: the second-order delta
  correction `Forecast is Base**(1/Lambda) * (1 + V*(1 - Lambda) / (2*Base*Base))`,
  with `Base = Lambda*Value + 1`.
- `box_cox(Lambda)` with `abs(Lambda) =< 1.0e-6` uses the `log` formula.

The `bias_adjustment(delta)` option is rejected when the `transformation/1`
option is `none`.


Prediction intervals
--------------------

The `forecast_interval/5` predicate computes residual-bootstrap prediction interval
bounds for the next `Horizon` forecasts:

	exponential_smoothing::forecast_interval(Forecaster, Horizon, Lower, Upper, Options)

This requires a forecaster learned with the `retain_residuals(true)` option; a
`domain_error(retained_residuals, Forecaster)` is raised for a positive
`Horizon` otherwise. A zero `Horizon` returns two empty lists and does not
require retained residuals.

The `Options` argument accepts interval options, validated independently
from the learning options of the `learn/3` predicate:

- `confidence(Level)`: central interval coverage, a number in the open
  interval `]0.0, 1.0[` (default: `0.95`).
- `method(residual_bootstrap)`: the only supported interval method
  (default, and currently the only accepted value).
- `samples(Count)`: number of simulated bootstrap paths, a positive
  integer (default: `1000`).
- `seed(Seed)`: positive integer seed for the dedicated bootstrap random
  number generator (default: `42`).

For each simulated path requested by the `samples/1` option and each horizon
step, the method independently resamples, with replacement, one retained
one-step training residual and adds it to the deterministic point forecast
for that step (from the shared `forecast_smoothing/4` predicate on the fitted,
possibly transformed, scale), thus preserving the seasonal phase of the point
forecast without re-simulating the level, trend, or seasonal state. Each
simulated value is then inverse-transformed using the same transformation and
`bias_adjustment/1` option as the `forecast/3` predicate. `Lower` and `Upper`
are the empirical `(1-Confidence)/2` and `(1+Confidence)/2` quantiles
(nearest-rank method) of the simulated values for each step, widened when
necessary with the `min/2` and `max/2` arithmetic functions against the point
forecast so that `Lower =< Point =< Upper` always holds.

Sampling uses a dedicated `fast_random(as183)` generator instance, isolated
from the default `fast_random` and `random` objects. The generator seed is
saved, reset from the `seed/1` option using the `randomize/1` predicate,
used for sampling, and then restored. The `forecast_interval/5` predicate
neither depends on nor mutates any global random state, and repeated calls
with the same forecaster, horizon, and options are deterministic.

Because simulated values are point forecasts plus additive residual noise
rather than full re-simulated forecasting paths, interval bounds for
multiplicative or transformed models are only an approximation: a sampled
value can in principle fall outside the domain that produced the retained
residuals (for example, a non-positive multiplicative-scale value with
`transformation(none)`). Such simulated values are not filtered or
clipped.


Notes
-----

Seasonal fitting uses the `deques` library internally. Initial seasonal
factors are converted to an opaque deque, updated factors are rotated from
front to back, and the deque is normalized once per completed seasonal
cycle. The learned state remains an ordered seasonal-factor list beginning
at the next forecast position. Thus, fitting a series of length `n` and
frequency `m` takes `O(n)` time and `O(m)` seasonal state, while preserving
the documented and exported forecaster representation.

Automatic selection is opt-in: supply the `model(auto)` option to compare
methods, or the `frequency(auto)` option with candidate periods to select
a seasonal frequency. Missing observations and immutable online updates
are supported as described above. Irregular timestamps, exogenous
regressors, and multivariate series are not supported. For prediction
intervals, use the residual-bootstrap approximation provided by the
`forecast_interval/5` predicate.

Initialization is deterministic. The default `two_cycles` strategy uses the
first two complete cycles. Multiplicative two-cycle initialization uses cycle
means without within-cycle detrending, which is robust for stable seasonality
but only an approximation when a strong trend is present. The `regression`
strategy estimates non-seasonal level and trend by linear regression. For
seasonal models, it regresses complete-cycle means, converts the slope to a
per-observation trend, averages detrended values or ratios by phase, and
normalizes the seasonal factors. Although the shared frequency contract
accepts any positive integer, this library rejects seasonal frequency `1` as
a degenerate seasonal model.

The `optimized` strategy consumes `initial_cycles*Frequency` observations for
seasonal initialization, just like `regression`, and jointly minimizes the
subsequent one-step errors over the initial state and automatic smoothing
parameters. It uses the deterministic two-cycle estimates only as an optimizer
starting point. Optimization or final-fit errors are propagated; the strategy
never falls back silently to `two_cycles` or `regression`.

Automatic fitting uses deterministic initial values of `0.2` for
`alpha`, `0.1` for `beta`, and `0.1` for `gamma`. Multiplicative
Holt-Winters instead starts an automatic `alpha` at `1.0`, ensuring that
the initial optimizer simplex contains a feasible point for sharply
declining positive series. Every automatic factor is constrained to
`[0.0, 1.0]`. Multiplicative trial points that would produce a
non-positive level receive a finite objective penalty, allowing the
optimizer to continue. The same condition in a model with explicitly
fixed factors remains a domain error.

Multiplicative forecasts must remain strictly positive. Forecasting
raises a `domain_error(positive_multiplicative_forecast, Forecast)` error
when the additive trend component would otherwise produce a zero or
negative forecast.

Solver objective and update-reporting options are controlled internally
and cannot be overridden through the `optimizer_options/1` or
`de_options/1` options. Nelder-Mead is a local optimizer with one
deterministic initial simplex. The `multi_start(Starts)` and
`differential_evolution` strategies explore the parameter box more broadly
but remain deterministic heuristics, so automatic fitting never
guarantees the global minimum of the training objective under any
strategy selected by the `optimizer/1` option.


Limitations
-----------

The library forecasts a single series using the seven documented smoothing
methods and, for seasonal methods, one integer seasonal period. It does not
support irregular timestamps, multiple seasonal periods, multivariate
series, or exogenous regressors. Seasonal frequency must be at least two.

Automatic selection compares the configured candidate methods or periods;
it is not an unrestricted search for a model. Automatic frequency selection
requires candidates supplied through the `frequency_candidates/1` option.
With the `model(auto)` option, fixed numeric `alpha/1`, `beta/1`, `gamma/1`,
and `phi/1` options are rejected. Optimizer convergence and selection scores
do not guarantee a global optimum or out-of-sample forecast accuracy.

The default `missing_policy(error)` option rejects missing observations.
The `missing_policy(skip_update)` option preserves their positions without
imputing values, but initialization must still have enough known values and
estimable seasonal phases. Learning also requires at least one scored known
observation after initialization. Multiplicative methods require positive
values on the fitted scale; log and Box-Cox transformations require positive
original observations. These constraints can exclude a series even when
missing observations are otherwise permitted.

The `update/3` predicate advances the state and error diagnostics with fixed
parameters. It does not re-optimize smoothing factors, repeat method or
frequency selection, or re-estimate a transformation. Learn a new model when
those choices need to change. Retained residuals grow with scored updates
when the `retain_residuals(true)` option is enabled.

Positive-horizon prediction intervals require retained residuals. The
`forecast_interval/5` predicate independently resamples one-step training
errors around fixed point forecasts; simulated shocks do not update future
levels, trends, or seasonal factors. This approximation does not model
serially dependent errors, horizon-dependent error accumulation, or
parameter uncertainty. Its finite-sample quantiles, widened to contain the
point forecast, are not a guarantee of nominal coverage. Forecasts and
interval bounds are not generally constrained to non-negative or integer
values, although multiplicative point forecasts must remain positive.


References
----------

- Hyndman, R.J. and Athanasopoulos, G. (2021). *Forecasting: Principles
  and Practice*. 3rd edition. OTexts. Chapter 8: exponential smoothing,
  including simple, Holt, Holt-Winters, and damped-trend methods.
  https://otexts.com/fpp3/expsmooth.html

- Holt, C.C. (2004). Forecasting Seasonals and Trends by Exponentially
  Weighted Moving Averages. *International Journal of Forecasting*,
  20(1), 5-10. Reprint of the 1957 research memorandum.
  https://doi.org/10.1016/j.ijforecast.2003.09.015

- Winters, P.R. (1960). Forecasting Sales by Exponentially Weighted
  Moving Averages. *Management Science*, 6(3), 324-342.
  https://doi.org/10.1287/mnsc.6.3.324
