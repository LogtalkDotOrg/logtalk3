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


`theta_forecasting`
===================

This library provides standard Theta (theta = 2) forecasting. It combines
simple exponential smoothing (SES) with half the least-squares linear
slope, with optional automatic seasonality detection and classical
additive or multiplicative seasonal adjustment. It reuses the
`forecaster_common` category for dataset validation, seasonal
preprocessing, diagnostics, forecast construction, and export,
and the existing `exponential_smoothing` and `local_optimization`
libraries for SES fitting and bounded deterministic optimization.

Datasets implement `time_series_dataset_protocol`. Observations are
numbers or anonymous variables representing missing observations, with
at least two known observations and complete gap-free 1-based indices.
The declared series length must include missing positions. Learning does
not instantiate missing observations or retain them in the ground model.
Atoms such as `missing` are not missing observations and are rejected.
There is no interpolation or removal of positions.


API documentation
-----------------

Open the [../../apis/library_index.html#theta-forecasting](../../apis/library_index.html#theta-forecasting)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(theta_forecasting(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(theta_forecasting(tester)).


Examples
--------

    | ?- theta_forecasting::learn(my_series, Forecaster),
         theta_forecasting::forecast(Forecaster, 12, Forecasts).

To fit a nonseasonal model with a fixed alpha and first-observation
initialization, supply these learning options:

    | ?- theta_forecasting::learn(
            my_series, Forecaster,
            [alpha(0.5), initialization(first), seasonal(none)]
         ).


Options
-------

The `learn/3` predicate accepts the following options:

- `alpha(Alpha)` selects `auto` (default) or a number in `[0, 1]`.
    Automatic fitting minimizes SES training SSE on the adjusted scale.
- `initialization(Strategy)` selects `optimized` (default) or `first`.
    Optimized initialization fits the initial level within the existing
    SES fitter's data-derived bounds. First initialization anchors the
    level at the first known adjusted observation. Both strategies use
    that position as the unscored initialization anchor.
- `seasonal(Mode)` selects `auto` (default), `none`, `additive`, or
    `multiplicative`. Explicit seasonal methods bypass the test.
- `frequency(Frequency)` selects `dataset` (default) or a positive integer.
    An explicit integer overrides the dataset's `frequency/1` predicate.
    With the `seasonal(auto)` option, a dataset without frequency metadata
    uses frequency one. Forced
    seasonal adjustment requires a supplied frequency greater than one.
    With `seasonal(none)`, dataset frequency is not inspected and any
    explicitly supplied frequency option is rejected as irrelevant.
- `optimizer_options(List)` supplies deterministic Nelder-Mead
    tuning sub-options (default: `[]`): `max_iterations/1`, `tol_x/1`, `tol_f/1`,
    `initial_step/1`, and `adaptive/1`, using their existing solver
    domains and defaults. Duplicate tuning names are rejected. Objective,
    progress output and initial-point overrides are not accepted.
- `missing_policy(Policy)` selects `skip_update` (default) or `error`.
    Missing observations leave the SES level unchanged and are not scored.
    The strict policy rejects unbound observations without binding them.
- `retain_residuals(Boolean)` selects `false` (default) or `true`.
    Enable it to retain numeric
    training residuals and matching original time indices in diagnostics.
    Retention does not change fitting, forecasts or fitted values.

Fixed alpha with first initialization does not run an optimizer.
Automatic alpha and/or optimized initialization use the existing bounded
Nelder-Mead solver. The optimizer uses a deterministic start and returns
its best point at convergence or the iteration limit. This is local
optimization, not a guarantee of a global optimum. Fixed numeric alpha
is not silently replaced by an optimizer-selected value.


Theta formula
-------------

The library fits `Adjusted(t) = Intercept + Slope*t` by ordinary least
squares using known values and their original positions. Let J be the first known
position and `Level(J)` the selected initial level. For subsequent known
positions through N, SES uses:

    Level(t) = Alpha*Adjusted(t) + (1-Alpha)*Level(t-1)

The adjusted forecast at horizon step h is:

    Level(N) + Slope/2 * (h - 1 + Correction)
    Correction = (1 - (1-Alpha)^N)/Alpha

Missing positions leave the level unchanged. The correction uses the full
elapsed training length N, including leading and trailing gaps, not the
known count or residual count. This is the library's calendar-time
extension of the standard Theta combination convention. A stable
recurrence computes it without subtractive cancellation:

    Correction(0) = 0
    Correction(k) = 1 + (1-Alpha)*Correction(k-1)

This also defines the alpha-zero limit N. Alpha one gives correction
one. Forecasts are not clamped to positive values or rounded.

For `[10,12,14,16]`, alpha 0.5, first initialization and no seasonality,
the terminal level is 14.25, the slope is 2, and the correction is
1.875. The next three forecasts are `[16.125,17.125,18.125]`.

For `[10,_,14,16]` with the same options, the original known indices are
`[1,3,4]`, the slope remains 2, and the terminal level is 14. The next
three forecasts are `[15.875,16.875,17.875]`.

Forecast horizons are non-negative integers. A zero horizon returns an
empty list after validating the complete model.


Seasonal adjustment
-------------------

Automatic detection uses mean-centered, biased autocorrelations at lags
one through m, where m is the supplied seasonal frequency. Let K be the
known observation count. It tests:

    abs(r(m)) / sqrt((1 + 2*sum(r(k)^2, k=1..m-1))/K)
        > 1.6448536269514722

The mean and variance use known values; lag covariances use only pairs
whose original positions are both known, divided by the known variance
sum. Gaps never compress lag distances. Constant series, frequency one,
at most two elapsed cycles, at most `2*m` known values, or fewer than two
known pairs at any tested lag are treated as nonseasonal. This is a
conservative missing-data heuristic, not a calibrated significance
guarantee for arbitrary gap patterns. The comparison is strict. When
seasonality is detected, strictly positive known values use multiplicative
adjustment; other numeric series use additive adjustment. This additive
choice for nonpositive data is intentional, rather than an attempt to
reproduce every reference implementation's default behavior.

Forced seasonal adjustment requires at least two elapsed cycles. The
trend estimate uses a centered m-point moving average for odd m and a
centered two-by-m moving average for even m. Only interior trend estimates
calculated from windows with no missing observations contribute to phase
averages. An odd period requires m consecutive known observations per
window; an even period requires m+1. For each seasonal phase, at least one
known observation must have a trend estimate calculated from such a window;
otherwise learning raises
`domain_error(insufficient_seasonal_phase_observations, Phase)`, reporting
the lowest unestimable phase. This also applies after automatic detection;
decomposition errors never silently fall back to a nonseasonal model.
Additive factors are centered to zero
mean; multiplicative factors are normalized to unit mean. The resulting
cycle adjusts every known observation, including endpoints, and leaves
missing positions unbound.

Multiplicative adjustment requires strictly positive observations,
trend estimates and factors. Invalid factors or arithmetic errors are
reported, not silently replaced with a nonseasonal fit.

Factors are stored in phase order one through m. Forecast restoration
starts at `(N mod m)+1`, so incomplete final cycles retain correct phase.
For example, additive `[8,10,12,8,10,12,8]` with frequency three and
fixed first initialization forecasts `[10,12,8,10]`.


Fitted values
-------------

The `fitted_values/2` predicate uses default learning options. The
`fitted_values/3` predicate accepts the same learning options as the
`learn/3` predicate, including automatic alpha, optimized
initialization, seasonal adjustment and both missing policies.

    | ?- theta_forecasting::fitted_values(
            my_series, Values,
            [alpha(0.5), initialization(first), seasonal(none)]
         ).

These are the pre-update SES training fits underlying the existing
`error_basis(ses_training_fit)` diagnostic term, restored to the original
seasonal scale. They do not include the Theta slope/correction combination
and are not prefix-wise Theta forecasts or forecasts from the terminal
model state.

The output has one position per dataset observation. Missing targets,
leading gaps and the first known initialization anchor have fresh unbound
placeholders, independent of the input variables and each other. The
anchor is unscored for both first and optimized initialization. Seasonal
phase advances at every elapsed position, including placeholders.

For `[10,12,14,16]` with alpha 0.5, first initialization and no seasonality,
the fitted values are `[_,10,11,12.5]`. For `[10,_,14,16]`, they are
`[_,_,10,12]`. Errors at numeric fitted positions agree with the five
training-error diagnostics, subject to floating-point rounding.

Automatic parameters and seasonal factors are fitted once using the full
sample. Thus these are in-sample fits, not holdout predictions or evidence
of out-of-sample accuracy. No observation history or fitted values are
retained in the forecaster. Fitted-value extraction does not bypass learning
validation or error-total arithmetic; their exceptions propagate even
when individual predictions would be representable.


Residual history
----------------

Enable the `retain_residuals(true)` option to retain chronological numeric
errors and their matching original one-based time positions:

    | ?- theta_forecasting::learn(
            my_series, Forecaster, [retain_residuals(true)]
         ),
         theta_forecasting::diagnostic(Forecaster, residuals(Errors)),
         theta_forecasting::diagnostic(Forecaster, residual_indices(Indices)).

The `residuals(Errors)` diagnostic term holds a list of numbers, following the
convention used by `exponential_smoothing` and `time_series_regression`.
The separate `residual_indices(Indices)` diagnostic term holds the original
time positions, preserving the time axis through gaps. Both lists have
length `K-1`, the value recorded by the `scored_count/1` diagnostic, and
correspond element for element. They omit missing targets, leading and
trailing gaps, and the first known initialization anchor for either
initialization strategy. No observations or caller variables are retained.

Errors are actual minus pre-update SES fit on the original seasonal scale,
matching the `error_basis(ses_training_fit)` diagnostic term. For `[10,12,14,16]`
with alpha 0.5, first initialization and no seasonality, the lists are
`residuals([2,3,3.5])` and `residual_indices([2,3,4])`. For `[10,_,14,16]`,
they are `residuals([4,4])` and `residual_indices([3,4])`.

With retention disabled, both diagnostics contain `none`. Both
diagnostics and the effective retention option are mandatory in every
model. Retained histories are ground and included in clause and file
exports. They reuse the fitted errors and indices without collecting or
optimizing the dataset again.

Validation checks option/payload agreement, finite numeric errors, equal
list lengths matching the scored count, and strictly increasing integer
indices after the anchor and within the elapsed length. It does not
recompute aggregate errors from the retained list or certify that manually
constructed entries came from the original observations. Existing metric
consistency checks remain unchanged. These are full-sample training
residuals, not holdout errors or evidence of out-of-sample accuracy.


Representation and diagnostics
------------------------------

Learned models use the following term representation:

    theta_forecaster(
        theta_state(Level, Slope, Alpha, Correction, Seasonality),
        Diagnostics
    )

The `Seasonality` argument is either the atom `none` or a
`seasonal(Method, Frequency, NextPhase, Factors)` compound term.
Models do not retain observations or optimizer problem objects. Residual
history is optional.

The inherited `diagnostics/2`, `diagnostic/2`, and `forecaster_options/2`
predicates let you inspect the model's metadata. The canonical list of
effective learning options is:

    [
        alpha(Number), initialization(Strategy),
        seasonal(EffectiveMethod), frequency(EffectiveFrequency),
        missing_policy(Policy), retain_residuals(Boolean)
    ]

There are no unresolved auto coefficients or dataset-frequency requests
in these options. Nonseasonal models record frequency one. The fitted
initial level is recorded separately; effective options alone are not a
promise of identical refitting of an optimized initial level.

The `Diagnostics` argument is a list of diagnostic terms. Required terms
include `model(theta)`, `training_series_length/1`, `observed_count(K)`,
`missing_count(N-K)`, `update_count(0)`, `slope/1`, `seasonal_mode/1`,
`frequency/1`, `initial_level/1`, and the first-known `initialization_index(J)`,
`residuals/1`, and `residual_indices/1`. Fitting metadata includes
`fitting(fixed_parameters)` or `fitting(nelder_mead)`, `optimizer_options/1`,
`convergence/1`, `iterations/1`, `evaluations/1`, and
`optimization_sum_squared_error/1` on the adjusted scale.

All five training-error diagnostic terms are mandatory:

- `scored_count(K-1)`
- `sum_absolute_error(Sum)`
- `sum_squared_error(Sum)`
- `mean_absolute_error(MAE)`
- `root_mean_squared_error(RMSE)`

They describe pre-update SES fits at known positions after J, restored to the
original seasonal scale. The `error_basis(ses_training_fit)` diagnostic term
labels this convention. The first known observation anchors the reused fitter
and is not scored, including with optimized initialization.

These are fitted training diagnostics, not causal prefix-wise Theta
forecasts or holdout accuracy. Seasonal factors, slope, alpha and the
optimized initial level use the training sample. For multiplicative
adjustment, original-scale SSE generally differs from the adjusted-scale
objective minimized by the optimizer.

The `check_forecaster/1` predicate rejects partial, duplicated, wrong-arity
or inconsistent diagnostics. It checks numerical state, correction,
canonical options, counters, error totals and seasonal phase.
It also validates the residual-history payloads as described above.

The `valid_forecaster/1` predicate fails without binding invalid terms.


Limitations and cost
--------------------

The library implements only standard Theta with the Theta coefficient fixed
at two. Optimizing the SES alpha or initial level does not optimize that
coefficient. Generalized Theta, online updates, missing-value reconstruction,
prediction intervals, Box-Cox transformations, multiple seasonality,
frequency estimation, and exogenous regression are not provided. To
incorporate new observations, learn a new model.

Learning requires at least two known observations. Very sparse seasonal
data can lack a complete centered window or an
estimable phase. Such data may still support fitting with the
`seasonal(none)` option; seasonal fitting does not impute missing values.

Automatic SES fitting uses bounded local optimization, not a guaranteed
global search. The slope, seasonal factors, and automatic SES settings use
the training sample. Fitted values and retained residuals describe the SES
training fit, not the complete Theta forecast combination or held-out
accuracy. Their error diagnostics do not replace out-of-sample evaluation.

Forecasts are not constrained to be positive or integer-valued. A fitted
negative slope can continue to reduce long-horizon forecasts even when the
known observations are positive.

Numerical results need not match other packages with different SES
initialization, bounds, solver tolerances or seasonal defaults. The
standard combination formula is shared, but fitting conventions matter.
Arithmetic evaluation errors, including overflow in squared errors,
are propagated.

Dataset collection sorts observations by index. Seasonal detection
costs O(N\*m), decomposition O(N+m), and SES optimization O(E*N) for E
objective evaluations. With residual retention disabled, model storage is
O(1) nonseasonal or O(m) seasonal; enabling it adds O(K) storage for the
parallel residual and index lists. Transient fitting data costs O(N+m).
Forecast construction is O(H), following full model validation, which
recomputes the correction in O(N). Seasonal cycling does not repeatedly
index a list per forecast.

Fitted-value construction adds O(N) time and output memory to one normal
fitting operation, reusing its residuals without a second dataset
collection or optimizer invocation.

Retained-history validation adds O(K) work and does not refit the model.


References
----------

- Assimakopoulos, V. and Nikolopoulos, K. (2000). The theta model: a
    decomposition approach to forecasting. International Journal of
    Forecasting, 16(4), 521-530.
- Hyndman, R. J. and Billah, B. (2003). Unmasking the Theta method.
    International Journal of Forecasting, 19(2), 287-290.
