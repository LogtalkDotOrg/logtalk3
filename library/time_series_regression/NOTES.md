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


`time_series_regression`
========================

Use this library to forecast a single time series from its previous values.
It fits autoregressive (AR) models by least squares, with an optional
intercept and differencing (ARI models). You can also request automatic
order selection using information criteria and analytic prediction
intervals. The library object implements `forecaster_protocol` and reuses
dataset validation, diagnostics, export, lag construction, and differencing
support from the `time_series_protocols` library. Least-squares problems
are solved using the `linear_algebra` library, and prediction interval
quantiles are computed using the `univariate_distributions` library.

Datasets are objects implementing `time_series_dataset_protocol`. Every
observation must be a number or a missing observation (represented as an
unbound variable; see the "Missing observations" section below).


API documentation
-----------------

Open the [../../apis/library_index.html#time-series-regression](../../apis/library_index.html#time-series-regression)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(time_series_regression(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(time_series_regression(tester)).

The test suite compares fitted coefficients, error sums, information
criteria, forecasts, and prediction interval bounds for a noisy AR(2)
dataset with values computed independently using NumPy least squares
(and, for prediction intervals, SciPy's normal quantile function), and
covers learning, forecasting, and updating with missing observations,
including automatic order selection over a series with scattered
missing observations.


Model
-----

An AR(p) model with differencing order d and an intercept predicts the
d-times differenced series `y` as:

	y(t) = c + phi(1) * y(t-1) + ... + phi(p) * y(t-p) + e(t)

The intercept `c` and the coefficients `phi(i)` are estimated by
conditional least squares: the model is fitted to the `n - d - p`
observations of the differenced series that have `p` preceding values.
Least squares is solved using a pivoted orthogonal (QR) method that does
not form the normal equations. Forecasts are computed recursively and, when
differencing is used, integrated back to the scale of the original series.

To fit an order-two model and request twelve forecasts:

	| ?- time_series_regression::learn(my_series, Forecaster, [order(2)]),
	     time_series_regression::forecast(Forecaster, 12, Forecasts).


Options
-------

The `learn/3` predicate supports the following options:

- `order(Order)` sets the autoregressive order to a positive
  integer or `auto`. The default is `1`.
- `max_order(MaxOrder)` limits the orders considered with the
  `order(auto)` option. The default is `10`; it is valid only with `order(auto)`.
- `selection_criterion(Criterion)` selects the criterion used
  with `order(auto)`: `aic`, `aicc`, or `bic`. The default is `aicc`; it is
  valid only with `order(auto)`.
- `intercept(Boolean)` controls whether to estimate an intercept.
  The default is `true`.
- `differencing(Differencing)` sets the number of times the
  series is differenced before fitting, a non-negative integer. The default is `0`.
- `retain_residuals(Boolean)` controls whether to keep the
  one-step training residuals in the learned forecaster diagnostics. The default is `false`.

Passing a `max_order/1` or `selection_criterion/1` option together with an explicit
integer order raises a `domain_error(time_series_regression_option, Option)`
error.

The minimum series length is `Differencing + 2 * Order + Intercept`, where
`Intercept` is `1` or `0`, and `Differencing + Intercept + 4` for
the `order(auto)` option. A shorter series causes the `learn/3` predicate
to raise a `domain_error(series_length, Dataset)` error.


Automatic order selection
-------------------------

With the `order(auto)` option, the library compares orders from `1` up to
the limit supplied by the `max_order/1` option. This limit is capped so
that every candidate has more observations than parameters, plus one.
Candidates are compared using the selected information criterion:

- `aic`: `n * ln(SSE/n) + 2k`
- `aicc`: `aic + 2k(k+1)/(n-k-1)`
- `bic`: `n * ln(SSE/n) + k * ln(n)`

where `k` is the number of estimated regression coefficients and `n` and
`SSE` are the sample size and sum of squared errors. To make the criteria
comparable, all candidate orders are fitted to the same sample, which is
the one available to the largest candidate order. The selected order is
then refitted using all the observations available to it. Ties are broken
in favor of the smaller order. The candidate scores are recorded in the
`order_selection(Criterion, Candidates)` diagnostic, where `Candidates` is
a list of `Order-Score` pairs.


Rank-deficient designs
----------------------

When the design matrix is rank deficient (for example, when fitting an
intercept to a constant or differenced linear-trend series), the least-squares
solution is not unique. The solver returns a basic solution with the
dependent columns dropped. Forecasts are still valid least-squares
forecasts, but the individual coefficient values should not be interpreted.
The `design_rank/1` diagnostic reports the numerical rank of the design
matrix.


Missing observations
--------------------

A missing observation is represented, following common practice, as an
unbound variable: `observation(Index, _)` in a dataset object, or an
unbound `Observation` argument to the `update/3` predicate. A series may
freely mix numbers and missing observations; only its length and index
sequence need to be well-formed (checked by the shared `dataset_series/2`
and `check_series_length/3` predicates).

Differencing propagates missingness: a difference with a missing operand
is itself missing. Fitting uses casewise deletion: a design matrix row
(built from `Order` lagged values and a target, after differencing) that
involves any missing value is excluded from the least-squares fit. This
is applied identically to an explicit order and to every candidate order
considered by automatic order selection (candidates are still compared
on a common sample, now the sample complete at the largest candidate
order). The number of raw missing observations in the training series is
reported in the `missing_count/1` diagnostic; `scored_count/1` already
reflects the number of complete rows actually used for fitting. If casewise
deletion leaves no complete row at all, the `learn/3` predicate raises a
`domain_error(insufficient_observations, Dataset)` error.

Learning can succeed even if some recent past observations are missing,
provided enough complete rows remain. Those missing values are retained in
the lag window or in the levels used to undo differencing. If any past
value required to calculate a forecast is missing, the `forecast/3`
predicate raises `domain_error(missing_observation, Forecaster)` for a
positive horizon. Missing required past values also prevent positive-horizon
prediction intervals. A zero horizon still returns empty forecast or bound
lists.

The `update/3` predicate appends a new observation; it does not fill in a
missing historical observation. Forecasting becomes possible once enough
numeric observations have been appended that every value in the current
window and levels is known. Without differencing, a missing window entry
is pushed out after at most `Order` numeric updates. Differencing can
propagate a gap and require additional numeric updates.


Immutable online updates
------------------------

The `update/3` predicate returns a new forecaster after appending one
observation to the series while keeping the fitted intercept and
coefficients unchanged. The original forecaster is not modified. The new
`Observation` may be left an unbound variable to represent a missing
observation (see "Missing observations" above).

When the new observation, its required differences, and the past values
in the forecaster's prior window are all numeric, the one-step prediction
error updates the `sum_squared_error/1`, `mean_squared_error/1`, and
`scored_count/1` diagnostic values (and is appended to the retained
residuals, if enabled), and the information criteria diagnostics keep
describing the original fit. Otherwise, no prediction error is available
and only `training_series_length/1`, `update_count/1`, and, when
`Observation` is a variable, `missing_count/1` are updated. In both cases
`training_series_length/1` and `update_count/1` are incremented, and the
observation, known or not, is pushed into the window (and used to update
the levels).


Forecaster representation
-------------------------

A learned forecaster uses the following term representation:

	time_series_regression_forecaster(Model, State, Parameters, Diagnostics)

The `Model` argument is an `ar(Order, Differencing)` term, `State` is an
`ar_state(Window, Levels)` term, and `Parameters` is an
`ar_parameters(Intercept, Coefficients)` term. The window holds the last `Order`
values of the differenced series, most recent first. The levels list holds
the last value of the series at each differencing level, starting with the
original series. The intercept is `0.0` when the `intercept(false)` option is used.

The `Diagnostics` argument is a list of diagnostic terms. It includes
`model/1`, `training_series_length/1`, and `options/1` terms common to all
forecasters plus `order/1`, `differencing/1`, `intercept/1`,
`missing_count/1`, `parameter_count/1`, `scored_count/1`, `design_rank/1`,
`sum_squared_error/1`, `mean_squared_error/1`, `aic/1`, `aicc/1` (with value
`none` when undefined), `bic/1`, `update_count/1`, and `residuals/1` (with
value `none` unless retained) terms, and `order_selection/2` when the
`order(auto)` option is used.


Prediction intervals
--------------------

The `forecast_interval/5` predicate computes analytic prediction interval
bounds for the next `Horizon` forecasts:

	time_series_regression::forecast_interval(Forecaster, Horizon, Lower, Upper, Options)

The bounds assume independent, identically distributed, zero-mean Gaussian
innovations and treat the fitted intercept and coefficients as known. The
`h`-step forecast error variance is the residual variance times the sum of
the squared `psi(0)..psi(h-1)` weights of the model (the AR polynomial
combined with the differencing polynomial, when the `differencing/1` option is
positive), and the bounds are the point forecast plus or minus the standard
normal quantile for the requested confidence times the forecast error
standard deviation. The residual variance is the value of the
`sum_squared_error/1` diagnostic divided by the residual degrees of freedom:
the `scored_count/1` diagnostic value minus the `design_rank/1` diagnostic
value. A zero `Horizon` returns two empty lists.

The `Options` argument accepts interval options, validated independently from
the learning options of the `learn/3` predicate. The interval options are:

- `confidence(Level)` sets central interval coverage, a number in the open
  interval `]0.0, 1.0[` (default: `0.95`).
- `method(normal)` selects the only supported interval method (default, and
  currently the only accepted value).

When the residual degrees of freedom are not positive (an exactly
determined fit, where the `scored_count/1` and `design_rank/1` diagnostic
values are equal), calling the `forecast_interval/5` predicate with a positive
`Horizon` raises a `domain_error(residual_degrees_of_freedom, Forecaster)`
error, since the residual variance is then undefined.

The standard normal quantile is computed by the `univariate_distributions`
library.


Limitations
-----------

- The library fits linear autoregressive models with optional differencing
  and an intercept. Moving-average error terms, dedicated seasonal models,
  multivariate series, and exogenous regressors are not provided.
- Missing observations are handled by casewise deletion at fitting time,
  which discards an entire design matrix row rather than imputing a value
  or modeling the series with a method robust to missing data (such as a
  state-space or Kalman filter formulation). Without differencing, one gap
  can invalidate up to `Order + 1` consecutive rows; differencing can spread
  it further. This reduces the effective
  sample size and, under automatic order selection, can favor a lower
  order than the same series would with no missing observations.
- Prediction intervals are the analytic normal-theory approximation
  described above. They do not account for uncertainty in the estimated
  intercept and coefficients (only in future innovations), which
  can understate interval width, particularly for short training series or
  high orders; no bootstrap or simulation-based alternative is provided.
  They are also unavailable whenever the forecaster's window or levels
  are not fully known (see "Missing observations" above).
- Positive-horizon intervals also require a positive residual degrees of
  freedom: the `scored_count/1` diagnostic value must exceed the
  `design_rank/1` diagnostic value. Learning an exactly determined model
  can succeed even though the `forecast_interval/5` predicate cannot
  provide positive-horizon bounds for it.
- The `update/3` predicate keeps the fitted intercept, coefficients, and
  selected order fixed. It advances the forecasting state and error totals
  but does not refit the regression or repeat order selection. The recorded
  information criteria remain those from the original fit; learn a new
  model when parameter or order estimates need to change.


References
----------

- Hyndman, R.J. and Athanasopoulos, G. (2021). *Forecasting: Principles
  and Practice*. 3rd edition. OTexts. Chapter 9: autoregressive models,
  differencing, estimation, and order selection.
  https://otexts.com/fpp3/arima.html
