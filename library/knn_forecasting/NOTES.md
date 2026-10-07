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


`knn_forecasting`
=================

This library implements univariate time series forecasting using the
k-Nearest Neighbors ("analog") method: the most recent window of
observations is matched, by distance, against historical windows of the
same length, and the forecast is an aggregate of what actually followed
the most similar ones. It implements the `forecaster_protocol` and
reuses dataset validation, diagnostics, export, lag construction,
differencing, and forecast error metric support from the
`time_series_protocols` library.

Unlike `exponential_smoothing` and `time_series_regression`, this is a
lazy (instance-based) learner: `learn/3` does not fit any parameters, it
memorizes the historical windows themselves, and every forecast is
computed by searching those memorized windows.

Datasets are objects implementing the `time_series_dataset_protocol`
protocol. Every observation must be a number or a missing observation,
represented as an unbound variable (see "Missing observations" below).


API documentation
-----------------

Open the [../../apis/library_index.html#knn-forecasting](../../apis/library_index.html#knn-forecasting)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(knn_forecasting(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(knn_forecasting(tester)).

The test suite checks exact recovery on periodic and seasonal patterns
(where the nearest historical window is an exact match), compares
distance metrics, weighting schemes, and leave-one-out diagnostics for a
synthetic noisy dataset against an independent Python re-implementation
of the same nearest-neighbor search, and covers learning, forecasting,
and updating with missing observations.


Model
-----

Given a window length (`order/1`) `p`, the (optionally differenced)
training series is turned into `Lags-Target` rows exactly as for
`time_series_regression` (via the shared `lagged_rows/3` predicate):
`Lags` is the `p` most recent values before `Target`, most recent
first. These rows are memorized as-is; there is no fitting step.

To forecast one step ahead from a window `W` (the `p` most recent
values, most recent first), the `k(K)` nearest memorized rows to `W` are
found (by `distance_metric/1`) and their targets are aggregated
(weighted by `weight_scheme/1`) into the prediction. Multi-step
forecasts are computed recursively: each prediction is pushed into the
window (dropping the oldest value) before finding the next set of
neighbors, exactly as `time_series_regression` recursively projects AR
forecasts. When differencing is used, forecasts are integrated back to
the scale of the original series.

Typical usage:

	| ?- knn_forecasting::learn(my_series, Forecaster, [order(5), k(3)]),
	     knn_forecasting::forecast(Forecaster, 12, Forecasts).


Options
-------

The `learn/3` predicate supports the following options:

- `order(Order)`: window length, a positive integer. Default is `3`.
- `k(K)`: number of nearest neighbors, a positive integer. Default is
  `3`.
- `distance_metric(Metric)`: one of `euclidean`, `manhattan`,
  `chebyshev`, or `minkowski`. Default is `euclidean`.
- `minkowski_power(Power)`: the Minkowski distance power, a number no
  smaller than `1.0`. Only used when `distance_metric(minkowski)` is
  selected; accepted (but unused) with every other metric. Default is
  `3.0`.
- `weight_scheme(Scheme)`: how neighbors are aggregated; one of
  `uniform` (plain average), `distance` (inverse-distance weighted), or
  `gaussian` (weighted by `exp(-distance^2 / 2)`). Default is `uniform`.
- `differencing(Differencing)`: number of times the series is
  differenced before matching, a non-negative integer. Default is `0`.

The minimum series length is `Differencing + Order + K + 1`: enough for
at least `K + 1` rows after differencing and windowing, which is also
enough for the leave-one-out cross-validation described below. Below
that, `learn/3` throws a `domain_error(series_length, Dataset)` error.


Distance metrics and weighting
-------------------------------

Distance metrics and neighbor weighting follow the same conventions,
option names, and formulas as the `knn_regression` library, so the two
are directly comparable:

- `euclidean_distance/3`, `manhattan_distance/3`, `chebyshev_distance/3`,
  and `minkowski_distance/4` (from the `numberlist` library) compute the
  distance between the query window and a memorized window.
- `uniform` weighting averages the `K` neighbors' targets equally.
- `distance` weighting weights each neighbor by `1 / distance` (a very
  small distance is capped at a large fixed weight instead of dividing
  by (near) zero).
- `gaussian` weighting weights each neighbor by `exp(-distance^2 / 2)`.

Unlike `knn_regression`, there is no per-attribute feature scaling
option: every "feature" of a window is a past value of the same series,
already on the same scale, so cross-feature scaling does not apply here.
Per-window normalization (matching windows by shape rather than level,
as is common in time series similarity search) is not implemented; see
"Limitations" below.


Leave-one-out training diagnostics
-----------------------------------

Since this is a lazy learner, there is no fitted in-sample error the way
there is for `time_series_regression`. Instead, `learn/3` computes a
leave-one-out (LOO) cross-validation over the memorized rows: each row's
target is predicted from the `K` nearest of the *other* rows (using the
same distance metric and weighting scheme configured for forecasting),
and the resulting errors are summarized using the shared
`mean_absolute_error/3` and `root_mean_squared_error/3` predicates from
`time_series_protocols`. This is an `O(n^2)` computation (every row is
compared against every other row), so `learn/3` may be slow for very
long training series; see "Limitations" below.


Missing observations
--------------------

A missing observation is represented, following the same convention as
`time_series_regression`, as an unbound variable: `observation(Index,
_)` in a dataset object, or an unbound `Observation` argument to
`update/3`. A series may freely mix numbers and missing observations;
only its length and index sequence need to be well-formed (checked as
usual by `dataset_series/2` and `check_series_length/3`).

Differencing propagates missingness: a difference with a missing
operand is itself missing. A memorized row (built from `Order` lagged
values and a target, after differencing) that involves any missing
value is excluded from the memorized set (casewise deletion); since
every memorized row is therefore always fully known, this has no effect
on the leave-one-out cross-validation, which always runs over complete
rows. The number of raw missing observations in the training series is
reported in the `missing_count/1` diagnostic; `scored_count/1` already
reflects the number of complete rows actually memorized. If casewise
deletion leaves `K` or fewer complete rows, `learn/3` raises a
`consistency_error(k, K, RowCount)` error.

`learn/3` still succeeds when the most recent observations (needed to
seed the forecaster's window or, under differencing, its levels) are
missing; the missing values are simply carried into the learned
forecaster's state. `forecast/3` then raises a
`domain_error(missing_observation, Forecaster)` error for a positive
horizon (a zero horizon still trivially succeeds), until `update/3`
supplies the missing values. Note that a missing window entry is only
resolved once it has been pushed out of the window by `Order` further
updates (as for `time_series_regression`), not by the next update alone
unless `Order` is `1`.


Immutable online updates
------------------------

The `update/3` predicate returns a new forecaster after appending one
observation to the series. Unlike `time_series_regression`, the
memorized rows (and so the set of possible analogs) are kept unchanged;
only the forecasting window (and, under differencing, the levels) are
advanced. This keeps the cost of an update, and of every subsequent
forecast, independent of how many updates have been applied, at the
cost of the model never learning from observations seen after `learn/3`
was called. The original forecaster is not modified. The new
`Observation` may be left an unbound variable to represent a missing
observation (see "Missing observations" above).

When both the new observation and the forecaster's prior window are
fully known, the one-step prediction error for the new observation
(predicted from the prior window using the same neighbor search used
for forecasting) is added to the `sum_squared_error/1`,
`mean_squared_error/1`, `sum_absolute_error/1`, and
`mean_absolute_error/1` diagnostics. Otherwise, no prediction error is
available and only `training_series_length/1`, `update_count/1`, and,
when `Observation` is a variable, `missing_count/1` are updated. In both
cases `training_series_length/1` and `update_count/1` are incremented,
and the observation, known or not, is pushed into the window (and used
to update the levels).


Forecaster representation
--------------------------

A learned forecaster is represented as a term with the format:

	knn_forecaster(Model, State, Rows, Diagnostics)

where `Model` is `knn(Order, Differencing, K, DistanceMetric,
MinkowskiPower, WeightScheme)`, `State` is `knn_state(Window, Levels)`,
and `Rows` is the memorized list of `Lags-Target` pairs. The window
holds the last `Order` values of the differenced series, most recent
first. The levels list holds the last value of the series at each
differencing level, starting with the original series (as in
`time_series_regression`).

The diagnostics list includes the `model/1`, `training_series_length/1`,
and `options/1` terms common to all forecasters plus `order/1`,
`differencing/1`, `missing_count/1`, `k/1`, `distance_metric/1`,
`minkowski_power/1`, `weight_scheme/1`, `scored_count/1`,
`sum_squared_error/1`, `mean_squared_error/1`, `sum_absolute_error/1`,
`mean_absolute_error/1`, and `update_count/1` terms. `scored_count/1` is
initially the number of memorized rows (the leave-one-out sample size)
and grows by one with each successful, fully known `update/3` call.


Limitations
-----------

- Missing observations are handled by casewise deletion when memorizing
  rows, which discards an entire row (up to `Order + 1` consecutive rows
  per missing observation) rather than imputing a value; this reduces
  the pool of analogs available for matching, and can turn an otherwise
  sufficient series into one with too few complete rows for the
  requested `k/1`.
- Automatic selection of `order/1` or `k/1` is not implemented; both
  must be set explicitly (or left at their defaults). Choosing a good
  window length is a well-known hard problem for analog methods; a
  cross-validation or false-nearest-neighbors-based search could be
  added in the future.
- Matching is done on raw (or differenced) values, not on a per-window
  normalized (e.g. z-scored) shape, so two windows with the same shape
  but a different level or scale are not recognized as similar analogs.
- Distance search is brute-force (every memorized row is compared
  against the query window), and the leave-one-out training diagnostics
  compare every row against every other row; both are `O(n)` per
  forecast step and `O(n^2)` at `learn/3` time, respectively, which may
  be slow for very long training series.
- Prediction intervals are not provided.
- `update/3` never grows the memorized set of analogs, so the model's
  pool of historical patterns is fixed at `learn/3` time; observations
  supplied only through `update/3` extend the forecasting window but
  are never themselves available as future analogs.
