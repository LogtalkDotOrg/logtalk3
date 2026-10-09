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


`time_series_protocols`
=======================

Use this library when implementing a time series forecaster or providing
a dataset for one. It defines the protocols that let datasets and
forecasting algorithms work together. Datasets are represented as objects
implementing the `time_series_dataset_protocol` protocol. Forecasters
are represented as objects importing the `forecaster_common` category.
This library also provides a category defining common predicates for
dataset validation, differencing and reconstruction, lag construction,
forecast error metrics, naive baselines, diagnostics metadata, export,
and pretty-printing support.

Learned forecasters expose diagnostics using the shared `diagnostics/2`,
`diagnostic/2`, and `forecaster_options/2` predicates. Concrete
forecaster implementations store effective training options in the
diagnostics metadata under an `options(Options)` term.

Dataset observation indices must form a complete, gap-free, 1-based
sequence. The dataset's `series_length/1` predicate must report a positive
length matching the number of enumerated observations. When provided,
its `frequency/1` predicate must report a positive integer; this is dataset
metadata, not a learning option.

Forecast horizons are non-negative integers. A zero horizon produces an
empty forecast list. Forecast error metrics require non-empty numeric
actual and predicted series of equal length.

This library also provides time series test datasets under the
`test_datasets` directory.


API documentation
-----------------

Open the [../../apis/library_index.html#time-series-protocols](../../apis/library_index.html#time-series-protocols)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(time_series_protocols(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(time_series_protocols(tester)).


Test datasets
-------------

The `test_datasets` directory includes the following compact time series
datasets and validation fixtures:

- `gap_index.lgt`: invalid dataset fixture containing a gap in the observation index sequence (index 3 is missing).
- `inconsistent_series_length.lgt`: invalid dataset fixture whose declared series length differs from its number of observations.
- `linear_trend.lgt`: simple non-seasonal series following the line `value = 2*index + 8` (values 10, 12, 14, 16, 18, 20).
- `malformed_series_lengths.lgt`: invalid dataset fixtures declaring zero and non-integer series lengths.
- `non_numeric_value.lgt`: invalid dataset fixture containing a non-numeric observation value.
- `seasonal_series.lgt`: two repetitions of a length-4 seasonal cycle (10, 20, 15, 5) with a declared `frequency/1` of 4.
- `short_series.lgt`: two-observation series, too short for most forecaster minimum-length checks.


Shared predicates
-----------------

The `forecaster_common` category provides common auxiliary predicates that
include:

- `dataset_series/2` collects a dataset's observation values.
	It checks that indices form a complete, gap-free, 1-based sequence and
	that the declared length matches the observed length.
- `check_series/2` and `check_series_length/3` check
	that a series is non-empty, numeric, and long enough for an algorithm.
- `difference_series/2` and `integrate_series/3` perform
	first-order differencing and its inverse. They support algorithms that
	difference a series before fitting and reconstruct levels for forecasts.
- `lagged_rows/3` builds `Lags-Target` rows for
	autoregressive-style model fitting.
- `mean_absolute_error/3`, `root_mean_squared_error/3`, and
	`mean_absolute_percentage_error/3` compute standard
	forecast accuracy metrics.
- `accumulate_forecast_error/4` adds a numeric
	actual/prediction error to compact
	`forecast_error_totals(Count, AbsoluteSum, SquaredSum)`. Start with
	`forecast_error_totals(0,0,0)` and supply validated non-negative totals.
	The caller decides which observations to score and must skip missing
	targets.
- `forecast_error_metrics/3` computes MAE and RMSE from
	those totals using a positive integer score count. Neither of these
	error-total predicates retains history. Both propagate arithmetic
	evaluation errors, including overflow when squaring or accumulating
	large errors.
- `naive_forecast/3` and `seasonal_naive_forecast/4` build
	persistence and seasonal-persistence forecasts. Use them as standalone
	baselines, building blocks, or fallbacks in another forecaster.
- `constant_forecast/3` and `linear_trend_forecast/4` build
	constant-value and linear-trend forecasts, as used by mean and drift
	baselines.
- `check_observation/1` checks numeric-or-missing values.
	`series_observation_summary/4` collects elapsed length,
	numeric count, and sum using tail recursion. Neither binds missing
	observations.
- `indexed_series_observations/3` collects known numeric
	values and their original one-based indices. It skips missing values
	without compressing the time axis.
- `normalize_missing_series/3` copies numeric-or-unbound
	observations, using a fresh shared variable for missing positions.
	It does not bind caller variables. The shared variable is returned
	separately and is not a configurable data marker. Smoothing fitters
	accept unbound observations directly and do not need this normalization.
- `residual_fitted_values/3` reconstructs aligned pre-update
	fits as observation minus residual. The first known observation is the
	unscored initialization anchor; it and missing positions receive fresh,
	independent placeholders. The predicate validates inputs without binding
	them and raises `domain_error(residual_count, Residuals)` unless the
	residual count equals the known count minus the anchor (zero for empty
	or all-missing series). Reconstruction takes linear time and output
	memory and propagates arithmetic evaluation errors. To restore seasonal
	fits, use `restore_seasonality/5` at training phase one.
- `valid_residual_history/5` validates ground parallel lists
	of numeric residuals and original integer indices without binding inputs.
	Both lists must match the scored count. Indices must increase strictly
	after the anchor and stay within the elapsed length. The caller checks
	finiteness; this predicate does not reconstruct error totals or certify
	historical observations. Validation is linear in the retained count and
	propagates index/count arithmetic evaluation errors.
- `replace_diagnostic/4` replaces a unary diagnostic value
	while preserving metadata order.
- `updated_observation_diagnostics/3` advances training
	length, update count, observed count, and missing count after an
	observation. It preserves all other metadata and its order.
- `seasonal_autocorrelation_test/4` tests lag-frequency
	autocorrelation using the standard Theta threshold. It reports constant
	series and series with at most two cycles as nonseasonal.
- `classical_seasonal_adjustment/5` computes additive or
	multiplicative factors using centered moving averages. It returns the
	adjusted series and phase-ordered, normalized factors.
- `restore_seasonality/5` restores an additive or
	multiplicative cycle from an explicit starting phase.

These predicates are declared `protected` and intended for reuse by concrete
forecaster libraries (e.g. `baseline_forecasting`, `exponential_smoothing`,
`intermittent_demand_forecasting`, `knn_forecasting`, and
`time_series_regression`) that import this category.

Seasonal preprocessing
----------------------

Seasonal predicates accept numbers and anonymous variables representing
missing observations. The autocorrelation test uses the mean-centered
available-pair biased autocorrelations up to the supplied seasonal lag
and the standard Theta cutoff 1.6448536269514722. Constant data, frequency
one, series with at most two elapsed cycles or `2*Frequency` known values,
and any tested lag with fewer than two known pairs are nonseasonal. The
mean and variance use known values, covariance sums skip unavailable
pairs at original lags, and the test statistic uses the known count.
This missing-data extension is a conservative heuristic, not a calibrated
significance guarantee for arbitrary gaps. Its direct
lag calculation costs O(N*Frequency).

Classical seasonal adjustment requires frequency at least two and at least two
elapsed cycles. It uses only full centered moving-average windows with no
missing observations, including the two-by-frequency filter for even periods,
followed by phase means of
interior differences or ratios. Factors are normalized to zero mean for
additive adjustment or unit mean for multiplicative adjustment. For each
seasonal phase, at least one known observation must have a trend estimate
calculated from such a window; otherwise the lowest unestimable
phase raises `domain_error(insufficient_seasonal_phase_observations, Phase)`.
Windows containing missing observations are discarded, not renormalized or
interpolated. Known
values, including endpoints, are adjusted and missing values remain unbound.
Multiplicative known observations, valid trends and factors must be positive.
Rolling window sums and cycle-wise phase totals cost O(N+Frequency).

Restoration uses an explicit 1-based start phase and cycles factors in
input order, advancing phase through missing positions without binding
them. It validates the phase and factors even when the values
list is empty. These predicates do not interpolate missing values, mutate
input data or silently replace failed seasonal decompositions.


Limitations
-----------

This library supplies interfaces and reusable auxiliary predicates, not a
standalone forecasting algorithm. A concrete forecaster must implement learning
and forecast construction. The `forecaster_protocol` protocol does not declare
online updates, fitted-value extraction, or prediction intervals; check the
chosen implementation for those capabilities and their options.

The dataset interface describes one series of numeric observations at
complete, gap-free, 1-based positions. It has no timestamp, multivariate,
or exogenous-variable interface, and does not resample irregular data.
Represent an unknown observation by an unbound value at its original
position, not by omitting that position. The `dataset_series/2` predicate
collects and sorts the entire dataset in memory; it is not a streaming
dataset reader.

Missing-value support depends on the predicate. The `check_series/2` predicate,
the `difference_series/2` predicate, and the numeric error-metric predicates
require known numeric values. Missing-aware predicates preserve gaps but do
not impute them. A concrete forecaster remains responsible for its missing
policy, minimum usable sample, numerical checks, and model-specific
diagnostic consistency.

Seasonal predicates use a single supplied integer period; they do not estimate
frequency or model multiple seasonal cycles. Sparse gaps can leave too few
lag pairs or complete centered windows to detect or estimate seasonality.
The missing-data autocorrelation test is a heuristic, not a calibrated
significance test for arbitrary gaps. Multiplicative adjustment requires
positive known values, trends, and factors; failed decompositions are not
silently replaced with a nonseasonal fit.
