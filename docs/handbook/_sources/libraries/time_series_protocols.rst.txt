.. _library_time_series_protocols:

``time_series_protocols``
=========================

This library provides protocols used in the implementation of time
series forecasting algorithms. Datasets are represented as objects
implementing the ``time_series_dataset_protocol`` protocol. Forecasters
are represented as objects importing the ``forecaster_common`` category.
This category provides shared helpers for dataset validation,
differencing and reconstruction, lag construction, forecast error
metrics, naive baselines, diagnostics metadata, export, and
pretty-printing support.

Learned forecasters expose diagnostics using the shared
``diagnostics/2``, ``diagnostic/2``, and ``forecaster_options/2``
predicates. Concrete forecaster implementations store effective training
options in the diagnostics metadata under an ``options(Options)`` term.

Dataset observation indices must form a complete, gap-free, 1-based
sequence. The positive length reported by ``series_length/1`` must match
the number of enumerated observations. When provided, ``frequency/1``
must report a positive integer.

Forecast horizons are non-negative integers. A zero horizon produces an
empty forecast list. Forecast error metrics require non-empty numeric
actual and predicted series of equal length.

This library also provides time series test datasets under the
``test_datasets`` directory.

API documentation
-----------------

Open the
`../../apis/library_index.html#time-series-protocols <../../apis/library_index.html#time-series-protocols>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(time_series_protocols(loader)).

Testing
-------

To test this library predicates, shared categories, and datasets, load
the ``tester.lgt`` file:

::

   | ?- logtalk_load(time_series_protocols(tester)).

Test datasets
-------------

The ``test_datasets`` directory includes the following compact time
series datasets and validation fixtures:

- ``gap_index.lgt``: invalid dataset fixture containing a gap in the
  observation index sequence (index 3 is missing).
- ``inconsistent_series_length.lgt``: invalid dataset fixture whose
  declared series length differs from its number of observations.
- ``linear_trend.lgt``: simple non-seasonal series following the line
  ``value = 2*index + 8`` (values 10, 12, 14, 16, 18, 20).
- ``malformed_series_lengths.lgt``: invalid dataset fixtures declaring
  zero and non-integer series lengths.
- ``non_numeric_value.lgt``: invalid dataset fixture containing a
  non-numeric observation value.
- ``seasonal_series.lgt``: two repetitions of a length-4 seasonal cycle
  (10, 20, 15, 5) with a declared ``frequency/1`` of 4.
- ``short_series.lgt``: two-observation series, too short for most
  forecaster minimum-length checks.

Shared predicates
-----------------

``forecaster_common`` provides, among others:

- ``dataset_series/2``: collects a dataset's observation values,
  checking that indices form a complete, gap-free, 1-based sequence and
  that the declared length matches the observed length.

- ``check_series/2`` and ``check_series_length/3``: validate that a
  series is non-empty, numeric, and long enough for a given algorithm.

- ``difference_series/2`` and ``integrate_series/3``: first-order
  differencing and its inverse, for algorithms that difference a series
  before fitting (e.g. ARIMA-style models) and need to undo it on
  forecasts.

- ``lagged_rows/3``: builds ``Lags-Target`` rows from a series for
  autoregressive-style model fitting.

- ``mean_absolute_error/3``, ``root_mean_squared_error/3``,
  ``mean_absolute_percentage_error/3``: standard forecast accuracy
  metrics.

- ``accumulate_forecast_error/4``: adds a numeric actual/prediction
  error to compact
  ``forecast_error_totals(Count, AbsoluteSum, SquaredSum)``, initialized
  with ``forecast_error_totals(0,0,0)``. Requires validated non-negative
  totals; callers decide which observations to score and must skip
  missing targets.

- ``forecast_error_metrics/3``: computes MAE and RMSE from those totals
  using a positive integer score count. These two helpers retain no
  history and propagate arithmetic evaluation errors, including overflow
  when squaring or accumulating large errors.

- ``naive_forecast/3`` and ``seasonal_naive_forecast/4``: persistence
  and seasonal-persistence baseline forecasts, usable both as standalone
  baselines and as building blocks or fallbacks in other forecasters.

- ``constant_forecast/3`` and ``linear_trend_forecast/4``:
  constant-value and linear-trend forecast construction, also used by
  mean and drift baselines.

- ``check_observation/1`` and ``series_observation_summary/4``:
  numeric-or-missing observation validation and tail-recursive
  collection of elapsed length, numeric count, and sum without
  instantiating missing observations.

- ``indexed_series_observations/3``: collects known numeric values and
  their original one-based indices, skipping anonymous-variable
  observations without compressing time positions.

- ``normalize_missing_series/3``: copies numeric-or-unbound observations
  with a fresh shared variable among missing positions, without binding
  caller variables. The shared variable is returned separately and is
  not a configurable data marker. Smoothing fitters accept unbound
  observations directly and do not require this normalization.

- ``residual_fitted_values/3``: reconstructs aligned numeric pre-update
  fits as observation minus residual, treating the first known
  observation as the unscored initialization anchor. Missing positions
  and the anchor receive fresh independent placeholders. It validates
  numeric-or-unbound observations and numeric residuals without binding
  caller inputs, and raises ``domain_error(residual_count, Residuals)``
  unless the residual count equals the known count minus the anchor
  (zero for empty or all-missing series). Reconstruction takes linear
  time and output memory and propagates arithmetic evaluation errors.
  Seasonal fits can be restored with ``restore_seasonality/5`` starting
  at training phase one.

- ``valid_residual_history/5``: validates ground parallel numeric
  residuals and original integer indices without binding inputs. Both
  lists must match the supplied scored count; indices must increase
  strictly after the initialization anchor and remain within the elapsed
  length. The caller checks numeric finiteness. This helper does not
  recompute aggregate errors or certify historical observations.
  Validation is linear in the retained count and propagates index/count
  arithmetic evaluation errors.

- ``replace_diagnostic/4``: replaces a unary diagnostic value while
  preserving metadata order.

- ``updated_observation_diagnostics/3``: advances training length,
  update count, observed count, and missing count after a numeric or
  missing observation, preserving all other metadata and its order.

- ``seasonal_autocorrelation_test/4``: tests the lag-frequency
  autocorrelation using the standard Theta threshold, returning
  nonseasonal for constant series or series with at most two cycles.

- ``classical_seasonal_adjustment/5``: computes classical additive or
  multiplicative seasonal factors using centered moving averages and
  returns the adjusted series and phase-ordered normalized factors.

- ``restore_seasonality/5``: restores an additive or multiplicative
  seasonal cycle from an explicit starting phase.

These are declared ``protected``, intended to be reused by concrete
forecaster libraries (e.g. ``baseline_forecasting``,
``exponential_smoothing``, ``intermittent_demand_forecasting``,
``knn_forecasting``, and ``time_series_regression``) that import this
category.

Seasonal preprocessing
----------------------

Seasonal helpers accept numeric observations and anonymous variables for
missing observations. The autocorrelation test uses the mean-centered
available-pair biased autocorrelations up to the supplied seasonal lag
and the standard Theta cutoff 1.6448536269514722. Constant data,
frequency one, series with at most two elapsed cycles or ``2*Frequency``
known values, and any tested lag with fewer than two known pairs are
nonseasonal. The mean and variance use known values, covariance sums
skip unavailable pairs at original lags, and the test statistic uses the
known count. This missing-data extension is a conservative heuristic,
not a calibrated significance guarantee for arbitrary gaps. Its direct
lag calculation costs O(N*Frequency).

Classical seasonal adjustment requires frequency at least two and at
least two elapsed cycles. It uses complete centered moving-average
windows, including the two-by-frequency filter for even periods,
followed by phase means of interior differences or ratios. Factors are
normalized to zero mean for additive adjustment or unit mean for
multiplicative adjustment. Every original phase must have a valid
interior deviation; otherwise the lowest unestimable phase raises
``domain_error(insufficient_seasonal_phase_observations, Phase)``.
Incomplete windows are discarded, not renormalized or interpolated.
Known values, including endpoints, are adjusted and missing values
remain unbound. Multiplicative known observations, valid trends and
factors must be positive. Rolling window sums and cycle-wise phase
totals cost O(N+Frequency).

Restoration uses an explicit 1-based start phase and cycles factors in
input order, advancing phase through missing positions without binding
them. It validates the phase and factors even when the values list is
empty. These helpers do not interpolate missing values, mutate input
data or silently replace failed seasonal decompositions.
