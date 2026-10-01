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

To load all entities in this library, load the ``loader.lgt`` file:

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
- ``replace_diagnostic/4``: replaces a unary diagnostic value while
  preserving metadata order.

These are declared ``protected``, intended to be reused by concrete
forecaster libraries (e.g. ``baseline_forecasting``,
``exponential_smoothing``, ``time_series_regression``,
``similarity_forecasting``) that import this category.
