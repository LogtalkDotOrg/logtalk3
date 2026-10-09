.. _library_knn_forecasting:

``knn_forecasting``
===================

Use this library to forecast from similar patterns in a single time
series. The k-Nearest Neighbors ("analog") method compares the most
recent window of observations with historical windows of the same
length. It combines what followed the closest matches to produce a
forecast. The library object implements ``forecaster_protocol`` and
reuses dataset validation, diagnostics, export, lag construction,
differencing, and forecast error metric support from the
``time_series_protocols`` library.

Unlike ``exponential_smoothing`` and ``time_series_regression``, this is
a lazy (instance-based) learner: the ``learn/3`` predicate does not fit
parameters. Instead, it memorizes the historical windows, and every
forecast is computed by searching those memorized windows.

Datasets are objects implementing ``time_series_dataset_protocol``.
Every observation must be a number or a missing observation (represented
as an unbound variable; see the "Missing observations" section below).

API documentation
-----------------

Open the
`../../apis/library_index.html#knn-forecasting <../../apis/library_index.html#knn-forecasting>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(knn_forecasting(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(knn_forecasting(tester)).

The test suite checks exact recovery on periodic and seasonal patterns
(where the nearest historical window is an exact match), compares
distance metrics, weighting schemes, and leave-one-out diagnostics for a
synthetic noisy dataset against an independent Python re-implementation
of the same nearest-neighbor search, and covers learning, forecasting,
and updating with missing observations.

Model
-----

Given a window length ``p``, supplied by the ``order/1`` option, the
(optionally differenced) training series is turned into ``Lags-Target``
rows exactly as for the ``time_series_regression`` library (via the
shared ``lagged_rows/3`` predicate): ``Lags`` is the ``p`` most recent
values before ``Target``, most recent first. These rows are memorized
as-is; there is no fitting step.

To forecast one step ahead from a window ``W`` (the ``p`` most recent
values, most recent first), the library finds the number of nearest
memorized rows requested by the ``k(K)`` option. The
``distance_metric/1`` option determines how matches are compared, and
the ``weight_scheme/1`` option determines how their targets are combined
into a prediction. Multi-step forecasts are computed recursively: each
prediction is pushed into the window (dropping the oldest value) before
finding the next set of neighbors, exactly as ``time_series_regression``
recursively projects AR forecasts. When differencing is used, forecasts
are integrated back to the scale of the original series.

To learn a forecaster with a five-observation window and three
neighbors, then request twelve forecasts:

::

   | ?- knn_forecasting::learn(my_series, Forecaster, [order(5), k(3)]),
        knn_forecasting::forecast(Forecaster, 12, Forecasts).

Options
-------

The ``learn/3`` predicate supports the following options:

- ``order(Order)`` sets the window length, a positive integer. The
  default is ``3``.
- ``k(K)`` sets the number of nearest neighbors, a positive integer. The
  default is ``3``.
- ``distance_metric(Metric)`` selects ``euclidean``, ``manhattan``,
  ``chebyshev``, or ``minkowski``. Default is ``euclidean``.
- ``minkowski_power(Power)`` sets the Minkowski distance power, a number
  no smaller than ``1.0``. Only used when ``distance_metric(minkowski)``
  is selected; accepted (but unused) with every other metric. Default is
  ``3.0``.
- ``weight_scheme(Scheme)`` selects how neighbors are combined:
  ``uniform`` (plain average), ``distance`` (inverse-distance weighted),
  or ``gaussian`` (weighted by ``exp(-distance^2 / 2)``). Default is
  ``uniform``.
- ``differencing(Differencing)`` sets the number of times the series is
  differenced before matching, a non-negative integer. Default is ``0``.

The minimum series length is ``Differencing + Order + K + 1``: enough
for at least ``K + 1`` rows after differencing and windowing, which is
also enough for the leave-one-out cross-validation described below.
Below that, the ``learn/3`` predicate raises
``domain_error(series_length, Dataset)``.

Distance metrics and weighting
------------------------------

Distance metrics and neighbor weighting follow the same conventions,
option names, and formulas as the ``knn_regression`` library, so the two
are directly comparable:

- ``euclidean_distance/3``, ``manhattan_distance/3``,
  ``chebyshev_distance/3``, and ``minkowski_distance/4`` predicates from
  the ``numberlist`` library compute the distance between the query
  window and a memorized window.
- ``uniform`` weighting averages the ``K`` neighbors' targets equally.
- ``distance`` weighting weights each neighbor by ``1 / distance`` (a
  very small distance is capped at a large fixed weight instead of
  dividing by (near) zero).
- ``gaussian`` weighting weights each neighbor by
  ``exp(-distance^2 / 2)``.

Unlike ``knn_regression``, there is no per-attribute feature scaling
option: every "feature" of a window is a past value of the same series,
already on the same scale, so cross-feature scaling does not apply here.
Per-window normalization (matching windows by shape rather than level,
as is common in time series similarity search) is not implemented; see
"Limitations" below.

Leave-one-out training diagnostics
----------------------------------

Since this is a lazy learner, there is no fitted in-sample error as in
the ``time_series_regression`` library. Instead, the ``learn/3``
predicate computes leave-one-out (LOO) cross-validation over the
memorized rows: each row's target is predicted from the ``K`` nearest of
the *other* rows (using the same distance metric and weighting scheme
configured for forecasting), and the resulting errors are summarized
using the shared ``mean_absolute_error/3`` and
``root_mean_squared_error/3`` predicates from ``time_series_protocols``.
Every row is compared against every other row, and each search sorts its
distances. For fixed order, this takes ``O(n^2 log n)`` time, so the
``learn/3`` predicate may be slow for very long training series; see
"Limitations" below.

Missing observations
--------------------

A missing observation is represented, following the same convention as
``time_series_regression``, as an unbound variable:
``observation(Index, _)`` in a dataset object, or an unbound
``Observation`` argument to the ``update/3`` predicate. A series may
freely mix numbers and missing observations; only its length and index
sequence need to be well-formed (checked as usual by the shared
``dataset_series/2`` and ``check_series_length/3`` predicates).

Differencing propagates missingness: a difference with a missing operand
is itself missing. A memorized row (built from ``Order`` lagged values
and a target, after differencing) that involves any missing value is
excluded from the memorized set (casewise deletion). Leave-one-out
cross-validation uses only the remaining complete rows. Removing
incomplete rows can change the available neighbors and the resulting
error diagnostics. The number of raw missing observations in the
training series is reported in the ``missing_count/1`` diagnostic;
``scored_count/1`` already reflects the number of complete rows actually
memorized. If casewise deletion leaves ``K`` or fewer complete rows, the
``learn/3`` predicate raises a ``consistency_error(k, K, RowCount)``
error.

Learning can succeed even if some recent past observations are missing,
provided enough complete rows remain. Those missing values are retained
in the lag window or in the levels used to undo differencing. If any
past value required to calculate a forecast is missing, the
``forecast/3`` predicate raises
``domain_error(missing_observation, Forecaster)`` for a positive
horizon. A zero horizon still returns an empty list.

The ``update/3`` predicate appends a new observation; it does not fill
in a missing historical observation. Forecasting becomes possible once
enough numeric observations have been appended that every value in the
current window and levels is known. Without differencing, a missing
window entry is pushed out after at most ``Order`` numeric updates.
Differencing can propagate a gap and require additional numeric updates.

Immutable online updates
------------------------

The ``update/3`` predicate returns a new forecaster after appending one
observation to the series. The memorized rows, and therefore the set of
historical patterns available for matching, are kept unchanged; only the
forecasting window (and, under differencing, the levels) are advanced.
This keeps the cost of an update, and of every subsequent forecast,
independent of how many updates have been applied, at the cost of the
model never learning from observations seen after the ``learn/3``
predicate was called. The original forecaster is not modified. The new
``Observation`` may be left an unbound variable to represent a missing
observation (see "Missing observations" above).

When the new observation, its required differences, and the past values
in the forecaster's prior window are all numeric, the one-step
prediction error (predicted from the prior window using the same
neighbor search used for forecasting) is added to the
``sum_squared_error/1``, ``mean_squared_error/1``,
``sum_absolute_error/1``, and ``mean_absolute_error/1`` diagnostics.
Otherwise, no prediction error is available and only
``training_series_length/1``, ``update_count/1``, and, when
``Observation`` is a variable, ``missing_count/1`` are updated. In both
cases ``training_series_length/1`` and ``update_count/1`` are
incremented, and the observation, known or not, is pushed into the
window (and used to update the levels).

Forecaster representation
-------------------------

A learned forecaster uses the following term representation:

::

   knn_forecaster(Model, State, Rows, Diagnostics)

The ``Model`` argument is a
``knn(Order, Differencing, K, DistanceMetric, MinkowskiPower, WeightScheme)``
term, and ``State`` is a ``knn_state(Window, Levels)`` term. The
``Rows`` argument is the memorized list of ``Lags-Target`` pairs. The
window holds the last ``Order`` values of the differenced series, most
recent first. The levels list holds the last value of the series at each
differencing level, starting with the original series (as in
``time_series_regression``).

The ``Diagnostics`` argument is a list of diagnostic terms. It includes
the ``model/1``, ``training_series_length/1``, and ``options/1`` terms
common to all forecasters plus ``order/1``, ``differencing/1``,
``missing_count/1``, ``k/1``, ``distance_metric/1``,
``minkowski_power/1``, ``weight_scheme/1``, ``scored_count/1``,
``sum_squared_error/1``, ``mean_squared_error/1``,
``sum_absolute_error/1``, ``mean_absolute_error/1``, and
``update_count/1`` terms. ``scored_count/1`` is initially the number of
memorized rows (the leave-one-out sample size) and grows by one whenever
a call to the ``update/3`` predicate can score a numeric target using a
window of known past values.

Limitations
-----------

- Missing observations are handled by casewise deletion when memorizing
  rows, which discards an entire row rather than imputing a value.
  Without differencing, one missing observation can invalidate up to
  ``Order + 1`` consecutive rows; differencing can spread the gap
  further. This reduces the pool of analogs available for matching, and
  can turn an otherwise sufficient series into one with too few complete
  rows for the requested ``k/1`` option.
- Automatic selection of the ``order/1`` or ``k/1`` options is not
  implemented; choose them explicitly or use their defaults. The library
  does not search for a suitable window length or number of neighbors.
- Matching is done on raw (or differenced) values, not on a per-window
  normalized (e.g. z-scored) shape, so two windows with the same shape
  but a different level or scale are not recognized as similar analogs.
- Distance search is brute-force (every memorized row is compared
  against the query window), and the leave-one-out training diagnostics
  compare every row against every other row. Each search sorts all
  distances before taking the nearest neighbors. For fixed order, this
  takes ``O(n log n)`` time per forecast step and ``O(n^2 log n)`` time
  for leave-one-out diagnostics, where ``n`` is the number of memorized
  rows. Long training series can therefore be expensive.
- Leave-one-out diagnostics exclude only the row being scored, not rows
  from later times or overlapping windows. They are not rolling-origin
  validation and should not be treated as out-of-sample forecast
  accuracy.
- Prediction intervals are not provided.
- The ``update/3`` predicate never grows the memorized set of analogs,
  so the pool of historical patterns is fixed when the ``learn/3``
  predicate runs. Observations supplied only through the ``update/3``
  predicate extend the forecasting window but are never themselves
  available as future analogs.
