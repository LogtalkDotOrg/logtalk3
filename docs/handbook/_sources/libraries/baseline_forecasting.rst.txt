.. _library_baseline_forecasting:

``baseline_forecasting``
========================

This library provides four straightforward ways to forecast a single
time series: naive, seasonal naive, mean, and drift forecasting. Use
these methods on their own or as reference forecasts when evaluating
more complex models. The ``baseline_forecasting`` object implements the
``forecaster_protocol`` protocol and imports the ``forecaster_common``
category from the ``time_series_protocols`` library for dataset
collection, validation, diagnostics, forecast construction, and export
support.

Datasets are objects implementing ``time_series_dataset_protocol``.
Observations can be numbers or unbound variables representing missing
values. No backend-specific facilities are required.

API documentation
-----------------

Open the
`../../apis/library_index.html#baseline-forecasting <../../apis/library_index.html#baseline-forecasting>`__
link in a web browser.

Loading
-------

To load this library, load its ``loader.lgt`` file:

::

   | ?- logtalk_load(baseline_forecasting(loader)).

Testing
-------

To test this library, load its ``tester.lgt`` file:

::

   | ?- logtalk_load(baseline_forecasting(tester)).

The test suite checks forecasts against explicit numeric expectations,
seasonal phase alignment, missing observations, malformed datasets and
forecasters, immutable updates, update equivalence to fresh learning,
diagnostics, and export round-trips.

Example
-------

::

   | ?- baseline_forecasting::learn(my_series, Forecaster, [model(drift)]),
        baseline_forecasting::forecast(Forecaster, 12, Forecasts).

Models and options
------------------

The ``learn/3`` predicate accepts a ``model(Method)`` option to select
the forecasting method. The default method is ``naive``. Choose from:

- ``naive``: repeats the final observation. Requires at least one
  position.
- ``seasonal_naive``: repeats the last full cycle of observations.
  Requires at least ``Frequency`` positions. The first forecast
  corresponds to the oldest observation in that final cycle, including
  when the series length is not a multiple of the frequency.
- ``mean``: repeats the arithmetic mean of all known observations.
  Requires at least one position and one known observation.
- ``drift``: extends the line through the first and final observations.
  Requires at least two positions. For horizon step ``h``, the forecast
  is ``Last + h * (Last - First) / (Length - 1)``.

For the ``seasonal_naive`` method, a ``frequency(Frequency)`` learning
option takes precedence over the dataset's ``frequency/1`` predicate.
The frequency must be a positive integer; ``1`` is accepted and gives
persistence forecasts. Without an explicit option, the dataset must
supply a valid frequency. Otherwise, learning raises
``domain_error(seasonal_frequency, Dataset)``.

Supplying a ``frequency/1`` option for any non-seasonal model raises
``domain_error(baseline_forecasting_option, Option)``. Effective options
in diagnostics are canonical: ``[model(Method)]``, or
``[model(seasonal_naive), frequency(Frequency)]`` with the resolved
frequency.

The ``forecast/3`` predicate accepts a non-negative integer horizon. A
zero horizon returns an empty list after forecaster validation.
Forecasting does not mutate the model. Mean and drift forecasts use
floating-point arithmetic; naive and seasonal naive preserve the numeric
values they repeat.

Missing observations
--------------------

An unbound observation variable denotes a missing value. Ground marker
terms such as ``missing`` are not accepted. Missing values retain their
positions in the gap-free, 1-based time axis; the library does not
remove, interpolate, or replace them with older known values.

All-missing training series raise
``domain_error(insufficient_observations, Dataset)``. Otherwise,
learning can succeed even if a model's forecast inputs are missing:

- Naive needs the final observation for any positive horizon.
- Seasonal naive needs only the cycle slots actually requested. A final
  cycle ``[10, 20, _, 5]`` allows a horizon of two but not three.
- Mean excludes missing observations from both its sum and divisor.
- Drift needs the original first and current final observations. Its
  divisor uses the elapsed series length, not the known-observation
  count. For ``[10, _, 14]``, the next drift forecast is ``16.0``.

If an observation required to calculate a forecast is missing, the
``forecast/3`` predicate raises a
``domain_error(missing_observation, Forecaster)`` error instead of
returning unbound forecast values. A zero horizon succeeds even with
missing state.

Immutable online updates
------------------------

The ``update/3`` predicate appends one numeric or missing observation
and returns a new forecaster. Neither the original forecaster nor a
supplied missing observation variable is instantiated. Missing variables
in returned state are copied rather than shared with the original state
or input variable.

Every update advances the elapsed length and update count. Naive
replaces its final value; seasonal naive advances its cycle even for
missing observations; mean updates its sum and known count only for
numeric observations; drift preserves its original first endpoint and
replaces its final endpoint. Thus updates produce the same state as
fresh learning on the appended series, up to floating-point summation
rounding and renaming of missing variables. Diagnostics additionally
record updates.

A numeric update can recover a missing final observation for naive or
drift. Seasonal slots become known as new numeric observations replace
them. A missing original first drift observation cannot be repaired by
appending observations. In that case, correct the dataset and learn a
new forecaster.

Representation and diagnostics
------------------------------

Models use a ``baseline_forecaster(Method, State, Diagnostics)`` term
representation. The ``State`` argument uses one of the following terms:

- ``naive_state(Last)``
- ``seasonal_naive_state(Frequency, Cycle)``, with the final cycle in
  chronological order
- ``mean_state(Sum, ObservedCount)``
- ``drift_state(First, Last)``

The ``Diagnostics`` argument is a list of diagnostic terms. It contains
``model(baseline_forecasting)``, ``method(Method)``,
``training_series_length(Length)``, ``options(EffectiveOptions)``,
``observed_count(Count)``, ``missing_count(MissingCount)``, and
``update_count(UpdateCount)`` terms. The ``Length`` argument includes
updates and missing positions: ``Count + MissingCount = Length``. The
original fitted length is ``Length - UpdateCount``. Initial models have
zero updates.

Use the ``check_forecaster/1`` predicate to validate the state, options,
and diagnostic consistency, including permitted missing variables. The
``valid_forecaster/1`` predicate succeeds only when validation completes
without an exception. To inspect metadata, use the inherited
``diagnostics/2``, ``diagnostic/2``, and ``forecaster_options/2``
predicates. The export predicates serialize the model as a single fact,
while the ``print_forecaster/1`` predicate prints its method, state, and
diagnostics.

Except for the seasonal cycle, state size is constant. Learning is
linear in series length, and forecasting is linear in horizon for fixed
frequency. Seasonal updates copy a cycle; other state updates are
constant-size.

Limitations
-----------

The library provides only naive, seasonal naive, mean, and drift
forecasts. It does not fit smoothing parameters or regressors, select a
method automatically, or estimate seasonal frequency. Choose the
``model/1`` option explicitly when the default naive method is not
appropriate. Seasonal naive requires a supplied frequency and at least
one complete cycle of positions.

Missing observations are not imputed. Learning requires at least one
known observation, but that alone does not guarantee a positive-horizon
forecast: naive needs the final value, seasonal naive needs the
requested cycle slots, and drift needs both endpoints. Appending numeric
observations cannot repair a missing original first drift observation;
correct the dataset and relearn.

Prediction intervals, transformations, and training-error diagnostics
are not provided. Models retain only the state needed by their method,
not the observation history. Drift extrapolates the line through the
first and last values, so those endpoints determine its trend; mean
forecasts use all known values without discounting older observations.
Forecasts are not constrained to be non-negative or integer-valued.
