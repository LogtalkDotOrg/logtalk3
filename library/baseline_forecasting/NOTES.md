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


`baseline_forecasting`
======================

This library implements univariate naive, seasonal naive, mean, and drift
forecasting. These methods provide reference forecasts for evaluating more
complex models and are also useful directly. The object implements
`forecaster_protocol` and imports `forecaster_common` from the
`time_series_protocols` library for dataset collection, validation,
diagnostics, forecast construction, and export support.

Datasets are objects implementing `time_series_dataset_protocol`.
Observations can be numbers or unbound variables representing missing
values. No backend-specific facilities are required.


API documentation
-----------------

Open the [../../apis/library_index.html#baseline-forecasting](../../apis/library_index.html#baseline-forecasting)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(baseline_forecasting(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(baseline_forecasting(tester)).

The test suite checks forecasts against explicit numeric expectations,
seasonal phase alignment, missing observations, malformed datasets and
forecasters, immutable updates, update equivalence to fresh learning,
diagnostics, and export round-trips.


Example
-------

	| ?- baseline_forecasting::learn(my_series, Forecaster, [model(drift)]),
	     baseline_forecasting::forecast(Forecaster, 12, Forecasts).


Models and options
------------------

`learn/3` accepts `model(Method)` with default `model(naive)`:

- `naive`: repeats the final observation. Requires at least one position.
- `seasonal_naive`: repeats the last full cycle of observations. Requires
  at least `Frequency` positions. The first forecast corresponds to the
  oldest observation in that final cycle, including when the series length
  is not a multiple of the frequency.
- `mean`: repeats the arithmetic mean of all known observations. Requires
  at least one position and one known observation.
- `drift`: extends the line through the first and final observations.
  Requires at least two positions. For horizon step `h`, the forecast is
  `Last + h * (Last - First) / (Length - 1)`.

For `seasonal_naive`, an optional `frequency(Frequency)` takes precedence
over the dataset's `frequency/1` metadata. The frequency must be a positive
integer; `1` is accepted and gives persistence forecasts. Without an
explicit option, the dataset must supply a valid frequency. Otherwise,
learning raises `domain_error(seasonal_frequency, Dataset)`.

A `frequency/1` option supplied for any non-seasonal model raises
`domain_error(baseline_forecasting_option, Option)`. Effective options in
diagnostics are canonical: `[model(Method)]`, or
`[model(seasonal_naive), frequency(Frequency)]` with the resolved frequency.
The public `valid_option/1` and `default_option/1` hooks can be queried
through the inherited options interface.

`forecast/3` accepts a non-negative integer horizon. A zero horizon returns
an empty list after forecaster validation. Forecasting does not mutate
the model. Mean and drift forecasts use floating-point arithmetic; naive
and seasonal naive preserve the numeric values they repeat.


Missing observations
--------------------

An unbound observation variable denotes a missing value. Ground marker
terms such as `missing` are not accepted. Missing values retain their
positions in the gap-free, 1-based time axis; the library does not remove,
interpolate, or replace them with older known values.

All-missing training series raise
`domain_error(insufficient_observations, Dataset)`. Otherwise, learning
can succeed even if a model's forecast inputs are missing:

- Naive needs the final observation for any positive horizon.
- Seasonal naive needs only the cycle slots actually requested. A final
  cycle `[10, 20, _, 5]` allows a horizon of two but not three.
- Mean excludes missing observations from both its sum and divisor.
- Drift needs the original first and current final observations. Its
  divisor uses the elapsed series length, not the known-observation count.
  For `[10, _, 14]`, the next drift forecast is `16.0`.

If a requested forecast needs a missing observation, `forecast/3` raises
`domain_error(missing_observation, Forecaster)` instead of returning
unbound forecast values. A zero horizon succeeds even with missing state.


Immutable online updates
------------------------

`update/3` appends one numeric or missing observation and returns a new
forecaster. Neither the original forecaster nor the supplied missing
observation variable is instantiated. Missing variables in returned state
are copied rather than shared with the original state or input variable.

Every update advances the elapsed length and update count. Naive replaces
its final value; seasonal naive advances its cycle even for missing
observations; mean updates its sum and known count only for numeric
observations; drift preserves its original first endpoint and replaces
its final endpoint. Thus updates produce the same state as fresh learning
on the appended series, up to floating-point summation rounding and
renaming of missing variables. Diagnostics additionally record updates.

A numeric update can recover a missing final observation for naive or
drift. Seasonal slots become known as new numeric observations replace
them. A missing original first drift observation cannot be repaired by
appending observations; relearn after correcting the dataset.


Representation and diagnostics
------------------------------

Models use `baseline_forecaster(Method, State, Diagnostics)`. State is:

- `naive_state(Last)`
- `seasonal_naive_state(Frequency, Cycle)`, with the final cycle in
  chronological order
- `mean_state(Sum, ObservedCount)`
- `drift_state(First, Last)`

Diagnostics contain `model(baseline_forecasting)`, `method(Method)`,
`training_series_length(Length)`, `options(EffectiveOptions)`,
`observed_count(Count)`, `missing_count(MissingCount)`, and
`update_count(UpdateCount)`. Length includes updates and missing positions;
`Count + MissingCount = Length`. The original fitted length is
`Length - UpdateCount`. Initial models have zero updates.

`check_forecaster/1` validates state, options, and diagnostic consistency,
including permitted missing variables. `valid_forecaster/1` succeeds only
when validation succeeds without an exception. The inherited
`diagnostics/2`, `diagnostic/2`, and `forecaster_options/2` expose metadata.
Export predicates serialize the model as a single fact; `print_forecaster/1`
prints its method, state, and diagnostics.

Except for the seasonal cycle, state size is constant. Learning is linear
in series length, and forecasting is linear in horizon for fixed frequency.
Seasonal updates copy a cycle; other state updates are constant-size.
Prediction intervals, automatic model selection, transformations, and
training-error diagnostics are not provided.
