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


`intermittent_demand_forecasting`
=================================

This library implements Croston, the Syntetos-Boylan approximation (SBA),
and Teunter-Syntetos-Babai (TSB) forecasting for non-negative demand series
with periods of zero demand. The object implements `forecaster_protocol`
and imports `forecaster_common` from `time_series_protocols` for dataset
collection, options, diagnostics, forecast construction, and export support.

Datasets implement `time_series_dataset_protocol`. Values can be
non-negative integers or floats, or unbound variables representing missing
observations. Indices must be a complete, gap-free, 1-based sequence, and
the declared length must match the enumerated observations. Dataset
frequency metadata is not used. The implementation requires no
backend-specific facilities.


API documentation
-----------------

Open the [../../apis/library_index.html#intermittent-demand-forecasting](../../apis/library_index.html#intermittent-demand-forecasting)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(intermittent_demand_forecasting(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(intermittent_demand_forecasting(tester)).

Tests cover independent numerical expectations, initialization, missing
observations, malformed datasets and models, coefficient boundaries,
long series, immutable updates, equivalence to fresh learning, diagnostics,
automatic method selection and ties, and export round-trips.

Coefficient-fitting tests cover independent grid optima, deterministic
ties, agreement with exhaustive explicit fits, fixed off-grid values,
stored-model validation, and updates using the selected coefficients.
Custom-grid tests also cover normalization, duplicate removal, mixed
integer/float entries, invalid and non-ground grids, both missing policies,
more than 75 candidates, and fitted replay and export continuation.

Fitted-value tests cover causal predictions, missing-position alignment,
independent placeholders, agreement with error diagnostics, and replay of
the automatically selected method.


Example
-------

	| ?- intermittent_demand_forecasting::learn(
	        my_demand_series, Forecaster,
	        [model(tsb), alpha(0.1), beta(0.2), missing(elapsed)]
	     ),
	     intermittent_demand_forecasting::forecast(Forecaster, 12, Forecasts).

Append a new observation without changing the original model:

	| ?- intermittent_demand_forecasting::update(Forecaster, 0, Updated),
	     intermittent_demand_forecasting::forecast(Updated, 12, Forecasts).


Options
-------

The `learn/3` predicate accepts the following options:

- `model(croston|sba|tsb|auto)`: forecasting method; default `sba`.
  `auto` selects among the three concrete methods using causal training RMSE.
- `alpha(Alpha)`: positive-demand size smoothing coefficient or `auto`;
  default `0.1`.
- `beta(Beta)`: interarrival-time smoothing coefficient for Croston/SBA,
  or occurrence-probability smoothing coefficient for TSB, or `auto`;
  default `0.1`.
- `missing(skip|elapsed)`: missing-value clock policy; default `skip`.
- `coefficient_grid(Grid)`: shared candidate grid for coefficients requested
  as `auto`; default `[0.1,0.2,0.5,0.8,1.0]`.

Numeric coefficients must be greater than zero and at most one. Each
coefficient can independently request `auto` for bounded grid fitting.
Effective options are stored as
`[model(Method), alpha(Alpha), beta(Beta), missing(Policy)]`, with the
selected concrete method and numeric coefficients, never `auto` atoms.
The request-only grid is not stored in effective options or diagnostics.
The selected coefficients remain fixed during subsequent updates.
The public `valid_option/1` and `default_option/1` hooks can be queried
through the inherited options interface.


Automatic method selection
--------------------------

Use `model(auto)` to fit SBA, Croston, and TSB with the same fixed
coefficients and missing policy, selecting the lowest causal one-step
training RMSE. The default remains SBA; automatic selection is opt-in.
Exact RMSE ties prefer SBA, then Croston, then TSB, without a tolerance.
Each candidate scores the same numeric targets, including cold-start
zero predictions, and skips missing targets. `Beta` retains its
method-specific meaning. Numeric coefficient requests remain fixed;
`alpha(auto)` or `beta(auto)` enables the grid fitting described below.

	| ?- intermittent_demand_forecasting::learn(
	         my_demand_series, Forecaster,
	         [model(auto), alpha(0.1), beta(0.2), missing(elapsed)]
	     ),
	     intermittent_demand_forecasting::forecaster_options(Forecaster, Options).

The returned forecaster has the same representation and diagnostics as
explicitly learning the winning method, with concrete effective options.
No candidate history or additional selection metadata is retained.
Updates preserve the selected method; relearning with `model(auto)`
performs selection again and may choose a different method.

Selection uses the supplied training sequence, not held-out observations.
It does not guarantee better future forecasts or inventory performance.
Any candidate fitting or arithmetic exception is propagated rather than
silently excluding that method. The dataset is collected once, followed
by three linear fitting passes when both coefficients are numeric;
coefficient fitting expands the candidate set. Only the winning model
is retained.


Automatic coefficient fitting
-----------------------------

Use `alpha(auto)` and/or `beta(auto)` to select coefficients from the
default grid `[0.1,0.2,0.5,0.8,1.0]`. Only requested `auto` coefficients
are searched. Supplied numeric values are preserved exactly, including
off-grid values such as `0.37` and integer coefficients such as `1`.
Defaults remain numeric `0.1`; fitting is opt-in.

	| ?- intermittent_demand_forecasting::learn(
	         my_demand_series, Forecaster,
	         [model(auto), alpha(auto), beta(auto)]
	     ),
	     intermittent_demand_forecasting::forecaster_options(Forecaster, Options).

Supply `coefficient_grid(Grid)` to customize the candidates shared by
`alpha(auto)` and `beta(auto)`:

	| ?- intermittent_demand_forecasting::learn(
	         my_demand_series, Forecaster,
	         [model(auto), alpha(auto), beta(auto),
	         coefficient_grid([0.01,0.05,0.1,0.2,0.5])]
	     ).

The grid must be a nonempty proper list of numbers in `(0,1]`. Variable,
open, improper, empty, nonnumeric, and out-of-range grids are rejected
with `domain_error(option, coefficient_grid(Grid))`, even when neither
coefficient requests `auto`. A valid unused grid has no numerical effect.
Validation never completes lists or binds entries.

For automatic coefficients, a separate internal grid is normalized once
to floats, sorted ascending, and deduplicated without modifying the supplied
list. Thus `[0.8,1,0.01,1.0,0.8]` becomes `[0.01,0.8,1.0]`. Duplicate
removal is exact, with no tolerance. A grid entry `1` yields candidate
`1.0`, but an explicit `alpha(1)` or `beta(1)` remains integer `1`.
Grid ordering never changes tie priority. The grid is request-only;
retained models store only the selected numeric coefficients.

Every concrete method/coefficient combination is fitted using the same
missing policy and scored by causal one-step training RMSE, including
initial zero predictions and skipping missing targets. Select the lowest
RMSE. Exact ties prefer SBA, then Croston, then TSB, then lower alpha and
lower beta; no comparison tolerance is used. The model stores only the
winning concrete method and numeric coefficients. Stored `auto` atoms
are invalid forecaster metadata.

For Croston on `[6,10,10]`, `alpha(auto)` with fixed `beta(0.37)` selects
`alpha(1.0)`. For TSB on `[6,0,0]`, `beta(auto)` with fixed `alpha(0.37)`
selects `beta(1.0)`. These examples use the default grid. For TSB on
`[6,0,6]`, fixed `alpha(0.37)` and `beta(auto)` with
`coefficient_grid([0.2,0.01,0.1])` selects `beta(0.01)`, with squared-error
sum `72.0036`. All-zero and single-observation series can leave the
coefficients unidentified by RMSE; the deterministic tie rule applies.

With the default grid, one automatic coefficient gives five fits per
method; two give 25. With `model(auto)`, these become 15 or 75 fits
respectively. For a custom grid with `K` distinct normalized values, one
automatic coefficient gives `K` fits per method and two give `K * K`;
`model(auto)` multiplies these counts by three. Custom grids have no
additional size cap and can exceed 75 candidates. Normalization sorts
the supplied entries; fitting time and temporary candidate storage grow
with the candidate count. The dataset is collected once, and each fit
is linear in its length. Losing models are discarded after selection;
retained models and online updates remain constant-size. Any candidate
exception propagates rather than excluding that configuration.

This is a finite grid search, not continuous optimization over `(0,1]`.
The default grid's smallest value is `0.1`; custom grids and explicit
numeric requests can use smaller values. Minimizing training RMSE does not
guarantee holdout accuracy or better inventory performance. Updates never
refit coefficients; relearning with automatic requests may select again.


Algorithms and initialization
-----------------------------

Let `Size` denote the estimated positive-demand size. After initialization,
each positive observation `Demand` updates it:

	NewSize = Alpha * Demand + (1 - Alpha) * Size

Zero and missing observations do not update `Size`.

**Croston** estimates the interarrival time between known positive demands.
At a positive observation, `Gap = Age + 1`, where `Age` counts algorithm
periods since the previous positive demand, and:

	NewInterval = Beta * Gap + (1 - Beta) * Interval

The age resets to zero after the positive observation. Observed zero
demand advances the age but leaves both estimates unchanged. The forecast
is `Size / Interval`.

**SBA** uses exactly the same state and transitions as Croston, with the
approximately bias-corrected forecast:

	Forecast = (1 - Beta / 2) * (Size / Interval)

The correction uses the interval coefficient `Beta`, not the size
coefficient `Alpha`. Croston forecasts are known to be biased; SBA is an
approximate correction, not a guarantee of unbiased forecasts for every
series.

**TSB** replaces the interval estimate with occurrence probability. Every
numeric observation updates it, with `Indicator = 1` for positive demand
and `Indicator = 0` for zero demand:

	NewProbability = Beta * Indicator + (1 - Beta) * Probability

The forecast is `Size * Probability`. Thus each observed zero demand
reduces the probability by a factor of `1 - Beta`, allowing forecasts to
adapt to declining occurrence or obsolescence. Missing values never cause
probability decay.

Initialization is causal. Before any known positive demand, retain a
pending clock and forecast zero. At the first positive observation in
algorithm period `K`, initialize `Size` to that demand, `Interval` to `K`
for Croston/SBA, and `Probability` to `1 / K` for TSB. Do not additionally
smooth this initializing observation. No full-series averages or future
observations are used. All-zero and single-positive series are valid.

With `[0, 0, 6, 0, 10]` and both coefficients `0.5`, the final forecasts
are `3.2` for Croston, `2.4` for SBA, and approximately `4.6666666667` for
TSB. These are expected demand per period, not rounded order quantities.

The `forecast/3` predicate returns the same expected demand for every horizon
step. It does not assume that future observations are zero. In particular,
TSB decay occurs through actual zero-demand updates, not by extending the
forecast horizon. A zero horizon returns `[]` after validating the model
and the non-negative integer horizon.


Missing observations
--------------------

Only unbound variables denote missing observations. Marker atoms such as
`missing` are not accepted. Numeric zero is an observed absence of demand,
not a missing value. Negative values raise
`domain_error(non_negative_number, Observation)`. Empty datasets are
rejected; all-missing training series raise
`domain_error(insufficient_observations, Dataset)`.

Every missing position counts in the elapsed training length and missing
count, regardless of policy:

- `missing(skip)` pauses the algorithm clock. No state estimate changes.
  Initial period `K` and Croston/SBA gaps count numeric observations only.
  Estimates therefore use observed-period time; they should not be
  interpreted as rates from a fully observed calendar-time series.
- `missing(elapsed)` advances the pending initialization clock or the
  initialized Croston/SBA age, without smoothing any estimate. Initial
  period `K` includes missing positions. TSB also uses this `K` for its
  initial probability; after initialization, missing values leave its
  probability and size unchanged.

Neither policy imputes zero demand. The elapsed policy measures gaps
between known positive demands; it cannot reconstruct positive demand
that may have occurred while unobserved. These policies are explicit
extensions of the algorithms for incomplete data, not unique textbook
missing-data conventions.


Immutable online updates
------------------------

The `update(Forecaster, Observation, UpdatedForecaster)` predicate uses
exactly the same state transition as learning. It preserves the method,
coefficients, and missing policy, and returns a new ground model without
modifying its input model or instantiating missing observation variables.
No observation history or caller variables are retained. An all-zero
learned model can be initialized by its first positive update.

Every update advances the elapsed length and update count, including
missing updates. Numeric updates advance observed count, and positive
updates also advance positive count. Learning a prefix and appending
observations through `update/3` produces the same state, forecasts, and
error diagnostics as learning the full series with the same concrete
effective options. Only `update_count/1` differs. Relearning the full
series with `model(auto)` may select another method; updates do not rerun
selection.


Training-error diagnostics
--------------------------

Every numeric observation is scored against the one-step prediction from
the state before that observation is processed. This includes observed
zeros and the initial zero forecasts before the first positive demand.
For example, learning `[6]` yields MAE and RMSE of `6.0`, not zero. Missing
targets are not scored, do not bind caller variables, and do not cause a
prediction to be computed for scoring, regardless of the missing policy.
Existing state transitions are unchanged.

The model retains only a score count and two running sums, not the actual
or predicted series. Five diagnostic terms are appended to the existing
metadata:

- `scored_count(Count)`: number of scored numeric observations; equals
  `observed_count(Count)` and is positive.
- `sum_absolute_error(AbsoluteSum)`: sum of absolute one-step errors.
- `sum_squared_error(SquaredSum)`: sum of squared one-step errors.
- `mean_absolute_error(MAE)`: `float(AbsoluteSum / Count)`.
- `root_mean_squared_error(RMSE)`: `float(sqrt(SquaredSum / Count))`.

Query these through the existing diagnostics interface:

	| ?- intermittent_demand_forecasting::diagnostic(
	         Forecaster, mean_absolute_error(MAE)
	     ),
	     intermittent_demand_forecasting::diagnostic(
	         Forecaster, root_mean_squared_error(RMSE)
	     ).

Numeric updates score against the original model's prediction and extend
the same totals, so metrics include both training observations and later
updates. Missing updates preserve all five terms unchanged. These are
causal errors over the supplied sequence with fixed coefficients, not
holdout accuracy estimates or inventory-policy performance measures.

All five error terms are required in every forecaster. Missing, partial,
duplicated, or inconsistent metric groups are rejected; sums must be
non-negative numbers and MAE/RMSE must agree exactly with the stored totals.

Error arithmetic can overflow, particularly when squaring large errors
or accumulating totals. Evaluation errors are propagated, even when a
point forecast alone would be representable; totals are not clamped.


Fitted values
-------------

The `fitted_values(Dataset, Values)` predicate uses the default learning
options. The `fitted_values(Dataset, Values, Options)` predicate accepts
the same options as the `learn/3` predicate, including `model(auto)`,
`alpha(auto)`, `beta(auto)`, and `coefficient_grid(Grid)`. Both predicates
fit the supplied dataset and then replay from the initial state, returning
the prediction before processing each numeric observation. These are causal
one-step fitted predictions, not smoothed hindsight estimates or forecasts
from the final state.

	| ?- intermittent_demand_forecasting::fitted_values(
	         my_demand_series, Values,
	         [model(croston), alpha(0.5), beta(0.5)]
	     ).

The output has one position per dataset observation, including initial
zero predictions. For `[0,6,0,10]` with both coefficients `0.5`, the fitted
values are `[0,0,3.0,3.0]` for Croston, `[0,0,2.25,2.25]` for SBA, and
`[0,0,3.0,1.5]` for TSB. A single positive observation `[6]` has fitted
values `[0]`, not `[6]`.

Missing targets have fresh unbound output placeholders, independent of
the input variables and other placeholders. They are not predicted or
scored, but their existing `skip` or `elapsed` state transitions still
affect later predictions. Numeric fitted-value errors agree with the
model's aggregate training-error diagnostics.

With automatic requests, the method and coefficients are selected using
the full supplied dataset, and that one concrete configuration is
replayed throughout. There is no
per-prefix reselection. Although its state transitions are causal, the
configuration selection used the full sequence; fitted values are not holdout
predictions or evidence of out-of-sample accuracy.

No history or fitted values are retained inside forecasters. To replay
the observations underlying an existing learned or updated model, supply
the complete external sequence, including appended observations, and its
concrete effective options:

	| ?- intermittent_demand_forecasting::forecaster_options(Forecaster, Options),
	     intermittent_demand_forecasting::fitted_values(full_demand_series, Values, Options).

Using automatic requests instead may select another configuration. The API
validates the supplied dataset but cannot certify that it matches a model's
past observations. Empty, all-missing, negative, and malformed data are
rejected as in learning. Fitting and replay exceptions propagate, including
error-total overflow even when individual predictions are representable.

The dataset is collected once. Concrete method/coefficient requests
require fitting and replay passes. Automatic requests require one fitting
pass per candidate (at most 75 with the default grid) plus replay. Custom
grids can expand this count. The output list uses linear memory, while
retained models remain constant-size.


Representation and diagnostics
------------------------------

Models use a `intermittent_demand_forecaster(Method, State, Diagnostics)`
term representation where `State` is one of:

- `pending_state(Clock)` before any positive demand.
- `croston_state(Size, Interval, Age)` for initialized Croston or SBA.
- `tsb_state(Size, Probability)` for initialized TSB.

Diagnostics contain `model(intermittent_demand_forecasting)`,
`training_series_length(Length)`, `options(EffectiveOptions)`,
`method(Method)`, `observed_count(Count)`, `missing_count(Missing)`,
`positive_count(Positive)`, `effective_period_count(Effective)`, and
`update_count(Updates)`. Every model must also include the five
training-error diagnostics listed above.

`Count + Missing = Length`; `Positive` counts known positive observations.
Effective periods equal `Count` for `skip`, and `Length` for `elapsed`.
Length includes all updates and missing positions. The original fitted
length is `Length - Updates`; freshly learned models have zero updates.

The `check_forecaster/1` predicate checks ground state, effective options,
counter consistency, and complete error metadata, without completing
malformed models.

The `valid_forecaster/1` predicate succeeds only when validation succeeds
without an exception. The inherited `diagnostics/2`, `diagnostic/2`, and
`forecaster_options/2` predicates expose metadata. Exports serialize the
model as a single fact, including state required for future updates.
The `print_forecaster/1` predicate prints method, state, and metadata.

Retained state is constant-size and each update uses constant-size state
and metadata. Dataset collection validates and sorts indices; the
subsequent training pass is linear in series length and tail-recursive.
Automatic selection uses one such pass per candidate over the same
collected series, at most 75 with the default grid. Custom-grid counts
follow the formula above. Forecast construction is linear in horizon.


Limitations
-----------

- Only univariate, non-negative demand series are supported. Seasonal
  adjustment, trend components, explanatory variables, and transformations
  are not provided; dataset frequency metadata is ignored.
- Automatic coefficient fitting searches a finite configurable grid, not all
  values in `(0,1]`, and cannot guarantee a continuous optimum. Method and
  coefficient selection compare causal training RMSE, not held-out
  accuracy, and are not repeated during updates.
- Forecasts are point estimates, constant across the requested horizon.
  Prediction intervals and predictive distributions are not provided.
  Croston forecasts are biased, and SBA applies only an approximate bias
  correction. Croston and SBA do not reduce their forecasts during runs of
  observed zero demand; TSB does so through probability updates.
- Missing-value policies do not reconstruct unobserved demand. The skip
  policy estimates rates on observed-period time; the elapsed policy counts
  gaps between known positive demands. Neither guarantees the estimates
  that would result from a fully observed series. Empty and all-missing
  training datasets are rejected.
- Compact models do not retain observation history. Updates append one
  observation; correcting historical observations requires relearning.
  Fitted values require a supplied external dataset. Aggregate causal
  error diagnostics do not replace out-of-sample evaluation and can
  overflow for large errors.
- Forecasts represent expected demand per period, not integer order
  quantities. Inventory-control policies and order rounding are outside
  the library's scope.


References
----------

- Croston, J. D. (1972). Forecasting and stock control for intermittent
  demands. Operational Research Quarterly, 23(3), 289-303.
- Syntetos, A. A., and Boylan, J. E. (2005). The accuracy of intermittent
  demand estimates. International Journal of Forecasting, 21(2), 303-314.
- Teunter, R. H., Syntetos, A. A., and Babai, M. Z. (2011). Intermittent
  demand: Linking forecasting to inventory obsolescence. European Journal
  of Operational Research, 214(3), 606-615.
- [Forecasting: Principles and Practice, Time series of counts](https://otexts.com/fpp3/counts.html).
