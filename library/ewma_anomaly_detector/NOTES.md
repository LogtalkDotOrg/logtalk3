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


`ewma_anomaly_detector`
======

EWMA (Exponentially Weighted Moving Average) anomaly detector for
continuous sequence-like datasets. It is a statistical anomaly-detection
method based on a two-sided EWMA control chart: for each known step
value `x_t`, it computes a standardized deviation, updates the EWMA
statistic `E_t`, and uses the maximum normalized excursion
`|E_t| / (L*c_t)` as the raw anomaly score, so a score of `1.0`
corresponds exactly to reaching the chosen EWMA control limit.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_detection_protocols` library. It learns a detector from a
continuous dataset, computes anomaly scores for new instances, predicts
`normal` or `anomaly`, and exports learned detectors as clauses or files.

Datasets are represented as objects implementing the
`anomaly_dataset_protocol` protocol from the `anomaly_detection_protocols`
library. Declared continuous attributes are interpreted as ordered
monitoring steps in a sequence. See the `ewma_anomaly_detector/tests.lgt` file for example
datasets.


API documentation
-----------------

Open the [../../apis/library_index.html#ewma_anomaly_detector](../../apis/library_index.html#ewma_anomaly_detector)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(ewma_anomaly_detector(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(ewma_anomaly_detector(tester)).


Features
--------

- **Statistical method**: implements anomaly detection based on a
  two-sided EWMA control chart, using learned per-step population means
  and standard deviations for continuous attributes.

- **Ordered sequence interpretation**: declared continuous attributes are
  treated as ordered monitoring steps. For each known step value `x_t`,
  the library computes `z_t = (x_t - mu_t) / sigma_t` and updates the
  EWMA recurrence `E_t = lambda*z_t + (1 - lambda)*E_(t-1)` with
  `E_0 = 0`.

- **EWMA control limits**: the raw anomaly score is the maximum
  normalized excursion `|E_t| / (L*c_t)`, where `L` is the learn-time
  `control_limit_multiplier/1` option and
  `c_t = sqrt(lambda/(2-lambda) * (1 - (1-lambda)^(2*t)))` is the
  classical EWMA control-limit factor after `t` EWMA updates. The
  learned detector stores a precomputed attribute schema so that query
  values can be reordered efficiently during scoring.

- **Continuous features only**: accepts datasets whose declared
  attributes are all `continuous`.

- **Baseline training selection**: supports learn-time
  `baseline_class_values(ClassValues)` and
  `baseline_selection_policy(Policy)` options. The default baseline class
  values are `[normal]`. The default `reject` policy throws an error if
  any non-baseline training example is found. The `filter` policy removes
  non-baseline examples before fitting the baseline statistics.

- **Missing-value tolerant**: ignores missing values when fitting
  per-step statistics. During scoring, missing step values do not update
  the EWMA state or advance the EWMA update count. Queries must still
  provide at least one known value.

- **Bounded scoring**: maps the raw EWMA excursion to `[0.0, 1.0)` using
  `Score = Raw / (1 + Raw)`.

- **EWMA control parameters**: supports learn-time
  `control_limit_multiplier/1` and `smoothing_factor/1` options. The
  default `control_limit_multiplier(3.0)` and `smoothing_factor(0.2)`
  correspond to a common EWMA monitoring setup.

- **Default threshold**: the default `anomaly_threshold(0.5)`
  corresponds to the chosen EWMA control limit after score
  normalization. Because the raw score is already normalized by the
  learned control limit, this threshold remains the same for any learned
  `control_limit_multiplier/1` value.

- **Learn-time control parameters**: `control_limit_multiplier/1` and
  `smoothing_factor/1` are recorded in the learned detector and reused
  for subsequent scoring and prediction. Passing them to `predict/4`
  does not override the learned values. Only `anomaly_threshold/1` can
  be overridden at predict time.

- **All-missing queries rejected**: scoring and prediction throw a
  `domain_error(non_empty_known_values, AttributeNames)` exception when
  every declared step is missing in the query.

- **Featureless datasets rejected**: datasets must declare at least one
  continuous feature; otherwise `learn/2-3` throws a
  `domain_error(non_empty_features, Dataset)` exception.

- **Detector export**: learned detectors can be exported as predicate clauses.

- **Explicit validation and diagnostics**: supports the shared
  `check_anomaly_detector/1`, `valid_anomaly_detector/1`, `diagnostics/2`,
  `diagnostic/2`, and `anomaly_detector_options/2` predicates.


Options
-------

The following options are supported by the public API:

- `anomaly_threshold(Threshold)`: Threshold for `predict/3-4`
  (default: `0.5`)
- `baseline_class_values(ClassValues)`: Learn-time class labels that are
  admissible for baseline fitting (default: `[normal]`)
- `baseline_selection_policy(Policy)`: Learn-time handling of examples
  whose class is not listed in `baseline_class_values/1`. Supported
  values are `reject` and `filter` (default: `reject`)
- `control_limit_multiplier(ControlLimitMultiplier)`: Learn-time EWMA
  control-limit multiplier `L` (default: `3.0`)
- `smoothing_factor(SmoothingFactor)`: Learn-time EWMA smoothing factor
  `lambda` (default: `0.2`)


Detector representation
-----------------------

The learned detector is represented by default as:

	ewma_detector(TrainingDataset, AttributeSchema, Encoders, Diagnostics)

Where:

- `TrainingDataset`: training dataset object identifier
- `AttributeSchema`: precomputed attribute ordering metadata used to
  validate and reorder query step values efficiently during scoring
- `Encoders`: list of `ewma_encoder(Attribute, Mean, Scale)` records
- `Diagnostics`: learned metadata terms including `model/1`,
  `training_dataset/1`, `attribute_names/1`, `feature_count/1`,
  `example_count/1`, and `options/1`. The `example_count/1` value is the
  effective number of training examples after applying the selected
  baseline selection policy.

When exported using `export_to_clauses/4` or `export_to_file/4`, this
detector term is serialized directly as the single argument of the
generated predicate clause so that the exported model can be loaded and
reused as-is.


Notes
-----

Scoring has three stages. First, the detector computes one standardized
deviation `z_t = (x_t - mu_t) / sigma_t` for each known monitoring step.
Second, those deviations are processed sequentially using the EWMA
recurrence `E_t = lambda*z_t + (1 - lambda)*E_(t-1)` with the learned
`smoothing_factor/1` value. Third, the maximum normalized excursion
`|E_t| / (L*c_t)` is mapped to the interval `[0.0, 1.0)` using
`Score = Raw / (1 + Raw)`.

The control-limit factor `c_t` is computed from the number of actual
EWMA updates, not from the declared attribute position. Accordingly,
leading or intermediate missing step values neither update the EWMA
state nor widen the control limits.

The `smoothing_factor/1` option changes the EWMA update rule itself.
Smaller values make the detector retain longer-term history while larger
values react more strongly to recent deviations. The
`control_limit_multiplier/1` option scales the control limits directly
in the score path. Larger values therefore make the detector less
sensitive by requiring larger EWMA excursions before the raw score
reaches `1.0`.

The `baseline_class_values/1` option declares which dataset class labels
are admissible for baseline fitting. The `baseline_selection_policy/1`
option then controls what happens when other labels are present in the
training data. The default `reject` policy raises a
`domain_error(baseline_only_training_data, Dataset)` exception when any
non-baseline example is found. The `filter` policy removes non-baseline
examples before fitting and raises a
`domain_error(non_empty_baseline_training_data, Dataset)` exception if no
training examples remain after filtering.

Attributes with zero observed dispersion are assigned a fallback scale of
`1.0`. This keeps the detector well-defined for singleton datasets or
constant steps while still yielding zero score for matching values and
positive scores for deviating values.
