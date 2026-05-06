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


`cusum_anomaly_detector`
=======

CUSUM (Cumulative Sum Control Chart) anomaly detector for continuous
sequence-like datasets. This is a statistical anomaly-detection method
based on a two-sided CUSUM control chart. Declared continuous attributes
are interpreted as ordered monitoring steps.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_detection_protocols` library. It learns a detector from a
continuous dataset, computes anomaly scores for new instances, predicts
`normal` or `anomaly`, and exports learned detectors as clauses or files.

Datasets are represented as objects implementing the
`anomaly_dataset_protocol` protocol from the `anomaly_detection_protocols`
library. Declared continuous attributes are interpreted as ordered
monitoring steps in a sequence. See the `cusum_anomaly_detector/tests.lgt` file for
example datasets.


API documentation
-----------------

Open the [../../apis/library_index.html#cusum_anomaly_detector](../../apis/library_index.html#cusum_anomaly_detector)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(cusum_anomaly_detector(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(cusum_anomaly_detector(tester)).


Features
--------

- **Statistical method**: implements anomaly detection based on a
  two-sided CUSUM control chart, using learned per-step population means
  and standard deviations for continuous attributes.

- **Ordered sequence interpretation**: declared continuous attributes are
  treated as ordered monitoring steps. For each known step value `x_t`,
  the library computes `z_t = (x_t - mu_t) / sigma_t` and updates the
  positive and negative CUSUM recurrences along that attribute order. The
  learned detector stores a precomputed attribute schema so that this
  ordering does not need to be rebuilt for every scoring call.

- **CUSUM recurrences**: the positive and negative cumulative sums are
  updated as `C+_t = max(0, C+_(t-1) + z_t - k)` and
  `C-_t = max(0, C-_(t-1) - z_t - k)`, where `k` is the learn-time
  allowance. The raw anomaly score is the maximum excursion over all
  positive and negative cumulative sums.

- **Continuous features only**: accepts datasets whose declared
  attributes are all `continuous`.

- **Baseline training selection**: supports learn-time
  `baseline_class_values(ClassValues)` and
  `baseline_selection_policy(Policy)` options. The default baseline class
  values are `[normal]`. The default `reject` policy throws an error if
  any non-baseline training example is found. The `filter` policy removes
  non-baseline examples before fitting the baseline statistics.

- **Missing-value tolerant**: ignores missing values when fitting
  per-step statistics and skips them during scoring. Queries must still
  provide at least one known value.

- **Bounded scoring**: maps the raw CUSUM excursion to `[0.0, 1.0)` using
  `Score = Raw / (1 + Raw)`.

- **CUSUM control parameters**: supports learn-time `allowance/1` and
  `decision_interval/1` options. The default `allowance(0.5)` and
  `decision_interval(5.0)` correspond to a common standardized CUSUM
  setup.

- **Default threshold**: the default
  `anomaly_threshold(0.8333333333333334)` corresponds to the default raw
  decision interval `5.0`. If a custom `decision_interval/1` is passed to
  `learn/3` without an explicit `anomaly_threshold/1`, the stored anomaly
  threshold is derived automatically as `H / (1 + H)`.

- **Learn-time control parameters**: `allowance/1` and
  `decision_interval/1` are recorded in the learned detector and reused
  for subsequent scoring and prediction. Passing them to `predict/4` does
  not override the learned values. Only `anomaly_threshold/1` can be
  overridden at predict time.

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
  (default: `0.8333333333333334`)
- `allowance(Allowance)`: Learn-time CUSUM allowance `k`
  (default: `0.5`)
- `baseline_class_values(ClassValues)`: Learn-time class labels that are
  admissible for baseline fitting (default: `[normal]`)
- `baseline_selection_policy(Policy)`: Learn-time handling of examples
  whose class is not listed in `baseline_class_values/1`. Supported
  values are `reject` and `filter` (default: `reject`)
- `decision_interval(DecisionInterval)`: Learn-time raw decision interval
  `H` (default: `5.0`). If no explicit `anomaly_threshold/1` is passed to
  `learn/3`, the stored threshold is derived from this value as
  `H / (1 + H)`.


Detector representation
-----------------------

The learned detector is represented by default as:

	cusum_detector(TrainingDataset, AttributeSchema, Encoders, Diagnostics)

Where:

- `TrainingDataset`: training dataset object identifier
- `AttributeSchema`: precomputed attribute ordering metadata used to
  validate and reorder query step values efficiently during scoring
- `Encoders`: list of `cusum_encoder(Attribute, Mean, Scale)` records
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
Second, those deviations are processed sequentially using the positive and
negative CUSUM recurrences with the learned `allowance/1` value. Third,
the maximum raw excursion is mapped to the interval `[0.0, 1.0)` using
`Score = Raw / (1 + Raw)`.

The `allowance/1` option changes the CUSUM update rule itself by
controlling how much drift must accumulate before the chart grows.
Larger values make the detector less sensitive to small shifts. The
`decision_interval/1` option does not change scoring; it only affects the
default threshold stored when learning a detector.

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
