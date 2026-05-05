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


`iqr`
=======

Statistical interquartile-range anomaly detector for continuous
datasets. It is a statistical anomaly-detection method based on Tukey
interquartile fences: for each known continuous attribute value it
learns `Q1` and `Q3`, computes the exceedance beyond `[Q1,Q3]` in
interquartile-range units normalized by the learned
`fence_multiplier/1`, and then aggregates the per-attribute normalized
deviations according to `score_mode/1`, so any value at or beyond
`[Q1 - k*IQR, Q3 + k*IQR]` reaches the default anomaly boundary when
using `fence_multiplier(k)`.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_detection_protocols` library. It learns a detector from a
continuous dataset, computes anomaly scores for new instances, predicts
`normal` or `anomaly`, and exports learned detectors as clauses or files.

Datasets are represented as objects implementing the
`anomaly_dataset_protocol` protocol from the `anomaly_detection_protocols`
library. See the `anomaly_detection_protocols/test_datasets` directory
for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#iqr](../../apis/library_index.html#iqr)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(iqr(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(iqr(tester)).


Features
--------

- **Statistical method**: implements anomaly detection based on
  interquartile-range fences, using per-attribute first and third
  quartiles to measure how far new observations deviate from the central
  baseline distribution.

- **Quartile-based scoring**: for each known attribute value `x`, the
  library computes the positive exceedance of `x` beyond the interval
  `[Q1, Q3]` in interquartile-range units, where `Q1` and `Q3` are the
  learned sample quartiles.

- **Continuous features only**: accepts datasets whose declared
  attributes are all `continuous`.

- **Robust statistics**: reuses the `statistics` library `sample`
  object `quartiles/4` predicate to compute per-attribute quartiles.

- **Baseline training selection**: supports learn-time
  `baseline_class_values(ClassValues)` and
  `baseline_selection_policy(Policy)` options. The default baseline
  class values are `[normal]`. The default `reject` policy throws an
  error if non-baseline examples are present, while `filter` removes
  them before fitting.

- **Missing-value tolerant**: ignores missing values when fitting
  attribute statistics. During scoring, queries must provide at least
  one known value. In the default `score_mode(root_mean_square)`, the
  raw score is aggregated over attributes with positive normalized
  deviation so that neutral inlier attributes do not dilute fence
  anomalies. The learned detector stores a precomputed attribute schema
  so that scoring reuses the same attribute ordering without rebuilding
  it on every call.

- **Configurable scoring semantics**: supports both dense multivariate
  deviation scoring using `score_mode(root_mean_square)` and sparse
  anomaly detection using `score_mode(any_feature_extreme)`. The default
  root-mean-square mode reuses the `numberlist` library Euclidean norm
  predicate as part of the computation.

- **Configurable Tukey fences**: supports a learn-time
  `fence_multiplier/1` option. The default `1.5` corresponds to the
  classical Tukey inner fence cutoff, and the learned multiplier is
  applied directly in the score path.

- **Bounded scoring**: maps the raw multivariate IQR score to
  `[0.0, 1.0)` using `Score = Raw / (1 + Raw)`.

- **Default threshold**: the default `anomaly_threshold(0.5)`
  corresponds to the learned Tukey fence cutoff after score scaling,
  while remaining overrideable in `learn/3` and `predict/4`.

- **Learn-time options**: `fence_multiplier/1` and `score_mode/1` are
  recorded in the learned detector and reused for subsequent scoring and
  prediction. Passing either option to `predict/4` does not override the
  learned value.

- **All-missing queries rejected**: scoring and prediction throw a
  `domain_error(non_empty_known_values, AttributeNames)` exception when
  every declared feature is missing in the query.

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
  values are `filter` and `reject` (default: `reject`)
- `fence_multiplier(Multiplier)`: Learn-time Tukey fence multiplier
  stored in the learned detector (default: `1.5`)
- `score_mode(Mode)`: Learn-time score aggregation mode for `learn/3`.
  Supported values are `root_mean_square` and `any_feature_extreme`
  (default: `root_mean_square`). If passed to `predict/4`, it is ignored
  and the value stored in the learned detector is used.


Detector representation
-----------------------

The learned detector is represented by default as:

	iqr_detector(TrainingDataset, AttributeSchema, Encoders, Diagnostics)

Where:

- `TrainingDataset`: training dataset object identifier
- `AttributeSchema`: precomputed attribute ordering used for validation
  and scoring
- `Encoders`: list of `iqr(Attribute, Q1, Q3, Scale)` records
- `Diagnostics`: learned metadata terms including `model/1`,
  `training_dataset/1`, `attribute_names/1`, `feature_count/1`,
  `example_count/1`, and `options/1`

When exported using `export_to_clauses/4` or
`export_to_file/4`, this detector term is serialized directly as
the single argument of the generated predicate clause so that the exported
model can be loaded and reused as-is.


Notes
-----

Scoring has three stages. First, the detector computes one per-attribute
IQR deviation score for each known attribute value using its exceedance
beyond the interval `[Q1, Q3]` in interquartile-range units. Second,
each per-attribute score is normalized by the learned
`fence_multiplier/1`, so that it reaches `1.0` exactly at the chosen
Tukey fence cutoff. Third, those normalized per-attribute scores are
aggregated into a single raw deviation score according to the learned
`score_mode/1` option before being mapped to the interval `[0.0, 1.0)`
using `Score = Raw / (1 + Raw)`.

The `score_mode/1` option does not change the per-attribute quartile
formula. It only changes the aggregation step. With
`score_mode(root_mean_square)`, the raw score is the root mean square of
the positive normalized per-attribute deviations, computed over the
attributes with positive deviation so that inlier padding does not
dilute fence-reaching anomalies. With
`score_mode(any_feature_extreme)`, the raw score is the maximum
normalized per-attribute deviation.

The `fence_multiplier/1` option defines the classical Tukey anomaly
cutoff and directly normalizes the per-attribute deviation scores. With
any learned `fence_multiplier(K)`, a per-attribute score of `1.0` means
the query has reached the chosen Tukey fence on that attribute, so the
default normalized threshold `0.5` corresponds to that cutoff in both
supported aggregation modes.

The `baseline_class_values/1` option declares which dataset class labels
are admissible for fitting the baseline quartiles and interquartile
ranges. The `baseline_selection_policy/1` option then controls what
happens when other labels are present in the training data. The default
`reject` policy raises a `domain_error(baseline_only_training_data,
Dataset)` exception when any non-baseline example is found. The `filter`
policy removes non-baseline examples before fitting.

Attributes with zero observed interquartile range are assigned a
fallback scale of `1.0`. This keeps the detector well-defined for
singleton datasets or constant columns while still yielding zero score
for matching values and positive scores for deviating values.

The root-mean-square aggregation keeps the default threshold stable
while avoiding dilution from missing or neutral inlier attributes.

Use `score_mode(any_feature_extreme)` when a single extreme feature
should be sufficient to flag an anomaly in high-dimensional data.
