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


`lof_anomaly_detector`
======================

Local Outlier Factor anomaly detector supporting multiple distance metrics,
mixed continuous and categorical features, and missing values. The detector
memorizes the training instances and computes Local Outlier Factor values
by comparing the local reachability density of a query to the densities of
its neighbors.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_detection_protocols` library. It learns a detector from a
dataset by selecting baseline training examples from the declared class
labels, computes normalized anomaly scores for new instances, predicts
`normal` or `anomaly`, and exports learned detectors as clauses or files.

Datasets are represented as objects implementing the
`anomaly_dataset_protocol` protocol from the `anomaly_detection_protocols`
library. See the `anomaly_detection_protocols/test_datasets` directory
for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#lof-anomaly-detector](../../apis/library_index.html#lof-anomaly-detector)
link in a web browser.


Loading
-------

To load this library, load its `loader.lgt` file:

	| ?- logtalk_load(lof_anomaly_detector(loader)).


Testing
-------

To test this library, load its `tester.lgt` file:

	| ?- logtalk_load(lof_anomaly_detector(tester)).


Features
--------

- **Density-based anomaly scoring**: computes Local Outlier Factor scores
  from local reachability densities on a fixed training population. Each
  training point is excluded only from its own neighbor candidates, not
  from the reference population used for other points. Every neighbor tied
  at the kth distance is included. Density and score averages use the
  actual number of comparable neighbors, which can differ from `k`.

- **Normalized scores**: Raw LOF values are normalized to the interval
  `[0.0, 1.0]` by mapping the ideal baseline value `1.0` to `0.0` and
  scaling larger values against the largest training raw score.

- **Mixed features**: automatically handles continuous and categorical
  features declared by the dataset.

- **Missing values**: ignores missing dimensions while normalizing distances
  (distances are normalized by the number of comparable dimensions). A
  neighborhood can contain fewer than `k` comparable instances. A query
  with no comparable neighbors has raw score `1.0`. Every declared attribute
  must occur in the input pairs. An unbound attribute value represents
  missing data; `'$missing'` is reserved internally.

- **Baseline training selection**: `baseline_class_values/1` declares which
  class labels are admissible for fitting the detector, while
  `baseline_selection_policy/1` controls whether non-baseline examples are
  rejected (default) or filtered before training.

- **Multiple metrics**: supports Euclidean, Manhattan, Chebyshev, and
  Minkowski distance metrics. Minkowski uses power `3`. Numeric differences
  are divided by the training range; constant or wholly missing features
  use scale `1.0`. Euclidean, Manhattan, and Minkowski sums are divided by
  the number of comparable dimensions before applying any root.

- **Detector export**: learned detectors can be exported as predicate clauses.

- **Dataset validation**: learning rejects empty datasets with a
  `domain_error(non_empty_dataset, Dataset)` exception.


Validation
----------

Continuous feature values must be numbers, and categorical feature values
must be atoms. An unbound feature value is accepted as missing. Validation
occurs during learning and fresh-query preparation.


Options
-------

The following options can be passed to the `learn/3` and `predict/4`
predicates:

- `k(K)`: Positive target neighbor count (default: 5). Training clamps it
  to the number of other training points. Fresh queries clamp it to the
  full training count. Missing dimensions can reduce the comparable count,
  while boundary ties can increase the retained neighborhood size.
- `distance_metric(Metric)`: Distance metric to use. Options:
  `euclidean` (default), `manhattan`, `chebyshev`, `minkowski`
- `anomaly_threshold(Threshold)`: Threshold for `predict/3-4`
  (default: `0.4`)
- `baseline_class_values(Classes)`: Learn-time list of admissible baseline
  class labels (default: `[normal]`)
- `baseline_selection_policy(Policy)`: Learn-time handling of non-baseline
  examples. Supported values are `reject` (default) and `filter`


Detector representation
-----------------------

The `learn/2-3` predicates return the following detector representation:

	lof_detector(TrainingDataset, AttributeNames, FeatureTypes, AttributeScales, Instances, ReferenceScores, Cache, Diagnostics)

Where:

- `AttributeNames`: List of attribute names in order
- `FeatureTypes`: List of feature types (`numeric` or `categorical`)
- `AttributeScales`: Normalization scales for numeric features
- `Instances`: List of retained baseline training `Id-Class-Values` triples
- `ReferenceScores`: Cached self-excluded raw training scores for the
  retained baseline training instances
- `Cache`: Ground immutable `lof_cache(K, Metric, Graph, ReferenceIndex, Maximum)`
  term containing effective training `k`, resolved metric, balanced dictionaries
  of neighborhoods/densities and reference scores, and the normalization maximum
- `Diagnostics`: Learned metadata terms including `model/1`,
  `training_dataset/1`, `attribute_names/1`, `feature_types/1`,
  `example_count/1`, `reference_score_count/1`, and `options/1`

The `score/3` predicate always treats its input as a fresh query. Only
`score_all/3` on the original training dataset with the `reject` baseline
selection policy reuses the cached self-excluded `ReferenceScores` for all
examples. With the `filter` policy, retained baseline training examples
reuse the cached self-excluded scores while excluded examples are scored as
fresh queries against the learned baseline detector.

When exported using `export_to_clauses/4` or
`export_to_file/4`, this detector term is serialized directly as
the single argument of the generated predicate clause so that the exported
model can be loaded and reused as-is.

Training example identifiers must be distinct integers. The cache is a snapshot
of the retained training population. Fresh-query scoring searches for neighbors
once and reads their k-distances and densities from the cache. It does not
rebuild training neighborhoods. Tie-heavy datasets can require quadratic graph
storage.

Graph neighborhoods contain compact `Distance-Id` pairs. Neighbor selection
uses a bounded maximum heap for small `k`, followed by an exact scan to retain
every boundary tie. Small populations and larger neighborhoods use compact
full sorting. The heap requires space proportional to `k`; retained boundary
ties can exceed `k`.

For training populations of at most 256 examples, each unordered pairwise
distance is calculated once and reused in both directions. The temporary
distance rows require quadratic storage and are discarded after graph
construction. Larger populations build rows independently to avoid that
additional temporary quadratic table.

Candidate selection uses actual metric distances, not squared or cubed keys.
Different powered values can round to the same rooted floating-point distance;
ranking only their powered values would incorrectly discard boundary ties.
Spatial indexing is not used. Pair-dependent missing-dimension normalization
does not generally satisfy the metric assumptions needed for safe pruning,
and no generic exact index dependency is required by this library.

An isolated training point has density `1.0`. A nonempty neighborhood with
zero summed reachability uses the finite density `1.0e12`. These conventions
keep duplicate-point and singleton scores finite. A query without comparable
neighbors has raw score `1.0` and normalized score `0.0`; that fallback does
not indicate evidence that the query is normal.

The graph caches training densities once, avoiding nested neighbor searches
for every query. Balanced dictionary lookups cost logarithmic time. For `n`
training points, `d` features, and `t` retained neighbors, full-sort query
selection costs approximately `O(n*d + n*log(n))`; the bounded-heap path costs
`O(n*d + n*log(k) + t*log(t))` with two distance scans. Density lookup adds
`O(t*log(n))`. Large tie groups can eliminate the small-neighborhood benefit.


Limitations
-----------

- Learning requires pairwise distance calculations, and fresh queries scan
  the retained training population. No approximate neighbor search or spatial
  index is provided.
- Boundary ties can produce neighborhoods much larger than `k` and require
  quadratic graph storage. Small `k` does not guarantee small neighborhoods.
- Distances with missing values depend on the dimensions comparable for each
  pair and need not satisfy the triangle inequality. A query with no comparable
  neighbors receives score `0.0`, which is a fallback rather than evidence of
  normality.
- Isolated points and zero-reachability neighborhoods use finite density
  conventions rather than infinite densities. Scores for degenerate datasets
  should be interpreted with these conventions in mind.
- Normalized scores are relative to the learned baseline, not calibrated
  anomaly probabilities. Clamping can give different raw LOF values the same
  normalized score.
- Detectors are immutable training snapshots. Changes to the training data
  require learning another detector; incremental updates are not supported.


References
----------

1. Breunig, M. M., Kriegel, H.-P., Ng, R. T., and Sander, J. (2000).
   "LOF: Identifying density-based local outliers". SIGMOD.
