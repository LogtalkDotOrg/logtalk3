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


`knn_distance`
==============

k-nearest-neighbor distance anomaly detector supporting multiple distance
metrics, mixed continuous and categorical features, and missing values.

The library implements the `anomaly_detector_protocol` defined in the
`anomaly_protocols` library. It learns a compact detector from a dataset,
computes anomaly scores for new instances, predicts `normal` or `anomaly`,
and exports learned detectors as clauses or files.

Datasets are represented as objects implementing the
`anomaly_dataset_protocol` protocol from the `anomaly_protocols` library.
See the `anomaly_protocols/test_datasets` directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#knn_distance](../../apis/library_index.html#knn_distance)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(knn_distance(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(knn_distance(tester)).


Features
--------

- **Distance-based anomaly scoring**: supports both distance to the k-th
  neighbor and average distance to the k nearest neighbors.

- **Mixed features**: automatically handles continuous and categorical
  features declared by the dataset.

- **Missing values**: ignores missing dimensions while normalizing distances.

- **Multiple metrics**: supports Euclidean, Manhattan, Chebyshev, and
  Minkowski distance metrics.

- **Detector export**: learned detectors can be exported as predicate clauses.


Options
-------

The following options can be passed to the `learn/3` and `predict/4`
predicates:

- `k(K)`: Number of neighbors to consider (default: 5)
- `distance_metric(Metric)`: Distance metric to use. Options:
  `euclidean` (default), `manhattan`, `chebyshev`, `minkowski`
- `score_mode(Mode)`: Score computation mode. Options:
  `kth_distance` (default) and `mean_distance`
- `anomaly_threshold(Threshold)`: Threshold for `predict/3-4`
  (default: `0.5`)


Detector Representation
-----------------------

The learned detector is represented by default as:

	knn_distance_detector(AttributeNames, FeatureTypes, AttributeScales, Instances, Options)

Where:

- `AttributeNames`: List of attribute names in order
- `FeatureTypes`: List of feature types (`numeric` or `categorical`)
- `AttributeScales`: Normalization scales for numeric features
- `Instances`: List of training `Id-Class-Values` triples
- `Options`: Learned options

When exported using `export_to_clauses/4` or
`export_to_file/4`, this detector term is serialized directly as
the single argument of the generated predicate clause so that the exported
model can be loaded and reused as-is.


References
----------

1. Angiulli, F. and Pizzuti, C. (2002). "Fast outlier detection in high
   dimensional spaces". PKDD.
2. Chandola, V., Banerjee, A., and Kumar, V. (2009). "Anomaly detection: A
   survey". ACM Computing Surveys.
