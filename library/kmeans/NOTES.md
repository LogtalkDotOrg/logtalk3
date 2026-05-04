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


`kmeans`
========

k-Means clusterer. It uses Lloyd's algorithm with deterministic
initialization. Supports continuous attributes only.

The library implements the `clusterer_protocol` defined in the
`clustering_protocols` library. It provides predicates for learning a
clusterer from a dataset, assigning new instances to clusters, and
exporting the learned clusterer as a list of predicate clauses or to a
file.

Datasets are represented as objects implementing the
`clustering_dataset_protocol` protocol from the `clustering_protocols`
library.


API documentation
-----------------

Open the [../../apis/library_index.html#kmeans](../../apis/library_index.html#kmeans)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kmeans(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kmeans(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(kmeans(tester_performance)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Deterministic Initialization**: Supports `first_k` and deterministic `spread` initialization that repeatedly chooses the farthest example from the centroids selected so far.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Rich Training Diagnostics**: Learned clusterers report training example count, convergence status, iteration count, and final centroid shift.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous centroids instead of failing.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to learn. Default is `2`.
- `maximum_iterations(Iterations)`: Maximum number of Lloyd iterations. Default is `100`.
- `tolerance(Tolerance)`: Maximum centroid shift threshold for convergence. Default is `1.0e-6`.
- `initialization(Initialization)`: Centroid initialization strategy. Options: `spread` (default) or `first_k`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Diagnostics
-----------

The `diagnostics/2` predicate returns a list containing:

- `model(kmeans)`
- `centroid_count(Count)`
- `training_example_count(Count)`
- `convergence(Reason)`
- `iterations(Count)`
- `final_shift(Shift)`
- `options(Options)`


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	kmeans_clusterer(Encoders, Centroids, Options, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Centroids`: List of centroid vectors in cluster-id order.
- `Options`: Effective training options used to learn the clusterer.
- `Diagnostics`: Training diagnostics metadata returned by the `diagnostics/2` predicate.


References
----------

1. MacQueen (1967) - "Some Methods for Classification and Analysis of Multivariate Observations". Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability.
2. Lloyd (1982) - "Least Squares Quantization in PCM". IEEE Transactions on Information Theory, 28(2), 129-137.
