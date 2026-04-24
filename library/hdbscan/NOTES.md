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


`hdbscan`
==========

Simplified HDBSCAN-style clusterer.

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

Open the [../../apis/library_index.html#hdbscan](../../apis/library_index.html#hdbscan)
link in a web browser.

Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(hdbscan(loader)).

Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(hdbscan(tester)).


Features
--------

- **Hierarchical Density Clustering**: Builds a mutual-reachability minimum spanning tree and extracts clusters by cutting strong gaps.
- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Leaf Cluster Extraction**: Uses a simplified `leaf` cluster selection method.
- **Distance Metrics**: Supports `euclidean` and `manhattan` distances.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Noise Detection**: Points not assigned to any extracted cluster are retained as noise.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `minimum_points(MinimumPoints)`: Minimum neighborhood size used when computing core distances and mutual reachability. Default is `2`.
- `minimum_cluster_size(MinimumClusterSize)`: Minimum number of points required for an extracted cluster. Default is `2`.
- `cluster_selection_method(Method)`: Cluster extraction policy. The current implementation accepts only `leaf` (default).
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default) or `manhattan`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Clusterer Representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	hdbscan_clusterer(Encoders, Clusters, Noise, Options)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Clusters`: List of `cluster(Id, Points, Prototype, Radius)` terms in cluster-id order.
- `Noise`: List of encoded training points classified as noise.
- `Options`: Effective training options used to learn the clusterer.
