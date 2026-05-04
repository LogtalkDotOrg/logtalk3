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

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(hdbscan(tester_performance)).


Features
--------

- **Hierarchical Density Clustering**: Builds the mutual-reachability graph, computes a minimum spanning tree, derives the single-linkage hierarchy, condenses it using `minimum_cluster_size`, and selects clusters using `eom` or `leaf` selection.
- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Cluster Selection Methods**: Supports both `eom` and `leaf` cluster selection.
- **Distance Metrics**: Supports `euclidean` and `manhattan` distances.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Reachability-Based Prediction**: New instances are assigned to the selected cluster with the nearest training point when the distance is within the learned cluster reachability threshold; otherwise the atom `noise` is returned.
- **Noise Detection**: Points not assigned to any extracted cluster are retained as noise.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `minimum_points(MinimumPoints)`: Minimum neighborhood size used when computing core distances and mutual reachability. Default is `2`.
- `minimum_cluster_size(MinimumClusterSize)`: Minimum number of points required for an extracted cluster. Default is `2`.
- `cluster_selection_method(Method)`: Cluster extraction policy. Options: `eom` (default) or `leaf`.
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default) or `manhattan`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	hdbscan_clusterer(Encoders, Clusters, Noise, Options)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
	- `Clusters`: List of `cluster(Id, Points, MaxCoreDistance, Stability)` terms in cluster-id order.
- `Noise`: List of encoded training points classified as noise.
- `Options`: Effective training options used to learn the clusterer.
