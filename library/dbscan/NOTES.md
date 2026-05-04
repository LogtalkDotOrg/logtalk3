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


`dbscan`
========

DBSCAN clusterer. Uses deterministic density-based clustering based on
epsilon neighborhoods and minimum point counts. Supports continuous
attributes only.

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

Open the [../../apis/library_index.html#dbscan](../../apis/library_index.html#dbscan)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(dbscan(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(dbscan(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(dbscan(tester_performance)).


Features
--------

- **Density-Based Clustering**: Learns density-connected clusters using epsilon neighborhoods and minimum point counts.
- **Adaptive Neighborhood Indexing**: Builds a low-dimensional epsilon-grid index when it is likely to be cheaper and otherwise falls back to a deterministic metric tree. The metric-tree path uses adaptive leaf sizing, balanced tie-aware subtree splits, selectable exact or heuristic pivot scoring, and lower-allocation range-query traversal.
- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Distance Metrics**: Supports `euclidean` and `manhattan` distances.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Reachable-Core Prediction**: New instances are assigned to the cluster of the nearest reachable core point within the learned epsilon radius; otherwise the atom `noise` is returned.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `epsilon(Epsilon)`: Neighborhood radius used to determine density connectivity. Default is `1.0`.
- `minimum_points(MinimumPoints)`: Minimum number of points in an epsilon neighborhood for a core point. Default is `2`.
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default) or `manhattan`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.
- `pivot_scoring(PivotScoring)`: Metric-tree pivot scoring strategy. Options: `heuristic` (default, single-pass dispersion scoring with one final sort) or `exact` (more expensive gap-and-range scoring that sorts each candidate profile).


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	dbscan_clusterer(Encoders, Clusters, Noise, Options)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Clusters`: List of `cluster(Id, CorePoints, BorderPoints)` terms in cluster-id order.
- `Noise`: List of encoded training points classified as noise.
- `Options`: Effective training options used to learn the clusterer.


References
----------

1. Ester, Kriegel, Sander, and Xu (1996) - "A Density-Based Algorithm for Discovering Clusters in Large Spatial Databases with Noise". KDD, 226-231.
