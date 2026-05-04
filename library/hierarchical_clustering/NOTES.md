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


`hierarchical_clustering`
=========================

Hierarchical clusterer.

The library implements the `clusterer_protocol` defined in the
`clustering_protocols` library. It provides predicates for learning a
full agglomerative hierarchy from a dataset, deriving a `k`-cluster cut
for prediction, and exporting the learned clusterer as a list of
predicate clauses or to a file.

Datasets are represented as objects implementing the
`clustering_dataset_protocol` protocol from the `clustering_protocols`
library.


API documentation
-----------------

Open the [../../apis/library_index.html#hierarchical_clustering](../../apis/library_index.html#hierarchical_clustering)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(hierarchical_clustering(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(hierarchical_clustering(tester)).


Features
--------

- **Full Hierarchy Learning**: Builds the complete bottom-up merge hierarchy before deriving the requested cluster partition.
- **Deterministic Dendrogram Cutting**: Produces the final `k` clusters by repeatedly splitting the highest remaining merge.
- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Linkage Strategies**: Supports `single`, `complete`, and `average` linkage.
- **Distance Metrics**: Supports `euclidean` and `manhattan` distances.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Linkage-Aware Prediction**: New instances are assigned to the nearest learned cluster using the selected linkage strategy and distance metric rather than a prototype shortcut.
- **Incremental Distance Updates**: Training caches singleton pair distances once and updates merge distances incrementally for `single`, `complete`, and `average` linkage instead of recomputing all member-pair distances after every merge.
- **Heap-Based Merge Selection**: Training uses a lazy min-heap of candidate cluster pairs and skips stale entries for merged-away nodes instead of rescanning all active pairs at every agglomeration step.
- **Reusable Hierarchy Cuts**: Learned hierarchies can be re-cut to a different `k` using `cut/3` without retraining.
- **Rich Diagnostics**: Learned clusterers record merge counts, dendrogram height, heap rebuilds, scan fallbacks, maximum heap size, and effective options.
- **Deterministic Tie-Breaking**: Equal merge distances and equal split heights are resolved by preferring the smallest node-id pair.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to retain after cutting the learned hierarchy. Default is `2`.
- `linkage(Linkage)`: Linkage strategy to use. Options: `single`, `complete`, or `average` (default).
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default) or `manhattan`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 5.
For example:

	hierarchical_clustering_clusterer(Encoders, hierarchy(RootState, MergeRecords, Dendrogram), Clusters, Prototypes, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `hierarchy(RootState, MergeRecords, Dendrogram)`: Reusable hierarchy state. `RootState` and `MergeRecords` are used internally by `cut/3`, and `Dendrogram` is the learned merge tree represented with `leaf(Id)` and `merge(Left, Right, Distance, Size)` terms.
- `Clusters`: List of `cluster(Id, Points)` terms for the selected `k`-cluster cut.
- `Prototypes`: List of average vectors kept for display and export metadata.
- `Diagnostics`: Diagnostics metadata including the effective training options used to learn the clusterer.


Additional API
--------------

- `cut(+Clusterer, +K, -RecutClusterer)`: Reuses the learned hierarchy to derive a new `K`-cluster cut without retraining.


Diagnostics
-----------

The `diagnostics/2` predicate returns metadata terms including:

- `model(hierarchical_clustering)`
- `cluster_count(Count)`
- `prototype_count(Count)`
- `training_example_count(Count)`
- `merge_count(Count)`
- `dendrogram_height(Height)`
- `heap_rebuild_count(Count)`
- `scan_fallback_count(Count)`
- `maximum_heap_size(Size)`
- `tie_breaking(node_id_order)`
- `options(Options)`
