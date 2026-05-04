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


`kcenters`
===========

k-Centers clusterer.

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

Open the [../../apis/library_index.html#kcenters](../../apis/library_index.html#kcenters)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kcenters(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kcenters(tester)).

To run the performance and reference-fit benchmarks, load the `tester_performance.lgt` file:

	| ?- logtalk_load(kcenters(tester_performance)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Farthest-First Center Selection**: Uses a deterministic farthest-first heuristic to choose centers.
- **Deterministic Initialization**: Supports `first_k` and deterministic `spread` initialization.
- **Distance Metrics**: Supports `euclidean` and `manhattan` distances.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Rich Training Diagnostics**: Learned clusterers report training example count, selected center count, and the center-selection strategy used during learning.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to learn. Default is `2`.
- `initialization(Initialization)`: Center initialization strategy. Options: `spread` (default) or `first_k`.
- `distance_metric(Metric)`: Distance metric to use. Options: `euclidean` (default) or `manhattan`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Diagnostics
-----------

The `diagnostics/2` predicate returns a list containing:

- `model(kcenters)`
- `center_count(Count)`
- `training_example_count(Count)`
- `selection_strategy(Strategy)`
- `options(Options)`


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	kcenters_clusterer(Encoders, Centers, Options, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Centers`: List of center vectors in cluster-id order.
- `Options`: Effective training options used to learn the clusterer.
- `Diagnostics`: Training diagnostics metadata returned by the `diagnostics/2` predicate.


References
----------

1. Gonzalez (1985) - "Clustering to minimize the maximum intercluster distance". Theoretical Computer Science, 38, 293-306.
