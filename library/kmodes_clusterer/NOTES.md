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


`kmodes_clusterer`
========

k-Modes clusterer. It uses an iterative mode-update algorithm with
deterministic initialization and deterministic cluster assignments.
Supports discrete attributes only.

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

Open the [../../apis/library_index.html#kmodes_clusterer](../../apis/library_index.html#kmodes_clusterer)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kmodes_clusterer(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kmodes_clusterer(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(kmodes_clusterer(tester_performance)).


Features
--------

- **Discrete Datasets**: Accepts datasets containing only discrete attributes.
- **Deterministic Initialization**: Supports `first_k` and deterministic `spread` initialization that repeatedly chooses the farthest example from the modes selected so far.
- **Rich Training Diagnostics**: Learned clusterers report training example count, convergence status, iteration count, and final mode shift.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous modes instead of failing.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to learn. Default is `2`.
- `maximum_iterations(Iterations)`: Maximum number of mode-update iterations. Default is `100`.
- `tolerance(Tolerance)`: Maximum mode shift threshold for convergence. Default is `0.0`.
- `initialization(Initialization)`: Mode initialization strategy. Options: `spread` (default) or `first_k`.


Diagnostics
-----------

The `diagnostics/2` predicate returns a list containing:

- `model(kmodes_clusterer)`
- `mode_count(Count)`
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

	kmodes_clusterer(Encoders, Modes, Options, Diagnostics)

Where:

- `Encoders`: List of discrete attribute encoders.
- `Modes`: List of learned categorical modes in cluster-id order.
- `Options`: Effective training options used to learn the clusterer.
- `Diagnostics`: Training diagnostics metadata returned by the `diagnostics/2` predicate.


References
----------

1. Huang (1998) - "Extensions to the k-means algorithm for clustering large data sets with categorical values". Data Mining and Knowledge Discovery, 2, 283-304.
