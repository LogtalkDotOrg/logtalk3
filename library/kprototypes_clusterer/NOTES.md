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


`kprototypes_clusterer`
=============

k-Prototypes clusterer. It uses an iterative prototype-update
algorithm with deterministic initialization and deterministic cluster
assignments. Supports continuous and discrete attributes in the same
dataset.

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

Open the [../../apis/library_index.html#kprototypes_clusterer](../../apis/library_index.html#kprototypes_clusterer)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kprototypes_clusterer(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kprototypes_clusterer(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(kprototypes_clusterer(tester_performance)).


Features
--------

- **Mixed Datasets**: Accepts datasets with continuous, discrete, or mixed attributes.
- **Strict Attribute Validation**: Training examples and prediction instances must contain each declared attribute exactly once and no undeclared attributes.
- **Deterministic Initialization**: Supports `first_k` and deterministic `spread` initialization that repeatedly chooses the farthest example from the prototypes selected so far.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Categorical Weighting**: Uses a `gamma` mismatch penalty for discrete attributes in the mixed distance function.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.
- **Stable Empty-Cluster Handling**: Empty clusters keep their previous prototypes instead of failing.
- **Training Diagnostics**: Exposes convergence metadata including training example count, iteration count, and final prototype shift.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to learn. Default is `2`.
- `maximum_iterations(Iterations)`: Maximum number of prototype-update iterations. Default is `100`.
- `tolerance(Tolerance)`: Maximum prototype shift threshold for convergence. Default is `1.0e-6`.
- `initialization(Initialization)`: Prototype initialization strategy. Options: `spread` (default) or `first_k`.
- `gamma(Gamma)`: Penalty added for each discrete-feature mismatch. Default is `1.0`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Distance Function
-----------------

The mixed k-prototypes distance used for both assignment and prototype
spread initialization is:

	D(X, P) = Sum((x_i - p_i)^2) + gamma * M

where the sum is taken over all continuous attributes and `M` is the
number of discrete-attribute mismatches between the instance `X` and the
prototype `P`.

For discrete prototype updates, the selected value for each categorical
attribute is the most frequent value among the cluster members. When two
or more values are tied, the implementation deterministically keeps the
first value in the declared attribute-values list.


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 4.
For example:

	kprototypes_clusterer(Encoders, Prototypes, Options, Diagnostics)

Where:

- `Encoders`: List of continuous and discrete attribute encoders.
- `Prototypes`: List of learned mixed prototypes in cluster-id order.
- `Options`: Effective training options used to learn the clusterer.
- `Diagnostics`: Training metadata including convergence reason, iteration count, and final prototype shift.


References
----------

1. Huang (1997) - "Clustering large data sets with mixed numeric and categorical values". Proceedings of the First Pacific-Asia Conference on Knowledge Discovery and Data Mining.
2. Huang (1998) - "Extensions to the k-means algorithm for clustering large data sets with categorical values". Data Mining and Knowledge Discovery, 2, 283-304.
