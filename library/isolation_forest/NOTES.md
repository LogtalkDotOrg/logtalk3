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


`isolation_forest`
==================

This library implements the Extended Isolation Forest (EIF) algorithm for
anomaly detection as described by Hariri et al. (2019). The Extended
Isolation Forest improves upon the original Isolation Forest algorithm
(Liu et al., 2008) by using random hyperplane cuts instead of axis-aligned
cuts, eliminating bias artifacts in anomaly scores along coordinate axes.

The algorithm builds an ensemble of isolation trees (iTrees) by recursively
partitioning data using random hyperplanes. Anomalous points, being few
and different from normal points, require fewer partitions (shorter path
lengths) to be isolated. The anomaly score for an instance is computed
based on the average path length across all trees in the forest.

Datasets are represented as objects implementing the `dataset_protocol`
protocol from the `classifier_protocols` library. See the `test_datasets`
directory for examples.


API documentation
-----------------

Open the [../../apis/library_index.html#isolation-forest](../../apis/library_index.html#isolation-forest)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(isolation_forest(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(isolation_forest(tester)).


Implemented features
--------------------

- Extended Isolation Forest with random hyperplane cuts: splits are defined
  by random normal vectors and intercept points drawn from the data range,
  producing `(x - p) * n =< 0` partitions that generalize to arbitrary
  orientations
- Configurable extension level: level 0 corresponds to the original
  axis-aligned Isolation Forest; levels up to `d - 1` (the default) use
  fully extended random hyperplanes where `d` is the number of dimensions
- Anomaly score computation following Liu et al. (2008):
  `s(x) = 2^(-E(h(x)) / c(psi))` where `E(h(x))` is the average path
  length across all trees, `c(psi)` is the average path length of
  unsuccessful searches in a BST, and `psi` is the subsample size
- Handling of both continuous (numeric) and discrete (categorical)
  attributes: discrete attributes are mapped to numeric indices based
  on their position in the attribute value list declared by the dataset
- Handling of missing attribute values (represented using anonymous
  variables): during tree construction, missing values are replaced with
  random values drawn from the observed range of the corresponding
  attribute; during scoring, missing dimensions are excluded from the
  hyperplane dot product computation so that routing decisions at each
  tree node are based entirely on the known attribute values
- Configurable parameters via options:
  - `number_of_trees/1` (default: `100`): number of isolation trees
  - `subsample_size/1` (default: `256` or number of instances if smaller):
    subsample size for each tree
  - `extension_level/1` (default: `d - 1`): controls the dimensionality
    of the random hyperplane normal vectors
  - `anomaly_threshold/1` (default: `0.5`): threshold for anomaly
    prediction
- Scoring all dataset instances with results sorted by descending anomaly
  score for easy identification of top anomalies
- Pretty-printing of learned models with tree depth and node count
  summaries


Limitations
-----------

- No incremental learning (the forest must be rebuilt from scratch when
  new examples are added)
- No streaming or online variant


References
----------

- Liu, F.T., Ting, K.M. and Zhou, Z.-H. (2008). Isolation Forest.
  *Proceedings of the 2008 Eighth IEEE International Conference on
  Data Mining*, 413-422.
  https://doi.org/10.1109/ICDM.2008.17

- Hariri, S., Kind, M.C. and Brunner, R.J. (2019). Extended Isolation
  Forest. *IEEE Transactions on Knowledge and Data Engineering*, 33(4),
  1479-1489.
  https://doi.org/10.1109/TKDE.2019.2947676


Usage
-----

To learn an isolation forest model from a dataset with default options:

	| ?- isolation_forest::learn(gaussian_anomalies, Model).

To learn with custom options:

	| ?- isolation_forest::learn(gaussian_anomalies, Model, [
	         number_of_trees(200),
	         subsample_size(128),
	         extension_level(1),
	         anomaly_threshold(0.6)
	     ]).

To compute the anomaly score for a new instance:

	| ?- isolation_forest::learn(gaussian_anomalies, Model),
	     isolation_forest::score(Model, [x-0.12, y-0.34], Score).

To predict whether an instance is an anomaly or normal:

	| ?- isolation_forest::learn(gaussian_anomalies, Model),
	     isolation_forest::predict(Model, [x-4.50, y-4.20], Prediction).

To compute and rank anomaly scores for all instances in a dataset:

	| ?- isolation_forest::learn(gaussian_anomalies, Model),
	     isolation_forest::score_all(gaussian_anomalies, Model, Scores).

The `Scores` list contains `Id-Class-Score` triples sorted by descending
anomaly score. This makes it easy to inspect top anomalies:

	| ?- isolation_forest::learn(gaussian_anomalies, Model),
	     isolation_forest::score_all(gaussian_anomalies, Model, [Top1, Top2, Top3| _]).

To print a summary of the learned model:

	| ?- isolation_forest::learn(gaussian_anomalies, Model),
	     isolation_forest::print_model(Model).

To use the original (non-extended) Isolation Forest, set the extension
level to 0:

	| ?- isolation_forest::learn(gaussian_anomalies, Model, [extension_level(0)]).
