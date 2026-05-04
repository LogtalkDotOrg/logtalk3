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


`gaussian_mixture`
===================

Gaussian mixture model clusterer.

The library implements the `clusterer_protocol` defined in the
`clustering_protocols` library. It provides predicates for learning a
clusterer from a dataset, assigning new instances to clusters,
returning Gaussian-mixture posterior component probabilities for new
instances, and exporting the learned clusterer as a list of predicate
clauses or to a file.

Datasets are represented as objects implementing the
`clustering_dataset_protocol` protocol from the `clustering_protocols`
library.


API documentation
-----------------

Open the [../../apis/library_index.html#gaussian_mixture](../../apis/library_index.html#gaussian_mixture)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(gaussian_mixture(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(gaussian_mixture(tester)).

To run the performance benchmark suite, load the `tester_performance.lgt`
file:

	| ?- logtalk_load(gaussian_mixture(tester_performance)).


Features
--------

- **Expectation-Maximization**: Learns diagonal Gaussian components using deterministic EM updates.
- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Configurable Dead-Component Handling**: Dead components can either be preserved with zero weight or deterministically reseeded to the least-confident training row.
- **Deterministic Initialization**: Supports `first_k` and deterministic `spread` initialization for component means. The `spread` strategy uses a canonical first seed and canonical tie-breaking so equivalent row permutations produce the same initialization.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Posterior Prediction**: New instances are assigned to the component with the highest posterior score, and `cluster_probabilities/3` can return the full posterior distribution over components.
- **Training Diagnostics**: Learned clusterers record convergence reason, iteration count, average log-likelihood, final delta, and effective options.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Gaussian-Mixture-Specific Prediction API
----------------------------------------

In addition to the shared `cluster/3` predicate from the clustering
protocols library, this package provides a Gaussian-mixture-specific
predicate:

- `cluster_probabilities(Clusterer, Instance, Probabilities)`:
	Returns posterior component probabilities for `Instance` as
	`Cluster-Probability` pairs in component-id order.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of mixture components. Default is `2`.
- `initialization(Initialization)`: Mean initialization strategy. Options: `spread` (default) or `first_k`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.
- `maximum_iterations(MaximumIterations)`: Maximum number of EM iterations. Default is `100`.
- `tolerance(Tolerance)`: Per-example average log-likelihood convergence tolerance. Default is `0.0001`.
- `covariance_regularization(Regularization)`: Positive diagonal covariance regularization constant. Default is `0.001`.
- `dead_component_policy(Policy)`: Handling for components whose total responsibility collapses below the dead-component threshold. Options: `zero_weight` (default) keeps the previous component with zero weight; `reseed` relocates the component to the least-confident training row and gives it one-example prior weight.


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the
functor chosen by the user when exporting the clusterer and arity 5.
For example:

	gaussian_mixture_clusterer(Encoders, Components, Weights, Options, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Components`: List of `component(Mean, Variances)` terms in component-id order.
- `Weights`: List of mixture weights in component-id order.
- `Options`: Effective training options used to learn the clusterer.
- `Diagnostics`: Training diagnostics including convergence status, iteration count, average log-likelihood, final delta, and options.
