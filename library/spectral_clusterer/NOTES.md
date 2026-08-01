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


`spectral_clusterer`
====================

Normalized spectral clusterer for continuous datasets. It builds a dense
Gaussian RBF affinity matrix, computes a normalized spectral embedding, and
uses deterministic k-means clustering in the embedding space. New instances
are assigned using a degree-normalized Nyström extension.

The library implements the `clusterer_protocol` defined in the
`clustering_protocols` library. Datasets are represented as objects
implementing the `clustering_dataset_protocol` protocol.


API documentation
-----------------

Open the [../../apis/library_index.html#spectral_clusterer](../../apis/library_index.html#spectral_clusterer)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(spectral_clusterer(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(spectral_clusterer(tester)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Normalized Spectral Embedding**: Uses the leading eigenvectors of the symmetric normalized affinity matrix.
- **Gaussian RBF Affinity**: Supports an explicit positive sigma or automatic estimation from the median positive pairwise distance.
- **Nyström Prediction**: Projects unseen instances into the learned embedding before assigning the nearest embedding centroid.
- **Deterministic Optimization**: Uses deterministic eigendecomposition and `first_k` or `spread` centroid initialization.
- **Optional Feature Scaling**: Continuous attributes can be standardized using z-score scaling.
- **Portable Export**: Learned clusterers can be exported as clauses or files and reused later.


Options
-------

The following options can be passed to the `learn/3` predicate:

- `k(K)`: Number of clusters to learn. Default is `2`.
- `sigma(Sigma)`: Gaussian RBF scale. It can be `auto` (default) or a positive number. Automatic estimation uses the median positive pairwise distance.
- `maximum_iterations(Iterations)`: Maximum number of eigensolver and centroid optimization iterations. Default is `1000`.
- `tolerance(Tolerance)`: Numerical and centroid-shift convergence tolerance. Default is `1.0e-8`.
- `initialization(Initialization)`: Embedding centroid initialization strategy. Options: `spread` (default) or `first_k`.
- `feature_scaling(FeatureScaling)`: Whether to standardize continuous attributes before clustering. Options: `on` (default) or `off`.


Diagnostics
-----------

The `diagnostics/2` predicate returns a list containing:

- `model(spectral_clusterer)`
- `cluster_count(Count)`
- `training_example_count(Count)`
- `resolved_sigma(Sigma)`
- `eigenvalues(Eigenvalues)`
- `convergence(Reason)`
- `iterations(Count)`
- `final_shift(Shift)`
- `options(Options)`


Algorithm
---------

For encoded training rows `x_i`, the affinity and degree values are:

	W_ij = exp(-||x_i-x_j||^2 / (2*sigma^2))
	d_i  = sum_j W_ij

The symmetric normalized affinity matrix is:

	A_ij = W_ij / sqrt(d_i*d_j)

The leading `K` eigenvectors of `A` are the bottom `K` eigenvectors of the
corresponding symmetric normalized Laplacian. Their transposed rows are
normalized to unit length and clustered using deterministic k-means.

For a new instance `x`, the Nyström embedding coordinate for component `j`
is computed as:

	z_j(x) = (1/mu_j) * sum_i A(x,x_i) * v_j(i)

where `mu_j` and `v_j` are a selected eigenvalue and eigenvector. The
resulting row is normalized before nearest-centroid assignment.


Clusterer representation
------------------------

The learned clusterer is represented as a compound term with the functor
chosen by the user when exporting the clusterer and arity 9:

	spectral_clusterer(Encoders, TrainingRows, Degrees, Components, Eigenvalues, Centroids, ResolvedSigma, Options, Diagnostics)

The training rows, degrees, components, eigenvalues, and resolved sigma are
retained to support Nyström prediction.


Complexity
----------

The implementation uses dense matrices. Affinity construction and storage
require quadratic time and space in the number of training examples. The
full portable symmetric eigendecomposition is intended for small and medium
datasets rather than large sparse graphs.


References
----------

1. Ng, Jordan, and Weiss (2002) - "On Spectral Clustering: Analysis and an Algorithm". Advances in Neural Information Processing Systems 14.
2. Bengio, Paiement, Vincent, Delalleau, Le Roux, and Ouimet (2004) - "Out-of-Sample Extensions for LLE, Isomap, MDS, Eigenmaps, and Spectral Clustering". Advances in Neural Information Processing Systems 16.
