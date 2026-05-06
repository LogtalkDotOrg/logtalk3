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


`kernel_pca_projection`
============

Kernel Principal Component Analysis reducer for continuous datasets. The
library implements the `dimension_reducer_protocol` defined in the
`dimension_reduction_protocols` library and learns a nonlinear
projection by centering the training data, optionally standardizing
continuous attributes, building a centered kernel Gram matrix, and
extracting deterministic principal directions in sample space using
portable power iteration with deflation.


API documentation
-----------------

Open the [../../apis/library_index.html#kernel_pca_projection](../../apis/library_index.html#kernel_pca_projection)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(kernel_pca_projection(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(kernel_pca_projection(tester)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes. Missing or nonnumeric values are rejected.
- **Centering and Optional Scaling**: Centers all attributes and optionally standardizes them before evaluating kernels.
- **Configurable Shortfall Handling**: Lets callers choose whether a component-extraction shortfall raises an error or truncates the learned reducer with explicit diagnostics.
- **Supported Kernels**: Supports linear, polynomial with non-negative offset, and radial basis function kernels through the `kernel/1` option.
- **Shared Gram-Centering Helpers**: Delegates training Gram-matrix centering and out-of-sample Gram-vector centering to the shared `linear_algebra` library.
- **Portable Eigensolver**: Uses deterministic power iteration with deflation instead of backend-specific linear algebra libraries.
- **Projection API**: Transforms a new instance into a list of `component_N-Value` pairs using centered kernel evaluations against the training rows.
- **Model Export**: Learned reducers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate accepts the following options:

- `n_components/1`: Number of kernel principal components to extract. Requests that exceed `SampleCount - 1` raise `domain_error(component_count, Requested-Maximum)`. The default is `2`.
- `feature_scaling/1`: Whether to standardize continuous attributes before evaluating kernels. Options: `true` (default) or `false`.
- `shortfall_policy/1`: Controls what happens when the centered kernel Gram matrix yields fewer numerically significant components than requested. Options: `truncate` (default), which returns a reducer with fewer components and records a `shortfall(truncated(Requested, Learned, ResidualEigenvalue, Tolerance))` diagnostic, or `error`, which raises `domain_error(component_count, Requested-Learned)`.
- `kernel/1`: Kernel specification. Supported values are `linear` (default), `polynomial(Degree, Gamma, Coef0)` with positive `Degree`, positive `Gamma`, and non-negative `Coef0`, and `rbf(Gamma)` with positive `Gamma`.
- `maximum_iterations/1`: Maximum number of power-iteration steps used when estimating each dual principal direction. The default is `1000`.
- `tolerance/1`: Positive convergence tolerance used both for power-iteration stopping and for deciding when deflated eigenvalues are negligible. The default is `1.0e-8`.


Usage
-----

The following examples use the sample datasets shipped with the
`dimension_reduction_protocols` library:

	| ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')).

### Learning a reducer

	| ?- kernel_pca_projection::learn(correlated_plane, DimensionReducer).

	| ?- kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1), kernel(rbf(0.25))]).

### Transforming new instances

	| ?- kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(2), kernel(rbf(0.25))]),
	     kernel_pca_projection::transform(DimensionReducer, [x-2.0, y-4.0, z-6.0], ReducedInstance).

### Exporting and reusing the reducer

	| ?- kernel_pca_projection::learn(correlated_plane, DimensionReducer, [n_components(1)]),
	     kernel_pca_projection::export_to_file(correlated_plane, DimensionReducer, reducer, 'kernel_pca_reducer.pl').

	| ?- logtalk_load('kernel_pca_reducer.pl'),
	     reducer(Reducer),
	     kernel_pca_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).


Dimension reducer representation
-------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 7. For example:

	kernel_pca_reducer(Encoders, TrainingRows, RowMeans, TotalMean, Components, ExplainedVariances, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `TrainingRows`: Encoded training rows used when evaluating kernels for new instances.
- `RowMeans`: Per-training-row kernel means used for centering out-of-sample kernel vectors.
- `TotalMean`: Global kernel mean used for centering both the training Gram matrix and new kernel vectors.
- `Components`: List of normalized dual projection vectors in descending variance order.
- `ExplainedVariances`: List of kernel Gram matrix eigenvalues matching the extracted components.
- `Diagnostics`: Learned metadata including the effective training options, kernel preprocessing, sample count, explained variances, and optional truncate-mode shortfall details.

When exported using `export_to_clauses/4` or
`export_to_file/4`, this reducer term is serialized directly
as the single argument of the generated predicate clause so that the
exported model can be loaded and reused as-is.


References
----------

1. Schölkopf, B., Smola, A., and Müller, K.-R. (1998) - "Nonlinear component analysis as a kernel eigenvalue problem".
2. Shawe-Taylor, J. and Cristianini, N. (2004) - "Kernel Methods for Pattern Analysis".
