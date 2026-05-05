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


`truncated_svd`
===============

Truncated singular value decomposition reducer for continuous datasets.
The library implements the `dimension_reducer_protocol` defined in the
`dimension_reduction_protocols` library and learns a low-rank linear
projection by building a preprocessed data matrix using optional
centering and scaling, extracting singular triplets using deterministic
two-sided power iteration, and applying rank-one deflation directly to
the data matrix.


API documentation
-----------------

Open the [../../apis/library_index.html#truncated_svd](../../apis/library_index.html#truncated_svd)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(truncated_svd(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(truncated_svd(tester)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes. Missing or nonnumeric values are rejected.
- **Low-Rank Projection**: Extracts right singular vectors and singular values in descending order.
- **Portable SVD Solver**: Uses deterministic two-sided power iteration with rank-one deflation over the data matrix instead of backend-specific linear algebra libraries.
- **Optional Centering and Scaling**: Supports explicit control over whether training and transform inputs are centered and/or scaled.
- **Training Diagnostics**: Records per-component convergence reasons, iteration counts, and final deltas alongside the learned singular values.
- **Projection API**: Transforms a new instance into a list of `component_N-Value` pairs.
- **Model Export**: Learned reducers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate accepts the following options:

- `n_components/1`: Number of singular vectors to extract. The default is `2`. Requests that exceed the numerical rank after shape capping raise `domain_error(component_count, Requested-Extracted)` instead of silently returning fewer components.
- `center/1`: Whether to center the training and transform inputs using the training-set means. Options: `true` or `false` (default).
- `feature_scaling/1`: Whether to divide each continuous attribute by its training-set standard deviation. Options: `true` or `false` (default).
- `maximum_iterations/1`: Maximum number of power-iteration steps used when estimating each singular vector. The default is `1000`.
- `tolerance/1`: Positive convergence tolerance used both for power-iteration stopping and for deciding when the residual matrix no longer supports the requested number of components. The default is `1.0e-8`.

The learned diagnostics also include:

- `convergence(Statuses)`: Per-component stop reasons, such as `tolerance` or `maximum_iterations_exhausted`.
- `iterations(Counts)`: Per-component iteration counts aligned with the extracted singular vectors.
- `final_delta(Deltas)`: Per-component final update magnitudes aligned with the extracted singular vectors.


Usage
-----

The following examples use the sample datasets shipped with the
`dimension_reduction_protocols` library:

	| ?- logtalk_load(dimension_reduction_protocols('test_datasets/low_rank_rectangular')).

### Learning a reducer

	| ?- truncated_svd::learn(low_rank_rectangular, DimensionReducer).

	| ?- truncated_svd::learn(low_rank_rectangular, DimensionReducer, [n_components(1), center(false), feature_scaling(false), maximum_iterations(200), tolerance(1.0e-7)]).

### Transforming new instances

	| ?- truncated_svd::learn(low_rank_rectangular, DimensionReducer),
	     truncated_svd::transform(DimensionReducer, [f1-1.0, f2-1.0, f3-2.0], ReducedInstance).

### Exporting and reusing the reducer

	| ?- truncated_svd::learn(low_rank_rectangular, DimensionReducer, [n_components(1)]),
	     truncated_svd::export_to_file(low_rank_rectangular, DimensionReducer, reducer, 'truncated_svd_reducer.pl').

	| ?- logtalk_load('truncated_svd_reducer.pl'),
	     reducer(Reducer),
	     truncated_svd::transform(Reducer, [f1-1.0, f2-1.0, f3-2.0], ReducedInstance).


Dimension reducer representation
-------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 4. For example:

	truncated_svd_reducer(Encoders, Components, SingularValues, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, centering offset, and scale factor.
- `Components`: List of right singular vectors in descending singular-value order.
- `SingularValues`: List of singular values matching the extracted components.
- `Diagnostics`: Learned reducer metadata including the effective training options, singular values, and per-component convergence information.


References
----------

1. Eckart, C. and Young, G. (1936) - "The approximation of one matrix by another of lower rank".
