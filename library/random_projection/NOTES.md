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


`random_projection`
===================

Random projection reducer for continuous datasets. The library
implements the `dimension_reducer_protocol` defined in the
`dimension_reduction_protocols` library and learns a seeded dense
Rademacher projection matrix using the portable `fast_random`
pseudo-random generator.


API documentation
-----------------

Open the [../../apis/library_index.html#random_projection](../../apis/library_index.html#random_projection)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(random_projection(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(random_projection(tester)).


Features
--------

- **Continuous Datasets**: Accepts datasets containing only continuous attributes.
- **Centering and Optional Scaling**: Centers all attributes and optionally standardizes them before projection.
- **Portable Seeded Sampling**: Uses `fast_random(xoshiro128pp)` so learned projection matrices are portable and reproducible.
- **Projection API**: Transforms a new instance into a list of `component_N-Value` pairs.
- **Model Export**: Learned reducers can be exported as predicate clauses or written to a file.


Options
-------

The `learn/3` predicate accepts the following options:

- `n_components/1`: Number of random projection components to sample. Requests that exceed the number of features raise `domain_error(component_count, Requested-Maximum)`. The default is `2`.
- `feature_scaling/1`: Whether to standardize continuous attributes before projection. Options: `true` (default) or `false`.
- `random_seed/1`: Positive integer used to seed the portable pseudo-random generator before sampling the projection matrix. The default is `1357911`.


Usage
-----

The following examples use the sample datasets shipped with the
`dimension_reduction_protocols` library:

	| ?- logtalk_load(dimension_reduction_protocols('test_datasets/correlated_plane')),
	     logtalk_load(dimension_reduction_protocols('test_datasets/high_dimensional_measurements')).

### Learning a reducer

	| ?- random_projection::learn(correlated_plane, DimensionReducer).

	| ?- random_projection::learn(correlated_plane, DimensionReducer, [n_components(1), feature_scaling(false), random_seed(17)]).

### Transforming new instances

	| ?- random_projection::learn(high_dimensional_measurements, DimensionReducer, [random_seed(11)]),
	     random_projection::transform(DimensionReducer, [f1-0.9, f2-1.1, f3-1.0, f4-2.0, f5-2.2, f6-2.1], ReducedInstance).

	| ?- random_projection::learn(correlated_plane, DimensionReducer, [n_components(1), random_seed(19)]),
	     random_projection::transform(DimensionReducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).

### Exporting and reusing the reducer

	| ?- random_projection::learn(correlated_plane, DimensionReducer, [n_components(1), random_seed(29)]),
	     random_projection::export_to_file(correlated_plane, DimensionReducer, reducer, 'random_projection_reducer.pl').

	| ?- logtalk_load('random_projection_reducer.pl'),
	     reducer(Reducer),
	     random_projection::transform(Reducer, [x-1.0, y-2.0, z-3.0], ReducedInstance).


Dimension reducer representation
-------------------------------

The learned dimension reducer is represented by a compound term with the
functor chosen by the implementation and arity 3. For example:

	random_projection_reducer(Encoders, Components, Diagnostics)

Where:

- `Encoders`: List of continuous attribute encoders storing attribute name, mean, and scale.
- `Components`: List of sampled projection vectors in component order.
- `Diagnostics`: Learned reducer metadata including the effective training options and reproducibility details.

When exported using `export_to_clauses/4` or
`export_to_file/4`, this reducer term is serialized directly
as the single argument of the generated predicate clause so that the
exported model can be loaded and reused as-is.


References
----------

1. Johnson, W. B. and Lindenstrauss, J. (1984) - "Extensions of Lipschitz mappings into a Hilbert space".
2. Achlioptas, D. (2003) - "Database-friendly random projections: Johnson-Lindenstrauss with binary coins".
