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


`nmf`
=====

This library implements Non-negative Matrix Factorization (NMF) for
continuous datasets whose attribute values are all non-negative. It
learns a non-negative basis using deterministic multiplicative updates
and represents each transformed instance as a list of non-negative
component weights.


API documentation
-----------------

Open the [../../apis/library_index.html#nmf](../../apis/library_index.html#nmf)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(nmf(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(nmf(tester)).


Features
--------

- Implements deterministic multiplicative-update training for NMF.

- Requires all training and inference attribute values to be
  non-negative.

- Supports exporting learned reducers and reloading them for later use.

- Records diagnostics including convergence status, iteration count,
  final update delta, and reconstruction error.


Options
-------

- `n_components/1`

  Number of latent components to learn. The current implementation
  requires the requested count to not exceed the minimum of the sample
  count and the number of features. Larger requests raise
  `domain_error(component_count, Requested-Maximum)`.

- `center/1`

  Always `false`. Centering is intentionally disabled because NMF
  requires non-negative features.

- `feature_scaling/1`

  When `true`, each continuous attribute is divided by its maximum
  observed training value. When `false`, raw non-negative values are
  used.

- `maximum_iterations/1`

  Maximum number of multiplicative-update iterations used during both
  training and per-instance coefficient inference.

- `tolerance/1`

  Stop iteration when the maximum absolute update delta is less than or
  equal to this value.


Usage
-----

Assuming a dataset object `parts_based_measurements` implementing the
`dimension_reduction_dataset_protocol` protocol:

	| ?- nmf::learn(parts_based_measurements, DimensionReducer).

	| ?- nmf::learn(parts_based_measurements, DimensionReducer, [n_components(2), feature_scaling(true), maximum_iterations(250), tolerance(1.0e-7)]).

	| ?- nmf::learn(parts_based_measurements, DimensionReducer),
	     nmf::transform(DimensionReducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ReducedInstance).

	| ?- nmf::learn(parts_based_measurements, DimensionReducer, [n_components(2)]),
	     nmf::export_to_file(parts_based_measurements, DimensionReducer, reducer, 'nmf_reducer.pl').

	| ?- logtalk_load('nmf_reducer.pl'),
	     reducer(Reducer),
	     nmf::transform(Reducer, [f1-3.0, f2-0.0, f3-1.5, f4-0.0], ReducedInstance).


Dimension reducer representation
--------------------------------

Learned NMF reducers use the representation:

	nmf_reducer(Encoders, Components, Diagnostics)

where `Encoders` stores the continuous attribute encoders used to map
instances into a non-negative feature vector, `Components` stores the
learned non-negative basis vectors, and `Diagnostics` stores metadata
about the learned reducer.


References
----------

- Daniel D. Lee and H. Sebastian Seung. *Algorithms for Non-negative
  Matrix Factorization*. Advances in Neural Information Processing
  Systems 13, 2001.
