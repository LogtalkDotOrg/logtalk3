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


`linear_algebra`
================

This library provides predicates for numeric vectors and matrices, including
vector construction and scaling, general vector norms, matrix construction and transformation,
matrix-matrix products, row Gram matrices and Gram-centering helpers, offset diagonal helpers,
triangular-part extraction, diagonal shifts, lower- and upper-triangular matrix solves,
direct row and column lookup, tolerance-aware vector normalization and sign stabilization,
generic dense square-system solves, determinants, matrix inversion,
thin QR decomposition, QR-backed least-squares solving, tolerance-aware rank estimation,
real symmetric eigendecomposition, pseudo-inverse and null-space helpers,
covariance helpers, and Cholesky-based linear solves.

Vectors are represented as lists of numbers. Matrices are represented as lists
of row lists, where each row is a numeric vector.

Predicates that index into vectors or matrices use one-based indices. Matrix
access predicates use one-based row and column indices.

Square-system predicates include `solve_linear_system/3`,
`solve_linear_systems/3`, `determinant/2`, and `inverse_matrix/2`.
Rectangular-system predicates include `qr_decomposition/3` and
`least_squares/3`, `pseudo_inverse/2-3`, and `null_space/2-3`.
Row-matrix structural predicates include `gram_matrix/2`,
`matrix_row_means/2`, `matrix_column_means/2`, `center_gram_matrix/4`, and
`center_gram_vector/4`.
Diagonal and triangular helpers include `matrix_diagonal/2-3`,
`diagonal_matrix/2-4`, `upper_triangular_part/2-3`, and
`lower_triangular_part/2-3`.
Norm helpers include `vector_norm/3` and `frobenius_norm/2`.
Real symmetric spectral predicates include `symmetric_eigen/3-5`.
Tolerance-sensitive numerical predicates include
`normalize_vector/2-3`, `first_significant_component/2-3`,
`stabilize_vector_sign/2-3`, `matrix_rank/2-3`,
	`symmetric_eigen/3-5`, `pseudo_inverse/2-3`, and `null_space/2-3`.

The `symmetric_eigen/5` predicate can be used when callers need to control
the power-iteration budget explicitly instead of relying on the default
iteration bound.


API documentation
-----------------

Open the [../../apis/library_index.html#linear_algebra](../../apis/library_index.html#linear_algebra)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(linear_algebra(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(linear_algebra(tester)).
