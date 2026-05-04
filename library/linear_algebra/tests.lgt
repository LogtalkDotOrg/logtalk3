%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Unit tests for the "linear_algebra" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(linear_algebra).

	test(linear_algebra_new_vector_3, deterministic(Vector == [0.0, 0.0, 0.0])) :-
		linear_algebra::new_vector(3, 0.0, Vector).

	test(linear_algebra_new_vector_like_2, deterministic(Vector == [0.0, 0.0, 0.0])) :-
		linear_algebra::new_vector_like([1.0, 2.0, 3.0], Vector).

	test(linear_algebra_new_matrix_4, deterministic(Matrix == [[1.0, 1.0], [1.0, 1.0]])) :-
		linear_algebra::new_matrix(2, 2, 1.0, Matrix).

	test(linear_algebra_add_scaled_vector_4, deterministic(Updated == [2.0, 3.0, 4.0])) :-
		linear_algebra::add_scaled_vector([1.0, 2.0, 3.0], 1.0, [1.0, 1.0, 1.0], Updated).

	test(linear_algebra_dot_product_3, deterministic(Value == 11.0)) :-
		linear_algebra::dot_product([1.0, 2.0], [3.0, 4.0], Value).

	test(linear_algebra_euclidean_norm_2, deterministic(Norm =~= 5.0)) :-
		linear_algebra::euclidean_norm([3.0, 4.0], Norm).

	test(linear_algebra_vector_norm_3_order_1, deterministic(Norm == 7.0)) :-
		linear_algebra::vector_norm([3.0, 4.0], 1, Norm).

	test(linear_algebra_vector_norm_3_order_inf, deterministic(Norm == 4.0)) :-
		linear_algebra::vector_norm([3.0, 4.0], inf, Norm).

	test(linear_algebra_vector_norm_3_invalid_order, error(domain_error(positive_number_or_infinity, 0))) :-
		linear_algebra::vector_norm([3.0, 4.0], 0, _).

	test(linear_algebra_basis_vector_3, deterministic(Vector == [0.0, 1.0, 0.0])) :-
		linear_algebra::basis_vector(3, 2, Vector).

	test(linear_algebra_transpose_matrix_2, deterministic(Transpose == [[1.0, 4.0], [2.0, 5.0], [3.0, 6.0]])) :-
		linear_algebra::transpose_matrix([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], Transpose).

	test(linear_algebra_vector_value_3, deterministic(Value == 2.0)) :-
		linear_algebra::vector_value([1.0, 2.0, 3.0], 2, Value).

	test(linear_algebra_matrix_row_3, deterministic(Row == [4.0, 5.0, 6.0])) :-
		linear_algebra::matrix_row([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], 2, Row).

	test(linear_algebra_matrix_column_3, deterministic(Column == [2.0, 5.0])) :-
		linear_algebra::matrix_column([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]], 2, Column).

	test(linear_algebra_normalize_vector_2, deterministic((X =~= 0.6, Y =~= 0.8))) :-
		linear_algebra::normalize_vector([3.0, 4.0], [X, Y]).

	test(linear_algebra_normalize_vector_3_large_tolerance, deterministic(Vector == [3.0, 4.0])) :-
		linear_algebra::normalize_vector([3.0, 4.0], 10.0, Vector).

	test(linear_algebra_normalize_vector_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::normalize_vector([3.0, 4.0], -1.0, _).

	test(linear_algebra_first_significant_component_2, deterministic(Value == 0.5)) :-
		linear_algebra::first_significant_component([1.0e-13, 0.5, -0.2], Value).

	test(linear_algebra_first_significant_component_3, deterministic(Value == 0.6)) :-
		linear_algebra::first_significant_component([0.1, 0.4, 0.6], 0.5, Value).

	test(linear_algebra_first_significant_component_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::first_significant_component([0.1, 0.4, 0.6], -1.0, _).

	test(linear_algebra_stabilize_vector_sign_2, deterministic(Vector == [1.0, -2.0])) :-
		linear_algebra::stabilize_vector_sign([-1.0, 2.0], Vector).

	test(linear_algebra_stabilize_vector_sign_3, deterministic(Vector == [1.0e-13, 2.0])) :-
		linear_algebra::stabilize_vector_sign([-1.0e-13, -2.0], 1.0e-12, Vector).

	test(linear_algebra_stabilize_vector_sign_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::stabilize_vector_sign([1.0, -2.0], -1.0, _).

	test(linear_algebra_matrix_vector_product_3, deterministic(Product == [2.0, 4.0])) :-
		linear_algebra::matrix_vector_product([[1.0, 2.0], [3.0, 4.0]], [0.0, 1.0], Product).

	test(linear_algebra_identity_matrix_2, deterministic(Matrix == [[1.0, 0.0], [0.0, 1.0]])) :-
		linear_algebra::identity_matrix(2, Matrix).

	test(linear_algebra_matrix_diagonal_2, deterministic(Diagonal == [1.0, 5.0, 9.0])) :-
		linear_algebra::matrix_diagonal([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], Diagonal).

	test(linear_algebra_matrix_diagonal_3, deterministic(Diagonal == [2.0, 6.0])) :-
		linear_algebra::matrix_diagonal([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], 1, Diagonal).

	test(linear_algebra_diagonal_matrix_2, deterministic(Matrix == [[1.0, 0.0, 0.0], [0.0, 2.0, 0.0], [0.0, 0.0, 3.0]])) :-
		linear_algebra::diagonal_matrix([1.0, 2.0, 3.0], Matrix).

	test(linear_algebra_diagonal_matrix_3, deterministic(Matrix == [[0.0, 1.0, 0.0, 0.0], [0.0, 0.0, 2.0, 0.0], [0.0, 0.0, 0.0, 3.0], [0.0, 0.0, 0.0, 0.0]])) :-
		linear_algebra::diagonal_matrix([1.0, 2.0, 3.0], 1, Matrix).

	test(linear_algebra_diagonal_matrix_4, deterministic(Matrix == [[0.0, 0.0, 0.0, 0.0], [1.0, 0.0, 0.0, 0.0], [0.0, 2.0, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0]])) :-
		linear_algebra::diagonal_matrix([1.0, 2.0], -1, 4, Matrix).

	test(linear_algebra_diagonal_matrix_4_small_size, error(domain_error(minimum_matrix_size(3), 2))) :-
		linear_algebra::diagonal_matrix([1.0, 2.0], 1, 2, _).

	test(linear_algebra_matrix_trace_2, deterministic(Trace == 15.0)) :-
		linear_algebra::matrix_trace([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], Trace).

	test(linear_algebra_shift_matrix_diagonal_3, deterministic(Matrix == [[1.5, 2.0], [3.0, 4.5]])) :-
		linear_algebra::shift_matrix_diagonal([[1.0, 2.0], [3.0, 4.0]], 0.5, Matrix).

	test(linear_algebra_upper_triangular_part_2, deterministic(Matrix == [[1.0, 2.0, 3.0], [0.0, 5.0, 6.0], [0.0, 0.0, 9.0]])) :-
		linear_algebra::upper_triangular_part([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], Matrix).

	test(linear_algebra_upper_triangular_part_3, deterministic(Matrix == [[0.0, 2.0, 3.0], [0.0, 0.0, 6.0], [0.0, 0.0, 0.0]])) :-
		linear_algebra::upper_triangular_part([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], 1, Matrix).

	test(linear_algebra_lower_triangular_part_2, deterministic(Matrix == [[1.0, 0.0, 0.0], [4.0, 5.0, 0.0], [7.0, 8.0, 9.0]])) :-
		linear_algebra::lower_triangular_part([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], Matrix).

	test(linear_algebra_lower_triangular_part_3, deterministic(Matrix == [[1.0, 2.0, 0.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]])) :-
		linear_algebra::lower_triangular_part([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]], 1, Matrix).

	test(linear_algebra_solve_linear_system_3, deterministic((X =~= 1.0, Y =~= 2.0))) :-
		linear_algebra::solve_linear_system([[2.0, 1.0], [1.0, 3.0]], [4.0, 7.0], [X, Y]).

	test(linear_algebra_solve_linear_system_3_singular, error(evaluation_error(zero_divisor))) :-
		linear_algebra::solve_linear_system([[1.0, 2.0], [2.0, 4.0]], [3.0, 6.0], _).

	test(linear_algebra_solve_linear_systems_3, deterministic(Solutions =~= [[1.0, 0.2], [2.0, 1.6]])) :-
		linear_algebra::solve_linear_systems([[2.0, 1.0], [1.0, 3.0]], [[4.0, 2.0], [7.0, 5.0]], Solutions).

	test(linear_algebra_determinant_2, deterministic(Determinant =~= 5.0)) :-
		linear_algebra::determinant([[2.0, 1.0], [1.0, 3.0]], Determinant).

	test(linear_algebra_determinant_2_singular, deterministic(Determinant == 0.0)) :-
		linear_algebra::determinant([[1.0, 2.0], [2.0, 4.0]], Determinant).

	test(linear_algebra_inverse_matrix_2, deterministic((A11 =~= 0.6, A12 =~= -0.2, A21 =~= -0.2, A22 =~= 0.4))) :-
		linear_algebra::inverse_matrix([[2.0, 1.0], [1.0, 3.0]], Inverse),
		linear_algebra::matrix_value(Inverse, 1, 1, A11),
		linear_algebra::matrix_value(Inverse, 1, 2, A12),
		linear_algebra::matrix_value(Inverse, 2, 1, A21),
		linear_algebra::matrix_value(Inverse, 2, 2, A22).

	test(linear_algebra_qr_decomposition_3, deterministic) :-
		Scale is sqrt(0.5),
		NegativeScale is -Scale,
		Norm is sqrt(2.0),
		linear_algebra::qr_decomposition([[1.0, 1.0], [1.0, -1.0]], Orthogonal, UpperTriangular),
		Orthogonal =~= [[Scale, Scale], [Scale, NegativeScale]],
		UpperTriangular =~= [[Norm, 0.0], [0.0, Norm]].

	test(linear_algebra_least_squares_3, deterministic(Solution =~= [1.0, 2.0])) :-
		linear_algebra::least_squares([[1.0, 0.0], [1.0, 1.0], [1.0, 2.0]], [1.0, 3.0, 5.0], Solution).

	test(linear_algebra_least_squares_3_underdetermined, deterministic(Solution == [2.0, 3.0, 0.0])) :-
		linear_algebra::least_squares([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]], [2.0, 3.0], Solution).

	test(linear_algebra_matrix_rank_2, deterministic(Rank == 1)) :-
		linear_algebra::matrix_rank([[1.0, 2.0], [2.0, 4.0], [3.0, 6.0]], Rank).

	test(linear_algebra_matrix_rank_3, deterministic(Rank == 2)) :-
		linear_algebra::matrix_rank([[1.0, 0.0], [0.0, 1.0e-13]], 1.0e-14, Rank).

	test(linear_algebra_matrix_rank_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::matrix_rank([[1.0, 0.0], [0.0, 1.0]], -1.0, _).

	test(linear_algebra_symmetric_eigen_3, deterministic((FirstValue =~= 3.0, SecondValue =~= 1.0, V11 =~= Scale, V12 =~= Scale, V21 =~= Scale, V22 =~= NegativeScale))) :-
		Scale is sqrt(0.5),
		NegativeScale is -Scale,
		linear_algebra::symmetric_eigen([[2.0, 1.0], [1.0, 2.0]], [FirstVector, SecondVector], [FirstValue, SecondValue]),
		FirstVector = [V11, V12],
		SecondVector = [V21, V22].

	test(linear_algebra_symmetric_eigen_3_indefinite, deterministic((FirstValue =~= 1.0, SecondValue =~= -1.0))) :-
		linear_algebra::symmetric_eigen([[0.0, 1.0], [1.0, 0.0]], _Eigenvectors, [FirstValue, SecondValue]).

	test(linear_algebra_symmetric_eigen_4_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::symmetric_eigen([[1.0, 0.0], [0.0, 1.0]], -1.0, _, _).

	test(linear_algebra_symmetric_eigen_5, deterministic((FirstVector == [1.0, 0.0], SecondVector == [0.0, 1.0], FirstValue == 3.0, SecondValue == 1.0))) :-
		linear_algebra::symmetric_eigen([[3.0, 0.0], [0.0, 1.0]], 1.0e-12, 1, [FirstVector, SecondVector], [FirstValue, SecondValue]).

	test(linear_algebra_symmetric_eigen_5_invalid_maximum_iterations, error(domain_error(positive_integer, 0))) :-
		linear_algebra::symmetric_eigen([[1.0, 0.0], [0.0, 1.0]], 1.0e-12, 0, _, _).

	test(linear_algebra_pseudo_inverse_2, deterministic(PseudoInverse == [[1.0, 0.0], [0.0, 1.0], [0.0, 0.0]])) :-
		linear_algebra::pseudo_inverse([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]], PseudoInverse).

	test(linear_algebra_pseudo_inverse_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::pseudo_inverse([[1.0, 0.0], [0.0, 1.0]], -1.0, _).

	test(linear_algebra_null_space_2, deterministic(Basis =~= [[0.0, 0.0, 1.0]])) :-
		linear_algebra::null_space([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0]], Basis).

	test(linear_algebra_null_space_2_full_rank, deterministic(Basis == [])) :-
		linear_algebra::null_space([[1.0, 0.0], [0.0, 1.0]], Basis).

	test(linear_algebra_null_space_3_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		linear_algebra::null_space([[1.0, 0.0], [0.0, 1.0]], -1.0, _).

	test(linear_algebra_matrix_matrix_product_3, deterministic(Product == [[19.0, 22.0], [43.0, 50.0]])) :-
		linear_algebra::matrix_matrix_product([[1.0, 2.0], [3.0, 4.0]], [[5.0, 6.0], [7.0, 8.0]], Product).

	test(linear_algebra_gram_matrix_2, deterministic(GramMatrix == [[5.0, 11.0], [11.0, 25.0]])) :-
		linear_algebra::gram_matrix([[1.0, 2.0], [3.0, 4.0]], GramMatrix).

	test(linear_algebra_matrix_row_means_2, deterministic(Means == [1.5, 3.5])) :-
		linear_algebra::matrix_row_means([[1.0, 2.0], [3.0, 4.0]], Means).

	test(linear_algebra_matrix_column_means_2, deterministic(Means == [2.0, 3.0])) :-
		linear_algebra::matrix_column_means([[1.0, 2.0], [3.0, 4.0]], Means).

	test(linear_algebra_center_gram_matrix_4, deterministic((CenteredGramMatrix == [[0.5, -0.5], [-0.5, 0.5]], RowMeans == [1.5, 3.5], TotalMean == 2.5))) :-
		linear_algebra::center_gram_matrix([[1.0, 2.0], [2.0, 5.0]], CenteredGramMatrix, RowMeans, TotalMean).

	test(linear_algebra_center_gram_vector_4, deterministic(CenteredGramVector == [-1.0, 1.0])) :-
		linear_algebra::center_gram_vector([3.0, 7.0], [1.5, 3.5], 2.5, CenteredGramVector).

	test(linear_algebra_frobenius_norm_2, deterministic(Norm =~= 5.477225575051661)) :-
		linear_algebra::frobenius_norm([[1.0, 2.0], [3.0, 4.0]], Norm).

	test(linear_algebra_solve_lower_triangular_matrix_3, deterministic(Solution == [[1.0, 2.0], [2.0, 5.0]])) :-
		linear_algebra::solve_lower_triangular_matrix([[2.0, 0.0], [3.0, 1.0]], [[2.0, 4.0], [5.0, 11.0]], Solution).

	test(linear_algebra_solve_upper_triangular_3, deterministic((X =~= 1.5, Y =~= 2.0))) :-
		linear_algebra::solve_upper_triangular([[2.0, 1.0], [0.0, 3.0]], [5.0, 6.0], [X, Y]).

	test(linear_algebra_solve_upper_triangular_matrix_3, deterministic(Solution == [[1.5, 2.0], [2.0, 3.0]])) :-
		linear_algebra::solve_upper_triangular_matrix([[2.0, 1.0], [0.0, 3.0]], [[5.0, 7.0], [6.0, 9.0]], Solution).

	test(linear_algebra_backward_substitution_3, deterministic((X =~= 1.0, Y =~= 2.0))) :-
		linear_algebra::backward_substitution([[2.0, 0.0], [1.0, 1.0]], [4.0, 2.0], [X, Y]).

	test(linear_algebra_covariance_matrix_2, deterministic((Covariance11 =~= 10.0, Covariance12 =~= 10.0, Covariance22 =~= 10.0))) :-
		linear_algebra::covariance_matrix([[1.0, 1.0], [3.0, 3.0]], CovarianceMatrix),
		linear_algebra::matrix_value(CovarianceMatrix, 1, 1, Covariance11),
		linear_algebra::matrix_value(CovarianceMatrix, 1, 2, Covariance12),
		linear_algebra::matrix_value(CovarianceMatrix, 2, 2, Covariance22).

	test(linear_algebra_solve_cholesky_3, deterministic((X =~= 1.0, Y =~= 2.0))) :-
		linear_algebra::cholesky_decomposition([[4.0, 2.0], [2.0, 2.0]], CholeskyFactor),
		linear_algebra::solve_cholesky(CholeskyFactor, [8.0, 6.0], [X, Y]).

:- end_object.
