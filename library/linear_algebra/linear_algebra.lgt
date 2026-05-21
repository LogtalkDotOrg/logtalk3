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


:- object(linear_algebra,
	implements(linear_algebra_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-21,
		comment is 'Linear algebra predicates for numeric vectors and matrices implemented without dependencies on machine learning libraries.'
	]).

	:- uses(list, [
		append/3, length/2, nth1/3
	]).

	:- uses(type, [
		check/3
	]).

	:- uses(numberlist, [
		euclidean_norm/2 as numberlist_euclidean_norm/2, rescale/3, scalar_product/3 as numberlist_dot_product/3,
		sum/2 as numberlist_sum/2, chebyshev_norm/2 as numberlist_chebyshev_norm/2,
		manhattan_norm/2 as numberlist_manhattan_norm/2
	]).

	new_vector(Count, Value, Vector) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		check(number, Value, Context),
		new_vector_checked(Count, Value, Vector).

	new_vector_checked(0, _Value, []) :-
		!.
	new_vector_checked(Count, Value, [Value| Values]) :-
		Count > 0,
		RemainingCount is Count - 1,
		new_vector_checked(RemainingCount, Value, Values).

	new_vector_like(Reference, Zeroes) :-
		length(Reference, Count),
		new_vector(Count, 0.0, Zeroes).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	subtract_vectors([], [], []).
	subtract_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 - Value2,
		subtract_vectors(Values1, Values2, Values).

	add_scaled_vector(Vector, Scale, Vector0, UpdatedVector) :-
		context(Context),
		check(number, Scale, Context),
		add_scaled_vector_checked(Vector, Scale, Vector0, UpdatedVector).

	add_scaled_vector_checked([], _Scale, [], []).
	add_scaled_vector_checked([Value| Values], Scale, [Value0| Values0], [UpdatedValue| UpdatedValues]) :-
		UpdatedValue is Value0 + Scale * Value,
		add_scaled_vector_checked(Values, Scale, Values0, UpdatedValues).

	scale_vector(Vector, Scale, ScaledVector) :-
		context(Context),
		check(number, Scale, Context),
		rescale(Vector, Scale, ScaledVector).

	dot_product(Vector1, Vector2, Product) :-
		numberlist_dot_product(Vector1, Vector2, Product).

	euclidean_norm(Vector, Norm) :-
		numberlist_euclidean_norm(Vector, Norm).

	vector_norm(Vector, Order, Norm) :-
		(   Order == inf
		;   Order == infinity
		),
		!,
		numberlist_chebyshev_norm(Vector, Norm).

	vector_norm(Vector, Order, Norm) :-
		context(Context),
		check(number, Order, Context),
		vector_norm_for_order(Vector, Order, Norm).

	vector_norm_for_order(Vector, Order, Norm) :-
		(   Order =:= 1 ->
			numberlist_manhattan_norm(Vector, Norm)
		;   Order =:= 2 ->
			euclidean_norm(Vector, Norm)
		;   Order > 0.0 ->
			vector_p_norm(Vector, Order, Norm)
		;   domain_error(positive_number_or_infinity, Order)
		).

	vector_p_norm([Value| Values], Order, Norm) :-
		Sum0 is abs(Value) ** Order,
		vector_p_norm(Values, Order, Sum0, Norm).

	vector_p_norm([], Order, Sum, Norm) :-
		Norm is Sum ** (1.0 / Order).
	vector_p_norm([Value| Values], Order, Sum0, Norm) :-
		Sum1 is Sum0 + abs(Value) ** Order,
		vector_p_norm(Values, Order, Sum1, Norm).

	basis_vector(Size, Index, Vector) :-
		context(Context),
		check(positive_integer, Size, Context),
		check(positive_integer, Index, Context),
		basis_vector_checked(1, Size, Index, Vector).

	basis_vector_checked(Current, Size, _Index, []) :-
		Current > Size,
		!.
	basis_vector_checked(Index, Size, Index, [1.0| Vector]) :-
		!,
		Next is Index + 1,
		basis_vector_checked(Next, Size, Index, Vector).
	basis_vector_checked(Current, Size, Index, [0.0| Vector]) :-
		Next is Current + 1,
		basis_vector_checked(Next, Size, Index, Vector).

	new_matrix(Rows, Columns, Value, Matrix) :-
		context(Context),
		check(non_negative_integer, Rows, Context),
		check(non_negative_integer, Columns, Context),
		check(number, Value, Context),
		new_matrix_checked(Rows, Columns, Value, Matrix).

	new_matrix_checked(0, _Columns, _Value, []) :-
		!.
	new_matrix_checked(Rows, Columns, Value, [Row| Matrix]) :-
		Rows > 0,
		new_vector_checked(Columns, Value, Row),
		NextRows is Rows - 1,
		new_matrix_checked(NextRows, Columns, Value, Matrix).

	identity_matrix(Size, Matrix) :-
		context(Context),
		check(non_negative_integer, Size, Context),
		identity_matrix_checked(1, Size, Matrix).

	identity_matrix_checked(Index, Size, []) :-
		Index > Size,
		!.
	identity_matrix_checked(Index, Size, [Row| Matrix]) :-
		basis_vector_checked(1, Size, Index, Row),
		NextIndex is Index + 1,
		identity_matrix_checked(NextIndex, Size, Matrix).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	matrix_matrix_product(Matrix1, Matrix2, Product) :-
		transpose_matrix(Matrix2, TransposedMatrix2),
		matrix_matrix_product_rows(Matrix1, TransposedMatrix2, Product).

	gram_matrix(Rows, GramMatrix) :-
		transpose_matrix(Rows, Columns),
		matrix_matrix_product(Rows, Columns, GramMatrix).

	matrix_row_means([], []).
	matrix_row_means([Row| Rows], [Mean| Means]) :-
		mean_list(Row, Mean),
		matrix_row_means(Rows, Means).

	matrix_column_means(Matrix, Means) :-
		transpose_matrix(Matrix, Columns),
		matrix_row_means(Columns, Means).

	center_gram_matrix(GramMatrix, CenteredGramMatrix, RowMeans, TotalMean) :-
		matrix_row_means(GramMatrix, RowMeans),
		mean_list(RowMeans, TotalMean),
		center_gram_rows(GramMatrix, RowMeans, RowMeans, TotalMean, CenteredGramMatrix).

	center_gram_rows([], [], _AllRowMeans, _TotalMean, []).
	center_gram_rows([GramRow| GramMatrix], [RowMean| RowMeans], AllRowMeans, TotalMean, [CenteredGramRow| CenteredGramMatrix]) :-
		center_gram_row(GramRow, RowMean, AllRowMeans, TotalMean, CenteredGramRow),
		center_gram_rows(GramMatrix, RowMeans, AllRowMeans, TotalMean, CenteredGramMatrix).

	center_gram_row([], _RowMean, [], _TotalMean, []).
	center_gram_row([GramValue| GramValues], RowMean, [ColumnMean| ColumnMeans], TotalMean, [CenteredGramValue| CenteredGramValues]) :-
		CenteredGramValue is GramValue - RowMean - ColumnMean + TotalMean,
		center_gram_row(GramValues, RowMean, ColumnMeans, TotalMean, CenteredGramValues).

	center_gram_vector(GramVector, RowMeans, TotalMean, CenteredGramVector) :-
		mean_list(GramVector, GramMean),
		center_gram_vector(GramVector, RowMeans, GramMean, TotalMean, CenteredGramVector).

	center_gram_vector([], [], _GramMean, _TotalMean, []).
	center_gram_vector([GramValue| GramValues], [RowMean| RowMeans], GramMean, TotalMean, [CenteredGramValue| CenteredGramValues]) :-
		CenteredGramValue is GramValue - RowMean - GramMean + TotalMean,
		center_gram_vector(GramValues, RowMeans, GramMean, TotalMean, CenteredGramValues).

	matrix_matrix_product_rows([], _TransposedMatrix2, []).
	matrix_matrix_product_rows([Row| Rows], TransposedMatrix2, [ProductRow| ProductRows]) :-
		matrix_row_products(TransposedMatrix2, Row, ProductRow),
		matrix_matrix_product_rows(Rows, TransposedMatrix2, ProductRows).

	matrix_row_products([], _Row, []).
	matrix_row_products([Column| Columns], Row, [Value| Values]) :-
		dot_product(Row, Column, Value),
		matrix_row_products(Columns, Row, Values).

	outer_product([], _Vector, []).
	outer_product([Value| Values], Vector, [Row| Rows]) :-
		rescale(Vector, Value, Row),
		outer_product(Values, Vector, Rows).

	add_matrices([], [], []).
	add_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		add_vectors(Row1, Row2, Row),
		add_matrices(Rows1, Rows2, Rows).

	subtract_matrices([], [], []).
	subtract_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		subtract_vectors(Row1, Row2, Row),
		subtract_matrices(Rows1, Rows2, Rows).

	scale_matrix([], _Scale, []).
	scale_matrix([Row| Rows], Scale, [ScaledRow| ScaledRows]) :-
		rescale(Row, Scale, ScaledRow),
		scale_matrix(Rows, Scale, ScaledRows).

	frobenius_norm(Matrix, Norm) :-
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		frobenius_sum_squares(Matrix, 0.0, SumSquares),
		Norm is sqrt(SumSquares).

	frobenius_sum_squares([], SumSquares, SumSquares).
	frobenius_sum_squares([Row| Rows], SumSquares0, SumSquares) :-
		sum_squares(Row, RowSumSquares),
		SumSquares1 is SumSquares0 + RowSumSquares,
		frobenius_sum_squares(Rows, SumSquares1, SumSquares).

	transpose_matrix([], []) :-
		!.
	transpose_matrix([[]| _], []) :-
		!.
	transpose_matrix(Matrix, [Column| Columns]) :-
		extract_first_column(Matrix, Column, RemainingMatrix),
		transpose_matrix(RemainingMatrix, Columns).

	extract_first_column([], [], []).
	extract_first_column([[Head| Tail]| Rows], [Head| Column], [Tail| RemainingRows]) :-
		extract_first_column(Rows, Column, RemainingRows).

	vector_value(Vector, Index, Value) :-
		context(Context),
		check(positive_integer, Index, Context),
		nth1(Index, Vector, Value).

	matrix_row(Matrix, RowIndex, Row) :-
		context(Context),
		check(positive_integer, RowIndex, Context),
		nth1(RowIndex, Matrix, Row).

	matrix_column(Matrix, ColumnIndex, Column) :-
		context(Context),
		check(positive_integer, ColumnIndex, Context),
		matrix_column_checked(Matrix, ColumnIndex, Column).

	matrix_column_checked([], _ColumnIndex, []).
	matrix_column_checked([Row| Rows], ColumnIndex, [Value| Values]) :-
		nth1(ColumnIndex, Row, Value),
		matrix_column_checked(Rows, ColumnIndex, Values).

	matrix_value(Matrix, RowIndex, ColumnIndex, Value) :-
		context(Context),
		check(positive_integer, RowIndex, Context),
		check(positive_integer, ColumnIndex, Context),
		nth1(RowIndex, Matrix, Row),
		nth1(ColumnIndex, Row, Value).

	matrix_diagonal(Matrix, Diagonal) :-
		matrix_diagonal(Matrix, 0, Diagonal).

	matrix_diagonal(Matrix, Offset, Diagonal) :-
		context(Context),
		check(integer, Offset, Context),
		require_rectangular_matrix(Matrix, RowCount, ColumnCount),
		diagonal_start_indices(Offset, RowIndex, ColumnIndex),
		matrix_diagonal_from(RowCount, ColumnCount, Matrix, RowIndex, ColumnIndex, Diagonal).

	matrix_diagonal_from(RowCount, _ColumnCount, _Matrix, RowIndex, _ColumnIndex, []) :-
		RowIndex > RowCount,
		!.
	matrix_diagonal_from(_RowCount, ColumnCount, _Matrix, _RowIndex, ColumnIndex, []) :-
		ColumnIndex > ColumnCount,
		!.
	matrix_diagonal_from(RowCount, ColumnCount, Matrix, RowIndex, ColumnIndex, [Value| Values]) :-
		matrix_value(Matrix, RowIndex, ColumnIndex, Value),
		NextRowIndex is RowIndex + 1,
		NextColumnIndex is ColumnIndex + 1,
		matrix_diagonal_from(RowCount, ColumnCount, Matrix, NextRowIndex, NextColumnIndex, Values).

	diagonal_matrix(Diagonal, Matrix) :-
		diagonal_matrix(Diagonal, 0, Matrix).

	diagonal_matrix(Diagonal, Offset, Matrix) :-
		context(Context),
		check(integer, Offset, Context),
		length(Diagonal, DiagonalLength),
		minimum_diagonal_matrix_size(DiagonalLength, Offset, Size),
		diagonal_matrix(Diagonal, Offset, Size, Matrix).

	diagonal_matrix(Diagonal, Offset, Size, Matrix) :-
		context(Context),
		check(integer, Offset, Context),
		check(non_negative_integer, Size, Context),
		length(Diagonal, DiagonalLength),
		minimum_diagonal_matrix_size(DiagonalLength, Offset, MinimumSize),
		(   Size >= MinimumSize ->
			diagonal_matrix_rows(1, Size, Diagonal, Offset, Matrix)
		;   domain_error(minimum_matrix_size(MinimumSize), Size)
		).

	diagonal_matrix_rows(RowIndex, Size, _Diagonal, _Offset, []) :-
		RowIndex > Size,
		!.
	diagonal_matrix_rows(RowIndex, Size, Diagonal, Offset, [Row| Rows]) :-
		diagonal_matrix_row(1, Size, RowIndex, Diagonal, Offset, Row),
		NextRowIndex is RowIndex + 1,
		diagonal_matrix_rows(NextRowIndex, Size, Diagonal, Offset, Rows).

	diagonal_matrix_row(ColumnIndex, Size, _RowIndex, _Diagonal, _Offset, []) :-
		ColumnIndex > Size,
		!.
	diagonal_matrix_row(ColumnIndex, Size, RowIndex, Diagonal, Offset, [Value| Row]) :-
		(   diagonal_entry_value(RowIndex, ColumnIndex, Diagonal, Offset, EntryValue) ->
			Value = EntryValue
		;   Value = 0.0
		),
		NextColumnIndex is ColumnIndex + 1,
		diagonal_matrix_row(NextColumnIndex, Size, RowIndex, Diagonal, Offset, Row).

	diagonal_entry_value(RowIndex, ColumnIndex, Diagonal, Offset, Value) :-
		ColumnIndex =:= RowIndex + Offset,
		diagonal_value_index(RowIndex, Offset, DiagonalIndex),
		nth1(DiagonalIndex, Diagonal, Value).

	diagonal_value_index(RowIndex, Offset, DiagonalIndex) :-
		(   Offset >= 0 ->
			DiagonalIndex is RowIndex
		;   DiagonalIndex is RowIndex + Offset
		).

	matrix_trace(Matrix, Trace) :-
		matrix_diagonal(Matrix, Diagonal),
		numberlist_sum(Diagonal, Trace).

	shift_matrix_diagonal(Matrix, Shift, ShiftedMatrix) :-
		context(Context),
		check(number, Shift, Context),
		shift_matrix_diagonal_checked(Matrix, Shift, 1, ShiftedMatrix).

	shift_matrix_diagonal_checked([], _Shift, _Index, []).
	shift_matrix_diagonal_checked([Row| Rows], Shift, Index, [ShiftedRow| ShiftedRows]) :-
		shift_row_diagonal(Row, Shift, 1, Index, ShiftedRow),
		NextIndex is Index + 1,
		shift_matrix_diagonal_checked(Rows, Shift, NextIndex, ShiftedRows).

	shift_row_diagonal([], _Shift, _ColumnIndex, _DiagonalIndex, []).
	shift_row_diagonal([Value| Values], Shift, DiagonalIndex, DiagonalIndex, [ShiftedValue| ShiftedValues]) :-
		!,
		ShiftedValue is Value + Shift,
		NextColumnIndex is DiagonalIndex + 1,
		shift_row_diagonal(Values, Shift, NextColumnIndex, DiagonalIndex, ShiftedValues).
	shift_row_diagonal([Value| Values], Shift, ColumnIndex, DiagonalIndex, [Value| ShiftedValues]) :-
		NextColumnIndex is ColumnIndex + 1,
		shift_row_diagonal(Values, Shift, NextColumnIndex, DiagonalIndex, ShiftedValues).

	upper_triangular_part(Matrix, UpperTriangular) :-
		upper_triangular_part(Matrix, 0, UpperTriangular).

	upper_triangular_part(Matrix, Offset, UpperTriangular) :-
		context(Context),
		check(integer, Offset, Context),
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		extract_upper_triangular_rows(Matrix, Offset, 1, UpperTriangular).

	extract_upper_triangular_rows([], _Offset, _RowIndex, []).
	extract_upper_triangular_rows([Row| Rows], Offset, RowIndex, [UpperRow| UpperRows]) :-
		extract_upper_triangular_row(Row, Offset, RowIndex, 1, UpperRow),
		NextRowIndex is RowIndex + 1,
		extract_upper_triangular_rows(Rows, Offset, NextRowIndex, UpperRows).

	extract_upper_triangular_row([], _Offset, _RowIndex, _ColumnIndex, []).
	extract_upper_triangular_row([Value| Values], Offset, RowIndex, ColumnIndex, [UpperValue| UpperValues]) :-
		(   ColumnIndex - RowIndex >= Offset ->
			UpperValue = Value
		;   UpperValue = 0.0
		),
		NextColumnIndex is ColumnIndex + 1,
		extract_upper_triangular_row(Values, Offset, RowIndex, NextColumnIndex, UpperValues).

	lower_triangular_part(Matrix, LowerTriangular) :-
		lower_triangular_part(Matrix, 0, LowerTriangular).

	lower_triangular_part(Matrix, Offset, LowerTriangular) :-
		context(Context),
		check(integer, Offset, Context),
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		extract_lower_triangular_rows(Matrix, Offset, 1, LowerTriangular).

	extract_lower_triangular_rows([], _Offset, _RowIndex, []).
	extract_lower_triangular_rows([Row| Rows], Offset, RowIndex, [LowerRow| LowerRows]) :-
		extract_lower_triangular_row(Row, Offset, RowIndex, 1, LowerRow),
		NextRowIndex is RowIndex + 1,
		extract_lower_triangular_rows(Rows, Offset, NextRowIndex, LowerRows).

	extract_lower_triangular_row([], _Offset, _RowIndex, _ColumnIndex, []).
	extract_lower_triangular_row([Value| Values], Offset, RowIndex, ColumnIndex, [LowerValue| LowerValues]) :-
		(   ColumnIndex - RowIndex =< Offset ->
			LowerValue = Value
		;   LowerValue = 0.0
		),
		NextColumnIndex is ColumnIndex + 1,
		extract_lower_triangular_row(Values, Offset, RowIndex, NextColumnIndex, LowerValues).

	solve_linear_system(Matrix, Values, Solution) :-
		require_square_matrix(Matrix, Size),
		require_vector_length(Values, Size),
		vector_as_column_matrix(Values, RightHandSideMatrix),
		solve_linear_systems(Matrix, RightHandSideMatrix, SolutionMatrix),
		matrix_column(SolutionMatrix, 1, Solution).

	solve_linear_systems(Matrix, RightHandSideMatrix, Solutions) :-
		require_square_matrix(Matrix, Size),
		require_matrix_row_count(RightHandSideMatrix, Size),
		linear_system_scale(Matrix, RightHandSideMatrix, Scale),
		triangularize_system(Matrix, RightHandSideMatrix, Scale, _Sign, UpperMatrix, UpperRightHandSideMatrix, Status),
		(   Status == ok ->
			solve_upper_triangular_matrix(UpperMatrix, UpperRightHandSideMatrix, Solutions)
		;   evaluation_error(zero_divisor)
		).

	determinant(Matrix, Determinant) :-
		require_square_matrix(Matrix, Size),
		new_matrix(Size, 0, 0.0, EmptyRightHandSideMatrix),
		matrix_scale(Matrix, Scale),
		triangularize_system(Matrix, EmptyRightHandSideMatrix, Scale, Sign, UpperMatrix, _UpperRightHandSideMatrix, Status),
		(   Status == ok ->
			diagonal_product(UpperMatrix, Product),
			Determinant is Sign * Product
		;   Determinant = 0.0
		).

	inverse_matrix(Matrix, Inverse) :-
		require_square_matrix(Matrix, Size),
		identity_matrix(Size, Identity),
		solve_linear_systems(Matrix, Identity, Inverse).

	qr_decomposition(Matrix, Orthogonal, UpperTriangular) :-
		default_numerical_tolerance(Tolerance),
		require_rectangular_matrix(Matrix, RowCount, _ColumnCount),
		transpose_matrix(Matrix, Columns),
		initialize_qr_candidates(Columns, 1, Candidates),
		process_ordered_qr_candidates(Candidates, Tolerance, SelectedColumns, ProcessedColumns),
		selected_column_count(SelectedColumns, Rank),
		orthogonal_matrix_from_selected(SelectedColumns, RowCount, Orthogonal),
		upper_triangular_factor(ProcessedColumns, Rank, UpperTriangular).

	least_squares(Matrix, Values, Solution) :-
		default_numerical_tolerance(Tolerance),
		require_rectangular_matrix(Matrix, RowCount, ColumnCount),
		require_vector_length(Values, RowCount),
		transpose_matrix(Matrix, Columns),
		initialize_qr_candidates(Columns, 1, Candidates),
		process_pivoted_qr_candidates(Candidates, Tolerance, SelectedColumns, ProcessedColumns),
		selected_column_count(SelectedColumns, Rank),
		(   Rank =:= 0 ->
			new_vector(ColumnCount, 0.0, Solution)
		;   q_columns_from_selected(SelectedColumns, OrthogonalColumns),
			q_transpose_times_vector(OrthogonalColumns, Values, OrthogonalValues),
			upper_triangular_factor(ProcessedColumns, Rank, UpperTriangularFactor),
			leading_square_factor(UpperTriangularFactor, Rank, LeadingFactor),
			solve_upper_triangular(LeadingFactor, OrthogonalValues, PivotedLeadingSolution),
			pad_solution(PivotedLeadingSolution, ColumnCount, PivotedSolution),
			processed_indices(ProcessedColumns, Permutation),
			permuted_solution_to_original(Permutation, PivotedSolution, Solution)
		).

	matrix_rank(Matrix, Rank) :-
		default_numerical_tolerance(Tolerance),
		matrix_rank(Matrix, Tolerance, Rank).

	matrix_rank(Matrix, Tolerance, Rank) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		transpose_matrix(Matrix, Columns),
		initialize_qr_candidates(Columns, 1, Candidates),
		process_pivoted_qr_candidates(Candidates, Tolerance, SelectedColumns, _ProcessedColumns),
		selected_column_count(SelectedColumns, Rank).

	symmetric_eigen(Matrix, Eigenvectors, Eigenvalues) :-
		default_numerical_tolerance(Tolerance),
		symmetric_eigen(Matrix, Tolerance, Eigenvectors, Eigenvalues).

	symmetric_eigen(Matrix, Tolerance, Eigenvectors, Eigenvalues) :-
		default_spectral_maximum_iterations(MaximumIterations),
		symmetric_eigen(Matrix, Tolerance, MaximumIterations, Eigenvectors, Eigenvalues).

	symmetric_eigen(Matrix, Tolerance, MaximumIterations, Eigenvectors, Eigenvalues) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		check(positive_integer, MaximumIterations, Context),
		require_square_matrix(Matrix, Size),
		require_symmetric_matrix(Matrix, Tolerance),
		(   Size =:= 0 ->
			Eigenvectors = [],
			Eigenvalues = []
		;   initial_vectors(Size, InitialVectors),
			extract_symmetric_eigenpairs(Matrix, Size, InitialVectors, Tolerance, MaximumIterations, [], [], Eigenvectors0, Eigenvalues0),
			sort_eigenpairs_descending(Eigenvectors0, Eigenvalues0, Eigenvectors, Eigenvalues)
		).

	pseudo_inverse(Matrix, PseudoInverse) :-
		default_numerical_tolerance(Tolerance),
		pseudo_inverse(Matrix, Tolerance, PseudoInverse).

	pseudo_inverse(Matrix, Tolerance, PseudoInverse) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		transpose_matrix(Matrix, Transpose),
		matrix_matrix_product(Transpose, Matrix, GramMatrix),
		symmetric_eigen(GramMatrix, Tolerance, Eigenvectors, Eigenvalues),
		symmetric_matrix_pseudo_inverse(Eigenvectors, Eigenvalues, Tolerance, GramPseudoInverse),
		matrix_matrix_product(GramPseudoInverse, Transpose, PseudoInverse).

	null_space(Matrix, Basis) :-
		default_numerical_tolerance(Tolerance),
		null_space(Matrix, Tolerance, Basis).

	null_space(Matrix, Tolerance, Basis) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		require_rectangular_matrix(Matrix, _RowCount, _ColumnCount),
		transpose_matrix(Matrix, Transpose),
		matrix_matrix_product(Transpose, Matrix, GramMatrix),
		symmetric_eigen(GramMatrix, Tolerance, Eigenvectors, Eigenvalues),
		collect_null_space_basis(Eigenvectors, Eigenvalues, Tolerance, Basis).

	extract_symmetric_eigenpairs(_Matrix, 0, _InitialVectors, _Tolerance, _MaximumIterations, Eigenvectors, Eigenvalues, Eigenvectors, Eigenvalues) :-
		!.
	extract_symmetric_eigenpairs(Matrix, Remaining, InitialVectors, Tolerance, MaximumIterations, Eigenvectors0, Eigenvalues0, Eigenvectors, Eigenvalues) :-
		principal_symmetric_component(Matrix, Eigenvectors0, InitialVectors, Tolerance, MaximumIterations, Eigenvalue, Eigenvector),
		NegativeEigenvalue is -Eigenvalue,
		add_scaled_outer_product(Eigenvector, NegativeEigenvalue, Matrix, DeflatedMatrix),
		NextRemaining is Remaining - 1,
		extract_symmetric_eigenpairs(DeflatedMatrix, NextRemaining, InitialVectors, Tolerance, MaximumIterations, [Eigenvector| Eigenvectors0], [Eigenvalue| Eigenvalues0], Eigenvectors, Eigenvalues).

	principal_symmetric_component(Matrix, SelectedEigenvectors, InitialVectors, Tolerance, MaximumIterations, Eigenvalue, Eigenvector) :-
		length(Matrix, Size),
		new_vector(Size, 0.0, ZeroVector),
		principal_symmetric_component_candidates(Matrix, SelectedEigenvectors, InitialVectors, Tolerance, MaximumIterations, false, 0.0, ZeroVector, Eigenvalue, Eigenvector).

	principal_symmetric_component_candidates(_Matrix, _SelectedEigenvectors, [], _Tolerance, _MaximumIterations, _Found, BestEigenvalue, BestEigenvector, BestEigenvalue, BestEigenvector) :-
		!.
	principal_symmetric_component_candidates(Matrix, SelectedEigenvectors, [InitialVector| InitialVectors], Tolerance, MaximumIterations, Found0, BestEigenvalue0, BestEigenvector0, BestEigenvalue, BestEigenvector) :-
		(   orthogonal_initial_vector(InitialVector, SelectedEigenvectors, Tolerance, NormalizedInitial) ->
			iterate_symmetric_component(Matrix, SelectedEigenvectors, Tolerance, MaximumIterations, 0, NormalizedInitial, _CandidateEigenvalue0, CandidateEigenvector0),
			stabilized_orthogonal_vector(CandidateEigenvector0, SelectedEigenvectors, Tolerance, CandidateEigenvector),
			rayleigh_quotient(Matrix, CandidateEigenvector, CandidateEigenvalue),
			(   better_eigenpair(Found0, CandidateEigenvalue, BestEigenvalue0) ->
				Found1 = true,
				BestEigenvalue1 = CandidateEigenvalue,
				BestEigenvector1 = CandidateEigenvector
			;   Found1 = Found0,
				BestEigenvalue1 = BestEigenvalue0,
				BestEigenvector1 = BestEigenvector0
			)
		;   Found1 = Found0,
			BestEigenvalue1 = BestEigenvalue0,
			BestEigenvector1 = BestEigenvector0
		),
		principal_symmetric_component_candidates(Matrix, SelectedEigenvectors, InitialVectors, Tolerance, MaximumIterations, Found1, BestEigenvalue1, BestEigenvector1, BestEigenvalue, BestEigenvector).

	orthogonal_initial_vector(InitialVector, SelectedEigenvectors, Tolerance, NormalizedInitial) :-
		orthogonalize_against_basis(SelectedEigenvectors, InitialVector, OrthogonalInitial0),
		euclidean_norm(OrthogonalInitial0, Norm),
		Norm > Tolerance,
		normalize_vector(OrthogonalInitial0, Tolerance, NormalizedInitial0),
		stabilize_vector_sign(NormalizedInitial0, Tolerance, NormalizedInitial).

	iterate_symmetric_component(Matrix, SelectedEigenvectors, Tolerance, MaximumIterations, Iteration, Vector0, Eigenvalue, Eigenvector) :-
		matrix_vector_product(Matrix, Vector0, Product0),
		orthogonalize_against_basis(SelectedEigenvectors, Product0, Product),
		euclidean_norm(Product, Norm),
		(   Norm =< Tolerance ->
			Eigenvalue = 0.0,
			Eigenvector = Vector0
		;   Scale is 1.0 / Norm,
			scale_vector(Product, Scale, Vector1),
			stabilized_orthogonal_vector(Vector1, SelectedEigenvectors, Tolerance, StableVector),
			difference_norm(StableVector, Vector0, Delta),
			(   (Delta =< Tolerance ; Iteration >= MaximumIterations) ->
				rayleigh_quotient(Matrix, StableVector, Eigenvalue),
				Eigenvector = StableVector
			;   NextIteration is Iteration + 1,
				iterate_symmetric_component(Matrix, SelectedEigenvectors, Tolerance, MaximumIterations, NextIteration, StableVector, Eigenvalue, Eigenvector)
			)
		).

	rayleigh_quotient(Matrix, Vector, Eigenvalue) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, Eigenvalue).

	stabilized_orthogonal_vector(Vector, SelectedEigenvectors, Tolerance, StableVector) :-
		orthogonalize_against_basis(SelectedEigenvectors, Vector, OrthogonalVector0),
		normalize_vector(OrthogonalVector0, Tolerance, OrthogonalVector1),
		stabilize_vector_sign(OrthogonalVector1, Tolerance, StableVector).

	better_eigenpair(false, _CandidateEigenvalue, _BestEigenvalue).
	better_eigenpair(true, CandidateEigenvalue, BestEigenvalue) :-
		abs(CandidateEigenvalue) > abs(BestEigenvalue).

	orthogonalize_against_basis([], Vector, Vector).
	orthogonalize_against_basis([Basis| Bases], Vector0, Vector) :-
		dot_product(Basis, Vector0, Coefficient),
		NegativeCoefficient is -Coefficient,
		add_scaled_vector(Basis, NegativeCoefficient, Vector0, Vector1),
		orthogonalize_against_basis(Bases, Vector1, Vector).

	collect_null_space_basis([], [], _Tolerance, []).
	collect_null_space_basis([Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues], Tolerance, Basis) :-
		(   abs(Eigenvalue) =< Tolerance ->
			Basis = [Eigenvector| Rest]
		;   Basis = Rest
		),
		collect_null_space_basis(Eigenvectors, Eigenvalues, Tolerance, Rest).

	symmetric_matrix_pseudo_inverse([], [], _Tolerance, []) :-
		!.
	symmetric_matrix_pseudo_inverse(Eigenvectors, Eigenvalues, Tolerance, PseudoInverse) :-
		inverse_eigenvalues(Eigenvalues, Tolerance, InverseEigenvalues),
		diagonal_matrix(InverseEigenvalues, InverseDiagonal),
		columns_to_rows(Eigenvectors, EigenvectorMatrix),
		transpose_matrix(EigenvectorMatrix, EigenvectorTranspose),
		matrix_matrix_product(EigenvectorMatrix, InverseDiagonal, LeftProduct),
		matrix_matrix_product(LeftProduct, EigenvectorTranspose, PseudoInverse).

	inverse_eigenvalues([], _Tolerance, []).
	inverse_eigenvalues([Eigenvalue| Eigenvalues], Tolerance, [InverseEigenvalue| InverseEigenvalues]) :-
		(   Eigenvalue > Tolerance ->
			InverseEigenvalue is 1.0 / Eigenvalue
		;   InverseEigenvalue = 0.0
		),
		inverse_eigenvalues(Eigenvalues, Tolerance, InverseEigenvalues).

	sort_eigenpairs_descending(Eigenvectors0, Eigenvalues0, Eigenvectors, Eigenvalues) :-
		zip_eigenpairs(Eigenvectors0, Eigenvalues0, Eigenpairs0),
		insertion_sort_eigenpairs(Eigenpairs0, [], Eigenpairs),
		unzip_eigenpairs(Eigenpairs, Eigenvectors, Eigenvalues).

	zip_eigenpairs([], [], []).
	zip_eigenpairs([Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues], [eigenpair(Eigenvalue, Eigenvector)| Eigenpairs]) :-
		zip_eigenpairs(Eigenvectors, Eigenvalues, Eigenpairs).

	insertion_sort_eigenpairs([], Eigenpairs, Eigenpairs).
	insertion_sort_eigenpairs([Eigenpair| Eigenpairs0], Sorted0, Sorted) :-
		insert_eigenpair(Sorted0, Eigenpair, Sorted1),
		insertion_sort_eigenpairs(Eigenpairs0, Sorted1, Sorted).

	insert_eigenpair([], Eigenpair, [Eigenpair]).
	insert_eigenpair([eigenpair(NextEigenvalue, NextEigenvector)| Eigenpairs], eigenpair(Eigenvalue, Eigenvector), [eigenpair(Eigenvalue, Eigenvector), eigenpair(NextEigenvalue, NextEigenvector)| Eigenpairs]) :-
		Eigenvalue >= NextEigenvalue,
		!.
	insert_eigenpair([NextEigenpair| Eigenpairs0], Eigenpair, [NextEigenpair| Eigenpairs]) :-
		insert_eigenpair(Eigenpairs0, Eigenpair, Eigenpairs).

	unzip_eigenpairs([], [], []).
	unzip_eigenpairs([eigenpair(Eigenvalue, Eigenvector)| Eigenpairs], [Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues]) :-
		unzip_eigenpairs(Eigenpairs, Eigenvectors, Eigenvalues).

	initial_vectors(Size, [InitialVector| BasisVectors]) :-
		initial_vector(Size, InitialVector),
		basis_initial_vectors(1, Size, BasisVectors).

	initial_vector(0, []) :-
		!.
	initial_vector(Size, [1.0| Vector]) :-
		Size > 0,
		NextSize is Size - 1,
		initial_vector(NextSize, Vector).

	basis_initial_vectors(Index, Size, []) :-
		Index > Size,
		!.
	basis_initial_vectors(Index, Size, [BasisVector| BasisVectors]) :-
		basis_vector(Size, Index, BasisVector),
		NextIndex is Index + 1,
		basis_initial_vectors(NextIndex, Size, BasisVectors).

	default_numerical_tolerance(1.0e-12).

	mean_list(Values, Mean) :-
		length(Values, Count),
		(   Count > 0 ->
			numberlist_sum(Values, Sum),
			Mean is Sum / Count
		;   domain_error(minimum_number_of_values(1), Values)
		).

	minimum_diagonal_matrix_size(DiagonalLength, Offset, MinimumSize) :-
		MinimumSize is DiagonalLength + abs(Offset).

	diagonal_start_indices(Offset, RowIndex, ColumnIndex) :-
		(   Offset >= 0 ->
			RowIndex = 1,
			ColumnIndex is 1 + Offset
		;   RowIndex is 1 - Offset,
			ColumnIndex = 1
		).

	default_spectral_maximum_iterations(1000).

	normalize_vector(Vector, NormalizedVector) :-
		default_numerical_tolerance(Tolerance),
		normalize_vector(Vector, Tolerance, NormalizedVector).

	normalize_vector(Vector, Tolerance, NormalizedVector) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		euclidean_norm(Vector, Norm),
		(   Norm =< Tolerance ->
			NormalizedVector = Vector
		;   rescale(Vector, 1.0 / Norm, NormalizedVector)
		).

	difference_norm(Vector1, Vector2, Norm) :-
		subtract_vectors(Vector1, Vector2, Difference),
		euclidean_norm(Difference, Norm).

	stabilize_vector_sign(Vector, StableVector) :-
		default_numerical_tolerance(Tolerance),
		stabilize_vector_sign(Vector, Tolerance, StableVector).

	stabilize_vector_sign(Vector, Tolerance, StableVector) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		(   first_significant_component(Vector, Tolerance, First),
			First < 0.0 ->
			rescale(Vector, -1.0, StableVector)
		;   StableVector = Vector
		).

	first_significant_component(Vector, First) :-
		default_numerical_tolerance(Tolerance),
		first_significant_component(Vector, Tolerance, First).

	first_significant_component(Vector, Tolerance, First) :-
		context(Context),
		check(non_negative_number, Tolerance, Context),
		first_significant_component_at_tolerance(Vector, Tolerance, First).

	first_significant_component_at_tolerance([Value| _Values], Tolerance, Value) :-
		abs(Value) > Tolerance,
		!.
	first_significant_component_at_tolerance([_Value| Values], Tolerance, First) :-
		first_significant_component_at_tolerance(Values, Tolerance, First).
	first_significant_component_at_tolerance([], _Tolerance, 0.0).

	add_scaled_outer_product(Vector, Scale, Matrix0, Matrix) :-
		context(Context),
		check(number, Scale, Context),
		add_scaled_outer_product_checked(Vector, Vector, Scale, Matrix0, Matrix).

	add_scaled_outer_product_checked([], _Vector, _Scale, [], []).
	add_scaled_outer_product_checked([Value| Values], Vector, Scale, [Row0| Rows0], [Row| Rows]) :-
		ScaledValue is Scale * Value,
		add_scaled_vector_checked(Vector, ScaledValue, Row0, Row),
		add_scaled_outer_product_checked(Values, Vector, Scale, Rows0, Rows).

	covariance_matrix([FirstRow| Rows], CovarianceMatrix) :-
		length([FirstRow| Rows], Count),
		(   Count > 1 ->
			length(FirstRow, FeatureCount),
			new_matrix(FeatureCount, FeatureCount, 0.0, ZeroMatrix),
			accumulate_outer_products([FirstRow| Rows], ZeroMatrix, SumMatrix),
			Scale is 1.0 / (Count - 1),
			scale_matrix(SumMatrix, Scale, CovarianceMatrix)
		;   domain_error(minimum_number_of_rows(2), [FirstRow| Rows])
		).
	covariance_matrix([], _CovarianceMatrix) :-
		domain_error(minimum_number_of_rows(2), []).

	accumulate_outer_products([], Matrix, Matrix).
	accumulate_outer_products([Row| Rows], Matrix0, Matrix) :-
		outer_product(Row, Row, Outer),
		add_matrices(Matrix0, Outer, Matrix1),
		accumulate_outer_products(Rows, Matrix1, Matrix).

	cholesky_decomposition(Matrix, CholeskyFactor) :-
		length(Matrix, Size),
		cholesky_decomposition(1, Size, Matrix, [], CholeskyFactor).

	cholesky_decomposition(Index, Size, _Matrix, CholeskyFactor, CholeskyFactor) :-
		Index > Size,
		!.
	cholesky_decomposition(Index, Size, Matrix, PreviousRows, CholeskyFactor) :-
		nth1(Index, Matrix, MatrixRow),
		cholesky_row(1, Index, Size, Matrix, MatrixRow, PreviousRows, [], CholeskyRow),
		append(PreviousRows, [CholeskyRow], NextRows),
		NextIndex is Index + 1,
		cholesky_decomposition(NextIndex, Size, Matrix, NextRows, CholeskyFactor).

	cholesky_row(ColumnIndex, RowIndex, Size, _Matrix, _MatrixRow, _PreviousRows, Prefix, CholeskyRow) :-
		ColumnIndex > RowIndex,
		!,
		Remaining is Size - RowIndex,
		new_vector(Remaining, 0.0, Zeroes),
		append(Prefix, Zeroes, CholeskyRow).
	cholesky_row(ColumnIndex, RowIndex, Size, Matrix, MatrixRow, PreviousRows, Prefix, CholeskyRow) :-
		nth1(ColumnIndex, MatrixRow, MatrixValue),
		(   ColumnIndex =:= RowIndex ->
			sum_squares(Prefix, Correction),
			DiagonalValue0 is MatrixValue - Correction,
			(   DiagonalValue0 > 1.0e-12 ->
				DiagonalValue is sqrt(DiagonalValue0)
			;   domain_error(positive_definite_matrix, Matrix)
			),
			append(Prefix, [DiagonalValue], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, Matrix, MatrixRow, PreviousRows, NextPrefix, CholeskyRow)
		;   nth1(ColumnIndex, PreviousRows, PreviousRow),
			prefix_dot(Prefix, PreviousRow, 0.0, Correction),
			nth1(ColumnIndex, PreviousRow, Diagonal),
			Entry is (MatrixValue - Correction) / Diagonal,
			append(Prefix, [Entry], NextPrefix),
			NextColumnIndex is ColumnIndex + 1,
			cholesky_row(NextColumnIndex, RowIndex, Size, Matrix, MatrixRow, PreviousRows, NextPrefix, CholeskyRow)
		).

	solve_cholesky(CholeskyFactor, Values, Solution) :-
		forward_substitution(CholeskyFactor, Values, ForwardSolution),
		backward_substitution(CholeskyFactor, ForwardSolution, Solution).

	forward_substitution(CholeskyFactor, Values, Solution) :-
		forward_substitution(CholeskyFactor, Values, 1, [], Solution).

	forward_substitution([], [], _Index, Solution, Solution).
	forward_substitution([Row| Rows], [Value| Values], Index, KnownSolutions0, Solution) :-
		PreviousCount is Index - 1,
		forward_correction(Row, KnownSolutions0, PreviousCount, 0.0, Correction),
		nth1(Index, Row, Diagonal),
		CurrentSolution is (Value - Correction) / Diagonal,
		append(KnownSolutions0, [CurrentSolution], KnownSolutions1),
		NextIndex is Index + 1,
		forward_substitution(Rows, Values, NextIndex, KnownSolutions1, Solution).

	forward_correction(_Row, _KnownSolutions, 0, Correction, Correction) :-
		!.
	forward_correction([Coefficient| Coefficients], [KnownSolution| KnownSolutions], Count, Correction0, Correction) :-
		Correction1 is Correction0 + Coefficient * KnownSolution,
		NextCount is Count - 1,
		forward_correction(Coefficients, KnownSolutions, NextCount, Correction1, Correction).

	solve_lower_triangular_matrix(LowerTriangular, Matrix, Solution) :-
		transpose_matrix(Matrix, Columns),
		solve_lower_triangular_columns(Columns, LowerTriangular, SolutionColumns),
		transpose_matrix(SolutionColumns, Solution).

	solve_lower_triangular_columns([], _LowerTriangular, []).
	solve_lower_triangular_columns([Column| Columns], LowerTriangular, [SolutionColumn| SolutionColumns]) :-
		forward_substitution(LowerTriangular, Column, SolutionColumn),
		solve_lower_triangular_columns(Columns, LowerTriangular, SolutionColumns).

	solve_upper_triangular(UpperTriangular, Values, Solution) :-
		length(Values, Size),
		solve_upper_triangular(Size, UpperTriangular, Values, [], Solution).

	solve_upper_triangular(0, _UpperTriangular, _Values, Solution, Solution) :-
		!.
	solve_upper_triangular(Index, UpperTriangular, Values, KnownSolutions0, Solution) :-
		nth1(Index, UpperTriangular, Row),
		nth1(Index, Row, Diagonal),
		nth1(Index, Values, Value),
		upper_correction(KnownSolutions0, Row, Index, 0.0, Correction),
		CurrentSolution is (Value - Correction) / Diagonal,
		NextIndex is Index - 1,
		solve_upper_triangular(NextIndex, UpperTriangular, Values, [CurrentSolution| KnownSolutions0], Solution).

	upper_correction([], _Row, _Index, Correction, Correction).
	upper_correction([KnownSolution| KnownSolutions], Row, Index, Correction0, Correction) :-
		NextIndex is Index + 1,
		nth1(NextIndex, Row, Coefficient),
		Correction1 is Correction0 + Coefficient * KnownSolution,
		upper_correction(KnownSolutions, Row, NextIndex, Correction1, Correction).

	solve_upper_triangular_matrix(UpperTriangular, Matrix, Solution) :-
		transpose_matrix(Matrix, Columns),
		solve_upper_triangular_columns(Columns, UpperTriangular, SolutionColumns),
		transpose_matrix(SolutionColumns, Solution).

	solve_upper_triangular_columns([], _UpperTriangular, []).
	solve_upper_triangular_columns([Column| Columns], UpperTriangular, [SolutionColumn| SolutionColumns]) :-
		solve_upper_triangular(UpperTriangular, Column, SolutionColumn),
		solve_upper_triangular_columns(Columns, UpperTriangular, SolutionColumns).

	backward_substitution(CholeskyFactor, Values, Solution) :-
		transpose_matrix(CholeskyFactor, UpperTriangular),
		solve_upper_triangular(UpperTriangular, Values, Solution).

	invert_from_cholesky(CholeskyFactor, Inverse) :-
		length(CholeskyFactor, Size),
		identity_columns(Size, Columns),
		solve_inverse_columns(Columns, CholeskyFactor, InverseColumns),
		columns_to_rows(InverseColumns, Inverse).

	identity_columns(Size, Columns) :-
		identity_columns(1, Size, Columns).

	identity_columns(Index, Size, []) :-
		Index > Size,
		!.
	identity_columns(Index, Size, [Column| Columns]) :-
		identity_column(1, Size, Index, Column),
		NextIndex is Index + 1,
		identity_columns(NextIndex, Size, Columns).

	identity_column(Index, Size, _OneIndex, []) :-
		Index > Size,
		!.
	identity_column(Index, Size, OneIndex, [Value| Column]) :-
		(   Index =:= OneIndex ->
			Value = 1.0
		;   Value = 0.0
		),
		NextIndex is Index + 1,
		identity_column(NextIndex, Size, OneIndex, Column).

	solve_inverse_columns([], _CholeskyFactor, []).
	solve_inverse_columns([Column| Columns], CholeskyFactor, [InverseColumn| InverseColumns]) :-
		solve_cholesky(CholeskyFactor, Column, InverseColumn),
		solve_inverse_columns(Columns, CholeskyFactor, InverseColumns).

	columns_to_rows([], []).
	columns_to_rows([Column| Columns], Rows) :-
		length(Column, Size),
		columns_to_rows(1, Size, [Column| Columns], Rows).

	columns_to_rows(Index, Size, _Columns, []) :-
		Index > Size,
		!.
	columns_to_rows(Index, Size, Columns, [Row| Rows]) :-
		column_values_at(Columns, Index, Row),
		NextIndex is Index + 1,
		columns_to_rows(NextIndex, Size, Columns, Rows).

	column_values_at([], _Index, []).
	column_values_at([Column| Columns], Index, [Value| Values]) :-
		nth1(Index, Column, Value),
		column_values_at(Columns, Index, Values).

	require_rectangular_matrix([], 0, 0).
	require_rectangular_matrix(Matrix, RowCount, ColumnCount) :-
		Matrix = [Row| Rows],
		length(Matrix, RowCount),
		length(Row, ColumnCount),
		(   matrix_rows_have_length(Rows, ColumnCount) ->
			true
		;   domain_error(rectangular_matrix, Matrix)
		).

	require_square_matrix(Matrix, Size) :-
		require_rectangular_matrix(Matrix, Size, ColumnCount),
		(   Size =:= ColumnCount ->
			true
		;   domain_error(square_matrix, Matrix)
		).

	require_symmetric_matrix(Matrix, Tolerance) :-
		(   symmetric_matrix(Matrix, Tolerance) ->
			true
		;   domain_error(symmetric_matrix, Matrix)
		).

	symmetric_matrix(Matrix, Tolerance) :-
		length(Matrix, Size),
		symmetric_matrix(1, Size, Matrix, Tolerance).

	symmetric_matrix(RowIndex, Size, _Matrix, _Tolerance) :-
		RowIndex > Size,
		!.
	symmetric_matrix(RowIndex, Size, Matrix, Tolerance) :-
		nth1(RowIndex, Matrix, Row),
		NextColumnIndex is RowIndex + 1,
		symmetric_row(RowIndex, NextColumnIndex, Size, Matrix, Row, Tolerance),
		NextRowIndex is RowIndex + 1,
		symmetric_matrix(NextRowIndex, Size, Matrix, Tolerance).

	symmetric_row(_RowIndex, ColumnIndex, Size, _Matrix, _Row, _Tolerance) :-
		ColumnIndex > Size,
		!.
	symmetric_row(RowIndex, ColumnIndex, Size, Matrix, Row, Tolerance) :-
		nth1(ColumnIndex, Row, Value),
		nth1(ColumnIndex, Matrix, MirrorRow),
		nth1(RowIndex, MirrorRow, MirrorValue),
		abs(Value - MirrorValue) =< Tolerance,
		NextColumnIndex is ColumnIndex + 1,
		symmetric_row(RowIndex, NextColumnIndex, Size, Matrix, Row, Tolerance).

	matrix_rows_have_length([], _Size).
	matrix_rows_have_length([Row| Rows], Size) :-
		length(Row, Size),
		matrix_rows_have_length(Rows, Size).

	require_vector_length(Vector, Size) :-
		(   length(Vector, Size) ->
			true
		;   domain_error(vector_length(Size), Vector)
		).

	require_matrix_row_count(Matrix, Size) :-
		(   length(Matrix, Size) ->
			true
		;   domain_error(matrix_row_count(Size), Matrix)
		).

	vector_as_column_matrix([], []).
	vector_as_column_matrix([Value| Values], [[Value]| Rows]) :-
		vector_as_column_matrix(Values, Rows).

	linear_system_scale(Matrix, RightHandSideMatrix, Scale) :-
		matrix_max_abs(Matrix, 0.0, MatrixScale),
		matrix_max_abs(RightHandSideMatrix, 0.0, RightHandSideScale),
		Maximum is max(MatrixScale, RightHandSideScale),
		Scale is max(1.0, Maximum).

	matrix_scale(Matrix, Scale) :-
		matrix_max_abs(Matrix, 0.0, Maximum),
		Scale is max(1.0, Maximum).

	matrix_max_abs([], Maximum, Maximum).
	matrix_max_abs([Row| Matrix], Maximum0, Maximum) :-
		row_max_abs(Row, 0.0, RowMaximum),
		Maximum1 is max(Maximum0, RowMaximum),
		matrix_max_abs(Matrix, Maximum1, Maximum).

	row_max_abs([], Maximum, Maximum).
	row_max_abs([Value| Row], Maximum0, Maximum) :-
		Magnitude is abs(Value),
		Maximum1 is max(Maximum0, Magnitude),
		row_max_abs(Row, Maximum1, Maximum).

	initialize_qr_candidates([], _Index, []).
	initialize_qr_candidates([Column| Columns], Index, [candidate(Index, Column, [])| Candidates]) :-
		NextIndex is Index + 1,
		initialize_qr_candidates(Columns, NextIndex, Candidates).

	process_ordered_qr_candidates([], _Tolerance, [], []).
	process_ordered_qr_candidates([Candidate| Candidates], Tolerance, SelectedColumns, ProcessedColumns) :-
		candidate_norm(Candidate, Norm),
		(   Norm > Tolerance ->
			promote_qr_candidate(Candidate, Norm, Basis, SelectedColumn, ProcessedColumn),
			orthogonalize_qr_candidates(Candidates, Basis, OrthogonalCandidates),
			SelectedColumns = [SelectedColumn| SelectedColumns0],
			ProcessedColumns = [ProcessedColumn| ProcessedColumns0],
			process_ordered_qr_candidates(OrthogonalCandidates, Tolerance, SelectedColumns0, ProcessedColumns0)
		;   candidate_to_processed_column(Candidate, ProcessedColumn),
			ProcessedColumns = [ProcessedColumn| ProcessedColumns0],
			process_ordered_qr_candidates(Candidates, Tolerance, SelectedColumns, ProcessedColumns0)
		).

	process_pivoted_qr_candidates([], _Tolerance, [], []) :-
		!.
	process_pivoted_qr_candidates(Candidates, Tolerance, SelectedColumns, ProcessedColumns) :-
		select_best_candidate(Candidates, Candidate, RemainingCandidates),
		candidate_norm(Candidate, Norm),
		(   Norm > Tolerance ->
			promote_qr_candidate(Candidate, Norm, Basis, SelectedColumn, ProcessedColumn),
			orthogonalize_qr_candidates(RemainingCandidates, Basis, OrthogonalCandidates),
			SelectedColumns = [SelectedColumn| SelectedColumns0],
			ProcessedColumns = [ProcessedColumn| ProcessedColumns0],
			process_pivoted_qr_candidates(OrthogonalCandidates, Tolerance, SelectedColumns0, ProcessedColumns0)
		;   SelectedColumns = [],
			candidates_to_processed_columns([Candidate| RemainingCandidates], ProcessedColumns)
		).

	candidate_to_processed_column(candidate(Index, _Residual, Coefficients), processed_column(Index, Coefficients)).

	candidates_to_processed_columns([], []).
	candidates_to_processed_columns([Candidate| Candidates], [ProcessedColumn| ProcessedColumns]) :-
		candidate_to_processed_column(Candidate, ProcessedColumn),
		candidates_to_processed_columns(Candidates, ProcessedColumns).

	select_best_candidate([Candidate| Candidates], BestCandidate, RemainingCandidates) :-
		select_best_candidate(Candidates, Candidate, [], BestCandidate, RemainingCandidates).

	select_best_candidate([], BestCandidate, RemainingCandidates, BestCandidate, RemainingCandidates).
	select_best_candidate([Candidate| Candidates], Candidate0, RemainingCandidates0, BestCandidate, RemainingCandidates) :-
		candidate_norm(Candidate, Norm),
		candidate_norm(Candidate0, Norm0),
		(   Norm > Norm0 ->
			Candidate1 = Candidate,
			RemainingCandidates1 = [Candidate0| RemainingCandidates0]
		;   Candidate1 = Candidate0,
			RemainingCandidates1 = [Candidate| RemainingCandidates0]
		),
		select_best_candidate(Candidates, Candidate1, RemainingCandidates1, BestCandidate, RemainingCandidates).

	candidate_norm(candidate(_Index, Residual, _Coefficients), Norm) :-
		dot_product(Residual, Residual, NormSquared),
		Norm is sqrt(NormSquared).

	promote_qr_candidate(candidate(Index, Residual, Coefficients0), Norm, Basis, selected_column(Index, Coefficients, Basis), processed_column(Index, Coefficients)) :-
		Scale is 1.0 / Norm,
		rescale(Residual, Scale, Basis),
		append(Coefficients0, [Norm], Coefficients).

	orthogonalize_qr_candidates([], _Basis, []).
	orthogonalize_qr_candidates([candidate(Index, Residual0, Coefficients0)| Candidates0], Basis, [candidate(Index, Residual, Coefficients)| Candidates]) :-
		dot_product(Basis, Residual0, Coefficient),
		NegativeCoefficient is -Coefficient,
		add_scaled_vector(Basis, NegativeCoefficient, Residual0, Residual),
		append(Coefficients0, [Coefficient], Coefficients),
		orthogonalize_qr_candidates(Candidates0, Basis, Candidates).

	selected_column_count(SelectedColumns, Count) :-
		length(SelectedColumns, Count).

	q_columns_from_selected([], []).
	q_columns_from_selected([selected_column(_Index, _Coefficients, Basis)| SelectedColumns], [Basis| Bases]) :-
		q_columns_from_selected(SelectedColumns, Bases).

	orthogonal_matrix_from_selected(SelectedColumns, RowCount, Orthogonal) :-
		q_columns_from_selected(SelectedColumns, OrthogonalColumns),
		(   OrthogonalColumns == [] ->
			new_matrix(RowCount, 0, 0.0, Orthogonal)
		;   columns_to_rows(OrthogonalColumns, Orthogonal)
		).

	upper_triangular_factor(_ProcessedColumns, 0, []) :-
		!.
	upper_triangular_factor(ProcessedColumns, Rank, UpperTriangular) :-
		upper_triangular_rows(1, Rank, ProcessedColumns, UpperTriangular).

	upper_triangular_rows(RowIndex, Rank, _ProcessedColumns, []) :-
		RowIndex > Rank,
		!.
	upper_triangular_rows(RowIndex, Rank, ProcessedColumns, [Row| Rows]) :-
		upper_triangular_row(ProcessedColumns, RowIndex, Row),
		NextRowIndex is RowIndex + 1,
		upper_triangular_rows(NextRowIndex, Rank, ProcessedColumns, Rows).

	upper_triangular_row([], _RowIndex, []).
	upper_triangular_row([ProcessedColumn| ProcessedColumns], RowIndex, [Value| Values]) :-
		processed_column_coefficient(ProcessedColumn, RowIndex, Value),
		upper_triangular_row(ProcessedColumns, RowIndex, Values).

	processed_column_coefficient(processed_column(_Index, Coefficients), RowIndex, Value) :-
		(   nth1(RowIndex, Coefficients, Coefficient) ->
			Value = Coefficient
		;   Value = 0.0
		).

	leading_square_factor([], _Rank, []).
	leading_square_factor([Row| Rows], Rank, [LeadingRow| LeadingRows]) :-
		leading_values(Row, Rank, LeadingRow),
		leading_square_factor(Rows, Rank, LeadingRows).

	leading_values(_Values, 0, []) :-
		!.
	leading_values([Value| Values], Count, [Value| LeadingValues]) :-
		NextCount is Count - 1,
		leading_values(Values, NextCount, LeadingValues).

	q_transpose_times_vector([], _Values, []).
	q_transpose_times_vector([Basis| Bases], Values, [Projection| Projections]) :-
		dot_product(Basis, Values, Projection),
		q_transpose_times_vector(Bases, Values, Projections).

	processed_indices([], []).
	processed_indices([processed_column(Index, _Coefficients)| ProcessedColumns], [Index| Indices]) :-
		processed_indices(ProcessedColumns, Indices).

	pad_solution(Solution0, Size, Solution) :-
		length(Solution0, Count),
		Padding is Size - Count,
		new_vector(Padding, 0.0, Zeroes),
		append(Solution0, Zeroes, Solution).

	permuted_solution_to_original(Permutation, PivotedSolution, Solution) :-
		length(Permutation, Size),
		new_vector(Size, 0.0, Solution0),
		assign_permuted_solution(Permutation, PivotedSolution, Solution0, Solution).

	assign_permuted_solution([], [], Solution, Solution).
	assign_permuted_solution([Index| Indices], [Coefficient| Coefficients], Solution0, Solution) :-
		set_vector_value(Solution0, Index, Coefficient, Solution1),
		assign_permuted_solution(Indices, Coefficients, Solution1, Solution).

	set_vector_value([_Value0| Values], 1, Value, [Value| Values]) :-
		!.
	set_vector_value([Value0| Values0], Index, Value, [Value0| Values]) :-
		NextIndex is Index - 1,
		set_vector_value(Values0, NextIndex, Value, Values).

	triangularize_system(Matrix, RightHandSideMatrix, Scale, Sign, UpperMatrix, UpperRightHandSideMatrix, Status) :-
		augment_system_rows(Matrix, RightHandSideMatrix, Rows),
		triangularize_rows(Rows, 1, Scale, 1.0, Sign, UpperRows, Status),
		split_system_rows(UpperRows, UpperMatrix, UpperRightHandSideMatrix).

	augment_system_rows([], [], []).
	augment_system_rows([MatrixRow| Matrix], [RightHandSideRow| RightHandSideMatrix], [row(MatrixRow, RightHandSideRow)| Rows]) :-
		augment_system_rows(Matrix, RightHandSideMatrix, Rows).

	split_system_rows([], [], []).
	split_system_rows([row(MatrixRow, RightHandSideRow)| Rows], [MatrixRow| Matrix], [RightHandSideRow| RightHandSideMatrix]) :-
		split_system_rows(Rows, Matrix, RightHandSideMatrix).

	triangularize_rows([], _Index, _Scale, Sign, Sign, [], ok).
	triangularize_rows([Row0| Rows0], Index, Scale, Sign0, Sign, [PivotRow| UpperRows], Status) :-
		select_pivot_row([Row0| Rows0], Index, PivotRow, RemainingRows, SwapSign),
		pivot_row_value(Index, PivotRow, PivotValue),
		Sign1 is Sign0 * SwapSign,
		(   pivot_is_numerically_non_zero(PivotValue, Scale) ->
			eliminate_system_rows(RemainingRows, Index, PivotRow, ReducedRows),
			NextIndex is Index + 1,
			triangularize_rows(ReducedRows, NextIndex, Scale, Sign1, Sign, UpperRows, Status)
		;   Sign = Sign1,
			UpperRows = [],
			Status = zero_pivot
		).

	select_pivot_row([Row| Rows], Index, PivotRow, RemainingRows, SwapSign) :-
		select_pivot_row(Rows, Index, Row, false, [], PivotRow, RemainingRows, CandidateMoved),
		(   CandidateMoved == true ->
			SwapSign = -1.0
		;   SwapSign = 1.0
		).

	select_pivot_row([], _Index, Candidate, CandidateMoved, RemainingRows, Candidate, RemainingRows, CandidateMoved).
	select_pivot_row([Row| Rows], Index, Candidate0, CandidateMoved0, RemainingRows0, PivotRow, RemainingRows, CandidateMoved) :-
		pivot_magnitude(Index, Row, Magnitude),
		pivot_magnitude(Index, Candidate0, CandidateMagnitude),
		(   Magnitude > CandidateMagnitude ->
			Candidate = Row,
			CandidateMoved1 = true,
			RemainingRows1 = [Candidate0| RemainingRows0]
		;   Candidate = Candidate0,
			CandidateMoved1 = CandidateMoved0,
			RemainingRows1 = [Row| RemainingRows0]
		),
		select_pivot_row(Rows, Index, Candidate, CandidateMoved1, RemainingRows1, PivotRow, RemainingRows, CandidateMoved).

	pivot_magnitude(Index, row(MatrixRow, _RightHandSideRow), Magnitude) :-
		vector_value(MatrixRow, Index, Value),
		Magnitude is abs(Value).

	pivot_row_value(Index, row(MatrixRow, _RightHandSideRow), Value) :-
		vector_value(MatrixRow, Index, Value).

	pivot_is_numerically_non_zero(Value, Scale) :-
		Threshold is Scale * 1.0e-12,
		abs(Value) > Threshold.

	eliminate_system_rows([], _Index, _PivotRow, []).
	eliminate_system_rows([row(MatrixRow, RightHandSideRow)| Rows], Index, row(PivotMatrixRow, PivotRightHandSideRow), [row(UpdatedMatrixRow, UpdatedRightHandSideRow)| UpdatedRows]) :-
		vector_value(MatrixRow, Index, LeadingValue),
		vector_value(PivotMatrixRow, Index, PivotValue),
		Factor is LeadingValue / PivotValue,
		NegativeFactor is -Factor,
		add_scaled_vector(PivotMatrixRow, NegativeFactor, MatrixRow, UpdatedMatrixRow),
		add_scaled_vector(PivotRightHandSideRow, NegativeFactor, RightHandSideRow, UpdatedRightHandSideRow),
		eliminate_system_rows(Rows, Index, row(PivotMatrixRow, PivotRightHandSideRow), UpdatedRows).

	diagonal_product(Matrix, Product) :-
		diagonal_product(Matrix, 1, 1.0, Product).

	diagonal_product([], _Index, Product, Product).
	diagonal_product([Row| Rows], Index, Product0, Product) :-
		vector_value(Row, Index, Value),
		Product1 is Product0 * Value,
		NextIndex is Index + 1,
		diagonal_product(Rows, NextIndex, Product1, Product).

	sum_squares([], 0.0).
	sum_squares([Value| Values], SumSquares) :-
		SumSquares0 is Value * Value,
		sum_squares(Values, SumSquares0, SumSquares).

	sum_squares([], SumSquares, SumSquares).
	sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_squares(Values, SumSquares1, SumSquares).

	prefix_dot([], _OtherValues, DotProduct, DotProduct).
	prefix_dot([Value| Values], [OtherValue| OtherValues], DotProduct0, DotProduct) :-
		DotProduct1 is DotProduct0 + Value * OtherValue,
		prefix_dot(Values, OtherValues, DotProduct1, DotProduct).

:- end_object.
