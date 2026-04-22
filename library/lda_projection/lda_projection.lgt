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


:- object(lda_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Linear Discriminant Analysis projection for labeled continuous datasets using a portable regularized Fisher eigensolver.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, builds regularized within-class and between-class scatter matrices, whitens the Fisher criterion using a Cholesky factorization, and extracts discriminant directions using deterministic power iteration with deflation.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Supervision' - 'Requires a dataset implementing ``supervised_dimension_reduction_dataset_protocol`` and therefore uses class labels during training.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``lda_projection_reducer(Encoders, Components, ClassValues, Options)`` where ``Encoders`` stores attribute centering/scaling metadata and ``Components`` stores the learned discriminant vectors.'
		],
		see_also is [
			dimension_reducer_protocol, dimension_reduction_dataset_protocol,
			supervised_dimension_reduction_dataset_protocol, pca, random_projection
		]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns an LDA projection from the given labeled dataset object using the specified options.',
		argnames is ['Dataset', 'DimensionReducer', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth1/3, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		Dataset::class_values(ClassValues),
		check_class_values(ClassValues),
		findall(Id-Class-AttributeValues, Dataset::example(Id, Class, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, ClassValues, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		class_statistics(ClassValues, Rows, ClassStatistics),
		rows_only(Rows, FeatureRows),
		mean_vector(FeatureRows, OverallMean),
		length(AttributeNames, FeatureCount),
		within_class_scatter(ClassStatistics, FeatureCount, WithinScatter),
		between_class_scatter(ClassStatistics, OverallMean, FeatureCount, BetweenScatter),
		^^option(regularization(Regularization), Options),
		regularize_matrix(WithinScatter, Regularization, RegularizedWithinScatter),
		cholesky_decomposition(RegularizedWithinScatter, LowerTriangular),
		inverse_lower_triangular(LowerTriangular, InverseLowerTriangular),
		transpose_matrix(InverseLowerTriangular, InverseLowerTriangularTranspose),
		matrix_multiply(InverseLowerTriangular, BetweenScatter, WhiteningLeft),
		matrix_multiply(WhiteningLeft, InverseLowerTriangularTranspose, SymmetricCriterion),
		length(ClassValues, ClassCount),
		^^option(n_components(RequestedComponents), Options),
		ComponentCount is min(RequestedComponents, min(FeatureCount, ClassCount - 1)),
		extract_components(SymmetricCriterion, ComponentCount, Options, WhitenedComponents, _Eigenvalues),
		map_whitened_components(WhitenedComponents, InverseLowerTriangularTranspose, Components),
		DimensionReducer = lda_projection_reducer(Encoders, Components, ClassValues, Options),
		!.

	check_class_values(ClassValues) :-
		length(ClassValues, ClassCount),
		(   ClassCount >= 2 ->
			true
		;   domain_error(minimum_number_of_classes(2), ClassValues)
		).

	check_examples(Dataset, AttributeNames, ClassValues, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		check_example_values(Examples, AttributeNames, ClassValues).

	check_example_values([], _AttributeNames, _ClassValues).
	check_example_values([_-Class-AttributeValues| Examples], AttributeNames, ClassValues) :-
		check_example_class(Class, ClassValues),
		^^check_example_attributes(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames, ClassValues).

	check_example_class(Class, ClassValues) :-
		(   member(Class, ClassValues) ->
			true
		;   domain_error(class_value, Class)
		).

	example_attribute_values(_-_-AttributeValues, AttributeValues).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-Class-AttributeValues| Examples], Encoders, [Class-Features| Rows]) :-
		^^encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	rows_only([], []).
	rows_only([_-Row| Rows], [Row| FeatureRows]) :-
		rows_only(Rows, FeatureRows).

	class_statistics([], _Rows, []).
	class_statistics([Class| Classes], Rows, [class_statistics(Class, Count, Mean, ClassRows)| Statistics]) :-
		collect_class_rows(Rows, Class, ClassRows),
		length(ClassRows, Count),
		(   Count > 0 ->
			mean_vector(ClassRows, Mean)
		;   domain_error(non_empty_class, Class)
		),
		class_statistics(Classes, Rows, Statistics).

	collect_class_rows([], _Class, []).
	collect_class_rows([Class-Row| Rows], Class, [Row| ClassRows]) :-
		!,
		collect_class_rows(Rows, Class, ClassRows).
	collect_class_rows([_| Rows], Class, ClassRows) :-
		collect_class_rows(Rows, Class, ClassRows).

	mean_vector(Rows, Mean) :-
		transpose_matrix(Rows, Columns),
		column_means(Columns, Mean).

	column_means([], []).
	column_means([Column| Columns], [Mean| Means]) :-
		arithmetic_mean(Column, Mean),
		column_means(Columns, Means).

	within_class_scatter(ClassStatistics, FeatureCount, WithinScatter) :-
		zero_matrix(FeatureCount, ZeroMatrix),
		accumulate_within_class_scatter(ClassStatistics, ZeroMatrix, WithinScatter).

	accumulate_within_class_scatter([], WithinScatter, WithinScatter).
	accumulate_within_class_scatter([class_statistics(_Class, _Count, Mean, Rows)| Statistics], WithinScatter0, WithinScatter) :-
		class_within_scatter(Rows, Mean, ClassScatter),
		add_matrices(WithinScatter0, ClassScatter, WithinScatter1),
		accumulate_within_class_scatter(Statistics, WithinScatter1, WithinScatter).

	class_within_scatter([], Mean, Scatter) :-
		length(Mean, FeatureCount),
		zero_matrix(FeatureCount, Scatter).
	class_within_scatter([Row| Rows], Mean, Scatter) :-
		subtract_vectors(Row, Mean, Difference),
		outer_product(Difference, Difference, Scatter0),
		class_within_scatter(Rows, Mean, ScatterRest),
		add_matrices(Scatter0, ScatterRest, Scatter).

	between_class_scatter(ClassStatistics, OverallMean, FeatureCount, BetweenScatter) :-
		zero_matrix(FeatureCount, ZeroMatrix),
		accumulate_between_class_scatter(ClassStatistics, OverallMean, ZeroMatrix, BetweenScatter).

	accumulate_between_class_scatter([], _OverallMean, BetweenScatter, BetweenScatter).
	accumulate_between_class_scatter([class_statistics(_Class, Count, Mean, _Rows)| Statistics], OverallMean, BetweenScatter0, BetweenScatter) :-
		subtract_vectors(Mean, OverallMean, Difference),
		outer_product(Difference, Difference, OuterProduct),
		scale_matrix(OuterProduct, Count, ScaledOuterProduct),
		add_matrices(BetweenScatter0, ScaledOuterProduct, BetweenScatter1),
		accumulate_between_class_scatter(Statistics, OverallMean, BetweenScatter1, BetweenScatter).

	regularize_matrix(Matrix, Regularization, RegularizedMatrix) :-
		regularize_matrix(Matrix, Regularization, 1, RegularizedMatrix).

	regularize_matrix([], _Regularization, _Index, []).
	regularize_matrix([Row| Rows], Regularization, Index, [RegularizedRow| RegularizedRows]) :-
		regularize_row(Row, Regularization, Index, 1, RegularizedRow),
		NextIndex is Index + 1,
		regularize_matrix(Rows, Regularization, NextIndex, RegularizedRows).

	regularize_row([], _Regularization, _DiagonalIndex, _Index, []).
	regularize_row([Value| Values], Regularization, DiagonalIndex, DiagonalIndex, [RegularizedValue| RegularizedValues]) :-
		!,
		RegularizedValue is Value + Regularization,
		NextIndex is DiagonalIndex + 1,
		regularize_row(Values, Regularization, DiagonalIndex, NextIndex, RegularizedValues).
	regularize_row([Value| Values], Regularization, DiagonalIndex, Index, [Value| RegularizedValues]) :-
		NextIndex is Index + 1,
		regularize_row(Values, Regularization, DiagonalIndex, NextIndex, RegularizedValues).

	cholesky_decomposition(Matrix, LowerTriangular) :-
		length(Matrix, Size),
		cholesky_rows(1, Size, Matrix, [], LowerTriangular).

	cholesky_rows(Index, Size, _Matrix, LowerTriangular, LowerTriangular) :-
		Index > Size,
		!.
	cholesky_rows(Index, Size, Matrix, LowerTriangular0, LowerTriangular) :-
		cholesky_row(1, Index, Size, Matrix, LowerTriangular0, [], Row),
		append(LowerTriangular0, [Row], LowerTriangular1),
		NextIndex is Index + 1,
		cholesky_rows(NextIndex, Size, Matrix, LowerTriangular1, LowerTriangular).

	cholesky_row(Column, RowIndex, Size, _Matrix, _LowerTriangular, Prefix, Row) :-
		Column > RowIndex,
		!,
		PaddingSize is Size - RowIndex,
		zero_vector(PaddingSize, Padding),
		append(Prefix, Padding, Row).
	cholesky_row(Column, RowIndex, Size, Matrix, LowerTriangular, Prefix, Row) :-
		nth1(RowIndex, Matrix, MatrixRow),
		nth1(Column, MatrixRow, MatrixValue),
		(   Column =:= RowIndex ->
			sum_squares(Prefix, 0.0, SumSquares),
			DiagonalValue is max(MatrixValue - SumSquares, 1.0e-12),
			Value is sqrt(DiagonalValue)
		;   nth1(Column, LowerTriangular, PreviousRow),
			PrefixCount is Column - 1,
			prefix_dot(Prefix, PreviousRow, PrefixCount, 0.0, DotProduct),
			nth1(Column, PreviousRow, Diagonal),
			Value is (MatrixValue - DotProduct) / Diagonal
		),
		append(Prefix, [Value], Prefix1),
		NextColumn is Column + 1,
		cholesky_row(NextColumn, RowIndex, Size, Matrix, LowerTriangular, Prefix1, Row).

	sum_squares([], SumSquares, SumSquares).
	sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		sum_squares(Values, SumSquares1, SumSquares).

	prefix_dot(_Prefix, _Row, 0, DotProduct, DotProduct) :-
		!.
	prefix_dot([Value| Values], [Other| Others], Count, DotProduct0, DotProduct) :-
		Count > 0,
		NextCount is Count - 1,
		DotProduct1 is DotProduct0 + Value * Other,
		prefix_dot(Values, Others, NextCount, DotProduct1, DotProduct).

	inverse_lower_triangular(LowerTriangular, InverseLowerTriangular) :-
		length(LowerTriangular, Size),
		inverse_lower_columns(1, Size, LowerTriangular, [], InverseColumns),
		transpose_matrix(InverseColumns, InverseLowerTriangular).

	inverse_lower_columns(Index, Size, _LowerTriangular, Columns, Columns) :-
		Index > Size,
		!.
	inverse_lower_columns(Index, Size, LowerTriangular, Columns0, Columns) :-
		basis_vector(Size, Index, BasisVector),
		solve_lower_triangular(LowerTriangular, BasisVector, Column),
		append(Columns0, [Column], Columns1),
		NextIndex is Index + 1,
		inverse_lower_columns(NextIndex, Size, LowerTriangular, Columns1, Columns).

	basis_vector(Size, Index, Vector) :-
		basis_vector(1, Size, Index, Vector).

	basis_vector(Current, Size, _Index, []) :-
		Current > Size,
		!.
	basis_vector(Index, Size, Index, [1.0| Vector]) :-
		!,
		Next is Index + 1,
		basis_vector(Next, Size, Index, Vector).
	basis_vector(Current, Size, Index, [0.0| Vector]) :-
		Next is Current + 1,
		basis_vector(Next, Size, Index, Vector).

	solve_lower_triangular(LowerTriangular, RightHandSide, Solution) :-
		solve_lower_triangular(LowerTriangular, RightHandSide, [], 1, Solution).

	solve_lower_triangular([], [], Solution, _Index, Solution).
	solve_lower_triangular([Row| Rows], [Value| Values], Prefix, Index, Solution) :-
		PrefixCount is Index - 1,
		prefix_dot(Row, Prefix, PrefixCount, 0.0, DotProduct),
		nth1(Index, Row, Diagonal),
		SolutionValue is (Value - DotProduct) / Diagonal,
		append(Prefix, [SolutionValue], Prefix1),
		NextIndex is Index + 1,
		solve_lower_triangular(Rows, Values, Prefix1, NextIndex, Solution).

	map_whitened_components([], _InverseLowerTriangularTranspose, []).
	map_whitened_components([WhitenedComponent| WhitenedComponents], InverseLowerTriangularTranspose, [Component| Components]) :-
		matrix_vector_product(InverseLowerTriangularTranspose, WhitenedComponent, RawComponent),
		normalize_vector(RawComponent, NormalizedComponent),
		stabilize_vector_sign(NormalizedComponent, Component),
		map_whitened_components(WhitenedComponents, InverseLowerTriangularTranspose, Components).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	zero_matrix(Size, Matrix) :-
		zero_matrix(Size, Size, Matrix).

	zero_matrix(0, _Columns, []) :-
		!.
	zero_matrix(Rows, Columns, [Row| Matrix]) :-
		Rows > 0,
		zero_vector(Columns, Row),
		NextRows is Rows - 1,
		zero_matrix(NextRows, Columns, Matrix).

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

	outer_product([], _Vector, []).
	outer_product([Value| Values], Vector, [Row| Rows]) :-
		scale_vector(Vector, Value, Row),
		outer_product(Values, Vector, Rows).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [ScaledValue| ScaledValues]) :-
		ScaledValue is Value * Scale,
		scale_vector(Values, Scale, ScaledValues).

	add_matrices([], [], []).
	add_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		add_vectors(Row1, Row2, Row),
		add_matrices(Rows1, Rows2, Rows).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	subtract_matrices([], [], []).
	subtract_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		subtract_vectors(Row1, Row2, Row),
		subtract_matrices(Rows1, Rows2, Rows).

	subtract_vectors([], [], []).
	subtract_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 - Value2,
		subtract_vectors(Values1, Values2, Values).

	scale_matrix([], _Scale, []).
	scale_matrix([Row| Rows], Scale, [ScaledRow| ScaledRows]) :-
		scale_vector(Row, Scale, ScaledRow),
		scale_matrix(Rows, Scale, ScaledRows).

	matrix_multiply(Matrix1, Matrix2, Product) :-
		transpose_matrix(Matrix2, TransposedMatrix2),
		matrix_multiply_rows(Matrix1, TransposedMatrix2, Product).

	matrix_multiply_rows([], _TransposedMatrix2, []).
	matrix_multiply_rows([Row| Rows], TransposedMatrix2, [ProductRow| ProductRows]) :-
		matrix_row_products(TransposedMatrix2, Row, ProductRow),
		matrix_multiply_rows(Rows, TransposedMatrix2, ProductRows).

	matrix_row_products([], _Row, []).
	matrix_row_products([Column| Columns], Row, [Value| Values]) :-
		dot_product(Row, Column, Value),
		matrix_row_products(Columns, Row, Values).

	extract_components(_Matrix, 0, _Options, [], []) :-
		!.
	extract_components(Matrix, Requested, Options, Components, Eigenvalues) :-
		extract_components(Matrix, Requested, Options, [], [], Components, Eigenvalues).

	extract_components(_Matrix, 0, _Options, ComponentsAcc, EigenvaluesAcc, Components, Eigenvalues) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(EigenvaluesAcc, Eigenvalues).
	extract_components(Matrix, Requested, Options, ComponentsAcc, EigenvaluesAcc, Components, Eigenvalues) :-
		principal_component(Matrix, Options, Eigenvalue, Eigenvector),
		^^option(tolerance(Tolerance), Options),
		(   Eigenvalue =< Tolerance ->
			reverse(ComponentsAcc, Components),
			reverse(EigenvaluesAcc, Eigenvalues)
		;   deflate_matrix(Matrix, Eigenvalue, Eigenvector, DeflatedMatrix),
			NextRequested is Requested - 1,
			extract_components(DeflatedMatrix, NextRequested, Options, [Eigenvector| ComponentsAcc], [Eigenvalue| EigenvaluesAcc], Components, Eigenvalues)
		).

	principal_component(Matrix, Options, Eigenvalue, Eigenvector) :-
		length(Matrix, Size),
		initial_vector(Size, InitialVector),
		normalize_vector(InitialVector, NormalizedInitial),
		iterate_component(Matrix, Options, 0, NormalizedInitial, Eigenvalue, Eigenvector).

	initial_vector(0, []) :-
		!.
	initial_vector(Size, [1.0| Vector]) :-
		Size > 0,
		NextSize is Size - 1,
		initial_vector(NextSize, Vector).

	normalize_vector(Vector, NormalizedVector) :-
		vector_norm(Vector, Norm),
		(   Norm =< 1.0e-12 ->
			NormalizedVector = Vector
		;   scale_vector(Vector, 1.0 / Norm, NormalizedVector)
		).

	iterate_component(Matrix, Options, Iteration, Vector0, Eigenvalue, Eigenvector) :-
		matrix_vector_product(Matrix, Vector0, Product),
		vector_norm(Product, Norm),
		^^option(tolerance(Tolerance), Options),
		(   Norm =< Tolerance ->
			Eigenvalue = 0.0,
			Eigenvector = Vector0
		;   scale_vector(Product, 1.0 / Norm, Vector1),
			stabilize_vector_sign(Vector1, StableVector),
			difference_norm(StableVector, Vector0, Delta),
			^^option(maximum_iterations(MaximumIterations), Options),
			(   (Delta =< Tolerance ; Iteration >= MaximumIterations) ->
				rayleigh_quotient(Matrix, StableVector, Eigenvalue),
				Eigenvector = StableVector
			;   NextIteration is Iteration + 1,
				iterate_component(Matrix, Options, NextIteration, StableVector, Eigenvalue, Eigenvector)
			)
		).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	vector_norm(Vector, Norm) :-
		dot_product(Vector, Vector, SumSquares),
		Norm is sqrt(SumSquares).

	difference_norm(Vector1, Vector2, Norm) :-
		subtract_vectors(Vector1, Vector2, Difference),
		vector_norm(Difference, Norm).

	stabilize_vector_sign(Vector, StableVector) :-
		(   first_significant_component(Vector, First),
			First < 0.0 ->
			scale_vector(Vector, -1.0, StableVector)
		;   StableVector = Vector
		).

	first_significant_component([Value| _Values], Value) :-
		abs(Value) > 1.0e-12,
		!.
	first_significant_component([_Value| Values], First) :-
		first_significant_component(Values, First).
	first_significant_component([], 0.0).

	rayleigh_quotient(Matrix, Vector, Eigenvalue) :-
		matrix_vector_product(Matrix, Vector, Product),
		dot_product(Vector, Product, Eigenvalue).

	deflate_matrix(Matrix, Eigenvalue, Eigenvector, DeflatedMatrix) :-
		outer_product(Eigenvector, Eigenvector, OuterProduct),
		scale_matrix(OuterProduct, Eigenvalue, ScaledOuterProduct),
		subtract_matrices(Matrix, ScaledOuterProduct, DeflatedMatrix).

	dimension_reducer_data(DimensionReducer, Encoders, Components) :-
		DimensionReducer =.. [_Functor, Encoders, Components| _].

	print_dimension_reducer_properties(lda_projection_reducer(Encoders, Components, ClassValues, Options)) :-
		format('LDA Projection Reducer~n', []),
		format('======================~n~n', []),
		format('Options: ~w~n', [Options]),
		format('Classes: ~w~n', [ClassValues]),
		format('Encoders: ~w~n', [Encoders]),
		length(Components, ComponentCount),
		format('Components: ~w~n', [ComponentCount]).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).
	default_option(regularization(1.0e-6)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).
	valid_option(regularization(Regularization)) :-
		valid(positive_float, Regularization).

:- end_object.
