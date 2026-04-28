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


:- object(pca,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Principal Component Analysis reducer for continuous datasets using a portable power-iteration eigensolver.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, computes the covariance matrix, and extracts principal components using deterministic power iteration with deflation.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``pca_reducer(Encoders, Components, ExplainedVariances, Diagnostics)`` where ``Encoders`` stores attribute centering/scaling metadata, ``Components`` stores the principal direction vectors, and ``Diagnostics`` records the learned model metadata and effective options.'
		],
		see_also is [lda_projection, random_projection]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a PCA dimension reducer from the given dataset object using the specified options.',
		argnames is ['Dataset', 'DimensionReducer', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, reverse/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(pairs, [
		keys/2
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
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		^^option(n_components(RequestedComponents), Options),
		ComponentCount is min(RequestedComponents, FeatureCount),
		covariance_matrix(Rows, CovarianceMatrix),
		extract_components(CovarianceMatrix, ComponentCount, Options, Components, ExplainedVariances),
		build_diagnostics(AttributeNames, Components, ExplainedVariances, Options, Diagnostics),
		DimensionReducer = pca_reducer(Encoders, Components, ExplainedVariances, Diagnostics),
		!.

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		check_minimum_examples(Examples),
		^^check_example_values(Examples, AttributeNames).

	check_minimum_examples(Examples) :-
		length(Examples, Count),
		(   Count >= 2 ->
			true
		;   domain_error(minimum_number_of_examples, Count)
		).

	example_attribute_values(_-AttributeValues, AttributeValues).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-AttributeValues| Examples], Encoders, [Features| Rows]) :-
		^^encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	covariance_matrix(Rows, CovarianceMatrix) :-
		Rows = [FirstRow| _],
		length(FirstRow, FeatureCount),
		zero_matrix(FeatureCount, ZeroMatrix),
		accumulate_outer_products(Rows, ZeroMatrix, SumMatrix),
		length(Rows, Count),
		Scale is 1.0 / (Count - 1),
		scale_matrix(SumMatrix, Scale, CovarianceMatrix).

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

	accumulate_outer_products([], Matrix, Matrix).
	accumulate_outer_products([Row| Rows], Matrix0, Matrix) :-
		outer_product(Row, Row, Outer),
		add_matrices(Matrix0, Outer, Matrix1),
		accumulate_outer_products(Rows, Matrix1, Matrix).

	outer_product([], _Vector, []).
	outer_product([Value| Values], Vector, [Row| Rows]) :-
		scale_vector(Vector, Value, Row),
		outer_product(Values, Vector, Rows).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [Scaled| ScaledValues]) :-
		Scaled is Value * Scale,
		scale_vector(Values, Scale, ScaledValues).

	add_matrices([], [], []).
	add_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		add_vectors(Row1, Row2, Row),
		add_matrices(Rows1, Rows2, Rows).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	scale_matrix([], _Scale, []).
	scale_matrix([Row| Rows], Scale, [ScaledRow| ScaledRows]) :-
		scale_vector(Row, Scale, ScaledRow),
		scale_matrix(Rows, Scale, ScaledRows).

	extract_components(_Matrix, 0, _Options, [], []) :-
		!.
	extract_components(Matrix, Requested, Options, Components, ExplainedVariances) :-
		extract_components(Matrix, Requested, Options, [], [], Components, ExplainedVariances).

	extract_components(_Matrix, 0, _Options, ComponentsAcc, EigenvaluesAcc, Components, ExplainedVariances) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(EigenvaluesAcc, ExplainedVariances).
	extract_components(Matrix, Requested, Options, ComponentsAcc, EigenvaluesAcc, Components, ExplainedVariances) :-
		principal_component(Matrix, Options, Eigenvalue, Eigenvector),
		^^option(tolerance(Tolerance), Options),
		(   Eigenvalue =< Tolerance ->
			reverse(ComponentsAcc, Components),
			reverse(EigenvaluesAcc, ExplainedVariances)
		;   deflate_matrix(Matrix, Eigenvalue, Eigenvector, DeflatedMatrix),
			NextRequested is Requested - 1,
			extract_components(DeflatedMatrix, NextRequested, Options, [Eigenvector| ComponentsAcc], [Eigenvalue| EigenvaluesAcc], Components, ExplainedVariances)
		).

	principal_component(Matrix, Options, Eigenvalue, Eigenvector) :-
		length(Matrix, Size),
		initial_vectors(Size, InitialVectors),
		zero_vector(Size, ZeroVector),
		principal_component_candidates(Matrix, Options, InitialVectors, 0.0, ZeroVector, Eigenvalue, Eigenvector).

	initial_vectors(Size, [InitialVector| BasisVectors]) :-
		initial_vector(Size, InitialVector),
		basis_initial_vectors(1, Size, BasisVectors).

	basis_initial_vectors(Index, Size, []) :-
		Index > Size,
		!.
	basis_initial_vectors(Index, Size, [BasisVector| BasisVectors]) :-
		basis_vector(Size, Index, BasisVector),
		NextIndex is Index + 1,
		basis_initial_vectors(NextIndex, Size, BasisVectors).

	principal_component_candidates(_Matrix, _Options, [], BestEigenvalue, BestEigenvector, BestEigenvalue, BestEigenvector) :-
		!.
	principal_component_candidates(Matrix, Options, [InitialVector| InitialVectors], BestEigenvalue0, BestEigenvector0, BestEigenvalue, BestEigenvector) :-
		normalize_vector(InitialVector, NormalizedInitial),
		iterate_component(Matrix, Options, 0, NormalizedInitial, CandidateEigenvalue, CandidateEigenvector),
		(   CandidateEigenvalue > BestEigenvalue0 ->
			BestEigenvalue1 = CandidateEigenvalue,
			BestEigenvector1 = CandidateEigenvector
		;   BestEigenvalue1 = BestEigenvalue0,
			BestEigenvector1 = BestEigenvector0
		),
		principal_component_candidates(Matrix, Options, InitialVectors, BestEigenvalue1, BestEigenvector1, BestEigenvalue, BestEigenvector).

	initial_vector(0, []) :-
		!.
	initial_vector(Size, [1.0| Vector]) :-
		Size > 0,
		NextSize is Size - 1,
		initial_vector(NextSize, Vector).

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

	subtract_vectors([], [], []).
	subtract_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 - Value2,
		subtract_vectors(Values1, Values2, Values).

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
		outer_product(Eigenvector, Eigenvector, Outer),
		scale_matrix(Outer, Eigenvalue, ScaledOuter),
		subtract_matrices(Matrix, ScaledOuter, DeflatedMatrix).

	build_diagnostics(AttributeNames, Components, ExplainedVariances, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		length(Components, ComponentCount),
		Diagnostics = [
			model(pca),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			component_count(ComponentCount),
			explained_variances(ExplainedVariances)
		].

	subtract_matrices([], [], []).
	subtract_matrices([Row1| Rows1], [Row2| Rows2], [Row| Rows]) :-
		subtract_vectors(Row1, Row2, Row),
		subtract_matrices(Rows1, Rows2, Rows).

	dimension_reducer_data(DimensionReducer, Encoders, Components) :-
		DimensionReducer =.. [_Functor, Encoders, Components| _].

	dimension_reducer_diagnostics_data(pca_reducer(_Encoders, _Components, _ExplainedVariances, Diagnostics), Diagnostics).

	print_dimension_reducer_properties(pca_reducer(Encoders, Components, ExplainedVariances, Diagnostics)) :-
		format('PCA Dimension Reducer~n', []),
		format('=====================~n~n', []),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(Components, ComponentCount),
		format('Components: ~w~n', [ComponentCount]),
		format('Explained variances: ~w~n', [ExplainedVariances]).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

:- end_object.
