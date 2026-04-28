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


:- object(truncated_svd,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Truncated singular value decomposition reducer for continuous datasets using a portable two-sided power-iteration solver over the data matrix.',
		remarks is [
			'Algorithm' - 'Builds a preprocessed data matrix using optional centering and scaling, extracts singular triplets using deterministic two-sided power iteration, and applies rank-one deflation directly to the data matrix.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``truncated_svd_reducer(Encoders, Components, SingularValues, Diagnostics)`` where ``Encoders`` stores preprocessing metadata, ``Components`` stores right singular vectors, and ``Diagnostics`` records the learned model metadata and effective options.'
		],
		see_also is [pca, random_projection]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a truncated singular value decomposition reducer from the given dataset object using the specified options.',
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

	:- uses(population, [
		arithmetic_mean/2, variance/2
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
		build_truncated_svd_encoders(AttributeNames, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		length(Examples, ExampleCount),
		^^option(n_components(RequestedComponents), Options),
		ComponentCount is min(RequestedComponents, min(FeatureCount, ExampleCount)),
		extract_components(Rows, ComponentCount, Options, Components, SingularValues, ComponentDiagnostics),
		build_diagnostics(AttributeNames, ExampleCount, Components, SingularValues, ComponentDiagnostics, Options, Diagnostics),
		DimensionReducer = truncated_svd_reducer(Encoders, Components, SingularValues, Diagnostics).

	check_dimension_reducer(DimensionReducer) :-
		^^check_dimension_reducer(DimensionReducer),
		(   DimensionReducer = truncated_svd_reducer(_Encoders, Components, SingularValues, _Diagnostics),
			valid_singular_values(Components, SingularValues) ->
			true
		;   domain_error(valid_dimension_reducer, DimensionReducer)
		).

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		^^check_example_values(Examples, AttributeNames).

	example_attribute_values(_-AttributeValues, AttributeValues).

	build_truncated_svd_encoders([], _Examples, _Options, []).
	build_truncated_svd_encoders([Attribute| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		encoder_statistics(Examples, Attribute, Options, Mean, Scale),
		build_truncated_svd_encoders(Attributes, Examples, Options, Encoders).

	encoder_statistics(Examples, Attribute, Options, Mean, Scale) :-
		^^known_attribute_values(Examples, Attribute, Values),
		^^option(center(Center), Options),
		(   Center == true ->
			arithmetic_mean(Values, Mean)
		;   Mean = 0.0
		),
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
			length(Values, Count),
			(   Count > 1 ->
				variance(Values, Variance)
			;   Variance = 0.0
			),
			(   Variance > 0.0 ->
				Scale is sqrt(Variance)
			;   Scale = 1.0
			)
		;   Scale = 1.0
		).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-AttributeValues| Examples], Encoders, [Features| Rows]) :-
		^^encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	scale_vector([], _Scale, []).
	scale_vector([Value| Values], Scale, [Scaled| ScaledValues]) :-
		Scaled is Value * Scale,
		scale_vector(Values, Scale, ScaledValues).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	extract_components(_Rows, 0, _Options, [], [], []) :-
		!.
	extract_components(Rows, Requested, Options, Components, SingularValues, ComponentDiagnostics) :-
		extract_components(Rows, Requested, Requested, Options, [], [], [], Components, SingularValues, ComponentDiagnostics).

	extract_components(_Rows, 0, _Requested, _Options, ComponentsAcc, SingularValuesAcc, ComponentDiagnosticsAcc, Components, SingularValues, ComponentDiagnostics) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(SingularValuesAcc, SingularValues),
		reverse(ComponentDiagnosticsAcc, ComponentDiagnostics).
	extract_components(Rows, Requested, TotalRequested, Options, ComponentsAcc, SingularValuesAcc, ComponentDiagnosticsAcc, Components, SingularValues, ComponentDiagnostics) :-
		singular_triplet(Rows, Options, SingularValue, LeftSingularVector, RightSingularVector, Convergence, Iterations, FinalDelta),
		^^option(tolerance(Tolerance), Options),
		(   SingularValue =< Tolerance ->
			length(ComponentsAcc, ExtractedCount),
			domain_error(component_count, TotalRequested-ExtractedCount)
		;   deflate_rows(Rows, SingularValue, LeftSingularVector, RightSingularVector, DeflatedRows),
			NextRequested is Requested - 1,
			extract_components(DeflatedRows, NextRequested, TotalRequested, Options, [RightSingularVector| ComponentsAcc], [SingularValue| SingularValuesAcc], [component_diagnostics(Convergence, Iterations, FinalDelta)| ComponentDiagnosticsAcc], Components, SingularValues, ComponentDiagnostics)
		).

	singular_triplet(Rows, Options, SingularValue, LeftSingularVector, RightSingularVector, Convergence, Iterations, FinalDelta) :-
		Rows = [FirstRow| _],
		length(FirstRow, FeatureCount),
		length(Rows, ExampleCount),
		initial_vectors(FeatureCount, InitialVectors),
		zero_vector(ExampleCount, ZeroLeftSingularVector),
		zero_vector(FeatureCount, ZeroRightSingularVector),
		singular_triplet_candidates(Rows, Options, InitialVectors, 0.0, ZeroLeftSingularVector, ZeroRightSingularVector, tolerance, 0, 0.0, SingularValue, LeftSingularVector, RightSingularVector, Convergence, Iterations, FinalDelta).

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

	singular_triplet_candidates(_Rows, _Options, [], BestSingularValue, BestLeftSingularVector, BestRightSingularVector, BestConvergence, BestIterations, BestFinalDelta, BestSingularValue, BestLeftSingularVector, BestRightSingularVector, BestConvergence, BestIterations, BestFinalDelta) :-
		!.
	singular_triplet_candidates(Rows, Options, [InitialVector| InitialVectors], BestSingularValue0, BestLeftSingularVector0, BestRightSingularVector0, BestConvergence0, BestIterations0, BestFinalDelta0, BestSingularValue, BestLeftSingularVector, BestRightSingularVector, BestConvergence, BestIterations, BestFinalDelta) :-
		normalize_vector(InitialVector, NormalizedInitial),
		iterate_singular_triplet(Rows, Options, 0, NormalizedInitial, CandidateSingularValue, CandidateLeftSingularVector, CandidateRightSingularVector, CandidateConvergence, CandidateIterations, CandidateFinalDelta),
		(   CandidateSingularValue > BestSingularValue0 ->
			BestSingularValue1 = CandidateSingularValue,
			BestLeftSingularVector1 = CandidateLeftSingularVector,
			BestRightSingularVector1 = CandidateRightSingularVector,
			BestConvergence1 = CandidateConvergence,
			BestIterations1 = CandidateIterations,
			BestFinalDelta1 = CandidateFinalDelta
		;   BestSingularValue1 = BestSingularValue0,
			BestLeftSingularVector1 = BestLeftSingularVector0,
			BestRightSingularVector1 = BestRightSingularVector0,
			BestConvergence1 = BestConvergence0,
			BestIterations1 = BestIterations0,
			BestFinalDelta1 = BestFinalDelta0
		),
		singular_triplet_candidates(Rows, Options, InitialVectors, BestSingularValue1, BestLeftSingularVector1, BestRightSingularVector1, BestConvergence1, BestIterations1, BestFinalDelta1, BestSingularValue, BestLeftSingularVector, BestRightSingularVector, BestConvergence, BestIterations, BestFinalDelta).

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

	iterate_singular_triplet(Rows, Options, Iteration, RightSingularVector0, SingularValue, LeftSingularVector, RightSingularVector, Convergence, Iterations, FinalDelta) :-
		matrix_vector_product(Rows, RightSingularVector0, LeftProduct),
		vector_norm(LeftProduct, LeftNorm),
		^^option(tolerance(Tolerance), Options),
		(   LeftNorm =< Tolerance ->
			length(Rows, ExampleCount),
			zero_vector(ExampleCount, LeftSingularVector),
			SingularValue = 0.0,
			RightSingularVector = RightSingularVector0,
			Convergence = tolerance,
			Iterations = Iteration,
			FinalDelta = 0.0
		;   scale_vector(LeftProduct, 1.0 / LeftNorm, LeftSingularVector0),
			transpose_matrix_vector_product(Rows, LeftSingularVector0, RightProduct),
			vector_norm(RightProduct, RightNorm),
			(   RightNorm =< Tolerance ->
				length(Rows, ExampleCount),
				zero_vector(ExampleCount, LeftSingularVector),
				SingularValue = 0.0,
				RightSingularVector = RightSingularVector0,
				Convergence = tolerance,
				Iterations = Iteration,
				FinalDelta = 0.0
			;   scale_vector(RightProduct, 1.0 / RightNorm, RightSingularVector1),
				stabilize_vector_sign(RightSingularVector1, StableRightSingularVector),
				difference_norm(StableRightSingularVector, RightSingularVector0, Delta),
				^^option(maximum_iterations(MaximumIterations), Options),
				(   Delta =< Tolerance ->
					finalize_singular_triplet(Rows, StableRightSingularVector, Tolerance, SingularValue, LeftSingularVector),
					RightSingularVector = StableRightSingularVector,
					Convergence = tolerance,
					Iterations = Iteration,
					FinalDelta = Delta
				;   Iteration >= MaximumIterations ->
					finalize_singular_triplet(Rows, StableRightSingularVector, Tolerance, SingularValue, LeftSingularVector),
					RightSingularVector = StableRightSingularVector,
					Convergence = maximum_iterations_exhausted,
					Iterations = Iteration,
					FinalDelta = Delta
				;   NextIteration is Iteration + 1,
					iterate_singular_triplet(Rows, Options, NextIteration, StableRightSingularVector, SingularValue, LeftSingularVector, RightSingularVector, Convergence, Iterations, FinalDelta)
				)
			)
		).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	transpose_matrix_vector_product([FirstRow| Rows], LeftSingularVector, RightProduct) :-
		length(FirstRow, FeatureCount),
		zero_vector(FeatureCount, ZeroRightProduct),
		transpose_matrix_vector_product([FirstRow| Rows], LeftSingularVector, ZeroRightProduct, RightProduct).

	transpose_matrix_vector_product([], [], RightProduct, RightProduct).
	transpose_matrix_vector_product([Row| Rows], [LeftValue| LeftSingularVector], RightProduct0, RightProduct) :-
		scale_vector(Row, LeftValue, Contribution),
		add_vectors(RightProduct0, Contribution, RightProduct1),
		transpose_matrix_vector_product(Rows, LeftSingularVector, RightProduct1, RightProduct).

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

	finalize_singular_triplet(Rows, RightSingularVector, Tolerance, SingularValue, LeftSingularVector) :-
		matrix_vector_product(Rows, RightSingularVector, LeftProduct),
		vector_norm(LeftProduct, SingularValue),
		(   SingularValue =< Tolerance ->
			length(Rows, ExampleCount),
			zero_vector(ExampleCount, LeftSingularVector)
		;   scale_vector(LeftProduct, 1.0 / SingularValue, LeftSingularVector)
		).

	deflate_rows([], _SingularValue, [], _RightSingularVector, []).
	deflate_rows([Row| Rows], SingularValue, [LeftValue| LeftSingularVector], RightSingularVector, [DeflatedRow| DeflatedRows]) :-
		Scale is SingularValue * LeftValue,
		scale_vector(RightSingularVector, Scale, ReconstructionRow),
		subtract_vectors(Row, ReconstructionRow, DeflatedRow),
		deflate_rows(Rows, SingularValue, LeftSingularVector, RightSingularVector, DeflatedRows).


	valid_singular_values(Components, SingularValues) :-
		valid(list(number), SingularValues),
		length(Components, Count),
		length(SingularValues, Count),
		valid_singular_value_sequence(SingularValues).

	valid_singular_value_sequence([]).
	valid_singular_value_sequence([SingularValue| SingularValues]) :-
		SingularValue > 0.0,
		valid_singular_value_sequence(SingularValues, SingularValue).

	valid_singular_value_sequence([], _Previous).
	valid_singular_value_sequence([SingularValue| SingularValues], Previous) :-
		SingularValue > 0.0,
		Previous >= SingularValue,
		valid_singular_value_sequence(SingularValues, SingularValue).

	build_diagnostics(AttributeNames, ExampleCount, Components, SingularValues, ComponentDiagnostics, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		length(Components, ComponentCount),
		component_diagnostics(ComponentDiagnostics, Convergences, IterationCounts, FinalDeltas),
		^^option(center(Center), Options),
		^^option(feature_scaling(FeatureScaling), Options),
		Diagnostics = [
			model(truncated_svd),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			sample_count(ExampleCount),
			component_count(ComponentCount),
			singular_values(SingularValues),
			convergence(Convergences),
			iterations(IterationCounts),
			final_delta(FinalDeltas),
			preprocessing([center(Center), feature_scaling(FeatureScaling)])
		].

	component_diagnostics([], [], [], []).
	component_diagnostics([component_diagnostics(Convergence, Iterations, FinalDelta)| ComponentDiagnostics], [Convergence| Convergences], [Iterations| IterationCounts], [FinalDelta| FinalDeltas]) :-
		component_diagnostics(ComponentDiagnostics, Convergences, IterationCounts, FinalDeltas).

	dimension_reducer_data(DimensionReducer, Encoders, Components) :-
		DimensionReducer =.. [_Functor, Encoders, Components| _].

	dimension_reducer_diagnostics_data(truncated_svd_reducer(_Encoders, _Components, _SingularValues, Diagnostics), Diagnostics).

	print_dimension_reducer_properties(truncated_svd_reducer(Encoders, Components, SingularValues, Diagnostics)) :-
		format('Truncated SVD Dimension Reducer~n', []),
		format('===============================~n~n', []),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(Components, ComponentCount),
		format('Components: ~w~n', [ComponentCount]),
		format('Singular values: ~w~n', [SingularValues]).

	default_option(n_components(2)).
	default_option(center(false)).
	default_option(feature_scaling(false)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(center(Center)) :-
		valid(boolean, Center).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

:- end_object.
