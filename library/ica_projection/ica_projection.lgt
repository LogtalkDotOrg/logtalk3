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


:- object(ica_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Independent Component Analysis reducer for continuous datasets using a portable FastICA-style solver with symmetric eigendecomposition whitening.',
		see_also is [nmf_projection, pca_projection, random_projection, truncated_svd_projection]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(numberlist, [
		euclidean_norm/2, rescale/3, scalar_product/3 as dot_product/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(linear_algebra, [
		add_vectors/3, covariance_matrix/2, matrix_vector_product/3, new_vector/3,
		stabilize_vector_sign/2, subtract_vectors/3, symmetric_eigen/5
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
		^^examples_to_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		length(Examples, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		covariance_matrix(Rows, CovarianceMatrix),
		extract_whitening_rows(CovarianceMatrix, ComponentCount, Options, WhiteningRows, WhiteningEigenvalues),
		whiten_rows(Rows, WhiteningRows, WhitenedRows),
		extract_components(WhitenedRows, WhiteningRows, ComponentCount, Options, Components, ComponentDiagnostics),
		build_diagnostics(AttributeNames, SampleCount, Components, WhiteningEigenvalues, ComponentDiagnostics, Options, Diagnostics),
		DimensionReducer = ica_reducer(Encoders, Components, Diagnostics).

	check_dimension_reducer(DimensionReducer) :-
		^^check_dimension_reducer(DimensionReducer),
		(   DimensionReducer = ica_reducer(Encoders, Components, Diagnostics),
			valid_ica_diagnostics(Encoders, Components, Diagnostics) ->
			true
		;   domain_error(dimension_reducer, DimensionReducer)
		).

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

	extract_whitening_rows(CovarianceMatrix, Requested, Options, WhiteningRows, WhiteningEigenvalues) :-
		^^option(tolerance(Tolerance), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		symmetric_eigen(CovarianceMatrix, Tolerance, MaximumIterations, OrderedEigenvectors, OrderedEigenvalues),
		positive_eigenpairs(OrderedEigenvectors, OrderedEigenvalues, Tolerance, PositiveEigenvectors, PositiveEigenvalues),
		length(PositiveEigenvalues, AvailableComponentCount),
		(   Requested =< AvailableComponentCount ->
			whitening_rows(PositiveEigenvectors, PositiveEigenvalues, Requested, WhiteningRows, WhiteningEigenvalues)
		;   domain_error(component_count, Requested-AvailableComponentCount)
		).

	positive_eigenpairs([], [], _Tolerance, [], []).
	positive_eigenpairs([Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues], Tolerance, PositiveEigenvectors, PositiveEigenvalues) :-
		(   Eigenvalue > Tolerance ->
			PositiveEigenvectors = [Eigenvector| PositiveEigenvectors0],
			PositiveEigenvalues = [Eigenvalue| PositiveEigenvalues0],
			positive_eigenpairs(Eigenvectors, Eigenvalues, Tolerance, PositiveEigenvectors0, PositiveEigenvalues0)
		;   PositiveEigenvectors = [],
			PositiveEigenvalues = []
		).

	whitening_rows(_Eigenvectors, _Eigenvalues, 0, [], []) :-
		!.
	whitening_rows([Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues], Requested, [WhiteningRow| WhiteningRows], [Eigenvalue| WhiteningEigenvalues]) :-
		Requested > 0,
		WhiteningScale is 1.0 / sqrt(Eigenvalue),
		rescale(Eigenvector, WhiteningScale, WhiteningRow),
		NextRequested is Requested - 1,
		whitening_rows(Eigenvectors, Eigenvalues, NextRequested, WhiteningRows, WhiteningEigenvalues).

	normalize_nonzero_vector(Vector, NormalizedVector) :-
		euclidean_norm(Vector, Norm),
		Norm > 1.0e-12,
		rescale(Vector, 1.0 / Norm, NormalizedVector).

	whiten_rows([], _WhiteningRows, []).
	whiten_rows([Row| Rows], WhiteningRows, [WhitenedRow| WhitenedRows]) :-
		matrix_vector_product(WhiteningRows, Row, WhitenedRow),
		whiten_rows(Rows, WhiteningRows, WhitenedRows).

	extract_components(WhitenedRows, WhiteningRows, Requested, Options, Components, ComponentDiagnostics) :-
		extract_components(WhitenedRows, WhiteningRows, Requested, Options, [], Components, ComponentDiagnostics).

	extract_components(_WhitenedRows, _WhiteningRows, 0, _Options, _PreviousWhitenedComponents, [], []) :-
		!.
	extract_components(WhitenedRows, WhiteningRows, Requested, Options, PreviousWhitenedComponents, [Component| Components], [component_diagnostics(Convergence, Iterations, FinalDelta)| ComponentDiagnostics]) :-
		fastica_component(WhitenedRows, Options, PreviousWhitenedComponents, WhiteningVector, Convergence, Iterations, FinalDelta),
		unmixing_component(WhiteningVector, WhiteningRows, Component),
		NextRequested is Requested - 1,
		extract_components(WhitenedRows, WhiteningRows, NextRequested, Options, [WhiteningVector| PreviousWhitenedComponents], Components, ComponentDiagnostics).

	fastica_component(WhitenedRows, Options, PreviousWhitenedComponents, BestVector, Convergence, Iterations, FinalDelta) :-
		WhitenedRows = [FirstRow| _],
		length(FirstRow, Size),
		^^initial_vectors(Size, InitialVectors),
		fastica_component_candidates(WhitenedRows, Options, PreviousWhitenedComponents, InitialVectors, -1.0, [], tolerance, 0, 0.0, BestVector, Convergence, Iterations, FinalDelta),
		BestVector \== [].

	fastica_component_candidates(_WhitenedRows, _Options, _PreviousWhitenedComponents, [], _BestScore, BestVector, BestConvergence, BestIterations, BestFinalDelta, BestVector, BestConvergence, BestIterations, BestFinalDelta) :-
		!.
	fastica_component_candidates(WhitenedRows, Options, PreviousWhitenedComponents, [InitialVector| InitialVectors], BestScore0, BestVector0, BestConvergence0, BestIterations0, BestFinalDelta0, BestVector, BestConvergence, BestIterations, BestFinalDelta) :-
		(   normalize_nonzero_vector(InitialVector, NormalizedInitial),
			orthonormalize_vector(NormalizedInitial, PreviousWhitenedComponents, OrthogonalInitial),
			normalize_nonzero_vector(OrthogonalInitial, StartVector),
			iterate_fastica(WhitenedRows, Options, PreviousWhitenedComponents, 0, StartVector, CandidateVector, CandidateConvergence, CandidateIterations, CandidateFinalDelta),
			kurtosis_score(WhitenedRows, CandidateVector, CandidateScore) ->
			(   CandidateScore > BestScore0 ->
				BestScore1 = CandidateScore,
				BestVector1 = CandidateVector,
				BestConvergence1 = CandidateConvergence,
				BestIterations1 = CandidateIterations,
				BestFinalDelta1 = CandidateFinalDelta
			;   BestScore1 = BestScore0,
				BestVector1 = BestVector0,
				BestConvergence1 = BestConvergence0,
				BestIterations1 = BestIterations0,
				BestFinalDelta1 = BestFinalDelta0
			)
		;   BestScore1 = BestScore0,
			BestVector1 = BestVector0,
			BestConvergence1 = BestConvergence0,
			BestIterations1 = BestIterations0,
			BestFinalDelta1 = BestFinalDelta0
		),
		fastica_component_candidates(WhitenedRows, Options, PreviousWhitenedComponents, InitialVectors, BestScore1, BestVector1, BestConvergence1, BestIterations1, BestFinalDelta1, BestVector, BestConvergence, BestIterations, BestFinalDelta).

	iterate_fastica(WhitenedRows, Options, PreviousWhitenedComponents, Iteration0, Vector0, Vector, Convergence, Iterations, FinalDelta) :-
		fastica_update(WhitenedRows, PreviousWhitenedComponents, Vector0, Vector1, Delta),
		Iteration is Iteration0 + 1,
		^^option(tolerance(Tolerance), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Delta =< Tolerance ->
			Vector = Vector1,
			Convergence = tolerance,
			Iterations = Iteration,
			FinalDelta = Delta
		;   Iteration >= MaximumIterations ->
			Vector = Vector1,
			Convergence = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDelta = Delta
		;   iterate_fastica(WhitenedRows, Options, PreviousWhitenedComponents, Iteration, Vector1, Vector, Convergence, Iterations, FinalDelta)
		).

	fastica_update(WhitenedRows, PreviousWhitenedComponents, Vector0, Vector, Delta) :-
		row_projections(WhitenedRows, Vector0, Projections),
		cubic_expectation(WhitenedRows, Projections, Numerator),
		projection_square_mean(Projections, SquareMean),
		CorrectionScale is 3.0 * SquareMean,
		rescale(Vector0, CorrectionScale, Correction),
		subtract_vectors(Numerator, Correction, Updated0),
		orthonormalize_vector(Updated0, PreviousWhitenedComponents, Updated1),
		normalize_nonzero_vector(Updated1, NormalizedUpdated),
		stabilize_vector_sign(NormalizedUpdated, Vector),
		alignment_delta(Vector, Vector0, Delta).

	row_projections([], _Vector, []).
	row_projections([Row| Rows], Vector, [Projection| Projections]) :-
		dot_product(Row, Vector, Projection),
		row_projections(Rows, Vector, Projections).

	cubic_expectation([FirstRow| Rows], Projections, Expectation) :-
		length(FirstRow, Size),
		new_vector(Size, 0.0, ZeroExpectation),
		cubic_expectation([FirstRow| Rows], Projections, ZeroExpectation, SumExpectation),
		length([FirstRow| Rows], Count),
		Scale is 1.0 / Count,
		rescale(SumExpectation, Scale, Expectation).

	cubic_expectation([], [], Expectation, Expectation).
	cubic_expectation([Row| Rows], [Projection| Projections], Expectation0, Expectation) :-
		Weight is Projection * Projection * Projection,
		rescale(Row, Weight, Contribution),
		add_vectors(Expectation0, Contribution, Expectation1),
		cubic_expectation(Rows, Projections, Expectation1, Expectation).

	projection_square_mean(Projections, Mean) :-
		projection_square_mean(Projections, 0.0, SumSquares),
		length(Projections, Count),
		Mean is SumSquares / Count.

	projection_square_mean([], SumSquares, SumSquares).
	projection_square_mean([Projection| Projections], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Projection * Projection,
		projection_square_mean(Projections, SumSquares1, SumSquares).

	projection_fourth_mean(Projections, Mean) :-
		projection_fourth_mean(Projections, 0.0, SumFourth),
		length(Projections, Count),
		Mean is SumFourth / Count.

	projection_fourth_mean([], SumFourth, SumFourth).
	projection_fourth_mean([Projection| Projections], SumFourth0, SumFourth) :-
		ProjectionSquared is Projection * Projection,
		SumFourth1 is SumFourth0 + ProjectionSquared * ProjectionSquared,
		projection_fourth_mean(Projections, SumFourth1, SumFourth).

	kurtosis_score(WhitenedRows, Vector, Score) :-
		row_projections(WhitenedRows, Vector, Projections),
		projection_square_mean(Projections, SquareMean),
		projection_fourth_mean(Projections, FourthMean),
		Score is abs(FourthMean - 3.0 * SquareMean * SquareMean).

	orthonormalize_vector(Vector, [], Vector).
	orthonormalize_vector(Vector0, [PreviousVector| PreviousWhitenedComponents], Vector) :-
		dot_product(Vector0, PreviousVector, Projection),
		rescale(PreviousVector, Projection, Contribution),
		subtract_vectors(Vector0, Contribution, Vector1),
		orthonormalize_vector(Vector1, PreviousWhitenedComponents, Vector).

	alignment_delta(Vector1, Vector2, Delta) :-
		dot_product(Vector1, Vector2, Alignment),
		Delta is max(0.0, 1.0 - abs(Alignment)).

	unmixing_component(WhiteningVector, WhiteningRows, Component) :-
		^^zero_vector_like(WhiteningRows, ZeroComponent),
		accumulate_weighted_rows(WhiteningVector, WhiteningRows, ZeroComponent, Component).

	accumulate_weighted_rows([], [], Component, Component).
	accumulate_weighted_rows([Weight| Weights], [Row| Rows], Component0, Component) :-
		rescale(Row, Weight, Contribution),
		add_vectors(Component0, Contribution, Component1),
		accumulate_weighted_rows(Weights, Rows, Component1, Component).

	valid_ica_diagnostics(Encoders, Components, Diagnostics) :-
		memberchk(model(ica_projection), Diagnostics),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		length(Encoders, FeatureCount),
		length(AttributeNames, FeatureCount),
		memberchk(feature_count(FeatureCount), Diagnostics),
		length(Components, ComponentCount),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		valid(positive_integer, SampleCount),
		memberchk(whitening_eigenvalues(WhiteningEigenvalues), Diagnostics),
		length(WhiteningEigenvalues, ComponentCount),
		valid_positive_descending_numbers(WhiteningEigenvalues),
		memberchk(convergence(Convergences), Diagnostics),
		length(Convergences, ComponentCount),
		valid_convergences(Convergences),
		memberchk(iterations(IterationCounts), Diagnostics),
		length(IterationCounts, ComponentCount),
		valid(list(non_negative_integer), IterationCounts),
		memberchk(final_delta(FinalDeltas), Diagnostics),
		length(FinalDeltas, ComponentCount),
		valid(list(non_negative_number), FinalDeltas),
		memberchk(preprocessing([center(true), feature_scaling(FeatureScaling)]), Diagnostics),
		valid(boolean, FeatureScaling).

	valid_positive_descending_numbers([]).
	valid_positive_descending_numbers([Number| Numbers]) :-
		Number > 0.0,
		valid_positive_descending_numbers(Numbers, Number).

	valid_positive_descending_numbers([], _Previous).
	valid_positive_descending_numbers([Number| Numbers], Previous) :-
		Number > 0.0,
		Previous >= Number,
		valid_positive_descending_numbers(Numbers, Number).

	valid_convergences([]).
	valid_convergences([Convergence| Convergences]) :-
		once((
			Convergence == tolerance
		;	Convergence == maximum_iterations_exhausted
		)),
		valid_convergences(Convergences).

	build_diagnostics(AttributeNames, SampleCount, Components, WhiteningEigenvalues, ComponentDiagnostics, Options, Diagnostics) :-
		^^component_iteration_diagnostics(ComponentDiagnostics, Convergences, IterationCounts, FinalDeltas),
		^^preprocessing_diagnostics(true, Options, Preprocessing),
		^^iterative_dimension_reducer_diagnostics(
			ica_projection,
			AttributeNames,
			Components,
			SampleCount,
			Options,
			[whitening_eigenvalues(WhiteningEigenvalues)],
			Convergences,
			IterationCounts,
			FinalDeltas,
			[preprocessing(Preprocessing)],
			Diagnostics
		).

	print_dimension_reducer_properties(ica_reducer(Encoders, Components, Diagnostics)) :-
		format('ICA Dimension Reducer~n', []),
		format('=====================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components).

	default_option(n_components(2)).
	default_option(feature_scaling(false)).
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
