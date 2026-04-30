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


:- object(ica,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Independent Component Analysis reducer for continuous datasets using a portable FastICA-style solver with symmetric Jacobi whitening.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, whitens the covariance matrix using a deterministic symmetric Jacobi eigendecomposition, and then extracts independent components using a deterministic cubic FastICA fixed-point iteration with orthogonal deflation.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``ica_reducer(Encoders, Components, Diagnostics)`` where ``Encoders`` stores preprocessing metadata, ``Components`` stores the learned unmixing vectors in feature space, and ``Diagnostics`` records whitening and FastICA metadata.'
		],
		see_also is [nmf, pca, random_projection, truncated_svd]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2, reverse/2
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
		^^examples_to_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		length(Examples, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		^^covariance_matrix(Rows, CovarianceMatrix),
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
		jacobi_eigenpairs(CovarianceMatrix, Tolerance, Eigenpairs),
		positive_eigenpairs(Eigenpairs, Tolerance, PositiveEigenpairs),
		length(PositiveEigenpairs, AvailableComponentCount),
		(   Requested =< AvailableComponentCount ->
			whitening_rows(PositiveEigenpairs, Requested, WhiteningRows, WhiteningEigenvalues)
		;   domain_error(component_count, Requested-AvailableComponentCount)
		).

	jacobi_eigenpairs(Matrix, Tolerance, Eigenpairs) :-
		length(Matrix, Size),
		^^basis_initial_vectors(1, Size, Eigenvectors0),
		jacobi_maximum_sweeps(Size, MaximumSweeps),
		jacobi_sweeps(Matrix, Eigenvectors0, Size, Tolerance, MaximumSweeps, DiagonalMatrix, Eigenvectors),
		diagonal_values(DiagonalMatrix, Eigenvalues),
		^^transpose_matrix(Eigenvectors, EigenvectorColumns),
		eigenpairs(Eigenvalues, EigenvectorColumns, Eigenpairs0),
		sort_eigenpairs_descending(Eigenpairs0, Eigenpairs).

	jacobi_maximum_sweeps(Size, MaximumSweeps) :-
		MaximumSweeps is max(1, 8 * Size * Size).

	jacobi_sweeps(Matrix, Eigenvectors, _Size, Tolerance, _RemainingSweeps, Matrix, Eigenvectors) :-
		max_off_diagonal(Matrix, MaximumOffDiagonal),
		MaximumOffDiagonal =< Tolerance,
		!.
	jacobi_sweeps(Matrix, Eigenvectors, _Size, _Tolerance, 0, Matrix, Eigenvectors) :-
		!.
	jacobi_sweeps(Matrix0, Eigenvectors0, Size, Tolerance, RemainingSweeps, Matrix, Eigenvectors) :-
		jacobi_sweep_pairs(1, 2, Size, Tolerance, Matrix0, Eigenvectors0, Matrix1, Eigenvectors1),
		NextRemainingSweeps is RemainingSweeps - 1,
		jacobi_sweeps(Matrix1, Eigenvectors1, Size, Tolerance, NextRemainingSweeps, Matrix, Eigenvectors).

	jacobi_sweep_pairs(Pivot, _Partner, Size, _Tolerance, Matrix, Eigenvectors, Matrix, Eigenvectors) :-
		Pivot >= Size,
		!.
	jacobi_sweep_pairs(Pivot, Partner, Size, Tolerance, Matrix0, Eigenvectors0, Matrix, Eigenvectors) :-
		(   Partner > Size ->
			NextPivot is Pivot + 1,
			NextPartner is NextPivot + 1,
			jacobi_sweep_pairs(NextPivot, NextPartner, Size, Tolerance, Matrix0, Eigenvectors0, Matrix, Eigenvectors)
		;   jacobi_rotate_if_needed(Matrix0, Eigenvectors0, Pivot, Partner, Tolerance, Matrix1, Eigenvectors1),
			NextPartner is Partner + 1,
			jacobi_sweep_pairs(Pivot, NextPartner, Size, Tolerance, Matrix1, Eigenvectors1, Matrix, Eigenvectors)
		).

	jacobi_rotate_if_needed(Matrix0, Eigenvectors0, Pivot, Partner, Tolerance, Matrix, Eigenvectors) :-
		^^matrix_value(Matrix0, Pivot, Partner, OffDiagonal),
		(   abs(OffDiagonal) =< Tolerance ->
			Matrix = Matrix0,
			Eigenvectors = Eigenvectors0
		;   jacobi_rotation(Matrix0, Pivot, Partner, Cosine, Sine),
			rotate_matrix(Matrix0, Pivot, Partner, Cosine, Sine, Matrix),
			rotate_eigenvectors(Eigenvectors0, Pivot, Partner, Cosine, Sine, Eigenvectors)
		).

	jacobi_rotation(Matrix, Pivot, Partner, Cosine, Sine) :-
		^^matrix_value(Matrix, Pivot, Pivot, PivotDiagonal),
		^^matrix_value(Matrix, Partner, Partner, PartnerDiagonal),
		^^matrix_value(Matrix, Pivot, Partner, OffDiagonal),
		HalfDelta is (PartnerDiagonal - PivotDiagonal) / (2.0 * OffDiagonal),
		(   abs(HalfDelta) =< 1.0e-12 ->
			Tangent = 1.0
		;   Sign is HalfDelta / abs(HalfDelta),
			Tangent is Sign / (abs(HalfDelta) + sqrt(1.0 + HalfDelta * HalfDelta))
		),
		Cosine is 1.0 / sqrt(1.0 + Tangent * Tangent),
		Sine is Tangent * Cosine.

	rotate_matrix(Matrix0, Pivot, Partner, Cosine, Sine, Matrix) :-
		length(Matrix0, Size),
		rotate_matrix_rows(1, Size, Matrix0, Pivot, Partner, Cosine, Sine, Matrix).

	rotate_matrix_rows(RowIndex, Size, _Matrix0, _Pivot, _Partner, _Cosine, _Sine, []) :-
		RowIndex > Size,
		!.
	rotate_matrix_rows(RowIndex, Size, Matrix0, Pivot, Partner, Cosine, Sine, [Row| Matrix]) :-
		rotate_matrix_columns(RowIndex, 1, Size, Matrix0, Pivot, Partner, Cosine, Sine, Row),
		NextRowIndex is RowIndex + 1,
		rotate_matrix_rows(NextRowIndex, Size, Matrix0, Pivot, Partner, Cosine, Sine, Matrix).

	rotate_matrix_columns(_RowIndex, ColumnIndex, Size, _Matrix0, _Pivot, _Partner, _Cosine, _Sine, []) :-
		ColumnIndex > Size,
		!.
	rotate_matrix_columns(RowIndex, ColumnIndex, Size, Matrix0, Pivot, Partner, Cosine, Sine, [Value| Row]) :-
		rotated_matrix_value(Matrix0, Pivot, Partner, Cosine, Sine, RowIndex, ColumnIndex, Value),
		NextColumnIndex is ColumnIndex + 1,
		rotate_matrix_columns(RowIndex, NextColumnIndex, Size, Matrix0, Pivot, Partner, Cosine, Sine, Row).

	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, Pivot, Pivot, Value) :-
		!,
		^^matrix_value(Matrix, Pivot, Pivot, PivotDiagonal),
		^^matrix_value(Matrix, Partner, Partner, PartnerDiagonal),
		^^matrix_value(Matrix, Pivot, Partner, OffDiagonal),
		Value is Cosine * Cosine * PivotDiagonal - 2.0 * Sine * Cosine * OffDiagonal + Sine * Sine * PartnerDiagonal.
	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, Partner, Partner, Value) :-
		!,
		^^matrix_value(Matrix, Pivot, Pivot, PivotDiagonal),
		^^matrix_value(Matrix, Partner, Partner, PartnerDiagonal),
		^^matrix_value(Matrix, Pivot, Partner, OffDiagonal),
		Value is Sine * Sine * PivotDiagonal + 2.0 * Sine * Cosine * OffDiagonal + Cosine * Cosine * PartnerDiagonal.
	rotated_matrix_value(_Matrix, Pivot, Partner, _Cosine, _Sine, Pivot, Partner, 0.0) :-
		!.
	rotated_matrix_value(_Matrix, Pivot, Partner, _Cosine, _Sine, Partner, Pivot, 0.0) :-
		!.
	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, Pivot, ColumnIndex, Value) :-
		ColumnIndex =\= Pivot,
		ColumnIndex =\= Partner,
		!,
		^^matrix_value(Matrix, Pivot, ColumnIndex, PivotValue),
		^^matrix_value(Matrix, Partner, ColumnIndex, PartnerValue),
		Value is Cosine * PivotValue - Sine * PartnerValue.
	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, Partner, ColumnIndex, Value) :-
		ColumnIndex =\= Pivot,
		ColumnIndex =\= Partner,
		!,
		^^matrix_value(Matrix, Pivot, ColumnIndex, PivotValue),
		^^matrix_value(Matrix, Partner, ColumnIndex, PartnerValue),
		Value is Sine * PivotValue + Cosine * PartnerValue.
	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, RowIndex, Pivot, Value) :-
		RowIndex =\= Pivot,
		RowIndex =\= Partner,
		!,
		^^matrix_value(Matrix, RowIndex, Pivot, PivotValue),
		^^matrix_value(Matrix, RowIndex, Partner, PartnerValue),
		Value is Cosine * PivotValue - Sine * PartnerValue.
	rotated_matrix_value(Matrix, Pivot, Partner, Cosine, Sine, RowIndex, Partner, Value) :-
		RowIndex =\= Pivot,
		RowIndex =\= Partner,
		!,
		^^matrix_value(Matrix, RowIndex, Pivot, PivotValue),
		^^matrix_value(Matrix, RowIndex, Partner, PartnerValue),
		Value is Sine * PivotValue + Cosine * PartnerValue.
	rotated_matrix_value(Matrix, _Pivot, _Partner, _Cosine, _Sine, RowIndex, ColumnIndex, Value) :-
		^^matrix_value(Matrix, RowIndex, ColumnIndex, Value).

	rotate_eigenvectors(Eigenvectors0, Pivot, Partner, Cosine, Sine, Eigenvectors) :-
		length(Eigenvectors0, Size),
		rotate_eigenvector_rows(1, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, Eigenvectors).

	rotate_eigenvector_rows(RowIndex, Size, _Eigenvectors0, _Pivot, _Partner, _Cosine, _Sine, []) :-
		RowIndex > Size,
		!.
	rotate_eigenvector_rows(RowIndex, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, [Row| Eigenvectors]) :-
		rotate_eigenvector_columns(RowIndex, 1, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, Row),
		NextRowIndex is RowIndex + 1,
		rotate_eigenvector_rows(NextRowIndex, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, Eigenvectors).

	rotate_eigenvector_columns(_RowIndex, ColumnIndex, Size, _Eigenvectors0, _Pivot, _Partner, _Cosine, _Sine, []) :-
		ColumnIndex > Size,
		!.
	rotate_eigenvector_columns(RowIndex, ColumnIndex, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, [Value| Row]) :-
		rotated_eigenvector_value(Eigenvectors0, Pivot, Partner, Cosine, Sine, RowIndex, ColumnIndex, Value),
		NextColumnIndex is ColumnIndex + 1,
		rotate_eigenvector_columns(RowIndex, NextColumnIndex, Size, Eigenvectors0, Pivot, Partner, Cosine, Sine, Row).

	rotated_eigenvector_value(Eigenvectors, Pivot, Partner, Cosine, Sine, RowIndex, Pivot, Value) :-
		!,
		^^matrix_value(Eigenvectors, RowIndex, Pivot, PivotValue),
		^^matrix_value(Eigenvectors, RowIndex, Partner, PartnerValue),
		Value is Cosine * PivotValue - Sine * PartnerValue.
	rotated_eigenvector_value(Eigenvectors, Pivot, Partner, Cosine, Sine, RowIndex, Partner, Value) :-
		!,
		^^matrix_value(Eigenvectors, RowIndex, Pivot, PivotValue),
		^^matrix_value(Eigenvectors, RowIndex, Partner, PartnerValue),
		Value is Sine * PivotValue + Cosine * PartnerValue.
	rotated_eigenvector_value(Eigenvectors, _Pivot, _Partner, _Cosine, _Sine, RowIndex, ColumnIndex, Value) :-
		^^matrix_value(Eigenvectors, RowIndex, ColumnIndex, Value).

	max_off_diagonal(Matrix, MaximumOffDiagonal) :-
		length(Matrix, Size),
		max_off_diagonal(1, 2, Size, Matrix, 0.0, MaximumOffDiagonal).

	max_off_diagonal(Pivot, _Partner, Size, _Matrix, Maximum0, Maximum0) :-
		Pivot >= Size,
		!.
	max_off_diagonal(Pivot, Partner, Size, Matrix, Maximum0, Maximum) :-
		(   Partner > Size ->
			NextPivot is Pivot + 1,
			NextPartner is NextPivot + 1,
			max_off_diagonal(NextPivot, NextPartner, Size, Matrix, Maximum0, Maximum)
		;   ^^matrix_value(Matrix, Pivot, Partner, Value),
			Maximum1 is max(Maximum0, abs(Value)),
			NextPartner is Partner + 1,
			max_off_diagonal(Pivot, NextPartner, Size, Matrix, Maximum1, Maximum)
		).

	diagonal_values(Matrix, Values) :-
		length(Matrix, Size),
		diagonal_values(1, Size, Matrix, Values).

	diagonal_values(Index, Size, _Matrix, []) :-
		Index > Size,
		!.
	diagonal_values(Index, Size, Matrix, [Value| Values]) :-
		^^matrix_value(Matrix, Index, Index, Value),
		NextIndex is Index + 1,
		diagonal_values(NextIndex, Size, Matrix, Values).

	eigenpairs([], [], []).
	eigenpairs([Eigenvalue| Eigenvalues], [Eigenvector| Eigenvectors], [eigenpair(Eigenvalue, Eigenvector)| Eigenpairs]) :-
		eigenpairs(Eigenvalues, Eigenvectors, Eigenpairs).

	sort_eigenpairs_descending([], []).
	sort_eigenpairs_descending([Eigenpair| Eigenpairs], SortedEigenpairs) :-
		sort_eigenpairs_descending(Eigenpairs, SortedEigenpairs0),
		insert_eigenpair_descending(Eigenpair, SortedEigenpairs0, SortedEigenpairs).

	insert_eigenpair_descending(eigenpair(Eigenvalue, Eigenvector), [], [eigenpair(Eigenvalue, Eigenvector)]) :-
		!.
	insert_eigenpair_descending(eigenpair(Eigenvalue, Eigenvector), [eigenpair(OtherEigenvalue, OtherEigenvector)| Eigenpairs], [eigenpair(Eigenvalue, Eigenvector), eigenpair(OtherEigenvalue, OtherEigenvector)| Eigenpairs]) :-
		Eigenvalue >= OtherEigenvalue,
		!.
	insert_eigenpair_descending(Eigenpair, [OtherEigenpair| Eigenpairs], [OtherEigenpair| SortedEigenpairs]) :-
		insert_eigenpair_descending(Eigenpair, Eigenpairs, SortedEigenpairs).

	positive_eigenpairs([], _Tolerance, []).
	positive_eigenpairs([eigenpair(Eigenvalue, Eigenvector)| Eigenpairs], Tolerance, [eigenpair(Eigenvalue, Eigenvector)| PositiveEigenpairs]) :-
		Eigenvalue > Tolerance,
		!,
		positive_eigenpairs(Eigenpairs, Tolerance, PositiveEigenpairs).
	positive_eigenpairs([_Eigenpair| Eigenpairs], Tolerance, PositiveEigenpairs) :-
		positive_eigenpairs(Eigenpairs, Tolerance, PositiveEigenpairs).

	whitening_rows(_Eigenpairs, 0, [], []) :-
		!.
	whitening_rows([eigenpair(Eigenvalue, Eigenvector)| Eigenpairs], Requested, [WhiteningRow| WhiteningRows], [Eigenvalue| WhiteningEigenvalues]) :-
		Requested > 0,
		WhiteningScale is 1.0 / sqrt(Eigenvalue),
		^^scale_vector(Eigenvector, WhiteningScale, WhiteningRow),
		NextRequested is Requested - 1,
		whitening_rows(Eigenpairs, NextRequested, WhiteningRows, WhiteningEigenvalues).

	normalize_nonzero_vector(Vector, NormalizedVector) :-
		^^vector_norm(Vector, Norm),
		Norm > 1.0e-12,
		^^scale_vector(Vector, 1.0 / Norm, NormalizedVector).

	whiten_rows([], _WhiteningRows, []).
	whiten_rows([Row| Rows], WhiteningRows, [WhitenedRow| WhitenedRows]) :-
		^^matrix_vector_product(WhiteningRows, Row, WhitenedRow),
		whiten_rows(Rows, WhiteningRows, WhitenedRows).

	extract_components(WhitenedRows, WhiteningRows, Requested, Options, Components, ComponentDiagnostics) :-
		extract_components(WhitenedRows, WhiteningRows, Requested, Options, [], [], [], Components, ComponentDiagnostics).

	extract_components(_WhitenedRows, _WhiteningRows, 0, _Options, _PreviousWhitenedComponents, ComponentsAcc, DiagnosticsAcc, Components, ComponentDiagnostics) :-
		!,
		reverse(ComponentsAcc, Components),
		reverse(DiagnosticsAcc, ComponentDiagnostics).
	extract_components(WhitenedRows, WhiteningRows, Requested, Options, PreviousWhitenedComponents, ComponentsAcc, DiagnosticsAcc, Components, ComponentDiagnostics) :-
		fastica_component(WhitenedRows, Options, PreviousWhitenedComponents, WhiteningVector, Convergence, Iterations, FinalDelta),
		unmixing_component(WhiteningVector, WhiteningRows, Component),
		NextRequested is Requested - 1,
		extract_components(WhitenedRows, WhiteningRows, NextRequested, Options, [WhiteningVector| PreviousWhitenedComponents], [Component| ComponentsAcc], [component_diagnostics(Convergence, Iterations, FinalDelta)| DiagnosticsAcc], Components, ComponentDiagnostics).

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
		^^scale_vector(Vector0, CorrectionScale, Correction),
		^^subtract_vectors(Numerator, Correction, Updated0),
		orthonormalize_vector(Updated0, PreviousWhitenedComponents, Updated1),
		normalize_nonzero_vector(Updated1, NormalizedUpdated),
		^^stabilize_vector_sign(NormalizedUpdated, Vector),
		alignment_delta(Vector, Vector0, Delta).

	row_projections([], _Vector, []).
	row_projections([Row| Rows], Vector, [Projection| Projections]) :-
		dot_product(Row, Vector, Projection),
		row_projections(Rows, Vector, Projections).

	cubic_expectation([FirstRow| Rows], Projections, Expectation) :-
		length(FirstRow, Size),
		^^zero_vector(Size, ZeroExpectation),
		cubic_expectation([FirstRow| Rows], Projections, ZeroExpectation, SumExpectation),
		length([FirstRow| Rows], Count),
		Scale is 1.0 / Count,
		^^scale_vector(SumExpectation, Scale, Expectation).

	cubic_expectation([], [], Expectation, Expectation).
	cubic_expectation([Row| Rows], [Projection| Projections], Expectation0, Expectation) :-
		Weight is Projection * Projection * Projection,
		^^scale_vector(Row, Weight, Contribution),
		^^add_vectors(Expectation0, Contribution, Expectation1),
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
		^^scale_vector(PreviousVector, Projection, Contribution),
		^^subtract_vectors(Vector0, Contribution, Vector1),
		orthonormalize_vector(Vector1, PreviousWhitenedComponents, Vector).

	alignment_delta(Vector1, Vector2, Delta) :-
		dot_product(Vector1, Vector2, Alignment),
		Delta is max(0.0, 1.0 - abs(Alignment)).

	unmixing_component(WhiteningVector, WhiteningRows, Component) :-
		^^zero_vector_like(WhiteningRows, ZeroComponent),
		accumulate_weighted_rows(WhiteningVector, WhiteningRows, ZeroComponent, Component).

	accumulate_weighted_rows([], [], Component, Component).
	accumulate_weighted_rows([Weight| Weights], [Row| Rows], Component0, Component) :-
		^^scale_vector(Row, Weight, Contribution),
		^^add_vectors(Component0, Contribution, Component1),
		accumulate_weighted_rows(Weights, Rows, Component1, Component).

	valid_ica_diagnostics(Encoders, Components, Diagnostics) :-
		memberchk(model(ica), Diagnostics),
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
			ica,
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
