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


:- object(nmf,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-28,
		comment is 'Non-negative Matrix Factorization dimension reduction for non-negative continuous datasets.'
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, memberchk/2, take/3
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		attribute_names(Attributes, AttributeNames),
		findall(example(Id, AttributeValues), Dataset::example(Id, AttributeValues), Examples),
		^^check_examples_non_empty(Dataset, Examples),
		^^check_example_values(Examples, AttributeNames),
		check_non_negative_examples(Examples, AttributeNames),
		build_non_negative_encoders(AttributeNames, Examples, Options, Encoders),
		example_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		length(Rows, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(FeatureCount, SampleCount),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		initialize_coefficients(SampleCount, ComponentCount, Coefficients0),
		initialize_basis(Rows, ComponentCount, Components0),
		train_factorization(Rows, Coefficients0, Components0, Options, Convergence, Iterations, FinalDelta, Coefficients, Components),
		reconstruction_error(Rows, Coefficients, Components, ReconstructionError),
		build_diagnostics(AttributeNames, SampleCount, Components, Options, Convergence, Iterations, FinalDelta, ReconstructionError, Diagnostics),
		DimensionReducer = nmf_reducer(Encoders, Components, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = nmf_reducer(Encoders, Components, Diagnostics),
		check_non_negative_instance(Encoders, Instance),
		^^encode_instance(Encoders, Instance, Features),
		infer_weights(Features, Components, Diagnostics, Weights),
		weights_pairs(Weights, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		^^check_dimension_reducer(DimensionReducer),
		(   DimensionReducer = nmf_reducer(Encoders, Components, Diagnostics),
			valid_non_negative_components(Components),
			valid_nmf_diagnostics(Encoders, Components, Diagnostics) ->
			true
		;   domain_error(valid_dimension_reducer, DimensionReducer)
		).

	print_dimension_reducer_properties(nmf_reducer(Encoders, Components, Diagnostics)) :-
		format('NMF Dimension Reducer~n', []),
		format('=====================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components).

	example_attribute_values(example(_Id, AttributeValues), AttributeValues).

	attribute_names([], []).
	attribute_names([Attribute-_Values| Attributes], [Attribute| AttributeNames]) :-
		attribute_names(Attributes, AttributeNames).

	check_non_negative_examples([], _AttributeNames).
	check_non_negative_examples([Example| Examples], AttributeNames) :-
		::example_attribute_values(Example, AttributeValues),
		check_non_negative_attribute_values(AttributeNames, AttributeValues),
		check_non_negative_examples(Examples, AttributeNames).

	check_non_negative_instance(Encoders, AttributeValues) :-
		^^encoder_attribute_names(Encoders, AttributeNames),
		check_non_negative_attribute_values(AttributeNames, AttributeValues).

	check_non_negative_attribute_values([], _AttributeValues).
	check_non_negative_attribute_values([Attribute| Attributes], AttributeValues) :-
		^^attribute_value(Attribute, AttributeValues, Value),
		(   Value >= 0.0 ->
			true
		;   domain_error(non_negative_attribute, Attribute-Value)
		),
		check_non_negative_attribute_values(Attributes, AttributeValues).

	build_non_negative_encoders([], _Examples, _Options, []).
	build_non_negative_encoders([Attribute| Attributes], Examples, Options, [continuous(Attribute, 0.0, Scale)| Encoders]) :-
		^^known_attribute_values(Examples, Attribute, Values),
		non_negative_scale(Values, Options, Scale),
		build_non_negative_encoders(Attributes, Examples, Options, Encoders).

	non_negative_scale(Values, Options, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
			max_value(Values, Maximum),
			(   Maximum > 0.0 ->
				Scale = Maximum
			;   Scale = 1.0
			)
		;   Scale = 1.0
		).

	max_value([Value| Values], Maximum) :-
		max_value(Values, Value, Maximum).

	max_value([], Maximum, Maximum).
	max_value([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Value, Maximum0),
		max_value(Values, Maximum1, Maximum).

	example_rows([], _Encoders, []).
	example_rows([Example| Examples], Encoders, [Row| Rows]) :-
		::example_attribute_values(Example, AttributeValues),
		^^encode_instance(Encoders, AttributeValues, Row),
		example_rows(Examples, Encoders, Rows).

	initialize_coefficients(RowCount, ComponentCount, Coefficients) :-
		^^make_matrix(RowCount, ComponentCount, 1.0, Coefficients).

	initialize_basis(Rows, ComponentCount, Components) :-
		take(ComponentCount, Rows, SeedRows),
		positive_floor_matrix(SeedRows, Components).

	positive_floor_matrix([], []).
	positive_floor_matrix([Row| Rows], [PositiveRow| PositiveRows]) :-
		positive_floor_row(Row, PositiveRow),
		positive_floor_matrix(Rows, PositiveRows).

	positive_floor_row([], []).
	positive_floor_row([Value| Values], [PositiveValue| PositiveValues]) :-
		positive_floor(Value, PositiveValue),
		positive_floor_row(Values, PositiveValues).

	positive_floor(Value, PositiveValue) :-
		(   Value > 0.0 ->
			PositiveValue = Value
		;   PositiveValue = 1.0e-12
		).

	train_factorization(Rows, Coefficients0, Components0, Options, Convergence, Iterations, FinalDelta, Coefficients, Components) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		iterate_factorization(Rows, MaximumIterations, Tolerance, 0, Coefficients0, Components0, Convergence, Iterations, FinalDelta, Coefficients, Components).

	iterate_factorization(Rows, MaximumIterations, Tolerance, Iteration0, Coefficients0, Components0, Convergence, Iterations, FinalDelta, Coefficients, Components) :-
		update_components(Rows, Coefficients0, Components0, Components1, ComponentsDelta),
		update_coefficients(Rows, Coefficients0, Components1, Coefficients1, CoefficientsDelta),
		CurrentDelta is max(ComponentsDelta, CoefficientsDelta),
		Iteration is Iteration0 + 1,
		(   CurrentDelta =< Tolerance ->
			Convergence = tolerance,
			Iterations = Iteration,
			FinalDelta = CurrentDelta,
			Coefficients = Coefficients1,
			Components = Components1
		;   Iteration >= MaximumIterations ->
			Convergence = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDelta = CurrentDelta,
			Coefficients = Coefficients1,
			Components = Components1
		;   iterate_factorization(Rows, MaximumIterations, Tolerance, Iteration, Coefficients1, Components1, Convergence, Iterations, FinalDelta, Coefficients, Components)
		).

	update_components(Rows, Coefficients, Components0, Components, Delta) :-
		reconstruct_rows(Coefficients, Components0, ApproximationRows),
		^^transpose_matrix(Coefficients, CoefficientColumns),
		^^transpose_matrix(Rows, FeatureColumns),
		^^transpose_matrix(ApproximationRows, ApproximationColumns),
		update_component_rows(Components0, CoefficientColumns, FeatureColumns, ApproximationColumns, 0.0, Components, Delta).

	update_coefficients(Rows, Coefficients0, Components, Coefficients, Delta) :-
		reconstruct_rows(Coefficients0, Components, ApproximationRows),
		update_coefficient_rows(Coefficients0, Rows, Components, ApproximationRows, 0.0, Coefficients, Delta).

	infer_weights(_Features, [], _Diagnostics, []) :-
		!.
	infer_weights(Features, Components, Diagnostics, Weights) :-
		memberchk(options(Options), Diagnostics),
		length(Components, ComponentCount),
		^^make_vector(ComponentCount, 1.0, InitialWeights),
		iterate_weights(Options, Features, Components, 0, InitialWeights, Weights).

	iterate_weights(Options, Features, Components, Iteration0, Weights0, Weights) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		reconstruct_row(Weights0, Components, Approximation),
		update_weight_vector(Weights0, Features, Components, Approximation, 0.0, Weights1, Delta),
		Iteration is Iteration0 + 1,
		(   Delta =< Tolerance ->
			Weights = Weights1
		;   Iteration >= MaximumIterations ->
			Weights = Weights1
		;   iterate_weights(Options, Features, Components, Iteration, Weights1, Weights)
		).

	weights_pairs([], _Index, []).
	weights_pairs([Weight| Weights], Index, [Name-Weight| Pairs]) :-
		atomic_concat(component_, Index, Name),
		NextIndex is Index + 1,
		weights_pairs(Weights, NextIndex, Pairs).

	reconstruction_error(Rows, Coefficients, Components, Error) :-
		reconstruct_rows(Coefficients, Components, Approximation),
		matrix_difference(Rows, Approximation, Residuals),
		matrix_sum_squares(Residuals, 0.0, SumSquares),
		Error is sqrt(SumSquares).

	reconstruct_rows([], _Components, []).
	reconstruct_rows([Weights| WeightsRows], Components, [Row| Rows]) :-
		reconstruct_row(Weights, Components, Row),
		reconstruct_rows(WeightsRows, Components, Rows).

	reconstruct_row(Weights, Components, Row) :-
			^^zero_vector_like(Components, Zeroes),
		accumulate_weighted_components(Weights, Components, Zeroes, Row).

	accumulate_weighted_components([], [], Row, Row).
	accumulate_weighted_components([Weight| Weights], [Component| Components], Accumulator0, Row) :-
		scale_and_add_row(Weight, Component, Accumulator0, Accumulator1),
		accumulate_weighted_components(Weights, Components, Accumulator1, Row).

	scale_and_add_row(_Weight, [], [], []) :-
		!.
	scale_and_add_row(Weight, [Value| Values], [Accumulator0| Accumulators0], [Accumulator| Accumulators]) :-
		Accumulator is Accumulator0 + Weight * Value,
		scale_and_add_row(Weight, Values, Accumulators0, Accumulators).

	update_component_rows([], [], _FeatureColumns, _ApproximationColumns, Delta, [], Delta).
	update_component_rows([Component| Components], [CoefficientColumn| CoefficientColumns], FeatureColumns, ApproximationColumns, Delta0, [UpdatedComponent| UpdatedComponents], Delta) :-
		update_component_row(Component, CoefficientColumn, FeatureColumns, ApproximationColumns, 0.0, UpdatedComponent, ComponentDelta),
		Delta1 is max(Delta0, ComponentDelta),
		update_component_rows(Components, CoefficientColumns, FeatureColumns, ApproximationColumns, Delta1, UpdatedComponents, Delta).

	update_component_row([], _CoefficientColumn, [], [], Delta, [], Delta).
	update_component_row([Current| Currents], CoefficientColumn, [FeatureColumn| FeatureColumns], [ApproximationColumn| ApproximationColumns], Delta0, [Updated| Updateds], Delta) :-
		weighted_sum(CoefficientColumn, FeatureColumn, 0.0, Numerator),
		weighted_sum(CoefficientColumn, ApproximationColumn, 0.0, Denominator),
		SafeDenominator is max(Denominator, 1.0e-12),
		Updated is Current * Numerator / SafeDenominator,
		ElementDelta is abs(Updated - Current),
		Delta1 is max(Delta0, ElementDelta),
		update_component_row(Currents, CoefficientColumn, FeatureColumns, ApproximationColumns, Delta1, Updateds, Delta).

	update_coefficient_rows([], [], _Components, [], Delta, [], Delta).
	update_coefficient_rows([CoefficientRow| CoefficientRows], [Row| Rows], Components, [ApproximationRow| ApproximationRows], Delta0, [UpdatedCoefficientRow| UpdatedCoefficientRows], Delta) :-
		update_coefficient_row(CoefficientRow, Row, Components, ApproximationRow, 0.0, UpdatedCoefficientRow, RowDelta),
		Delta1 is max(Delta0, RowDelta),
		update_coefficient_rows(CoefficientRows, Rows, Components, ApproximationRows, Delta1, UpdatedCoefficientRows, Delta).

	update_coefficient_row([], _Row, [], _ApproximationRow, Delta, [], Delta).
	update_coefficient_row([Current| Currents], Row, [Component| Components], ApproximationRow, Delta0, [Updated| Updateds], Delta) :-
		dot_product(Component, Row, Numerator),
		dot_product(Component, ApproximationRow, Denominator),
		SafeDenominator is max(Denominator, 1.0e-12),
		Updated is Current * Numerator / SafeDenominator,
		ElementDelta is abs(Updated - Current),
		Delta1 is max(Delta0, ElementDelta),
		update_coefficient_row(Currents, Row, Components, ApproximationRow, Delta1, Updateds, Delta).

	update_weight_vector([], _Features, [], _Approximation, Delta, [], Delta).
	update_weight_vector([Current| Currents], Features, [Component| Components], Approximation, Delta0, [Updated| Updateds], Delta) :-
		dot_product(Component, Features, Numerator),
		dot_product(Component, Approximation, Denominator),
		SafeDenominator is max(Denominator, 1.0e-12),
		Updated is Current * Numerator / SafeDenominator,
		ElementDelta is abs(Updated - Current),
		Delta1 is max(Delta0, ElementDelta),
		update_weight_vector(Currents, Features, Components, Approximation, Delta1, Updateds, Delta).

	weighted_sum([], [], Sum, Sum).
	weighted_sum([Weight| Weights], [Value| Values], Sum0, Sum) :-
		Sum1 is Sum0 + Weight * Value,
		weighted_sum(Weights, Values, Sum1, Sum).

	matrix_difference([], [], []).
	matrix_difference([LeftRow| LeftRows], [RightRow| RightRows], [DifferenceRow| DifferenceRows]) :-
		row_difference(LeftRow, RightRow, DifferenceRow),
		matrix_difference(LeftRows, RightRows, DifferenceRows).

	row_difference([], [], []).
	row_difference([Left| Lefts], [Right| Rights], [Difference| Differences]) :-
		Difference is Left - Right,
		row_difference(Lefts, Rights, Differences).

	matrix_sum_squares([], SumSquares, SumSquares).
	matrix_sum_squares([Row| Rows], SumSquares0, SumSquares) :-
		row_sum_squares(Row, 0.0, RowSumSquares),
		SumSquares1 is SumSquares0 + RowSumSquares,
		matrix_sum_squares(Rows, SumSquares1, SumSquares).

	row_sum_squares([], SumSquares, SumSquares).
	row_sum_squares([Value| Values], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Value * Value,
		row_sum_squares(Values, SumSquares1, SumSquares).

	valid_non_negative_components([]).
	valid_non_negative_components([Component| Components]) :-
		valid(list(non_negative_number), Component),
		valid_non_negative_components(Components).

	valid_nmf_diagnostics(Encoders, Components, Diagnostics) :-
		memberchk(model(nmf), Diagnostics),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		length(Encoders, FeatureCount),
		length(AttributeNames, FeatureCount),
		memberchk(feature_count(FeatureCount), Diagnostics),
		length(Components, ComponentCount),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		valid(positive_integer, SampleCount),
		memberchk(convergence(Convergence), Diagnostics),
		once((
			Convergence == tolerance
		;	Convergence == maximum_iterations_exhausted
		)),
		memberchk(iterations(Iterations), Diagnostics),
		valid(non_negative_integer, Iterations),
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_number, FinalDelta),
		memberchk(reconstruction_error(ReconstructionError), Diagnostics),
		valid(non_negative_number, ReconstructionError),
		memberchk(preprocessing([center(false), feature_scaling(FeatureScaling)]), Diagnostics),
		valid(boolean, FeatureScaling).

	build_diagnostics(AttributeNames, SampleCount, Components, Options, Convergence, Iterations, FinalDelta, ReconstructionError, Diagnostics) :-
		^^preprocessing_diagnostics(false, Options, Preprocessing),
		^^iterative_dimension_reducer_diagnostics(
			nmf,
			AttributeNames,
			Components,
			SampleCount,
			Options,
			[],
			Convergence,
			Iterations,
			FinalDelta,
			[reconstruction_error(ReconstructionError), preprocessing(Preprocessing)],
			Diagnostics
		).

	default_option(n_components(2)).
	default_option(center(false)).
	default_option(feature_scaling(false)).
	default_option(maximum_iterations(500)).
	default_option(tolerance(1.0e-6)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(center(false)).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

:- end_object.
