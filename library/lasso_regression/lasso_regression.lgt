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


:- object(lasso_regression,
	imports(regressor_common)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-06-16,
		comment is 'Lasso regression regressor supporting continuous and mixed-feature datasets using coordinate descent with coefficient-wise L1 shrinkage. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		see_also is [
			linear_regression, ridge_regression, knn_regression, regression_tree, random_forest_regression,
			gradient_boosting_regression
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, reverse/2
	]).

	:- uses(numberlist, [
		average/2, sum/2, scalar_product/3 as dot_product/3
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		fit_lasso_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = lasso_regressor(Encoders, Bias, Weights, Diagnostics).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Bias, Weights, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		dot_product(Weights, Features, Linear),
		Target is Bias + Linear.

	build_diagnostics(Target, Encoders, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(lasso_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	fit_lasso_model(Dataset, Options, Encoders, TrainingExampleCount, Bias, Weights, TrainingDiagnostics) :-
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_lasso_encoders(Attributes, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		^^encoded_feature_count(Encoders, FeatureCount),
		train_lasso_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics),
		length(Examples, TrainingExampleCount).

	build_lasso_encoders([], _Examples, _Options, []).
	build_lasso_encoders([Attribute-Values| Attributes], Examples, Options, [Encoder| Encoders]) :-
		(	Values == continuous ->
			^^continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;	Encoder = categorical(Attribute, Values)
		),
		build_lasso_encoders(Attributes, Examples, Options, Encoders).

	train_lasso_model(Rows, FeatureCount, Options, Bias, Weights, TrainingDiagnostics) :-
		rows_to_training_matrix(Rows, FeatureCount, Targets, Columns, RowCount),
		columns_sum_squares(Columns, ColumnSumSquares),
		average(Targets, Bias0),
		initial_residuals(Targets, Bias0, Residuals0),
		zero_vector(FeatureCount, Weights0),
		optimize_lasso_model(RowCount, Columns, ColumnSumSquares, Options, 0, Bias0, Weights0, Residuals0, 0.0, Bias, Weights, Convergence, Iterations, FinalDelta),
		TrainingDiagnostics = [convergence(Convergence), iterations(Iterations), final_delta(FinalDelta)].

	rows_to_training_matrix(Rows, FeatureCount, Targets, Columns, RowCount) :-
		length(Rows, RowCount),
		zero_columns(FeatureCount, EmptyColumns),
		rows_to_training_matrix_(Rows, Targets, EmptyColumns, Columns0),
		reverse_nested_lists(Columns0, Columns).

	rows_to_training_matrix_([], [], Columns, Columns).
	rows_to_training_matrix_([Features-Target| Rows], [Target| Targets], Columns0, Columns) :-
		prepend_features_to_columns(Features, Columns0, Columns1),
		rows_to_training_matrix_(Rows, Targets, Columns1, Columns).

	prepend_features_to_columns([], [], []).
	prepend_features_to_columns([Feature| Features], [Column| Columns0], [[Feature| Column]| Columns]) :-
		prepend_features_to_columns(Features, Columns0, Columns).

	reverse_nested_lists([], []).
	reverse_nested_lists([List| Lists], [Reversed| ReversedLists]) :-
		reverse(List, Reversed),
		reverse_nested_lists(Lists, ReversedLists).

	columns_sum_squares([], []).
	columns_sum_squares([Column| Columns], [SumSquares| SumsSquares]) :-
		column_sum_squares(Column, 0.0, SumSquares),
		columns_sum_squares(Columns, SumsSquares).

	column_sum_squares([], SumSquares, SumSquares).
	column_sum_squares([Feature| Features], SumSquares0, SumSquares) :-
		SumSquares1 is SumSquares0 + Feature * Feature,
		column_sum_squares(Features, SumSquares1, SumSquares).

	initial_residuals([], _Bias, []).
	initial_residuals([Target| Targets], Bias, [Residual| Residuals]) :-
		Residual is Target - Bias,
		initial_residuals(Targets, Bias, Residuals).

	zero_columns(0, []) :-
		!.
	zero_columns(Count, [[]| Columns]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_columns(NextCount, Columns).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	optimize_lasso_model(RowCount, Columns, ColumnSumSquares, Options, Iteration, Bias0, Weights0, Residuals0, PreviousViolation, Bias, Weights, Convergence, Iterations, FinalDelta) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Iteration >= MaximumIterations ->
			Bias = Bias0,
			Weights = Weights0,
			Convergence = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDelta = PreviousViolation
		;	update_bias(RowCount, Bias0, Residuals0, Bias1, Residuals1),
			sweep_lasso_weights(Columns, ColumnSumSquares, RowCount, Options, Weights0, Residuals1, Weights1, Residuals2),
			^^option(regularization(Regularization), Options),
			max_optimality_violation(RowCount, Columns, ColumnSumSquares, Weights1, Residuals2, Regularization, MaxViolation),
			NextIteration is Iteration + 1,
			^^option(tolerance(Tolerance), Options),
			(	MaxViolation =< Tolerance ->
				Bias = Bias1,
				Weights = Weights1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalDelta = MaxViolation
			;	optimize_lasso_model(RowCount, Columns, ColumnSumSquares, Options, NextIteration, Bias1, Weights1, Residuals2, MaxViolation, Bias, Weights, Convergence, Iterations, FinalDelta)
			)
		).

	update_bias(RowCount, Bias0, Residuals0, Bias1, Residuals1) :-
		sum(Residuals0, ResidualSum),
		BiasAdjustment is ResidualSum / RowCount,
		Bias1 is Bias0 + BiasAdjustment,
		shift_residuals(Residuals0, BiasAdjustment, Residuals1).

	shift_residuals([], _Shift, []).
	shift_residuals([Residual0| Residuals0], Shift, [Residual1| Residuals1]) :-
		Residual1 is Residual0 - Shift,
		shift_residuals(Residuals0, Shift, Residuals1).

	sweep_lasso_weights([], [], _RowCount, _Options, [], Residuals, [], Residuals).
	sweep_lasso_weights([Column| Columns], [SumSquares| ColumnsSumSquares], RowCount, Options, [Weight0| Weights0], Residuals0, [Weight1| Weights1], Residuals) :-
		update_lasso_weight(RowCount, Column, SumSquares, Options, Weight0, Residuals0, Weight1, Residuals1),
		sweep_lasso_weights(Columns, ColumnsSumSquares, RowCount, Options, Weights0, Residuals1, Weights1, Residuals).

	update_lasso_weight(RowCount, Column, SumSquares, Options, Weight0, Residuals0, Weight1, Residuals1) :-
		column_correlation(Column, Residuals0, Weight0, 0.0, SumCorrelation),
		(	SumSquares =< 1.0e-12 ->
			Weight1 = 0.0
		;	MeanCorrelation is SumCorrelation / RowCount,
			MeanSquare is SumSquares / RowCount,
			^^option(regularization(Regularization), Options),
			soft_threshold(MeanCorrelation, Regularization, Thresholded),
			Weight1 is Thresholded / MeanSquare
		),
		WeightDelta is Weight1 - Weight0,
		update_residuals(Column, Residuals0, WeightDelta, Residuals1).

	column_correlation([], [], _Weight, SumCorrelation, SumCorrelation).
	column_correlation([Feature| Column], [Residual| Residuals], Weight, SumCorrelation0, SumCorrelation) :-
		PartialResidual is Residual + Feature * Weight,
		SumCorrelation1 is SumCorrelation0 + Feature * PartialResidual,
		column_correlation(Column, Residuals, Weight, SumCorrelation1, SumCorrelation).

	max_optimality_violation(RowCount, Columns, ColumnSumSquares, Weights, Residuals, Regularization, MaxViolation) :-
		bias_optimality_violation(RowCount, Residuals, BiasViolation),
		weights_optimality_violation(Columns, ColumnSumSquares, Weights, Residuals, RowCount, Regularization, 0.0, WeightsViolation),
		MaxViolation is max(BiasViolation, WeightsViolation).

	bias_optimality_violation(RowCount, Residuals, Violation) :-
		sum(Residuals, ResidualSum),
		Violation is abs(ResidualSum / RowCount).

	weights_optimality_violation([], [], [], _Residuals, _RowCount, _Regularization, MaxViolation, MaxViolation).
	weights_optimality_violation([Column| Columns], [SumSquares| ColumnsSumSquares], [Weight| Weights], Residuals, RowCount, Regularization, MaxViolation0, MaxViolation) :-
		weight_optimality_violation(Column, SumSquares, Weight, Residuals, RowCount, Regularization, Violation),
		MaxViolation1 is max(MaxViolation0, Violation),
		weights_optimality_violation(Columns, ColumnsSumSquares, Weights, Residuals, RowCount, Regularization, MaxViolation1, MaxViolation).

	weight_optimality_violation(_Column, SumSquares, Weight, _Residuals, _RowCount, _Regularization, Violation) :-
		SumSquares =< 1.0e-12,
		!,
		AbsWeight is abs(Weight),
		(	AbsWeight =< 1.0e-12 ->
			Violation = 0.0
		;	Violation = AbsWeight
		).
	weight_optimality_violation(Column, _SumSquares, Weight, Residuals, RowCount, Regularization, Violation) :-
		dot_product(Column, Residuals, SumCorrelation),
		MeanCorrelation is SumCorrelation / RowCount,
		AbsWeight is abs(Weight),
		(	AbsWeight =< 1.0e-12 ->
			ExcessCorrelation is abs(MeanCorrelation) - Regularization,
			(	ExcessCorrelation =< 0.0 ->
				Violation = 0.0
			;	Violation = ExcessCorrelation
			)
		;	Weight > 0.0 ->
			Violation is abs(MeanCorrelation - Regularization)
		;	Violation is abs(MeanCorrelation + Regularization)
		).

	soft_threshold(Value, Threshold, Result) :-
		(	Value > Threshold ->
			Result is Value - Threshold
		;	Value < -Threshold ->
			Result is Value + Threshold
		;	Result = 0.0
		).

	update_residuals(_Column, Residuals, 0.0, Residuals) :-
		!.
	update_residuals([], [], _WeightDelta, []).
	update_residuals([Feature| Column], [Residual0| Residuals0], WeightDelta, [Residual1| Residuals1]) :-
		Residual1 is Residual0 - Feature * WeightDelta,
		update_residuals(Column, Residuals0, WeightDelta, Residuals1).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Bias', 'Weights', 'Diagnostics'].

	regressor_term_template(lasso_regressor(_Encoders, _Bias, _Weights, _Diagnostics), lasso_regressor('Encoders', 'Bias', 'Weights', 'Diagnostics')).

	check_regressor(Regressor) :-
		(	Regressor = lasso_regressor(Encoders, Bias, Weights, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid(float, Bias),
			^^encoded_feature_count(Encoders, FeatureCount),
			valid(list(float, FeatureCount), Weights),
			^^valid_regressor_metadata(lasso_regression, Diagnostics),
			^^valid_linear_model_diagnostics(Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount) ->
			true
		;	domain_error(regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = lasso_regressor(Encoders, Bias, Weights, Diagnostics),
		Clause =.. [Functor, Encoders, Bias, Weights, Diagnostics].

	print_regressor(Regressor) :-
		Regressor = lasso_regressor(Encoders, Bias, Weights, Diagnostics),
		format('Lasso Regression Regressor~n', []),
		format('==========================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Bias: ~4f~n', [Bias]),
		format('Weights: ~w coefficients~n~n', [Weights]),
		format('Encoders: ~w~n', [Encoders]).

	default_option(maximum_iterations(2000)).
	default_option(tolerance(1.0e-7)).
	default_option(regularization(0.01)).
	default_option(feature_scaling(true)).

	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		valid(non_negative_float, Tolerance).
	valid_option(regularization(Regularization)) :-
		valid(non_negative_float, Regularization).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.
