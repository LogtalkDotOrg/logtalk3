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
		date is 2026-05-07,
		comment is 'Unit tests for the "ridge_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(ridge_regression).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_simple_line.pl'),
		^^clean_file('test_output_mixed_signal.pl').

	test(ridge_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		ridge_regression::learn(simple_line, Regressor).

	test(ridge_regression_valid_regressor_1, deterministic(ridge_regression::valid_regressor(Regressor))) :-
		ridge_regression::learn(simple_line, Regressor).

	test(ridge_regression_invalid_regressor_1, fail) :-
		ridge_regression::learn(simple_line, ridge_regressor(Encoders, _Bias, _Weights, Diagnostics)),
		ridge_regression::valid_regressor(ridge_regressor(Encoders, 0.0, [1.0], Diagnostics)).

	test(ridge_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), regularization(0.0)]),
		ridge_regression::predict(Regressor, [x-6], Prediction).

	test(ridge_regression_predict_3_simple_line_default_regularized, true(abs(Prediction - 13.0) < 0.25)) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::predict(Regressor, [x-6], Prediction).

	test(ridge_regression_regularization_shrinks_weight, true(abs(ShrunkWeight) < abs(UnregularizedWeight))) :-
		ridge_regression::learn(simple_line, ridge_regressor([continuous(x, 0.0, 1.0)], _UnregularizedBias, [UnregularizedWeight, _UnregularizedMissingWeight], _UnregularizedDiagnostics), [feature_scaling(false), regularization(0.0)]),
		ridge_regression::learn(simple_line, ridge_regressor([continuous(x, 0.0, 1.0)], _ShrunkBias, [ShrunkWeight, _ShrunkMissingWeight], _ShrunkDiagnostics), [feature_scaling(false), regularization(0.1)]).

	test(ridge_regression_regularization_shrinks_categorical_weight, true(abs(ShrunkPremiumWeight) < abs(UnregularizedPremiumWeight))) :-
		ridge_regression::learn(mixed_signal, ridge_regressor(_Encoders0, _Bias0, [_AgeWeight0, _AgeMissingWeight0, _StudentNoWeight0, _StudentMissingWeight0, UnregularizedPremiumWeight, _PlanMissingWeight0], _Diagnostics0), [regularization(0.0)]),
		ridge_regression::learn(mixed_signal, ridge_regressor(_Encoders1, _Bias1, [_AgeWeight1, _AgeMissingWeight1, _StudentNoWeight1, _StudentMissingWeight1, ShrunkPremiumWeight, _PlanMissingWeight1], _Diagnostics1), [regularization(0.2)]).

	test(ridge_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		ridge_regression::learn(plane, Regressor, [regularization(0.0)]),
		ridge_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(ridge_regression_predict_3_mixed_signal_default_regularized, true(abs(Prediction - 175.0) < 1.0)) :-
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(ridge_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		ridge_regression::learn(sparse_mixed_signal, Regressor, [regularization(0.0)]),
		ridge_regression::predict(Regressor, [age-20], Prediction).

	test(ridge_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		ridge_regression::learn(intercept_only, Regressor),
		ridge_regression::predict(Regressor, [dummy-0], Prediction).

		test(ridge_regression_learn_3_custom_options, deterministic([Regularization, FeatureScaling] == [0.02, false])) :-
		ridge_regression::learn(simple_line, Regressor, [regularization(0.02), feature_scaling(false)]),
		ridge_regression::regressor_options(Regressor, Options),
		memberchk(regularization(Regularization), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(ridge_regression_diagnostics_2, deterministic((Residual =< 1.0e-8, [Model, TrainingExampleCount, Solver, ActiveFeatureCount, PenaltyScaling, EncodedFeatureCount, Regularization] == [ridge_regression, 5, pivoted_gaussian_elimination, 1, encoded_feature_standardization, 2, 0.01]))) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::diagnostics(Regressor, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(solver(Solver), Diagnostics),
		memberchk(linear_system_residual(Residual), Diagnostics),
		memberchk(active_feature_count(ActiveFeatureCount), Diagnostics),
		memberchk(penalty_scaling(PenaltyScaling), Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(regularization(Regularization), Options).

	test(ridge_regression_mixed_signal_encoded_feature_count, deterministic([EncodedFeatureCount, ActiveFeatureCount] == [6, 3])) :-
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::diagnostics(Regressor, Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(active_feature_count(ActiveFeatureCount), Diagnostics).

	test(ridge_regression_simple_line_zero_variance_missing_weight_is_zero, deterministic(MissingWeight =~= 0.0)) :-
		ridge_regression::learn(simple_line, ridge_regressor(_Encoders, _Bias, [_Weight, MissingWeight], _Diagnostics), [feature_scaling(false), regularization(0.0)]).

	test(ridge_regression_intercept_only_zero_variance_weights_are_zero, deterministic(([ValueWeight, MissingWeight] =~= [0.0, 0.0]))) :-
		ridge_regression::learn(intercept_only, ridge_regressor(_Encoders, _Bias, [ValueWeight, MissingWeight], _Diagnostics)).

	test(ridge_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), regularization(0.0)]),
		ridge_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		ridge_regression::predict(Clause, [x-6], Prediction).

	test(ridge_regression_export_to_clauses_4_default_regularized, deterministic(ClausePrediction =~= ModelPrediction)) :-
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		ridge_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		ridge_regression::predict(Clause, [age-20, student-yes, plan-premium], ClausePrediction).

	test(ridge_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::export_to_file(simple_line, Regressor, regress, File).

	test(ridge_regression_export_to_file_4_loaded, deterministic(Prediction =~= 13.0)) :-
		^^file_path('test_output_simple_line.pl', File),
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), regularization(0.0)]),
		ridge_regression::export_to_file(simple_line, Regressor, regress_simple_line, File),
		logtalk_load(File),
		{regress_simple_line(Encoders, Bias, Weights, Diagnostics)},
		ridge_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [x-6], Prediction).

	test(ridge_regression_export_to_file_4_loaded_default_regularized, deterministic(LoadedPrediction =~= ModelPrediction)) :-
		^^file_path('test_output_mixed_signal.pl', File),
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		ridge_regression::export_to_file(mixed_signal, Regressor, regress_mixed_signal, File),
		logtalk_load(File),
		{regress_mixed_signal(Encoders, Bias, Weights, Diagnostics)},
		ridge_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [age-20, student-yes, plan-premium], LoadedPrediction).

	test(ridge_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::print_regressor(Regressor).

	test(ridge_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		ridge_regression::learn(invalid_target, _Regressor).

	test(ridge_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		ridge_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(ridge_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::predict(Regressor, [x-6, typo-1], _Prediction).

	test(ridge_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::predict(Regressor, [x-6, x-7], _Prediction).

	test(ridge_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

:- end_object.
