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
		comment is 'Unit tests for the "lasso_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(lasso_regression).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_simple_line.pl'),
		^^clean_file('test_output_mixed_signal.pl').

	test(lasso_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		lasso_regression::learn(simple_line, Regressor).

	test(lasso_regression_valid_regressor_1, deterministic(lasso_regression::valid_regressor(Regressor))) :-
		lasso_regression::learn(simple_line, Regressor).

	test(lasso_regression_invalid_regressor_1, fail) :-
		lasso_regression::learn(simple_line, lasso_regressor(Encoders, _Bias, _Weights, Diagnostics)),
		lasso_regression::valid_regressor(lasso_regressor(Encoders, 0.0, [1.0], Diagnostics)).

	test(lasso_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		lasso_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		lasso_regression::predict(Regressor, [x-6], Prediction).

	test(lasso_regression_predict_3_simple_line_default_regularized, true(abs(Prediction - 13.0) < 0.25)) :-
		lasso_regression::learn(simple_line, Regressor),
		lasso_regression::predict(Regressor, [x-6], Prediction).

	test(lasso_regression_regularization_shrinks_weight, true(abs(ShrunkWeight) < abs(UnregularizedWeight))) :-
		lasso_regression::learn(simple_line, lasso_regressor([continuous(x, 0.0, 1.0)], _UnregularizedBias, [UnregularizedWeight, _UnregularizedMissingWeight], _UnregularizedDiagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		lasso_regression::learn(simple_line, lasso_regressor([continuous(x, 0.0, 1.0)], _ShrunkBias, [ShrunkWeight, _ShrunkMissingWeight], _ShrunkDiagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.1)]).

	test(lasso_regression_regularization_sets_irrelevant_weight_to_zero, deterministic((NoiseWeight =:= 0.0, abs(SignalWeight) > 0.0))) :-
		lasso_regression::learn(sparse_signal, lasso_regressor([continuous(signal, 0.0, 1.0), continuous(noise, 0.0, 1.0)], _Bias, [SignalWeight, _SignalMissingWeight, NoiseWeight, _NoiseMissingWeight], _Diagnostics), [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10), regularization(0.5)]).

	test(lasso_regression_regularization_sets_irrelevant_categorical_coefficients_to_zero, deterministic((NoiseBWeight =:= 0.0, NoiseCWeight =:= 0.0, NoiseMissingWeight =:= 0.0, abs(SignalWeight) > 0.0))) :-
		lasso_regression::learn(grouped_categorical_signal, lasso_regressor([continuous(signal, 0.0, 1.0), categorical(noise, [a, b, c])], _Bias, [SignalWeight, _SignalMissingWeight, NoiseBWeight, NoiseCWeight, NoiseMissingWeight], _Diagnostics), [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10), regularization(0.1)]).

	test(lasso_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		lasso_regression::learn(plane, Regressor, [maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		lasso_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(lasso_regression_predict_3_mixed_signal_default_regularized, true(abs(Prediction - 175.0) < 1.0)) :-
		lasso_regression::learn(mixed_signal, Regressor),
		lasso_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(lasso_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		lasso_regression::learn(sparse_mixed_signal, Regressor, [maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		lasso_regression::predict(Regressor, [age-20], Prediction).

	test(lasso_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		lasso_regression::learn(intercept_only, Regressor, [maximum_iterations(5000), tolerance(1.0e-9)]),
		lasso_regression::predict(Regressor, [dummy-0], Prediction).

	test(lasso_regression_learn_3_custom_options, deterministic([MaximumIterations, Tolerance, Regularization, FeatureScaling] == [1500, 1.0e-6, 0.02, false])) :-
		lasso_regression::learn(simple_line, Regressor, [maximum_iterations(1500), tolerance(1.0e-6), regularization(0.02), feature_scaling(false)]),
		lasso_regression::regressor_options(Regressor, Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(regularization(Regularization), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(lasso_regression_diagnostics_2, deterministic((Iterations >= 1, FinalDelta >= 0.0, [Model, TrainingExampleCount, EncodedFeatureCount, Regularization] == [lasso_regression, 5, 2, 0.01]))) :-
		lasso_regression::learn(simple_line, Regressor),
		lasso_regression::diagnostics(Regressor, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(Convergence, [tolerance, maximum_iterations_exhausted]),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(regularization(Regularization), Options).

	test(lasso_regression_learn_3_maximum_iterations_diagnostics, deterministic((FinalDelta > 0.0, Convergence == maximum_iterations_exhausted, Iterations == 1))) :-
		lasso_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(1), tolerance(1.0e-12), regularization(0.0)]),
		lasso_regression::diagnostics(Regressor, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics).

	test(lasso_regression_learn_3_tolerance_diagnostics, deterministic((FinalDelta >= 0.0, FinalDelta =< 100.0, Convergence == tolerance, Iterations == 1))) :-
		lasso_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(100.0), regularization(0.01)]),
		lasso_regression::diagnostics(Regressor, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics).

	test(lasso_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		lasso_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		lasso_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		lasso_regression::predict(Clause, [x-6], Prediction).

	test(lasso_regression_export_to_clauses_4_default_regularized, deterministic(ClausePrediction =~= ModelPrediction)) :-
		lasso_regression::learn(mixed_signal, Regressor),
		lasso_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		lasso_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		lasso_regression::predict(Clause, [age-20, student-yes, plan-premium], ClausePrediction).

	test(lasso_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		lasso_regression::learn(simple_line, Regressor),
		lasso_regression::export_to_file(simple_line, Regressor, regress, File).

	test(lasso_regression_export_to_file_4_loaded, deterministic(Prediction =~= 13.0)) :-
		^^file_path('test_output_simple_line.pl', File),
		lasso_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		lasso_regression::export_to_file(simple_line, Regressor, regress_simple_line, File),
		logtalk_load(File),
		{regress_simple_line(Encoders, Bias, Weights, Diagnostics)},
		lasso_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [x-6], Prediction).

	test(lasso_regression_export_to_file_4_loaded_default_regularized, deterministic(LoadedPrediction =~= ModelPrediction)) :-
		^^file_path('test_output_mixed_signal.pl', File),
		lasso_regression::learn(mixed_signal, Regressor),
		lasso_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		lasso_regression::export_to_file(mixed_signal, Regressor, regress_mixed_signal, File),
		logtalk_load(File),
		{regress_mixed_signal(Encoders, Bias, Weights, Diagnostics)},
		lasso_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [age-20, student-yes, plan-premium], LoadedPrediction).

	test(lasso_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		lasso_regression::learn(mixed_signal, Regressor),
		lasso_regression::print_regressor(Regressor).

	test(lasso_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		lasso_regression::learn(invalid_target, _Regressor).

	test(lasso_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		lasso_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(lasso_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		lasso_regression::learn(simple_line, Regressor),
		lasso_regression::predict(Regressor, [x-6, typo-1], _Prediction).

	test(lasso_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		lasso_regression::learn(simple_line, Regressor),
		lasso_regression::predict(Regressor, [x-6, x-7], _Prediction).

	test(lasso_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		lasso_regression::learn(mixed_signal, Regressor),
		lasso_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

:- end_object.
