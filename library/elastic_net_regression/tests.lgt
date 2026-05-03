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
		date is 2026-05-02,
		comment is 'Unit tests for the "elastic_net_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(elastic_net_regression).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_simple_line.pl'),
		^^clean_file('test_output_mixed_signal.pl').

	test(elastic_net_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		elastic_net_regression::learn(simple_line, Regressor).

	test(elastic_net_regression_valid_regressor_1, deterministic(elastic_net_regression::valid_regressor(Regressor))) :-
		elastic_net_regression::learn(simple_line, Regressor).

	test(elastic_net_regression_invalid_regressor_1, fail) :-
		elastic_net_regression::learn(simple_line, elastic_net_regressor(Encoders, _Bias, _Weights, Diagnostics)),
		elastic_net_regression::valid_regressor(elastic_net_regressor(Encoders, 0.0, [1.0], Diagnostics)).

	test(elastic_net_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		elastic_net_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		elastic_net_regression::predict(Regressor, [x-6], Prediction).

	test(elastic_net_regression_predict_3_simple_line_default_regularized, true(abs(Prediction - 13.0) < 0.25)) :-
		elastic_net_regression::learn(simple_line, Regressor),
		elastic_net_regression::predict(Regressor, [x-6], Prediction).

	test(elastic_net_regression_regularization_shrinks_weight, true(abs(ShrunkWeight) < abs(UnregularizedWeight))) :-
		elastic_net_regression::learn(simple_line, elastic_net_regressor([continuous(x, 0.0, 1.0)], _UnregularizedBias, [UnregularizedWeight, _UnregularizedMissingWeight], _UnregularizedDiagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		elastic_net_regression::learn(simple_line, elastic_net_regressor([continuous(x, 0.0, 1.0)], _ShrunkBias, [ShrunkWeight, _ShrunkMissingWeight], _ShrunkDiagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.1), l1_ratio(0.5)]).

	test(elastic_net_regression_ridge_endpoint_shrinks_weight, deterministic((abs(RidgeWeight) < abs(UnregularizedWeight), abs(RidgeWeight) > 0.0, member(options(Options), Diagnostics), member(l1_ratio(0.0), Options)))) :-
		elastic_net_regression::learn(simple_line, elastic_net_regressor([continuous(x, 0.0, 1.0)], _UnregularizedBias, [UnregularizedWeight, _UnregularizedMissingWeight], _UnregularizedDiagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		elastic_net_regression::learn(simple_line, elastic_net_regressor([continuous(x, 0.0, 1.0)], _RidgeBias, [RidgeWeight, _RidgeMissingWeight], Diagnostics), [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.1), l1_ratio(0.0)]).

	test(elastic_net_regression_regularization_sets_irrelevant_weight_to_zero, deterministic((NoiseWeight =:= 0.0, abs(SignalWeight) > 0.0))) :-
		elastic_net_regression::learn(sparse_signal, elastic_net_regressor([continuous(signal, 0.0, 1.0), continuous(noise, 0.0, 1.0)], _Bias, [SignalWeight, _SignalMissingWeight, NoiseWeight, _NoiseMissingWeight], _Diagnostics), [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10), regularization(0.5), l1_ratio(1.0)]).

	test(elastic_net_regression_regularization_sets_irrelevant_categorical_coefficients_to_zero, deterministic((NoiseBWeight =:= 0.0, NoiseCWeight =:= 0.0, NoiseMissingWeight =:= 0.0, abs(SignalWeight) > 0.0))) :-
		elastic_net_regression::learn(grouped_categorical_signal, elastic_net_regressor([continuous(signal, 0.0, 1.0), categorical(noise, [a, b, c])], _Bias, [SignalWeight, _SignalMissingWeight, NoiseBWeight, NoiseCWeight, NoiseMissingWeight], _Diagnostics), [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10), regularization(0.1), l1_ratio(1.0)]).

	test(elastic_net_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		elastic_net_regression::learn(plane, Regressor, [maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		elastic_net_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(elastic_net_regression_predict_3_mixed_signal_default_regularized, true(abs(Prediction - 175.0) < 1.0)) :-
		elastic_net_regression::learn(mixed_signal, Regressor),
		elastic_net_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(elastic_net_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		elastic_net_regression::learn(sparse_mixed_signal, Regressor, [maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		elastic_net_regression::predict(Regressor, [age-20], Prediction).

	test(elastic_net_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		elastic_net_regression::learn(intercept_only, Regressor, [maximum_iterations(5000), tolerance(1.0e-9)]),
		elastic_net_regression::predict(Regressor, [dummy-0], Prediction).

	test(elastic_net_regression_learn_3_custom_options, deterministic((member(maximum_iterations(1500), Options), member(tolerance(1.0e-6), Options), member(regularization(0.02), Options), member(l1_ratio(0.75), Options), member(feature_scaling(false), Options)))) :-
		elastic_net_regression::learn(simple_line, Regressor, [maximum_iterations(1500), tolerance(1.0e-6), regularization(0.02), l1_ratio(0.75), feature_scaling(false)]),
		elastic_net_regression::regressor_options(Regressor, Options).

	test(elastic_net_regression_diagnostics_2, deterministic((member(model(elastic_net_regression), Diagnostics), member(training_example_count(5), Diagnostics), member(convergence(Convergence), Diagnostics), member(Convergence, [tolerance, maximum_iterations_exhausted]), member(iterations(Iterations), Diagnostics), Iterations >= 1, member(final_delta(FinalDelta), Diagnostics), FinalDelta >= 0.0, member(encoded_feature_count(2), Diagnostics), member(options(Options), Diagnostics), member(regularization(0.01), Options), member(l1_ratio(0.5), Options)))) :-
		elastic_net_regression::learn(simple_line, Regressor),
		elastic_net_regression::diagnostics(Regressor, Diagnostics).

	test(elastic_net_regression_learn_3_maximum_iterations_diagnostics, deterministic((member(convergence(maximum_iterations_exhausted), Diagnostics), member(iterations(1), Diagnostics), member(final_delta(FinalDelta), Diagnostics), FinalDelta > 0.0))) :-
		elastic_net_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(1), tolerance(1.0e-12), regularization(0.0)]),
		elastic_net_regression::diagnostics(Regressor, Diagnostics).

	test(elastic_net_regression_learn_3_tolerance_diagnostics, deterministic((member(convergence(tolerance), Diagnostics), member(iterations(1), Diagnostics), member(final_delta(FinalDelta), Diagnostics), FinalDelta >= 0.0, FinalDelta =< 100.0))) :-
		elastic_net_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(100.0), regularization(0.01), l1_ratio(0.5)]),
		elastic_net_regression::diagnostics(Regressor, Diagnostics).

	test(elastic_net_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		elastic_net_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		elastic_net_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		elastic_net_regression::predict(Clause, [x-6], Prediction).

	test(elastic_net_regression_export_to_clauses_4_default_regularized, deterministic(ClausePrediction =~= ModelPrediction)) :-
		elastic_net_regression::learn(mixed_signal, Regressor),
		elastic_net_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		elastic_net_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		elastic_net_regression::predict(Clause, [age-20, student-yes, plan-premium], ClausePrediction).

	test(elastic_net_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		elastic_net_regression::learn(simple_line, Regressor),
		elastic_net_regression::export_to_file(simple_line, Regressor, regress, File).

	test(elastic_net_regression_export_to_file_4_loaded, deterministic(Prediction =~= 13.0)) :-
		^^file_path('test_output_simple_line.pl', File),
		elastic_net_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		elastic_net_regression::export_to_file(simple_line, Regressor, regress_simple_line, File),
		logtalk_load(File),
		{regress_simple_line(Encoders, Bias, Weights, Diagnostics)},
		elastic_net_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [x-6], Prediction).

	test(elastic_net_regression_export_to_file_4_loaded_default_regularized, deterministic(LoadedPrediction =~= ModelPrediction)) :-
		^^file_path('test_output_mixed_signal.pl', File),
		elastic_net_regression::learn(mixed_signal, Regressor),
		elastic_net_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		elastic_net_regression::export_to_file(mixed_signal, Regressor, regress_mixed_signal, File),
		logtalk_load(File),
		{regress_mixed_signal(Encoders, Bias, Weights, Diagnostics)},
		elastic_net_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [age-20, student-yes, plan-premium], LoadedPrediction).

	test(elastic_net_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		elastic_net_regression::learn(mixed_signal, Regressor),
		elastic_net_regression::print_regressor(Regressor).

	test(elastic_net_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		elastic_net_regression::learn(invalid_target, _Regressor).

	test(elastic_net_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		elastic_net_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(elastic_net_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		elastic_net_regression::learn(simple_line, Regressor),
		elastic_net_regression::predict(Regressor, [x-6, typo-1], _Prediction).

	test(elastic_net_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		elastic_net_regression::learn(simple_line, Regressor),
		elastic_net_regression::predict(Regressor, [x-6, x-7], _Prediction).

	test(elastic_net_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		elastic_net_regression::learn(mixed_signal, Regressor),
		elastic_net_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

:- end_object.
