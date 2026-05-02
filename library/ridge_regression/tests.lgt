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
		comment is 'Unit tests for the "ridge_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(ridge_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(ridge_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		ridge_regression::learn(simple_line, Regressor).

	test(ridge_regression_valid_regressor_1, deterministic(ridge_regression::valid_regressor(Regressor))) :-
		ridge_regression::learn(simple_line, Regressor).

	test(ridge_regression_invalid_regressor_1, fail) :-
		ridge_regression::learn(simple_line, ridge_regressor(Encoders, _Bias, _Weights, Diagnostics)),
		ridge_regression::valid_regressor(ridge_regressor(Encoders, 0.0, [1.0], Diagnostics)).

	test(ridge_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		ridge_regression::predict(Regressor, [x-6], Prediction).

	test(ridge_regression_predict_3_simple_line_default_regularized, true(abs(Prediction - 13.0) < 0.25)) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::predict(Regressor, [x-6], Prediction).

	test(ridge_regression_regularization_shrinks_weight, true(abs(ShrunkWeight) < abs(UnregularizedWeight))) :-
		ridge_regression::learn(simple_line, ridge_regressor([continuous(x, 0.0, 1.0)], _UnregularizedBias, [UnregularizedWeight, _UnregularizedMissingWeight], _UnregularizedDiagnostics), [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		ridge_regression::learn(simple_line, ridge_regressor([continuous(x, 0.0, 1.0)], _ShrunkBias, [ShrunkWeight, _ShrunkMissingWeight], _ShrunkDiagnostics), [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.1)]).

	test(ridge_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		ridge_regression::learn(plane, Regressor, [learning_rate(0.05), maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		ridge_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(ridge_regression_predict_3_mixed_signal_default_regularized, true(abs(Prediction - 175.0) < 1.0)) :-
		ridge_regression::learn(mixed_signal, Regressor),
		ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(ridge_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		ridge_regression::learn(sparse_mixed_signal, Regressor, [learning_rate(0.05), maximum_iterations(8000), tolerance(1.0e-9), regularization(0.0)]),
		ridge_regression::predict(Regressor, [age-20], Prediction).

	test(ridge_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		ridge_regression::learn(intercept_only, Regressor, [learning_rate(0.05), maximum_iterations(5000), tolerance(1.0e-9)]),
		ridge_regression::predict(Regressor, [dummy-0], Prediction).

	test(ridge_regression_learn_3_custom_options, deterministic((member(learning_rate(0.1), Options), member(maximum_iterations(1500), Options), member(tolerance(1.0e-6), Options), member(regularization(0.02), Options), member(feature_scaling(false), Options)))) :-
		ridge_regression::learn(simple_line, Regressor, [learning_rate(0.1), maximum_iterations(1500), tolerance(1.0e-6), regularization(0.02), feature_scaling(false)]),
		ridge_regression::regressor_options(Regressor, Options).

	test(ridge_regression_diagnostics_2, deterministic((member(model(ridge_regression), Diagnostics), member(training_example_count(5), Diagnostics), member(convergence(Convergence), Diagnostics), member(Convergence, [tolerance, maximum_iterations_exhausted]), member(iterations(Iterations), Diagnostics), Iterations >= 1, member(final_delta(FinalDelta), Diagnostics), FinalDelta >= 0.0, member(encoded_feature_count(2), Diagnostics), member(options(Options), Diagnostics), member(regularization(0.01), Options)))) :-
		ridge_regression::learn(simple_line, Regressor),
		ridge_regression::diagnostics(Regressor, Diagnostics).

	test(ridge_regression_learn_3_maximum_iterations_diagnostics, deterministic((member(convergence(maximum_iterations_exhausted), Diagnostics), member(iterations(1), Diagnostics), member(final_delta(FinalDelta), Diagnostics), FinalDelta > 0.0))) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), learning_rate(0.01), maximum_iterations(1), tolerance(1.0e-12), regularization(0.0)]),
		ridge_regression::diagnostics(Regressor, Diagnostics).

	test(ridge_regression_learn_3_tolerance_diagnostics, deterministic((member(convergence(tolerance), Diagnostics), member(iterations(1), Diagnostics), member(final_delta(FinalDelta), Diagnostics), FinalDelta >= 0.0))) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(100.0), regularization(0.01)]),
		ridge_regression::diagnostics(Regressor, Diagnostics).

	test(ridge_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
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
		^^file_path('test_output.pl', File),
		ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10), regularization(0.0)]),
		ridge_regression::export_to_file(simple_line, Regressor, regress_simple_line, File),
		logtalk_load(File),
		{regress_simple_line(Encoders, Bias, Weights, Diagnostics)},
		ridge_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [x-6], Prediction).

	test(ridge_regression_export_to_file_4_loaded_default_regularized, deterministic(LoadedPrediction =~= ModelPrediction)) :-
		^^file_path('test_output.pl', File),
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
