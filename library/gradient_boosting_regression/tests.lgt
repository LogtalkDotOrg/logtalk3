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
		date is 2026-05-01,
		comment is 'Unit tests for the "gradient_boosting_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		length/2, member/2
	]).

	cover(gradient_boosting_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(gradient_boosting_regression_learn_2_step_signal, deterministic(ground(Regressor))) :-
		gradient_boosting_regression::learn(step_signal, Regressor).

	test(gradient_boosting_regression_valid_regressor_1, deterministic(gradient_boosting_regression::valid_regressor(Regressor))) :-
		gradient_boosting_regression::learn(step_signal, Regressor).

	test(gradient_boosting_regression_invalid_regressor_1, fail) :-
		gradient_boosting_regression::valid_regressor(gradient_boosting_regressor(
			0.0,
			[weighted_tree(-0.1, regression_tree_regressor([continuous(x, 0.0, 1.0)], [feature(x, value), feature(x, missing)], leaf(1.0), [model(regression_tree), target(residual), training_example_count(5), options([maximum_depth(3), minimum_samples_leaf(1), minimum_variance_reduction(0.0), maximum_features_per_split(all), feature_scaling(false)]), encoded_feature_count(2)]))],
			[model(gradient_boosting_regression), target(y), training_example_count(5), options([number_of_estimators(1), learning_rate(0.1), maximum_depth(3), minimum_samples_leaf(1), minimum_variance_reduction(0.0), feature_scaling(false)]), initial_prediction(0.0), stage_count(1)]
		)).

	test(gradient_boosting_regression_learn_2_structure, deterministic(functor(Regressor, gradient_boosting_regressor, 3))) :-
		gradient_boosting_regression::learn(step_signal, Regressor).

	test(gradient_boosting_regression_learn_3_custom_options, deterministic((length(WeightedTrees, StageCount), StageCount >= 1, StageCount =< 5, member(number_of_estimators(5), Options), member(learning_rate(1.0), Options), member(maximum_depth(3), Options), member(minimum_samples_leaf(2), Options), member(feature_scaling(false), Options)))) :-
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(3), minimum_samples_leaf(2), feature_scaling(false)]),
		Regressor = gradient_boosting_regressor(_InitialPrediction, WeightedTrees, _Diagnostics),
		gradient_boosting_regression::regressor_options(Regressor, Options).

	test(gradient_boosting_regression_diagnostics_2, deterministic((member(model(gradient_boosting_regression), Diagnostics), member(stage_count(StageCount), Diagnostics), StageCount >= 1, member(options(Options), Diagnostics), member(number_of_estimators(5), Options)))) :-
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(2)]),
		gradient_boosting_regression::diagnostics(Regressor, Diagnostics).

	test(gradient_boosting_regression_predict_3_step_signal_left_band, deterministic(Prediction =~= 10.0)) :-
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(2)]),
		gradient_boosting_regression::predict(Regressor, [x-1.5], Prediction).

	test(gradient_boosting_regression_predict_3_step_signal_right_band, deterministic(Prediction =~= 20.0)) :-
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(2)]),
		gradient_boosting_regression::predict(Regressor, [x-8.5], Prediction).

	test(gradient_boosting_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		gradient_boosting_regression::learn(intercept_only, Regressor),
		gradient_boosting_regression::predict(Regressor, [dummy-0], Prediction).

	test(gradient_boosting_regression_predict_3_mixed_signal, deterministic(Prediction =~= 175.0)) :-
		gradient_boosting_regression::learn(mixed_signal, Regressor, [number_of_estimators(3), learning_rate(1.0), maximum_depth(4), feature_scaling(false)]),
		gradient_boosting_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(gradient_boosting_regression_export_to_clauses_4, deterministic(Prediction =~= 20.0)) :-
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(2)]),
		gradient_boosting_regression::export_to_clauses(step_signal, Regressor, regress, [Clause]),
		gradient_boosting_regression::predict(Clause, [x-9], Prediction).

	test(gradient_boosting_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		gradient_boosting_regression::learn(step_signal, Regressor),
		gradient_boosting_regression::export_to_file(step_signal, Regressor, regress, File).

	test(gradient_boosting_regression_export_to_file_4_loaded, deterministic(Prediction =~= 10.0)) :-
		^^file_path('test_output.pl', File),
		gradient_boosting_regression::learn(step_signal, Regressor, [number_of_estimators(5), learning_rate(1.0), maximum_depth(2)]),
		gradient_boosting_regression::export_to_file(step_signal, Regressor, regress, File),
		logtalk_load(File),
		{regress(InitialPrediction, WeightedTrees, Diagnostics)},
		gradient_boosting_regression::predict(regress(InitialPrediction, WeightedTrees, Diagnostics), [x-2], Prediction).

	test(gradient_boosting_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		gradient_boosting_regression::learn(step_signal, Regressor),
		gradient_boosting_regression::print_regressor(Regressor).

	test(gradient_boosting_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		gradient_boosting_regression::learn(invalid_target, _Regressor).

	test(gradient_boosting_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		gradient_boosting_regression::learn(mixed_signal, Regressor, [number_of_estimators(3), learning_rate(1.0), maximum_depth(4)]),
		gradient_boosting_regression::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

:- end_object.
