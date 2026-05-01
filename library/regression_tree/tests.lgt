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
		comment is 'Unit tests for the "regression_tree" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(regression_tree).

	cleanup :-
		^^clean_file('test_output.pl').

	test(regression_tree_learn_2_step_signal, deterministic(ground(Regressor))) :-
		regression_tree::learn(step_signal, Regressor).

	test(regression_tree_valid_regressor_1, deterministic(regression_tree::valid_regressor(Regressor))) :-
		regression_tree::learn(step_signal, Regressor).

	test(regression_tree_invalid_regressor_1, fail) :-
		regression_tree::valid_regressor(regression_tree_regressor(
			[continuous(x, 0.0, 1.0)],
			[feature(x, value), feature(x, missing)],
			node(3, 0.5, 7.0, leaf(1.0), leaf(2.0)),
			[model(regression_tree), target(y), training_example_count(5), options([maximum_depth(10), minimum_samples_leaf(1), minimum_variance_reduction(0.0), maximum_features_per_split(all), feature_scaling(false)]), encoded_feature_count(2)]
		)).

	test(regression_tree_predict_3_step_signal_left_band, deterministic(Prediction =~= 10.0)) :-
		regression_tree::learn(step_signal, Regressor, [minimum_samples_leaf(2)]),
		regression_tree::predict(Regressor, [x-1.5], Prediction).

	test(regression_tree_predict_3_step_signal_right_band, deterministic(Prediction =~= 20.0)) :-
		regression_tree::learn(step_signal, Regressor, [minimum_samples_leaf(2)]),
		regression_tree::predict(Regressor, [x-8.5], Prediction).

	test(regression_tree_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		regression_tree::learn(intercept_only, Regressor),
		regression_tree::predict(Regressor, [dummy-0], Prediction).

	test(regression_tree_predict_3_mixed_signal, deterministic(Prediction =~= 175.0)) :-
		regression_tree::learn(mixed_signal, Regressor, [feature_scaling(false)]),
		regression_tree::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(regression_tree_learn_3_default_maximum_features_per_split, deterministic(member(maximum_features_per_split(all), Options))) :-
		regression_tree::learn(step_signal, Regressor),
		regression_tree::regressor_options(Regressor, Options).

	test(regression_tree_learn_3_custom_maximum_features_per_split, deterministic((member(maximum_features_per_split(1), Options), regression_tree::valid_regressor(Regressor)))) :-
		regression_tree::learn(mixed_signal, Regressor, [maximum_features_per_split(1), feature_scaling(false)]),
		regression_tree::regressor_options(Regressor, Options).

	test(regression_tree_learn_3_custom_options, deterministic((member(maximum_depth(4), Options), member(minimum_samples_leaf(2), Options), member(minimum_variance_reduction(0.1), Options), member(feature_scaling(true), Options)))) :-
		regression_tree::learn(step_signal, Regressor, [maximum_depth(4), minimum_samples_leaf(2), minimum_variance_reduction(0.1), feature_scaling(true)]),
		regression_tree::regressor_options(Regressor, Options).

	test(regression_tree_diagnostics_2, deterministic((member(model(regression_tree), Diagnostics), member(encoded_feature_count(2), Diagnostics), member(options(Options), Diagnostics), member(maximum_features_per_split(all), Options)))) :-
		regression_tree::learn(step_signal, Regressor),
		regression_tree::diagnostics(Regressor, Diagnostics).

	test(regression_tree_export_to_clauses_4, deterministic(Prediction =~= 20.0)) :-
		regression_tree::learn(step_signal, Regressor),
		regression_tree::export_to_clauses(step_signal, Regressor, regress, [Clause]),
		regression_tree::predict(Clause, [x-9], Prediction).

	test(regression_tree_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		regression_tree::learn(step_signal, Regressor),
		regression_tree::export_to_file(step_signal, Regressor, regress, File).

	test(regression_tree_export_to_file_4_loaded, deterministic(Prediction =~= 10.0)) :-
		^^file_path('test_output.pl', File),
		regression_tree::learn(step_signal, Regressor),
		regression_tree::export_to_file(step_signal, Regressor, regress, File),
		logtalk_load(File),
		{regress(Encoders, FeatureLabels, Tree, Diagnostics)},
		regression_tree::predict(regress(Encoders, FeatureLabels, Tree, Diagnostics), [x-2], Prediction).

	test(regression_tree_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		regression_tree::learn(step_signal, Regressor),
		regression_tree::print_regressor(Regressor).

	test(regression_tree_learn_2_invalid_target, error(type_error(number, bad))) :-
		regression_tree::learn(invalid_target, _Regressor).

	test(regression_tree_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		regression_tree::learn(mixed_signal, Regressor),
		regression_tree::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

:- end_object.
