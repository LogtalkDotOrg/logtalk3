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
		date is 2026-04-27,
		comment is 'Unit tests for the "random_forest_regression" library.'
	]).

	:- uses(lgtunit, [op(700, xfx, =~=), (=~=)/2]).
	:- uses(list, [length/2, memberchk/2]).

	cover(random_forest_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(random_forest_regression_learn_2_step_signal, deterministic(ground(Regressor))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_valid_regressor_1, deterministic(random_forest_regression::valid_regressor(Regressor))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_invalid_regressor_1, fail) :-
		random_forest_regression::learn(intercept_only, rf_regressor([tree(TreeRegressor, _AttributeNames)], Options), [number_of_trees(1)]),
		random_forest_regression::valid_regressor(rf_regressor([tree(TreeRegressor, [dummy, dummy])], Options)).

	test(random_forest_regression_learn_2_structure, deterministic(functor(Regressor, rf_regressor, 2))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_learn_3_custom_num_trees, deterministic((length(Trees, 5), memberchk(number_of_trees(5), Options), memberchk(maximum_depth(4), Options), memberchk(minimum_samples_leaf(2), Options), memberchk(feature_scaling(true), Options), memberchk(random_seed(17), Options)))) :-
		random_forest_regression::learn(step_signal, rf_regressor(Trees, Options), [number_of_trees(5), maximum_depth(4), minimum_samples_leaf(2), feature_scaling(true), random_seed(17)]).

	test(random_forest_regression_learn_3_same_seed_same_regressor, deterministic(Regressor1 == Regressor2)) :-
		random_forest_regression::learn(step_signal, Regressor1, [number_of_trees(5), random_seed(19)]),
		random_forest_regression::learn(step_signal, Regressor2, [number_of_trees(5), random_seed(19)]).

	test(random_forest_regression_trees_respect_max_features, deterministic(FeatureCount =< 2)) :-
		random_forest_regression::learn(mixed_signal, rf_regressor(Trees, _Options), [number_of_trees(4), maximum_features_per_tree(2)]),
		memberchk(tree(_Tree, FeatureNames), Trees),
		length(FeatureNames, FeatureCount).

	test(random_forest_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		random_forest_regression::learn(intercept_only, Regressor, [number_of_trees(5)]),
		random_forest_regression::predict(Regressor, [dummy-0], Prediction).

	test(random_forest_regression_predict_3_step_signal_left_band, true(Prediction < 15.0)) :-
		random_forest_regression::learn(step_signal, Regressor, [number_of_trees(11), maximum_features_per_tree(1), minimum_samples_leaf(2)]),
		random_forest_regression::predict(Regressor, [x-1.5], Prediction).

	test(random_forest_regression_predict_3_step_signal_right_band, true(Prediction > 15.0)) :-
		random_forest_regression::learn(step_signal, Regressor, [number_of_trees(11), maximum_features_per_tree(1), minimum_samples_leaf(2)]),
		random_forest_regression::predict(Regressor, [x-8.5], Prediction).

	test(random_forest_regression_export_to_clauses_4, deterministic(Prediction =~= 7.0)) :-
		random_forest_regression::learn(intercept_only, Regressor, [number_of_trees(3)]),
		random_forest_regression::export_to_clauses(intercept_only, Regressor, regress, [Clause]),
		random_forest_regression::predict(Clause, [dummy-0], Prediction).

	test(random_forest_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		random_forest_regression::learn(intercept_only, Regressor),
		random_forest_regression::export_to_file(intercept_only, Regressor, regress, File).

	test(random_forest_regression_export_to_file_4_loaded, deterministic(Prediction =~= 7.0)) :-
		^^file_path('test_output.pl', File),
		random_forest_regression::learn(intercept_only, Regressor, [number_of_trees(3)]),
		random_forest_regression::export_to_file(intercept_only, Regressor, regress, File),
		logtalk_load(File),
		{regress(Trees, Options)},
		random_forest_regression::predict(regress(Trees, Options), [dummy-0], Prediction).

	test(random_forest_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		random_forest_regression::learn(intercept_only, Regressor),
		random_forest_regression::print_regressor(Regressor).

	test(random_forest_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		random_forest_regression::learn(invalid_target, _Regressor).

	test(random_forest_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		random_forest_regression::learn(mixed_signal, Regressor, [number_of_trees(1), maximum_features_per_tree(3)]),
		random_forest_regression::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

:- end_object.
