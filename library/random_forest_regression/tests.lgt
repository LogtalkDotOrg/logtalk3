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
		comment is 'Unit tests for the "random_forest_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(random_forest_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(random_forest_regression_learn_2_step_signal, deterministic(ground(Regressor))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_valid_regressor_1, deterministic(random_forest_regression::valid_regressor(Regressor))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_invalid_regressor_1, fail) :-
		random_forest_regression::learn(intercept_only, rf_regressor([tree(_TreeRegressor)], Diagnostics), [number_of_trees(1)]),
		random_forest_regression::valid_regressor(rf_regressor([tree(bad)], Diagnostics)).

	test(random_forest_regression_learn_2_structure, deterministic(functor(Regressor, rf_regressor, 2))) :-
		random_forest_regression::learn(step_signal, Regressor).

	test(random_forest_regression_learn_3_custom_num_trees, deterministic((length(Trees, 5), [NumberOfTrees, MaximumDepth, MinimumSamplesLeaf, FeatureScaling, RandomSeed] == [5, 4, 2, true, 17]))) :-
		random_forest_regression::learn(step_signal, Regressor, [number_of_trees(5), maximum_depth(4), minimum_samples_leaf(2), feature_scaling(true), random_seed(17)]),
		Regressor = rf_regressor(Trees, _Diagnostics),
		random_forest_regression::regressor_options(Regressor, Options),
		memberchk(number_of_trees(NumberOfTrees), Options),
		memberchk(maximum_depth(MaximumDepth), Options),
		memberchk(minimum_samples_leaf(MinimumSamplesLeaf), Options),
		memberchk(feature_scaling(FeatureScaling), Options),
		memberchk(random_seed(RandomSeed), Options).

	test(random_forest_regression_learn_3_same_seed_same_regressor, deterministic(Regressor1 == Regressor2)) :-
		random_forest_regression::learn(step_signal, Regressor1, [number_of_trees(5), random_seed(19)]),
		random_forest_regression::learn(step_signal, Regressor2, [number_of_trees(5), random_seed(19)]).

	test(random_forest_regression_trees_respect_max_features, deterministic(MaximumFeaturesPerSplit == 2)) :-
		random_forest_regression::learn(mixed_signal, rf_regressor(Trees, _Diagnostics), [number_of_trees(4), maximum_features_per_split(2)]),
		memberchk(tree(TreeRegressor), Trees),
		regression_tree::regressor_options(TreeRegressor, TreeOptions),
		memberchk(maximum_features_per_split(MaximumFeaturesPerSplit), TreeOptions).

	test(random_forest_regression_trees_accept_all_features_per_split, deterministic(MaximumFeaturesPerSplit == all)) :-
		random_forest_regression::learn(mixed_signal, rf_regressor(Trees, _Diagnostics), [number_of_trees(2), maximum_features_per_split(all)]),
		memberchk(tree(TreeRegressor), Trees),
		regression_tree::regressor_options(TreeRegressor, TreeOptions),
		memberchk(maximum_features_per_split(MaximumFeaturesPerSplit), TreeOptions).

	test(random_forest_regression_diagnostics_2, deterministic([Model, TreeCount, NumberOfTrees] == [random_forest_regression, 3, 3])) :-
		random_forest_regression::learn(intercept_only, Regressor, [number_of_trees(3)]),
		random_forest_regression::diagnostics(Regressor, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(tree_count(TreeCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(number_of_trees(NumberOfTrees), Options).

	test(random_forest_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		random_forest_regression::learn(intercept_only, Regressor, [number_of_trees(5)]),
		random_forest_regression::predict(Regressor, [dummy-0], Prediction).

	test(random_forest_regression_predict_3_step_signal_left_band, true(Prediction < 15.0)) :-
		random_forest_regression::learn(step_signal, Regressor, [number_of_trees(11), maximum_features_per_split(1), minimum_samples_leaf(2)]),
		random_forest_regression::predict(Regressor, [x-1.5], Prediction).

	test(random_forest_regression_predict_3_step_signal_right_band, true(Prediction > 15.0)) :-
		random_forest_regression::learn(step_signal, Regressor, [number_of_trees(11), maximum_features_per_split(1), minimum_samples_leaf(2)]),
		random_forest_regression::predict(Regressor, [x-8.5], Prediction).

	test(random_forest_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		random_forest_regression::learn(sparse_mixed_signal, Regressor, [number_of_trees(9), maximum_features_per_split(2), minimum_samples_leaf(2), feature_scaling(false), random_seed(29)]),
		random_forest_regression::predict(Regressor, [age-20], Prediction).

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
		{regress(Trees, Diagnostics)},
		random_forest_regression::predict(regress(Trees, Diagnostics), [dummy-0], Prediction).

	test(random_forest_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		random_forest_regression::learn(intercept_only, Regressor),
		random_forest_regression::print_regressor(Regressor).

	test(random_forest_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		random_forest_regression::learn(invalid_target, _Regressor).

	test(random_forest_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		random_forest_regression::learn(mixed_signal, Regressor, [number_of_trees(1), maximum_features_per_split(3)]),
		random_forest_regression::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

:- end_object.
