%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

:- object(isolation_forest_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


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


:- object(missing_value_branching_fixture,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.00]).
	example(2, normal, [x-0.10]).
	example(3, normal, [x-0.20]).
	example(4, normal, [x-0.30]).
	example(5, anomaly, [x-5.00]).
	example(6, anomaly, [x-5.20]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Unit tests for the "isolation_forest" library.'
	]).

	:- uses(list, [
		length/2, take/3, memberchk/2
	]).

	cover(isolation_forest).

	cleanup :-
		^^clean_file('test_output.pl').

	learn_filtered(Dataset, Model) :-
		isolation_forest::learn(Dataset, Model, [baseline_selection_policy(filter)]).

	learn_filtered(Dataset, ExtraOptions, Model) :-
		isolation_forest::learn(Dataset, Model, [baseline_selection_policy(filter)| ExtraOptions]).

	% ===================================================================
	% learn/2 tests - gaussian_anomalies dataset
	% ===================================================================

	test(isolation_forest_learn_2_gaussian_anomalies_error, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		isolation_forest::learn(gaussian_anomalies, _Model).

	test(isolation_forest_learn_3_gaussian_anomalies_filter, true(ground(Model))) :-
		learn_filtered(gaussian_anomalies, Model).

	test(isolation_forest_learn_2_gaussian_model_structure, true(functor(Model, if_model, 6))) :-
		learn_filtered(gaussian_anomalies, Model).

	test(isolation_forest_valid_anomaly_detector_1, deterministic(isolation_forest::valid_anomaly_detector(Model))) :-
		learn_filtered(gaussian_anomalies, Model).

	test(isolation_forest_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, if_model([external(0)], 2, [x], [x-continuous], [0.0-1.0], [model(isolation_forest), tree_count(1), subsample_size(2), attribute_names([x]), feature_count(1), options([number_of_trees(10)])])))) :-
		isolation_forest::check_anomaly_detector(if_model([external(0)], 2, [x], [x-continuous], [0.0-1.0], [model(isolation_forest), tree_count(1), subsample_size(2), attribute_names([x]), feature_count(1), options([number_of_trees(10)])])).

	test(isolation_forest_diagnostics_2, deterministic((memberchk(model(isolation_forest), Diagnostics), memberchk(tree_count(50), Diagnostics), memberchk(subsample_size(40), Diagnostics), memberchk(feature_count(2), Diagnostics)))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::diagnostics(Model, Diagnostics).

	test(isolation_forest_anomaly_detector_options_2, deterministic((memberchk(number_of_trees(50), Options), memberchk(baseline_selection_policy(filter), Options)))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::anomaly_detector_options(Model, Options).

	test(isolation_forest_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::diagnostics(Model, Diagnostics),
		findall(Diagnostic, isolation_forest::diagnostic(Model, Diagnostic), Enumerated).

	test(isolation_forest_learn_3_empty_dataset_error, error(domain_error(non_empty_dataset, isolation_forest_empty_anomalies))) :-
		isolation_forest::learn(isolation_forest_empty_anomalies, _Model, [number_of_trees(10)]).

	test(isolation_forest_learn_2_gaussian_default_trees, true(NumTrees == 100)) :-
		learn_filtered(gaussian_anomalies, Model),
		Model = if_model(Trees, _, _, _, _, _),
		length(Trees, NumTrees).

	% ===================================================================
	% learn/3 tests - with options
	% ===================================================================

	test(isolation_forest_learn_3_custom_trees, true(NumTrees == 20)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(20)], Model),
		Model = if_model(Trees, _, _, _, _, _),
		length(Trees, NumTrees).

	test(isolation_forest_learn_3_custom_subsample, true(ground(Model))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(10), subsample_size(32)], Model).

	test(isolation_forest_learn_3_extension_level_0, true(ground(Model))) :-
		% Extension level 0 should behave like original Isolation Forest
		learn_filtered(gaussian_anomalies, [number_of_trees(10), extension_level(0)], Model).

	% ===================================================================
	% score/3 tests - gaussian_anomalies dataset
	% ===================================================================

	test(isolation_forest_score_3_gaussian_normal_point, true(Score < 0.65)) :-
		% A point at the center of the cluster should have a low anomaly score
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score(Model, [x-0.12, y-0.34], Score).

	test(isolation_forest_score_3_gaussian_anomaly_point, true(Score > 0.5)) :-
		% A far-away point should have a high anomaly score
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score(Model, [x-4.50, y-4.20], Score).

	test(isolation_forest_score_3_gaussian_anomaly_higher_than_normal, true(AnomalyScore > NormalScore)) :-
		% Anomalous points should consistently score higher than normal points
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score(Model, [x-0.12, y-0.34], NormalScore),
		isolation_forest::score(Model, [x-4.50, y-4.20], AnomalyScore).

	test(isolation_forest_score_3_gaussian_score_range, true((Score >= 0.0, Score =< 1.0))) :-
		% Anomaly scores should be in [0, 1]
		learn_filtered(gaussian_anomalies, [number_of_trees(20)], Model),
		isolation_forest::score(Model, [x-0.12, y-0.34], Score).

	% ===================================================================
	% predict/3 tests - gaussian_anomalies dataset
	% ===================================================================

	test(isolation_forest_predict_3_gaussian_normal, true(Prediction == normal)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [x-0.12, y-0.34], Prediction).

	test(isolation_forest_predict_3_gaussian_anomaly, true(Prediction == anomaly)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [x-4.50, y-4.20], Prediction).

	test(isolation_forest_predict_3_gaussian_custom_threshold, true(ground(Prediction))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(50), anomaly_threshold(0.6)], Model),
		isolation_forest::predict(Model, [x-0.12, y-0.34], Prediction).

	% ===================================================================
	% score_all/3 tests - gaussian_anomalies dataset
	% ===================================================================

	test(isolation_forest_score_all_3_gaussian, true(length(Scores, 48))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score_all(gaussian_anomalies, Model, Scores),
		length(Scores, 48).

	test(isolation_forest_score_all_3_gaussian_sorted_desc, true(FirstScore >= SecondScore)) :-
		% Scores should be sorted in descending order
		learn_filtered(gaussian_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score_all(gaussian_anomalies, Model, [_-_-FirstScore, _-_-SecondScore| _]).

	test(isolation_forest_score_all_3_gaussian_top_anomalies, true(AnomalyCount >= 4)) :-
		% Most of the top-scoring instances should be actual anomalies
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score_all(gaussian_anomalies, Model, Scores),
		take(8, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	% ===================================================================
	% learn/2 and score/3 tests - shuttle_anomalies dataset
	% ===================================================================

	test(isolation_forest_learn_2_shuttle_anomalies, true(ground(Model))) :-
		learn_filtered(shuttle_anomalies, Model).

	test(isolation_forest_score_shuttle_anomaly_vs_normal, true(AnomalyScore > NormalScore)) :-
		learn_filtered(shuttle_anomalies, [number_of_trees(100)], Model),
		% Normal instance (typical Rad Flow)
		isolation_forest::score(Model, [a1-55, a2-42, a3-13, a4-42, a5-55, a6-13, a7-0, a8-0, a9-0], NormalScore),
		% Anomalous instance (Fpv Close)
		isolation_forest::score(Model, [a1-80, a2-37, a3-43, a4-37, a5-43, a6-6, a7-(-37), a8-(-37), a9-0], AnomalyScore).

	test(isolation_forest_predict_shuttle_normal, true(Prediction == normal)) :-
		learn_filtered(shuttle_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [a1-55, a2-42, a3-13, a4-42, a5-55, a6-13, a7-0, a8-0, a9-0], Prediction).

	test(isolation_forest_predict_shuttle_anomaly, true(Prediction == anomaly)) :-
		learn_filtered(shuttle_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [a1-80, a2-37, a3-43, a4-37, a5-43, a6-6, a7-(-37), a8-(-37), a9-0], Prediction).

	test(isolation_forest_score_all_shuttle_top_anomalies, true(AnomalyCount >= 5)) :-
		% Most of the top-scoring instances should be actual anomalies
		learn_filtered(shuttle_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score_all(shuttle_anomalies, Model, Scores),
		take(10, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	% ===================================================================
	% learn/2 and score/3 tests - water_potability dataset
	% ===================================================================

	test(isolation_forest_learn_2_water_potability, true(ground(Model))) :-
		learn_filtered(water_potability, Model).

	test(isolation_forest_score_water_anomaly_vs_normal, true(AnomalyScore > NormalScore)) :-
		learn_filtered(water_potability, [number_of_trees(100)], Model),
		% Normal water sample
		isolation_forest::score(Model, [ph-7.08, hardness-204.89, solids-20791.32, chloramines-7.30, sulfate-368.52, conductivity-564.31, organic_carbon-10.38, trihalomethanes-86.99, turbidity-2.96], NormalScore),
		% Anomalous water sample (extreme pH, high hardness and solids)
		isolation_forest::score(Model, [ph-3.20, hardness-320.45, solids-45678.90, chloramines-2.10, sulfate-490.34, conductivity-890.12, organic_carbon-25.67, trihalomethanes-150.23, turbidity-7.89], AnomalyScore).

	test(isolation_forest_predict_water_normal, true(Prediction == normal)) :-
		learn_filtered(water_potability, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [ph-7.08, hardness-204.89, solids-20791.32, chloramines-7.30, sulfate-368.52, conductivity-564.31, organic_carbon-10.38, trihalomethanes-86.99, turbidity-2.96], Prediction).

	test(isolation_forest_predict_water_anomaly, true(Prediction == anomaly)) :-
		learn_filtered(water_potability, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [ph-3.20, hardness-320.45, solids-45678.90, chloramines-2.10, sulfate-490.34, conductivity-890.12, organic_carbon-25.67, trihalomethanes-150.23, turbidity-7.89], Prediction).

	test(isolation_forest_score_all_water_top_anomalies, true(AnomalyCount >= 4)) :-
		% Most of the top-scoring instances should be actual anomalies
		learn_filtered(water_potability, [number_of_trees(100)], Model),
		isolation_forest::score_all(water_potability, Model, Scores),
		take(8, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	% ===================================================================
	% predict/4 tests - with options
	% ===================================================================

	test(isolation_forest_predict_4_gaussian_custom_threshold_low, true(Prediction == anomaly)) :-
		% A low threshold should make more points classified as anomalies
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [x-0.12, y-0.34], Prediction, [anomaly_threshold(0.1)]).

	test(isolation_forest_predict_4_gaussian_custom_threshold_high, true(Prediction == normal)) :-
		% A high threshold should make more points classified as normal
		learn_filtered(gaussian_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.99)]).

	test(isolation_forest_predict_4_overrides_model_threshold, true(Prediction == anomaly)) :-
		% The options threshold should override the model threshold
		learn_filtered(gaussian_anomalies, [number_of_trees(100), anomaly_threshold(0.99)], Model),
		isolation_forest::predict(Model, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.5)]).

	test(isolation_forest_predict_4_sensor_missing_values, true(Prediction == anomaly)) :-
		% predict/4 should work with missing values
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [temperature-102.0, pressure-45.0, vibration- _], Prediction, [anomaly_threshold(0.5)]).

	% ===================================================================
	% export_to_clauses/4 tests
	% ===================================================================

	test(isolation_forest_export_to_clauses_4_gaussian, true(N == 1)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_clauses(gaussian_anomalies, Model, iforest, Clauses),
		length(Clauses, N).

	test(isolation_forest_export_to_clauses_4_clause_is_ground, true(ground(Clauses))) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_clauses(gaussian_anomalies, Model, iforest, Clauses).

	test(isolation_forest_export_to_clauses_4_clause_functor, true(Functor == iforest)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_clauses(gaussian_anomalies, Model, iforest, [Clause]),
		functor(Clause, Functor, _).

	test(isolation_forest_export_to_clauses_4_clause_arity, true(Arity == 1)) :-
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_clauses(gaussian_anomalies, Model, iforest, [Clause]),
		functor(Clause, _, Arity).

	test(isolation_forest_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_file(gaussian_anomalies, Model, iforest, File),
		logtalk_load(File),
		{iforest(LoadedModel)},
		isolation_forest::predict(LoadedModel, [x-4.50, y-4.20], Prediction).

	test(isolation_forest_export_to_clauses_4_sensor, true(N == 1)) :-
		learn_filtered(sensor_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_clauses(sensor_anomalies, Model, detect, Clauses),
		length(Clauses, N).

	% ===================================================================
	% export_to_file/4 tests
	% ===================================================================

	test(isolation_forest_export_to_file_4_gaussian, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn_filtered(gaussian_anomalies, [number_of_trees(10)], Model),
		isolation_forest::export_to_file(gaussian_anomalies, Model, iforest, File).

	% ===================================================================
	% print_anomaly_detector/1 tests
	% ===================================================================

	test(isolation_forest_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_filtered(gaussian_anomalies, [number_of_trees(5)], Model),
		isolation_forest::print_anomaly_detector(Model).

	% ===================================================================
	% Extension level tests
	% ===================================================================

	test(isolation_forest_extension_level_0_vs_full, true((ground(Model0), ground(ModelFull)))) :-
		% Both extension levels should produce valid models
		learn_filtered(gaussian_anomalies, [number_of_trees(10), extension_level(0)], Model0),
		learn_filtered(gaussian_anomalies, [number_of_trees(10), extension_level(1)], ModelFull).

	test(isolation_forest_extension_level_0_scores, true(AnomalyScore > NormalScore)) :-
		% Even with extension level 0, anomalies should score higher
		learn_filtered(gaussian_anomalies, [number_of_trees(100), extension_level(0)], Model),
		isolation_forest::score(Model, [x-0.12, y-0.34], NormalScore),
		isolation_forest::score(Model, [x-4.50, y-4.20], AnomalyScore).

	% ===================================================================
	% Missing values tests - sensor_anomalies dataset
	% ===================================================================

	test(isolation_forest_learn_2_sensor_anomalies, true(ground(Model))) :-
		% Dataset with missing values should be learnable
		learn_filtered(sensor_anomalies, Model).

	test(isolation_forest_learn_2_sensor_model_structure, true(functor(Model, if_model, 6))) :-
		% Model should have 6 arguments (includes Ranges)
		learn_filtered(sensor_anomalies, Model).

	test(isolation_forest_score_sensor_normal_complete, true(Score < 0.65)) :-
		% A normal point with all values known should have a low score
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score(Model, [temperature-71.0, pressure-31.0, vibration-0.30], Score).

	test(isolation_forest_score_sensor_anomaly_complete, true(Score > 0.5)) :-
		% An anomalous point with all values known should have a high score
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score(Model, [temperature-102.0, pressure-45.0, vibration-2.20], Score).

	test(isolation_forest_score_sensor_normal_missing_one, true(Score < 0.70)) :-
		% A normal point with one missing value should still score relatively low
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score(Model, [temperature-71.0, pressure- _, vibration-0.30], Score).

	test(isolation_forest_score_sensor_anomaly_missing_one, true(AnomalyScore > NormalScore)) :-
		% An anomalous point with one missing value should score higher than a normal one
		% Use vibration as missing (not involved in most splits for 3D data)
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score(Model, [temperature-71.0, pressure-31.0, vibration- _], NormalScore),
		isolation_forest::score(Model, [temperature-102.0, pressure-45.0, vibration- _], AnomalyScore).

	test(isolation_forest_predict_sensor_normal_missing, true(Prediction == normal)) :-
		% A normal point with a missing value should still be predicted as normal
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [temperature-71.0, pressure-31.0, vibration- _], Prediction).

	test(isolation_forest_predict_sensor_anomaly_missing, true(Prediction == anomaly)) :-
		% An anomalous point with a missing value should still be predicted as anomaly
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::predict(Model, [temperature-102.0, pressure-45.0, vibration- _], Prediction).

	test(isolation_forest_score_all_sensor_with_missing, true(Length == 40)) :-
		% All instances should be scored, including those with missing values
		learn_filtered(sensor_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score_all(sensor_anomalies, Model, Scores),
		length(Scores, Length).

	test(isolation_forest_score_all_sensor_top_anomalies, true(AnomalyCount >= 5)) :-
		% Most of the top-scoring instances should be actual anomalies
		learn_filtered(sensor_anomalies, [number_of_trees(100)], Model),
		isolation_forest::score_all(sensor_anomalies, Model, Scores),
		take(10, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(isolation_forest_score_sensor_score_range_with_missing, true((Score >= 0.0, Score =< 1.0))) :-
		% Scores should still be in [0, 1] even with missing values
		learn_filtered(sensor_anomalies, [number_of_trees(50)], Model),
		isolation_forest::score(Model, [temperature- _, pressure- _, vibration-0.30], Score).

	test(isolation_forest_missing_split_branches_both_ways, true((MissingScore > NormalScore, MissingScore < AnomalyScore))) :-
		learn_filtered(missing_value_branching_fixture, [number_of_trees(256), subsample_size(6), extension_level(0)], Model),
		isolation_forest::score(Model, [x-0.15], NormalScore),
		isolation_forest::score(Model, [x- _], MissingScore),
		isolation_forest::score(Model, [x-5.10], AnomalyScore).

	% ===================================================================
	% Auxiliary predicates
	% ===================================================================

	count_class([], _, 0).
	count_class([_-Class-_| Rest], Class, Count) :-
		!,
		count_class(Rest, Class, RestCount),
		Count is RestCount + 1.
	count_class([_| Rest], Class, Count) :-
		count_class(Rest, Class, Count).

:- end_object.
