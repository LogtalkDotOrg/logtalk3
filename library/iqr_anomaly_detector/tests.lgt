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


:- object(iqr_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(iqr_singleton_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-1.00]).

:- end_object.


:- object(iqr_featureless_anomalies,
	implements(anomaly_dataset_protocol)).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, []).

:- end_object.


:- object(iqr_high_dimensional_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x1, continuous).
	attribute_values(x2, continuous).
	attribute_values(x3, continuous).
	attribute_values(x4, continuous).
	attribute_values(x5, continuous).
	attribute_values(x6, continuous).
	attribute_values(x7, continuous).
	attribute_values(x8, continuous).
	attribute_values(x9, continuous).
	attribute_values(x10, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x1- -1.0, x2- -1.0, x3- -1.0, x4- -1.0, x5- -1.0, x6- -1.0, x7- -1.0, x8- -1.0, x9- -1.0, x10- -1.0]).
	example(2, normal, [x1-1.0, x2-1.0, x3-1.0, x4-1.0, x5-1.0, x6-1.0, x7-1.0, x8-1.0, x9-1.0, x10-1.0]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "iqr_anomaly_detector" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2, take/3
	]).

	:- uses(iqr_anomaly_detector, [
		anomaly_detector_options/2, check_anomaly_detector/1, diagnostic/2, diagnostics/2, export_to_clauses/4,
		export_to_file/4, learn/2, learn/3, predict/3, predict/4, print_anomaly_detector/1, score/3,
		score_all/3, valid_anomaly_detector/1
	]).

	cover(iqr_anomaly_detector).

	cleanup :-
		^^clean_file('test_output.pl').

	learn_filtered_gaussian_anomalies(Detector) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)]).

	learn_filtered_gaussian_anomalies(ExtraOptions, Detector) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)| ExtraOptions]).

	test(iqr_learn_2_gaussian_error, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		learn(gaussian_anomalies, _Detector).

	test(iqr_learn_3_gaussian_filter, true(ground(Detector))) :-
		learn_filtered_gaussian_anomalies(Detector).

	test(iqr_learn_3_non_baseline_dataset_error, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		learn(gaussian_anomalies, _Detector, [baseline_selection_policy(reject)]).

	test(iqr_learn_2_invalid_dataset, error(domain_error(continuous_attribute, student))) :-
		learn(mixed_anomalies, _Detector).

	test(iqr_learn_3_empty_dataset_error, error(domain_error(non_empty_dataset, iqr_empty_anomalies))) :-
		learn(iqr_empty_anomalies, _Detector, []).

	test(iqr_learn_2_featureless_dataset_error, error(domain_error(non_empty_features, iqr_featureless_anomalies))) :-
		learn(iqr_featureless_anomalies, _Detector).

	test(iqr_score_3_gaussian_anomaly_higher_than_normal, true(AnomalyScore > NormalScore)) :-
		learn_filtered_gaussian_anomalies(Detector),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(iqr_score_3_gaussian_score_range, true((Score >= 0.0, Score < 1.0))) :-
		learn_filtered_gaussian_anomalies(Detector),
		score(Detector, [x-4.50, y-4.20], Score).

	test(iqr_score_3_fence_multiplier_affects_score, true(NarrowFenceScore > WideFenceScore)) :-
		learn_filtered_gaussian_anomalies([fence_multiplier(1.5)], NarrowFenceDetector),
		learn_filtered_gaussian_anomalies([fence_multiplier(3.0)], WideFenceDetector),
		score(NarrowFenceDetector, [x-4.50, y-4.20], NarrowFenceScore),
		score(WideFenceDetector, [x-4.50, y-4.20], WideFenceScore).

	test(iqr_score_3_sensor_missing_value, true((Score >= 0.0, Score < 1.0))) :-
		learn(sensor_anomalies, Detector, [baseline_selection_policy(filter)]),
		score(Detector, [temperature-101.0, pressure-_, vibration-2.10], Score).

	test(iqr_score_3_all_missing_error, error(domain_error(non_empty_known_values, [x, y]))) :-
		learn_filtered_gaussian_anomalies(Detector),
		score(Detector, [x-_, y-_], _Score).

	test(iqr_score_3_singleton_dataset_matching_point, true(Score =:= 0.0)) :-
		learn(iqr_singleton_anomalies, Detector),
		score(Detector, [x-1.00], Score).

	test(iqr_predict_3_gaussian_normal, true(Prediction == normal)) :-
		learn_filtered_gaussian_anomalies(Detector),
		predict(Detector, [x-0.12, y-0.34], Prediction).

	test(iqr_predict_3_gaussian_anomaly, true(Prediction == anomaly)) :-
		learn_filtered_gaussian_anomalies(Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction).

	test(iqr_predict_3_fence_multiplier_changes_default_boundary, true((NarrowFencePrediction == anomaly, WideFencePrediction == normal))) :-
		learn(iqr_singleton_anomalies, NarrowFenceDetector, [fence_multiplier(1.5)]),
		learn(iqr_singleton_anomalies, WideFenceDetector, [fence_multiplier(3.0)]),
		predict(NarrowFenceDetector, [x-2.60], NarrowFencePrediction),
		predict(WideFenceDetector, [x-2.60], WideFencePrediction).

	test(iqr_predict_4_threshold_override, true(Prediction == anomaly)) :-
		learn_filtered_gaussian_anomalies([anomaly_threshold(0.99)], Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.6)]).

	test(iqr_predict_4_score_mode_override_ignored, true(Prediction == anomaly)) :-
		learn(iqr_high_dimensional_anomalies, Detector, [score_mode(any_feature_extreme)]),
		predict(Detector, [x1-4.0, x2-2.0, x3-0.0, x4-0.0, x5-0.0, x6-0.0, x7-0.0, x8-0.0, x9-0.0, x10-0.0], Prediction, [score_mode(root_mean_square)]).

	test(iqr_predict_3_singleton_far_point, true(Prediction == anomaly)) :-
		learn(iqr_singleton_anomalies, Detector),
		predict(Detector, [x-5.00], Prediction).

	test(iqr_predict_3_high_dimensional_training_normal, true(Prediction == normal)) :-
		learn(iqr_high_dimensional_anomalies, Detector),
		predict(Detector, [x1-1.0, x2-1.0, x3-1.0, x4-1.0, x5-1.0, x6-1.0, x7-1.0, x8-1.0, x9-1.0, x10-1.0], Prediction).

	test(iqr_predict_3_high_dimensional_anomaly, true(Prediction == anomaly)) :-
		learn(iqr_high_dimensional_anomalies, Detector),
		predict(Detector, [x1-4.0, x2-4.0, x3-4.0, x4-4.0, x5-4.0, x6-4.0, x7-4.0, x8-4.0, x9-4.0, x10-4.0], Prediction).

	test(iqr_predict_3_high_dimensional_sparse_anomaly_dense_mode, true(Prediction == anomaly)) :-
		learn(iqr_high_dimensional_anomalies, Detector),
		predict(Detector, [x1-4.0, x2-0.0, x3-0.0, x4-0.0, x5-0.0, x6-0.0, x7-0.0, x8-0.0, x9-0.0, x10-0.0], Prediction).

	test(iqr_score_3_inlier_padding_does_not_dilute_fence_anomaly, true(Delta < 1.0e-12)) :-
		learn(iqr_high_dimensional_anomalies, Detector),
		score(Detector, [x1-4.0, x2-0.0, x3-0.0, x4-0.0, x5-0.0, x6-0.0, x7-0.0, x8-0.0, x9-0.0, x10-0.0], DenseScore),
		score(Detector, [x1-4.0, x2-_, x3-_, x4-_, x5-_, x6-_, x7-_, x8-_, x9-_, x10-_], SparseScore),
		Delta is abs(DenseScore - SparseScore).

	test(iqr_predict_3_high_dimensional_sparse_anomaly_any_feature_extreme, true(Prediction == anomaly)) :-
		learn(iqr_high_dimensional_anomalies, Detector, [score_mode(any_feature_extreme)]),
		predict(Detector, [x1-4.0, x2-0.0, x3-0.0, x4-0.0, x5-0.0, x6-0.0, x7-0.0, x8-0.0, x9-0.0, x10-0.0], Prediction).

	test(iqr_score_3_missing_value_dimension_normalization, true(Delta < 1.0e-12)) :-
		learn(iqr_high_dimensional_anomalies, Detector),
		score(Detector, [x1-2.0, x2-2.0, x3-2.0, x4-2.0, x5-2.0, x6-2.0, x7-2.0, x8-2.0, x9-2.0, x10-2.0], FullScore),
		score(Detector, [x1-2.0, x2-2.0, x3-_, x4-_, x5-_, x6-_, x7-_, x8-_, x9-_, x10-_], PartialScore),
		Delta is abs(FullScore - PartialScore).

	test(iqr_score_all_3_gaussian_count, true(length(Scores, 48))) :-
		learn_filtered_gaussian_anomalies(Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		length(Scores, 48).

	test(iqr_score_all_3_gaussian_sorted, true(FirstScore >= SecondScore)) :-
		learn_filtered_gaussian_anomalies(Detector),
		score_all(gaussian_anomalies, Detector, [_-_-FirstScore, _-_-SecondScore| _]).

	test(iqr_score_all_3_gaussian_top_anomalies, true(AnomalyCount >= 6)) :-
		learn_filtered_gaussian_anomalies(Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		take(8, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(iqr_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn_filtered_gaussian_anomalies(Detector).

	test(iqr_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, iqr_detector(gaussian_anomalies, attribute_schema([x], [x-1]), [iqr_anomaly_detector(x, 0.0, 1.0, 1.0)], [model(iqr_anomaly_detector), training_dataset(gaussian_anomalies), attribute_names([x]), feature_count(1), example_count(0), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), fence_multiplier(1.5), score_mode(root_mean_square)])])))) :-
		check_anomaly_detector(iqr_detector(gaussian_anomalies, attribute_schema([x], [x-1]), [iqr_anomaly_detector(x, 0.0, 1.0, 1.0)], [model(iqr_anomaly_detector), training_dataset(gaussian_anomalies), attribute_names([x]), feature_count(1), example_count(0), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), fence_multiplier(1.5), score_mode(root_mean_square)])])).

	test(iqr_diagnostics_2, deterministic([Model, TrainingDataset, FeatureCount, ExampleCount] == [iqr_anomaly_detector, gaussian_anomalies, 2, 40])) :-
		learn_filtered_gaussian_anomalies(Detector),
		diagnostics(Detector, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(training_dataset(TrainingDataset), Diagnostics),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(example_count(ExampleCount), Diagnostics).

	test(iqr_anomaly_detector_options_2, deterministic([AnomalyThreshold, BaselineClassValues, BaselineSelectionPolicy, FenceMultiplier, ScoreMode] == [0.5, [normal], filter, 1.5, root_mean_square])) :-
		learn_filtered_gaussian_anomalies(Detector),
		anomaly_detector_options(Detector, Options),
		memberchk(anomaly_threshold(AnomalyThreshold), Options),
		memberchk(baseline_class_values(BaselineClassValues), Options),
		memberchk(baseline_selection_policy(BaselineSelectionPolicy), Options),
		memberchk(fence_multiplier(FenceMultiplier), Options),
		memberchk(score_mode(ScoreMode), Options).

	test(iqr_anomaly_detector_options_2_custom_fence_multiplier, deterministic([AnomalyThreshold, FenceMultiplier] == [0.5, 3.0])) :-
		learn_filtered_gaussian_anomalies([fence_multiplier(3.0)], Detector),
		anomaly_detector_options(Detector, Options),
		memberchk(anomaly_threshold(AnomalyThreshold), Options),
		memberchk(fence_multiplier(FenceMultiplier), Options).

	test(iqr_anomaly_detector_options_2_any_feature_extreme, deterministic(ScoreMode == any_feature_extreme)) :-
		learn_filtered_gaussian_anomalies([score_mode(any_feature_extreme)], Detector),
		anomaly_detector_options(Detector, Options),
		memberchk(score_mode(ScoreMode), Options).

	test(iqr_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		learn_filtered_gaussian_anomalies(Detector),
		diagnostics(Detector, Diagnostics),
		findall(Diagnostic, diagnostic(Detector, Diagnostic), Enumerated).

	test(iqr_export_to_clauses_4, true(ground(Clauses))) :-
		learn_filtered_gaussian_anomalies(Detector),
		export_to_clauses(gaussian_anomalies, Detector, detect, Clauses).

	test(iqr_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn_filtered_gaussian_anomalies(Detector),
		export_to_file(gaussian_anomalies, Detector, detect, File).

	test(iqr_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_filtered_gaussian_anomalies(Detector),
		export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(iqr_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_filtered_gaussian_anomalies(Detector),
		print_anomaly_detector(Detector).

	% auxiliary predicates

	count_class([], _Class, 0).
	count_class([_-Class-_| Scores], Class, Count) :-
		!,
		count_class(Scores, Class, Rest),
		Count is Rest + 1.
	count_class([_| Scores], Class, Count) :-
		count_class(Scores, Class, Count).

:- end_object.
