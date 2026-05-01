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


:- object(ewma_empty_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(ewma_singleton_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1-1.00]).

:- end_object.


:- object(ewma_featureless_sequences,
	implements(anomaly_dataset_protocol)).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, []).

:- end_object.


:- object(ewma_shift_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).
	attribute_values(t4, continuous).
	attribute_values(t5, continuous).
	attribute_values(t6, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1-0.10,  t2- -0.05, t3-0.03,  t4- -0.08, t5-0.05,  t6-0.00]).
	example(2, normal, [t1- -0.12, t2-0.07,  t3- -0.02, t4-0.09,  t5- -0.04, t6-0.02]).
	example(3, normal, [t1-0.05,  t2-0.02,  t3- -0.06, t4-0.04,  t5-0.01,  t6- -0.03]).
	example(4, normal, [t1- -0.08, t2-0.11,  t3-0.04,  t4- -0.02, t5-0.03,  t6-0.06]).
	example(5, normal, [t1-0.09,  t2- -0.07, t3-0.08,  t4- -0.01, t5- -0.02, t6-0.04]).
	example(6, normal, [t1- -0.04, t2-0.03,  t3- -0.09, t4-0.06,  t5-0.02,  t6- -0.05]).
	example(7, normal, [t1-0.07,  t2-0.01,  t3-0.05,  t4- -0.04, t5- -0.06, t6-0.03]).
	example(8, normal, [t1- -0.09, t2-0.04,  t3- -0.03, t4-0.08,  t5-0.00,  t6- -0.01]).

	example(9, anomaly, [t1-1.20,  t2-1.35,  t3-1.50,  t4-1.40,  t5-1.55,  t6-1.60]).
	example(10, anomaly, [t1-1.10, t2-1.25,  t3-1.45,  t4-1.50,  t5-1.60,  t6-1.70]).
	example(11, anomaly, [t1- -1.15, t2- -1.30, t3- -1.40, t4- -1.50, t5- -1.55, t6- -1.65]).
	example(12, anomaly, [t1- -1.05, t2- -1.20, t3- -1.35, t4- -1.45, t5- -1.60, t6- -1.70]).

:- end_object.


:- object(ewma_high_dimensional_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).
	attribute_values(t4, continuous).
	attribute_values(t5, continuous).
	attribute_values(t6, continuous).
	attribute_values(t7, continuous).
	attribute_values(t8, continuous).
	attribute_values(t9, continuous).
	attribute_values(t10, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [t1- -1.0, t2- -1.0, t3- -1.0, t4- -1.0, t5- -1.0, t6- -1.0, t7- -1.0, t8- -1.0, t9- -1.0, t10- -1.0]).
	example(2, normal, [t1-1.0, t2-1.0, t3-1.0, t4-1.0, t5-1.0, t6-1.0, t7-1.0, t8-1.0, t9-1.0, t10-1.0]).

:- end_object.


:- object(ewma_status_sequences,
	implements(anomaly_dataset_protocol)).

	attribute_values(t1, continuous).
	attribute_values(t2, continuous).
	attribute_values(t3, continuous).

	class(status).

	class_values([stable, drift]).

	example(1, stable, [t1-0.05,  t2- -0.03, t3-0.02]).
	example(2, stable, [t1- -0.04, t2-0.06,  t3- -0.01]).
	example(3, stable, [t1-0.03,  t2-0.01,  t3- -0.02]).

	example(4, drift, [t1-1.20, t2-1.25, t3-1.30]).
	example(5, drift, [t1- -1.15, t2- -1.20, t3- -1.25]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-01,
		comment is 'Unit tests for the "ewma" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2, take/3
	]).

	:- uses(ewma, [
		anomaly_detector_options/2, check_anomaly_detector/1, diagnostic/2, diagnostics/2, export_to_clauses/4,
		export_to_file/4, learn/2, learn/3, predict/3, predict/4, print_anomaly_detector/1, score/3,
		score_all/3, valid_anomaly_detector/1
	]).

	cover(ewma).

	cleanup :-
		^^clean_file('test_output.pl').

	learn_filtered_shift_sequences(Detector) :-
		learn(ewma_shift_sequences, Detector, [baseline_selection_policy(filter)]).

	learn_filtered_shift_sequences(ExtraOptions, Detector) :-
		learn(ewma_shift_sequences, Detector, [baseline_selection_policy(filter)| ExtraOptions]).

	test(ewma_learn_2_high_dimensional_sequences, true(ground(Detector))) :-
		learn(ewma_high_dimensional_sequences, Detector).

	test(ewma_learn_2_non_baseline_dataset_error, error(domain_error(baseline_only_training_data, ewma_shift_sequences))) :-
		learn(ewma_shift_sequences, _Detector).

	test(ewma_learn_3_shift_sequences_filter, true(ground(Detector))) :-
		learn_filtered_shift_sequences(Detector).

	test(ewma_learn_3_custom_baseline_class_values_filter, deterministic(memberchk(example_count(3), Diagnostics))) :-
		learn(ewma_status_sequences, Detector, [baseline_class_values([stable]), baseline_selection_policy(filter)]),
		diagnostics(Detector, Diagnostics).

	test(ewma_learn_3_custom_baseline_class_values_reject_error, error(domain_error(baseline_only_training_data, ewma_status_sequences))) :-
		learn(ewma_status_sequences, _Detector, [baseline_class_values([stable]), baseline_selection_policy(reject)]).

	test(ewma_learn_2_invalid_dataset, error(domain_error(continuous_attribute, student))) :-
		learn(mixed_anomalies, _Detector).

	test(ewma_learn_3_empty_dataset_error, error(domain_error(non_empty_dataset, ewma_empty_sequences))) :-
		learn(ewma_empty_sequences, _Detector, []).

	test(ewma_learn_2_featureless_dataset_error, error(domain_error(non_empty_features, ewma_featureless_sequences))) :-
		learn(ewma_featureless_sequences, _Detector).

	test(ewma_score_3_shift_anomaly_higher_than_normal, true(AnomalyScore > NormalScore)) :-
		learn_filtered_shift_sequences(Detector),
		score(Detector, [t1-0.08, t2-0.02, t3- -0.03, t4-0.05, t5-0.00, t6- -0.01], NormalScore),
		score(Detector, [t1-1.25, t2-1.35, t3-1.45, t4-1.55, t5-1.65, t6-1.75], AnomalyScore).

	test(ewma_score_3_shift_score_range, true((Score >= 0.0, Score < 1.0))) :-
		learn_filtered_shift_sequences(Detector),
		score(Detector, [t1-1.25, t2-1.35, t3-1.45, t4-1.55, t5-1.65, t6-1.75], Score).

	test(ewma_score_3_missing_values, true((Score >= 0.0, Score < 1.0))) :-
		learn_filtered_shift_sequences(Detector),
		score(Detector, [t1-1.20, t2-_, t3-1.30, t4-_, t5-1.40, t6-_], Score).

	test(ewma_predict_3_missing_leading_steps_do_not_advance_update_count, true(Prediction == anomaly)) :-
		learn(ewma_high_dimensional_sequences, Detector),
		predict(Detector, [t1-_, t2-_, t3-_, t4-_, t5-_, t6-_, t7-_, t8-_, t9-_, t10-3.2], Prediction).

	test(ewma_score_3_all_missing_error, error(domain_error(non_empty_known_values, [t1, t2, t3, t4, t5, t6]))) :-
		learn_filtered_shift_sequences(Detector),
		score(Detector, [t1-_, t2-_, t3-_, t4-_, t5-_, t6-_], _Score).

	test(ewma_score_3_singleton_dataset_matching_point, true(Score =:= 0.0)) :-
		learn(ewma_singleton_sequences, Detector),
		score(Detector, [t1-1.00], Score).

	test(ewma_predict_3_shift_normal, true(Prediction == normal)) :-
		learn_filtered_shift_sequences(Detector),
		predict(Detector, [t1-0.08, t2-0.02, t3- -0.03, t4-0.05, t5-0.00, t6- -0.01], Prediction).

	test(ewma_predict_3_shift_anomaly, true(Prediction == anomaly)) :-
		learn_filtered_shift_sequences(Detector),
		predict(Detector, [t1-1.25, t2-1.35, t3-1.45, t4-1.55, t5-1.65, t6-1.75], Prediction).

	test(ewma_predict_4_threshold_override, true(Prediction == anomaly)) :-
		learn_filtered_shift_sequences([anomaly_threshold(0.99)], Detector),
		predict(Detector, [t1-1.25, t2-1.35, t3-1.45, t4-1.55, t5-1.65, t6-1.75], Prediction, [anomaly_threshold(0.5)]).

	test(ewma_predict_4_control_limit_multiplier_override_ignored, true(Prediction == normal)) :-
		learn(ewma_high_dimensional_sequences, Detector, [control_limit_multiplier(5.0)]),
		predict(Detector, [t1-4.0, t2-0.0, t3-0.0, t4-0.0, t5-0.0, t6-0.0, t7-0.0, t8-0.0, t9-0.0, t10-0.0], Prediction, [control_limit_multiplier(3.0)]).

	test(ewma_predict_3_custom_smoothing_factor_changes_boundary, true((LowPrediction == anomaly, HighPrediction == normal))) :-
		learn(ewma_high_dimensional_sequences, LowDetector, [smoothing_factor(0.2)]),
		learn(ewma_high_dimensional_sequences, HighDetector, [smoothing_factor(0.8)]),
		Instance = [t1-1.6, t2-1.6, t3-1.6, t4-1.6, t5-1.6, t6-1.6, t7-1.6, t8-1.6, t9-1.6, t10-1.6],
		predict(LowDetector, Instance, LowPrediction),
		predict(HighDetector, Instance, HighPrediction).

	test(ewma_predict_4_smoothing_factor_override_ignored, true(Prediction == normal)) :-
		learn(ewma_high_dimensional_sequences, Detector, [smoothing_factor(0.8)]),
		predict(Detector, [t1-1.6, t2-1.6, t3-1.6, t4-1.6, t5-1.6, t6-1.6, t7-1.6, t8-1.6, t9-1.6, t10-1.6], Prediction, [smoothing_factor(0.2)]).

	test(ewma_predict_3_singleton_far_point, true(Prediction == anomaly)) :-
		learn(ewma_singleton_sequences, Detector),
		predict(Detector, [t1-5.00], Prediction).

	test(ewma_predict_3_high_dimensional_in_control_sequence, true(Prediction == normal)) :-
		learn(ewma_high_dimensional_sequences, Detector),
		predict(Detector, [t1-0.0, t2-0.0, t3-0.0, t4-0.0, t5-0.0, t6-0.0, t7-0.0, t8-0.0, t9-0.0, t10-0.0], Prediction).

	test(ewma_predict_3_high_dimensional_anomaly, true(Prediction == anomaly)) :-
		learn(ewma_high_dimensional_sequences, Detector),
		predict(Detector, [t1-4.0, t2-4.0, t3-4.0, t4-4.0, t5-4.0, t6-4.0, t7-4.0, t8-4.0, t9-4.0, t10-4.0], Prediction).

	test(ewma_predict_3_high_dimensional_sparse_shift, true(Prediction == anomaly)) :-
		learn(ewma_high_dimensional_sequences, Detector),
		predict(Detector, [t1-4.0, t2-0.0, t3-0.0, t4-0.0, t5-0.0, t6-0.0, t7-0.0, t8-0.0, t9-0.0, t10-0.0], Prediction).

	test(ewma_score_all_3_shift_count, true(length(Scores, 12))) :-
		learn_filtered_shift_sequences(Detector),
		score_all(ewma_shift_sequences, Detector, Scores),
		length(Scores, 12).

	test(ewma_score_all_3_shift_sorted, true(FirstScore >= SecondScore)) :-
		learn_filtered_shift_sequences(Detector),
		score_all(ewma_shift_sequences, Detector, [_-_-FirstScore, _-_-SecondScore| _]).

	test(ewma_score_all_3_shift_top_anomalies, true(AnomalyCount >= 4)) :-
		learn_filtered_shift_sequences(Detector),
		score_all(ewma_shift_sequences, Detector, Scores),
		take(4, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(ewma_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn_filtered_shift_sequences(Detector).

	test(ewma_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, ewma_detector(ewma_shift_sequences, attribute_schema([t2], [t2-1]), [ewma_encoder(t1, 0.0, 1.0)], [model(ewma), training_dataset(ewma_shift_sequences), attribute_names([t1]), feature_count(1), example_count(0), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), control_limit_multiplier(3.0), smoothing_factor(0.2)])])))) :-
		check_anomaly_detector(ewma_detector(ewma_shift_sequences, attribute_schema([t2], [t2-1]), [ewma_encoder(t1, 0.0, 1.0)], [model(ewma), training_dataset(ewma_shift_sequences), attribute_names([t1]), feature_count(1), example_count(0), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), control_limit_multiplier(3.0), smoothing_factor(0.2)])])).

	test(ewma_invalid_anomaly_detector_1_missing_effective_option, error(domain_error(anomaly_detector, ewma_detector(ewma_singleton_sequences, attribute_schema([t1], [t1-1]), [ewma_encoder(t1, 1.0, 1.0)], [model(ewma), training_dataset(ewma_singleton_sequences), attribute_names([t1]), feature_count(1), example_count(1), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), control_limit_multiplier(3.0)])])))) :-
		check_anomaly_detector(ewma_detector(ewma_singleton_sequences, attribute_schema([t1], [t1-1]), [ewma_encoder(t1, 1.0, 1.0)], [model(ewma), training_dataset(ewma_singleton_sequences), attribute_names([t1]), feature_count(1), example_count(1), options([anomaly_threshold(0.5), baseline_class_values([normal]), baseline_selection_policy(reject), control_limit_multiplier(3.0)])])).

	test(ewma_diagnostics_2, deterministic((memberchk(model(ewma), Diagnostics), memberchk(training_dataset(ewma_shift_sequences), Diagnostics), memberchk(feature_count(6), Diagnostics), memberchk(example_count(8), Diagnostics)))) :-
		learn_filtered_shift_sequences(Detector),
		diagnostics(Detector, Diagnostics).

	test(ewma_anomaly_detector_options_2, deterministic((memberchk(anomaly_threshold(0.5), Options), memberchk(baseline_class_values([normal]), Options), memberchk(baseline_selection_policy(filter), Options), memberchk(control_limit_multiplier(3.0), Options), memberchk(smoothing_factor(0.2), Options)))) :-
		learn_filtered_shift_sequences(Detector),
		anomaly_detector_options(Detector, Options).

	test(ewma_anomaly_detector_options_2_custom_control_limit_multiplier, deterministic(memberchk(control_limit_multiplier(5.0), Options))) :-
		learn_filtered_shift_sequences([control_limit_multiplier(5.0)], Detector),
		anomaly_detector_options(Detector, Options).

	test(ewma_anomaly_detector_options_2_custom_smoothing_factor, deterministic(memberchk(smoothing_factor(0.4), Options))) :-
		learn_filtered_shift_sequences([smoothing_factor(0.4)], Detector),
		anomaly_detector_options(Detector, Options).

	test(ewma_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		learn_filtered_shift_sequences(Detector),
		diagnostics(Detector, Diagnostics),
		findall(Diagnostic, diagnostic(Detector, Diagnostic), Enumerated).

	test(ewma_export_to_clauses_4, true(ground(Clauses))) :-
		learn_filtered_shift_sequences(Detector),
		export_to_clauses(ewma_shift_sequences, Detector, detect, Clauses).

	test(ewma_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn_filtered_shift_sequences(Detector),
		export_to_file(ewma_shift_sequences, Detector, detect, File).

	test(ewma_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_filtered_shift_sequences(Detector),
		export_to_file(ewma_shift_sequences, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [t1-1.25, t2-1.35, t3-1.45, t4-1.55, t5-1.65, t6-1.75], Prediction).

	test(ewma_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_filtered_shift_sequences(Detector),
		print_anomaly_detector(Detector).

	count_class(Scores, Class, Count) :-
		count_class(Scores, Class, 0, Count).

	count_class([], _Class, Count, Count).
	count_class([_-Class-_| Scores], Class, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		count_class(Scores, Class, Count1, Count).
	count_class([_| Scores], Class, Count0, Count) :-
		count_class(Scores, Class, Count0, Count).

:- end_object.
