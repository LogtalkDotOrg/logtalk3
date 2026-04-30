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


:- object(lof_identical_query_fixture,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-0.00]).
	example(2, normal, [x-10.00]).
	example(3, normal, [x-10.10]).
	example(4, normal, [x-10.20]).

:- end_object.


:- object(lof_empty_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

:- end_object.


:- object(lof_singleton_anomalies,
	implements(anomaly_dataset_protocol)).

	attribute_values(x, continuous).

	class(label).

	class_values([normal, anomaly]).

	example(1, normal, [x-1.00]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Unit tests for the "lof" library.'
	]).

	:- uses(list, [
		length/2, member/2, take/3
	]).

	:- uses(lof, [
		anomaly_detector_options/2,
		check_anomaly_detector/1,
		diagnostic/2, diagnostics/2,
		export_to_clauses/4, export_to_file/4, learn/2, learn/3, predict/3, predict/4,
		print_anomaly_detector/1, score/3, score_all/3,
		valid_anomaly_detector/1
	]).

	cover(lof).

	cleanup :-
		^^clean_file('test_output.pl').

	test(lof_learn_2_gaussian, true(ground(Detector))) :-
		learn(gaussian_anomalies, Detector).

	test(lof_learn_2_mixed, true(ground(Detector))) :-
		learn(mixed_anomalies, Detector).

	test(lof_score_3_gaussian_anomaly_higher_than_normal, true(AnomalyScore > NormalScore)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(lof_score_3_sensor_missing_value, true((Score >= 0.0, Score =< 1.0))) :-
		learn(sensor_anomalies, Detector, [k(3)]),
		score(Detector, [temperature-101.0, pressure-_, vibration-2.10], Score).

	test(lof_score_3_mixed_distance_ordering, true((NumericScore > BaseScore, CategoricalScore > BaseScore, CombinedScore > NumericScore, CombinedScore > CategoricalScore, MissingCategoricalScore > BaseScore, MissingNumericScore > BaseScore))) :-
		learn(mixed_distance_behaviors, Detector, [k(3)]),
		score(Detector, [size-10.05, weight-100.05, color-red,  shape-round], BaseScore),
		score(Detector, [size-12.0,  weight-104.0,  color-red,  shape-round], NumericScore),
		score(Detector, [size-10.05, weight-100.05, color-blue, shape-square], CategoricalScore),
		score(Detector, [size-12.0,  weight-104.0,  color-blue, shape-square], CombinedScore),
		score(Detector, [size-12.0,  weight-104.0,  color-_,    shape-_], MissingCategoricalScore),
		score(Detector, [size-_,     weight-_,      color-blue, shape-square], MissingNumericScore).

	test(lof_score_3_mixed_distance_manhattan, true(CombinedScore > BaseScore)) :-
		learn(mixed_distance_behaviors, Detector, [k(3), distance_metric(manhattan)]),
		score(Detector, [size-10.05, weight-100.05, color-red,  shape-round], BaseScore),
		score(Detector, [size-12.0,  weight-104.0,  color-blue, shape-square], CombinedScore).

	test(lof_score_3_fresh_identical_query_is_not_leave_one_out, true((FreshScore =:= 0.0, TrainingScore > FreshScore))) :-
		learn(lof_identical_query_fixture, Detector, [k(1)]),
		score(Detector, [x-0.00], FreshScore),
		score_all(lof_identical_query_fixture, Detector, Scores),
		member(1-normal-TrainingScore, Scores).

	test(lof_learn_3_empty_dataset_error, error(domain_error(non_empty_dataset, lof_empty_anomalies))) :-
		learn(lof_empty_anomalies, _Detector, [k(1)]).

	test(lof_score_3_singleton_dataset_bounded_and_consistent, true((FreshScore =:= TrainingScore, FreshScore >= 0.0, FreshScore =< 1.0))) :-
		learn(lof_singleton_anomalies, Detector, [k(3)]),
		score(Detector, [x-1.00], FreshScore),
		score_all(lof_singleton_anomalies, Detector, [1-normal-TrainingScore]).

	test(lof_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn(lof_singleton_anomalies, Detector, [k(3)]).

	test(lof_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, lof_detector(lof_singleton_anomalies, [x], [numeric], [1.0], [1-normal-[1.0]], [], [k(1)])))) :-
		check_anomaly_detector(lof_detector(lof_singleton_anomalies, [x], [numeric], [1.0], [1-normal-[1.0]], [], [k(1)])).

	test(lof_diagnostics_2, deterministic((memberchk(model(lof), Diagnostics), memberchk(training_dataset(gaussian_anomalies), Diagnostics), memberchk(example_count(48), Diagnostics), memberchk(reference_score_count(48), Diagnostics)))) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		diagnostics(Detector, Diagnostics).

	test(lof_anomaly_detector_options_2, deterministic(memberchk(k(5), Options))) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		anomaly_detector_options(Detector, Options).

	test(lof_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		diagnostics(Detector, Diagnostics),
		findall(Diagnostic, diagnostic(Detector, Diagnostic), Enumerated).

	test(lof_predict_3_gaussian_normal, true(Prediction == normal)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		predict(Detector, [x-0.12, y-0.34], Prediction).

	test(lof_predict_3_gaussian_anomaly, true(Prediction == anomaly)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		predict(Detector, [x-4.50, y-4.20], Prediction).

	test(lof_predict_4_threshold_override, true(Prediction == anomaly)) :-
		learn(gaussian_anomalies, Detector, [k(5), anomaly_threshold(0.99)]),
		predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.4)]).

	test(lof_score_all_3_gaussian_count, true(length(Scores, 48))) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		score_all(gaussian_anomalies, Detector, Scores),
		length(Scores, 48).

	test(lof_score_all_3_gaussian_sorted, true(FirstScore >= SecondScore)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		score_all(gaussian_anomalies, Detector, [_-_-FirstScore, _-_-SecondScore| _]).

	test(lof_score_all_3_gaussian_top_anomalies, true(AnomalyCount >= 5)) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		score_all(gaussian_anomalies, Detector, Scores),
		take(8, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_score_all_3_mixed_top_anomalies, true(AnomalyCount >= 2)) :-
		learn(mixed_anomalies, Detector, [k(3)]),
		score_all(mixed_anomalies, Detector, Scores),
		take(4, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_score_all_3_mixed_distance_top_anomalies, true(AnomalyCount == 2)) :-
		learn(mixed_distance_behaviors, Detector, [k(3)]),
		score_all(mixed_distance_behaviors, Detector, Scores),
		take(2, Scores, TopScores),
		count_class(TopScores, anomaly, AnomalyCount).

	test(lof_export_to_clauses_4, true(ground(Clauses))) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		export_to_clauses(gaussian_anomalies, Detector, detect, Clauses).

	test(lof_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(gaussian_anomalies, Detector, [k(5)]),
		export_to_file(gaussian_anomalies, Detector, detect, File).

	test(lof_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn(gaussian_anomalies, Detector, [k(5)]),
		export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(lof_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn(gaussian_anomalies, Detector, [k(5)]),
		print_anomaly_detector(Detector).

	count_class([], _Class, 0).
	count_class([_-Class-_| Scores], Class, Count) :-
		!,
		count_class(Scores, Class, Rest),
		Count is Rest + 1.
	count_class([_| Scores], Class, Count) :-
		count_class(Scores, Class, Count).

:- end_object.
