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
		date is 2026-04-20,
		comment is 'Unit tests for the "lof" library.'
	]).

	:- uses(list, [
		length/2, take/3
	]).

	:- uses(lof, [
		anomaly_detector_to_clauses/4, anomaly_detector_to_file/4,
		learn/2, learn/3, predict/3, predict/4,
		print_anomaly_detector/1, score/3, score_all/3
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

	test(lof_anomaly_detector_to_clauses_4, true(ground(Clauses))) :-
		learn(gaussian_anomalies, Detector, [k(5)]),
		anomaly_detector_to_clauses(gaussian_anomalies, Detector, detect, Clauses).

	test(lof_anomaly_detector_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(gaussian_anomalies, Detector, [k(5)]),
		anomaly_detector_to_file(gaussian_anomalies, Detector, detect, File).

	test(lof_anomaly_detector_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn(gaussian_anomalies, Detector, [k(5)]),
		anomaly_detector_to_file(gaussian_anomalies, Detector, detector, File),
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
