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
		date is 2026-08-09,
		comment is 'Unit tests for the "one_class_svm_anomaly_detector" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(one_class_svm_anomaly_detector, [
		anomaly_detector_options/2, check_anomaly_detector/1, diagnostics/2, export_to_clauses/4,
		export_to_file/4, learn/2, learn/3, predict/3, predict/4, print_anomaly_detector/1, score/3,
		score_all/3, valid_anomaly_detector/1
	]).

	cover(one_class_svm_anomaly_detector).

	cleanup :-
		^^clean_file('test_output.pl').

	learn_gaussian(Detector) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter)]).

	test(one_class_svm_learn_2_rejects_labeled_anomalies, error(domain_error(baseline_only_training_data, gaussian_anomalies))) :-
		learn(gaussian_anomalies, _Detector).

	test(one_class_svm_learn_3_gaussian, deterministic(ground(Detector))) :-
		learn_gaussian(Detector).

	test(one_class_svm_valid_anomaly_detector_1, deterministic(valid_anomaly_detector(Detector))) :-
		learn_gaussian(Detector).

	test(one_class_svm_prunes_zero_coefficients, deterministic((SupportVectorCount < ExampleCount, ReferenceScoreCount == ExampleCount))) :-
		learn_gaussian(one_class_svm_detector(_Encoders, _Kernel, SupportVectors, Coefficients, ReferenceScores, Diagnostics)),
		length(SupportVectors, SupportVectorCount),
		length(Coefficients, SupportVectorCount),
		length(ReferenceScores, ReferenceScoreCount),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(support_vectors(SupportVectorCount), Diagnostics).

	test(one_class_svm_support_vector_tolerance, deterministic((PrunedCount < DefaultCount, AnomalyScore > NormalScore))) :-
		learn_gaussian(one_class_svm_detector(_Encoders0, _Kernel0, DefaultSupportVectors, _Coefficients0, _ReferenceScores0, _Diagnostics0)),
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), support_vector_tolerance(0.01)]),
		Detector = one_class_svm_detector(_Encoders, _Kernel, PrunedSupportVectors, _Coefficients, _ReferenceScores, _Diagnostics),
		length(DefaultSupportVectors, DefaultCount),
		length(PrunedSupportVectors, PrunedCount),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(one_class_svm_support_vector_tolerance_retains_largest, deterministic((SupportVectors = [_], Coefficients = [_], Score >= 0.0, Score =< 1.0))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), support_vector_tolerance(1.0)]),
		Detector = one_class_svm_detector(_Encoders, _Kernel, SupportVectors, Coefficients, _ReferenceScores, _Diagnostics),
		valid_anomaly_detector(Detector),
		score(Detector, [x-4.50, y-4.20], Score).

	test(one_class_svm_check_anomaly_detector_1_invalid, error(domain_error(anomaly_detector, one_class_svm_detector([], linear, [], [], [], [])))) :-
		check_anomaly_detector(one_class_svm_detector([], linear, [], [], [], [])).

	test(one_class_svm_score_3_gaussian_ordering, deterministic((AnomalyScore > NormalScore, NormalScore >= 0.0, AnomalyScore =< 1.0))) :-
		learn_gaussian(Detector),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore).

	test(one_class_svm_score_3_mixed, deterministic((AnomalyScore > NormalScore, AnomalyScore =< 1.0))) :-
		learn(mixed_anomalies, Detector, [baseline_selection_policy(filter)]),
		score(Detector, [age-35, income-60000, student-yes, credit_rating-excellent], NormalScore),
		score(Detector, [age-19, income-150000, student-no, credit_rating-excellent], AnomalyScore).

	test(one_class_svm_score_3_unseen_categorical_value, deterministic((Score >= 0.0, Score =< 1.0))) :-
		learn(mixed_anomalies, Detector, [baseline_selection_policy(filter)]),
		score(Detector, [age-35, income-60000, student-unknown, credit_rating-excellent], Score).

	test(one_class_svm_linear_kernel, deterministic(ground(Detector))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), kernel(linear)]).

	test(one_class_svm_polynomial_kernel, deterministic(ground(Detector))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), kernel(polynomial(2, 0.5, 1.0))]).

	test(one_class_svm_inverse_scaling_learning_schedule, deterministic((AnomalyScore > NormalScore, memberchk(learning_schedule(inverse_scaling(0.5)), Options)))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), learning_schedule(inverse_scaling(0.5))]),
		score(Detector, [x-0.12, y-0.34], NormalScore),
		score(Detector, [x-4.50, y-4.20], AnomalyScore),
		anomaly_detector_options(Detector, Options).

	test(one_class_svm_invalid_nu, error(domain_error(option, nu(0.0)))) :-
		learn(gaussian_anomalies, _Detector, [nu(0.0)]).

	test(one_class_svm_invalid_learning_schedule, error(domain_error(option, learning_schedule(inverse_scaling(0.0))))) :-
		learn(gaussian_anomalies, _Detector, [learning_schedule(inverse_scaling(0.0))]).

	test(one_class_svm_invalid_support_vector_tolerance, error(domain_error(option, support_vector_tolerance(-0.01)))) :-
		learn(gaussian_anomalies, _Detector, [support_vector_tolerance(-0.01)]).

	test(one_class_svm_predict_3_gaussian_normal, deterministic(Prediction == normal)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-0.12, y-0.34], Prediction).

	test(one_class_svm_predict_3_gaussian_anomaly, deterministic(Prediction == anomaly)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction).

	test(one_class_svm_predict_4_threshold_override, deterministic(Prediction == anomaly)) :-
		learn_gaussian(Detector),
		predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.5)]).

	test(one_class_svm_score_all_3_sorted, deterministic((length(Scores, 48), FirstScore >= SecondScore))) :-
		learn_gaussian(Detector),
		score_all(gaussian_anomalies, Detector, Scores),
		Scores = [_-_-FirstScore, _-_-SecondScore| _].

	test(one_class_svm_diagnostics_2, deterministic((memberchk(model(one_class_svm_anomaly_detector), Diagnostics), memberchk(kernel(rbf(0.5)), Diagnostics), memberchk(example_count(40), Diagnostics)))) :-
		learn_gaussian(Detector),
		diagnostics(Detector, Diagnostics).

	test(one_class_svm_anomaly_detector_options_2, deterministic((memberchk(nu(0.2), Options), memberchk(baseline_selection_policy(filter), Options)))) :-
		learn(gaussian_anomalies, Detector, [baseline_selection_policy(filter), nu(0.2)]),
		anomaly_detector_options(Detector, Options).

	test(one_class_svm_export_to_clauses_4, deterministic(ExportedDetector == Detector)) :-
		learn_gaussian(Detector),
		export_to_clauses(gaussian_anomalies, Detector, detector, [detector(ExportedDetector)]).

	test(one_class_svm_export_to_file_4_loadable, deterministic(Prediction == anomaly)) :-
		^^file_path('test_output.pl', File),
		learn_gaussian(Detector),
		export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(one_class_svm_print_anomaly_detector_1, deterministic) :-
		^^suppress_text_output,
		learn_gaussian(Detector),
		print_anomaly_detector(Detector).

:- end_object.
