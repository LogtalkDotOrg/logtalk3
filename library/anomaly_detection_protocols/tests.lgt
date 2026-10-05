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
		date is 2026-04-30,
		comment is 'Smoke tests for the "anomaly_detection_protocols" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	% Dataset protocol smoke tests.

	test(gaussian_anomalies_attribute_values, deterministic(Attributes == [x-continuous, y-continuous])) :-
		findall(Attribute-Values, gaussian_anomalies::attribute_values(Attribute, Values), Attributes).

	test(sensor_anomalies_examples_count, deterministic(Count == 40)) :-
		findall(Id, sensor_anomalies::example(Id, _Class, _Values), Ids),
		length(Ids, Count).

	test(mixed_anomalies_examples_count, deterministic(Count == 16)) :-
		findall(Id, mixed_anomalies::example(Id, _Class, _Values), Ids),
		length(Ids, Count).

	test(mixed_distance_behaviors_examples_count, deterministic(Count == 8)) :-
		findall(Id, mixed_distance_behaviors::example(Id, _Class, _Values), Ids),
		length(Ids, Count).

	test(water_potability_class_values, deterministic(ClassValues == [normal, anomaly])) :-
		water_potability::class_values(ClassValues).

	test(gaussian_anomalies_validation, deterministic) :-
		anomaly_test_support::validate_anomaly_dataset(gaussian_anomalies).

	test(sensor_anomalies_validation, deterministic) :-
		anomaly_test_support::validate_anomaly_dataset(sensor_anomalies).

	test(mixed_anomalies_validation, deterministic) :-
		anomaly_test_support::validate_anomaly_dataset(mixed_anomalies).

	test(mixed_distance_behaviors_validation, deterministic) :-
		anomaly_test_support::validate_anomaly_dataset(mixed_distance_behaviors).

	test(malformed_anomalies_validation, error(domain_error(class_values, [normal, alert]))) :-
		anomaly_test_support::validate_anomaly_dataset(malformed_anomalies).

	% Sample anomaly detector smoke tests.

	test(sample_anomaly_detector_learn_2, deterministic(ground(Detector))) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector).

	test(sample_anomaly_detector_valid_anomaly_detector_1, deterministic(sample_anomaly_detector::valid_anomaly_detector(Detector))) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector).

	test(sample_anomaly_detector_invalid_anomaly_detector_1, error(domain_error(anomaly_detector, sample_anomaly_detector(gaussian_anomalies, [x, 1], 5.3, [anomaly_threshold(0.5)])))) :-
		sample_anomaly_detector::check_anomaly_detector(sample_anomaly_detector(gaussian_anomalies, [x, 1], 5.3, [anomaly_threshold(0.5)])).

	test(sample_anomaly_detector_diagnostics_2, deterministic((memberchk(model(sample_anomaly_detector), Diagnostics), memberchk(training_dataset(gaussian_anomalies), Diagnostics), memberchk(attribute_names([x, y]), Diagnostics), memberchk(options([anomaly_threshold(0.5)]), Diagnostics)))) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::diagnostics(Detector, Diagnostics).

	test(sample_anomaly_detector_options_2, deterministic(Options == [anomaly_threshold(0.5)])) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::anomaly_detector_options(Detector, Options).

	test(sample_anomaly_detector_diagnostic_2, deterministic(Diagnostics == Enumerated)) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::diagnostics(Detector, Diagnostics),
		findall(Diagnostic, sample_anomaly_detector::diagnostic(Detector, Diagnostic), Enumerated).

	test(sample_anomaly_detector_predict_3_normal, deterministic(Prediction == normal)) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::predict(Detector, [x-0.12, y-0.34], Prediction).

	test(sample_anomaly_detector_predict_3_anomaly, deterministic(Prediction == anomaly)) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::predict(Detector, [x-4.50, y-4.20], Prediction).

	test(sample_anomaly_detector_predict_4_threshold_override, deterministic(Prediction == anomaly)) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector, [anomaly_threshold(0.99)]),
		sample_anomaly_detector::predict(Detector, [x-4.50, y-4.20], Prediction, [anomaly_threshold(0.5)]).

	test(sample_anomaly_detector_score_all_3, deterministic((length(AllScores, 48), FirstScore >= SecondScore))) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::score_all(gaussian_anomalies, Detector, AllScores),
		AllScores = [_-_-FirstScore, _-_-SecondScore| _].

	test(sample_export_to_clauses_4, deterministic(Clause == detector(sample_anomaly_detector(gaussian_anomalies, [x, y], 5.3, [anomaly_threshold(0.5)])))) :-
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::export_to_clauses(gaussian_anomalies, Detector, detector, [Clause]).

	test(sample_export_to_file_4_header, deterministic(HeaderLines == ['% exported anomaly detector predicate: detector/1', '% training dataset: gaussian_anomalies', '% options: [anomaly_threshold(0.5)]', '% detector(Detector)'])) :-
		^^file_path('test_output.pl', File),
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::export_to_file(gaussian_anomalies, Detector, detector, File),
		header_lines(File, HeaderLines).

	test(sample_export_to_file_4_loadable, deterministic((LoadedDetector == sample_anomaly_detector(gaussian_anomalies, [x, y], 5.3, [anomaly_threshold(0.5)]), Prediction == anomaly))) :-
		^^file_path('test_output.pl', File),
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::export_to_file(gaussian_anomalies, Detector, detector, File),
		logtalk_load(File),
		{detector(LoadedDetector)},
		sample_anomaly_detector::predict(LoadedDetector, [x-4.50, y-4.20], Prediction).

	test(sample_anomaly_detector_print_1, deterministic) :-
		^^suppress_text_output,
		sample_anomaly_detector::learn(gaussian_anomalies, Detector),
		sample_anomaly_detector::print_anomaly_detector(Detector).

	% auxiliary predicates

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_line_atom(Stream, Line1),
		read_line_atom(Stream, Line2),
		read_line_atom(Stream, Line3),
		read_line_atom(Stream, Line4),
		close(Stream),
		Lines = [Line1, Line2, Line3, Line4].

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.
