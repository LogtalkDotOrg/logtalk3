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


:- object(anomaly_test_support).

	:- public(validate_anomaly_dataset/1).
	:- public(training_model/3).
	:- public(instance_score/2).
	:- public(instance_score/4).
	:- public(sorted_scores/2).
	:- public(sorted_scores/4).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, reverse/2
	]).

	validate_anomaly_dataset(Dataset) :-
		findall(Attribute-Values, Dataset::attribute_values(Attribute, Values), Attributes),
		Attributes \== [],
		Dataset::class_values(ClassValues),
		(   ClassValues == [normal, anomaly] ->
			true
		;   domain_error(class_values, ClassValues)
		),
		forall(
			Dataset::example(_Id, Class, AttributeValues),
			validate_example(Attributes, Class, AttributeValues)
		).

	validate_example(Attributes, Class, AttributeValues) :-
		memberchk(Class, [normal, anomaly]),
		forall(
			member(Attribute-Values, Attributes),
			validate_attribute_value(Attribute, Values, AttributeValues)
		).

	validate_attribute_value(Attribute, Values, AttributeValues) :-
		(   memberchk(Attribute-Value, AttributeValues) ->
			true
		;   existence_error(attribute, Attribute)
		),
		(   var(Value) ->
			true
		;   Values == continuous ->
			number(Value)
		;   memberchk(Value, Values)
		).

	training_model(Dataset, AttributeNames, Scale) :-
		findall(Attribute, Dataset::attribute_values(Attribute, _), AttributeNames),
		findall(
			Absolute,
			(
				Dataset::example(_Id, _Class, AttributeValues),
				member(Attribute, AttributeNames),
				memberchk(Attribute-Value, AttributeValues),
				nonvar(Value),
				number(Value),
				Absolute is abs(Value)
			),
			AbsoluteValues
		),
		max_or_one(AbsoluteValues, Scale).

	instance_score(Instance, Score) :-
		findall(
			Absolute,
			(
				member(_-Value, Instance),
				nonvar(Value),
				number(Value),
				Absolute is abs(Value)
			),
			AbsoluteValues
		),
		max_or_zero(AbsoluteValues, Maximum),
		Score0 is Maximum / 5.0,
		(   Score0 > 1.0 ->
			Score = 1.0
		;   Score = Score0
		).

	instance_score(AttributeNames, Scale, Instance, Score) :-
		findall(
			Absolute,
			(
				member(Attribute, AttributeNames),
				memberchk(Attribute-Value, Instance),
				nonvar(Value),
				number(Value),
				Absolute is abs(Value)
			),
			AbsoluteValues
		),
		max_or_zero(AbsoluteValues, Maximum),
		(   Scale > 0.0 ->
			Score0 is Maximum / Scale
		;   Score0 = 0.0
		),
		(   Score0 > 1.0 ->
			Score = 1.0
		;   Score = Score0
		).

	max_or_zero([], 0.0).
	max_or_zero([Value| Values], Maximum) :-
		max_or_zero(Values, Value, Maximum).

	max_or_one([], 1.0).
	max_or_one([Value| Values], Maximum) :-
		max_or_zero(Values, Value, Maximum).

	max_or_zero([], Maximum, Maximum).
	max_or_zero([Value| Values], Maximum0, Maximum) :-
		(   Value > Maximum0 ->
			Maximum1 = Value
		;   Maximum1 = Maximum0
		),
		max_or_zero(Values, Maximum1, Maximum).

	sorted_scores(Dataset, Scores) :-
		findall(
			Score-Id-Class,
			(
				Dataset::example(Id, Class, AttributeValues),
				instance_score(AttributeValues, Score)
			),
			Pairs
		),
		msort(Pairs, Ascending),
		reverse(Ascending, Descending),
		extract_scores(Descending, Scores).

	sorted_scores(Dataset, AttributeNames, Scale, Scores) :-
		findall(
			Score-Id-Class,
			(
				Dataset::example(Id, Class, AttributeValues),
				instance_score(AttributeNames, Scale, AttributeValues, Score)
			),
			Pairs
		),
		msort(Pairs, Ascending),
		reverse(Ascending, Descending),
		extract_scores(Descending, Scores).

	extract_scores([], []).
	extract_scores([Score-Id-Class| Pairs], [Id-Class-Score| Scores]) :-
		extract_scores(Pairs, Scores).

:- end_object.


:- object(sample_anomaly_detector,
	imports(anomaly_detector_common)).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, sample_anomaly_detector(Dataset, AttributeNames, Scale, Options), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		anomaly_test_support::training_model(Dataset, AttributeNames, Scale).

	score(sample_anomaly_detector(_Dataset, AttributeNames, Scale, _Options), Instance, Score) :-
		anomaly_test_support::instance_score(AttributeNames, Scale, Instance, Score).

	score_all(Dataset, sample_anomaly_detector(_TrainingDataset, AttributeNames, Scale, _Options), Scores) :-
		anomaly_test_support::sorted_scores(Dataset, AttributeNames, Scale, Scores).

	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		writeq(Detector), nl.

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(sample_anomaly_detector(_Dataset, _AttributeNames, _Scale, _Options), sample_anomaly_detector('Dataset', 'AttributeNames', 'Scale', 'Options')).

	default_option(anomaly_threshold(0.5)).

	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Smoke tests for the "anomaly_detection_protocols" library.'
	]).

	:- uses(list, [
		length/2
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
