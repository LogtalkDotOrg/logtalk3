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
		(	ClassValues == [normal, anomaly] ->
			true
		;	domain_error(class_values, ClassValues)
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
		(	memberchk(Attribute-Value, AttributeValues) ->
			true
		;	existence_error(attribute, Attribute)
		),
		(	var(Value) ->
			true
		;	Values == continuous ->
			number(Value)
		;	memberchk(Value, Values)
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
		(	Score0 > 1.0 ->
			Score = 1.0
		;	Score = Score0
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
		(	Scale > 0.0 ->
			Score0 is Maximum / Scale
		;	Score0 = 0.0
		),
		(	Score0 > 1.0 ->
			Score = 1.0
		;	Score = Score0
		).

	max_or_zero([], 0.0).
	max_or_zero([Value| Values], Maximum) :-
		max_or_zero(Values, Value, Maximum).

	max_or_one([], 1.0).
	max_or_one([Value| Values], Maximum) :-
		max_or_zero(Values, Value, Maximum).

	max_or_zero([], Maximum, Maximum).
	max_or_zero([Value| Values], Maximum0, Maximum) :-
		(	Value > Maximum0 ->
			Maximum1 = Value
		;	Maximum1 = Maximum0
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

	check_anomaly_detector(Detector) :-
		(	Detector = sample_anomaly_detector(Dataset, AttributeNames, Scale, Options),
			valid(object_identifier, Dataset),
			valid(list(atom), AttributeNames),
			AttributeNames \== [],
			number(Scale),
			Scale > 0.0,
			valid(list(compound), Options),
			catch(^^check_options(Options), _Error, fail) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(sample_anomaly_detector(Dataset, AttributeNames, Scale, Options), [
		model(sample_anomaly_detector),
		training_dataset(Dataset),
		attribute_names(AttributeNames),
		score_scale(Scale),
		options(Options)
	]).

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
