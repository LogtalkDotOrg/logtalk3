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


:- object(sample_classifier,
	imports(classifier_common)).

	:- public(validate_attributes/1).
	:- mode(validate_attributes(+object_identifier), one_or_error).
	:- info(validate_attributes/1, [
		comment is 'Testing wrapper for the shared dataset attribute validation helper.',
		argnames is ['Dataset']
	]).

	:- public(validate_examples/1).
	:- mode(validate_examples(+object_identifier), one_or_error).
	:- info(validate_examples/1, [
		comment is 'Testing wrapper for the shared example validation helper that allows missing attribute bindings.',
		argnames is ['Dataset']
	]).

	:- public(validate_complete_examples/1).
	:- mode(validate_complete_examples(+object_identifier), one_or_error).
	:- info(validate_complete_examples/1, [
		comment is 'Testing wrapper for the shared complete-example validation helper that allows missing values represented by variables.',
		argnames is ['Dataset']
	]).

	:- public(validate_complete_examples_nonvar/1).
	:- mode(validate_complete_examples_nonvar(+object_identifier), one_or_error).
	:- info(validate_complete_examples_nonvar/1, [
		comment is 'Testing wrapper for the shared complete-example validation helper that requires all attribute values to be instantiated.',
		argnames is ['Dataset']
	]).

	:- public(mixed_feature_distance/5).
	:- mode(mixed_feature_distance(+term, +list, +list, +list, -float), one_or_error).
	:- info(mixed_feature_distance/5, [
		comment is 'Testing wrapper for the shared mixed-feature distance helper.',
		argnames is ['Metric', 'FeatureTypes', 'Values1', 'Values2', 'Distance']
	]).

	:- uses(list, [
		memberchk/2
	]).

	learn(Dataset, sample_classifier(DefaultClass, Diagnostics)) :-
		^^dataset_attributes(Dataset, _),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		Dataset::class_values([DefaultClass| _]),
		Diagnostics = [
			model(sample_classifier),
			options([]),
			training_dataset(Dataset)
		].

	predict(sample_classifier(DefaultClass, _Diagnostics), _Instance, DefaultClass).

	check_classifier(Classifier) :-
		(	Classifier = sample_classifier(DefaultClass, Diagnostics),
			atom(DefaultClass),
			^^valid_classifier_metadata(sample_classifier, Diagnostics),
			memberchk(training_dataset(_Dataset), Diagnostics),
			memberchk(options([]), Diagnostics) ->
			true
		;	domain_error(classifier, Classifier)
		).

	classifier_diagnostics_data(sample_classifier(_DefaultClass, Diagnostics), Diagnostics).

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(sample_classifier(_DefaultClass, _Diagnostics), sample_classifier('DefaultClass', 'Diagnostics')).

	validate_attributes(Dataset) :-
		^^dataset_attributes(Dataset, _).

	validate_examples(Dataset) :-
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples).

	validate_complete_examples(Dataset) :-
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples).

	validate_complete_examples_nonvar(Dataset) :-
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples_nonvar(Dataset, Examples).

	mixed_feature_distance(Metric, FeatureTypes, Values1, Values2, Distance) :-
		^^mixed_feature_distance(Metric, FeatureTypes, Values1, Values2, Distance).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	print_classifier(Classifier) :-
		^^print_classifier_template(Classifier),
		writeq(Classifier), nl.

:- end_object.


:- object(duplicate_attribute_declarations,
	implements(dataset_protocol)).

	attribute_values(outlook, [sunny, rainy]).
	attribute_values(outlook, [overcast]).

	class(play).

	class_values([yes, no]).

	example(1, yes, [outlook-sunny]).

:- end_object.


:- object(invalid_class_dataset,
	implements(dataset_protocol)).

	attribute_values(age, continuous).

	class(label).

	class_values([yes, no]).

	example(1, maybe, [age-30]).

:- end_object.


:- object(undeclared_attribute_dataset,
	implements(dataset_protocol)).

	attribute_values(age, continuous).

	class(label).

	class_values([yes, no]).

	example(1, yes, [age-30, income-50000]).

:- end_object.


:- object(invalid_continuous_value_dataset,
	implements(dataset_protocol)).

	attribute_values(age, continuous).

	class(label).

	class_values([yes, no]).

	example(1, yes, [age-young]).

:- end_object.


:- object(invalid_categorical_value_dataset,
	implements(dataset_protocol)).

	attribute_values(student, [yes, no]).

	class(label).

	class_values([yes, no]).

	example(1, yes, [student-maybe]).

:- end_object.


:- object(duplicate_example_attribute_dataset,
	implements(dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(student, [yes, no]).

	class(label).

	class_values([yes, no]).

	example(1, yes, [age-30, age-31, student-yes]).

:- end_object.


:- object(incomplete_example_dataset,
	implements(dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(student, [yes, no]).

	class(label).

	class_values([yes, no]).

	example(1, yes, [age-30]).

:- end_object.
