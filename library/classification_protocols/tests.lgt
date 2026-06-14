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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Smoke tests for the "classification_protocols" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	test(play_tennis_attributes, deterministic(Attributes == [outlook, temperature, humidity, wind])) :-
		findall(Attribute, play_tennis::attribute_values(Attribute, _), Attributes).

	test(play_tennis_class, deterministic(Class == play_tennis)) :-
		play_tennis::class(Class).

	test(contact_lenses_class_values, deterministic(Values == [hard, soft, none])) :-
		contact_lenses::class_values(Values).

	test(sample_classifier_learn_2, deterministic(ground(Classifier))) :-
		sample_classifier::learn(play_tennis, Classifier).

	test(sample_classifier_validate_examples_1, deterministic) :-
		sample_classifier::validate_examples(mixed).

	test(sample_classifier_validate_examples_missing_values_1, deterministic) :-
		sample_classifier::validate_examples(missing_mixed).

	test(sample_classifier_validate_complete_examples_1, deterministic) :-
		sample_classifier::validate_complete_examples(mixed).

	test(sample_classifier_validate_complete_examples_missing_values_1, deterministic) :-
		sample_classifier::validate_complete_examples(missing_mixed).

	test(sample_classifier_validate_complete_examples_nonvar_1, error(instantiation_error)) :-
		sample_classifier::validate_complete_examples_nonvar(missing_mixed).

	test(sample_classifier_validate_attributes_duplicate_declaration_1, error(domain_error(attribute_declarations, outlook))) :-
		sample_classifier::validate_attributes(duplicate_attribute_declarations).

	test(sample_classifier_validate_examples_invalid_class_1, error(existence_error(class_value, maybe))) :-
		sample_classifier::validate_examples(invalid_class_dataset).

	test(sample_classifier_validate_examples_undeclared_attribute_1, error(domain_error(declared_attribute([age]), income))) :-
		sample_classifier::validate_examples(undeclared_attribute_dataset).

	test(sample_classifier_validate_examples_invalid_continuous_value_1, error(type_error(number, young))) :-
		sample_classifier::validate_examples(invalid_continuous_value_dataset).

	test(sample_classifier_validate_examples_invalid_categorical_value_1, error(domain_error(attribute_value(student, [yes, no]), maybe))) :-
		sample_classifier::validate_examples(invalid_categorical_value_dataset).

	test(sample_classifier_validate_examples_duplicate_attribute_1, error(domain_error(attribute_occurrences(age, 1), 2))) :-
		sample_classifier::validate_examples(duplicate_example_attribute_dataset).

	test(sample_classifier_validate_complete_examples_missing_attribute_1, error(existence_error(attribute, student))) :-
		sample_classifier::validate_complete_examples(incomplete_example_dataset).

	test(sample_classifier_predict_3, deterministic(Class == yes)) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(sample_classifier_diagnostics_2, deterministic(memberchk(model(sample_classifier), Diagnostics))) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::diagnostics(Classifier, Diagnostics).

	test(sample_classifier_valid_classifier_1, deterministic(sample_classifier::valid_classifier(Classifier))) :-
		sample_classifier::learn(play_tennis, Classifier).

	test(sample_classifier_invalid_classifier_1, fail) :-
		sample_classifier::valid_classifier(sample_classifier(1, [model(sample_classifier), options([]), training_dataset(play_tennis)])).

	test(sample_classifier_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::diagnostics(Classifier, Diagnostics),
		findall(Diagnostic, sample_classifier::diagnostic(Classifier, Diagnostic), Enumerated).

	test(sample_classifier_classifier_options_2, deterministic(Options == [])) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::classifier_options(Classifier, Options).

	test(sample_classifier_mixed_feature_distance_euclidean_5, deterministic(Distance =:= sqrt(6.0))) :-
		sample_classifier::mixed_feature_distance(euclidean, [numeric, categorical, numeric], [1.0, red, 2.0], [3.0, blue, 3.0], Distance).

	test(sample_classifier_mixed_feature_distance_manhattan_5, deterministic(Distance =:= 4.0)) :-
		sample_classifier::mixed_feature_distance(manhattan, [numeric, categorical, numeric], [1.0, red, 2.0], [3.0, blue, 3.0], Distance).

	test(sample_classifier_mixed_feature_distance_chebyshev_5, deterministic(Distance =:= 2.0)) :-
		sample_classifier::mixed_feature_distance(chebyshev, [numeric, categorical, numeric], [1.0, red, 2.0], [3.0, blue, 3.0], Distance).

	test(sample_classifier_mixed_feature_distance_minkowski_5, deterministic(Distance =:= 10.0 ** (1.0 / 3.0))) :-
		sample_classifier::mixed_feature_distance(minkowski(3.0), [numeric, categorical, numeric], [1.0, red, 2.0], [3.0, blue, 3.0], Distance).

	test(sample_classifier_mixed_feature_distance_cosine_5, deterministic(abs(Distance - (1.0 - 1.0 / sqrt(5.0))) < 1.0e-12)) :-
		sample_classifier::mixed_feature_distance(cosine, [numeric, categorical, numeric], [1.0, red, 2.0], [2.0, blue, 0.0], Distance).

	test(sample_classifier_mixed_feature_distance_cosine_zero_numeric_5, deterministic(Distance =:= 1.0)) :-
		sample_classifier::mixed_feature_distance(cosine, [categorical, categorical], [red, square], [blue, circle], Distance).

	test(sample_classifier_export_to_clauses_4, deterministic(Clause == classifier(sample_classifier(yes, [model(sample_classifier), options([]), training_dataset(play_tennis)])))) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::export_to_clauses(play_tennis, Classifier, classifier, [Clause]).

	test(sample_classifier_export_to_file_4_header, deterministic(HeaderLines == ['% exported classifier predicate: classifier/1', '% training dataset: play_tennis', '% dataset prediction schema: classifier(Outlook,Temperature,Humidity,Wind,Play_tennis)', '% diagnostics: [model(sample_classifier),options([]),training_dataset(play_tennis)]', '% classifier(Classifier)'])) :-
		^^file_path('test_output.pl', File),
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::export_to_file(play_tennis, Classifier, classifier, File),
		header_lines(File, HeaderLines).

	test(sample_classifier_export_to_file_4_loadable, deterministic((DefaultClass == yes, memberchk(model(sample_classifier), Diagnostics)))) :-
		^^file_path('test_output.pl', File),
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::export_to_file(play_tennis, Classifier, classifier, File),
		logtalk_load(File),
		{classifier(LoadedClassifier)},
		LoadedClassifier = sample_classifier(DefaultClass, Diagnostics).

	test(sample_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::print_classifier(Classifier).

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_line_atom(Stream, Line1),
		read_line_atom(Stream, Line2),
		read_line_atom(Stream, Line3),
		read_line_atom(Stream, Line4),
		read_line_atom(Stream, Line5),
		close(Stream),
		Lines = [Line1, Line2, Line3, Line4, Line5].

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
