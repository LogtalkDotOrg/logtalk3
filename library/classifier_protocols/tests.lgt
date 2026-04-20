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

	learn(Dataset, sample_classifier(DefaultClass, Diagnostics)) :-
		Dataset::class_values([DefaultClass| _]),
		Diagnostics = [
			model(sample_classifier),
			options([]),
			training_dataset(Dataset)
		].

	predict(sample_classifier(DefaultClass, _Diagnostics), _Instance, DefaultClass).

	classifier_diagnostics_data(sample_classifier(_DefaultClass, Diagnostics), Diagnostics).

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(sample_classifier(_DefaultClass, _Diagnostics), sample_classifier('DefaultClass', 'Diagnostics')).

	classifier_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	print_classifier(Classifier) :-
		^^print_classifier_template(Classifier),
		writeq(Classifier), nl.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Smoke tests for the "classifier_protocols" library.'
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

	test(sample_classifier_predict_3, deterministic(Class == yes)) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::predict(Classifier, [outlook-sunny, temperature-hot, humidity-high, wind-weak], Class).

	test(sample_classifier_diagnostics_2, deterministic(memberchk(model(sample_classifier), Diagnostics))) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::diagnostics(Classifier, Diagnostics).

	test(sample_classifier_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::diagnostics(Classifier, Diagnostics),
		findall(Diagnostic, sample_classifier::diagnostic(Classifier, Diagnostic), Enumerated).

	test(sample_classifier_classifier_options_2, deterministic(Options == [])) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::classifier_options(Classifier, Options).

	test(sample_classifier_classifier_to_clauses_4, deterministic(Clause == classifier(sample_classifier(yes, [model(sample_classifier), options([]), training_dataset(play_tennis)])))) :-
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::classifier_to_clauses(play_tennis, Classifier, classifier, [Clause]).

	test(sample_classifier_classifier_to_file_4_header, deterministic(HeaderLines == ['% exported classifier predicate: classifier/1', '% training dataset: play_tennis', '% dataset prediction schema: classifier(Outlook,Temperature,Humidity,Wind,Play_tennis)', '% diagnostics: [model(sample_classifier),options([]),training_dataset(play_tennis)]', '% classifier(Classifier)'])) :-
		^^file_path('test_output.pl', File),
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::classifier_to_file(play_tennis, Classifier, classifier, File),
		header_lines(File, HeaderLines).

	test(sample_classifier_classifier_to_file_4_loadable, deterministic((DefaultClass == yes, memberchk(model(sample_classifier), Diagnostics)))) :-
		^^file_path('test_output.pl', File),
		sample_classifier::learn(play_tennis, Classifier),
		sample_classifier::classifier_to_file(play_tennis, Classifier, classifier, File),
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
		(   Code == -1 ->
			Line = end_of_file
		;   read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(   NextCode == 10 ->
			Codes = []
		;   read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.
