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


:- object(sample_regressor,
	imports([options, regressor_common])).

	learn(Dataset, sample_regressor(TargetName, Attributes, Target, Options), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(TargetName),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		Examples = [example(_Id, Target, _AttributeValues)| _].

	predict(sample_regressor(_TargetName, _Attributes, Target, _Options), _Instance, Target).

	check_regressor(Regressor) :-
		(   Regressor = sample_regressor(TargetName, Attributes, Target, Options),
			atom(TargetName),
			^^valid_attribute_declarations(Attributes),
			number(Target),
			^^valid_regressor_options(Options) ->
			true
		;   domain_error(valid_regressor, Regressor)
		).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Regressor'].

	regressor_term_template(sample_regressor(_TargetName, _Attributes, _Target, _Options), sample_regressor('TargetName', 'Attributes', 'Target', 'Options')).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Clause =.. [Functor, Regressor].

	print_regressor(Regressor) :-
		^^print_regressor_template(Regressor),
		writeq(Regressor), nl.

	default_option(sample_option(enabled)).

	valid_option(sample_option(Value)) :-
		once((Value == enabled; Value == disabled)).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Smoke tests for the "regression_protocols" library.'
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	test(simple_line_attribute_values, deterministic(Attributes == [x-continuous])) :-
		findall(Attribute-Values, simple_line::attribute_values(Attribute, Values), Attributes).

	test(plane_target, deterministic(Target == z)) :-
		plane::target(Target).

	test(sample_regressor_learn_2, deterministic(ground(Regressor))) :-
		sample_regressor::learn(simple_line, Regressor).

	test(sample_regressor_learn_3, deterministic(ground(Regressor))) :-
		sample_regressor::learn(simple_line, Regressor, [sample_option(enabled)]).

	test(sample_regressor_valid_regressor_1, deterministic(sample_regressor::valid_regressor(Regressor))) :-
		sample_regressor::learn(simple_line, Regressor).

	test(sample_regressor_invalid_regressor_1, fail) :-
		sample_regressor::valid_regressor(sample_regressor(y, [x], bad, [sample_option(enabled)])).

	test(sample_regressor_predict_3, deterministic(Prediction == 3)) :-
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::predict(Regressor, [x-6], Prediction).

	test(sample_regressor_export_to_clauses_4, deterministic(Clause == regress(sample_regressor(y, [x-continuous], 3, [sample_option(enabled)])))) :-
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::export_to_clauses(simple_line, Regressor, regress, [Clause]).

	test(sample_regressor_export_to_file_4_header, deterministic(HeaderLine == '% exported regressor predicate: regress/1')) :-
		^^file_path('test_output.pl', File),
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::export_to_file(simple_line, Regressor, regress, File),
		first_header_line(File, HeaderLine).

	test(sample_regressor_export_to_file_4_metadata, deterministic(HeaderLines == ['% exported regressor predicate: regress/1', '% training dataset: simple_line', '% target: y', '% attributes: [x-continuous]', '% options: [sample_option(enabled)]', '% regress(Regressor)'])) :-
		^^file_path('test_output.pl', File),
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::export_to_file(simple_line, Regressor, regress, File),
		header_lines(File, HeaderLines).

	test(sample_regressor_export_to_file_4_loadable, deterministic(LoadedRegressor == sample_regressor(y, [x-continuous], 3, [sample_option(enabled)]))) :-
		^^file_path('test_output.pl', File),
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::export_to_file(simple_line, Regressor, regress, File),
		logtalk_load(File),
		{regress(LoadedRegressor)}.

	test(sample_regressor_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		sample_regressor::learn(simple_line, Regressor),
		sample_regressor::print_regressor(Regressor).

	test(sample_regressor_learn_2_invalid_target, error(type_error(number, bad))) :-
		sample_regressor::learn(invalid_target, _Regressor).

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_header_lines(Stream, Lines),
		close(Stream).

	first_header_line(File, Line) :-
		header_lines(File, [Line| _]).

	read_header_lines(Stream, Lines) :-
		read_line_atom(Stream, Line),
		(   Line == end_of_file ->
			Lines = []
		;   sub_atom(Line, 0, 1, _, '%') ->
			Lines = [Line| RestLines],
			read_header_lines(Stream, RestLines)
		;   Lines = []
		).

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
