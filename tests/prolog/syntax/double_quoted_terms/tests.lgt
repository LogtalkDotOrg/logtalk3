%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-11-08,
		comment is 'Unit tests for the ISO Prolog standard double quoted term syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.7.1

	test(iso_double_quoted_term_01, true) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_02, true) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_03, true) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_04, true) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_05, true) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_06, true) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	test(iso_double_quoted_term_07, true(atom_chars('jim', T))) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, chars),
		{read(T)}.

	test(iso_double_quoted_term_08, true(atom_codes('jim', T))) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, codes),
		{read(T)}.

	test(iso_double_quoted_term_09, true(T == 'jim')) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, atom),
		{read(T)}.

	test(iso_double_quoted_term_10, true(T == [])) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, chars),
		{read(T)}.

	test(iso_double_quoted_term_11, true(T == [])) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, codes),
		{read(T)}.

	test(iso_double_quoted_term_12, true(T == '')) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, atom),
		{read(T)}.

	cleanup :-
		^^clean_text_input.

	double_quotes_example_1([
		'(      current_prolog_flag(double_quotes, chars), atom_chars(\'jim\', "jim")',
		';      current_prolog_flag(double_quotes, codes), atom_codes(\'jim\', "jim")',
		';      current_prolog_flag(double_quotes, atom), \'jim\' == "jim"',
		'). '
	]).

	double_quotes_example_2([
		'(      current_prolog_flag(double_quotes, chars), [] == ""',
		';      current_prolog_flag(double_quotes, codes), [] == ""',
		';      current_prolog_flag(double_quotes, atom), \'\' == ""',
		'). '
	]).

:- end_object.
