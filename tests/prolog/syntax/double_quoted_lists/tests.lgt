%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/11/07,
		comment is 'Unit tests for the ISO Prolog standard double quoted list syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.7.1

	succeeds(iso_double_quoted_list_01) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_02) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_03) :-
		double_quotes_example_1(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_04) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, chars),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_05) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, codes),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_06) :-
		double_quotes_example_2(Contents),
		^^set_text_input(Contents),
		set_prolog_flag(double_quotes, atom),
		{read(Goal), call(Goal)}.

	succeeds(iso_double_quoted_list_07) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, chars),
		{read(T)},
		atom_chars('jim', T).

	succeeds(iso_double_quoted_list_08) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, codes),
		{read(T)},
		atom_codes('jim', T).

	succeeds(iso_double_quoted_list_09) :-
		^^set_text_input('"jim". '),
		set_prolog_flag(double_quotes, atom),
		{read(T)},
		T == 'jim'.

	succeeds(iso_double_quoted_list_10) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, chars),
		{read(T)},
		T == [].

	succeeds(iso_double_quoted_list_11) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, codes),
		{read(T)},
		T == [].

	succeeds(iso_double_quoted_list_12) :-
		^^set_text_input('"". '),
		set_prolog_flag(double_quotes, atom),
		{read(T)},
		T == ''.

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
