%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- set_prolog_flag(double_quotes, codes).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2018/01/20,
		comment is 'Unit tests for the de facto Prolog standard format/2 built-in predicate.'
	]).

	succeeds(lgt_format_2_empty_control_sequence) :-
		^^set_text_output(''),
		{format('', [])},
		^^check_text_output('').

	succeeds(lgt_format_2_empty_arguments) :-
		^^set_text_output(''),
		{format('abc', [])},
		^^check_text_output('abc').

	succeeds(lgt_format_2_tilde) :-
		^^set_text_output(''),
		{format('~~', [])},
		^^check_text_output('~').

	succeeds(lgt_format_2_write) :-
		^^set_text_output(''),
		{format('~w', ['ABC'])},
		^^check_text_output('ABC').

	succeeds(lgt_format_2_quoted) :-
		^^set_text_output(''),
		{format('~q', ['ABC'])},
		^^check_text_output('\'ABC\'').

	succeeds(lgt_format_2_canonical) :-
		^^set_text_output(''),
		{format('~k', [(:-a)])},
		^^check_text_output(':-(a)').

	succeeds(lgt_format_2_atom) :-
		^^set_text_output(''),
		{format('~a', [abc])},
		^^check_text_output(abc).

	succeeds(lgt_format_2_code) :-
		^^set_text_output(''),
		{format('~c', [65])},
		^^check_text_output('A').

	succeeds(lgt_format_2_code_n) :-
		^^set_text_output(''),
		{format('~8c', [65])},
		^^check_text_output('AAAAAAAA').

	succeeds(lgt_format_2_string) :-
		^^set_text_output(''),
		{format('~s', ["ABC"])},
		^^check_text_output('ABC').

	succeeds(lgt_format_2_new_line) :-
		^^set_text_output(''),
		{format('~n', [])},
		^^check_text_output('\n').

	succeeds(lgt_format_2_new_line_n) :-
		^^set_text_output(''),
		{format('~4n', [])},
		^^check_text_output('\n\n\n\n').

	- succeeds(lgt_format_2_tab) :-
		^^set_text_output(''),
		{format('~t~a~8|', [abc])},
		^^check_text_output('     abc').

	succeeds(lgt_format_2_ignore) :-
		^^set_text_output(''),
		{format('~a~i~a', [a,b,c])},
		^^check_text_output(ac).

	succeeds(lgt_format_2_decimal) :-
		^^set_text_output(''),
		{format('~d', [123])},
		^^check_text_output('123').

	succeeds(lgt_format_2_float) :-
		^^set_text_output(''),
		{format('~4f', [-1.0e-1])},
		^^check_text_output('-0.1000').

	cleanup :-
		^^clean_text_input.

:- end_object.
