%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
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
		comment is 'Unit tests for the de facto Prolog standard format/3 built-in predicate.'
	]).

	succeeds(lgt_format_3_empty_control_sequence) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '', [])},
		^^check_text_output('').

	succeeds(lgt_format_3_empty_arguments) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, 'abc', [])},
		^^check_text_output('abc').

	succeeds(lgt_format_3_tilde) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~~', [])},
		^^check_text_output('~').

	succeeds(lgt_format_3_write) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~w', ['ABC'])},
		^^check_text_output('ABC').

	succeeds(lgt_format_3_quoted) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~q', ['ABC'])},
		^^check_text_output('\'ABC\'').

	succeeds(lgt_format_3_canonical) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~k', [(:-a)])},
		^^check_text_output(':-(a)').

	succeeds(lgt_format_3_atom) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', [abc])},
		^^check_text_output(abc).

	succeeds(lgt_format_3_code) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~c', [65])},
		^^check_text_output('A').

	succeeds(lgt_format_3_code_n) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~8c', [65])},
		^^check_text_output('AAAAAAAA').

	succeeds(lgt_format_3_string) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~s', ["ABC"])},
		^^check_text_output('ABC').

	succeeds(lgt_format_3_new_line) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~n', [])},
		^^check_text_output('\n').

	succeeds(lgt_format_3_new_line_n) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4n', [])},
		^^check_text_output('\n\n\n\n').

	- succeeds(lgt_format_3_tab) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~a~8|', [abc])},
		^^check_text_output('     abc').

	succeeds(lgt_format_3_ignore) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a~i~a', [a,b,c])},
		^^check_text_output(ac).

	succeeds(lgt_format_3_decimal) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', [123])},
		^^check_text_output('123').

	succeeds(lgt_format_3_float) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4f', [-1.0e-1])},
		^^check_text_output('-0.1000').

	cleanup :-
		^^clean_text_input.

:- end_object.
