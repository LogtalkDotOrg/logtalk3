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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2018/02/13,
		comment is 'Unit tests for the de facto Prolog standard format/3 built-in predicate.'
	]).

	test(lgt_format_3_empty_control_sequence, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '', [])},
		^^text_output_assertion('', Assertion).

	test(lgt_format_3_empty_arguments, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, 'abc', [])},
		^^text_output_assertion('abc', Assertion).

	test(lgt_format_3_tilde, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~~', [])},
		^^text_output_assertion('~', Assertion).

	test(lgt_format_3_write, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~w', ['ABC'])},
		^^text_output_assertion('ABC', Assertion).

	test(lgt_format_3_quoted, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~q', ['ABC'])},
		^^text_output_assertion('\'ABC\'', Assertion).

	test(lgt_format_3_canonical, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~k', [(:-a)])},
		^^text_output_assertion(':-(a)', Assertion).

	test(lgt_format_3_atom, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a', [abc])},
		^^text_output_assertion(abc, Assertion).

	test(lgt_format_3_code, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~c', [65])},
		^^text_output_assertion('A', Assertion).

	test(lgt_format_3_code_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~8c', [65])},
		^^text_output_assertion('AAAAAAAA', Assertion).

	test(lgt_format_3_string, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~s', ["ABC"])},
		^^text_output_assertion('ABC', Assertion).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi)
	)).

	test(lgt_format_3_new_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~n', [])},
		^^text_output_assertion('\r\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4n', [])},
		^^text_output_assertion('\r\n\r\n\r\n\r\n', Assertion).

	:- else.

	test(lgt_format_3_new_line, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~n', [])},
		^^text_output_assertion('\n', Assertion).

	test(lgt_format_3_new_line_n, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4n', [])},
		^^text_output_assertion('\n\n\n\n', Assertion).

	:- endif.

	- test(lgt_format_3_tab, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~t~a~8|', [abc])},
		^^text_output_assertion('     abc', Assertion).

	test(lgt_format_3_ignore, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~a~i~a', [a,b,c])},
		^^text_output_assertion(ac, Assertion).

	test(lgt_format_3_decimal, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~d', [123])},
		^^text_output_assertion('123', Assertion).

	test(lgt_format_3_float, true(Assertion)) :-
		^^set_text_output(''),
		current_output(S),
		{format(S, '~4f', [-1.0e-1])},
		^^text_output_assertion('-0.1000', Assertion).

	cleanup :-
		^^clean_text_input.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, xsb)).
	% workaround XSB atom-based module system
	:- import(from(/(format,3), format)).
:- endif.
