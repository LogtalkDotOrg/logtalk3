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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-12-03,
		comment is 'Unit tests for the ISO Prolog standard curly bracketed term syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.6.1

	test(iso_curly_bracketed_term_01, true(T == '{}'(a))) :-
		^^set_text_input('{a}. '),
		{read(T)}.

	test(iso_curly_bracketed_term_02, true(T == '{}'(','(a,b)))) :-
		^^set_text_input('{a,b}. '),
		{read(T)}.

	% tests from the Logtalk portability work

	test(lgt_curly_bracketed_term_03, true(T == '{}')) :-
		^^set_text_input('{}. '),
		{read(T)}.

	test(lgt_curly_bracketed_term_04, true(T == '{}'(','(a,b)))) :-
		^^set_text_input('{(a,b)}. '),
		{read(T)}.

	test(lgt_curly_bracketed_term_05, true(T1 == T2)) :-
		^^set_text_input('{a}. {}(a). '),
		{read(T1), read(T2)}.

	test(lgt_curly_bracketed_term_06, true(T1 == T2)) :-
		^^set_text_input('{a,b}. {}((a,b)). '),
		{read(T1), read(T2)}.

	test(lgt_curly_bracketed_term_07, true(Assertion)) :-
		^^set_text_output(''),
		{write_canonical({a})},
		^^text_output_assertion('{}(a)', Assertion).

	test(lgt_curly_bracketed_term_08, true(Assertion)) :-
		^^set_text_output(''),
		{write_canonical({a,b})},
		^^text_output_assertion('{}(\',\'(a,b))', Assertion).

	test(lgt_curly_bracketed_term_09, true(Assertion)) :-
		^^set_text_output(''),
		{writeq({'A','B','C'})},
		^^text_output_assertion('{\'A\',\'B\',\'C\'}', Assertion).

	test(lgt_curly_bracketed_term_10, true(Assertion)) :-
		^^set_text_output(''),
		{write({1,2,3})},
		^^text_output_assertion('{1,2,3}', Assertion).

	test(lgt_curly_bracketed_term_11, true(Assertion)) :-
		^^set_text_output(''),
		{write({(1,2,3)})},
		^^text_output_assertion('{1,2,3}', Assertion).

	cleanup :-
		^^clean_text_input,
		^^clean_text_output.

:- end_object.
