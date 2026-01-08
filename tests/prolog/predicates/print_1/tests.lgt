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


% for testing the de facto standard print/2 predicate
:- multifile(portray/1).
:- dynamic(portray/1).

portray(Atom) :-
	atom(Atom),
	write(Atom),
	write(Atom).
portray(Float) :-
	float(Float),
	Integer is truncate(Float),
	write(Integer).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-10-28,
		comment is 'Unit tests for the Prolog the facto standard print/1 built-in predicate.'
	]).

	condition :-
		predicate_property(print(_), built_in).

	test(lgt_print_1_01, true(Assertion)) :-
		^^set_text_output(''),
		{print(42)},
		^^text_output_assertion('42', Assertion).

	test(lgt_print_1_02, true(Assertion)) :-
		^^set_text_output(''),
		{print(3.14)},
		^^text_output_assertion('3', Assertion).

	test(lgt_print_1_03, true(Assertion)) :-
		^^set_text_output(''),
		{print(foo)},
		^^text_output_assertion(foofoo, Assertion).

	test(lgt_print_1_04, true(Assertion)) :-
		^^set_text_output(''),
		{print(a(foo))},
		^^text_output_assertion('a(foofoo)', Assertion).

	test(lgt_print_1_05, true(Assertion)) :-
		^^set_text_output(''),
		{print(a(foo,b(c(foo,3.14))))},
		^^text_output_assertion('a(foofoo,b(c(foofoo,3)))', Assertion).

:- end_object.
