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


:- object(meta_test_object).

	:- public(m/1).
    :- meta_predicate(m(0)).
    m(Goal) :-
		call(Goal).

:- end_object.


:- object(call_super_test_object_1).

	:- public([
		p/1, t1/1, t2/1, t3/1, v/1
	]).

	:- protected(q/1).
	q(1).

	:- protected(r/1).

	:- private(s/1).
	s(2).

	v(42).

:- end_object.


:- object(call_super_test_object_2,
	extends(call_super_test_object_1)).

	p(Goal) :-
		^^Goal.

	t1(X) :-
		^^q(X).

	t2(X) :-
		Closure = ^^q,
		call(Closure, X).

	:- set_logtalk_flag(suspicious_calls, silent).
	t3(X) :-
		{Closure = ^^q},
		call(Closure, X).

	v(X) :-
		meta_test_object::m(^^v(X)).

:- end_object.


:- category(cat).

    :- protected(p/0).
    p :-
		write('p called').

:- end_category.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-06-27,
		comment is 'Unit tests for the (^^)/1 built-in control construct.'
	]).

	test(call_super_1_01, error(instantiation_error)) :-
		call_super_test_object_2::p(_).

	test(call_super_1_02, error(type_error(callable,1))) :-
		call_super_test_object_2::p(1).

	test(call_super_1_03, error(permission_error(access,private_predicate,s/1))) :-
		call_super_test_object_2::p(s(_)).

	test(call_super_1_04, error(existence_error(predicate_declaration,t/1))) :-
		call_super_test_object_2::p(t(_)).

	test(call_super_1_05, deterministic(X == 1)) :-
		call_super_test_object_2::p(q(X)).

	test(call_super_1_06, deterministic(X == 1)) :-
		call_super_test_object_2::t1(X).

	test(call_super_1_07, deterministic(X == 1)) :-
		call_super_test_object_2::t2(X).

	test(call_super_1_08, deterministic(X == 1)) :-
		call_super_test_object_2::t3(X).

	test(call_super_1_09, false) :-
		call_super_test_object_2::p(r(_)).

	test(call_super_1_10, deterministic(X == 42)) :-
		call_super_test_object_2::v(X).

	test(call_super_1_11, deterministic(Assertion)) :-
		^^set_text_output(''),
		create_object(_, [imports(cat)], [initialization(^^p)], []),
		^^text_output_assertion('p called', Assertion),
		^^clean_text_output.

:- end_object.
