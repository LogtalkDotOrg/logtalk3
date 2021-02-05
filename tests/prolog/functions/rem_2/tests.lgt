%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-07-22,
		comment is 'Unit tests for the ISO Prolog standard rem/2 built-in predicate.'
	]).

	% tests from the Logtalk portability work

	test(lgt_rem_2_01, true(X == 0)) :-
		{X is rem(4, 2)}.

	test(lgt_rem_2_02, true(X == 1)) :-
		{X is rem(5, 2)}.

	test(iso_mod_2_03, true(X == 1)) :-
		{X is rem(5, -2)}.

	test(lgt_rem_2_04, true(X == -1)) :-
		{X is rem(-5, 2)}.

	test(lgt_rem_2_05, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is rem(77, N)}.

	test(lgt_rem_2_06, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is rem(Foo, 77)}.

	test(lgt_rem_2_07, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is rem(77, Foo)}.

	test(lgt_rem_2_08, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		{G = (_X is rem(7.5, 2)), call(G)}.

	test(lgt_rem_2_09, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is rem(7, 0)), call(G)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.
