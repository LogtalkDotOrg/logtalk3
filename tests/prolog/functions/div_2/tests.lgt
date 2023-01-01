%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-03-31,
		comment is 'Unit tests for the ISO Prolog standard div/2 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_div_2_01, true(X == 2)) :-
		{X is div(5, 2)}.

	test(lgt_div_2_02, true(X == 0)) :-
		{X is div(0, 3+11)}.

	test(lgt_div_2_03, true(X == -3)) :-
		{X is div(5, -2)}.

	test(lgt_div_2_04, true(X == -3)) :-
		{X is div(-5, 2)}.

	test(lgt_div_2_05, true(X == -1)) :-
		{X is div(1, -12)}.

	test(lgt_div_2_06, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is div(77, N)}.

	test(lgt_div_2_07, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is div(Foo, 77)}.

	test(lgt_div_2_08, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		real(Float),
		{_X is div(Float, 2)}.

	test(lgt_div_2_09, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		zero(Zero),
		{_X is div(7, Zero)}.

	test(lgt_div_2_10, error(evaluation_error(int_overflow)), [condition(verify_min_max_integers)]) :-
		current_prolog_flag(min_integer, Min),
		{_X is div(Min, -1)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

	real(7.5).

	zero(0).

	verify_min_max_integers :-
		current_prolog_flag(bounded, true),
		current_prolog_flag(min_integer, Min),
		current_prolog_flag(max_integer, Max),
		Min < -Max.

:- end_object.
