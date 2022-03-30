%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		date is 2022-03-30,
		comment is 'Unit tests for the ISO Prolog standard (*)/2 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_multiplication_2_01, true(X == 6)) :-
		{X is 3 * 2}.

	test(lgt_multiplication_2_02, true((float(X), X == 7.0))) :-
		{X is 3.5 * 2}.

	test(lgt_multiplication_2_03, true(X == 7.5)) :-
		{X is 3 * 2.5}.

	test(lgt_multiplication_2_04, true((float(X), X == 6.0))) :-
		{X is 3.0 * 2.0}.

	test(lgt_multiplication_2_05, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is 3 * N}.

	test(lgt_multiplication_2_06, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is N * 3}.

	test(lgt_multiplication_2_07, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is 3 * Foo}.

	test(lgt_multiplication_2_08, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is Foo * 3}.

	test(lgt_multiplication_2_09, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is 3 * Foo}.

	test(lgt_multiplication_2_10, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is Foo * 3}.

	test(lgt_multiplication_2_11, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is 3 * Foo}.

	test(lgt_multiplication_2_12, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is Foo * 3}.

	test(lgt_multiplication_2_13, error(evaluation_error(int_overflow)), [condition(current_prolog_flag(bounded,true))]) :-
		current_prolog_flag(max_integer, Max),
		{_X is Max * 2}.

	test(lgt_multiplication_2_14, error(evaluation_error(float_overflow))) :-
		% try to delay the error to runtime
		big_float(Big),
		{_X is Big * Big}.

	test(lgt_multiplication_2_15, true) :-
		% try to delay the error to runtime
		small_float(Small),
		catch({X is Small * Small}, Error, true),
		(	var(Error) ->
			X == 0.0
		;	subsumes_term(error(evaluation_error(underflow),_), Error)
		).

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

	big_float(1.0e+300).

	small_float(1.0e-320).

:- end_object.
