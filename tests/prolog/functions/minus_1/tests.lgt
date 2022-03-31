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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-03-31,
		comment is 'Unit tests for the ISO Prolog standard (-)/1 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_minus_1_01, true((N == -42))) :-
		{N = - 42}.

	test(lgt_minus_1_02, true((N == 42, M == -42))) :-
		{N = 42, M is - N}.

	test(lgt_minus_1_03, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is - N}.

	test(lgt_minus_1_04, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_05, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_06, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is - Foo}.

	test(lgt_minus_1_07, error(evaluation_error(int_overflow)), [condition(verify_min_max_integers)]) :-
		current_prolog_flag(min_integer, Min),
		{_X is - Min}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

	verify_min_max_integers :-
		current_prolog_flag(bounded, true),
		current_prolog_flag(min_integer, Min),
		current_prolog_flag(max_integer, Max),
		Min < -Max.

:- end_object.
