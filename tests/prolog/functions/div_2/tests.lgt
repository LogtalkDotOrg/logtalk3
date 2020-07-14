%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		date is 2020-07-14,
		comment is 'Unit tests for the ISO Prolog standard div/2 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_div_2_01, true(X == 2)) :-
		{X is div(7, 3)}.

	test(lgt_div_2_02, true(X == 0)) :-
		{X is div(0, 3+11)}.

	test(lgt_div_2_03, true(X == -4)) :-
		{X is div(7,-2)}.

	test(lgt_div_2_04, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is div(77, N)}.

	test(lgt_div_2_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is div(Foo, 77)}.

	test(lgt_div_2_06, error(type_error(integer,7.5))) :-
		% try to delay the expected error to runtime
		{G = (_X is div(7.5, 2)), call(G)}.

	test(lgt_div_2_07, error(evaluation_error(zero_divisor))) :-
		% try to delay the expected error to runtime
		{G = (_X is div(7, 0)), call(G)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.
