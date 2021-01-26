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
		date is 2021-01-26,
		comment is 'Unit tests for the ISO Prolog standard min/2 built-in function.'
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 9.3.9.4

	test(iso_min_2_01, true(X == 2)) :-
		{X is min(2, 3)}.

	test(iso_min_2_02, true) :-
		catch(
			({X is min(2, 3.0), (X == 2; X == 2.0)}),
			Error,
			Error = error(evaluation_error(undefined), _)
		).

	test(iso_min_2_03, true) :-
		catch(
			({X is min(2.0, 3), X == 2.0}),
			Error,
			Error = error(evaluation_error(undefined), _)
		).

	test(iso_min_2_04, true) :-
		catch(
			({X is min(0, 0.0), (X == 0; X == 0.0)}),
			Error,
			Error = error(evaluation_error(undefined), _)
		).

	% tests from the Logtalk portability work

	test(lgt_min_2_05, true(X == 2.0)) :-
		{X is min(2.0, 3.0)}.

	test(lgt_min_2_06, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is min(2, N)}.

	test(lgt_min_2_07, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is min(N, 3)}.

	test(lgt_min_2_08, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is min(2, Foo)}.

	test(lgt_min_2_09, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is min(Foo, 3)}.

	test(lgt_min_2_10, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is min(2, Foo)}.

	test(lgt_min_2_11, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is min(Foo, 3)}.

	test(lgt_min_2_12, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is min(2, Foo)}.

	test(lgt_min_2_13, error(type_error(evaluable,foo/2))) :-
		% try to delay the error to runtime
		foo(2, Foo),
		{_X is min(Foo, 3)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).
	foo(2, foo(1,2)).

:- end_object.
