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
		date is 2021-06-20,
		comment is 'Unit tests for the Prolog popcount/1 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_popcount_1_01, true(X == 0)) :-
		{X is popcount(0)}.

	test(lgt_popcount_1_02, true(X == 1)) :-
		{X is popcount(1)}.

	test(lgt_popcount_1_03, true(X == 1)) :-
		{X is popcount(128)}.

	test(lgt_popcount_1_04, true(X == 4)) :-
		{X is popcount(170)}.

	test(lgt_popcount_1_05, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is popcount(N)}.

	test(lgt_popcount_1_06, error(type_error(integer,3.14))) :-
		% try to delay the error to runtime
		{_X is popcount(3.14)}.

	test(lgt_popcount_1_07, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is popcount(Foo)}.

	test(lgt_popcount_1_08, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is popcount(Foo)}.

	test(lgt_popcount_1_09, error(domain_error(not_less_than_zero,-1))) :-
		% try to delay the error to runtime
		minus_one(MinusOne),
		{_X is popcount(MinusOne)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

	minus_one(-1).

:- end_object.
