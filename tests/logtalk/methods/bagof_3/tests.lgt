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


% silence warning about no shared variable between template and test
:- set_logtalk_flag(suspicious_calls, silent).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2022-03-12,
		comment is 'Unit tests for the bagof/3 built-in method.'
	]).

	test(bagof_3_01, true(L == [1, 2, 3, 4])) :-
		Goal = Y^foo(X, Y),
		bagof(X, Goal, L).

	test(bagof_3_02, true(L == [1, 2, 3, 4])) :-
		Goal = foo(X, Y),
		bagof(X, Y^Goal, L).

	test(bagof_3_03, true(L == [1, 2, 3, 4])) :-
		bagof(X, Y^foo(X, Y), L).

	test(bagof_3_04, true(LL == [even-[2,4], odd-[1,3]])) :-
		findall(Y-L, bagof(X, foo(X, Y), L), LL).

	% the following tests are taken from the ISO Prolog Core standard

	test(bagof_3_05, true(LL == [[1, 2]])) :-
		findall(L, bagof(X, (X=1; X=2), L), LL).

	test(bagof_3_06, true(XX == [[1, 2]])) :-
		findall(X, bagof(X, (X=1; X=2), X), XX).

	test(bagof_3_07, true(L == [Y, Z])) :-
		bagof(X, (X=Y; X=Z), L).

	test(bagof_3_08, true) :-
		findall(Y-L, bagof(1, (Y=1; Y=2), L), LL),
		(	LL == [1-[1], 2-[1]] ->
			true
		;	LL == [2-[1], 1-[1]]
		).

	test(bagof_3_09, variant(LL, [[f(a,_), f(_,b)]])) :-
		findall(L, bagof(f(X,Y), (X=a; Y=b), L), LL).

	test(bagof_3_10, true(LL == [[1, 2]])) :-
		findall(L, bagof(X, Y^((X=1, Y=1); (X=2, Y=2)), L), LL).

	test(bagof_3_11, variant(LL, [[1, _, 2]])) :-
		findall(L, bagof(X, Y^((X=1; Y=1); (X=2, Y=2)), L), LL).

	% Logtalk doesn't support setting the `unknown` standard Prolog
	% flag *locally* to an entity to `warning` for the folowing test
	- test(bagof_3_12, variant(LL, [_-[3]])) :-
		findall(Y-L, bagof(X, ((Y^(X=1; Y=2)); X=3), L), LL).

	test(bagof_3_13, true) :-
		bagof(Y-L, bagof(X, (X=Y; X=Z; Y=1), L), LL),
		(	LL = [Y-[Y,Z], 1-[_]] ->
			true
		;	LL = [1-[_], Y-[Y,Z]]
		).

	test(bagof_3_14, variant(LL, [f(_)-[1,2]])) :-
		findall(Y-L, bagof(X, a(X,Y), L), LL).

	test(bagof_3_15, true) :-
		findall(Y-L, bagof(X, b(X,Y), L), LL),
		(	LL == [1-[1,1,2], 2-[1,2,2]] ->
			true
		;	LL == [2-[1,2,2], 1-[1,1,2]]
		).

	test(bagof_3_16, false) :-
		bagof(_X, fail, _L).

	test(bagof_3_17, ball(error(instantiation_error,logtalk(call(_),_)))) :-
		bagof(_, _, _).

	test(bagof_3_18, ball(error(type_error(callable,1),logtalk(call(1),_)))) :-
		Goal = 1,
		bagof(_, Goal, _).

	% data for some of the tests

	foo(1, odd).
	foo(2, even).
	foo(3, odd).
	foo(4, even).

	a(1, f(_)).
	a(2, f(_)).

	b(1, 1).
	b(1, 1).
	b(1, 2).
	b(2, 1).
	b(2, 2).
	b(2, 2).

:- end_object.
