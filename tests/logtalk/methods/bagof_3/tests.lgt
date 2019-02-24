%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/03/24,
		comment is 'Unit tests for the bagof/3 built-in method.'
	]).

	succeeds(bagof_3_01) :-
		Goal = Y^foo(X, Y),
		bagof(X, Goal, L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_02) :-
		Goal = foo(X, Y),
		bagof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_03) :-
		bagof(X, Y^foo(X, Y), L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_04) :-
		findall(Y-L, bagof(X, foo(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	% the following tests are taken from the ISO Prolog Core standard

	succeeds(bagof_3_05) :-
		findall(L, bagof(X, (X=1; X=2), L), LL),
		LL == [[1, 2]].

	succeeds(bagof_3_06) :-
		findall(X, bagof(X, (X=1; X=2), X), XX),
		XX == [[1, 2]].

	succeeds(bagof_3_07) :-
		bagof(X, (X=Y; X=Z), L),
		L == [Y, Z].

	succeeds(bagof_3_08) :-
		findall(Y-L, bagof(1, (Y=1; Y=2), L), LL),
		(	LL == [1-[1], 2-[1]] ->
			true
		;	LL == [2-[1], 1-[1]]
		).

	succeeds(bagof_3_09) :-
		findall(L, bagof(f(X,Y), (X=a; Y=b), L), LL),
		LL = [[f(a,_), f(_,b)]].

	succeeds(bagof_3_10) :-
		findall(L, bagof(X, Y^((X=1, Y=1); (X=2, Y=2)), L), LL),
		LL == [[1, 2]].

	succeeds(bagof_3_11) :-
		findall(L, bagof(X, Y^((X=1; Y=1); (X=2, Y=2)), L), LL),
		LL = [[1, _, 2]].

	% Logtalk doesn't support setting the `unknown` standard Prolog
	% flag *locally* to an entity to `warning` for the folowing test
	- succeeds(bagof_3_12) :-
		findall(Y-L, bagof(X, ((Y^(X=1; Y=2)); X=3), L), LL),
		LL = [_-[3]].

	succeeds(bagof_3_13) :-
		bagof(Y-L, bagof(X, (X=Y; X=Z; Y=1), L), LL),
		(	LL = [Y-[Y,Z], 1-[_]] ->
			true
		;	LL = [1-[_], Y-[Y,Z]]
		).

	succeeds(bagof_3_14) :-
		findall(Y-L, bagof(X, a(X,Y), L), LL),
		LL = [f(_)-[1,2]].

	succeeds(bagof_3_15) :-
		findall(Y-L, bagof(X, b(X,Y), L), LL),
		(	LL == [1-[1,1,2], 2-[1,2,2]] ->
			true
		;	LL == [2-[1,2,2], 1-[1,1,2]]
		).

	fails(bagof_3_16) :-
		bagof(_X, fail, _L).

	throws(bagof_3_17, error(instantiation_error,logtalk(call(_),_))) :-
		bagof(_, _, _).

	throws(bagof_3_18, error(type_error(callable,1),logtalk(call(1),_))) :-
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
