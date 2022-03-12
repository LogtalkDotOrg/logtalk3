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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2022-03-12,
		comment is 'Unit tests for the setof/3 built-in method.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	test(setof_3_01, true(L == [1, 2, 3, 4])) :-
		Goal = Y^foo(X, Y),
		setof(X, Goal, L).

	test(setof_3_02, true(L == [1, 2, 3, 4])) :-
		Goal = foo(X, Y),
		setof(X, Y^Goal, L).

	test(setof_3_03, true(L == [1, 2, 3, 4])) :-
		setof(X, Y^foo(X, Y), L).

	test(setof_3_04, true(LL == [even-[2,4], odd-[1,3]])) :-
		findall(Y-L, setof(X, foo(X, Y), L), LL).

	% the following two tests were posted by Ulrich Neumerkel in the SWI-Prolog
	% mailing list in the context of a discussion about existential variables

	test(setof_3_05, true(LL == [2-[t], 3-[t]])) :-
		findall(X-Ts, setof(t, (X^2 = 2^2 ; X^2 = 3^2), Ts), LL).

	test(setof_3_06, true(LL == [1-[t], 3-[t]])) :-
		findall(X-Ts, setof(t, member(X^2,[1^2,3^2]), Ts), LL).

	% the following tests are taken from the ISO Prolog Core standard

	test(setof_3_07, true(LL == [[1,2]])) :-
		findall(L, setof(X, (X=1; X=2), L), LL).

	test(setof_3_08, true(XX == [[1,2]])) :-
		findall(X, setof(X, (X=1; X=2), X), XX).

	test(setof_3_09, true(LL == [[1,2]])) :-
		findall(L, setof(X, (X=2; X=1), L), LL).

	test(setof_3_10, true(LL == [[2]])) :-
		findall(L, setof(X, (X=2; X=2), L), LL).

	test(setof_3_11, true) :-
		setof(X, (X=Y; X=Z), L),
		(	L == [Y,Z] ->
			true
		;	L == [Z,Y]
		).

	test(setof_3_12, true) :-
		findall(Y-L, setof(1, (Y=2; Y=1), L), LL),
		(	LL == [1-[1], 2-[1]] ->
			true
		;	LL == [2-[1], 1-[1]]
		).

	test(setof_3_13, variant(LL, [[f(_,b), f(a,_)]])) :-
		findall(L, setof(f(X,Y), (X=a; Y=b), L), LL).

	test(setof_3_14, true(LL == [[1,2]])) :-
		findall(L, setof(X, Y^((X=1, Y=1); (X=2, Y=2)), L), LL).

	test(setof_3_15, variant(LL, [[_,1,2]])) :-
		findall(L, setof(X, Y^((X=1; Y=1); (X=2, Y=2)), L), LL).

	% Logtalk doesn't support setting the `unknown` standard Prolog
	% flag *locally* to an entity to `warning` for the folowing test
	- test(setof_3_16, variant(LL, [_-[3]])) :-
		findall(Y-L, setof(X, ((Y^(X=1; Y=2)); X=3), L), LL).

	test(setof_3_17, true) :-
		bagof(Y-L, setof(X, (X=Y; X=Z; Y=1), L), LL),
		(	variant(LL, [Y-[Y,Z], 1-[_]]) ->
			true
		;	variant(LL, [1-[_], Y-[Y,Z]])
		).

	test(setof_3_18, variant(LL, [f(_)-[1,2]])) :-
		findall(Y-L, setof(X, a(X,Y), L), LL).

	test(setof_3_19, true) :-
		setof(X, member(X, [f(U,b),f(V,c)]), L),
		(	L == [f(U,b),f(V,c)] ->
			true
		;	L == [f(V,c),f(U,b)]
		).

	test(setof_3_20, true) :-
		(	setof(X, member(X, [f(U,b),f(V,c)]), [f(U,b),f(V,c)]) ->
			\+ setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)])
		;	setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]),
			U == a, V == a
		).

	test(setof_3_21, true(U-V == a-a)) :-
		setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]).

	test(setof_3_22, true) :-
		setof(X, member(X,[V,U,f(U),f(V)]), L),
		(	L == [U,V,f(U),f(V)] ->
			true
		;	L == [V,U,f(V),f(U)]
		).

	test(setof_3_23, true) :-
		setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]),
		(	U == a, V == b ->
			true
		;	U == b, V == a
		).

	test(setof_3_24, true) :-
		setof(X, exists(U,V)^member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).

	test(setof_3_25, true) :-
		findall(Y-L, setof(X, b(X,Y), L), LL),
		(	LL == [1-[1,2],2-[1,2]] ->
			true
		;	LL == [2-[1,2],1-[1,2]]
		).

	test(setof_3_26, true(LL == [[1-[1,2],2-[1,2]]])) :-
		findall(L, setof(X-Xs, Y^setof(Y, b(X,Y), Xs), L), LL).

	test(setof_3_27, variant(LL, [_-[1-[1,2],2-[1,2]]])) :-
		findall(Y-L, setof(X-Xs, setof(Y, b(X,Y), Xs), L), LL).

	test(setof_3_28, variant(LL, [_-[1-[1,2,1],2-[2,1,2]]])) :-
		findall(Y-L, setof(X-Xs, bagof(Y, d(X,Y), Xs), L), LL).

	test(setof_3_29, false) :-
		setof(_X, fail, _L).

	test(setof_3_30, false) :-
		setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).

	% tests for error conditions

	test(setof_3_31, ball(error(instantiation_error,logtalk(call(_),_)))) :-
		setof(_, _, _).

	test(setof_3_32, ball(error(type_error(callable,1),logtalk(call(1),_)))) :-
		Goal = 1,
		setof(_, Goal, _).

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

	d(1, 1).
	d(1, 2).
	d(1, 1).
	d(2, 2).
	d(2, 1).
	d(2, 2).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
