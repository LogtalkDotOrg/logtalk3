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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.3.4

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

% database for tests from the Logtalk portability work

c(2, b, 'B').
c(1, a, 'A').
c(3, c, 'C').

a(foo(_), p/0, 1).
a(baz(_), x/1, 2).
a(foo(_), p/0, 2).
a(bar(_), s/0, 2).
a(foo(_), p/0, 3).
a(foo(_), q/1, 1).
a(foo(_), r/2, 2).
a(bar(_), s/0, 1).
a(bar(_), s/0, 3).
a(foo(_), q/1, 2).
a(foo(_), q/1, 3).
a(foo(_), r/2, 1).
a(bar(_), t/1, 1).
a(bar(_), t/1, 2).
a(bar(_), u/2, 2).
a(bar(_), u/2, 3).
a(baz(_), v/0, 1).
a(baz(_), v/0, 2).
a(bar(_), t/1, 3).
a(bar(_), u/2, 1).
a(baz(_), v/0, 3).
a(baz(_), x/1, 1).
a(foo(_), r/2, 3).
a(baz(_), x/1, 3).
a(baz(_), z/2, 1).
a(baz(_), z/2, 2).
a(baz(_), z/2, 3).

% avoid conflicts with a possible member/2 built-in predicate
setof_3_member(X, [X| _]).
setof_3_member(X, [_| L]) :-
	setof_3_member(X, L).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2020-12-13,
		comment is 'Unit tests for the ISO Prolog standard setof/3 built-in predicate.'
	]).

	:- uses(lgtunit, [assertion/1, variant/2]).
	:- uses(list, [msort/2]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.3.4

	test(iso_setof_3_01, true(S == [1,2])) :-
		{setof(X, (X=1;X=2), S)}.

	test(iso_setof_3_02, true(X == [1,2])) :-
		{setof(X, (X=1;X=2), X)}.

	test(iso_setof_3_03, true(S == [1,2])) :-
		{setof(X, (X=2;X=1), S)}.

	test(iso_setof_3_04, true(S == [2])) :-
		{setof(X, (X=2;X=2), S)}.

	test(iso_setof_3_05, true((S == [Z,Y]; S == [Y,Z]))) :-
		{setof(X, (X=Y;X=Z), S)}.

	test(iso_setof_3_06, false) :-
		{setof(_X, fail, _S)}.

	test(iso_setof_3_07, true(LL == [[1]-1, [1]-2])) :-
		findall(L-Y, {setof(1,(Y=1;Y=2),L)}, LL).

	test(iso_setof_3_08, variant(L, [f(_,b), f(a,_)])) :-
		{setof(f(X,Y), (X=a;Y=b), L)}.

	test(iso_setof_3_09, true(S == [1, 2])) :-
		{setof(X, Y^((X=1,Y=1);(X=2,Y=2)), S)}.

	test(iso_setof_3_10, variant(S, [_, 1, 2])) :-
		{setof(X, Y^((X=1;Y=1);(X=2,Y=2)), S)}.

	:- if(catch({1^true}, _, fail)).

		test(iso_setof_3_11, variant(L, [[1,3]-_,[_]-2])) :-
			findall(S-Y, {setof(X, (Y^(X=1;Y=2);X=3), S)}, L).

	:- else.

		test(iso_setof_3_11, true(S == [3])) :-
			{set_prolog_flag(unknown, fail),
			 setof(X, (Y^(X=1;Y=2);X=3), S)}.

	:- endif.

	test(iso_setof_3_12, true) :-
		findall(S-Y, {setof(X,(X=Y;X=_Z;Y=1),S)}, LL),
		(	variant(LL, [[A,_]-A, [_]-1]) ->
			true
		;	variant(LL, [[_,A]-A, [_]-1])
		).

	test(iso_setof_3_13, variant(Y, f(_))) :-
		{setof(X, a(X,Y), L)},
		assertion(L == [1, 2]).

	test(iso_setof_3_14, true) :-
		{setof(X, setof_3_member(X,[f(U,b),f(V,c)]), L)},
		(	variant(L, [f(U,b),f(V,c)]) ->
			true
		;	variant(L, [f(V,c),f(U,b)])
		).

	test(iso_setof_3_15, true) :-
		(	{setof(X, setof_3_member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)])} ->
			U == a, V == a
		;	true
		).

	test(iso_setof_3_16, true((U == a, V == a))) :-
		{setof(X, setof_3_member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)])}.

	test(iso_setof_3_17, true) :-
		(	{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), L)} ->
			variant(L, [U, V, f(U), f(V)])
		;	variant(L, [V, U, f(V), f(U)])
		).

	test(iso_setof_3_18, true) :-
		{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)])},
		(	(U == a, V == b) ->
			true
		;	U == b, V == a
		).

	test(iso_setof_3_19, false) :-
		{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)])}.

	test(iso_setof_3_20, true) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{setof(X, exists(U,V)^setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)])}.

	test(iso_setof_3_21, true(LL == [[1, 2]-1, [1, 2]-2])) :-
		findall(L-Y, {setof(X,b(X,Y),L)}, LL).

	test(iso_setof_3_22, true(L == [1-[1,2], 2-[1,2]])) :-
		{setof(X-Xs, Y^setof(Y,b(X,Y),Xs), L)}.

	test(iso_setof_3_23, true) :-
		{setof(X-Xs, setof(Y,b(X,Y),Xs), L)},
		assertion(var(Y)),
		assertion(L == [1-[1,2], 2-[1,2]]).

	test(iso_setof_3_24, true) :-
		{setof(X-Xs, bagof(Y,d(X,Y),Xs), L)},
		assertion(var(Y)),
		assertion(L == [1-[1,2,1], 2-[2,1,2]]).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(eddbali_setof_3_25, true) :-
			{setof(f(X,Y),X=Y,[f(g(Z),Z)])}.

	:- else.

		- test(eddbali_setof_3_25, true) :-
			% STO; Undefined
			{setof(f(X,Y),X=Y,[f(g(Z),Z)])}.

	:- endif.

	test(eddbali_setof_3_26, errors([type_error(callable,(true;4)), type_error(callable,4)])) :-
		% the second exception term is a common but not strictly conforming alternative
		{setof(X, X^(true; 4), _L)}.

	test(sics_setof_3_27, error(type_error(callable,1))) :-
		{setof(_X, A^A^1, _L)}.

	test(sics_setof_3_28, true(A == [])) :-
		{setof(X, X=1, [1|A])}.

	test(sics_setof_3_29, error(type_error(list,[A|1]))) :-
		{setof(X, X=1, [A|1])}.

	% tests from the ECLiPSe test suite

	test(eclipse_setof_3_30, error(type_error(list,12))) :-
		{setof(X, (X=2; X=1), 12)}.

	test(eclipse_setof_3_31, error(type_error(list,[1|2]))) :-
		{setof(X, (X=2; X=1), [1|2])}.

	% tests from the Logtalk portability work

	test(lgt_setof_3_32, true(L == ['A', 'B', 'C'])) :-
		{setof(Z, X^Y^c(X,Y,Z), L)}.

	test(lgt_setof_3_33, true(L == ['A', 'B', 'C'])) :-
		{setof(Z, t(X,Y)^c(X,Y,Z), L)}.

	test(lgt_setof_3_34, true) :-
		findall(T2-L-T1, {setof(T3, a(T1, T2, T3), L)}, Ls),
		msort(Ls, Ss),
		variant(
			Ss,
			[
				p/0 - [1,2,3] - foo(_),
				q/1 - [1,2,3] - foo(_),
				r/2 - [1,2,3] - foo(_),
				s/0 - [1,2,3] - bar(_),
				t/1 - [1,2,3] - bar(_),
				u/2 - [1,2,3] - bar(_),
				v/0 - [1,2,3] - baz(_),
				x/1 - [1,2,3] - baz(_),
				z/2 - [1,2,3] - baz(_)
			]
		).

	test(lgt_setof_3_35, error(instantiation_error)) :-
		{setof(_X, _Y^_Z, _L)}.

:- end_object.
