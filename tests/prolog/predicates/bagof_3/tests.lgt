%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


% database for tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.2.4

a(1, f(_)).
a(2, f(_)).

b(1, 1).
b(1, 1).
b(1, 2).
b(2, 1).
b(2, 2).
b(2, 2).

% database for tests from the Logtalk portability work

a(1).
a(2).
a(3).

c(2, b, 'B').
c(1, a, 'A').
c(3, c, 'C').

f(_, 1, a).
f(_, 2, b).

v1(_).
v1(_).
v1(_).

v2(_, _).
v2(_, _).
v2(_, _).

v3(A, A).
v3(A, A).
v3(_, _).

v4(A, _, A).
v4(A, A, _).
v4(_, A, A).

v5(A, _, A).
v5(A, A, _).
v5(_, A, A).
v5(A, _, A).

v6(A, B, B, A).
v6(A, _, _, A).
v6(_, B, B, _).
v6(A, B, B, A).

% avoid conflicts with a possible member/2 built-in predicate
bagof_3_member(X, [X| _]).
bagof_3_member(X, [_| L]) :-
	bagof_3_member(X, L).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:12:0,
		author is 'Paulo Moura',
		date is 2025-01-07,
		comment is 'Unit tests for the ISO Prolog standard bagof/3 built-in predicate.'
	]).

	:- uses(lgtunit, [
		assertion/1, variant/2
	]).

	:- uses(list, [
		msort/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.2.4

	test(iso_bagof_3_01, true(S == [1,2])) :-
		{bagof(X, (X=1;X=2), S)}.

	test(iso_bagof_3_02, true(X == [1,2])) :-
		{bagof(X, (X=1;X=2), X)}.

	test(iso_bagof_3_03, true(S == [Y, Z])) :-
		{bagof(X, (X=Y;X=Z), S)}.

	test(iso_bagof_3_04, false) :-
		{bagof(_X, fail, _S)}.

	test(iso_bagof_3_05, true((LL == [[1]-1, [1]-2]; LL == [[1]-2, [1]-1]))) :-
		findall(L-Y, {bagof(1,(Y=1;Y=2),L)}, LL).

	test(iso_bagof_3_06, variant(L, [f(a, _), f(_, b)])) :-
		{bagof(f(X,Y), (X=a;Y=b), L)}.

	test(iso_bagof_3_07, true(S == [1, 2])) :-
		{bagof(X, Y^((X=1,Y=1);(X=2,Y=2)), S)}.

	test(iso_bagof_3_08, variant(S, [1, _, 2])) :-
		{bagof(X, Y^((X=1;Y=1);(X=2,Y=2)), S)}.

	:- if(catch(1^true, _, fail)).

		test(iso_bagof_3_09, variant(L, [[1,3]-_,[_]-1])) :-
			findall(S-Y, {bagof(X,(Y^(X=1;Y=1);X=3),S)}, L).

	:- else.

		test(iso_bagof_3_09, true(S == [3])) :-
			{	set_prolog_flag(unknown, fail),
				bagof(X,(Y^(X=1;Y=1);X=3),S)
			}.

	:- endif.

	test(iso_bagof_3_10, variant(LL, [[A,_]-A, [_]-1])) :-
		findall(S-Y, {bagof(X,(X=Y;X=_Z;Y=1),S)}, LL).

	test(iso_bagof_3_11, variant(Y, f(_))) :-
		{bagof(X, a(X,Y), L)},
		assertion(L == [1, 2]).

	test(iso_bagof_3_12, true(LL == [[1, 1, 2]-1, [1, 2, 2]-2])) :-
		findall(L-Y, {bagof(X,b(X,Y),L)}, LL).

	test(iso_bagof_3_13, error(instantiation_error)) :-
		{bagof(_X, _Y^_Z, _L)}.

	test(iso_bagof_3_14, errors([type_error(callable,1), type_error(callable,':'(user,1))])) :-
		% try to delay the error to runtime; the second exception term
		% is used in some of the Prolog compilers supporting modules
		one(One),
		{bagof(_X, One, _L)}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.2.1 NOTES

	test(iso_bagof_3_15, subsumes([_], LL)) :-
		findall(L, {bagof(X, a(X), L)}, LL).

	test(iso_bagof_3_16, true(var(X))) :-
		{bagof(X, a(X), _)}.

	test(iso_bagof_3_17, true) :-
		forall(
			{bagof(X, Y^c(X, Y, _), _)},
			(var(X), var(Y))
		).

	% tests from the ECLiPSe test suite

	test(eclipse_bagof_3_18, error(type_error(list,12))) :-
		{bagof(X, (X=2; X=1), 12)}.

	test(eclipse_bagof_3_19, error(type_error(list,[1|2]))) :-
		{bagof(X, (X=2; X=1), [1|2])}.

	% tests from the Logtalk portability work

	test(lgt_bagof_3_20, true(L == ['B', 'A', 'C'])) :-
		{bagof(Z, X^Y^c(X,Y,Z), L)}.

	test(lgt_bagof_3_21, true(L == ['B', 'A', 'C'])) :-
		{bagof(Z, t(X,Y)^c(X,Y,Z), L)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_bagof_3_22, true(LL == [a-[1], b-[2]])) :-
			X = f(X,Y,Z),
			findall(Z-L, {bagof(Y, X, L)}, LL).

	:- else.

		- test(lgt_bagof_3_22, true(LL == [a-[1], b-[2]]), [note('STO')]) :-
			% STO; Undefined
			X = f(X,Y,Z),
			findall(Z-L, {bagof(Y, X, L)}, LL).

	:- endif.

	test(lgt_bagof_3_23, errors([existence_error(procedure,foobar/1), existence_error(procedure,':'(user,foobar/1))])) :-
		{	set_prolog_flag(unknown, error),
			bagof(X, foobar(X), _)
		}.

	% tests for variable handling

	test(lgt_bagof_3_24, variant(LL, [s(_,[1,1,1])])) :-
		findall(s(X,L), {bagof(1, v1(X), L)}, LL).

	test(lgt_bagof_3_25, variant(LL, [s(_,_,[1,1,1])])) :-
		findall(s(X,Y,L), {bagof(1, v2(X,Y), L)}, LL).

	test(lgt_bagof_3_26, true) :-
		findall(s(X,Y,L), {bagof(1, v3(X,Y), L)}, LL),
		% per ISO standard, the order in which list is found is undefined
		assertion((
			variant(LL, [s(A,A,[1,1]), s(_,_,[1])])
		;	variant(LL, [s(_,_,[1]),   s(A,A,[1,1])])
		)).

	test(lgt_bagof_3_27, true) :-
		findall(s(X,Y,Z,L), {bagof(1, v4(X,Y,Z), L)}, LL),
		% per ISO standard, the order in which list is found is undefined
		assertion((
			variant(LL, [s(A,A,_,[1]), s(C,_,C,[1]), s(_,F,F,[1])])
		;	variant(LL, [s(A,A,_,[1]), s(_,F,F,[1]), s(C,_,C,[1])])
		;	variant(LL, [s(C,_,C,[1]), s(A,A,_,[1]), s(_,F,F,[1])])
		;	variant(LL, [s(C,_,C,[1]), s(_,F,F,[1]), s(A,A,_,[1])])
		;	variant(LL, [s(_,F,F,[1]), s(A,A,_,[1]), s(C,_,C,[1])])
		;	variant(LL, [s(_,F,F,[1]), s(C,_,C,[1]), s(A,A,_,[1])])
		)).

	test(lgt_bagof_3_28, true) :-
		findall(s(X,Y,Z,L), {bagof(1, v5(X,Y,Z), L)}, LL),
		% per ISO standard, the order in which list is found is undefined
		assertion((
			variant(LL, [s(A,A,_,[1]),   s(C,_,C,[1,1]), s(_,F,F,[1])])
		;	variant(LL, [s(A,A,_,[1]),   s(_,F,F,[1]),   s(C,_,C,[1,1])])
		;	variant(LL, [s(C,_,C,[1,1]), s(A,A,_,[1]),   s(_,F,F,[1])])
		;	variant(LL, [s(C,_,C,[1,1]), s(_,F,F,[1]),   s(A,A,_,[1])])
		;	variant(LL, [s(_,F,F,[1]),   s(A,A,_,[1]),   s(C,_,C,[1,1])])
		;	variant(LL, [s(_,F,F,[1]),   s(C,_,C,[1,1]), s(A,A,_,[1])])
		)).

	test(lgt_bagof_3_29, true) :-
		findall(s(X,Y,Z,T,L), {bagof(1, v6(X,Y,Z,T), L)}, LL),
		% per ISO standard, the order in which list is found is undefined
		assertion((
			variant(LL, [s(A,B,B,A,[1,1]), s(C,_,_,C,[1]),   s(_,F,F,_,[1])])
		;	variant(LL, [s(A,B,B,A,[1,1]), s(_,F,F,_,[1]),   s(C,_,_,C,[1])])
		;	variant(LL, [s(C,_,_,C,[1]),   s(A,B,B,A,[1,1]), s(_,F,F,_,[1])])
		;	variant(LL, [s(C,_,_,C,[1]),   s(_,F,F,_,[1]),   s(A,B,B,A,[1,1])])
		;	variant(LL, [s(_,F,F,_,[1]),   s(A,B,B,A,[1,1]), s(C,_,_,C,[1])])
		;	variant(LL, [s(_,F,F,_,[1]),   s(C,_,_,C,[1]),   s(A,B,B,A,[1,1])])
		)).

	test(lgt_bagof_3_30, ball(err)) :-
		{bagof(X, (X = 1; throw(err)), _)}.

	% tests from the WG17 standardization work

	test(wg17_bagof_3_31, false) :-
		{bagof(t, (L=2; L=1), L)}.

	test(wg17_bagof_3_32, true((variant(Ls, [[E,_,_],[_,E,_],[_,_,E]]); variant(Ls, [[_,_,E],[_,E,_],[E,_,_]])))) :-
		bagof(L, A^B^C^{L = [A,B,C], bagof(t, bagof_3_member(E,L), _)}, Ls).

	test(wg17_bagof_3_33, true((L == [A,B,C]; L == [C,B,A]))) :-
		bagof(E, {bagof(t, (A=E;B=E;C=E), _)}, L).

	% auxiliary predicates

	one(1).

:- end_object.
