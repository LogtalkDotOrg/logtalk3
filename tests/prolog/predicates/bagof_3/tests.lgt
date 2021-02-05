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

c(2, b, 'B').
c(1, a, 'A').
c(3, c, 'C').


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2020-12-13,
		comment is 'Unit tests for the ISO Prolog standard bagof/3 built-in predicate.'
	]).

	:- uses(lgtunit, [
		assertion/1
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
			{set_prolog_flag(unknown, fail),
			 bagof(X,(Y^(X=1;Y=1);X=3),S)}.

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

	test(iso_bagof_3_14, error(type_error(callable,1))) :-
		{bagof(_X, 1, _L)}.

	% tests from the ECLiPSe test suite

	test(eclipse_bagof_3_15, error(type_error(list,12))) :-
		{bagof(X, (X=2; X=1), 12)}.

	test(eclipse_bagof_3_16, error(type_error(list,[1|2]))) :-
		{bagof(X, (X=2; X=1), [1|2])}.

	% tests from the Logtalk portability work

	test(lgt_bagof_3_17, true(L == ['B', 'A', 'C'])) :-
		{bagof(Z, X^Y^c(X,Y,Z), L)}.

	test(lgt_bagof_3_18, true(L == ['B', 'A', 'C'])) :-
		{bagof(Z, t(X,Y)^c(X,Y,Z), L)}.

:- end_object.
