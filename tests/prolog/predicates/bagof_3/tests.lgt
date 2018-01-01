%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/08/25,
		comment is 'Unit tests for the ISO Prolog standard bagof/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.2.4

	succeeds(iso_bagof_3_01) :-
		{bagof(X, (X=1;X=2), S)},
		S == [1,2].

	succeeds(iso_bagof_3_02) :-
		{bagof(X, (X=1;X=2), X)},
		X == [1,2].

	succeeds(iso_bagof_3_03) :-
		{bagof(X, (X=Y;X=Z), S)},
		S == [Y, Z].

	fails(iso_bagof_3_04) :-
		{bagof(_X, fail, _S)}.

	succeeds(iso_bagof_3_05) :-
		findall(L-Y, {bagof(1,(Y=1;Y=2),L)}, LL),
		(	LL == [[1]-1, [1]-2] ->
			true
		;	LL == [[1]-2, [1]-1]
		).

	succeeds(iso_bagof_3_06) :-
		{bagof(f(X,Y), (X=a;Y=b), L)},
		L = [f(a, _), f(_, b)].

	succeeds(iso_bagof_3_07) :-
		{bagof(X, Y^((X=1,Y=1);(X=2,Y=2)), S)},
		S == [1, 2].

	succeeds(iso_bagof_3_08) :-
		{bagof(X, Y^((X=1;Y=1);(X=2,Y=2)), S)},
		S = [1, _, 2].

	succeeds(iso_bagof_3_09) :-
		{(	catch(1^true, _, fail) ->
			findall(S-Y, bagof(X,(Y^(X=1;Y=1);X=3),S), L),
			L = [[1,3]-_,[_]-1]
		;	set_prolog_flag(unknown, fail),
			bagof(X,(Y^(X=1;Y=1);X=3),S),
			S == [3]
		)}.

	succeeds(iso_bagof_3_10) :-
		findall(S-Y, {bagof(X,(X=Y;X=Z;Y=1),S)}, LL),
		LL = [[Y,Z]-_, [_]-1].

	succeeds(iso_bagof_3_11) :-
		{bagof(X, a(X,Y), L)},
		L = [1, 2], Y = f(_).

	succeeds(iso_bagof_3_12) :-
		findall(L-Y, {bagof(X,b(X,Y),L)}, LL),
		LL = [[1, 1, 2]-1, [1, 2, 2]-2].

	throws(iso_bagof_3_13, error(instantiation_error,_)) :-
		{bagof(_X, _Y^_Z, _L)}.

	throws(iso_bagof_3_14, error(type_error(callable,1),_)) :-
		{bagof(_X, 1, _L)}.

	% tests from the ECLiPSe test suite

	throws(eclipse_bagof_3_15, error(type_error(list,12),_)) :-
		{bagof(X, (X=2; X=1), 12)}.

	throws(eclipse_bagof_3_16, error(type_error(list,[1|2]),_)) :-
		{bagof(X, (X=2; X=1), [1|2])}.

	% tests from the Logtalk portability work

	succeeds(lgt_bagof_3_17) :-
		{bagof(Z, X^Y^c(X,Y,Z), L)},
		L == ['B', 'A', 'C'].

	succeeds(lgt_bagof_3_18) :-
		{bagof(Z, t(X,Y)^c(X,Y,Z), L)},
		L == ['B', 'A', 'C'].

:- end_object.
