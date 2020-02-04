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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2018-03-12,
		comment is 'Unit tests for the ISO Prolog standard setof/3 built-in predicate.'
	]).

	:- uses(lgtunit, [variant/2]).
	:- uses(list, [msort/2]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.10.3.4

	succeeds(iso_setof_3_01) :-
		{setof(X, (X=1;X=2), S)},
		S == [1,2].

	succeeds(iso_setof_3_02) :-
		{setof(X, (X=1;X=2), X)},
		X == [1,2].

	succeeds(iso_setof_3_03) :-
		{setof(X, (X=2;X=1), S)},
		S == [1,2].

	succeeds(iso_setof_3_04) :-
		{setof(X, (X=2;X=2), S)},
		S == [2].

	succeeds(iso_setof_3_05) :-
		{setof(X, (X=Y;X=Z), S)},
		(	S = [Z,Y] ->
			true
		;	S = [Y,Z]
		).

	fails(iso_setof_3_06) :-
		{setof(_X, fail, _S)}.

	succeeds(iso_setof_3_07) :-
		findall(L-Y, {setof(1,(Y=1;Y=2),L)}, LL),
		LL = [[1]-1, [1]-2].

	succeeds(iso_setof_3_08) :-
		{setof(f(X,Y), (X=a;Y=b), L)},
		L = [f(_,b), f(a,_)].

	succeeds(iso_setof_3_09) :-
		{setof(X, Y^((X=1,Y=1);(X=2,Y=2)), S)},
		S = [1, 2].

	succeeds(iso_setof_3_10) :-
		{setof(X, Y^((X=1;Y=1);(X=2,Y=2)), S)},
		S = [_, 1, 2].

	succeeds(iso_setof_3_11) :-
		{(	catch(1^true, _, fail) ->
			findall(S-Y, setof(X,(Y^(X=1;Y=2);X=3),S), L),
			L = [[1,3]-_,[_]-2]
		;	set_prolog_flag(unknown, fail),
			setof(X,(Y^(X=1;Y=2);X=3),S),
			S == [3]
		)}.

	succeeds(iso_setof_3_12) :-
		findall(S-Y, {setof(X,(X=Y;X=Z;Y=1),S)}, LL),
		(	LL = [[Y,Z]-_, [_]-1] ->
			true
		;	LL = [[Z,Y]-_, [_]-1]
		).

	succeeds(iso_setof_3_13) :-
		{setof(X, a(X,Y), L)},
		L == [1, 2], Y = f(_).

	succeeds(iso_setof_3_14) :-
		{setof(X, setof_3_member(X,[f(U,b),f(V,c)]), L)},
		(	L = [f(U,b),f(V,c)] ->
			true
		;	L = [f(V,c),f(U,b)]
		).

	succeeds(iso_setof_3_15) :-
		(	{setof(X, setof_3_member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)])} ->
			U == a, V == a
		;	true
		).

	succeeds(iso_setof_3_16) :-
		{setof(X, setof_3_member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)])},
		U == a, V == a.

	succeeds(iso_setof_3_17) :-
		(	{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), L)} ->
			L = [U, V, f(U), f(V)]
		;	L = [V, U, f(V), f(U)]
		).

	succeeds(iso_setof_3_18) :-
		{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)])},
		(	U == a, V == b ->
			true
		;	U == b, V == a
		).

	fails(iso_setof_3_19) :-
		{setof(X, setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)])}.

	succeeds(iso_setof_3_20) :-
		% example fixed in ISO/IEC 13211-1:1995/Cor.1:2007
		{setof(X, exists(U,V)^setof_3_member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)])}.

	succeeds(iso_setof_3_21) :-
		findall(L-Y, {setof(X,b(X,Y),L)}, LL),
		LL == [[1, 2]-1, [1, 2]-2].

	succeeds(iso_setof_3_22) :-
		{setof(X-Xs, Y^setof(Y,b(X,Y),Xs), L)},
		L == [1-[1,2], 2-[1,2]].

	succeeds(iso_setof_3_23) :-
		{setof(X-Xs, setof(Y,b(X,Y),Xs), L)},
		var(Y), L == [1-[1,2], 2-[1,2]].

	succeeds(iso_setof_3_24) :-
		{setof(X-Xs, bagof(Y,d(X,Y),Xs), L)},
		var(Y), L == [1-[1,2,1], 2-[2,1,2]].

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).
		succeeds(eddbali_setof_3_25) :-
			{setof(f(X,Y),X=Y,[f(g(Z),Z)])}.
	:- else.
		- succeeds(eddbali_setof_3_25) :-
			% STO; Undefined
			{setof(f(X,Y),X=Y,[f(g(Z),Z)])}.
	:- endif.

	throws(eddbali_setof_3_26, [error(type_error(callable,(true;4)),_), error(type_error(callable,4),_)]) :-
		% the second exception term is a common but not strictly conforming alternative
		{setof(X, X^(true; 4), _L)}.

	throws(sics_setof_3_27, error(type_error(callable,1),_)) :-
		{setof(_X, A^A^1, _L)}.

	succeeds(sics_setof_3_28) :-
		{setof(X, X=1, [1|A])},
		A == [].

	throws(sics_setof_3_29, error(type_error(list,[A|1]),_)) :-
		{setof(X, X=1, [A|1])}.

	% tests from the ECLiPSe test suite

	throws(eclipse_setof_3_30, error(type_error(list,12),_)) :-
		{setof(X, (X=2; X=1), 12)}.

	throws(eclipse_setof_3_31, error(type_error(list,[1|2]),_)) :-
		{setof(X, (X=2; X=1), [1|2])}.

	% tests from the Logtalk portability work

	succeeds(lgt_setof_3_32) :-
		{setof(Z, X^Y^c(X,Y,Z), L)},
		L == ['A', 'B', 'C'].

	succeeds(lgt_setof_3_33) :-
		{setof(Z, t(X,Y)^c(X,Y,Z), L)},
		L == ['A', 'B', 'C'].

	succeeds(lgt_setof_3_34) :-
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

	throws(lgt_setof_3_35, error(instantiation_error,_)) :-
		{setof(_X, _Y^_Z, _L)}.

:- end_object.
