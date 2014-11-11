%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/09/08,
		comment is 'Unit tests for the setof/3 built-in method.'
	]).

	succeeds(setof_3_01) :-
		Goal = Y^foo(X, Y),
		setof(X, Goal, L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_02) :-
		Goal = foo(X, Y),
		setof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_03) :-
		setof(X, Y^foo(X, Y), L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_04) :-
		findall(Y-L, setof(X, foo(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	% the following two tests were posted by Ulrich Neumerkel in the SWI-Prolog
	% mailing list in the context of a discussion about existential variables

	succeeds(setof_3_05) :-
		findall(X-Ts, setof(t, (X^2 = 2^2 ; X^2 = 3^2), Ts), LL),
		LL == [2-[t], 3-[t]].

	succeeds(setof_3_06) :-
		findall(X-Ts, setof(t, member(X^2,[1^2,3^2]), Ts), LL),
		LL == [1-[t], 3-[t]].

	% the following tests are taken from the ISO Prolog Core standard

	succeeds(setof_3_07) :-
		findall(L, setof(X, (X=1; X=2), L), LL),
		LL == [[1,2]].

	succeeds(setof_3_08) :-
		findall(X, setof(X, (X=1; X=2), X), XX),
		XX == [[1,2]].

	succeeds(setof_3_09) :-
		findall(L, setof(X, (X=2; X=1), L), LL),
		LL == [[1,2]].

	succeeds(setof_3_10) :-
		findall(L, setof(X, (X=2; X=2), L), LL),
		LL == [[2]].

	succeeds(setof_3_11) :-
		setof(X, (X=Y; X=Z), L),
		(	L == [Y,Z] ->
			true
		;	L == [Z,Y]
		).

	succeeds(setof_3_12) :-
		findall(Y-L, setof(1, (Y=2; Y=1), L), LL),
		(	LL == [1-[1], 2-[1]] ->
			true
		;	LL == [2-[1], 1-[1]]
		).

	succeeds(setof_3_13) :-
		findall(L, setof(f(X,Y), (X=a; Y=b), L), LL),
		LL = [[f(a,_), f(_,b)]].

	succeeds(setof_3_14) :-
		findall(L, setof(X, Y^((X=1, Y=1); (X=2, Y=2)), L), LL),
		LL = [[1,2]].

	succeeds(setof_3_15) :-
		findall(L, setof(X, Y^((X=1; Y=1); (X=2, Y=2)), L), LL),
		LL = [[_,1,2]].

	% Logtalk doesn't support setting the `unknown` standard Prolog
	% flag *locally* to an entity to `warning` for the folowing test
	- succeeds(setof_3_16) :-
		findall(Y-L, setof(X, ((Y^(X=1; Y=2)); X=3), L), LL),
		LL = [_-[3]].

	succeeds(setof_3_17) :-
		bagof(Y-L, setof(X, (X=Y; X=Z; Y=1), L), LL),
		(	LL = [Y-[Y,Z], 1-[_]] ->
			true
		;	LL = [1-[_], Y-[Y,Z]]
		).

	succeeds(setof_3_18) :-
		findall(Y-L, setof(X, a(X,Y), L), LL),
		LL = [f(_)-[1,2]].

	succeeds(setof_3_19) :-
		setof(X, member(X, [f(U,b),f(V,c)]), L),
		(	L == [f(U,b),f(V,c)] ->
			true
		;	L == [f(V,c),f(U,b)]
		).

	succeeds(setof_3_20) :-
		(	setof(X, member(X, [f(U,b),f(V,c)]), [f(U,b),f(V,c)]) ->
			\+ setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)])
		;	setof(X, member(X,[f(U,b),f(V,c)]), [f(a,c),f(a,b)]),
			U == a, V == a
		).

	succeeds(setof_3_21) :-
		setof(X, member(X,[f(b,U),f(c,V)]), [f(b,a),f(c,a)]),
		U == a, V == a.

	succeeds(setof_3_22) :-
		setof(X, member(X,[V,U,f(U),f(V)]), L),
		(	L == [U,V,f(U),f(V)] ->
			true
		;	L == [V,U,f(V),f(U)]
		).

	succeeds(setof_3_23) :-
		setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(a),f(b)]),
		(	U == a, V == b ->
			true
		;	U == b, V == a
		).

	succeeds(setof_3_24) :-
		setof(X, exists(U,V)^member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).

	succeeds(setof_3_25) :-
		findall(Y-L, setof(X, b(X,Y), L), LL),
		(	LL == [1-[1,2],2-[1,2]] ->
			true
		;	LL == [2-[1,2],1-[1,2]]
		).

	succeeds(setof_3_26) :-
		findall(L, setof(X-Xs, Y^setof(Y, b(X,Y), Xs), L), LL),
		LL = [[1-[1,2],2-[1,2]]].

	succeeds(setof_3_27) :-
		findall(Y-L, setof(X-Xs, setof(Y, b(X,Y), Xs), L), LL),
		LL = [_-[1-[1,2],2-[1,2]]].

	succeeds(setof_3_28) :-
		findall(Y-L, setof(X-Xs, bagof(Y, d(X,Y), Xs), L), LL),
		LL = [_-[1-[1,2,1],2-[2,1,2]]].

	fails(setof_3_29) :-
		setof(_X, fail, _L).

	fails(setof_3_30) :-
		setof(X, member(X,[V,U,f(U),f(V)]), [a,b,f(b),f(a)]).

	% tests for error conditions

	throws(setof_3_31, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		setof(_, _, _).

	throws(setof_3_32, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
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
