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
		date is 2014/04/30,
		comment is 'Unit tests for the findall/3 built-in method.'
	]).

	succeeds(findall_3_01) :-
		findall(X, a(X, _), L),
		L == [1, 2, 3, 4].

	succeeds(findall_3_02) :-
		findall(Y-L, findall(X, a(X, Y), L), LL),
		LL = [_-[1,2,3,4]].

	% the following tests are taken from the ISO Prolog Core standard

	succeeds(findall_3_03) :-
		findall(X, (X=1; X=2), L),
		L == [1, 2].

	succeeds(findall_3_04) :-
		findall(X+_Y, (X=1), L),
		L = [1+_].

	succeeds(findall_3_05) :-
		findall(_X, fail, L),
		L == [].

	succeeds(findall_3_06) :-
		findall(X, (X=1; X=1), L),
		L == [1, 1].

	succeeds(findall_3_07) :-
		findall(X, (X=1; X=2), [X,Y]),
		X == 1, Y == 2.

	fails(findall_3_08) :-
		findall(X, (X=2; X=1), [1,2]).

	throws(findall_3_09, error(instantiation_error, logtalk(call(_),This))) :-
		this(This),
		findall(_X, _Goal, _L).

	throws(findall_3_10, error(type_error(callable,4), logtalk(call(4),This))) :-
		this(This),
		Goal = 4,
		findall(_X, Goal, _L).

	% data for some of the tests

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
