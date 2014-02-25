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
		date is 2014/02/25,
		comment is 'Unit tests for the setof/3 built-in method.'
	]).

	succeeds(setof_3_1) :-
		Goal = Y^a(X, Y),
		setof(X, Goal, L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_2) :-
		Goal = a(X, Y),
		setof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_3) :-
		setof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	succeeds(setof_3_4) :-
		findall(Y-L, setof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	% the following two tests were posted by Ulrich Neumerkel in the SWI-Prolog
	% mailing list in the context of a discussion about existential variables

	succeeds(setof_3_5) :-
		findall(X-Ts, setof(t, (X^2 = 2^2 ; X^2 = 3^2), Ts), LL),
		LL == [2-[t], 3-[t]].

	succeeds(setof_3_6) :-
		findall(X-Ts, setof(t, member(X^2,[1^2,3^2]), Ts), LL),
		LL == [1-[t], 3-[t]].

	throws(setof_3_7, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		setof(_, _, _).

	throws(setof_3_8, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		setof(_, Goal, _).

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
