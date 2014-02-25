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
		comment is 'Unit tests for the bagof/3 built-in method.'
	]).

	succeeds(bagof_3_1) :-
		Goal = Y^a(X, Y),
		bagof(X, Goal, L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_2) :-
		Goal = a(X, Y),
		bagof(X, Y^Goal, L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_3) :-
		bagof(X, Y^a(X, Y), L),
		L == [1, 2, 3, 4].

	succeeds(bagof_3_4) :-
		findall(Y-L, bagof(X, a(X, Y), L), LL),
		LL == [even-[2,4], odd-[1,3]].

	throws(bagof_3_5, error(instantiation_error,logtalk(call(_),This))) :-
		this(This),
		bagof(_, _, _).

	throws(bagof_3_6, error(type_error(callable,1),logtalk(call(1),This))) :-
		this(This),
		Goal = 1,
		bagof(_, Goal, _).

	a(1, odd).
	a(2, even).
	a(3, odd).
	a(4, even).

:- end_object.
