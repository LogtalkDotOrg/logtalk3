%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tak(_Threads)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/04/03,
		comment is 'Takeuchi function (recursive arithmetic).',
		parameters is ['Threads' - 'Number of threads to use. Valid values are 1, 3, 9, 27, 81, etc.']
	]).

	:- threaded.

	:- public(tak/4).
	:- mode(tak(+integer, +integer, +integer, -integer), one).
	:- info(tak/4, [
		comment is 'Takeuchi function.',
		argnames is ['X', 'Y', 'Z', 'A']
	]).

	tak(X, Y, Z, A) :-
		parameter(1, Threads),
		Threads > 0,
		tak_mt(Threads, X, Y, Z, A).

	tak_mt(1, X, Y, Z, A) :-
		!,
		tak_st(X, Y, Z, A).
	tak_mt(_, X, Y, Z, A) :-
		X =< Y, !,
		Z = A.
	tak_mt(Threads, X, Y, Z, A) :-
		Threads3 is Threads//3,
		%X > Y,
		X1 is X - 1,
		Y1 is Y - 1,
		Z1 is Z - 1,
		threaded((
			tak_mt(Threads3, X1, Y, Z, A1),
			tak_mt(Threads3, Y1, Z, X, A2),
			tak_mt(Threads3, Z1, X, Y, A3)
		)),
		tak_st(A1, A2, A3, A).

	tak_st(X, Y, Z, A) :-
		X =< Y, !,
		Z = A.
	tak_st(X, Y, Z, A) :-
		%X > Y,
		X1 is X - 1,
		tak_st(X1, Y, Z, A1),
		Y1 is Y - 1,
		tak_st(Y1, Z, X, A2),
		Z1 is Z - 1,
		tak_st(Z1, X, Y, A3),
		tak_st(A1, A2, A3, A).

:- end_object.
