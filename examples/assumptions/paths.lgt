%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(paths,
	imports(assumptions)).

	:- info([
		version is 1.0,
		author is 'Orginal example by Paul Tarau et al. Adapted to Logtalk by Paulo Moura.',
		date is 2014/06/26,
		comment is 'Find paths avoiding loops using linear assumptions.'
	]).

	:- public(init/0).
	init :-
		^^assumel(c(1,[2,3])),
		^^assumel(c(2,[1,4])),
		^^assumel(c(3,[1,5])),
		^^assumel(c(4,[1,5])).

	:- public(init/1).
	init([]).
	init([Start-Ends| Edges]) :-
		^^assumel(c(Start,Ends)),
		init(Edges).

	:- public(reset/0).
	reset :-
		retractall(c(_,_)).

	:- public(path/3).
	path(X, X, [X]).
	path(X, Z, [X| Xs]) :-
		linked(X, Y),
		path(Y, Z, Xs).

	linked(X, Y) :-
		c(X, Ys),
		member(Y, Ys).

	:- private(c/2).
	:- dynamic(c/2).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
