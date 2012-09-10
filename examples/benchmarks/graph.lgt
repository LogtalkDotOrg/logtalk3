%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(graph).

	:- info([
		version is 1.0,
		author is 'Theofrastos Mantadelis',
		date is 2010/11/14,
		comment is 'Example of a fully connected graph path search problem for benchmarking tests.']).

	:- public(path/3).

	path(X, Y, L) :-
		path(X, Y, [X], L).

	path(X, Y, L, [Y| L]) :-
		\+ member(Y, L),
		edge(X, Y).
	path(X, Y, L, R) :-
		edge(X, Z),
		Z =\= Y,
		\+ member(Z, L),
		path(Z, Y, [Z| L], R).

	edge(X, Y) :-
		node(X),
		node(Y),
		X =\= Y.

	node(0).
	node(1).
	node(2).
	node(3).
	node(4).
%	node(5).
%	node(6).
%	node(7).
%	node(8).
%	node(9).
%	node(10).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

:- end_object.
