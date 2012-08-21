%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
	        *******************************
	        *            *                *
	        *     2              9        *
	        *            *                *
	*************   ***********************
	1                                     * 
	*****   *********   ***********   *****
	*           *           *             *
	*     3     *     4             5     *
	*           *           *             *
	*****   *********   ***********   *****
	*           *            *            *
	*     6     *     7      *      8     *
	*           *            *            *
	***************************************
*/

:- object(maze).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/05/26,
		comment is 'Example maze path search problem for benchmarking tests.']).

	:- public(solve/3).

	solve(Start, Destination, Steps) :- 
		path(Start, Destination, [Start], Path),
		reverse(Path, Steps).

	path(Destination, Destination, Path, Path).
	path(Node, Destination, Path0, Path) :- 
		link(Node, Next),
		\+ member(Next, Path0),
		path(Next, Destination, [Next | Path0], Path).

	link(Node1, Node2 ) :-
		arc(Node1, Node2).
	link(Node1, Node2 ) :-
		arc(Node2, Node1).

	arc(1, 2). arc(1, 3). arc(1, 4). arc(1, 5). arc(1, 9).
	arc(2, 9).
	arc(3, 6).
	arc(4, 5). arc(4, 7).
	arc(5, 8).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).
    
	reverse(List, Reversed) :-
		reverse(List, [], Reversed, Reversed).

	reverse([], Reversed, Reversed, []).
	reverse([Head| Tail], List, Reversed, [_| Bound]) :-
		reverse(Tail, [Head| List], Reversed, Bound).

:- end_object.
