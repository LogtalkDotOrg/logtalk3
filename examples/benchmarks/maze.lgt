%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Example maze path search problem for benchmarking tests.'
	]).

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
