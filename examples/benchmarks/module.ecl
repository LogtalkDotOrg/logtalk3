%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>    
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


% benchmark test module for ECLiPSe

:- module(module).

:- export(mod_nrev/2).

mod_append([], List, List).
mod_append([Head| Tail], List, [Head| Tail2]) :-
	mod_append(Tail, List, Tail2).

mod_nrev([], []).
mod_nrev([Head| Tail], Reversed) :-
	mod_nrev(Tail, ReversedTail),
	mod_append(ReversedTail, [Head], Reversed).


:- export(mod_length/2).

mod_length(List, Length) :-
	(	integer(Length) ->
		Length >= 0,
		mod_make_list(Length, List)
	;	mod_length(List, 0, Length)
	).

mod_make_list(0, []):-
	!.
mod_make_list(N, [_| Tail]):-
	M is N-1,
	mod_make_list(M, Tail).

mod_length([], Length, Length).
mod_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	mod_length(Tail, Acc2, Length).


:- export(mod_maze_solve/3).

mod_maze_solve(Start, Destination, Steps) :- 
	mod_path(Start, Destination, [Start], Path),
	mod_reverse(Path, Steps).

mod_path(Destination, Destination, Path, Path).
mod_path(Node, Destination, Path0, Path) :- 
	mod_link(Node, Next),
	\+ mod_member(Next, Path0),
	mod_path(Next, Destination, [Next | Path0], Path).

mod_link(Node1, Node2 ) :-
	mod_arc(Node1, Node2).
mod_link(Node1, Node2 ) :-
	mod_arc(Node2, Node1).

mod_arc(1, 2). mod_arc(1, 3). mod_arc(1, 4). mod_arc(1, 5). mod_arc(1, 9).
mod_arc(2, 9).
mod_arc(3, 6).
mod_arc(4, 5). mod_arc(4, 7).
mod_arc(5, 8).

mod_member(Element, [Element| _]).
mod_member(Element, [_| List]) :-
	mod_member(Element, List).

mod_reverse(List, Reversed) :-
	mod_reverse(List, [], Reversed, Reversed).

mod_reverse([], Reversed, Reversed, []).
mod_reverse([Head| Tail], List, Reversed, [_| Bound]) :-
	mod_reverse(Tail, [Head| List], Reversed, Bound).


:- export(mod_graph_path/3).

mod_graph_path(X, Y, L):-
	mod_graph_path(X, Y, [X], L).

mod_graph_path(X, Y, L, [Y| L]):-
	\+ mod_member(Y, L),
	mod_graph_edge(X, Y).
mod_graph_path(X, Y, L, R):-
	mod_graph_edge(X, Z),
	Z =\= Y,
	\+ mod_member(Z, L),
	mod_graph_path(Z, Y, [Z| L], R).

mod_graph_edge(X, Y):-
	mod_graph_node(X),
	mod_graph_node(Y),
	X =\= Y.

mod_graph_node(0).
mod_graph_node(1).
mod_graph_node(2).
mod_graph_node(3).
mod_graph_node(4).
%mod_graph_node(5).
%mod_graph_node(6).
%mod_graph_node(7).
%mod_graph_node(8).
%mod_graph_node(9).
%mod_graph_node(10).
