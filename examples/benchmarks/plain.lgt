%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


my_append([], List, List).
my_append([Head| Tail], List, [Head| Tail2]) :-
	my_append(Tail, List, Tail2).


my_nrev([], []).
my_nrev([Head| Tail], Reversed) :-
	my_nrev(Tail, ReversedTail),
	my_append(ReversedTail, [Head], Reversed).


my_length(List, Length) :-
	my_length(List, 0, Length).

my_length([], Length, Length).
my_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	my_length(Tail, Acc2, Length).


my_member(Element, [Element| _]).
my_member(Element, [_| List]) :-
	my_member(Element, List).


:- dynamic(pred_plain/4).

plain_dyndb(N) :-
	retractall(pred_plain(N, _, _,    _)),
	assertz(pred_plain(N, _, a, 3.14)).


my_between(Lower, _, Lower).
my_between(Lower, Upper, Integer) :-
	Lower < Upper,
	Next is Lower + 1,
	my_between(Next, Upper, Integer).


my_repeat(_).
my_repeat(N) :-
	N > 1,
	N2 is N - 1,
	my_repeat(N2).


% generate a list containing the first N non-negative integers

generate_list(N, List) :-
	generate_list(0, N, List).

generate_list(N, N, []) :-
	!.
generate_list(M, N, [M| Ms]) :-
	M < N,
	M2 is M + 1,
	generate_list(M2, N, Ms).



maze_solve(Start, Destination, Steps) :-
	maze_path(Start, Destination, [Start], Path),
	maze_reverse(Path, Steps).

maze_path(Destination, Destination, Path, Path).
maze_path(Node, Destination, Path0, Path) :-
	maze_link(Node, Next),
	\+ my_member(Next, Path0),
	maze_path(Next, Destination, [Next | Path0], Path).

maze_link(Node1, Node2 ) :-
	maze_arc(Node1, Node2).
maze_link(Node1, Node2 ) :-
	maze_arc(Node2, Node1).

maze_arc(1, 2). maze_arc(1, 3). maze_arc(1, 4). maze_arc(1, 5). maze_arc(1, 9).
maze_arc(2, 9).
maze_arc(3, 6).
maze_arc(4, 5). maze_arc(4, 7).
maze_arc(5, 8).

maze_reverse(List, Reversed) :-
	maze_reverse(List, [], Reversed, Reversed).

maze_reverse([], Reversed, Reversed, []).
maze_reverse([Head| Tail], List, Reversed, [_| Bound]) :-
	maze_reverse(Tail, [Head| List], Reversed, Bound).



graph_path(X, Y, L) :-
	graph_path(X, Y, [X], L).

graph_path(X, Y, L, [Y| L]) :-
	\+ my_member(Y, L),
	graph_edge(X, Y).
graph_path(X, Y, L, R) :-
	graph_edge(X, Z),
	Z =\= Y,
	\+ my_member(Z, L),
	graph_path(Z, Y, [Z| L], R).

graph_edge(X, Y) :-
	graph_node(X),
	graph_node(Y),
	X =\= Y.

graph_node(0).
graph_node(1).
graph_node(2).
graph_node(3).
graph_node(4).
%graph_node(5).
%graph_node(6).
%graph_node(7).
%graph_node(8).
%graph_node(9).
%graph_node(10).
