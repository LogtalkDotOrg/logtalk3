%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(eight_puzzle,
	instantiates(heuristic_state_space)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2004/8/15,
		comment is 'Eight puzzle heuristic state space search problem.'
	]).

	:- uses(list, [member/2]).

	initial_state(four_steps, [2/2,1/3,3/2,2/3,3/3,3/1,2/1,1/1,1/2]).
	initial_state(five_steps, [2/1,1/2,1/3,3/3,3/2,3/1,2/2,1/1,2/3]).
	initial_state(eighteen_steps, [2/2,2/3,1/3,3/1,1/2,2/1,3/3,1/1,3/2]).

	goal_state(goal, [2/2,1/3,2/3,3/3,3/2,3/1,2/1,1/1,1/2]).

	print_state([S0,S1,S2,S3,S4,S5,S6,S7,S8]) :-
		member(Y, [3, 2, 1]),
		nl,
		member(X, [1, 2, 3]),
		member(Tile-X/Y, [' '-S0,1-S1,2-S2,3-S3,4-S4,5-S5,6-S6,7-S7,8-S8]),
		write(Tile),
		fail.
	print_state(_) :-
		nl.

	next_state([Empty| L], [Tile| L2], 1) :-
		swap(Empty, Tile, L, L2).

	swap(Empty, Tile, [Tile| L], [Empty| L]) :-
		dist(Empty, Tile, 1).
	swap(Empty, Tile, [Tile2| L], [Tile2| L2]) :-
		swap(Empty, Tile, L, L2).

	dist(X/Y, X2/Y2, D) :-
		abs_diff(X, X2, Dx),
		abs_diff(Y, Y2, Dy),
		D is Dx + Dy.

	abs_diff(A, B, D) :-
		A > B ->
			D is A - B
			;
			D is B - A.

	heuristic([_| L], H) :-
		goal_state(_, [_| G]),
		totdist(L, G, 0, D),
		seq(L, S),
		H is D + 3*S.

	totdist([], [], D, D).
	totdist([T| L], [T2| L2], Acc, D) :-
		dist(T, T2, D1),
		Acc2 is Acc + D1,
		totdist(L, L2, Acc2, D).

	seq([First| L], S) :-
		seq([First| L], First, 0, S).

	seq([T1, T2| L], First, Acc, S) :-
		score(T1, T2, S1),
		Acc2 is Acc + S1,
		seq([T2| L], First, Acc2, S).
	seq([Last], First, Acc, S) :-
		score(Last, First, Score),
		S is Acc + Score.

	score(2/2, _, 1) :- !.

	score(1/3, 2/3, 0) :- !.
	score(2/3, 3/3, 0) :- !.
	score(3/3, 3/2, 0) :- !.
	score(3/2, 3/1, 0) :- !.
	score(3/1, 2/1, 0) :- !.
	score(2/1, 1/1, 0) :- !.
	score(1/1, 1/2, 0) :- !.
	score(1/2, 1/3, 0) :- !.

	score(_, _, 2) :- !.

:- end_object.
