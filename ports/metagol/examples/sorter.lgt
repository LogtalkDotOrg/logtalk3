%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Learning robot sorting algorithm taken from the following paper:
%% A. Cropper and S.H. Muggleton. Learning efficient logical robot strategies involving composable objects. In Proceedings of the 24th International Joint Conference Artificial Intelligence (IJCAI 2015), pages 3423-3429. IJCAI, 2015.


:- set_logtalk_flag(hook, metagol).


:- object(sorter,
	extends(metagol)).

	:- uses(list, [append/3, length/2, memberchk/2, nth1/3]).
	:- uses(time, [now/3]).
	:- uses(fast_random, [random/3, randseq/4, set_seed/1]).
	:- uses(integer, [between/3]).
	:- uses(user, [setarg/3]).

	:- dynamic(energy_bound/1).

	%% metagol settings
	max_clauses(4).

	%% tell metagol to use the BK
	body_pred(comp_adjacent/2).
	body_pred(decrement_end/2).
	body_pred(go_to_start/2).
	body_pred(pick_up_left/2).
	body_pred(split/2).
	body_pred(combine/2).

	%% metarules
	metarule(tailrec, [P,Q],   [P,A,B], [[Q,A,C],@term_gt(A,C),[P,C,B],@term_gt(C,B)]).
	metarule(chain,   [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	:- public(learn/1).
	learn(Clauses) :-
		set_seed(111),
		examples(10,TrainExamples),
		::learn(TrainExamples, [], G),
		::pclauses(G, Clauses).

	:- public(learn/0).
	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

	example(N,A,B) :-
		randseq(N,1,N,L1),
		sort(L1,L2),
		A = [values(L1),energy(0),intervals([1-N]),robot_pos(1),holding_left(none),holding_right(none),left_bag([]),right_bag([])],
		B = [values(L2),energy(_),intervals(_),robot_pos(_),holding_left(_),holding_right(_),left_bag(_),right_bag(_)].

	examples(M,Xs) :-
		findall(
			f(A,B),
			(	between(1,M,_),
				random(2,25,N),
				example(N,A,B)
			),
			Xs
		).

	examples(M,N,Xs) :-
		findall(
			f(A,B),
			(	between(1,M,_),
				example(N,A,B)
			),
			Xs
		).

	sum_intervals([],Acc,Acc).
	sum_intervals([X-Y|T],Acc1,Total) :-
		integer(X),
		integer(Y),
		Dif is Y-X,
		Acc2 is Acc1 + Dif,
		sum_intervals(T,Acc2,Total).

	:- public(term_gt/2).

	term_gt(A, B) :-
		world_check(intervals(Xs),A),
		world_check(intervals(Zs),B),
		sum_intervals(Xs,0,SumXs),
		sum_intervals(Zs,0,SumZs),
		SumXs > SumZs, !.


	term_gt(A, B) :-
		world_check(robot_pos(APos),A),
		world_check(robot_pos(BPos),B),
		APos < BPos, !.


	%% FIRST-ORDER BACKGROUND KNOWLEDGE
	pocket_left(A,B) :-
		world_check(holding_right(none),A),!,
		pick_up_right(A,C),
		pocket_left(C,B).

	pocket_left(A,B) :-
		world_check(holding_right(Value),A),
		Value \= none,
		world_replace(left_bag(L),left_bag([Value|L]),A,C),
		world_replace(holding_right(Value),holding_right(none),C,B).

	pocket_right(A,B) :-
		world_check(holding_right(none),A),!,
		pick_up_right(A,C),
		pocket_right(C,B).

	pocket_right(A,B) :-
		world_check(holding_right(Value),A),
		Value \= none,
		world_replace(right_bag(L),right_bag([Value|L]),A,C),
		world_replace(holding_right(Value),holding_right(none),C,B).

	left_bag_empty(A) :-
		world_check(left_bag([]),A).

	right_bag_empty(A) :-
		world_check(right_bag([]),A).

	unbag_left(A,A) :-
		left_bag_empty(A),!.

	unbag_left(A,B) :-
		world_check(left_bag([H|T]),A),
		robot_pos(A,Pos),
		value_at(A,Pos,none),!,
		value_replace_at(A,C,Pos,H),
		world_replace(left_bag(_),left_bag(T),C,D),
		unbag_left(D,B).

	unbag_left(A,B) :-
		move_left(A,C),
		unbag_left(C,B).

	unbag_right(A,A) :-
		right_bag_empty(A),!.

	unbag_right(A,B) :-
		world_check(right_bag([H|T]),A),
		robot_pos(A,Pos),
		value_at(A,Pos,none),!,
		value_replace_at(A,C,Pos,H),
		world_replace(right_bag(_),right_bag(T),C,D),
		unbag_right(D,B).

	unbag_right(A,B) :-
		move_left(A,C),
		unbag_right(C,B).

	%% BSORT body_predS
	comp_adj(A,B) :- next_gt_current(A), !, move_right(A,B).
	comp_adj(A,B) :- next_lt_current(A), !, swap_adj(A,C), move_right(C,B).

	comp_adjacent(A,B) :- comp_adj(A,C), !, increment_energy(C,B,1).

	swap_adj(A,B) :-
		pick_up_left(A,C),
		move_right(C,D),
		pick_up_right(D,E),
		drop_left(E,F),
		move_left(F,G),
		drop_right(G,B).

	next_lt_current(A) :-
		robot_pos(A,Pos1),
		Pos2 is Pos1+1,
		value_at(A,Pos1,X),
		value_at(A,Pos2,Y),
		Y @< X.

	next_gt_current(A) :-
		robot_pos(A,Pos1),
		Pos2 is Pos1+1,
		value_at(A,Pos1,X),
		value_at(A,Pos2,Y),
		Y @> X.


	%% QSORT body_predS
	add_intervals(A,B) :-
		world_check(intervals(Intervals1),A),
		Intervals1 = [StartPos-EndPos|_],
		world_check(left_bag(LeftBag),A),
		world_check(right_bag(RightBag),A),
		length(LeftBag,LeftLen),
		length(RightBag,RightLen),

		LeftStartPos is StartPos,
		LeftEndPos is LeftStartPos+LeftLen-1,
		RightStartPos is LeftEndPos+2,
		RightEndPos is EndPos,

		(
			(
				LeftLen > 0,
				LeftStartPos \= LeftEndPos
			)
			->
			(
				append(Intervals1,[LeftStartPos-LeftEndPos], Intervals2),
				world_replace(intervals(_),intervals(Intervals2),A,C)
			);
				world_replace(intervals(Intervals1),intervals(Intervals1),A,C)
		),

		(
			(
				RightLen > 0,
				RightStartPos \= RightEndPos
			)
			->
			(
				world_check(intervals(Intervals3),C),
				append(Intervals3,[RightStartPos-RightEndPos],Intervals4),
				world_replace(intervals(_),intervals(Intervals4),C,B)
			)
			;
				world_replace(intervals(Tmp1),intervals(Tmp1),C,B)
		).

	remove_this_interval(A,B) :-
		world_replace(intervals([_|T]),intervals(T),A,B).

	compare_proxy(A,B) :- compare(A,C), !, increment_energy(C,B,1).

	compare(A,B) :-
		right_leq_left(A),!,
		pocket_left(A,B).

	compare(A,B) :-
		right_gt_left(A),!,
		pocket_right(A,B).

	split(A,B) :-
		at_end_pos(A),!,
		add_intervals(A,B).

	split(A,B) :-
		move_right(A,C),
		pick_up_right(C,D),
		compare_proxy(D,E),
		split(E,B).

	drop_pivot(A,B) :- drop_left(A,B), !.
	drop_pivot(A,B) :- move_left(A,C),drop_pivot(C,B).

	combine(A,B) :-
		unbag_right(A,C),
		drop_pivot(C,D),
		unbag_left(D,E),
		remove_this_interval(E,B).


	%% PREDICATES RELATED TO INTERVALS

	increment_start(A,B) :-
		world_check(intervals([StartPos1-EndPos|T]),A),
		StartPos2 is StartPos1 + 1,
		StartPos2 =< EndPos,
		world_replace(intervals([StartPos1-EndPos|T]),intervals([StartPos2-EndPos|T]),A,B).

	decrement_end(A,B) :-
		world_check(intervals([StartPos-EndPos1|T]),A),
		robot_pos(A,EndPos1),
		EndPos2 is EndPos1 - 1,
		EndPos2 >= StartPos,
		world_replace(intervals([StartPos-EndPos1|T]),intervals([StartPos-EndPos2|T]),A,B).

	world_check(X, A) :-
		nonvar(A),
		memberchk(X, A).

	world_replace(X,Y,A,B) :-
		nonvar(A),
		append(Prefix,[X|Suffix],A),
		append(Prefix,[Y|Suffix],B).

	value_at(A,Index,Value) :-
		world_check(values(L),A),
		nth1(Index,L,Value).

	value_replace_at(A,B,I,X) :-
		world_check(values(L),A),
		Dummy =.. [dummy|L],
		J is I,
		setarg(J,Dummy,X),
		Dummy =..[dummy|R],
		world_replace(values(L),values(R),A,B).

	%% BASIC MOVEMENTS AND ACTIONS
	move_right(A,B) :-
		world_check(robot_pos(X1),A),
		end_pos(A,EndPos),
		X2 is X1+1,
		X2 =< EndPos,
		world_replace(robot_pos(X1),robot_pos(X2),A,B).

	move_left(A,B) :-
		world_check(robot_pos(X1),A),
		start_pos(A,StartPos),
		X2 is X1-1,
		X2 >= StartPos,
		world_replace(robot_pos(X1),robot_pos(X2),A,B).

	go_to(A,A,Pos) :-
		world_check(robot_pos(Pos),A),!.

	go_to(A,B,Pos) :-
		world_check(robot_pos(X1),A),
		X1 < Pos,!,
		move_right(A,C),
		go_to(C,B,Pos).

	go_to(A,B,Pos) :-
		world_check(robot_pos(X1),A),
		X1 > Pos,!,
		move_left(A,C),
		go_to(C,B,Pos).

	go_to_start(A,B) :-
		start_pos(A,X),
		go_to(A,B,X).

	go_to_end(A,B) :-
		end_pos(A,X),
		go_to(A,B,X).

	at_end_pos(A) :-
		end_pos(A,X),
		robot_pos(A,X).

	at_start_pos(A) :-
		start_pos(A,X),
		robot_pos(A,X).

	robot_pos(A,X) :-
		world_check(robot_pos(X),A).

	end_pos(A,EndPos) :-
		world_check(intervals([_-EndPos|_]),A).

	start_pos(A,StartPos) :-
		world_check(intervals([StartPos-_|_]),A).

	drop_left(A,B) :-
		world_check(holding_left(X),A),
		X \= none,
		robot_pos(A,Pos),
		value_at(A,Pos,none),
		value_replace_at(A,C,Pos,X),
		world_replace(holding_left(_),holding_left(none),C,B).

	drop_right(A,B) :-
		world_check(holding_right(X),A),
		X \= none,
		robot_pos(A,Pos),
		value_at(A,Pos,none),
		value_replace_at(A,C,Pos,X),
		world_replace(holding_right(_),holding_right(none),C,B).

	pick_up_left(A,B) :-
		world_check(holding_left(none),A),
		world_check(robot_pos(Pos),A),
		value_at(A,Pos,Value),
		Value \= none,
		value_replace_at(A,C,Pos,none),
		world_replace(holding_left(_),holding_left(Value),C,B).

	pick_up_right(A,B) :-
		world_check(holding_right(none),A),
		world_check(robot_pos(Pos),A),
		value_at(A,Pos,Value),
		Value \= none,
		value_replace_at(A,C,Pos,none),
		world_replace(holding_right(_),holding_right(Value),C,B).

	left_gt_right(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		X > Y.

	left_lt_right(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		X < Y.

	right_gt_left(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		X < Y.

	right_lt_left(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		X > Y.

	right_leq_left(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		Y =< X.

	holding_same(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(X),A),
		X \= none.

	holding_different(A) :-
		world_check(holding_left(X),A),
		world_check(holding_right(Y),A),
		X \= none,
		Y \= none,
		X \= Y.

	%% ENERGY COSTS
	increment_energy(A,B,Amount) :-
		energy_bound(Bound),!,
		world_check(energy(E1),A),
		E2 is E1+Amount,
		E2 =< Bound,
		world_replace(energy(E1),energy(E2),A,B).

	increment_energy(A,B,Amount) :-
		world_check(energy(E1),A),
		E2 is E1+Amount,
		world_replace(energy(E1),energy(E2),A,B).

	:- public(set_rand/0).
	set_rand :-
		now(_Hours, Minute, Second),
		X is Minute * Second, Y=X, Z=X,
		set_seed(rand(X,Y,Z)).

	resource_complexity(S,E) :-
		world_check(energy(E),S).

	try_retract:-
		retract(energy_bound(_)), !.
	try_retract.

:- end_object.
