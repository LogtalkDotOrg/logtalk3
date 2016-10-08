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


/******************************************************************* 
   by Neng-Fa Zhou, 2001.
   Solve the blocks world planning problem as a CSP.
   Based on the modeling described in:
   "CPlan: A Constraint Programming Approach to Planning", AAAI'99,
   by Peter va Beek and Xinguang Chen.
   One difference from the modeling is that goal-relevant 
   constraints are employed.
********************************************************************/

:- object(blocks).

	:- public(go/0).

	go:-
		go(1).

	go(Prob) :-
		T1 is cputime,
		blocks(Prob,Blocks),
		length(Blocks,N),
	%	InitialLen is N*3//2,
		InitialLen=2,
		find_best_plan(Prob,InitialLen,N,Plan),
		T2 is cputime,
		T is T2-T1,
		write('plan found in '),write(T), write('milliseconds'),nl,
		write(Plan),nl.

	find_best_plan(Prob,Len,N,Plan) :-
		plan(Prob,Len,N,Plan),!.
	find_best_plan(Prob,Len,N,Plan) :-
		Len1 is Len+1,
		find_best_plan(Prob,Len1,N,Plan).

/*
	find_best_plan(Prob,Len,N,Plan) :-
		not(not(find_plan(Prob,Len,N,Plan))),!,
		current_best_plan(CurBestPlan),
		length(CurBestPlan,CurBestLen),
		write(CurBestLen),write(' '),write(CurBestPlan),nl,
		find_best_plan(Prob,CurBestLen,N,Plan).
	find_best_plan(Prob,Len,N,Plan) :-
		retract(current_best_plan(Plan)).

	find_plan(Prob,Len,N,Plan) :-
		plan(Prob,Len,N,Plan),!,
		(retract(current_best_plan(_))->true;true),
		assert(current_best_plan(Plan)).
*/

	plan(Prob,Len,N,Plan) :-
		generate_states(Prob,Len,States),
		%
		States=[InitState|States1],
		last_state(States1,GoalState,MidStates),
		initial(Prob,InitState),
		goal(Prob,GoalState),
		%
		action_constraints(States,GoalState),
	%	no_cycle_constraints(States),
		%
		label(MidStates),
		extract_plan(States,Plan).

	last_state([X],Last,Rest) :-!,Rest=[],Last=X.
	last_state([X|Xs],Last,[X|Rest]) :-last_state(Xs,Last,Rest).

	extract_plan([S1],Plan) :-!,Plan=[].
	extract_plan([S1,S2|States],Plan) :-
		functor(S1,_,N),
		add_moved_block(S1,S2,N,Plan,PlanR),
		extract_plan([S2|States],PlanR).
 
	add_moved_block(S1,S2,I,Plan,PlanR) :-I=:=0,!,Plan=PlanR.
	add_moved_block(S1,S2,I,Plan,PlanR) :-
		arg(I,S1,block(_,Below1)),
		arg(I,S2,block(_,Below2)),
		(	Below1=\=Below2->Plan=[(I=>Below2)|PlanR]
		;	I1 is I-1,
			add_moved_block(S1,S2,I1,Plan,PlanR)
		).

	% instantiate states forward.
	label([State|States]) :-
		get_below_vars(State,BVars),
		labeling_ffc(BVars),
		label(States).
	label([]).

/* 
   A plan is a list of states each of which is a structure of the following form:
   
   state(block(A1,B1),block(A2,B2),...,block(An,Bn))

   where Ai and Bi are two variables that tell what are the blocks above and below
   the block i, respectively. Ai is 0 if the block is clear and Bi is 0 if the block 
   is on the table
*/
	generate_states(Prob,N,States) :-N>0,!,
		States=[State|StatesR],
		generate_state(Prob,State),
		N1 is N-1,
		generate_states(Prob,N1,StatesR).
	generate_states(Prob,N,States) :-States=[].

	generate_state(Prob,State) :-
		blocks(Prob,Blocks),
		length(Blocks,NBlocks),
		functor(State,state,NBlocks),
		declare_domains(State,1,NBlocks),
		state_constraints(State).

	declare_domains(State,I,N) :-I>N,!.
	declare_domains(State,I,N) :-
		arg(I,State,block(Above,Below)),
		domain(Above,0,N), 
		domain(Below,0,N), 
		I1 is I+1,
		declare_domains(State,I1,N).

	get_below_vars(State,BVars) :-
		functor(State,_,N),
		get_below_vars(N,State,BVars).

	get_below_vars(I,State,BVars) :-I=:=0,!,BVars=[].
	get_below_vars(I,State,BVars) :-BVars=[Below|BVarsR],
		arg(I,State,block(_,Below)),
		I1 is I-1,
		get_below_vars(I1,State,BVarsR).

/*
  The following constraints ensure that a state is valid:
  (1) If block i is above block j, then block j is below block i, and vice versa.
  (2) For each block, there is at most one block above it and there is 
	  also at most one block below it.
*/
	state_constraints(State) :-
		functor(State,_,N),
		state_constraints(State,1,N).

	state_constraints(State,I,N) :-I>N,!.
	state_constraints(State,I,N) :-
		arg(I,State,block(Above,Below)),
		below_relationship(Below,State,I),
		above_relationship(Above,State,I),
		only_one_below(Below,State,I,N),
		I1 is I+1,
		state_constraints(State,I1,N).

% if Below, which is not 0, is below I, then Below cannot be below any other blocks
	only_one_below(Below,State,I,N) :-
		freeze(Below,(Below\== 0->outof_below(Below,State,I,N);true)).

	outof_below(Below,State,I,J) :-J=:=0,!.
	outof_below(Below,State,I,J) :-
		(I=:=J->true;
		 arg(J,State,block(AboveJ,BelowJ)),
		 BelowJ #\= Below),
		 J1 is J-1,
		 outof_below(Below,State,I,J1).

	% block J is below I, then I must be above Above J
	below_relationship(J,State,I) :-
		freeze(J,(J=\= 0->arg(J,State,block(AboveJ,BelowJ)),AboveJ=I;true)).

	% block J is above I, then I must be below Above J
	above_relationship(J,State,I) :-
		freeze(J,(J=\=0->arg(J,State,block(AboveJ,BelowJ)),BelowJ=I;true)).

	/*
	  A transition from Si to Sj is valid if only one block that is clear in Si is moved.
	*/
	action_constraints([_],GoalState) :-!.
	action_constraints([S1,S2|States],GoalState) :-
		transition_constraint(S1,S2,GoalState),
		action_constraints([S2|States],GoalState).

	transition_constraint(S1,S2,GoalState) :-
		functor(S1,_,N), % N blocks
		transition_constraint(S1,S2,GoalState,1,N,0).

	transition_constraint(S1,S2,GoalState,I,N,DiffBSum) :-I>N,!,
		DiffBSum #=< 1.  % at most one block is moved each time
	transition_constraint(S1,S2,GoalState,I,N,DiffBSum) :-
		arg(I,S1,block(A1,B1)),
		arg(I,S2,block(A2,B2)),
		arg(I,GoalState,block(A3,B3)),
		(B1#\=B2 #=> A1#=0 #/\A2#=0), % the moved block must be clear
		((B1 #\= B2) #<=> DiffB),
		((B #\=B2 #/\ B2#\=0) #=> B2#=B3), % make sure the move is required by the goal
		freeze(B1,freeze(B2,((B1=\=B2,B2=\=0)-> block_in_final_position(B2,S2,GoalState);true))),
		I1 is I+1,
		transition_constraint(S1,S2,GoalState,I1,N,DiffB+DiffBSum).

	% block i is moved to block j only when block j is in its final position
	block_in_final_position(I,S,GoalState) :-
		arg(I,S,block(A,B)),
		arg(I,GoalState,block(A,B)),
		(B=\=0->block_in_final_position(B,S,GoalState);true).

	/* 
	   No two states in a plan can be the same. Action constraints already guarantee that 
	   any two neiboring states are different
	*/
	no_cycle_constraints([_]) :-!.
	no_cycle_constraints([_,_]) :-!.
	no_cycle_constraints([S1,S2|States]) :-
		no_cycle_constraints(S1,States),
		no_cycle_constraints([S2|States]).

	no_cycle_constraints(S1,[]).
	no_cycle_constraints(S1,[S2|States]) :-
		different_states(S1,S2),
		no_cycle_constraints(S1,States).

	different_states(S1,S2) :-
		functor(S1,_,N),
		different_states(N,S1,S2,0).

	different_states(I,S1,S2,Sum) :-I=:=0,!,Sum#\=0.
	different_states(I,S1,S2,Sum) :-
		arg(I,S1,block(_,B1)),
		arg(I,S2,block(_,B2)),
		(B1#=B2 #<=> Same),
		I1 is I-1,
		different_states(I1,S1,S2,Sum+Same).

	/* 
	   blocks are numbered 1,2,3, and so on. 
	   clear(B,S): the block B is clear in state S.
	   on_table(B,S): the block B is on table in state S.
	   on(B1,B2,S): block B1 is one Block B2 in state S.
	*/
	%%
	clear(I,S) :-
		arg(I,S,block(Above,Below)),
		Above=0.

	on(I1,I2,S) :-
		arg(I1,S,block(Above1,Below1)),
		arg(I2,S,block(Above2,Below2)),
		Above2=I1,
		Below1=I2.

	on_table(I,S) :-
		arg(I,S,block(Above,Below)),
		Below= 0.

	%p-0
	blocks(0,[1,2]).
	initial(0,S) :-
		clear(1,S),
		on(1,2,S),
		on_table(2,S).

	goal(0,S) :-
		on_table(1,S),
		clear(2,S).

	% p-1
	blocks(01,[1,2,3,4,5,6,7,8,9]).
	initial(01,S) :-
		clear(3,S),
		clear(5,S),
		clear(9,S),
		on(2,1,S),
		on(3,2,S),
		on(5,4,S),
		on(7,6,S),
		on(8,7,S),
		on(9,8,S),
		on_table(1,S),
		on_table(4,S),
		on_table(6,S).

	goal(01,S) :-
		clear(1,S),
		clear(2,S),
		clear(8,S),
		on(1,5,S),
		on(2,3,S),
		on(3,7,S),
		on(7,6,S),
		on(8,9,S),
		on(9,4,S),
		on_table(4,S),
		on_table(5,S),
		on_table(6,S).

	%p-2
	blocks(2,[1,2,3,4,5,6,7,8,9,10,11]).
	initial(2,S) :-
		clear(11,S),
		clear(3,S),
		clear(9,S),
		on(10,5,S),
		on(11,10,S),
		on(2,1,S),
		on(3,2,S),
		on(5,4,S),
		on(7,6,S),
		on(8,7,S),
		on(9,8,S),
		on_table(1,S),
		on_table(4,S),
		on_table(6,S).
	goal(2,S) :-
		clear(1,S),
		clear(2,S),
		clear(8,S),
		on(1,5,S),
		on(11,7,S),
		on(2,3,S),
		on(3,11,S),
		on(5,10,S),
		on(7,6,S),
		on(8,9,S),
		on(9,4,S),
		on_table(10,S),
		on_table(4,S),
		on_table(6,S).

	%p3
	blocks(3,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]).
	initial(3,S) :-
		clear(11,S),
		clear(3,S),
		clear(9,S),
		on(1,12,S),
		on(10,5,S),
		on(11,10,S),
		on(12,13,S),
		on(14,15,S),
		on(2,1,S),
		on(3,2,S),
		on(4,14,S),
		on(5,4,S),
		on(7,6,S),
		on(8,7,S),
		on(9,8,S),
		on_table(13,S),
		on_table(15,S),
		on_table(6,S).
	goal(3,S) :-
		clear(12,S),
		clear(14,S),
		clear(15,S),
		on(1,5,S),
		on(11,7,S),
		on(12,2,S),
		on(13,8,S),
		on(14,1,S),
		on(15,13,S),
		on(2,3,S),
		on(3,11,S),
		on(5,10,S),
		on(7,6,S),
		on(8,9,S),
		on(9,4,S),
		on_table(10,S),
		on_table(4,S),
		on_table(6,S).

	%p4
	blocks(4,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]).
	initial(4,S) :-
		clear(1,S),
		clear(11,S),
		clear(19,S),
		clear(9,S),
		on(1,12,S),
		on(10,5,S),
		on(11,10,S),
		on(12,13,S),
		on(14,15,S),
		on(16,3,S),
		on(17,16,S),
		on(18,17,S),
		on(19,18,S),
		on(3,2,S),
		on(4,14,S),
		on(5,4,S),
		on(7,6,S),
		on(8,7,S),
		on(9,8,S),
		on_table(13,S),
		on_table(15,S),
		on_table(2,S),
		on_table(6,S).

	goal(4,S) :-
		clear(12,S),
		clear(15,S),
		clear(17,S),
		on(1,5,S),
		on(11,7,S),
		on(12,2,S),
		on(13,8,S),
		on(14,1,S),
		on(15,13,S),
		on(16,11,S),
		on(17,18,S),
		on(18,19,S),
		on(19,14,S),
		on(2,3,S),
		on(3,16,S),
		on(5,10,S),
		on(7,6,S),
		on(8,9,S),
		on(9,4,S),
		on_table(10,S),
		on_table(4,S),
		on_table(6,S).

:- end_object.
