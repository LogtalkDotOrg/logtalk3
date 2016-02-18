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


% code adapted to Logtalk by Paulo Moura from one of the examples
% distributed with SICStus Prolog 4.0.2 (November 2010)

/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Perfect Square Placement
 * Author	: Mats Carlsson
 */


/*
 * | ?- squares(dual,wcd,Xs,Ys). wcd | disjoint | spec | card
 * 
 * Xs = [1,71,76,1,80,51,1,51,47,28,53,36,60,36,36,51,28,53,47,76,51],
 * Ys = [1,71,34,51,1,1,86,30,89,94,71,66,55,51,83,55,86,64,83,30,64]
 */

:- object(squares).

	:- public(squares/2).

	:- use_module(clpfd, [
		cumulative/2, disjoint2/1, domain/3, fd_min/2, in/2,
		(#=)/2, (#>)/2, (#=<)/2, (#>=)/2, (#<=>)/2,
		op(700, xfx, #=), op(700, xfx, #>), op(700, xfx, #=<), op(700, xfx, #>=), op(760, yfx, #<=>),
		op(450, xfx, ..)
	]).

	squares(dual, Type) :-
		constraints(Type, Xs, Ys, _Sizes, Limit),
		dual_labeling(Xs, 1, Limit),
		dual_labeling(Ys, 1, Limit),
		writeq(Xs), nl,
		writeq(Ys), nl.

	% problem specific dual_labeling
	dual_labeling([], _, _) :- !.
	dual_labeling(L, I, Limit) :-
		dual_labeling(L, L1, I, Limit, J),
		dual_labeling(L1, J, Limit).

	dual_labeling([], [], _, J, J).
	dual_labeling([X|L1], L2, I, J0, J) :-
		(   integer(X) -> dual_labeling(L1, L2, I, J0, J)
		;   X #= I, dual_labeling(L1, L2, I, J0, J)
		;   X #> I,
			fd_min(X, J1),
			J2 is min(J0,J1),
			L2 = [X|L3],
			dual_labeling(L1, L3, I, J2, J)
		).

	constraints(Type, Xs, Ys, Sizes, Limit) :-
		generate_squares(Xs, Ys, Sizes, Limit),
		state_asymmetry(Xs, Ys, Sizes, Limit),
		state_no_overlap(Xs, Ys, Sizes, Type),
		cumulative(Xs, Sizes, Sizes, Limit),
		cumulative(Ys, Sizes, Sizes, Limit),
		true.

	% for compatibility
	cumulative(Os, Ds, Hs, L) :-
		mktasks(Os, Ds, Hs, Tasks),
		cumulative(Tasks, [limit(L),global(true)]).

	mktasks([], [], [], []).
	mktasks([O|Os], [D|Ds], [H|Hs], [task(O,D,E,H,0)|Tasks]) :-
		E in 0..1000,
		mktasks(Os, Ds, Hs, Tasks).

	generate_squares(Xs, Ys, Sizes, Size) :-
		size_squares(Size, Sizes),
		generate_coordinates(Xs, Ys, Sizes, Size).

	generate_coordinates([], [], [], _).
	generate_coordinates([X|Xs], [Y|Ys], [S|Ss], Size) :-
		Sd is Size-S+1,
		domain([X,Y], 1, Sd),
		generate_coordinates(Xs, Ys, Ss, Size).

	% first square has center in SW quarter, under the positive diagonal
	state_asymmetry([X|_], [Y|_], [D|_], Limit) :-
		UB is (Limit-D+2)>>1,
		X in 1..UB,
		Y #=< X.

	state_no_overlap(Xs, Ys, Sizes, disjoint) :- !,
		disjoint_data(Xs, Ys, Sizes, Data),
		disjoint2(Data).
	state_no_overlap([], [], [], _).
	state_no_overlap([X|Xs], [Y|Ys], [S|Ss], Type) :-
		state_no_overlap(X, Y, S, Xs, Ys, Ss, Type),
		state_no_overlap(Xs, Ys, Ss, Type).

	disjoint_data([], [], [], []).
	disjoint_data([X|Xs], [Y|Ys], [S|Ss], [r(X,S,Y,S)|Rs]) :-
		disjoint_data(Xs, Ys, Ss, Rs).

	state_no_overlap(_, _, _, [], [], [], _).
	state_no_overlap(X, Y, S, [X1|Xs], [Y1|Ys], [S1|Ss], Type) :-
		no_overlap(Type, X, Y, S, X1, Y1, S1),
		state_no_overlap(X, Y, S, Xs, Ys, Ss, Type).

	no_overlap(spec, X1, _Y1, S1, X2, _Y2, _S2) :-
			leqc(X1, S1, X2).
	no_overlap(spec, X1, _Y1, _S1, X2, _Y2, S2) :-
			leqc(X2, S2, X1).
	no_overlap(spec, _X1, Y1, S1, _X2, Y2, _S2) :-
			leqc(Y1, S1, Y2).
	no_overlap(spec, _X1, Y1, _S1, _X2, Y2, S2) :-
			leqc(Y2, S2, Y1).
	no_overlap(card, X1, Y1, S1, X2, Y2, S2) :-
		X1+S1 #=< X2 #<=> B1,
		X2+S2 #=< X1 #<=> B2,
		Y1+S1 #=< Y2 #<=> B3,
		Y2+S2 #=< Y1 #<=> B4,
		B1+B2+B3+B4 #>= 1.
	no_overlap(wcd, X1, Y1, S1, X2, Y2, S2) :-
		no_overlap_ix(X1, Y1, S1, X2, Y2, S2).

	leqc(X1, S1, X2) :-
		X1+S1 #=< X2.

	no_overlap_ix(X1, Y1, S1, X2, Y2, S2) +:
		X1 in ((((min(Y1)+S1)..max(Y2))\/((min(Y2)+S2)..max(Y1))) ? (inf..sup))
			\/ \(max(X2)-(S1-1) .. min(X2)+(S2-1)),
		X2 in ((((min(Y1)+S1)..max(Y2))\/((min(Y2)+S2)..max(Y1))) ? (inf..sup))
			\/ \(max(X1)-(S2-1) .. min(X1)+(S1-1)),
		Y1 in ((((min(X1)+S1) .. max(X2))\/((min(X2)+S2)..max(X1))) ? (inf..sup))
			\/ \(max(Y2)-(S1-1) .. min(Y2)+(S2-1)),
		Y2 in ((((min(X1)+S1)..max(X2))\/((min(X2)+S2)..max(X1))) ? (inf..sup))
			\/ \(max(Y1)-(S2-1) .. min(Y1)+(S1-1)).

	size_squares(10, [6,4,4,4,2,2,2,2]).

	% size_squares(20, [9,8,8,7,5,4,4,4,4,4,3,3,3,2,2,1,1]).

	% size_squares(175, [81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

	% size_squares(112, [50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).

:- end_object.
