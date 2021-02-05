%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, metagol).


:- object(robots,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% metagol settings
	functional.
	max_clauses(6).

	%% tell metagol to use the BK
	body_pred(move_left/2).
	body_pred(move_right/2).
	body_pred(move_forwards/2).
	body_pred(move_backwards/2).
	body_pred(grab_ball/2).
	body_pred(drop_ball/2).

	%% metarules
	metarule(ident,   [P,Q],   [P,A,B], [[Q,A,B]]).
	metarule(precon,  [P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
	metarule(postcon, [P,Q,R], [P,A,B], [[Q,A,B],[R,B]]).
	metarule(chain,   [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	%% functional check
	func_test(Atom1, Atom2, Condition):-
		Atom1 = [P,A,B],
		Atom2 = [P,A,Z],
		Condition = (Z = B).

	%% robot learning to move a ball to a specific position
	:- public(learn1/1).
	learn1(Clauses) :-
		Pos = [f(world((1-1),(1-1),false),world((3-3),(3-3),false))],
		^^learn(Pos, [], Prog),
		^^program_to_clauses(Prog, Clauses).

	:- public(learn2/1).
	learn2(Clauses) :-
		Pos = [f(world((1-1),(1-1),false),world((5-5),(5-5),false))],
		^^learn(Pos, [], Prog),
		^^program_to_clauses(Prog, Clauses).

	:- public(learn3/1).
	learn3(Clauses) :-
		Pos = [f(world((1-1),(1-1),false),world((6-6),(6-6),false))],
		^^learn(Pos, [], Prog),
		^^program_to_clauses(Prog, Clauses).

	:- public(learn4/1).
	learn4(Clauses) :-
		Pos = [f(world((1-1),(1-1),false),world((7-7),(7-7),false))],
		^^learn(Pos, [], Prog),
		^^program_to_clauses(Prog, Clauses).

	:- public(learn1/0).
	learn1 :-
		learn1(Clauses),
		::pprint_clauses(Clauses).

	:- public(learn2/0).
	learn2 :-
		learn2(Clauses),
		::pprint_clauses(Clauses).

	:- public(learn3/0).
	learn3 :-
		learn3(Clauses),
		::pprint_clauses(Clauses).

	:- public(learn4/0).
	learn4 :-
		learn4(Clauses),
		::pprint_clauses(Clauses).

	%% background knowledge
	max_right(6).
	max_forwards(6).

	grab_ball(world(Pos,Pos,false),world(Pos,Pos,true)).

	drop_ball(world(Pos,Pos,true),world(Pos,Pos,false)).

	move_left(world(X1-Y1,Bpos,false),world(X2-Y1,Bpos,false)) :-
		X1 > 0,
		X2 is X1-1.

	move_left(world(X1-Y1,_,true),world(X2-Y1,X2-Y1,true)) :-
		X1 > 0,
		X2 is X1-1.

	move_right(world(X1-Y1,Bpos,false),world(X2-Y1,Bpos,false)) :-
	  max_right(MAXRIGHT),
	  X1 < MAXRIGHT,
	  X2 is X1+1.

	move_right(world(X1-Y1,_,true),world(X2-Y1,X2-Y1,true)) :-
		max_right(MAXRIGHT),
		X1 < MAXRIGHT,
		X2 is X1+1.

	move_backwards(world(X1-Y1,Bpos,false),world(X1-Y2,Bpos,false)) :-
		Y1 > 0,
		Y2 is Y1-1.

	move_backwards(world(X1-Y1,_,true),world(X1-Y2,X1-Y2,true)) :-
		Y1 > 0,
		Y2 is Y1-1.

	move_forwards(world(X1-Y1,Bpos,false),world(X1-Y2,Bpos,false)) :-
		max_forwards(MAXFORWARDS),
		Y1 < MAXFORWARDS,
		Y2 is Y1+1.

	move_forwards(world(X1-Y1,_,true),world(X1-Y2,X1-Y2,true)) :-
		max_forwards(MAXFORWARDS),
		Y1 < MAXFORWARDS,
		Y2 is Y1+1.

:- end_object.
