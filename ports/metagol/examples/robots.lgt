%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  
%  Copyright 2016 Metagol authors
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


:- set_logtalk_flag(hook, metagol).


:- object(robots,
	extends(metagol)).

%% metagol settings
functional.
max_clauses(10).

%% tell metagol to use the BK
prim(move_left/2).
prim(move_right/2).
prim(move_forwards/2).
prim(move_backwards/2).
prim(grab_ball/2).
prim(drop_ball/2).


unfold_program.

%% metarules
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

%% functional check
func_test(Atom,PS,G):-
	Atom = [P,A,B],
	Actual = [P,A,Z],
	\+ (::prove_deduce([Actual],PS,G), Z \= B).

%% robot learning to move a ball to a specific position
:- public(learn1/0).
learn1 :-
	Pos = [f(world((1/1),(1/1),false),world((3/3),(3/3),false))],
	::learn(Pos,[]).

:- public(learn2/0).
learn2 :-
	Pos = [f(world((1/1),(1/1),false),world((5/5),(5/5),false))],
	::learn(Pos,[]).

:- public(learn3/0).
learn3 :-
	Pos = [f(world((1/1),(1/1),false),world((6/6),(6/6),false))],
	::learn(Pos,[]).

:- public(learn4/0).
learn4 :-
	Pos = [f(world((1/1),(1/1),false),world((7/7),(7/7),false))],
	::learn(Pos,[]).

%% background knowledge
max_right(6).
max_forwards(6).

grab_ball(world(Pos,Pos,false),world(Pos,Pos,true)).

drop_ball(world(Pos,Pos,true),world(Pos,Pos,false)).

move_left(world(X1/Y1,Bpos,false),world(X2/Y1,Bpos,false)):-
	X1 > 0,
	X2 is X1-1.

move_left(world(X1/Y1,_,true),world(X2/Y1,X2/Y1,true)):-
	X1 > 0,
	X2 is X1-1.

move_right(world(X1/Y1,Bpos,false),world(X2/Y1,Bpos,false)):-
  max_right(MAXRIGHT),
  X1 < MAXRIGHT,
  X2 is X1+1.

move_right(world(X1/Y1,_,true),world(X2/Y1,X2/Y1,true)):-
	max_right(MAXRIGHT),
	X1 < MAXRIGHT,
	X2 is X1+1.

move_backwards(world(X1/Y1,Bpos,false),world(X1/Y2,Bpos,false)):-
	Y1 > 0,
	Y2 is Y1-1.

move_backwards(world(X1/Y1,_,true),world(X1/Y2,X1/Y2,true)):-
	Y1 > 0,
	Y2 is Y1-1.

move_forwards(world(X1/Y1,Bpos,false),world(X1/Y2,Bpos,false)):-
	max_forwards(MAXFORWARDS),
	Y1 < MAXFORWARDS,
	Y2 is Y1+1.

move_forwards(world(X1/Y1,_,true),world(X1/Y2,X1/Y2,true)):-
	max_forwards(MAXFORWARDS),
	Y1 < MAXFORWARDS,
	Y2 is Y1+1.

:- end_object.
