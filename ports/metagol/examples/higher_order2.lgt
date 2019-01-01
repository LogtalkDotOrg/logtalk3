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


:- object(higher_order2,
	extends(metagol)).

	:- uses(integer, [succ/2]).
	:- uses(list, [length/2]).

%% tell metagol to use the BK
prim(my_succ/2).
prim(my_double/2).
prim(my_length/2).
interpreted(map/3).

%% background knowledge
my_double(A,B) :- integer(A),B is A*2.
my_succ(A,B) :- integer(A),(ground(B)->integer(B);true), succ(A,B).
my_length(A,B) :- A = [_|_], length(A,B).

map([],[],_F).
map([A|As],[B|Bs],F) :-
	call(F,A,B),
	map(As,Bs,F).

%% metarules
metarule([P,Q,F],([P,A,B]:-[[Q,A,B,F]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).

:- public(learn/1).
learn(Clauses) :-
	A = [[a],[a,a],[a,a,a],[a,a,a,a]],
	B = [2,4,6,8],
	::learn([f(A,B)], [], Prog),
	::pclauses(Prog, Clauses).

:- public(learn/0).
learn :-
	learn(Clauses),
	meta::maplist(::pprint_clause, Clauses).

:- end_object.
