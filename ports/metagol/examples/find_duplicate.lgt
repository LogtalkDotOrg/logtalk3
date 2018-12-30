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


:- object(find_duplicate,
	extends(metagol)).

	:- uses(list, [msort/2]).

%% metagol settings
functional.
fold_program.
max_clauses(4).

%% tell metagol to use the BK
prim(mergesort/2).
prim(tail/2).
prim(head/2).
prim(element/2).

%% metarules
metarule(dident,  [P,Q,R], ([P,A,B]:-[[Q,A,B],[R,A,B]])).
metarule(chain,   [P,Q,R], ([P,A,B]:-[[Q,A,C],[R,C,B]])).
metarule(tailrec, [P,Q], ([P,A,B]:-[[Q,A,C],[P,C,B]])).

%% background knowledge
head([H|_],H).
tail([_|T],T).
element([X|_T],X).
element([_|T],X) :- element(T,X).
mergesort([H|T],B) :- A = [H|T], msort(A,B), A\=B.

%% functional test
func_test(Atom,PS,G) :-
	Atom = [P,A,B],
	Actual = [P,A,Z],
	\+ (::prove_deduce([Actual],PS,G), Z \= B).

:- public(learn/0).
learn :-
	Pos = [
		f([1,3,3,4,2,5],3),
		f([6,4,2,5,3,5,1],5),
		f([7,3,4,2,1,5,6,7,8],7),
		f([6,5,7,8,4,2,1,3,7],7),
		f([14,4,13,6,12,1,9,2,10,8,15,5,7,14,3,11],14)
	],
	::learn(Pos, []).

:- end_object.
