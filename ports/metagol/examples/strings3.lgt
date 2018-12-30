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


:- object(strings3,
	extends(metagol)).

%% metagol settings
functional.

%% tell metagol to use the BK
prim(copy1/2).
prim(skip1/2).
prim(write1/3).
prim(next_empty/1).

%% metarules
metarule([P,Q,R],([P,A,B]:-[[Q,A,C],[R,C,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A],[R,A,B]])).
metarule([P,Q,X],([P,A,B]:-[[Q,A,B,X]])).
metarule([P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

%% background knowledge
copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
skip1([_|RestIn]/Out,RestIn/Out).
write1(In/[H|RestOut],In/RestOut,H).
next_empty([_]/_).

func_test(Atom,PS,G) :-
	Atom = [P,In/B,_/[]],
	Actual = [P,In/Z,_/[]],
	\+ (::prove_deduce([Actual],PS,G),Z \= B).

:- public(learn/0).
learn :-
	Pos = [
		f(['a','b','c']/['a','b','c','d'],_/[]),
		f(['a','a','c']/['a','a','c','d'],_/[]),
		f(['a','c']/['a','c','d'],_/[])
	],
	::learn(Pos,[]).

:- end_object.
