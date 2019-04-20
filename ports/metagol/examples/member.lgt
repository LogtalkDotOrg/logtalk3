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


:- set_logtalk_flag(hook, metagol).


:- object(member,
	extends(metagol)).

	%% tell Metagol to use the BK
	prim(cons/2).
	prim(value/2).

	%% metarules
	metarule([P,Q],   [P,A,B], [[Q,B,A]]).
	metarule([P,Q], [P,A,B], [[Q,B,C],[P,A,C]]).

	%% background knowledge
	cons([4,3,2,1],[3,2,1]).
	cons([3,2,1],[2,1]).
	cons([2,1],[1]).
	cons([1],0).
	value([4,3,2,1],4).
	value([3,2,1],3).
	value([2,1],2).
	value([1],1).

	:- public(learn/1).
	learn(Clauses) :-
		Pos = [
			target(4,[4,3,2,1]),
			target(3,[4,3,2,1]),
			target(2,[4,3,2,1]),
			target(1,[4,3,2,1]),
			target(3,[3,2,1]),
			target(2,[3,2,1]),
			target(1,[3,2,1]),
			target(2,[2,1]),
			target(1,[2,1]),
			target(1,[1])
		],
		Neg = [
			target(5,[4,3,2,1]),
			target(6,[4,3,2,1]),
			target(7,[4,3,2,1]),
			target(8,[4,3,2,1]),
			target(4,[3,2,1]),
			target(5,[3,2,1]),
			target(6,[3,2,1]),
			target(3,[2,1]),
			target(4,[2,1]),
			target(2,[1])
		],
		::learn(Pos, Neg, Prog),
		::pclauses(Prog, Clauses).

	:- public(learn/0).
	learn :-
		learn(Clauses),
		::pprint_clauses(Clauses).

:- end_object.
