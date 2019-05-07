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


:- object(less_than,
	extends(metagol)).

	%% metagol settings
	unfold_program.

	%% tell Metagol to use the BK
	body_pred(succ/2).

	%% metarules
	metarule([P,Q],   [P,A,B], [[Q,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	%% background knowledge
	succ(I, J) :-
		integer::succ(I, J).

	:- public(learn/1).
	learn(Clauses) :-
		Pos = [
			target(1,3),
			target(2,5),
			target(3,7),
			target(4,10),
			target(5,9),
			target(6,8),
			target(7,9),
			target(8,10),
			target(9,10)
		],
		Neg = [
			target(3,1),
			target(7,1),
			target(2,2),
			target(8,2),
			target(4,3),
			target(9,3),
			target(4,0),
			target(10,4),
			target(5,5),
			target(6,5)
		],
		::learn(Pos, Neg, Prog),
		^^pclauses(Prog, Clauses).

	:- public(learn/0).
	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
