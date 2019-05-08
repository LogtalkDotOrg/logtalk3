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


:- object(ibk1,
	implements(metagol_example_protocol),
	extends(metagol)).

	:- uses(integer, [succ/2]).

	%% allow metagol to use my_succ in a hypothesis
	body_pred(my_succ/2).

	%% for current_predicate/1 visibility
	:- protected([empty/1, head/2, tail/2]).

	%% background knowledge
	empty([]).
	head([H|_], H).
	tail([_|T], T).
	my_succ(A, B) :-
		(nonvar(A) -> integer(A); true),
		(nonvar(B) -> integer(B); true),
		succ(A,B).

	%% metarules
	metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
	metarule(curry, [P,Q,F], [P,A,B], [[Q,A,B,F]]).
	metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	%% ibk
	ibk([map,A,B,_], [[empty,A],[empty,B]]).
	ibk([map,A,B,F], [[head,A,H1],[head,B,H2],[F,H1,H2],[tail,A,T1],[tail,B,T2],[map,T1,T2,F]]).

	learn(Clauses) :-
		^^learn([f([1,2,3],[5,6,7])], [], Prog),
		^^program_to_clauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
