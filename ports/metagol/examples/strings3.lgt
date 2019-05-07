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


:- object(strings3,
	implements(metagol_example_protocol),
	extends(metagol)).

	:- uses(list, [length/2]).

	%% metagol settings
	functional.

	%% tell metagol to use the BK
	body_pred(copy1/2).
	body_pred(skip1/2).
	body_pred(write1/3).
	body_pred(next_empty/1).
	body_pred(empty/1).

	%% metarules
	metarule([P,Q], [P,A,B], [[Q,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
	metarule([P,Q,X], [P,A,B], [[Q,A,B,X]]).
	metarule([P,Q], [P,A,B], [[Q,A,C],@term_gt(A,C),[P,C,B]]).

	%% background knowledge
	copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
	skip1([_|RestIn]/Out,RestIn/Out).
	write1(In/[H|RestOut],In/RestOut,H).
	next_empty([_]/_).
	empty([]/_).

	func_test(Atom1, Atom2, Condition):-
		Atom1 = [P,In/B,_/[]],
		Atom2 = [P,In/Z,_/[]],
		Condition = (Z = B).

	%% term ordering for recursive metarule that ensures that with each iteration the length of the string decreases
	:- protected(term_gt/2).
	term_gt(A,B):-
		A = In1/_,
		B = In2/_,
		length(In1,X),
		length(In2,Y),
		X>Y.

	learn(Clauses) :-
		Pos = [
			f(['a','b','c']/['a','b','c','d'],_/[]),
			f(['a','a','c']/['a','a','c','d'],_/[]),
			f(['a','c']/['a','c','d'],_/[])
		],
		^^learn(Pos, [], Prog),
		^^pclauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
