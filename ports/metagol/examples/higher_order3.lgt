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


:- object(higher_order3,
	implements(metagol_example_protocol),
	extends(metagol)).

	:- uses(integer, [succ/2]).
	:- uses(list, [last/2, length/2, member/2, msort/2, reverse/2]).

	%% metarules
	metarule([P,Q], [P,A], [[Q,A]]).
	metarule([P,Q,B], [P,A], [[Q,A,B]]).
	metarule([P,Q,F], [P,A,B], [[Q,A,B,F]]).
	metarule([P,Q], [P,A,B], [[Q,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	%% background knowledge
	%% could we use the clpfd library?
	mydiv(A,B):-
		(nonvar(A) -> number(A); true),
		(nonvar(B) -> (number(B), B>0); true),
		(var(B) -> member(B,[2,3,4,5,6,7,8,9]); true),
		0 is A mod B.

	ibk([map,[],[],_],[]).
	ibk([map,[H1|T1],[H2|T2],F],[[F,H1,H2],[map,T1,T2,F]]).

	ibk([filter,[],[],_],[]).
	ibk([filter,[A|T1],[A|T2],F],[[F,A],[filter,T1,T2,F]]).
	ibk([filter,[_|T1],T2,F],[[filter,T1,T2,F]]).

	%% background knowledge
	my_double(A,B):-
		integer(A),B is A*2.
	my_succ(A,B):-
		integer(A),
		(ground(B)->integer(B);true),
		succ(A,B).
	my_prec(A,B):-
		integer(A),
		(ground(B)->integer(B);true),
		succ(B,A).
	my_length(A,B):-
		(nonvar(B)->integer(B);true),
		A=[_|_],
		length(A,B).
	my_last(A,B):-
		last(A,B).
	my_head([H|_],H).
	my_tail([_|T],T).
	my_reverse(A,B):-
		reverse(A,B).
	my_droplast([_],[]).
	my_droplast([H|T1],[H|T2]):-
		my_droplast(T1,T2).

	my_element([X|_T],X).
	my_element([_|T],X):-
		my_element(T,X).
	my_msort(A,B):-
		A = [_|_],
		B = [_|_],
		%% freeze(A,freeze(B,(length(A,N),length(B,N)))),
		msort(A,B).
	is_letter(A):-
	  my_char_code(_,A),!.
	%% not_letter(A):-
	  %% my_char_code(_,A),!.
	empty([]).
	not_empty([_|_]).

	%% tell metagol to use the BK
	body_pred(my_length/2).
	body_pred(my_last/2).
	body_pred(my_head/2).
	body_pred(my_tail/2).
	body_pred(my_reverse/2).
	body_pred(my_droplast/2).
	body_pred(my_msort/2).
	body_pred(empty/1).
	body_pred(not_empty/1).
	body_pred(mydiv/2).
	body_pred(my_succ/2).
	body_pred(my_prec/2).
	body_pred(my_double/2).

	learn(Clauses) :-
		Pos = [
			f([1,2,3,4,5,6,7,8,9,10],[2,4,5,6,8,10]),
			f([5,6,7,8,9,5],[5,8,5]),
			f([10,20,30],[10,20,30]),
			f([7],[])
		],
		::learn(Pos, [], Prog),
		^^pclauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
