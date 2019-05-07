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


:- object(droplasts,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% this example demonstrates a more normal setting with extra/redundant metarules and BK predicates.
	%% the example is from the papers:
	%% A. Cropper and S.H. Muggleton. Learning higher-order logic programs through abstraction and invention. IJCAI 2016.
	%% R. Morel, A. Cropper, and L. Ong. Typed meta-interpretive learning of logic programs.  JELIA 2019.

	%% target theory is:
	%% f(A,B):-map(A,B,f_1).
	%% f_1(A,B):-my_reverse(A,C),f_2(C,B).
	%% f_2(A,B):-my_tail(A,C),my_reverse(C,B).

	:- uses(integer, [between/3, succ/2]).
	:- uses(list, [last/2, length/2, msort/2, reverse/2, valid/1 as is_list/1]).
	:- uses(meta, [maplist/3]).

	functional.

	func_test(Atom1,Atom2,Condition):-
		Atom1 = [P,A,B],
		Atom2 = [P,A,Z],
		Condition = (Z = B).

	even(X):-
	    number(X),
	    0 is X mod 2.
	odd(X):-
	    number(X),
	    1 is X mod 2.

	my_double(A,B):-
	    integer(A),
	    B is A*2.
	my_succ(A,B):-
	    integer(A),
	    (ground(B) -> integer(B); true),
	    succ(A,B).
	my_prec(A,B):-
	    integer(A),
	    (ground(B) -> integer(B); true),
	    succ(B,A).
	my_length(A,B):-
	    is_list(A),
	    (nonvar(B) -> integer(B); true),
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
	my_msort(A,B):-
	    (nonvar(A) -> is_list(A); true),
	    (nonvar(B) -> is_list(B); true),
	    msort(A,B).

	empty([]).
	not_empty([_|_]).

	body_pred(my_succ/2).
	body_pred(my_prec/2).
	body_pred(my_double/2).
	body_pred(my_length/2).
	body_pred(my_last/2).
	body_pred(my_head/2).
	body_pred(my_tail/2).
	body_pred(my_reverse/2).
	body_pred(my_msort/2).
	body_pred(empty/1).
	body_pred(not_empty/1).
	body_pred(even/1).
	body_pred(odd/1).

	%% metarules
	metarule([P,Q], [P,A], [[Q,A]]).
	metarule([P,Q,R], [P,A], [[Q,A],[R,A]]).
	metarule([P,Q], [P,A,B], [[Q,A,B]]).
	metarule([P,Q,F], [P,A,B], [[Q,A,B,F]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,B]]).

	%% interpreted BK
	ibk([map,[],[],_],[]).
	ibk([map,[H1|T1],[H2|T2],F],[[F,H1,H2],[map,T1,T2,F]]).

	ibk([filter,[],[],_],[]).
	ibk([filter,[A|T1],[A|T2],F],[[F,A],[filter,T1,T2,F]]).
	ibk([filter,[_|T1],T2,F],[[filter,T1,T2,F]]).

	gen_ex(A,B):-
	    random::between(2, 30, NumRows),
	    findall(
			SubList,
			(	between(1, NumRows, _),
				random::between(2, 29, NumColumns),
				random::sequence(NumColumns, 1, NumColumns, SubList)
			),
			A
		),
	    maplist(my_droplast, A, B).

	learn(Clauses) :-
		findall(f(A,B), (between(1,5,_),gen_ex(A,B)), Pos),
		^^learn(Pos,[], Prog),
		^^pclauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
