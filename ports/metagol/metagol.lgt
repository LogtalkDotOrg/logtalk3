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


:- object(metagol,
	implements(expanding)).

	:- info([
		version is 0.15,
		author is 'Metagol authors; adapted to Logtalk by Paulo Moura.',
		date is 2019/01/08,
		copyright is 'Copyright 2016 Metagol authors; Copyright 2018-2019 Paulo Moura',
		license is 'BSD 3-Clause License',
		comment is 'Inductive logic programming (ILP) system based on meta-interpretive learning.'
	]).

	:- public(learn/3).
	:- mode(learn(@list(example), @list(example), -list(term)), zero_or_one).
	:- info(learn/3, [
		comment is 'Learns from a set of positive examples and a set of negative examples and returns the learned program.',
		argnames is ['PositiveExamples', 'NegativeExamples', 'Program']
	]).

	:- public(learn/2).
	:- mode(learn(@list(example), @list(example)), zero_or_one).
	:- info(learn/2, [
		comment is 'Learns from a set of positive examples and a set of negative examples and prints the learned program.',
		argnames is ['PositiveExamples', 'NegativeExamples']
	]).

	:- public(learn_seq/2).
	:- mode(learn_seq(@list(example), -list(clause)), zero_or_one).
	:- info(learn_seq/2, [
		comment is 'Learns from a sequence of examples represented as a list of PositiveExamples/NegativeExamples elements and returns the learned program.',
		argnames is ['Examples', 'Program']
	]).

	:- public(pclauses/2).
	:- mode(pclauses(@list(term), -list(clause)), one).
	:- info(pclauses/2, [
		comment is 'Converts a learned program into a list of clauses.',
		argnames is ['Program', 'Clauses']
	]).

	:- public(pprint/1).
	:- mode(pprint(@list(term)), one).
	:- info(pprint/1, [
		comment is 'Prints a learned program.',
		argnames is ['Program']
	]).

	:- public([metarule/7, metarule_init/6, prim/1, primcall/2, interpreted_bk/2]).
	:- dynamic([prim/1, primcall/2]).

	:- public([functional/0, unfold_program/0, min_clauses/1, max_clauses/1, max_inv_preds/1, metarule_next_id/1]).
	:- dynamic([functional/0, unfold_program/0, min_clauses/1, max_clauses/1, max_inv_preds/1, metarule_next_id/1]).

	:- protected([pprint_clause/1, pprint_clauses/1, func_test/3, prove_deduce/3, assert_prim/1, retract_prim/1, assert_program/1]).

	:- private(interpreted_/1).
	:- dynamic(interpreted_/1).

	:- uses(coroutining, [when/2]).
	:- uses(integer, [succ/2]).
	:- uses(list, [append/3, flatten/2, last/2, length/2, member/2, remove_duplicates/2 as list_to_set/2, select/3]).
	:- uses(logtalk, [print_message/3]).
	:- uses(meta, [maplist/2, maplist/3]).
	:- uses(pairs, [map/3 as map_list_to_pairs/3, values/2 as pairs_values/2]).
	:- uses(user, [between/3]).

	% defaults
	default(min_clauses(1)).
	default(max_clauses(6)).
	default(metarule_next_id(1)).
	default(max_inv_preds(10)).

	learn(Pos1,Neg1) :-
		learn(Pos1,Neg1,Prog),
		pprint(Prog).

	learn(Pos1,Neg1,Prog) :-
		maplist(atom_to_list,Pos1,Pos2),
		maplist(atom_to_list,Neg1,Neg2),
		proveall(Pos2,Sig,Prog),
		nproveall(Neg2,Sig,Prog),
		is_functional(Pos2,Sig,Prog).

	learn_seq(Seq,Prog) :-
		maplist(learn_task, Seq, Progs),
		flatten(Progs,Prog).

	learn_task(Pos/Neg,Prog) :-
		learn(Pos,Neg,Prog),!,
		maplist(assert_clause,Prog),
		assert_prog_prims(Prog).

	proveall(Atoms,Sig,Prog) :-
		target_predicate(Atoms,P/A),
		print_message(comment, metagol, learning_predicate(P/A)),
		iterator(MaxN),
		print_message(comment, metagol, number_of_clauses(MaxN)),
		invented_symbols(MaxN,P/A,Sig),
		prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

	prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
	prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		prove_deduce([Atom],FullSig,Prog1), !,
		is_functional([Atom],Sig,Prog1),
		prove_examples(Atoms,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
	prove_examples([Atom1|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		add_empty_path(Atom1,Atom2),
		prove([Atom2],FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
		prove_examples(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

	prove_deduce(Atoms1,Sig,Prog) :-
		maplist(add_empty_path,Atoms1,Atoms2),
		length(Prog,N),
		prove(Atoms2,Sig,_,N,N,N,Prog,Prog).

	prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
	prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
		prove(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

	prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
		!,
		::Atom.

	%% prove primitive atom
	prove_aux(p(prim,P,_A,Args,_,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
		::primcall(P,Args).

	%% use interpreted BK - can we skip this if no interpreted_bk?
	%% only works if interpreted/1 is before the corresponding definition
	prove_aux(p(inv,_P,_A,_Args,Atom,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		::interpreted_bk(Atom,Body1),
		add_path_to_body(Body1,[Atom|Path],Body2,_),
		prove(Body2,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

	%% use existing abduction
	prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
		select_lower(P,A,FullSig,Sig1,Sig2),
		member(sub(Name,P,A,MetaSub,PredTypes),Prog1),
		::metarule_init(Name,MetaSub,PredTypes,(Atom:-Body1),Recursive,[Atom|Path]),
		(	Recursive == true ->
			\+ member(Atom,Path)
		;	true
		),
		prove(Body1,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

	%% new abduction
	prove_aux(p(inv,P,A,_Args,Atom,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
		(	N1 == MaxN ->
			fail
		;	true
		),
		bind_lower(P,A,FullSig,Sig1,Sig2),
		::metarule(Name,MetaSub,PredTypes,(Atom:-Body1),FullSig,Recursive,[Atom|Path]),
		(	Recursive == true ->
			\+ member(Atom,Path)
		;	true
		),
		check_new_metasub(Name,P,A,MetaSub,Prog1),
		succ(N1,N3),
		prove(Body1,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,MetaSub,PredTypes)|Prog1],Prog2).

	add_empty_path([P|Args],p(inv,P,A,Args,[P|Args],[])) :-
		size(Args,A).

	select_lower(P,A,FullSig,_Sig1,Sig2) :-
		nonvar(P),!,
		append(_,[sym(P,A,_)|Sig2],FullSig), !.

	select_lower(P,A,_FullSig,Sig1,Sig2) :-
		append(_,[sym(P,A,U)|Sig2],Sig1),
		(	var(U) ->
			!,
			fail
		;	true
		).

	bind_lower(P,A,FullSig,_Sig1,Sig2) :-
		nonvar(P),
		!,
		append(_,[sym(P,A,_)|Sig2],FullSig), !.

	bind_lower(P,A,_FullSig,Sig1,Sig2) :-
		append(_,[sym(P,A,U)|Sig2],Sig1),
		(	var(U) ->
			U = 1,
			!
		;	true
		).

	check_new_metasub(Name,P,A,MetaSub,Prog) :-
		member(sub(Name,P,A,_,_),Prog),
		!,
		last(MetaSub,X),
		when(ground(X), \+ member(sub(Name,P,A,MetaSub,_), Prog)).
	check_new_metasub(_Name,_P,_A,_MetaSub,_Prog).

	size([],0) :-!.
	size([_],1) :-!.
	size([_,_],2) :-!.
	size([_,_,_],3) :-!.
	size(L,N) :- !,
		length(L,N).

	nproveall([],_PS,_Prog).
	nproveall([Atom|Atoms],PS,Prog) :-
		\+ prove_deduce([Atom],PS,Prog),
		nproveall(Atoms,PS,Prog).

	iterator(N) :-
		get_option(min_clauses(MinN)),
		get_option(max_clauses(MaxN)),
		between(MinN, MaxN, N).

	target_predicate([[P|Args]|_], P/A) :-
		length(Args,A).

	invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]) :-
		NumSymbols is MaxClauses - 1,
		get_option(max_inv_preds(MaxInvPreds)),
		M is min(NumSymbols,MaxInvPreds),
		findall(
			sym(InvSym,_Artiy,_Used),
			(	between(1, M, I),
				atom_concat(P, '_', InvSym0),
				number_codes(I, ICodes),
				atom_codes(IAtom, ICodes),
				atom_concat(InvSym0, IAtom, InvSym)
			),
			Sig
		).

	pclauses(Prog1,Prog6) :-
		map_list_to_pairs(arg(2),Prog1,Pairs),
		keysort(Pairs,Sorted),
		pairs_values(Sorted,Prog2),
		maplist(metasub_to_clause_list,Prog2,Prog3),
		(	get_option(unfold_program) ->
			unfold_program(Prog3,Prog4)
		;	Prog3 = Prog4
		),
		maplist(remove_orderings,Prog4,Prog5),
		maplist(clause_list_to_clause,Prog5,Prog6).

	pprint(Prog) :-
		pclauses(Prog, Clauses),
		pprint_clauses(Clauses).

	remove_orderings([],[]).
	remove_orderings(['@'(_H)|T],Out) :-
		!,
		remove_orderings(T,Out).
	remove_orderings([H|T],[H|Out]) :-
		remove_orderings(T,Out).

	pprint_clause(Clause) :-
		print_message(results, metagol, learned_clause(Clause)).

	pprint_clauses(Clauses) :-
		maplist(pprint_clause, Clauses).

	clause_list_to_clause([H|B1],Clause) :-
		list_to_atom(H,Head),
		(	B1 = [] ->
			Clause = Head
		;	maplist(list_to_atom,B1,B2),
			list_to_clause(B2,B3),
			Clause = (Head:-B3)
		).

	%% construct clause is horrible and needs refactoring
	metasub_to_clause_list(sub(Name,_,_,MetaSub,_),[HeadList|BodyAsList2]) :-
		::metarule_init(Name,MetaSub,_,(HeadList:-BodyAsList1),_,_),
		add_path_to_body(BodyAsList2,_,BodyAsList1,_).

	list_to_clause([Atom], Atom) :-
		!.
	list_to_clause([Atom|T1], (Atom,T2)) :-
		list_to_clause(T1, T2).

	list_to_atom(AtomList,Atom) :-
		Atom =.. AtomList.

	atom_to_list(Atom,AtomList) :-
		Atom =.. AtomList.

	is_functional(Atoms,Sig,Prog) :-
		(	get_option(functional) ->
			is_functional_aux(Atoms,Sig,Prog)
		;	true
		).
	is_functional_aux([],_Sig,_Prog).
	is_functional_aux([Atom|Atoms],Sig,Prog) :-
		::func_test(Atom,Sig,Prog),
		is_functional_aux(Atoms,Sig,Prog).

	get_option(Option) :-
		::Option,
		!.
	get_option(Option) :-
		default(Option).

	set_option(Option) :-
		functor(Option,Name,Arity),
		functor(Retract,Name,Arity),
		::retractall(Retract),
		::assertz(Option).

	gen_metarule_id(Id) :-
		get_option(metarule_next_id(Id)),
		succ(Id,IdNext),
		set_option(metarule_next_id(IdNext)).

	convert_to_interpreted((Head:-true), interpreted_bk(HeadAsList,[])) :-
		!,
		ho_atom_to_list(Head,HeadAsList).
	convert_to_interpreted((Head:-Body), interpreted_bk(HeadAsList,BodyList2)) :-
		ho_atom_to_list(Head,HeadAsList),
		clause_to_list(Body,BodyList1),
		maplist(ho_atom_to_list,BodyList1,BodyList2).

	term_expansion(
		(:- Directive),
		[	(:- Directive),
			% examples may require asserting new predicates during learning
			(:- set_logtalk_flag(dynamic_declarations, allow)),
			% currently term-expansion can generate discontiguous predicates
			(:- discontiguous([metarule/7, metarule_init/6, prim/1, primcall/2])),
			% learning may generate new clauses for the following predicates
			% which would have been compiled static if defioned in the examples
			(:- dynamic([prim/1, primcall/2]))
		]
	) :-
		functor(Directive, object, _),
		retractall(interpreted_(_)).

	term_expansion((:- end_object), _) :-
		retractall(interpreted_(_)),
		fail.

	term_expansion(interpreted(P/A), _) :-
		functor(Head,P,A),
		assertz(interpreted_(Head)),
		fail.

	term_expansion((Head:-Body), Converted) :-
		interpreted_(Head),
		convert_to_interpreted((Head:-Body), Converted).
	term_expansion(Head, Converted) :-
		interpreted_(Head),
		convert_to_interpreted((Head:-true), Converted).

	term_expansion(prim(P/A), [prim(P/A),(primcall(P,Args) :- Call)]) :-
		functor(Call,P,A),
		Call =.. [P|Args].

	term_expansion(metarule(MetaSub,Clause),Asserts) :-
		get_asserts(_Name,MetaSub,Clause,_,_PS,Asserts).
	term_expansion(metarule(Name,MetaSub,Clause),Asserts) :-
		get_asserts(Name,MetaSub,Clause,_,_PS,Asserts).
	term_expansion((metarule(MetaSub,Clause) :-Body),Asserts) :-
		get_asserts(_Name,MetaSub,Clause,Body,_PS,Asserts).
	term_expansion((metarule(Name,MetaSub,Clause) :-Body),Asserts) :-
		get_asserts(Name,MetaSub,Clause,Body,_PS,Asserts).
	term_expansion((metarule(Name,MetaSub,Clause,PS) :-Body),Asserts) :-
		get_asserts(Name,MetaSub,Clause,Body,PS,Asserts).

	get_asserts(
		Name,MetaSub,Clause1,MetaBody,PS,
		[MRule,metarule_init(AssertName,MetaSub,PredTypes,Clause2,Recursive,Path)]
	) :-
		Clause1 = (Head:-Body1),
		Head = [P|_],
		is_recursive(Body1,P,Recursive),
		add_path_to_body(Body1,Path,Body3,PredTypes),
		Clause2 = (Head:-Body3),
		(	var(Name) ->
			gen_metarule_id(AssertName)
		;	AssertName = Name
		),
		(	var(MetaBody) ->
			MRule = metarule(AssertName,MetaSub,PredTypes,Clause2,PS,Recursive,Path)
		;	MRule = (metarule(AssertName,MetaSub,PredTypes,Clause2,PS,Recursive,Path) :- MetaBody)
		).

	is_recursive([],_,false).
	is_recursive([[Q|_]|_],P,true) :-
		Q == P,
		!.
	is_recursive([_|T],P,Res) :-
		is_recursive(T,P,Res).

	add_path_to_body([],_Path,[],[]).
	add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest],Out) :-
		add_path_to_body(Atoms,Path,Rest,Out).
	add_path_to_body([[P|Args]|Atoms],Path,[p(PType,P,A,Args,[P|Args],Path)|Rest],[PType|Out]) :-
		size(Args,A),
		add_path_to_body(Atoms,Path,Rest,Out).

	assert_program(Prog) :-
		maplist(assert_clause, Prog).

	assert_clause(Sub) :-
		metasub_to_clause_list(Sub, ClauseAsList),
		clause_list_to_clause(ClauseAsList, Clause),
		::assertz(Clause).

	assert_prog_prims(Prog) :-
		findall(P/A, (member(sub(_Name,P,A,_MetaSub,_PredTypes),Prog)), Prims),
		list_to_set(Prims, PrimSet),
		maplist(assert_prim, PrimSet).

	assert_prim(Prim) :-
		prim_asserts(Prim, Asserts),
		maplist(assert_prim_aux, Asserts).

	assert_prim_aux(Assert) :-
		::assertz(Assert).

	retract_prim(Prim) :-
		Prim = P/_,
		::retractall(prim(Prim)),
		::retractall(primcall(P,_)).

	prim_asserts(P/A,[prim(P/A), (primcall(P,Args) :-Call)]) :-
		functor(Call,P,A),
		Call =.. [P|Args].

	clause_to_list((Atom,T1), [Atom|T2]) :-
		clause_to_list(T1,T2).
	clause_to_list(Atom, [Atom]) :- !.

	ho_atom_to_list(Atom,T) :-
		Atom =.. AtomList,
		AtomList = [call|T],
		!.
	ho_atom_to_list(Atom,AtomList) :-
		Atom=..AtomList.

	unfold_clause(C1,[[P|Args]|C2],P,D) :-
		append(Pre,[[P|Args]|Post],C1),
		append(Pre,C2,C_),
		append(C_,Post,D).

	head([H|_], H).
	head_pred([[P|_]|_],P).
	body_preds([_|T],Ps) :-
		findall(P, (member(X,T),head(X,P)), Ps).

	does_not_appear_twice(P, Prog) :-
		findall(
			Q,
			(	member(C, Prog),
				body_preds(C, Cs),
				member(Q, Cs)
			),
			Qs1
		),
		select(P,Qs1,Qs2),
		\+ member(P,Qs2).

	unfold_program(Prog1, Prog2) :-
		select(C1, Prog1, Prog3),
		head_pred(C1, P),
		% check that the head pred does not appear in a head elsewhere in the program
		\+ (member(X,Prog3), head_pred(X,P)),
		% check that the head pred does not appear in the body
		\+ (body_preds(C1,Ps), member(P,Ps)),
		% check that that head pred does not appear twice in the program
		does_not_appear_twice(P, Prog3),
		select(C2,Prog3,Prog4),
		body_preds(C2,C2Body),
		member(P,C2Body),
		unfold_clause(C2,C1,P,D),
		unfold_program([D|Prog4],Prog2).
	unfold_program(Prog,Prog) :- !.

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(learned_clause(Clause), metagol) -->
		{numbervars(Clause, 0, _)},
		['~q.'-[Clause], nl].
	logtalk::message_tokens(learning_predicate(Predicate), metagol) -->
		['learning ~q'-[Predicate], nl].
	logtalk::message_tokens(number_of_clauses(Clauses), metagol) -->
		['clauses: ~w'-[Clauses], nl].

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(comment, metagol, '% ', user_output).
	logtalk::message_prefix_stream(results, metagol, '',   user_output).

:- end_object.
