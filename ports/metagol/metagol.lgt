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
		version is 0:24:0,
		author is 'Metagol authors; adapted to Logtalk by Paulo Moura.',
		date is 2019-05-08,
		copyright is 'Copyright 2016 Metagol authors; Copyright 2018-2019 Paulo Moura',
		license is 'BSD 3-Clause License',
		comment is 'Inductive logic programming (ILP) system based on meta-interpretive learning.'
	]).

	:- public(learn/3).
	:- mode(learn(@list(example), @list(example), -list(term)), zero_or_more).
	:- info(learn/3, [
		comment is 'Learns from a set of positive examples and a set of negative examples and returns the learned program.',
		argnames is ['PositiveExamples', 'NegativeExamples', 'Program']
	]).

	:- public(learn/2).
	:- mode(learn(@list(example), @list(example)), zero_or_more).
	:- info(learn/2, [
		comment is 'Learns from a set of positive examples and a set of negative examples and pretty prints the learned program.',
		argnames is ['PositiveExamples', 'NegativeExamples']
	]).

	:- public(learn_seq/2).
	:- mode(learn_seq(@list(example), -list(clause)), zero_or_one).
	:- info(learn_seq/2, [
		comment is 'Learns from a sequence of examples represented as a list of PositiveExamples/NegativeExamples elements and returns the learned program.',
		argnames is ['Examples', 'Program']
	]).

	:- public(learn_with_timeout/4).
	:- mode(learn_with_timeout(@list(example), @list(example), -list(term), +number), zero_or_one).
	:- info(learn_with_timeout/4, [
		comment is 'Learns from a set of positive examples and a set of negative examples and returns the learned program.',
		argnames is ['PositiveExamples', 'NegativeExamples', 'Program', 'Timeout']
	]).

	:- public(program_to_clauses/2).
	:- mode(program_to_clauses(@list(term), -list(clause)), one).
	:- info(program_to_clauses/2, [
		comment is 'Converts a learned program into a list of clauses.',
		argnames is ['Program', 'Clauses']
	]).

	:- public(pprint/1).
	:- mode(pprint(@list(term)), one).
	:- info(pprint/1, [
		comment is 'Pretty prints a learned program.',
		argnames is ['Program']
	]).

	% example definition
	:- public([metarule/6, head_pred/1, body_pred/1, ibk/3, func_test/3]).
	:- dynamic([body_pred/1]).

	% example options
	:- public([functional/0, min_clauses/1, max_clauses/1, max_inv_preds/1, metarule_next_id/1, timeout/1]).
	:- dynamic([functional/0, min_clauses/1, max_clauses/1, max_inv_preds/1, metarule_next_id/1, timeout/1]).

	:- protected([pprint_clause/1, pprint_clauses/1, compiled_pred_call/2, body_pred_call/2, type/3]).
	:- dynamic([compiled_pred_call/2, body_pred_call/2, type/3]).

	:- uses(coroutining, [when/2]).
	:- uses(integer, [between/3, succ/2]).
	:- uses(list, [append/3, flatten/2, last/2, length/2, member/2, remove_duplicates/2 as list_to_set/2, reverse/2]).
	:- uses(logtalk, [print_message/3]).
	:- uses(meta, [include/3, maplist/2, maplist/3]).
	:- uses(pairs, [map/3 as map_list_to_pairs/3, values/2 as pairs_values/2]).
	:- uses(timeout, [call_with_timeout/2]).

	% defaults
	min_clauses(1).
	max_clauses(10).
	timeout(600). % 10 minutes
	metarule_next_id(0).
	max_inv_preds(9).

	learn(Pos, Neg) :-
		learn(Pos, Neg, Prog),
		pprint(Prog).

	learn(Pos1, Neg1, Prog) :-
		setup,
		make_atoms(Pos1, Pos2),
		make_atoms(Neg1, Neg2),
		proveall(Pos2, Sig, Prog),
		nproveall(Neg2, Sig, Prog),
		ground(Prog),
		check_functional(Pos2, Sig, Prog).
	learn(_,_,_) :-
		print_message(comment, metagol, unable_to_learn),
		fail.

	learn(Pos, Neg, Prog, Timeout) :-
		::timeout(Timeout),
		call_with_timeout(learn(Pos,Neg,Prog), Timeout).

	proveall(Atoms,Sig,Prog) :-
		target_predicate(Atoms,P/A),
		print_message(comment, metagol, learning_predicate(P/A)),
		iterator(MaxN),
		print_message(comment, metagol, number_of_clauses(MaxN)),
		invented_symbols(MaxN,P/A,Sig),
		assert_sig_types(Sig),
		prove_examples(Atoms,Sig,_Sig,MaxN,0,_N,[],Prog).

	prove_examples([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
	prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		deduce_atom(Atom,FullSig,Prog1),
		!,
		check_functional([Atom],Sig,Prog1),
		prove_examples(Atoms,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).
	prove_examples([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		prove([Atom],FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
		check_functional([Atom],Sig,Prog3),
		prove_examples(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

	deduce_atom(Atom,Sig,Prog) :-
		length(Prog,N),
		prove([Atom],Sig,_,N,N,N,Prog,Prog).

	prove([],_FullSig,_Sig,_MaxN,N,N,Prog,Prog).
	prove([Atom|Atoms],FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		prove_aux(Atom,FullSig,Sig,MaxN,N1,N3,Prog1,Prog3),
		prove(Atoms,FullSig,Sig,MaxN,N3,N2,Prog3,Prog2).

	prove_aux('@'(Atom),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
		!,
		::Atom.

	prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
		nonvar(P),
		::type(P,A,compiled_pred),
		!,
		::compiled_pred_call(P,Args).

	prove_aux(p(P,A,Args,_Path),_FullSig,_Sig,_MaxN,N,N,Prog,Prog) :-
		(nonvar(P) -> ::type(P,A,body_pred); true),
		::body_pred_call(P,Args).

	prove_aux(p(P,A,Args,Path),FullSig,Sig,MaxN,N1,N2,Prog1,Prog2) :-
		(var(P) -> true; (\+ ::type(P,A,head_pred), !, ::type(P,A,ibk_head_pred))),
		::ibk([P|Args],Body,Path),
		prove(Body,FullSig,Sig,MaxN,N1,N2,Prog1,Prog2).

	prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
		N1 \== 0,
		Atom = [P|Args],
		select_lower(P,A,FullSig,Sig1,Sig2),
		member(sub(Name,P,A,Subs),Prog1),
		::metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]),
		check_recursion(Recursive,MaxN,Atom,Path),
		prove(Body,FullSig,Sig2,MaxN,N1,N2,Prog1,Prog2).

	prove_aux(p(P,A,Args,Path),FullSig,Sig1,MaxN,N1,N2,Prog1,Prog2) :-
		N1 \== MaxN,
		Atom = [P|Args],
		bind_lower(P,A,FullSig,Sig1,Sig2),
		::metarule(Name,Subs,Atom,Body,Recursive,[Atom|Path]), % ??
		check_recursion(Recursive,MaxN,Atom,Path),
		check_new_metasub(Name,P,A,Subs,Prog1),
		succ(N1,N3),
		prove(Body,FullSig,Sig2,MaxN,N3,N2,[sub(Name,P,A,Subs)|Prog1],Prog2).

	nproveall(Atoms,Sig,Prog) :-
		forall(member(Atom,Atoms), \+ deduce_atom(Atom,Sig,Prog)).

	make_atoms(Atoms1,Atoms2) :-
		maplist(atom_to_list,Atoms1,Atoms3),
		maplist(make_atom,Atoms3,Atoms2).

	make_atom([P|Args], p(P,A,Args,[])) :-
		length(Args,A).

	check_functional(Atoms,Sig,Prog) :-
		(	::functional ->
			forall(
				member(Atom1,Atoms),
				\+ (
					make_atom(Atom2,Atom1),
					::func_test(Atom2,TestAtom2,Condition),
					make_atom(TestAtom2,TestAtom1),
					deduce_atom(TestAtom1,Sig,Prog),
					\+ call(Condition)
				)
			)
		;	true
		).

	check_recursion(false,_,_,_).
	check_recursion(true,MaxN,Atom,Path) :-
		MaxN \== 1, % need at least two clauses if we are using recursion
		\+ member(Atom,Path).

	select_lower(P,A,FullSig,_Sig1,Sig2) :-
		nonvar(P),
		!,
		append(_,[sym(P,A,_)|Sig2],FullSig), !.
	select_lower(P,A,_FullSig,Sig1,Sig2) :-
		append(_,[sym(P,A,U)|Sig2],Sig1),
		(var(U)-> !,fail;true ).

	bind_lower(P,A,FullSig,_Sig1,Sig2) :-
		nonvar(P),
		!,
		append(_,[sym(P,A,_)|Sig2],FullSig), !.
	bind_lower(P,A,_FullSig,Sig1,Sig2) :-
		append(_,[sym(P,A,U)|Sig2],Sig1),
		(var(U)-> U = 1,!; true).

	check_new_metasub(Name,P,A,Subs,Prog) :-
		member(sub(Name,P,A,_),Prog),
		!,
		last(Subs,X),
		when(ground(X), \+ member(sub(Name,_P,A,Subs),Prog)).
	check_new_metasub(_Name,_P,_A,_Subs,_Prog).

	assert_sig_types(Sig) :-
		forall(
			(member(sym(P,A,_),Sig), \+ ::type(P,A,head_pred)),
			::assertz(type(P,A,head_pred))
		).

	head_preds :-
		%% remove old invented predicates (not that it really matters)
		::retractall(type(_,_,head_pred)),
		forall(
			(::head_pred(P/A), \+ ::type(P,A,head_pred)),
			::assertz(type(P,A,head_pred))
		).

	body_preds :-
		::retractall(type(_,_,body_pred)),
		::retractall(body_pred_call(P,_)),
		findall(P/A, ::body_pred(P/A), S0),
		assert_body_preds(S0).

	assert_body_preds(S1) :-
		forall(
			member(P/A,S1),
			(	::retractall(type(P,A,body_pred)),
				::retractall(body_pred_call(P,_)),
				::retractall(body_pred(P/A)),
				(	::current_predicate(P/A) ->
					::assertz(type(P,A,body_pred)),
					::assertz(body_pred(P/A)),
					functor(Atom,P,A),
					Atom =.. [P|Args],
					::assertz((body_pred_call(P,Args) :- ::Atom))
				;	print_message(warning, metagol, missing_predicate(P/A))
				)
			)
		).

	ibk_head_preds :-
		findall(P/A, ::type(P,A,ibk_head_pred), S0),
		list_to_set(S0,S1),
		::retractall(type(_,_,ibk_head_pred)),
		forall(
			member(P/A,S1),
			::assertz(type(P,A,ibk_head_pred))
		).

	compiled_preds :-
		findall(
			P/A,
			(	::type(P,A,compiled_pred),
				\+ ::type(P,A,body_pred),
				\+ ::type(P,A,head_pred),
				\+ ::type(P,A,ibk_head_pred)
			),
			S0
		),
		list_to_set(S0,S1),
		::retractall(type(_,_,compiled_pred)),
		::retractall(compiled_pred_call(_,_)),
		forall(
			member(P/A,S1),
			(	::current_predicate(P/A) ->
				::assertz(type(P,A,compiled_pred)),
				functor(Atom,P,A),
				Atom =.. [P|Args],
				::assertz((compiled_pred_call(P,Args) :- ::Atom))
			;	print_message(warning, metagol, missing_predicate(P/A))
			)
		).

	setup :-
		head_preds,
		body_preds,
		ibk_head_preds,
		compiled_preds.

	iterator(N) :-
		::min_clauses(MinN),
		::max_clauses(MaxN),
		between(MinN, MaxN, N).

	target_predicate([p(P,A,_Args,[])|_],P/A).

	%% target_predicates(Atoms, Preds2) :-
	%%	findall(P/A, member([p(inv,P,A,_Args,_Atom,[])],Atoms), Preds1),
	%%	list_to_set(Preds1,Preds2).

	invented_symbols(MaxClauses,P/A,[sym(P,A,_U)|Sig]) :-
		NumSymbols is MaxClauses - 1,
		::max_inv_preds(MaxInvPreds),
		M is min(NumSymbols,MaxInvPreds),
		findall(
			sym(Sym1,_Arity,_Used1),
			(	between(1,M,I),
				atom_concat(P, '_', Sym0),
				number_codes(I, ICodes),
				atom_codes(IAtom, ICodes),
				atom_concat(Sym0, IAtom, Sym1)
			),
			Sig
		).

	program_to_clauses(Prog1, Prog2) :-
		reverse(Prog1, Prog3),
		maplist(metasub_to_clause, Prog3, Prog2).

	pprint(Prog) :-
		program_to_clauses(Prog, Clauses),
		pprint_clauses(Clauses).

	pprint_clause(Clause) :-
		print_message(results, metagol, learned_clause(Clause)).

	pprint_clauses(Clauses) :-
		maplist(pprint_clause, Clauses).

	metasub_to_clause(sub(Name,_,_,Subs),Clause2) :-
		::metarule(Name,Subs,HeadList,BodyAsList1,_,_),
		add_path_to_body(BodyAsList3,_,BodyAsList1),
		include(no_ordering,BodyAsList3,BodyAsList2),
		maplist(atom_to_list,ClauseAsList,[HeadList|BodyAsList2]),
		list_to_clause(ClauseAsList,Clause1),
		(	Clause1 = (H,T) ->
			Clause2=(H:-T)
		;	Clause2=Clause1
		).

	no_ordering(H) :-
		H \= '@'(_).

	list_to_clause([Atom], Atom) :-
		!.
	list_to_clause([Atom|T1], (Atom,T2)) :-
		list_to_clause(T1, T2).

	atom_to_list(Atom,AtomList) :-
		Atom =.. AtomList.

	set_option(Option) :-
		functor(Option, Name, Arity),
		functor(Retract, Name, Arity),
		::retractall(Retract),
		::assertz(Option).

	term_expansion(
		(:- Directive),
		[	(:- Directive),
			% examples may require asserting new predicates during learning
			(:- set_logtalk_flag(dynamic_declarations, allow)),
			% currently term-expansion can generate discontiguous predicates
			(:- discontiguous([metarule/6, ibk/2, ibk/3, type/3])),
			% learning may generate new clauses for the following predicates
			% which would have been compiled static if defined in the examples
			(:- dynamic([head_pred/1, body_pred/1, type/3]))
		]
	) :-
		functor(Directive, object, _).

	%% ensure calls to current_predicate/1 work
	term_expansion(body_pred(P/A), [(:- protected(P/A)), body_pred(P/A)]).

	%% build the internal metarule clauses
	term_expansion(metarule(Subs,Head,Body), Asserts) :-
		metarule_asserts(_Name,Subs,Head,Body,_MetaBody, Asserts).
	term_expansion(metarule(Name,Subs,Head,Body), Asserts) :-
		metarule_asserts(Name,Subs,Head,Body,_MetaBody, Asserts).
	term_expansion((metarule(Subs,Head,Body):-MetaBody), Asserts) :-
		metarule_asserts(_Name,Subs,Head,Body,MetaBody, Asserts).
	term_expansion((metarule(Name,Subs,Head,Body):-MetaBody), Asserts) :-
		metarule_asserts(Name,Subs,Head,Body,MetaBody, Asserts).

	term_expansion((ibk(Head,Body):-IbkBody), [(ibk(Head,Body):-IbkBody)| Types]) :-
		ibk_asserts(Head,Body,IbkBody,Types).

	term_expansion(ibk(Head,Body), [ibk(Head,Body)| Types]) :-
		ibk_asserts(Head,Body,false,Types).

	metarule_asserts(Name, Subs, Head, Body1, MetaBody, [MRule]) :-
		Head = [P|_],
		is_recursive(Body1,P,Recursive),
		add_path_to_body(Body1,Path,Body2),
		gen_metarule_id(Name,AssertName),
		%% very hacky - I assert that all ground body predicates are compiled
		%% I filter these in the setup call
		forall(
			(member(p(P1,A1,_,_),Body2), ground(P1)),
			::assertz(type(P1,A1,compiled_pred))
		),
		(	var(MetaBody) ->
			MRule = metarule(AssertName,Subs,Head,Body2,Recursive,Path)
		;	MRule = (metarule(AssertName,Subs,Head,Body2,Recursive,Path):-MetaBody)
		).

	ibk_asserts(Head, Body1, IbkBody, [type(P0,A0,ibk_head_pred), Type2| Types]) :-
		Head = [P0|Args1],
		length(Args1,A0),
		add_path_to_body(Body1,Path,Body2),
		(	IbkBody == false ->
			Type2 = ibk(Head,Body2,Path)
		;	Type2 = (ibk(Head,Body2,Path):-IbkBody)
		),
		%% very hacky - I assert that all ground body predicates are compiled
		%% I filter these in the setup call
		findall(
			type(P1,A1,compiled_pred),
			(member(p(P1,A1,_,_),Body2), ground(P1)),
			Types
		).

	is_recursive([],_,false).
	is_recursive([[Q|_]|_],P,true) :-
		Q == P,
		!.
	is_recursive([_|T],P,Res) :-
		is_recursive(T,P,Res).

	add_path_to_body([],_Path,[]).
	add_path_to_body(['@'(Atom)|Atoms],Path,['@'(Atom)|Rest]) :-
		add_path_to_body(Atoms,Path,Rest).
	add_path_to_body([[P|Args]|Atoms],Path,[p(P,A,Args,Path)|Rest]) :-
		length(Args,A),
		add_path_to_body(Atoms,Path,Rest).

	gen_metarule_id(Name, Name) :-
		ground(Name),
		!.
	gen_metarule_id(_Name, IdNext) :-
		::metarule_next_id(Id),
		succ(Id, IdNext),
		set_option(metarule_next_id(IdNext)).

	%% learns a sequence of programs and asserts each program that it learns
	learn_seq(Seq, Prog) :-
		maplist(learn_task, Seq, Progs),
		flatten(Progs, Prog).

	learn_task(Pos/Neg, Prog1) :-
		learn(Pos, Neg, Prog1), !,
		maplist(metasub_to_clause,Prog1,Prog2),
		forall(member(Clause,Prog2), ::assertz(Clause)),
		findall(P/A, (member(sub(_Name,P,A,_Subs),Prog1)), Preds),
		!,
		assert_body_preds(Preds).
	learn_task(_, []).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(learned_clause(Clause), metagol) -->
		{copy_term(Clause, Copy), numbervars(Copy, 0, _)},
		['~q.'-[Copy], nl].
	logtalk::message_tokens(learning_predicate(Predicate), metagol) -->
		['learning ~q'-[Predicate], nl].
	logtalk::message_tokens(number_of_clauses(Clauses), metagol) -->
		['clauses: ~w'-[Clauses], nl].
	logtalk::message_tokens(missing_predicate(Predicate), metagol) -->
		['~w does not exist'-[Predicate], nl].
	logtalk::message_tokens(unable_to_learn, metagol) -->
		['unable to learn a solution'-[], nl].

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(comment, metagol, '% ', user_output).
	logtalk::message_prefix_stream(results, metagol, '',   user_output).

:- end_object.
