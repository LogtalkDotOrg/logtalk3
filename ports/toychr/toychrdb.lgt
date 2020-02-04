/* toychrdb 3.1 -- simple CHR interpreter/debugger based on the refined
                   operational semantics of CHRs
   Copright (C) 2004 Gregory J. Duck

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TOYCHRDB INTERPRETER/DEBUGGER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DESCRIPTION:
%       This is a simple CHR interpreter which is, unlike Chameleon, based on
%       the refined operational semantics of CHRs.  It also supports a
%       primative trace debugger.
%
%       Unlike the original toychr, the interpreter implements all of the
%       refined operational semantics, including waking up constraints on
%       Solve.
%
% NOTES:
%       (1) Cannot use toychr and toychrdb at the same time.
%       (2) toychrdb, unlike toychr, only works with SWIProlog.
%       (3) If your program uses tell/1, then debugging won't work.
%
% USAGE:
%       chr_compile(+File),
%       -Result chr_is +Goal.
%
% Where `Goal' is your query, `File' is the source file containing the CHR
% program and `Result' is the result of the query.
%
% DEBUGGING:
%       chr_trace
%               Like Prolog's trace/0.
%
%       chr_notrace
%               Like Prolog's notrace/0.
%
%       chr_spy(C)
%               Sets a spy point on a constraint of form C being activated.
%
%       chr_nospy
%               Disables all spy points.
%
%       chr_no_spy(C)
%               TODO: not implemented!
%
% OPTIONS:
%       chr_option(optimization_level,+N)
%               Sets optimization level to integer N.  Higher numbers should
%               be faster (note "should").  (default: all optimizations on)
%
%               N = 0: -) all optimizations off.
%
%               N = 1: -) No history if rule is not a propagation rule.
%                      -) Wakeup only on builtin constraints that bind
%                         variables.
%
%               N = 2: -) Delete entries from history where an id is dead.
%                      -) Wakeup only the constraints affected by a binding.
%                      -) Drop will fire if active constraint is not in the
%                      store.
%
%       chr_option(show_(stack|store|history|id),(yes|no|toggle))
%               When tracing, show the execution stack, store, history or
%               current ID. (default: show store only)
%
%       chr_option(allow_deep_guards,(yes|no|toggle))
%               Allow CHR constraints in rule guards or if-conditions.
%               Operationally, a sub-derivation is spawned with the guard as
%               the goal.  The guard is deemed to succeed if the
%               sub-derivation terminates without failure. (default: off)
%
% TODO:
%       -) Fix the problems listed in `NOTES'
%       -) Implement matching better.
%
% DISCLAIMER:
%       Do not use toychrdb to judge my Prolog programming skills!
%       Thankyou!
%---------------------------------------------------------------------------%


:- op(1180, xfx, ==>).
:- op(1180, xfx, <=>).
:- op(1150, fx, constraints).
:- op(1150, fx, chr_constraint).
:- op(1150, fx, handler).
:- op(1150, fx, rules).
:- op(1100, xfx, \).
:- op(1200, xfx, @).


:- object(toychrdb,
	implements(expanding)).

	:- info([
		version is 0:4:0,
		author is 'Gregory J. Duck; adapted to Logtalk by Paulo Moura.',
		date is 2019-10-14,
		copyright is 'Copright 2004 Gregory J. Duck; Copyright 2019 Paulo Moura',
		license is 'GNU GPL 2.0 or later version',
		comment is 'Simple CHR interpreter/debugger based on the refined operational semantics of CHRs.'
	]).

	:- public([
		chr_is/2,
		chr_trace/0, chr_notrace/0, chr_spy/1, chr_nospy/0, chr_no_spy/1, chr_option/2
	]).

	:- protected(current_prog/1).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
	]).

	:- uses(user, [
		abort/0
	]).

	chr_is(Result,Goal) :-
		(	::current_prog(P) ->
			parse_bodies(Goal,Goal0),
			run_goal(Goal0,P,R),
			state_to_result(R,Result)
		;	write('Warning: no CHR program present.\n'),
			Result = Goal
		).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% COMPILER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% This is a modified version of toyCHR's compiler.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% changed to use Logtalk term-expansion mechanism

	:- private(chr_rule_/1).
	:- dynamic(chr_rule_/1).

	term_expansion('@'(Name,Rule), []) :-
		assertz(chr_rule_('@'(Name,Rule))).
	term_expansion('<=>'(Head,GuardBody), []) :-
		assertz(chr_rule_('<=>'(Head,GuardBody))).
	term_expansion('==>'(Head,GuardBody), []) :-
		assertz(chr_rule_('==>'(Head,GuardBody))).

	term_expansion((:- end_object), [current_prog(Prog), (:- end_object)]) :-
		findall(Rule, retract(chr_rule_(Rule)), Rules),
		parse_rules(Rules,Prog).
	term_expansion((:- end_category), [current_prog(Prog), (:- end_category)]) :-
		findall(Rule, retract(chr_rule_(Rule)), Rules),
		parse_rules(Rules,Prog).

	parse_rules(Rules, Prog) :-
		parse_rule_list(Rules, 0, _, [], Prog).

	parse_rule_list([], NN, NN, Prog, Prog).
	parse_rule_list([Rule| Rules], N, NN, Prog0, Prog) :-
		(	parse_rule(Rule,N,N0,Prog0,Prog1) ->
			parse_rule_list(Rules,N0,NN,Prog1,Prog)
		;	parse_rule_list(Rules,N,NN,Prog0,Prog)
		).

	parse_rule(Term,N,NN,Prog0,Prog1) :-
		(	Term = '@'(Name,Rule) ->
			parse_rule2(Rule,Name,N,NN,Prog0,Prog1)
		;	number_codes(N, Codes),
			atom_codes(Name, Codes),
			M is N + 1,
			parse_rule2(Term,Name,M,NN,Prog0,Prog1)
		).

	parse_rule2('<=>'(Head,GuardBody),Name,N,NN,Prog0,Prog1) :-
		parse_guard_body(GuardBody,Guard,Body),
		parse_heads(Head,simp,Name,Guard,Body,N,NN,Prog0,Prog1).
	parse_rule2('==>'(Head,GuardBody),Name,N,NN,Prog0,Prog1) :-
		parse_guard_body(GuardBody,Guard,Body),
		parse_heads(Head,prop,Name,Guard,Body,N,NN,Prog0,Prog1).

	parse_heads(Heads,Type,Name,Guard,Body,N,NN,Prog0,Prog2) :-
		(	Heads = (HeadRemain \ HeadKill) ->
			Type = simp,
			parse_terms(HeadRemain,Remain),
			parse_terms(HeadKill,Kill),
			heads2prog(Kill,[],kill,Remain,Name,Guard,Body,N,N0,Prog0,Prog1),
			heads2prog(Remain,[],remain,Kill,Name,Guard,Body,N0,NN,Prog1,Prog2)
		;	parse_terms(Heads,Head),
			(	Type = simp ->
				heads2prog(Head,[],kill,[],Name,Guard,Body,N,NN,Prog0,Prog2)
			;	heads2prog(Head,[],remain,[],Name,Guard,Body,N,NN,Prog0,Prog2)
			)
		).

	parse_guard_body(GuardBody,Guard,Body) :-
		(	GuardBody = '|'(Guard0,Body0) ->
			Guard = Guard0,
			parse_bodies(Body0,Body)
		;	GuardBody = ';'(Guard0,Body0), Guard0 \= '->'(_,_) ->      % For Sicstus
			Guard = Guard0,
			parse_bodies(Body0,Body)
		;	Guard = true,
			parse_bodies(GuardBody,Body)
		).

	parse_terms(Term,Terms) :-
		(	Term = ','(Term0,Term1) ->
			parse_terms(Term0,Terms0),
			parse_terms(Term1,Terms1),
			my_append(Terms0,Terms1,Terms)
		;	Terms = [Term]
		).

	parse_bodies(Term,Terms) :-
		(	Term = ','(Term0,Term1) ->
			parse_bodies(Term0,Terms0),
			parse_bodies(Term1,Terms1),
			my_append(Terms0,Terms1,Terms)
		;	Term = (Term0 -> Term1 ; Term2) ->
			parse_bodies(Term1,Terms1),
			parse_bodies(Term2,Terms2),
			Terms = [if_then_else(Term0,Terms1,Terms2)]
		;	Terms = [Term]
		).

	heads2prog([],_,_,_,_,_,_,N,N,Prog,Prog).
	heads2prog([Head|Heads],Seen,Type,Other,Name,Guard,Body,N,NN,Prog0,Prog2) :-
		(	Type == kill ->
			reverse([active|Seen],SeenRev),
			append(SeenRev,Heads,Kill),
			Occ = kill(Name,Head,Other,Kill,Guard,Body)
		;	reverse([active|Seen],SeenRev),
			append(SeenRev,Heads,Remain),
			Occ = remain(Name,Head,Remain,Other,Guard,Body)
		),
		functor(Head,F,A),
		prog_insert(F/A,Occ,Prog0,Prog1),
		M is N + 1,
		heads2prog(Heads,[Head|Seen],Type,Other,Name,Guard,Body,M,NN,Prog1,Prog2).

	prog_insert(CId,Occ,[],[occs(CId,[Occ])]).
	prog_insert(CId0,Occ,[Occs0|Prog0],[Occs1|Prog1]) :-
		Occs0 = occs(CId1,COccs0),
		(	CId0 = CId1 ->
			my_append(COccs0,[Occ],COccs1),
			Occs1 = occs(CId1,COccs1),
			Prog1 = Prog0
		;	Occs1 = Occs0,
			prog_insert(CId0,Occ,Prog0,Prog1)
		).

	my_append([],Bs,Bs).
	my_append([A|As],Bs,[A|Cs]) :-
		my_append(As,Bs,Cs).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% INTERPRETER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	run_goal(G,P,R) :-
		save_chr_options(Opts),
		state(G,S0),
		(	derivation(S0,P,R) ->
			clear_next,
			restore_chr_options(Opts)
		;	clear_next,
			restore_chr_options(Opts),
			fail
		).

	derivation(S0,P,R) :-
		(	final_state(S0) ->
			R = S0
		;	transition(P,S0,S1),
			derivation(S1,P,R)
		).

	state(G,state(G,[],[],1)).

	final_state(state([],_,_,_)).

	%Solve
	transition(P,St0,St1) :-
		St0 = state([C|A],S,T,N),
		(	C = if_then_else(Cd,Th,El) ->
			!,
			check_next([C|A]),
			print_trace('SOLVE      ',no,St0),
			(	test_guard(Cd,P) ->
				append(Th,A,A1)
			;	append(El,A,A1)
			)
		;	predicate_property(C, built_in),
			!,
			check_next([C|A]),
			print_trace('SOLVE      ',no,St0),
			get_wakeups(C,S,S1),
			call(C),
			append(S1,A,A1)
		),
		St1 = state(A1,S,T,N).

	%Activate
	transition(_,St0,St1) :-
		St0 = state([C|A],S,T,N0),
		chr_constraint(C),
		!,
		check_for_spy_point(C),
		print_trace('ACTIVATE   ',yes,St0),
		N1 is N0 + 1,
		St1 = state([act(C,N0,1)|A],[num(C,N0)|S],T,N1).

	%Reactivate
	transition(_,St0,St1) :-
		St0 = state([num(C,I)|A],S,T,N),
		!,
		check_for_spy_point(C),
		print_trace('REACTIVATE ',yes,St0),
		St1 = state([act(C,I,1)|A],S,T,N).

	%Drop
	transition(P,St0,St1) :-
		St0 = state(A0,S,T,N),
		A0 = [act(C,I,J)|A],
		check_drop(P,C,J,I,S),
		!,
		check_next(A0),
		print_trace('DROP       ',no,St0),
		St1 = state(A,S,T,N).

	%Simplify
	transition(P,St0,St1) :-
		St0 = state(A0,S,T,N),
		A0 = [act(C,I,J)|A],
		occurrence(C,J,P,Occ),
		Occ = kill(_,Active,Remain,Kill,_,_),
		term_match(Active,C),
		delete(I,S,S1,_),
		match(Kill,S1,MKill,S2,I,IdsK),
		match(Remain,S2,MRemain,_,I,IdsR),
		term_match(triple(Active,Remain,Kill),triple(C,MRemain,MKill)),
		copy_term(Occ,ROcc),
		ROcc = kill(RId,RActive,RRemain,RKill,RGuard,RBody),
		C = RActive,
		MKill = RKill,
		MRemain = RRemain,
		test_guard(RGuard,P),
		history_check(RId,IdsR,IdsK,Kill,T,T1),
		!,
		check_next(A0),
		print_trace('SIMPLIFY   ',no,St0),
		clean_history(T1,I,T2),
		append(RBody,A,A1),
		St1 = state(A1,S2,T2,N).

	%Propagate
	transition(P,St0,St1) :-
		St0 = state(A0,S,T,N),
		A0 = [act(C,I,J)|_],
		occurrence(C,J,P,Occ),
		Occ = remain(_,Active,Remain,Kill,_,_),
		term_match(Active,C),
		delete(I,S,S1,NC),
		match(Kill,S1,MKill,S2,I,IdsK),
		match(Remain,S2,MRemain,_,I,IdsR),
		term_match(triple(Active,Remain,Kill),triple(C,MRemain,MKill)),
		copy_term(Occ,ROcc),
		ROcc = remain(RId,RActive,RRemain,RKill,RGuard,RBody),
		C = RActive,
		MKill = RKill,
		MRemain = RRemain,
		test_guard(RGuard,P),
		history_check(RId,IdsR,IdsK,Kill,T,T1),
		!,
		print_trace('PROPAGATE  ',yes,St0),
		append(RBody,A0,A1),
		St1 = state(A1,[NC|S2],T1,N).

	%Default
	transition(_,St0,St1) :-
		St0 = state([act(C,I,J)|A],S,T,N),
		!,
		print_trace('DEFAULT    ',yes,St0),
		K is J + 1,
		St1 = state([act(C,I,K)|A],S,T,N).

	get_nonground([],[]).
	get_nonground([C|S],S1) :-
		(	ground(C) ->
			get_nonground(S,S1)
		;	S1 = [C|S0],
			get_nonground(S,S0)
		).

	need_wakeup(_ = _).
	need_wakeup(_ is _).
	need_wakeup(arg(_,_,_)).
	need_wakeup(call(_)).
	need_wakeup(compare(_,_,_)).
	need_wakeup(functor(_,_,_)).
	need_wakeup(get_byte(_)).
	need_wakeup(get_byte(_,_)).
	need_wakeup(get_char(_)).
	need_wakeup(get_char(_,_)).
	need_wakeup(get_code(_)).
	need_wakeup(get_code(_,_)).
	need_wakeup(length(_,_)).
	need_wakeup(name(_,_)).
	need_wakeup(peek_byte(_)).
	need_wakeup(peek_byte(_,_)).
	need_wakeup(peek_char(_)).
	need_wakeup(peek_char(_,_)).
	need_wakeup(peek_code(_)).
	need_wakeup(peek_code(_,_)).
	need_wakeup(read(_)).
	need_wakeup(read(_,_)).
	need_wakeup(read_term(_,_)).
	need_wakeup(read_term(_,_,_)).
	need_wakeup(statistics(_,_)).
	need_wakeup(sub_atom(_,_,_,_,_)).
	need_wakeup(append(_,_,_)).
	need_wakeup(member(_,_)).

	get_wakeups(C,S,S1) :-
		(	check_optimization_level(2) ->
			term_variables(C,Vs),
			(	Vs = [] ->
				S1 = []
			;	get_with_vars(Vs,S,S1)
			)
		;	check_optimization_level(1) ->
			(	need_wakeup(C) ->
				get_nonground(S,S1)
			;	S1 = []
			)
			;	get_nonground(S,S1)
		).

	get_with_vars(_,[],[]).
	get_with_vars(Vs,[C|Cs],Cs1) :-
		term_variables(C,Vs1),
		(	sharing(Vs,Vs1) ->
			Cs1 = [C|Cs2],
			get_with_vars(Vs,Cs,Cs2)
		;	get_with_vars(Vs,Cs,Cs1)
		).

	sharing([V|Vs1],Vs2) :-
		(	var_member(V,Vs2) ->
			true
		;	sharing(Vs1,Vs2)
		).

	var_member(V,[V1|Vs]) :-
		(	V == V1 ->
			true
		;	var_member(V,Vs)
		).

	chr_constraint(C) :-
		C \= num(_,_),
		C \= act(_,_,_).

	check_drop(P,C,J,I,S) :-
		(	pred_symb(C,CP),
			\+ get_occ(CP,P,J,_) ->
			true
		;	check_optimization_level(2),
			\+ delete(I,S,_,_)
		).

	occurrence(C,J,P,Occ) :-
		pred_symb(C,CP),
		get_occ(CP,P,J,Occ).

	pred_symb(C,CP) :-
		functor(C,F,A),
		CP = F/A.

	get_occ(CP,[occs(CP1,Occs)|P],J,Occ) :-
		(	CP = CP1 ->
			get_occ(J,Occs,Occ)
		;	get_occ(CP,P,J,Occ)
		).

	get_occ(J,[Occ0|Occs],Occ) :-
		(	J = 1 ->
			Occ = Occ0
		;	K is J - 1,
			get_occ(K,Occs,Occ)
		).

	match([],S,[],S,_,[]).
	match([C|Cs],S,[MC|MCs],SR,AI,[I|Ids]) :-
		(	C == active ->
			I = AI,
			MC = C,
			match(Cs,S,MCs,SR,AI,Ids)
		;	match1(C,S,MC,SR0,I),
			match(Cs,SR0,MCs,SR,AI,Ids)
		).

	match1(C,[C1|S],MC,SR,I) :-
		C1 = num(C2,I0),
		(	term_match(C,C2) ->
			(	MC = C2,
				I = I0,
				SR = S
			;	SR = [C1|SR0],
				match1(C,S,MC,SR0,I)
			)
		;	SR = [C1|SR0],
			match1(C,S,MC,SR0,I)
		).

	term_match(HT, T) :-
		\+ \+ (
			term_variables(T,TVars),
			unify_with_occurs_check(HT,T),
			term_variables(TVars,TVars1),
			TVars == TVars1
		).

	delete(I,[C|S1],S2,NC) :-
		C = num(_,J),
		(	I = J ->
			S2 = S1,
			NC = C
		;	S2 = [C|S3],
			delete(I,S1,S3,NC)
		).

	history_check(RId,IdsR,IdsK,Kill,T0,T1) :-
		(	Kill = [] ->
			E = [RId|IdsR],
			\+ member(E,T0),
			T1 = [E|T0]
		;	check_optimization_level(1) ->
			T1 = T0
		;	append([RId|IdsR],IdsK,E),
			\+ member(E,T0),
			T1 = [E|T0]
		).

	clean_history(T0,I,T1) :-
		(	check_optimization_level(2) ->
			do_clean_history(T0,I,T1)
		;	T1 = T0
		).

	do_clean_history([],_,[]).
	do_clean_history([E|T],I,T1) :-
		(	member(I,E) ->
			do_clean_history(T,I,T1)
		;	T1 = [E|T2],
			do_clean_history(T,I,T2)
		).

	:- meta_predicate(test_guard(*,*)).
	test_guard(G,P) :-
		(	chr_option_allow_deep_guards ->
			parse_bodies(G,Gs),
			state(Gs,State),
			(	chr_next_state(L) ->
				clear_next,
				(	derivation(State,P,_) ->
					assertz(chr_next_state(L))
				;	assertz(chr_next_state(L)),
					fail
				)
			;	derivation(State,P,_)
			)
		;	call(G)
		).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% DEBUGGER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- protected(chr_option_print_trace/0).
	:- dynamic(chr_option_print_trace/0).

	:- protected(chr_option_trace_interactive/0).
	:- dynamic(chr_option_trace_interactive/0).

	:- protected(chr_option_optimization_level/1).
	:- dynamic(chr_option_optimization_level/1).

	:- protected(chr_option_show_stack/0).
	:- dynamic(chr_option_show_stack/0).

	:- protected(chr_option_show_store/0).
	:- dynamic(chr_option_show_store/0).

	:- protected(chr_option_show_history/0).
	:- dynamic(chr_option_show_history/0).

	:- protected(chr_option_show_id/0).
	:- dynamic(chr_option_show_id/0).

	:- protected(chr_option_allow_deep_guards/0).
	:- dynamic(chr_option_allow_deep_guards/0).

	chr_option_show_store.

	chr_option(trace,N) :-
		do_option(chr_option_print_trace,N).
	chr_option(interactive,N) :-
		do_option(chr_option_trace_interactive,N).
	chr_option(show_stack,N) :-
		do_option(chr_option_show_stack,N).
	chr_option(show_store,N) :-
		do_option(chr_option_show_store,N).
	chr_option(show_history,N) :-
		do_option(chr_option_show_history,N).
	chr_option(show_id,N) :-
		do_option(chr_option_show_id,N).
	chr_option(optimization_level,N) :-
		(	integer(N) ->
			::retractall(chr_option_optimization_level(_)),
			::assertz(chr_option_optimization_level(N))
		;	true
		).
	chr_option(allow_deep_guards,N) :-
		do_option(chr_option_allow_deep_guards,N).

	do_option(Pred,YN) :-
		(	YN == toggle ->
			(	::Pred ->
				::retractall(Pred)
			;	::assertz(Pred)
			)
		;	YN == yes ->
			(	::Pred ->
				true
			;	::assertz(Pred)
			)
		;	YN == no ->
			::retractall(Pred)
		;	true
		).

	check_optimization_level(N) :-
		(	::chr_option_optimization_level(M) ->
			N =< M
		;	true
		).

	save_chr_options(opts(Tr,Int,SA,SS,ST,SN)) :-
		(	::chr_option_print_trace ->
			Tr = yes
		;	Tr = no
		),
		(	::chr_option_trace_interactive ->
			Int = yes
		;	Int = no
		),
		(	::chr_option_show_stack ->
			SA = yes
		;	SA = no
		),
		(	::chr_option_show_store ->
			SS = yes
		;	SS = no
		),
		(	::chr_option_show_history ->
			ST = yes
		;	ST = no
		),
		(	::chr_option_show_id ->
			SN = yes
		;	SN = no
		).

	restore_chr_options(opts(Tr,Int,SA,SS,ST,SN)) :-
		chr_option(trace,Tr),
		chr_option(interactive,Int),
		chr_option(show_stack,SA),
		chr_option(show_store,SS),
		chr_option(show_history,ST),
		chr_option(show_id,SN).

	:- protected(chr_next_state/1).
	:- dynamic(chr_next_state/1).

	do_next(state(A,_,_,_)) :-
		::retractall(chr_next_state(_)),
		length(A,L),
		::assertz(chr_next_state(L)),
		chr_option(trace,no).

	check_next(A) :-
		(	::chr_next_state(L0) ->
			length(A,L),
			(	L == L0 ->
				::retractall(chr_next_state(_)),
				chr_option(trace,yes),
				chr_option(interactive,yes)
			;	true
			)
		;	true
		).

	clear_next :-
		::retractall(chr_next_state(_)).

	print_trace(Trans,Next,State) :-
		(	::chr_option_print_trace ->
			nl, write(Trans), write(': '),
			print_state(State),
			(	::chr_option_trace_interactive ->
				write('(Action : ? for help) '),
				read_single_char(Comm),
				(	Comm == '\n' ->
					write('step\n')
				;	Comm == '\r' ->
					write('step\n')
				;	Comm == 'n' ->
					write('next\n'),
					(	Next = yes ->
						do_next(State)
					;	print_trace(Trans,Next,State)
					)
				;	Comm == 'f' ->
					write('fail\n'),
					fail
				;	Comm == 'a' ->
					write('abort\n'),
					abort
				;	Comm == 'l' ->
					write('leap\n'),
					chr_option(trace,no)
				;	Comm == 'c' ->
					write('continue\n'),
					chr_option(interactive,no)
				;	Comm == 'r' ->
					write('rule\n'),
					print_rule(State),
					print_trace(Trans,Next,State)
				;	Comm == 'h' ->
					write('history\n{'),
					State = state(_,_,T,_),
					print_history(T),
					write('}\n'),
					print_trace(Trans,Next,State)
				;	Comm == 'H' ->
					write('History\n'),
					chr_option(show_history,toggle),
					print_trace(Trans,Next,State)
				;	Comm == 'e' ->
					write('execution stack\n['),
					State = state(A,_,_,_),
					print_stack(A),
					write(']\n'),
					print_trace(Trans,Next,State)
				;	Comm == 'E' ->
					write('Execution stack\n'),
					chr_option(show_stack,toggle),
					print_trace(Trans,Next,State)
				;	Comm == 's' ->
					write('store\n{'),
					State = state(_,S,_,_),
					print_store(S),
					write('}\n'),
					print_trace(Trans,Next,State)
				;	Comm == 'S' ->
					write('Store\n'),
					chr_option(show_store,toggle),
					print_trace(Trans,Next,State)
				;	Comm == 'i' ->
					State = state(_,_,_,N),
					write('id'), nl, write(N), nl,
					print_trace(Trans,Next,State)
				;	Comm == 'I' ->
					write('Id\n'),
					chr_option(show_id,toggle),
					print_trace(Trans,Next,State)
				;	Comm == 'y' ->
					write('spy\n'),
					write('Set spy point on CHR constraint: '),
					read(Pattern),
					chr_spy(Pattern),
					print_trace(Trans,Next,State)
				;	Comm == ('?') ->
					write('\n(enter)\tstep (apply transition)\n'),
					write('n\tnext (finish executing current constraint)\n'),
					write('f\tfail (cause derivation to fail)\n'),
					write('a\tabort (aborts derivation)\n'),
					write('l\tleap (silently continue derivation)\n'),
					write('c\tcontinue (continue dervation)\n'),
					write('y\tspy (set spy point)\n'),
					write('r\trule (print current rule)\n'),
					write('e\texecution stack (print the execution stack)\n'),
					write('E\tExecution stack (toggles show stack mode)\n'),
					write('s\tstore (print the CHR store)\n'),
					write('S\tStore (toggles show store mode)\n'),
					write('h\thistory (print the propagation history)\n'),
					write('H\tHistory (toggles show history mode)\n'),
					write('i\tid (prints next free ID number)\n'),
					write('I\tId (toggles show ID mode)\n'),
					write('?\thelp (print this message)\n'),
					print_trace(Trans,Next,State)
				;	write('undefined command "'), write(Comm), write('", try ? for help.\n'),
					print_trace(Trans,Next,State)
				)
			;	nl
			)
		;	true
		).

	print_state(state([C|A],S,T,N)) :-
		write('('),
		print_cons(C),
		write(')\n'),
		(	::chr_option_show_stack ->
			write('stack = ['),
			print_stack([C|A]),
			write(']\n')
		;	true
		),
		(	::chr_option_show_store ->
			write('store = {'),
			print_store(S),
			write('}\n')
		;	true
		),
		(	::chr_option_show_history ->
			write('history = {'),
			print_history(T),
			write('}\n')
		;	true
		),
		(	::chr_option_show_id ->
			write('id = '), write(N), nl
		;	true
		).

	print_store([]).
	print_store([num(C,I)|S]) :-
		write(C), write('#'), write(I),
		(	S = [] ->
			true
		;	write(','),
			print_store(S)
		).

	print_stack([]).
	print_stack([C|A]) :-
		print_cons(C),
		(	A = [] ->
			true
		;	write(','),
			print_stack(A)
		).

	print_history([]).
	print_history([E|T]) :-
		write('('),
		print_stack(E),
		write(')'),
		(	T = [] ->
			true
		;	write(','),
			print_history(T)
		).

	print_cons(C) :-
		(	C = act(C0,I,J) ->
			write(C0), write('#'), write(I), write(':'), write(J)
		;	C = num(C0,I) ->
			write(C0), write('#'), write(I)
		;	C = if_then_else(C0,T,E) ->
			write('('), write(C0), write('->'),
			print_stack(T),
			write(';'),
			print_stack(E),
			write(')')
		;	write(C)
		).

	print_rule(St) :-
		(	St = state([act(C,_,J)|_],_,_,_),
			::current_prog(P),
			occurrence(C,J,P,Occ) ->
			(	Occ = kill(Name,Active,Remain,Kill,Guard,Body) ->
				print_rule(Name,Active,Remain,Kill,Guard,Body)
			;	Occ = remain(Name,Active,Remain,Kill,Guard,Body),
				print_rule(Name,Active,Remain,Kill,Guard,Body)
			)
		;	write('No rule associated with current state.\n')
		).

	print_rule(N,A,R,K,G,B) :-
		write(N), write(' @ '),
		(	R = [] ->
			print_heads(K,A),
			write(' <=> ')
		;	print_heads(R,A),
			(	K = [] ->
				write(' ==> ')
			;	write(' \\ '),
				print_heads(K,A),
				write(' <=> ')
			)
		),
		write(G), write(' | '),
		print_stack(B),
		write('.\n').

	print_heads([],_).
	print_heads([H|Hs],A) :-
		(	H = active ->
			write('(active) '),
			print_cons(A)
		;	print_cons(H)),
		(	Hs = [] ->
			true
		;	write(', '),
			print_heads(Hs,A)
		).

	state_to_result(state(_,S,_,_),R) :-
		store_to_result(S,R).

	store_to_result([],true).
	store_to_result([num(C,_)|S],R) :-
		(	S = [] ->
			R = C
		;	R = (C , R0),
			store_to_result(S,R0)
		).

	:- protected(chr_spy_point/1).
	:- dynamic(chr_spy_point/1).

	chr_nospy :-
		::retractall(chr_spy_point(_)).

	chr_spy(Pattern) :-
		::asserta((chr_spy_point(P0) :- variant(P0, Pattern))).

	check_for_spy_point(C) :-
		(	::chr_spy_point(C) ->
			clear_next,
			chr_option(trace,yes),
			chr_option(interactive,yes)
		;	true
		).

	chr_trace :-
		chr_option(trace,yes),
		chr_option(interactive,yes).

	chr_notrace :-
		chr_option(trace,no),
		chr_option(interactive,no).

	% read_single_char/1 definition copied from the Logtalk debugger

	:- if(current_logtalk_flag(prolog_dialect, cx)).

		read_single_char(Char) :-
			{get_single_char(Code)}, put_code(Code), char_code(Char, Code),
			(	Code =:= 10 ->
				true
			;	nl
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		read_single_char(Char) :-
			flush(user), tyi(Code), put(Code), nl, char_code(Char, Code).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		read_single_char(Char) :-
			get_key(Code), char_code(Char, Code), nl.

	:- elif(current_logtalk_flag(prolog_dialect, ji)).

		read_single_char(Char) :-
			get_code(Code),
			(	Code =:= -1 ->
				put_code(10), Char = '\n'
			;	put_code(Code), char_code(Char, Code), nl
			).

	:- elif(current_logtalk_flag(prolog_dialect, lean)).

		read_single_char(Char) :-
			kbd_wait(Code),
			put_code(Code),
			nl,
			char_code(Char, Code).

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

		read_single_char(Char) :-
			flush_output, get_code(Code), char_code(Char, Code),
			(	Code =:= 10 ->
				true
			;	skip(10)
			).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		read_single_char(Char) :-
			{get_single_char(Code)}, put_code(Code), nl, char_code(Char, Code).

	:- else.

		% hack to workaround the lack of built-in support for non-buffered character input
		read_single_char(Char) :-
			flush_output, get_code(Code), char_code(Char, Code),
			(	Code =:= 10 ->
				true
			;	peek_code(10) ->
				get_code(_)
			;	true
			).

	:- endif.

	% definition taken from the SWI-Prolog documentation
	variant(Term1, Term2) :-
		% avoid trouble in any shared variables
		copy_term(Term1, Term1Copy),
		copy_term(Term2, Term2Copy),
		% ground and compare the term copies
		numbervars(Term1Copy, 0, N),
		numbervars(Term2Copy, 0, N),
		Term1Copy == Term2Copy.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% PRINT BANNER
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	banner :-
		write('Experimental port of ToyCHR to Logtalk by Paulo Moura.\n'),
		write('Original banner follows.\n\n'),
		write('_|_ _    _|_ ._   _||_\n'),
		write(' |_(_)\\/(_| ||   (_||_)  Version 3.1\n'),
		write('      /\n'),
		write('toychrdb version 1.0, Copyright (C) 2004 Gregory J. Duck\n'),
		write('toychrdb comes with ABSOLUTELY NO WARRANTY; for details see `COPYING\'\n'),
		write('This is free software, and you are welcome to redistribute it\n'),
		write('under certain conditions; see `COPYING\' for details.\n\n').

	:- initialization(banner).

:- end_object.
