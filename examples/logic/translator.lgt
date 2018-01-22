%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(translator).

	:- info([
		version is 1.2,
		date is 2016/02/15,
		author is 'Paulo Moura',
		comment is 'Translator of logic propositions to clauses in conjunctive normal form.',
		source is 'Code partially based on an example found on the Clocksin and Mellish Prolog book.'
	]).

	:- public(translate/2).
	:- mode(translate(+nonvar, -list), zero_or_one).
	:- info(translate/2, [
		comment is 'Translates a proposition to a list of clauses.',
		argnames is ['Propostion', 'Clauses']
	]).

	:- public(step_by_step/2).
	:- mode(step_by_step(+nonvar, -list), zero_or_one).
	:- info(step_by_step/2, [
		comment is 'Translates a proposition to a list of clauses, printing the result of each translation step.',
		argnames is ['Propostion', 'Clauses']
	]).

	:- public(op(10,  fy, '~' )).	% negation
	:- public(op(20, yfx, '&' )).	% conjunction
	:- public(op(30, yfx, 'v' )).	% disjunction
	:- public(op(40, xfx, '=>')).	% implication
	:- public(op(40, xfx, '<=>')).	% equivalence

	:- private(gensym_counter_/1).
	:- dynamic(gensym_counter_/1).


	translate(P, Cs) :-
		remove_implications(P, P2),
		distribute_negation(P2, P3),
		remove_existential_quantifiers(P3, P4),
		convert_to_prenex_normal_form(P4, P5),
		remove_universal_quantifiers(P5, P6),
		convert_to_conjunctive_normal_form(P6, P7),
		convert_to_clauses(P7, Cs),
		print_clauses(Cs).


	step_by_step(P, Cs) :-
		nl, write('Processing proposition: '), write(P), nl, nl,
		write('  1. Remove implications: '),
		remove_implications(P, P2),
		write(P2), nl,
		write('  2. Distribute negation: '),
		distribute_negation(P2, P3),
		write(P3), nl,
		write('  3. Remove existential quantifiers: '),
		remove_existential_quantifiers(P3, P4),
		write(P4), nl,
		write('  4. Convert to prenex normal form: '),
		convert_to_prenex_normal_form(P4, P5),
		write(P5), nl,
		write('  5. Remove universal quantifiers: '),
		remove_universal_quantifiers(P5, P6),
		write(P6), nl,
		write('  6. Convert to conjunctive normal form: '),
		convert_to_conjunctive_normal_form(P6, P7),
		write(P7), nl,
		write('  7. Convert to clauses: '),
		convert_to_clauses(P7, Cs),
		write(Cs), nl, nl,
		write('Clauses in Prolog-like notation:'), nl,
		print_clauses(Cs).


	remove_implications(all(X, P), all(X, P2)) :-
		!,
		remove_implications(P, P2).

	remove_implications(exists(X, P), exists(X, P2)) :-
		!,
		remove_implications(P, P2).

	remove_implications(P <=> Q, P2 & Q2 v ~P2 & ~Q2) :-
		!,
		remove_implications(P, P2),
		remove_implications(Q, Q2).

	remove_implications(P => Q, ~P2 v Q2) :-
		!,
		remove_implications(P, P2),
		remove_implications(Q, Q2).

	remove_implications(P & Q, P2 & Q2) :-
		!,
		remove_implications(P, P2),
		remove_implications(Q, Q2).

	remove_implications(P v Q, P2 v Q2) :-
		!,
		remove_implications(P, P2),
		remove_implications(Q, Q2).

	remove_implications(~P, ~P2) :-
		!,
		remove_implications(P, P2).

	remove_implications(P, P).


	distribute_negation(all(X, P), all(X, P2)) :-
		!,
		distribute_negation(P, P2).

	distribute_negation(exists(X, P), exists(X, P2)) :-
		!,
		distribute_negation(P, P2).

	distribute_negation(P & Q, P2 & Q2) :-
		!,
		distribute_negation(P, P2),
		distribute_negation(Q, Q2).

	distribute_negation(P v Q, P2 v Q2) :-
		!,
		distribute_negation(P, P2),
		distribute_negation(Q, Q2).

	distribute_negation(~P, P2) :-
		!,
		apply_negation(P, P2).

	distribute_negation(P, P).


	apply_negation(all(X, P), exists(X, P2)) :-
		!,
		apply_negation(P, P2).

	apply_negation(exists(X, P), all(X, P2)) :-
		!,
		apply_negation(P, P2).

	apply_negation(P & Q, P2 v Q2) :-
		!,
		apply_negation(P, P2),
		apply_negation(Q, Q2).

	apply_negation(P v Q, P2 & Q2) :-
		!,
		apply_negation(P, P2),
		apply_negation(Q, Q2).

	apply_negation(~P, P2) :-
		!,
		distribute_negation(P, P2).

	apply_negation(P, ~P).


	remove_existential_quantifiers(P, P2) :-
		remove_existential_quantifiers(P, P2, []).

	remove_existential_quantifiers(all(X, P), all(X, P2), Vars) :-
		!,
		remove_existential_quantifiers(P, P2, [X| Vars]).

	remove_existential_quantifiers(exists(X, P), P2, Vars) :-
		!,
		gensym(f, F),
		X =.. [F| Vars],
		remove_existential_quantifiers(P, P2, Vars).

	remove_existential_quantifiers(P & Q, P2 & Q2, Vars) :-
		!,
		remove_existential_quantifiers(P, P2, Vars),
		remove_existential_quantifiers(Q, Q2, Vars).

	remove_existential_quantifiers(P v Q, P2 v Q2, Vars) :-
		!,
		remove_existential_quantifiers(P, P2, Vars),
		remove_existential_quantifiers(Q, Q2, Vars).

	remove_existential_quantifiers(P, P, _).


	convert_to_prenex_normal_form(P, P2) :-
		collect_vars(P, P1, [], Vars),
		add_vars_at_front(Vars, P1, P2).

	collect_vars(all(X, P), P2, Acc, Vars) :-
		!,
		collect_vars(P, P2, [X| Acc], Vars).

	collect_vars(P & Q, P2 & Q2, Acc, Vars) :-
		!,
		collect_vars(P, P2, Acc, Acc2),
		collect_vars(Q, Q2, Acc2, Vars).

	collect_vars(P v Q, P2 v Q2, Acc, Vars) :-
		!,
		collect_vars(P, P2, Acc, Acc2),
		collect_vars(Q, Q2, Acc2, Vars).

	collect_vars(P, P, Vars, Vars).


	add_vars_at_front([], P, P).

	add_vars_at_front([X| Vars], P, P2) :-
		add_vars_at_front(Vars, all(X, P), P2).


	remove_universal_quantifiers(all(_, P), P2) :-
		!,
		remove_universal_quantifiers(P, P2).

	remove_universal_quantifiers(P & Q, P2 & Q2) :-
		!,
		remove_universal_quantifiers(P, P2),
		remove_universal_quantifiers(Q, Q2).

	remove_universal_quantifiers(P v Q, P2 v Q2) :-
		!,
		remove_universal_quantifiers(P, P2),
		remove_universal_quantifiers(Q, Q2).

	remove_universal_quantifiers(P, P).


	convert_to_conjunctive_normal_form(P v Q, R) :-
		!,
		convert_to_conjunctive_normal_form(P, P2),
		convert_to_conjunctive_normal_form(Q, Q2),
		distribute_disjunction(P2 v Q2, R).

	convert_to_conjunctive_normal_form(P & Q, P2 & Q2) :-
		!,
		convert_to_conjunctive_normal_form(P, P2),
		convert_to_conjunctive_normal_form(Q, Q2).

	convert_to_conjunctive_normal_form(P, P).


	distribute_disjunction(P & Q v R, P2 & Q2) :-
		!,
		convert_to_conjunctive_normal_form(P v R, P2),
		convert_to_conjunctive_normal_form(Q v R, Q2).

	distribute_disjunction(P v Q & R, P2 & Q2) :-
		!,
		convert_to_conjunctive_normal_form(P v Q, P2),
		convert_to_conjunctive_normal_form(P v R, Q2).

	distribute_disjunction(P, P).


	convert_to_clauses(P, Cs) :-
		convert_to_clauses(P, [], Cs).


	convert_to_clauses(P & Q, Acc, Cs) :-
		!,
		convert_to_clauses(Q, Acc, Acc2),
		convert_to_clauses(P, Acc2, Cs).

	convert_to_clauses(P, Acc, [cl(Pos, Negs)| Acc]) :-
		convert_to_clauses(P, [], Pos, [], Negs),
		!.

	convert_to_clauses(_, Cs, Cs).


	convert_to_clauses(P v Q, AccPos, Pos, AccNegs, Negs) :-
		!,
		convert_to_clauses(Q, AccPos, AccPos2, AccNegs, AccNegs2),
		convert_to_clauses(P, AccPos2, Pos, AccNegs2, Negs).

	convert_to_clauses(~P, Pos, Pos, AccNegs, [P| AccNegs]) :-
		!,
		not_member_of(P, Pos).

	convert_to_clauses(P, AccPos, [P| AccPos], Negs, Negs) :-
		!,
		not_member_of(P, Negs).

/*
	convert_to_clauses(P & Q, {P2, Q2}) :-
		!,
		convert_to_clauses(P, P2),
		convert_to_clauses(Q, Q2).

	convert_to_clauses(P v Q, R) :-
		!,
		convert_to_clause(P v Q, R).

	convert_to_clauses(P, {P}).


	convert_to_clause(P & Q, R) :-
		!,
		convert_to_clauses(P & Q, {R}).

	convert_to_clause(P v Q, {P2, Q}) :-
		!,
		convert_to_clause(P, P2).

	convert_to_clause(P, P).
*/

	not_member_of(P, [P| _]) :-
		!,
		fail.

	not_member_of(P, [_| Ps]) :-
		!,
		not_member_of(P, Ps).

	not_member_of(_, []).


	print_clauses([]) :-
		nl.

	print_clauses([cl(Pos, Negs)| Cs]) :-
		print_clause(Pos, Negs), nl,
		print_clauses(Cs).

	print_clause(Pos, []) :-
		!,
		print_disjunctions(Pos), write(' :- .').

	print_clause([], Negs) :-
		!,
		write(':- '), print_conjunctions(Negs), write('.').

	print_clause(Pos, Negs) :-
		!,
		print_disjunctions(Pos), write(' :- '),
		print_conjunctions(Negs), write('.').


	print_disjunctions([P]) :-
		!,
		write(P).

	print_disjunctions([P| Ps]) :-
		!,
		write(P), write('; '),
		print_disjunctions(Ps).


	print_conjunctions([P]) :-
		!,
		write(P).

	print_conjunctions([P| Ps]) :-
		!,
		write(P), write(', '),
		print_conjunctions(Ps).


	gensym_counter_(0).


	gensym(Base, Atom) :-
		retract(gensym_counter_(Counter)),
		Counter2 is Counter + 1,
		number_codes(Counter2, Codes2),
		atom_codes(Number, Codes2),
		atom_concat(Base, Number, Atom),
		assertz(gensym_counter_(Counter2)).


:- end_object.
