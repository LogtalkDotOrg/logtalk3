%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(clauses_reordering(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-09,
		comment is 'Hook object implementing the ``clauses_reordering`` mutator by reordering the clauses of a non-discontiguous predicate or non-terminal definition.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'ClauseIndex' - '1-based clause index selecting which clause is swapped with its successor (last swaps with first).',
			'Occurrence' - '1-based clause index selecting which clause is swapped with its successor (last swaps with first).',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	:- private(clauses_/1).
	:- dynamic(clauses_/1).

	:- uses(list, [
		append/3, length/2, reverse/2, select/3
	]).

	term_expansion((Head :- Body), []) :-
		^^target_predicate((Head :- Body), _Entity_, _Predicate_),
		collect_clause((Head :- Body)),
		!.
	term_expansion((Head --> Body), []) :-
		^^target_predicate((Head --> Body), _Entity_, _Predicate_),
		collect_clause((Head --> Body)),
		!.
	term_expansion(Head, []) :-
		^^target_predicate(Head, _Entity_, _Predicate_),
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _),
		collect_clause(Head),
		!.
	term_expansion(end_of_file, Expanded) :-
		logtalk_load_context(entity_identifier, _Entity_),
		flush_collected_clauses(Clauses),
		select_reordered_clauses(Clauses, [end_of_file], Expanded),
		!.
	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_identifier, _Entity_),
		flush_collected_clauses(Clauses),
		(   Clauses == [] ->
			Expanded = Term
		;   select_reordered_clauses(Clauses, [Term], Expanded)
		).

	select_reordered_clauses(Clauses, Tail, Expanded) :-
		reset_seen,
		(   mutation(Clauses, Reordered),
			^^next_occurrence(Occurrence),
			Occurrence =:= _Occurrence_,
			^^print_mutation(_PrintMutation_, Clauses, Reordered),
			append(Reordered, Tail, Expanded)
		;   append(Clauses, Tail, Expanded)
		).

	mutation(Clauses, Reordered) :-
		permutation(Clauses, Reordered),
		Reordered \== Clauses.

	% simplified version (doesn't need to check that both arguments are the same length)
	permutation([], []).
	permutation(List, [Head| Tail]) :-
		select(Head, List, Remaining),
		permutation(Remaining, Tail).

	reset :-
		^^reset,
		retractall(clauses_(_)).

	reset_seen :-
		::retractall(seen_(_)),
		::assertz(seen_(0)).

	collect_clause(Clause) :-
		(   retract(clauses_(Clauses)) ->
			true
		;   Clauses = []
		),
		assertz(clauses_([Clause| Clauses])).

	flush_collected_clauses(Clauses) :-
		(   retract(clauses_(ReversedClauses)) ->
			reverse(ReversedClauses, Clauses)
		;   Clauses = []
		).

	clause_head((Head :- _), Head) :-
		!.
	clause_head((Head --> _), Head) :-
		!.
	clause_head(Head, Head).

:- end_object.
