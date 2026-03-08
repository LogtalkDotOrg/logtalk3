%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(head_arguments_reordering(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Hook object implementing the ``head_arguments_reordering`` mutator by swapping the first two arguments in matching rule or grammar rule heads.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'ClauseIndex' - '1-based clause index for the selected mutation (equal to ``Occurrence`` for this mutator).',
			'Occurrence' - '1-based mutation occurrence index to target within selected predicate clauses.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	:- private(seen_/1).
	:- dynamic(seen_/1).

	coverage_clause_mutator.

	term_expansion(Term, Mutation) :-
		^^target_predicate_clause_index(Term, _Entity_, _Predicate_, ClauseIndex),
		mutation(Term, Mutation),
		next_occurrence(Occurrence),
		ClauseIndex =:= _ClauseIndex_,
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((Head :- Body), (MutatedHead :- Body)) :-
		!,
		reorder_head_arguments(Head, MutatedHead).
	mutation((Head --> Body), (MutatedHead --> Body)) :-
		!,
		reorder_head_arguments(Head, MutatedHead).
	mutation(Head, MutatedHead) :-
		reorder_head_arguments(Head, MutatedHead).

	reset :-
		^^reset,
		retractall(seen_(_)),
		assertz(seen_(0)).

	next_occurrence(Occurrence) :-
		(   retract(seen_(Previous)) ->
			true
		;   Previous = 0
		),
		Occurrence is Previous + 1,
		assertz(seen_(Occurrence)).

	reorder_head_arguments(Head, MutatedHead) :-
		Head =.. [Functor| Arguments],
		list::permutation(Arguments, Permutation),
		Permutation \== Arguments,
		MutatedHead =.. [Functor| Permutation].

:- end_object.
