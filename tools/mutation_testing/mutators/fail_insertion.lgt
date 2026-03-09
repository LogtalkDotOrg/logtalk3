%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(fail_insertion(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-09,
		comment is 'Hook object implementing the ``fail_insertion`` mutator by inserting fail at deterministic body positions for matching predicate clauses.',
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

	mutation((Head :- Body), (Head :- MutatedBody)) :-
		fail_insertion_kind(Kind),
		insert_fail(Kind, Body, MutatedBody).
	mutation((Head --> Body), (Head --> MutatedBody)) :-
		fail_insertion_kind(Kind),
		insert_dcg_fail(Kind, Body, MutatedBody).
	mutation(Head, (Head :- fail)) :-
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _).

	fail_insertion_kind(replace).
	fail_insertion_kind(middle).
	fail_insertion_kind(append).

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

	insert_fail(replace, _Body, fail).
	insert_fail(middle, Body, _) :-
		Body \= (_, _),
		!,
		fail.
	insert_fail(middle, (A, B), (A, (fail, B))).
	insert_fail(middle, (A, B, C), (A, BB)) :-
		insert_fail(middle, (B, C), BB).
	insert_fail(append, Body, (Body, fail)).

	insert_dcg_fail(replace, _Body, {fail}).
	insert_dcg_fail(middle, Body, _) :-
		Body \= (_, _),
		!,
		fail.
	insert_dcg_fail(middle, (A, B), (A, ({fail}, B))).
	insert_dcg_fail(middle, (A, B, C), (A, BB)) :-
		insert_dcg_fail(middle, (B, C), BB).
	insert_dcg_fail(append, Body, (Body, {fail})).

:- end_object.
