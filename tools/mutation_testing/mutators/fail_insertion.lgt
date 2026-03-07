%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(fail_insertion(_Entity_, _Predicate_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Hook object implementing the ``fail_insertion`` mutator by inserting fail at deterministic body positions for matching predicate clauses.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'Occurrence' - '1-based mutation occurrence index to target within selected predicate clauses.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	:- private(seen_/1).
	:- dynamic(seen_/1).

	term_expansion(Term, Mutation) :-
		^^target_predicate(Term, _Entity_, _Predicate_),
		next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		fail_insertion_kind(Occurrence, Kind),
		mutation_with_kind(Term, Kind, Mutation),
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

	mutation_with_kind((Head :- Body), Kind, (Head :- MutatedBody)) :-
		insert_fail(Kind, Body, MutatedBody).
	mutation_with_kind((Head --> Body), Kind, (Head --> MutatedBody)) :-
		insert_dcg_fail(Kind, Body, MutatedBody).
	mutation_with_kind(Head, _Kind, (Head :- fail)) :-
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _).

	fail_insertion_kind(replace).
	fail_insertion_kind(prepend).
	fail_insertion_kind(append).
	fail_insertion_kind(middle).

	reset :-
		retractall(seen_(_)),
		assertz(seen_(0)).

	next_occurrence(Occurrence) :-
		(   retract(seen_(Previous)) ->
			true
		;   Previous = 0
		),
		Occurrence is Previous + 1,
		assertz(seen_(Occurrence)).

	fail_insertion_kind(Occurrence, replace) :-
		0 is (Occurrence - 1) mod 4,
		!.
	fail_insertion_kind(Occurrence, prepend) :-
		1 is (Occurrence - 1) mod 4,
		!.
	fail_insertion_kind(Occurrence, append) :-
		2 is (Occurrence - 1) mod 4,
		!.
	fail_insertion_kind(_Occurrence, middle).

	insert_fail(replace, _Body, fail) :-
		!.
	insert_fail(prepend, Body, (fail, Body)) :-
		!.
	insert_fail(append, Body, (Body, fail)) :-
		!.
	insert_fail(middle, (A, B), (A, (fail, B))) :-
		!.
	insert_fail(middle, Body, (Body, fail)).

	insert_dcg_fail(replace, _Body, {fail}) :-
		!.
	insert_dcg_fail(prepend, Body, ({fail}, Body)) :-
		!.
	insert_dcg_fail(append, Body, (Body, {fail})) :-
		!.
	insert_dcg_fail(middle, (A, B), (A, ({fail}, B))) :-
		!.
	insert_dcg_fail(middle, Body, (Body, {fail})).

:- end_object.
