%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(predicate_negation(_Entity_, _Predicate_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Hook object implementing the predicate_negation mutator by negating matching predicate clause bodies.',
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
		mutation(Term, Mutation),
		next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((Head :- Body), (Head :- \+ (Body))).
	mutation((Head --> Body), (Head --> ReplacedBody)) :-
		negate_dcg_goal(Body, ReplacedBody, true).
	mutation(Head, (Head :- fail)) :-
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _).

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

	negate_dcg_goal(Goal, Goal, false) :-
		var(Goal),
		!.
	negate_dcg_goal((A, B), (RA, RB), Changed) :-
		!,
		negate_dcg_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_dcg_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_dcg_goal((A; B), (RA; RB), Changed) :-
		!,
		negate_dcg_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_dcg_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_dcg_goal((A -> B), (RA -> RB), Changed) :-
		!,
		negate_dcg_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_dcg_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_dcg_goal((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		negate_dcg_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   negate_dcg_goal(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   negate_dcg_goal(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	negate_dcg_goal({Goal}, {\+ (Goal)}, true) :-
		!.
	negate_dcg_goal(Goal, Goal, false).

:- end_object.
