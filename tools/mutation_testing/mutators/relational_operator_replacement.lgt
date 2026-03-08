%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(relational_operator_replacement(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Hook object implementing the ``relational_operator_replacement`` mutator for matching predicate clauses.',
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

	mutation((Head :- Body), (Head :- ReplacedBody)) :-
		replace_relational_operators(Body, ReplacedBody, true).
	mutation((Head --> Body), (Head --> ReplacedBody)) :-
		replace_dcg_relational_operators(Body, ReplacedBody, true).

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

	replace_relational_operators(Goal, Goal, false) :-
		var(Goal),
		!.
	replace_relational_operators((A, B), (RA, RB), Changed) :-
		!,
		replace_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_relational_operators((A; B), (RA; RB), Changed) :-
		!,
		replace_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_relational_operators((A -> B), (RA -> RB), Changed) :-
		!,
		replace_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_relational_operators((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		replace_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   replace_relational_operators(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   replace_relational_operators(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	replace_relational_operators(Goal, Replaced, true) :-
		nonvar(Goal),
		Goal =.. [Operator, Left, Right],
		relational_operator_replacement(Operator, Replacement),
		!,
		Replaced =.. [Replacement, Left, Right].
	replace_relational_operators(Goal, Goal, false).

	replace_dcg_relational_operators((A, B), (RA, RB), Changed) :-
		!,
		replace_dcg_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_relational_operators((A; B), (RA; RB), Changed) :-
		!,
		replace_dcg_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_relational_operators((A -> B), (RA -> RB), Changed) :-
		!,
		replace_dcg_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_relational_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_relational_operators((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		replace_dcg_relational_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   replace_dcg_relational_operators(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   replace_dcg_relational_operators(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	replace_dcg_relational_operators({Goal}, {ReplacedGoal}, Changed) :-
		!,
		replace_relational_operators(Goal, ReplacedGoal, Changed).
	replace_dcg_relational_operators(Goal, Goal, false).

	relational_operator_replacement((>), (=<)).
	relational_operator_replacement((<), (>=)).
	relational_operator_replacement((>=), (<)).
	relational_operator_replacement((=<), (>)).
	relational_operator_replacement((=:=), (=\=)).
	relational_operator_replacement((=\=), (=:=)).
	relational_operator_replacement((@>), (@=<)).
	relational_operator_replacement((@<), (@>=)).
	relational_operator_replacement((@>=), (@<)).
	relational_operator_replacement((@=<), (@>)).
	relational_operator_replacement((==), (\==)).
	relational_operator_replacement((\==), (==)).

:- end_object.
