%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(predicate_negation(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-09,
		comment is 'Hook object implementing the predicate_negation mutator by negating matching predicate clause bodies.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'ClauseIndex' - '1-based clause index for the selected mutation (equal to ``Occurrence`` for this mutator).',
			'Occurrence' - '1-based mutation occurrence index to target within selected predicate clauses.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	coverage_clause_mutator.

	term_expansion(Term, Mutation) :-
		^^target_predicate_clause_index(Term, _Entity_, _Predicate_, ClauseIndex),
		mutation(Term, Mutation),
		^^next_occurrence(Occurrence),
		ClauseIndex =:= _ClauseIndex_,
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((Head :- Body), (Head :- MutatedBody)) :-
		negate_goal(Body, MutatedBody, true).
	mutation((Head --> Body), (Head --> MutatedBody)) :-
		negate_dcg_goal(Body, MutatedBody, true).
	mutation(Head, (Head :- fail)) :-
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _).

	negate_goal(Goal, Goal, false) :-
		var(Goal),
		!.
	negate_goal((A, B), (RA, RB), Changed) :-
		!,
		negate_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_goal((A; B), (RA; RB), Changed) :-
		!,
		negate_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_goal((A -> B), (RA -> RB), Changed) :-
		!,
		negate_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   negate_goal(B, RB, ChangedB),
			Changed = ChangedB
		).
	negate_goal((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		negate_goal(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   negate_goal(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   negate_goal(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	negate_goal(Goal, \+ (Goal), true) :-
		!.
	negate_goal(Goal, Goal, false).

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
