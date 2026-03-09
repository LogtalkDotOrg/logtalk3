%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(truth_literal_flip(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-09,
		comment is 'Hook object implementing the ``truth_literal_flip`` mutator for matching predicate clauses.',
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

	mutation((Head :- Body), (Head :- ReplacedBody)) :-
		flip_truth_literals(Body, ReplacedBody, true).
	mutation((Head --> Body), (Head --> ReplacedBody)) :-
		flip_dcg_truth_literals(Body, ReplacedBody, true).

	flip_truth_literals(Goal, Goal, false) :-
		var(Goal),
		!.
	flip_truth_literals((A, B), (RA, RB), Changed) :-
		!,
		flip_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_truth_literals((A; B), (RA; RB), Changed) :-
		!,
		flip_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_truth_literals((A -> B), (RA -> RB), Changed) :-
		!,
		flip_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_truth_literals((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		flip_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   flip_truth_literals(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   flip_truth_literals(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	flip_truth_literals(true,  fail, true) :- !.
	flip_truth_literals(fail,  true, true) :- !.
	flip_truth_literals(false, true, true) :- !.
	flip_truth_literals(Goal, Goal, false).

	flip_dcg_truth_literals((A, B), (RA, RB), Changed) :-
		!,
		flip_dcg_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_dcg_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_dcg_truth_literals((A; B), (RA; RB), Changed) :-
		!,
		flip_dcg_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_dcg_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_dcg_truth_literals((A -> B), (RA -> RB), Changed) :-
		!,
		flip_dcg_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   flip_dcg_truth_literals(B, RB, ChangedB),
			Changed = ChangedB
		).
	flip_dcg_truth_literals((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		flip_dcg_truth_literals(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   flip_dcg_truth_literals(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   flip_dcg_truth_literals(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	flip_dcg_truth_literals({Goal}, {ReplacedGoal}, Changed) :-
		!,
		flip_truth_literals(Goal, ReplacedGoal, Changed).
	flip_dcg_truth_literals(Goal, Goal, false).

:- end_object.
