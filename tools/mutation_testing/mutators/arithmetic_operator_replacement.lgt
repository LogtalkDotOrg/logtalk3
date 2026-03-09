%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(arithmetic_operator_replacement(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements(expanding),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-09,
		comment is 'Hook object implementing the ``arithmetic_operator_replacement`` mutator for matching predicate clauses.',
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
		replace_arithmetic_operators(Body, ReplacedBody, true).
	mutation((Head --> Body), (Head --> ReplacedBody)) :-
		replace_dcg_arithmetic_operators(Body, ReplacedBody, true).

	replace_arithmetic_operators((A, B), (RA, RB), Changed) :-
		!,
		replace_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_arithmetic_operators((A; B), (RA; RB), Changed) :-
		!,
		replace_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_arithmetic_operators((A -> B), (RA -> RB), Changed) :-
		!,
		replace_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_arithmetic_operators((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		replace_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   replace_arithmetic_operators(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   replace_arithmetic_operators(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	replace_arithmetic_operators((Left is Right), (Left is ReplacedRight), Changed) :-
		!,
		replace_arithmetic_expression(Right, ReplacedRight, Changed).
	replace_arithmetic_operators(Goal, Replaced, Changed) :-
		nonvar(Goal),
		Goal =.. [Operator, Left, Right],
		arithmetic_comparison_operator(Operator),
		!,
		replace_arithmetic_expression(Left, ReplacedLeft, ChangedLeft),
		(   ChangedLeft == true ->
			ReplacedRight = Right,
			Changed = true
		;   replace_arithmetic_expression(Right, ReplacedRight, ChangedRight),
			Changed = ChangedRight
		),
		Replaced =.. [Operator, ReplacedLeft, ReplacedRight].
	replace_arithmetic_operators(Goal, Goal, false).

	replace_arithmetic_expression(Expression, Replaced, true) :-
		nonvar(Expression),
		Expression =.. [Operator, Left, Right],
		arithmetic_operator_replacement(Operator, Replacement),
		!,
		ReplacedLeft = Left,
		ReplacedRight = Right,
		Replaced =.. [Replacement, ReplacedLeft, ReplacedRight].
	replace_arithmetic_expression(Expression, Replaced, Changed) :-
		nonvar(Expression),
		Expression =.. [Operator, Argument],
		!,
		replace_arithmetic_expression(Argument, ReplacedArgument, Changed),
		Replaced =.. [Operator, ReplacedArgument].
	replace_arithmetic_expression(Expression, Expression, false).

	arithmetic_operator_replacement(+, -).
	arithmetic_operator_replacement(-, +).
	arithmetic_operator_replacement(*, /).
	arithmetic_operator_replacement(/, *).

	arithmetic_comparison_operator(>).
	arithmetic_comparison_operator(<).
	arithmetic_comparison_operator(>=).
	arithmetic_comparison_operator(=<).
	arithmetic_comparison_operator(=:=).
	arithmetic_comparison_operator(=\=).

	replace_dcg_arithmetic_operators(Goal, Goal, false) :-
		var(Goal),
		!.
	replace_dcg_arithmetic_operators((A, B), (RA, RB), Changed) :-
		!,
		replace_dcg_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_arithmetic_operators((A; B), (RA; RB), Changed) :-
		!,
		replace_dcg_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_arithmetic_operators((A -> B), (RA -> RB), Changed) :-
		!,
		replace_dcg_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			Changed = true
		;   replace_dcg_arithmetic_operators(B, RB, ChangedB),
			Changed = ChangedB
		).
	replace_dcg_arithmetic_operators((A -> B; C), (RA -> RB; RC), Changed) :-
		!,
		replace_dcg_arithmetic_operators(A, RA, ChangedA),
		(   ChangedA == true ->
			RB = B,
			RC = C,
			Changed = true
		;   replace_dcg_arithmetic_operators(B, RB, ChangedB),
			(   ChangedB == true ->
				RC = C,
				Changed = true
			;   replace_dcg_arithmetic_operators(C, RC, ChangedC),
				Changed = ChangedC
			)
		).
	replace_dcg_arithmetic_operators({Goal}, {ReplacedGoal}, Changed) :-
		!,
		replace_arithmetic_operators(Goal, ReplacedGoal, Changed).
	replace_dcg_arithmetic_operators(Goal, Goal, false).

:- end_object.
