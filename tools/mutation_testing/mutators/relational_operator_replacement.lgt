%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(relational_operator_replacement(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, clause_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the ``relational_operator_replacement`` mutator for matching predicate clauses.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting clauses to mutate.',
			'ClauseIndex' - '1-based clause index for the selected mutation.',
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
		replace_relational_operators(Body, ReplacedBody, true).
	mutation((Head --> Body), (Head --> ReplacedBody)) :-
		replace_dcg_relational_operators(Body, ReplacedBody, true).

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
	relational_operator_replacement((>), (<)).
	relational_operator_replacement((>), (>=)).

	relational_operator_replacement((<), (>=)).
	relational_operator_replacement((<), (>)).
	relational_operator_replacement((<), (=<)).

	relational_operator_replacement((>=), (<)).
	relational_operator_replacement((>=), (=<)).
	relational_operator_replacement((>=), (>)).

	relational_operator_replacement((=<), (>)).
	relational_operator_replacement((=<), (>=)).
	relational_operator_replacement((=<), (<)).

	relational_operator_replacement((=:=), (=\=)).

	relational_operator_replacement((=\=), (=:=)).

	relational_operator_replacement((@>), (@=<)).
	relational_operator_replacement((@>), (@<)).
	relational_operator_replacement((@>), (@>=)).

	relational_operator_replacement((@<), (@>=)).
	relational_operator_replacement((@<), (@>)).
	relational_operator_replacement((@<), (@=<)).

	relational_operator_replacement((@>=), (@<)).
	relational_operator_replacement((@>=), (@=<)).
	relational_operator_replacement((@>=), (@>)).

	relational_operator_replacement((@=<), (@>)).
	relational_operator_replacement((@=<), (@>=)).
	relational_operator_replacement((@=<), (@<)).

	relational_operator_replacement((==), (\==)).

	relational_operator_replacement((\==), (==)).

:- end_object.
