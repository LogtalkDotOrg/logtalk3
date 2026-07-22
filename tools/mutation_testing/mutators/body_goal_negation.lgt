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


:- object(body_goal_negation(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, clause_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the body_goal_negation mutator by negating matching predicate clause bodies.',
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
		negate_goal(Body, MutatedBody).
	mutation((Head --> Body), (Head --> MutatedBody)) :-
		negate_dcg_goal(Body, MutatedBody).
	mutation(Head, (Head :- fail)) :-
		nonvar(Head),
		Head \= (:- _),
		Head \= (_ :- _).

	negate_goal(Goal, _) :-
		var(Goal),
		!,
		fail.
	negate_goal((A, B), (RA, B)) :-
		negate_goal(A, RA).
	negate_goal((A, B), (A, RB)) :-
		negate_goal(B, RB).
	negate_goal((A; B), (RA; B)) :-
		negate_goal(A, RA).
	negate_goal((A; B), (A; RB)) :-
		negate_goal(B, RB).
	negate_goal((A -> B), (RA -> B)) :-
		negate_goal(A, RA).
	negate_goal((A -> B), (A -> RB)) :-
		negate_goal(B, RB).
	negate_goal((A -> B; C), (RA -> B; C)) :-
		negate_goal(A, RA).
	negate_goal((A -> B; C), (A -> RB; C)) :-
		negate_goal(B, RB).
	negate_goal((A -> B; C), (A -> B; RC)) :-
		negate_goal(C, RC).
	negate_goal(Goal, \+ (Goal)).

	negate_dcg_goal(Goal, _) :-
		var(Goal),
		!,
		fail.
	negate_dcg_goal((A, B), (RA, B)) :-
		negate_dcg_goal(A, RA).
	negate_dcg_goal((A, B), (A, RB)) :-
		negate_dcg_goal(B, RB).
	negate_dcg_goal((A; B), (RA; B)) :-
		negate_dcg_goal(A, RA).
	negate_dcg_goal((A; B), (A; RB)) :-
		negate_dcg_goal(B, RB).
	negate_dcg_goal((A -> B), (RA -> B)) :-
		negate_dcg_goal(A, RA).
	negate_dcg_goal((A -> B), (A -> RB)) :-
		negate_dcg_goal(B, RB).
	negate_dcg_goal((A -> B; C), (RA -> B; C)) :-
		negate_dcg_goal(A, RA).
	negate_dcg_goal((A -> B; C), (A -> RB; C)) :-
		negate_dcg_goal(B, RB).
	negate_dcg_goal((A -> B; C), (A -> B; RC)) :-
		negate_dcg_goal(C, RC).
	negate_dcg_goal({Goal}, {\+ (Goal)}).

:- end_object.
