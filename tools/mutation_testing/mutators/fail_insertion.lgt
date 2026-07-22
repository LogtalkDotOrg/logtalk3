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


:- object(fail_insertion(_Entity_, _Predicate_, _ClauseIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, clause_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the ``fail_insertion`` mutator by inserting fail at deterministic body positions for matching predicate clauses.',
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

	insert_fail(replace, _Body, fail).
	insert_fail(middle, Body, _) :-
		Body \= (_, _),
		!,
		fail.
	insert_fail(middle, (A, B), (A, (fail, B))).
	insert_fail(middle, (A, B, C), (A, BB)) :-
		insert_fail(middle, (B, C), BB).
	insert_fail(append, Body, (Body, fail)) :-
		\+ body_ends_with_fail_or_false(Body).

	insert_dcg_fail(replace, _Body, {fail}).
	insert_dcg_fail(middle, Body, _) :-
		Body \= (_, _),
		!,
		fail.
	insert_dcg_fail(middle, (A, B), (A, ({fail}, B))).
	insert_dcg_fail(middle, (A, B, C), (A, BB)) :-
		insert_dcg_fail(middle, (B, C), BB).
	insert_dcg_fail(append, Body, (Body, {fail})) :-
		\+ dcg_body_ends_with_fail_or_false(Body).

	body_ends_with_fail_or_false((_, Tail)) :-
		body_ends_with_fail_or_false(Tail).
	body_ends_with_fail_or_false(fail).
	body_ends_with_fail_or_false(false).

	dcg_body_ends_with_fail_or_false((_, Tail)) :-
		dcg_body_ends_with_fail_or_false(Tail).
	dcg_body_ends_with_fail_or_false({fail}).
	dcg_body_ends_with_fail_or_false({false}).

:- end_object.
