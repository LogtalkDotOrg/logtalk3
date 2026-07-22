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


:- object(uses_directive_resource_deletion(_Entity_, _Predicate_, _DirectiveIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, directive_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the ``uses_directive_resource_deletion`` mutator by deleting one matching resource from a ``uses/2`` directive.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting resources.',
			'DirectiveIndex' - '1-based index for the selected matching ``uses/2`` directive.',
			'Occurrence' - '1-based mutation occurrence index selecting which matching resource to delete.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	term_expansion(Term, Mutation) :-
		^^target_uses_directive_index(Term, _Entity_, _Predicate_, DirectiveIndex),
		DirectiveIndex =:= _DirectiveIndex_,
		mutation(Term, Mutation),
		^^next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((:- uses(Object, [Resource01, Resource02| Resources0])), (:- uses(Object, Resources))) :-
		delete_matching_resource([Resource01, Resource02| Resources0], _Predicate_, Resources).

	delete_matching_resource([Resource| Resources], Predicate, Resources) :-
		resource_matches_predicate(Resource, Predicate).
	delete_matching_resource([Resource| Resources0], Predicate, [Resource| Resources]) :-
		delete_matching_resource(Resources0, Predicate, Resources).

	resource_matches_predicate(Resource, Predicate) :-
		(   Resource = Predicate ->
			true
		;   Resource = (_Original as Alias),
			alias_matches_predicate(Alias, Predicate) ->
			true
		;   Resource = (_Original::Alias),
			alias_matches_predicate(Alias, Predicate)
		).

	alias_matches_predicate(Alias, Predicate) :-
		Alias == Predicate.
	alias_matches_predicate(Alias, Predicate) :-
		nonvar(Alias),
		Alias \= (_/_),
		Alias \= (_//_),
		functor(Alias, Name, Arity),
		Predicate = Name/Arity.

:- end_object.
