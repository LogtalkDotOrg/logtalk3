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


:- object(scope_directive_replacement(_Entity_, _Predicate_, _DirectiveIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, directive_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the ``scope_directive_replacement`` mutator by replacing matching scope directives with an alternative visibility.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting directives.',
			'DirectiveIndex' - '1-based index for the selected matching scope directive.',
			'Occurrence' - '1-based mutation occurrence index selecting an alternative visibility for the directive.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	term_expansion(Term, Mutation) :-
		^^target_scope_directive_index(Term, _Entity_, _Predicate_, DirectiveIndex),
		DirectiveIndex =:= _DirectiveIndex_,
		mutation(Term, Mutation),
		^^next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((:- Directive), (:- MutatedDirective)) :-
		scope_directive_mutation(Directive, MutatedDirective).

	scope_directive_mutation(public(Resources), protected(Resources)).
	scope_directive_mutation(public(Resources), private(Resources)).
	scope_directive_mutation(protected(Resources), public(Resources)).
	scope_directive_mutation(protected(Resources), private(Resources)).
	scope_directive_mutation(private(Resources), public(Resources)).
	scope_directive_mutation(private(Resources), protected(Resources)).

:- end_object.
