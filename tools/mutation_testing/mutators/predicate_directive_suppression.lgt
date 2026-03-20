%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(predicate_directive_suppression(_Entity_, _Predicate_, _DirectiveIndex_, _Occurrence_, _PrintMutation_),
	implements((expanding, directive_mutator_protocol)),
	imports(mutator_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Hook object implementing the ``predicate_directive_suppression`` mutator by suppressing matching predicate or non-terminal directives.',
		parameters is [
			'Entity' - 'Identifier of the entity being mutated.',
			'Predicate' - 'Predicate or non-terminal indicator selecting directives.',
			'DirectiveIndex' - '1-based index for the selected matching directive.',
			'Occurrence' - '1-based mutation occurrence index to target within selected matching directives.',
			'PrintMutation' - 'Boolean flag to print the original and mutated term plus source location.'
		]
	]).

	term_expansion(Term, Mutation) :-
		^^target_predicate_directive_index(Term, _Entity_, _Predicate_, DirectiveIndex),
		DirectiveIndex =:= _DirectiveIndex_,
		mutation(Term, Mutation),
		^^next_occurrence(Occurrence),
		Occurrence =:= _Occurrence_,
		^^print_mutation(_PrintMutation_, Term, Mutation).

	mutation((:- Directive), []) :-
		predicate_directive(Directive).

	predicate_directive(alias(_, _)).
	predicate_directive(coinductive(_)).
	predicate_directive(discontiguous(_)).
	predicate_directive(dynamic(_)).
	predicate_directive(info(_, _)).
	predicate_directive(meta_predicate(_)).
	predicate_directive(meta_non_terminal(_)).
	predicate_directive(mode(_, _)).
	predicate_directive(mode_non_terminal(_, _)).
	predicate_directive(multifile(_)).
	predicate_directive(private(_)).
	predicate_directive(protected(_)).
	predicate_directive(public(_)).
	predicate_directive(synchronized(_)).
	predicate_directive(uses(_, _)).
	predicate_directive(use_module(_, _)).

:- end_object.
