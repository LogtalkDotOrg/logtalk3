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


:- category(mutator_common,
	implements(mutator_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-20,
		comment is 'Mutator common predicate utilities.'
	]).

	:- protected(print_mutation/3).
	:- mode(print_mutation(+boolean, @callable, @callable), one).
	:- info(print_mutation/3, [
		comment is 'Prints a term and its mutation when ``Flag`` is true. Succeeds otherwise.',
		argnames is ['Flag', 'Original', 'Mutation']
	]).

	:- protected(target_predicate/3).
	:- mode(target_predicate(@callable, @entity_identifier, @predicate_indicator), one).
	:- info(target_predicate/3, [
		comment is 'True iff ``Term`` is a candidate for mutation.',
		argnames is ['Term', 'Entity', 'Predicate']
	]).

	:- protected(target_predicate_clause_index/4).
	:- mode(target_predicate_clause_index(@callable, @entity_identifier, @predicate_indicator, -integer), zero_or_one).
	:- info(target_predicate_clause_index/4, [
		comment is 'True iff ``Term`` is a candidate for mutation while also returning its current 1-based contiguous clause index for the matching predicate or non-terminal.',
		argnames is ['Term', 'Entity', 'Predicate', 'ClauseIndex']
	]).

	:- protected(target_scope_directive/3).
	:- mode(target_scope_directive(@callable, @entity_identifier, @predicate_indicator), one).
	:- info(target_scope_directive/3, [
		comment is 'True iff ``Term`` is a matching predicate or non-terminal scope directive candidate for mutation.',
		argnames is ['Term', 'Entity', 'Predicate']
	]).

	:- protected(target_scope_directive_index/4).
	:- mode(target_scope_directive_index(@callable, @entity_identifier, @predicate_indicator, -integer), zero_or_one).
	:- info(target_scope_directive_index/4, [
		comment is 'True iff ``Term`` is a matching predicate or non-terminal scope directive candidate for mutation while also returning its 1-based index among matching scope directives for the selected predicate or non-terminal.',
		argnames is ['Term', 'Entity', 'Predicate', 'DirectiveIndex']
	]).

	:- protected(target_predicate_directive/3).
	:- mode(target_predicate_directive(@callable, @entity_identifier, @predicate_indicator), one).
	:- info(target_predicate_directive/3, [
		comment is 'True iff ``Term`` is a matching predicate or non-terminal directive candidate for mutation.',
		argnames is ['Term', 'Entity', 'Predicate']
	]).

	:- protected(target_predicate_directive_index/4).
	:- mode(target_predicate_directive_index(@callable, @entity_identifier, @predicate_indicator, -integer), zero_or_one).
	:- info(target_predicate_directive_index/4, [
		comment is 'True iff ``Term`` is a matching predicate or non-terminal directive candidate for mutation while also returning its 1-based index among matching directives for the selected predicate or non-terminal.',
		argnames is ['Term', 'Entity', 'Predicate', 'DirectiveIndex']
	]).

	:- protected(target_uses_directive/3).
	:- mode(target_uses_directive(@callable, @entity_identifier, @predicate_indicator), one).
	:- info(target_uses_directive/3, [
		comment is 'True iff ``Term`` is a matching ``uses/2`` directive candidate for mutation.',
		argnames is ['Term', 'Entity', 'Predicate']
	]).

	:- protected(target_uses_directive_index/4).
	:- mode(target_uses_directive_index(@callable, @entity_identifier, @predicate_indicator, -integer), zero_or_one).
	:- info(target_uses_directive_index/4, [
		comment is 'True iff ``Term`` is a matching ``uses/2`` directive candidate for mutation while also returning its 1-based index among matching directives for the selected predicate or non-terminal.',
		argnames is ['Term', 'Entity', 'Predicate', 'DirectiveIndex']
	]).

	:- protected(next_occurrence/1).
	:- mode(next_occurrence(-integer), one).
	:- info(next_occurrence/1, [
		comment is 'Next mutation occurrence.',
		argnames is ['Occurrence']
	]).

	:- private(current_predicate_clause_index_/2).
	:- dynamic(current_predicate_clause_index_/2).
	:- mode(current_predicate_clause_index_(?predicate_indicator, ?integer), zero_or_one).
	:- info(current_predicate_clause_index_/2, [
		comment is 'Table of current clause indexes per predicate.',
		argnames is ['Predicate', 'ClauseIndex']
	]).

	:- private(update_target_predicate_clause_index_/2).
	:- mode(update_target_predicate_clause_index_(@predicate_indicator, -integer), one).
	:- info(update_target_predicate_clause_index_/2, [
		comment is 'Updates and returns the next clause index for the given predicate.',
		argnames is ['Predicate', 'ClauseIndex']
	]).

	:- private(current_scope_directive_index_/2).
	:- dynamic(current_scope_directive_index_/2).
	:- mode(current_scope_directive_index_(?predicate_indicator, ?integer), zero_or_one).
	:- info(current_scope_directive_index_/2, [
		comment is 'Table of current scope directive indexes per predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(update_target_scope_directive_index_/2).
	:- mode(update_target_scope_directive_index_(@predicate_indicator, -integer), one).
	:- info(update_target_scope_directive_index_/2, [
		comment is 'Updates and returns the next scope directive index for the given predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(current_predicate_directive_index_/2).
	:- dynamic(current_predicate_directive_index_/2).
	:- mode(current_predicate_directive_index_(?predicate_indicator, ?integer), zero_or_one).
	:- info(current_predicate_directive_index_/2, [
		comment is 'Table of current predicate directive indexes per predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(update_target_predicate_directive_index_/2).
	:- mode(update_target_predicate_directive_index_(@predicate_indicator, -integer), one).
	:- info(update_target_predicate_directive_index_/2, [
		comment is 'Updates and returns the next predicate directive index for the given predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(current_uses_directive_index_/2).
	:- dynamic(current_uses_directive_index_/2).
	:- mode(current_uses_directive_index_(?predicate_indicator, ?integer), zero_or_one).
	:- info(current_uses_directive_index_/2, [
		comment is 'Table of current uses directive indexes per predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(update_target_uses_directive_index_/2).
	:- mode(update_target_uses_directive_index_(@predicate_indicator, -integer), one).
	:- info(update_target_uses_directive_index_/2, [
		comment is 'Updates and returns the next uses directive index for the given predicate.',
		argnames is ['Predicate', 'DirectiveIndex']
	]).

	:- private(seen_/1).
	:- dynamic(seen_/1).
	:- mode(seen_(?integer), zero_or_one).
	:- info(seen_/1, [
		comment is 'Table of last seen mutation occurrence.',
		argnames is ['Occurrence']
	]).

	reset :-
		::retractall(current_predicate_clause_index_(_, _)),
		::retractall(current_scope_directive_index_(_, _)),
		::retractall(current_predicate_directive_index_(_, _)),
		::retractall(current_uses_directive_index_(_, _)),
		::retractall(seen_(_)),
		::assertz(seen_(0)).

	% by default, mutators do not map mutation occurrences to clause numbers
	coverage_clause_mutator :-
		fail.

	print_mutation(true, Original, Mutation) :-
		self(Self),
		functor(Self, Mutator, _),
		(	logtalk_load_context(variable_names(Original), Variables) ->
			true
		;	% e.g., a mutant such as clauses_reordering that is not single term based
			Variables = []
		),
		logtalk_load_context(file, File),
		(	logtalk_load_context(term_position(Original), StartLine-EndLine) ->
			true
		;	% e.g., a mutant such as clauses_reordering that is not single term based
			StartLine-EndLine = 0-0
		),
		logtalk::print_message(information, mutation_testing, mutated_term(Mutator, Original, Mutation, Variables, File, StartLine-EndLine)).
	print_mutation(false, _Original, _Mutation).

	target_predicate((Head :- _Body), Entity, Predicate) :-
		!,
		target_predicate(Head, Entity, Predicate).
	target_predicate((Head, _ --> _Body), Entity, Predicate) :-
		!,
		target_predicate(Head, Entity, Predicate).
	target_predicate((Head --> _Body), Entity, Predicate) :-
		!,
		target_predicate(Head, Entity, Predicate).
	target_predicate(Head, Entity, Predicate) :-
		logtalk_load_context(entity_identifier, Entity),
		once((
			Predicate = Name/Arity
		;   Predicate = Name//Arity
		)),
		functor(Head, Name, Arity).

	target_predicate_clause_index(Term, Entity, Predicate, ClauseIndex) :-
		target_predicate(Term, Entity, Predicate),
		update_target_predicate_clause_index_(Predicate, ClauseIndex).

	target_scope_directive((:- Directive), Entity, Predicate) :-
		logtalk_load_context(entity_identifier, Entity),
		scope_directive_matches_predicate_(Directive, Predicate),
		!.

	target_scope_directive_index(Term, Entity, Predicate, DirectiveIndex) :-
		target_scope_directive(Term, Entity, Predicate),
		update_target_scope_directive_index_(Predicate, DirectiveIndex).

	target_predicate_directive((:- Directive), Entity, Predicate) :-
		logtalk_load_context(entity_identifier, Entity),
		predicate_directive_matches_predicate_(Directive, Predicate),
		!.

	target_predicate_directive_index(Term, Entity, Predicate, DirectiveIndex) :-
		target_predicate_directive(Term, Entity, Predicate),
		update_target_predicate_directive_index_(Predicate, DirectiveIndex).

	target_uses_directive((:- Directive), Entity, Predicate) :-
		logtalk_load_context(entity_identifier, Entity),
		uses_directive_matches_predicate_(Directive, Predicate),
		!.

	target_uses_directive_index(Term, Entity, Predicate, DirectiveIndex) :-
		target_uses_directive(Term, Entity, Predicate),
		update_target_uses_directive_index_(Predicate, DirectiveIndex).

	update_target_predicate_clause_index_(Predicate, ClauseIndex) :-
		(   ::retract(current_predicate_clause_index_(Predicate, Previous)) ->
			ClauseIndex is Previous + 1
		;   ::retractall(current_predicate_clause_index_(_, _)),
			ClauseIndex = 1
		),
		::assertz(current_predicate_clause_index_(Predicate, ClauseIndex)).

	update_target_scope_directive_index_(Predicate, DirectiveIndex) :-
		(   ::retract(current_scope_directive_index_(Predicate, Previous)) ->
			DirectiveIndex is Previous + 1
		;   DirectiveIndex = 1
		),
		::assertz(current_scope_directive_index_(Predicate, DirectiveIndex)).

	update_target_predicate_directive_index_(Predicate, DirectiveIndex) :-
		(   ::retract(current_predicate_directive_index_(Predicate, Previous)) ->
			DirectiveIndex is Previous + 1
		;   DirectiveIndex = 1
		),
		::assertz(current_predicate_directive_index_(Predicate, DirectiveIndex)).

	update_target_uses_directive_index_(Predicate, DirectiveIndex) :-
		(   ::retract(current_uses_directive_index_(Predicate, Previous)) ->
			DirectiveIndex is Previous + 1
		;   DirectiveIndex = 1
		),
		::assertz(current_uses_directive_index_(Predicate, DirectiveIndex)).

	scope_directive_matches_predicate_(public(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	scope_directive_matches_predicate_(protected(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	scope_directive_matches_predicate_(private(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).

	predicate_directive_matches_predicate_(alias(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(coinductive(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(discontiguous(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(dynamic(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(multifile(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(private(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(protected(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(public(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(synchronized(Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(uses(_Object, Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(use_module(_Module, Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).
	predicate_directive_matches_predicate_(info(Resource, _), Predicate) :-
		directive_resource_matches_predicate_(Resource, Predicate).
	predicate_directive_matches_predicate_(meta_predicate(Template), Predicate) :-
		template_matches_predicate_(Template, predicate, Predicate).
	predicate_directive_matches_predicate_(meta_non_terminal(Template), Predicate) :-
		template_matches_predicate_(Template, non_terminal, Predicate).
	predicate_directive_matches_predicate_(mode(Template, _), Predicate) :-
		template_matches_predicate_(Template, predicate, Predicate).
	predicate_directive_matches_predicate_(mode_non_terminal(Template, _), Predicate) :-
		template_matches_predicate_(Template, non_terminal, Predicate).

	uses_directive_matches_predicate_(uses(_Object, Resources), Predicate) :-
		directive_resources_match_predicate_(Resources, Predicate).

	directive_resources_match_predicate_(Resources, Predicate) :-
		(   Resources = [_| _] ->
			directive_resource_in_list_matches_predicate_(Resources, Predicate)
		;   directive_resource_matches_predicate_(Resources, Predicate)
		).

	directive_resource_in_list_matches_predicate_([Resource| _], Predicate) :-
		directive_resource_matches_predicate_(Resource, Predicate),
		!.
	directive_resource_in_list_matches_predicate_([_| Resources], Predicate) :-
		directive_resource_in_list_matches_predicate_(Resources, Predicate).

	directive_resource_matches_predicate_(Resource, Predicate) :-
		resource_exposed_predicate_(Resource, Predicate).

	resource_exposed_predicate_(Name/Arity, Name/Arity).
	resource_exposed_predicate_(Name//Arity, Name//Arity).
	resource_exposed_predicate_((_Original as Alias), Predicate) :-
		alias_exposed_predicate_(Alias, Predicate).
	resource_exposed_predicate_((_Original::Alias), Predicate) :-
		alias_exposed_predicate_(Alias, Predicate).

	alias_exposed_predicate_(Name/Arity, Name/Arity) :-
		!.
	alias_exposed_predicate_(Name//Arity, Name//Arity) :-
		!.
	alias_exposed_predicate_(Alias, Predicate) :-
		functor(Alias, Name, Arity),
		Predicate = Name/Arity.

	template_matches_predicate_(Template, predicate, Name/Arity) :-
		functor(Template, Name, Arity).
	template_matches_predicate_(Template, non_terminal, Name//Arity) :-
		functor(Template, Name, Arity).

	next_occurrence(Occurrence) :-
		(   ::retract(seen_(Previous)) ->
			true
		;   Previous = 0
		),
		Occurrence is Previous + 1,
		::assertz(seen_(Occurrence)).

:- end_category.
