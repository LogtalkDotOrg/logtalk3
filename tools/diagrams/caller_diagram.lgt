%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(caller_diagram(Format),
	imports(diagram(Format))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-12,
		comment is 'Predicates for generating caller diagrams showing direct and indirect callers of a predicate or a non-terminal.',
		parameters is [
			'Format' - 'Graph language file format.'
		],
		see_also is [xref_diagram(_), entity_diagram(_)]
	]).

	:- public(predicate/2).
	:- mode(predicate(+qualified_predicate_indicator, +list(compound)), one).
	:- info(predicate/2, [
		comment is 'Creates a caller diagram for the given predicate or non-terminal using the specified options. Predicates are specified as ``Entity::Name/Arity``. Non-terminals are specified as ``Entity::Name//Arity``.',
		argnames is ['QualifiedIndicator', 'Options']
	]).

	:- public(predicate/1).
	:- mode(predicate(+qualified_predicate_indicator), one).
	:- info(predicate/1, [
		comment is 'Creates a caller diagram for the given predicate using default options. Predicates are specified as ``Entity::Name/Arity``. Non-terminals are specified as ``Entity::Name//Arity``.',
		argnames is ['QualifiedIndicator']
	]).

	:- private(included_caller_/1).
	:- dynamic(included_caller_/1).
	:- mode(included_caller_(?qualified_predicate_indicator), zero_or_more).
	:- mode(included_caller_(?predicate_indicator), zero_or_more).
	:- info(included_caller_/1, [
		comment is 'Table of callers already included in the diagram.',
		argnames is ['Caller']
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	predicate(QualifiedIndicator, UserOptions) :-
		^^check_options(UserOptions),
		decompose_qualified_indicator(QualifiedIndicator, Entity, Kind, Functor, Arity, Indicator),
		self(Self),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, predicate, QualifiedIndicator)),
		^^format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		predicate_diagram_name(Entity, Functor, Arity, DiagramName),
		^^output_file_path(DiagramName, Options, Format, OutputPath),
		qualified_indicator_description(QualifiedIndicator, Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, DiagramName, [description(Description)| Options]),
			Format::graph_header(diagram_output_file, DiagramName, DiagramName, entity, Options),
			process_callers(Kind, Entity, Indicator, Options),
			^^output_edges(Options),
			Format::graph_footer(diagram_output_file, DiagramName, DiagramName, entity, Options),
			Format::file_footer(diagram_output_file, DiagramName, [description(Description)| Options]) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, predicate, QualifiedIndicator))
		;	logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::predicate(QualifiedIndicator, UserOptions)))
		),
		close(Stream).

	predicate(QualifiedIndicator) :-
		predicate(QualifiedIndicator, []).

	decompose_qualified_indicator(Entity::Name//Arity, Entity, Kind, Name, A, Name/A) :-
		!,
		A is Arity + 2,
		entity_kind(Entity, Kind).
	decompose_qualified_indicator(Entity::Name/Arity, Entity, Kind, Name, Arity, Name/Arity) :-
		entity_kind(Entity, Kind).

	entity_kind(Entity, object) :-
		current_object(Entity),
		!.
	entity_kind(Entity, category) :-
		current_category(Entity),
		!.

	predicate_diagram_name(Entity, Functor, Arity, DiagramName) :-
		(	atom(Entity) ->
			Name = Entity
		;	functor(Entity, EFunctor, EArity),
			atomic_list_concat([EFunctor, '_', EArity], Name)
		),
		atomic_list_concat([Name, '_', Functor, '_', Arity], DiagramName).

	qualified_indicator_description(Entity::Name//Arity, Description) :-
		!,
		ground_entity_name(Entity, EntityName),
		atomic_list_concat(['Caller diagram for non-terminal ', EntityName, '::', Name, '//', Arity], Description).
	qualified_indicator_description(Entity::Name/Arity, Description) :-
		ground_entity_name(Entity, EntityName),
		atomic_list_concat(['Caller diagram for predicate ', EntityName, '::', Name, '/', Arity], Description).

	ground_entity_name(Entity, Name) :-
		(	atom(Entity) ->
			Name = Entity
		;	functor(Entity, EFunctor, EArity),
			atomic_list_concat([EFunctor, '/', EArity], Name)
		).

	process_callers(Kind, Entity, Indicator, Options) :-
		% output the target predicate node
		ground_qualified_indicator(Kind, Entity, Indicator, QualifiedIndicator),
		^^output_node(QualifiedIndicator, QualifiedIndicator, target, [], public_predicate, Options),
		% find and output direct callers
		find_direct_callers(Kind, Entity, Indicator, Options, QualifiedIndicator),
		% find and output indirect callers
		find_indirect_callers(Options).

	ground_qualified_indicator(Kind, Entity, Indicator, GroundEntity::Indicator) :-
		^^ground_entity_identifier(Kind, Entity, GroundEntity).

	% find all entities that call the target predicate
	find_direct_callers(Kind, Entity, Indicator, Options, QualifiedIndicator) :-
		% local callers within the same entity
		entity_property(Kind, Entity, calls(Indicator, Properties)),
		memberchk(caller(Caller0), Properties),
		Caller0 \== Indicator,
		entity_property(Kind, Entity, defines(Caller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(Kind, Entity, Caller0, Caller),
		\+ included_caller_(Caller),
		assertz(included_caller_(Caller)),
		^^output_node(Caller, Caller, local, [], local_predicate, Options),
		^^save_edge(Caller, QualifiedIndicator, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_direct_callers(_, Entity, Indicator, Options, QualifiedIndicator) :-
		% callers from other objects
		current_object(Other),
		Other \= Entity,
		object_property(Other, calls(Entity::Indicator, Properties)),
		memberchk(caller(Caller0), Properties),
		object_property(Other, defines(Caller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(object, Other, Caller0, CallerIndicator),
		ground_qualified_indicator(object, Other, CallerIndicator, QualifiedCaller),
		\+ included_caller_(QualifiedCaller),
		assertz(included_caller_(QualifiedCaller)),
		^^output_node(QualifiedCaller, QualifiedCaller, external, [], external_predicate, Options),
		^^save_edge(QualifiedCaller, QualifiedIndicator, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_direct_callers(_, Entity, Indicator, Options, QualifiedIndicator) :-
		% callers from categories
		current_category(Category),
		Category \= Entity,
		category_property(Category, calls(Entity::Indicator, Properties)),
		memberchk(caller(Caller0), Properties),
		category_property(Category, defines(Caller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(category, Category, Caller0, CallerIndicator),
		ground_qualified_indicator(category, Category, CallerIndicator, QualifiedCaller),
		\+ included_caller_(QualifiedCaller),
		assertz(included_caller_(QualifiedCaller)),
		^^output_node(QualifiedCaller, QualifiedCaller, external, [], external_predicate, Options),
		^^save_edge(QualifiedCaller, QualifiedIndicator, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_direct_callers(_, _, _, _, _).

	% find indirect callers (callers of callers)
	find_indirect_callers(Options) :-
		included_caller_(Caller),
		Caller = CallerEntity::CallerIndicator,
		current_object(Other),
		object_property(Other, calls(CallerEntity::CallerIndicator, Properties)),
		memberchk(caller(IndirectCaller0), Properties),
		object_property(Other, defines(IndirectCaller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(object, Other, IndirectCaller0, IndirectCallerIndicator),
		ground_qualified_indicator(object, Other, IndirectCallerIndicator, QualifiedIndirectCaller),
		\+ included_caller_(QualifiedIndirectCaller),
		assertz(included_caller_(QualifiedIndirectCaller)),
		^^output_node(QualifiedIndirectCaller, QualifiedIndirectCaller, external, [], external_predicate, Options),
		^^save_edge(QualifiedIndirectCaller, Caller, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_indirect_callers(Options) :-
		included_caller_(Caller),
		Caller = CallerEntity::CallerIndicator,
		current_category(Category),
		category_property(Category, calls(CallerEntity::CallerIndicator, Properties)),
		memberchk(caller(IndirectCaller0), Properties),
		category_property(Category, defines(IndirectCaller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(category, Category, IndirectCaller0, IndirectCallerIndicator),
		ground_qualified_indicator(category, Category, IndirectCallerIndicator, QualifiedIndirectCaller),
		\+ included_caller_(QualifiedIndirectCaller),
		assertz(included_caller_(QualifiedIndirectCaller)),
		^^output_node(QualifiedIndirectCaller, QualifiedIndirectCaller, external, [], external_predicate, Options),
		^^save_edge(QualifiedIndirectCaller, Caller, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_indirect_callers(Options) :-
		% also look for local callers of local callers within any entity
		included_caller_(Caller),
		Caller = CallerEntity::CallerIndicator,
		(	current_object(CallerEntity) ->
			CallerKind = object
		;	current_category(CallerEntity) ->
			CallerKind = category
		;	fail
		),
		entity_property(CallerKind, CallerEntity, calls(CallerIndicator, Properties)),
		memberchk(caller(IndirectCaller0), Properties),
		IndirectCaller0 \== CallerIndicator,
		entity_property(CallerKind, CallerEntity, defines(IndirectCaller0, CallerProperties)),
		\+ member(auxiliary, CallerProperties),
		caller_node(CallerKind, CallerEntity, IndirectCaller0, IndirectCallerIndicator),
		ground_qualified_indicator(CallerKind, CallerEntity, IndirectCallerIndicator, QualifiedIndirectCaller),
		\+ included_caller_(QualifiedIndirectCaller),
		assertz(included_caller_(QualifiedIndirectCaller)),
		^^output_node(QualifiedIndirectCaller, QualifiedIndirectCaller, external, [], external_predicate, Options),
		^^save_edge(QualifiedIndirectCaller, Caller, [calls], calls_predicate, [tooltip(calls)| Options]),
		fail.
	find_indirect_callers(_).

	caller_node(Kind, Entity, Caller0, Caller) :-
		(	Caller0 = (From::Predicate) ->
			% multifile predicate caller
			(	current_object(From) ->
				FromKind = object
			;	FromKind = category
			),
			entity_property(FromKind, From, declares(Predicate, DeclaresProperties)),
			(	member(non_terminal(NonTerminal), DeclaresProperties) ->
				Caller = (From::NonTerminal)
			;	Caller = (From::Predicate)
			)
		;	Caller0 == (:-)/1 ->
			Caller = Caller0
		;	entity_property(Kind, Entity, defines(Caller0, DefinesProperties)),
			(	member(non_terminal(CallerNonTerminal), DefinesProperties) ->
				Caller = CallerNonTerminal
			;	Caller = Caller0
			)
		).

	entity_property(object, Entity, Property) :-
		object_property(Entity, Property).
	entity_property(category, Entity, Property) :-
		category_property(Entity, Property).

	reset :-
		^^reset,
		retractall(included_caller_(_)).

	output_externals(_).

	output_missing_externals(_).

	output_sub_diagrams(_).

	% by default, diagram layout is left to right (callers on the left, target on the right):
	default_option(layout(left_to_right)).
	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, don't print Logtalk and backend version data:
	default_option(versions(false)).
	% by default, print entity public predicates:
	default_option(interface(true)).
	% by default, print file labels:
	default_option(file_labels(true)).
	% by default, print file name extensions:
	default_option(file_extensions(true)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, print external nodes:
	default_option(externals(true)).
	% by default, print node type captions:
	default_option(node_type_captions(true)).
	% by default, write diagram to the current directory:
	default_option(output_directory('./dot_dias')).
	% by default, don't exclude any directories:
	default_option(exclude_directories([])).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, exclude only the "startup" and "scratch_directory" libraries:
	default_option(exclude_libraries([startup, scratch_directory])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, omit only the HOME path prefix when printing paths:
	default_option(omit_path_prefixes(Prefixes)) :-
		(	logtalk::expand_library_path(home, Home) ->
			Prefixes = [Home]
		;	Prefixes = []
		).
	% by default, use a '.html' suffix for entity documentation URLs:
	default_option(entity_url_suffix_target('.html', '#')).
	% by default, don't link to sub-diagrams:
	default_option(zoom(false)).
	% by default, use a '.svg' extension for linked diagrams
	default_option(zoom_url_suffix('.svg')).

	diagram_description('Caller diagram').

	diagram_name_suffix('_caller_diagram').

	message_diagram_description('caller diagram').

:- end_object.



:- object(caller_diagram,
	extends(caller_diagram(dot))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-12,
		comment is 'Predicates for generating caller diagrams in DOT format.',
		see_also is [xref_diagram, entity_diagram]
	]).

:- end_object.
