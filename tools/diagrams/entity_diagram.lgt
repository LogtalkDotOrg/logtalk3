%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(entity_diagram(Format),
	imports(diagram(Format))).

	:- info([
		version is 2:60:0,
		author is 'Paulo Moura',
		date is 2024-12-04,
		comment is 'Predicates for generating entity diagrams in the specified format with both inheritance and cross-referencing relation edges.',
		parameters is ['Format' - 'Graph language file format.'],
		see_also is [inheritance_diagram(_), uses_diagram(_), xref_diagram(_), library_diagram(_)]
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), one).
	:- info(file/2, [
		comment is 'Creates a diagram for all entities in a loaded source file using the specified options. The file can be specified by name, basename, full path, or using library notation.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a loaded source file using default options. The file can be specified by name, basename, full path, or using library notation.',
		argnames is ['File']
	]).

	:- private(included_entity_/1).
	:- dynamic(included_entity_/1).
	:- mode(included_entity_(?entity_identifier), zero_or_more).
	:- info(included_entity_/1, [
		comment is 'Table of Logtalk entities already included in the diagram.',
		argnames is ['Entity']
	]).

	:- private(included_module_/1).
	:- dynamic(included_module_/1).
	:- mode(included_module_(?module_identifier), zero_or_more).
	:- info(included_module_/1, [
		comment is 'Table of Prolog modules already included in the diagram.',
		argnames is ['Module']
	]).

	:- private(referenced_entity_/2).
	:- dynamic(referenced_entity_/2).
	:- mode(referenced_entity_(?entity_identifier, ?entity_identifier), zero_or_more).
	:- info(referenced_entity_/2, [
		comment is 'Table of referenced Logtalk entities in the diagram.',
		argnames is ['Referencer', 'Entity']
	]).

	:- private(referenced_module_/2).
	:- dynamic(referenced_module_/2).
	:- mode(referenced_module_(?entity_identifier, ?module_identifier), zero_or_more).
	:- info(referenced_module_/2, [
		comment is 'Table of referenced Logtalk entities in the diagram.',
		argnames is ['Referencer', 'Entity']
	]).

	file(Source, UserOptions) :-
		self(Self),
		^^check_options(UserOptions),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, file, Source)),
		^^format_object(Format),
		^^locate_file(Source, Basename, Extension, Directory, Path),
		atom_concat(Name, Extension, Basename),
		^^merge_options(UserOptions, Options),
		::reset,
		^^output_file_path(Name, Options, Format, OutputPath),
		^^omit_path_prefix(Path, Options, Relative),
		^^diagram_caption(file, Relative, Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Basename, [description(Description)| Options]),
			atom_concat(file_, Path, Identifier),
			^^add_link_options(Path, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, Basename, file, GraphOptions),
			process(Basename, Directory, GraphOptions),
			output_externals(Options),
			^^output_edges([container(Identifier)| Options]),
			output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, Basename, file, GraphOptions),
			Format::file_footer(diagram_output_file, Basename, [description(Description)| Options]) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, file, Source))
		;	% failure is usually caused by errors in the source itself
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::file(Source, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(Options).

	file(Source) :-
		file(Source, []).

	output_file(File, Basename, Directory, _) :-
		\+ object_property(_, file(Basename, Directory)),
		\+ protocol_property(_, file(Basename, Directory)),
		\+ category_property(_, file(Basename, Directory)),
		\+ modules_diagram_support::module_property(_, file(File)),
		!.
	output_file(File, Basename, Directory, Options) :-
		(	member(file_labels(true), Options) ->
			^^format_object(Format),
			% use the full path for the cluster identifier as we
			% can have more than file with the same basename
			atom_concat(file_, File, Identifier),
			^^filter_file_extension(Basename, Options, Name),
			^^add_link_options(File, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, Name, file, GraphOptions),
			process(Basename, Directory, [container(Identifier)| Options]),
			Format::graph_footer(diagram_output_file, Identifier, Name, file, GraphOptions)
		;	process(Basename, Directory, Options)
		).

	remember_referenced_entity(Referencer, Entity) :-
		(	referenced_entity_(Referencer, Entity) ->
			true
		;	functor(Referencer, ReferencerFunctor, ReferencerArity),
			functor(ReferencerTemplate, ReferencerFunctor, ReferencerArity),
			functor(Entity, EntityFunctor, EntityArity),
			functor(EntityTemplate, EntityFunctor, EntityArity),
			assertz(referenced_entity_(ReferencerTemplate, EntityTemplate))
		).

	remember_referenced_module(Referencer, Module) :-
		(	referenced_module_(Referencer, Module) ->
			true
		;	assertz(referenced_module_(Referencer, Module))
		).

	reset :-
		^^reset,
		retractall(included_entity_(_)),
		retractall(included_module_(_)),
		retractall(referenced_entity_(_, _)),
		retractall(referenced_module_(_, _)).

	output_externals(Options) :-
		member(externals(false), Options),
		!.
	output_externals(Options) :-
		referenced_entity_(_, Entity),
		\+ included_entity_(Entity),
		add_external_entity_documentation_url(logtalk, Entity, Options, EntityOptions),
		entity_name_kind_caption(external, Entity, Name, Kind, Caption),
		\+ ::node_(Name, _, _, _, _, _),
		^^output_node(Name, Name, Caption, [], Kind, [tooltip(Caption)| EntityOptions]),
		fail.
	output_externals(Options) :-
		referenced_module_(_, Module),
		\+ included_module_(Module),
		add_external_entity_code_url(module, Module, Options, EntityOptions),
		\+ ::node_(Module, _, _, _, _, _),
		^^output_node(Module, Module, module, [], external_module, [tooltip(module)| EntityOptions]),
		fail.
	output_externals(_).

	output_missing_externals(Options) :-
		member(externals(false), Options),
		!.
	output_missing_externals(Options) :-
		referenced_entity_(_, Entity),
		entity_name_kind_caption(external, Entity, Name, Kind, Caption),
		\+ ::node_(Name, _, _, _, _, _),
		^^output_node(Name, Name, Caption, [], Kind, [tooltip(Caption)| Options]),
		fail.
	output_missing_externals(Options) :-
		referenced_module_(_, Module),
		\+ ::node_(Module, _, _, _, _, _),
		^^output_node(Module, Module, module, [], external_module, [tooltip(module)| Options]),
		fail.
	output_missing_externals(_).

	% use the {}/1 control construct to avoid a warning due to the circular
	% reference between this object and the xref_diagram object
	output_sub_diagrams(Options) :-
		parameter(1, Format),
		^^option(zoom(true), Options),
		{xref_diagram(Format)::default_option(layout(Layout))},
		included_entity_(Entity),
		{xref_diagram(Format)::entity(Entity, [layout(Layout)| Options])},
		fail.
	output_sub_diagrams(Options) :-
		parameter(1, Format),
		^^option(zoom(true), Options),
		{xref_diagram(Format)::default_option(layout(Layout))},
		included_module_(Module),
		{xref_diagram(Format)::entity(Module, [layout(Layout)| Options])},
		fail.
	output_sub_diagrams(_).

	process(Basename, Directory, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		protocol_property(Protocol, file(Basename, Directory)),
		\+ member(Protocol, ExcludedEntities),
		add_entity_documentation_url(logtalk, Protocol, Options, ProtocolOptions),
		output_protocol(Protocol, ProtocolOptions),
		assertz(included_entity_(Protocol)),
		fail.
	process(Basename, Directory, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		object_property(Object, file(Basename, Directory)),
		\+ member(Object, ExcludedEntities),
		add_entity_documentation_url(logtalk, Object, Options, ObjectOptions),
		output_object(Object, ObjectOptions),
		assertz(included_entity_(Object)),
		fail.
	process(Basename, Directory, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		category_property(Category, file(Basename, Directory)),
		\+ member(Category, ExcludedEntities),
		add_entity_documentation_url(logtalk, Category, Options, CategoryOptions),
		output_category(Category, CategoryOptions),
		assertz(included_entity_(Category)),
		fail.
	process(Basename, Directory, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		atom_concat(Directory, Basename, Path),
		modules_diagram_support::module_property(Module, file(Path)),
		\+ member(Module, ExcludedEntities),
		add_entity_documentation_url(module, Module, Options, ModuleOptions),
		output_module(Module, ModuleOptions),
		assertz(included_module_(Module)),
		fail.
	process(_, _, _).

	add_entity_documentation_url(Kind, Entity, Options, EntityOptions) :-
		^^option(urls(_, DocPrefix), Options),
		(	DocPrefix \== '' ->
			entity_to_html_name(Kind, Entity, Name),
			^^option(entity_url_suffix_target(Suffix, _), Options),
			atomic_list_concat([DocPrefix, Name, Suffix], DocURL),
			EntityOptions = [url(DocURL)| Options]
		;	EntityOptions = [url('')| Options]
		).

	add_external_entity_documentation_url(module, Entity, Options, EntityOptions) :-
		modules_diagram_support::module_property(Entity, file(Path)),
		(	member(path_url_prefixes(Prefix, _, DocPrefix), Options),
			DocPrefix \== '',
			sub_atom(Path, 0, _, _, Prefix) ->
			entity_to_html_name(module, Entity, Name),
			^^option(entity_url_suffix_target(Suffix, _), Options),
			atomic_list_concat([DocPrefix, Name, Suffix], DocURL),
			EntityOptions = [url(DocURL)| Options]
		;	EntityOptions = [url('')| Options]
		).
	add_external_entity_documentation_url(logtalk, Entity, Options, EntityOptions) :-
		(	current_object(Entity) ->
			object_property(Entity, file(Path))
		;	current_category(Entity) ->
			category_property(Entity, file(Path))
		;	atom(Entity),
			current_protocol(Entity) ->
			protocol_property(Entity, file(Path))
		;	% entity is not loaded
			fail
		),
		(	(	member(path_url_prefixes(Prefix, _, DocPrefix), Options),
				DocPrefix \== '',
				sub_atom(Path, 0, _, _, Prefix)
			;	member(url_prefixes(_, DocPrefix), Options)
			) ->
			entity_to_html_name(logtalk, Entity, Name),
			^^option(entity_url_suffix_target(Suffix, _), Options),
			atomic_list_concat([DocPrefix, Name, Suffix], DocURL),
			EntityOptions = [url(DocURL)| Options]
		;	EntityOptions = [url('')| Options]
		).

	entity_to_html_name(module, Entity, Entity).
	entity_to_html_name(logtalk, Entity, Name) :-
		functor(Entity, Functor, Arity),
		atomic_list_concat([Functor, '_', Arity], Name).

	add_external_entity_code_url(Kind, Entity, Options, EntityOptions) :-
		(	% first, find the file defining the entity
			(	Kind == object ->
				object_property(Entity, file(Path))
			;	Kind == category ->
				category_property(Entity, file(Path))
			;	Kind == protocol ->
				protocol_property(Entity, file(Path))
			;	{atom(Entity), current_module(Entity)} ->
				modules_diagram_support::module_property(Entity, file(Path))
			;	% entity is not loaded
				fail
			),
			% second, find the code URL prefix, looking for a path
			% specific prefix before considering the generic prefix
			(	member(path_url_prefixes(Prefix, CodePrefix, _), Options),
				sub_atom(Path, 0, _, _, Prefix) ->
				true
			;	member(url_prefixes(CodePrefix, _), Options)
			) ->
			% third, cut down when specified local path prefix
			% before constructing the final code URL
			^^option(omit_path_prefixes(PathPrefixes), Options),
			(	member(PathPrefix, PathPrefixes),
				atom_concat(PathPrefix, RelativePath, Path) ->
				true
			;	RelativePath = Path
			),
			atom_concat(CodePrefix, RelativePath, CodeURL),
			EntityOptions = [url(CodeURL)| Options]
		;	% could not find entity file or URL prefixes not defined
			EntityOptions = [url('')| Options]
		).

	output_protocol(Protocol, Options) :-
		^^ground_entity_identifier(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Resources0))
		;	Resources0 = []
		),
		fix_non_terminals(Resources0, protocol, Protocol, Resources),
		protocol_name_kind_caption(Protocol, Name, Kind, Caption),
		(	protocol_property(Protocol, declares(_, _)) ->
			% use the {}/1 control construct to avoid a warning do to the circular
			% reference between this object and the xref_diagram object
			{xref_diagram::diagram_name_suffix(Suffix0)},
			atom_concat('_protocol', Suffix0, Suffix),
			^^add_node_zoom_option(Name, Suffix, Options, NodeOptions)
		;	% no locally declared predicates; xref diagram empty
			NodeOptions = Options
		),
		^^output_node(Name, Name, Caption, Resources, Kind, [tooltip(Caption)| NodeOptions]),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		^^ground_entity_identifier(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(PublicPredicates0)),
			fix_non_terminals(PublicPredicates0, object, Object, PublicPredicates),
			findall(
				ToName::Predicate,
				(	object_property(Object, provides(Predicate0, To, _)),
					(	current_object(To) ->
						object_property(To, declares(Predicate0, Properties)),
						^^ground_entity_identifier(object, To, ToName)
					;	category_property(To, declares(Predicate0, Properties)),
						^^ground_entity_identifier(category, To, ToName)
					),
					(	member(non_terminal(NonTerminal), Properties) ->
						Predicate = NonTerminal
					;	Predicate = Predicate0
					)
				),
				MultifilePredicates
			),
			append(PublicPredicates, MultifilePredicates, Resources)
		;	Resources = []
		),
		object_name_kind_caption(Object, Name, Kind, Caption),
		(	(	object_property(Object, declares(_, _))
			;	object_property(Object, provides(_, _, _))
			;	object_property(Object, defines(_, DefinesProperties)),
				memberchk(number_of_clauses(NumberOfClauses), DefinesProperties),
				NumberOfClauses > 0
			) ->
			% use the {}/1 control construct to avoid a warning do to the circular
			% reference between this object and the xref_diagram object
			{xref_diagram::diagram_name_suffix(Suffix0)},
			atom_concat('_object', Suffix0, Suffix),
			^^add_node_zoom_option(Name, Suffix, Options, NodeOptions)
		;	% no locally declared/defined/provided predicates; xref diagram empty
			NodeOptions = Options
		),
		^^output_node(Name, Name, Caption, Resources, Kind, [tooltip(Caption)| NodeOptions]),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		^^ground_entity_identifier(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(PublicPredicates0)),
			fix_non_terminals(PublicPredicates0, category, Category, PublicPredicates),
			findall(
				ToName::Predicate,
				(	category_property(Category, provides(Predicate0, To, _)),
					(	current_object(To) ->
						object_property(To, declares(Predicate0, Properties)),
						^^ground_entity_identifier(object, To, ToName)
					;	category_property(To, declares(Predicate0, Properties)),
						^^ground_entity_identifier(category, To, ToName)
					),
					(	member(non_terminal(NonTerminal), Properties) ->
						Predicate = NonTerminal
					;	Predicate = Predicate0
					)
				),
				MultifilePredicates
			),
			append(PublicPredicates, MultifilePredicates, Resources)
		;	Resources = []
		),
		category_name_kind_caption(Category, Name, Kind, Caption),
		(	(	category_property(Category, declares(_, _))
			;	category_property(Category, provides(_, _, _))
			;	category_property(Category, defines(_, DefinesProperties)),
				memberchk(number_of_clauses(NumberOfClauses), DefinesProperties),
				NumberOfClauses > 0
			) ->
			% use the {}/1 control construct to avoid a warning do to the circular
			% reference between this object and the xref_diagram object
			{xref_diagram::diagram_name_suffix(Suffix0)},
			atom_concat('_category', Suffix0, Suffix),
			^^add_node_zoom_option(Name, Suffix, Options, NodeOptions)
		;	% no locally declared/defined/provided predicates; xref diagram empty
			NodeOptions = Options
		),
		^^output_node(Name, Name, Caption, Resources, Kind, [tooltip(Caption)| NodeOptions]),
		output_category_relations(Category, Options).

	output_module(Module, Options) :-
		(	member(interface(true), Options) ->
			modules_diagram_support::module_property(Module, exports(ExportedPredicates)),
			findall(
				To:Predicate,
				modules_diagram_support::module_property(Module, provides(Predicate, To, _)),
				MultifilePredicates
			),
			append(ExportedPredicates, MultifilePredicates, Resources)
		;	Resources = []
		),
		(	(	MultifilePredicates = [_| _]
			;	member(_/_, ExportedPredicates)
			;	modules_diagram_support::module_property(Module, calls(_, _))
			;	modules_diagram_support::module_property(Module, defines(_, _))
			) ->
			% use the {}/1 control construct to avoid a warning do to the circular
			% reference between this object and the xref_diagram object
			{xref_diagram::diagram_name_suffix(Suffix0)},
			atom_concat('_module', Suffix0, Suffix),
			^^add_node_zoom_option(Module, Suffix, Options, NodeOptions)
		;	% no locally exported or defined predicates; xref diagram empty
			NodeOptions = Options
		),
		^^output_node(Module, Module, module, Resources, module, [tooltip(module)| NodeOptions]),
		output_module_relations(Module, Options).

	fix_non_terminals([], _, _, []).
	fix_non_terminals([Resource0| Resources0], Kind, Entity, [Resource| Resources]) :-
		fix_non_terminal(Resource0, Kind, Entity, Resource),
		fix_non_terminals(Resources0, Kind, Entity, Resources).

	fix_non_terminal(Functor/Arity, Kind, Entity, Predicate) :-
		!,
		(	Kind == object ->
			object_property(Entity, declares(Functor/Arity, Properties))
		;	Kind == category ->
			category_property(Entity, declares(Functor/Arity, Properties))
		;	% Kind == protocol,
			protocol_property(Entity, declares(Functor/Arity, Properties))
		),
		(	member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;	Predicate = Functor/Arity
		).
	fix_non_terminal(Resource, _, _, Resource).

	output_protocol_relations(Protocol, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_protocol_inheritance_relations(Protocol, Options)
		;	true
		).

	output_protocol_inheritance_relations(Protocol, Options) :-
		extends_protocol(Protocol, ExtendedProtocol, Scope),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(protocol, ExtendedProtocol, ExtendedProtocolName),
		scope_relation_label(Scope, extends, Label),
		^^save_edge(ProtocolName, ExtendedProtocolName, [Label], extends_protocol, [tooltip(Label)| Options]),
		remember_referenced_entity(Protocol, ExtendedProtocol),
		fail.
	output_protocol_inheritance_relations(_, _).

	output_object_relations(Object, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_object_inheritance_relations(Object, Options)
		;	true
		),
		(	member(provide_relations(true), Options) ->
			output_object_provide_relations(Object, Options)
		;	true
		),
		(	member(xref_relations(true), Options) ->
			output_object_xref_relations(Object, Options)
		;	member(xref_calls(true), Options) ->
			output_object_xref_calls(Object, Options)
		;	true
		).

	output_object_inheritance_relations(Object, Options) :-
		implements_protocol(Object, Protocol, Scope),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		scope_relation_label(Scope, implements, Label),
		^^save_edge(ObjectName, ProtocolName, [Label], implements_protocol, [tooltip(Label)| Options]),
		remember_referenced_entity(Object, Protocol),
		fail.
	output_object_inheritance_relations(Instance, Options) :-
		instantiates_class(Instance, Class, Scope),
		^^ground_entity_identifier(object, Instance, InstanceName),
		^^ground_entity_identifier(object, Class, ClassName),
		scope_relation_label(Scope, instantiates, Label),
		^^save_edge(InstanceName, ClassName, [Label], instantiates_class, [tooltip(Label)| Options]),
		remember_referenced_entity(Instance, Class),
		fail.
	output_object_inheritance_relations(Class, Options) :-
		specializes_class(Class, SuperClass, Scope),
		^^ground_entity_identifier(object, Class, ClassName),
		^^ground_entity_identifier(object, SuperClass, SuperClassName),
		scope_relation_label(Scope, specializes, Label),
		^^save_edge(ClassName, SuperClassName, [Label], specializes_class, [tooltip(Label)| Options]),
		remember_referenced_entity(Class, SuperClass),
		fail.
	output_object_inheritance_relations(Prototype, Options) :-
		extends_object(Prototype, Parent, Scope),
		^^ground_entity_identifier(object, Prototype, PrototypeName),
		^^ground_entity_identifier(object, Parent, ParentName),
		scope_relation_label(Scope, extends, Label),
		^^save_edge(PrototypeName, ParentName, [Label], extends_object, [tooltip(Label)| Options]),
		remember_referenced_entity(Prototype, Parent),
		fail.
	output_object_inheritance_relations(Object, Options) :-
		imports_category(Object, Category, Scope),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(category, Category, CategoryName),
		scope_relation_label(Scope, imports, Label),
		^^save_edge(ObjectName, CategoryName, [Label], imports_category, [tooltip(Label)| Options]),
		remember_referenced_entity(Object, Category),
		fail.
	output_object_inheritance_relations(_, _).

	output_object_provide_relations(Object, Options) :-
		setof(
			Predicate,
			Properties^object_property(Object, provides(Predicate, To, Properties)),
			_
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		(	current_object(To) ->
			^^ground_entity_identifier(object, To, ToName)
		;	current_category(To) ->
			^^ground_entity_identifier(category, To, ToName)
		;	% unknown entity type (entity not loaded)
			^^ground_entity_identifier(unknown, To, ToName)
		),
		^^save_edge(ObjectName, ToName, [provides], provides_clauses, [tooltip(provides)| Options]),
		remember_referenced_entity(Object, To),
		fail.
	output_object_provide_relations(_, _).

	output_object_xref_relations(Object, Options) :-
		object_property(Object, calls(Other::_, _)),
		nonvar(Other),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(object, Other, OtherName),
		\+ ^^edge(ObjectName, OtherName, [uses], calls_predicate, _),
		^^save_edge(ObjectName, OtherName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Object, Other),
		fail.
	output_object_xref_relations(Object, Options) :-
		object_property(Object, calls(':'(Module,_), _)),
		nonvar(Module),
		\+ referenced_module_(Object, Module),
		^^ground_entity_identifier(object, Object, ObjectName),
		\+ ^^edge(ObjectName, Module, [use_module], calls_predicate, _),
		^^save_edge(ObjectName, Module, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Object, Module),
		fail.
	output_object_xref_relations(_, _).

	output_object_xref_calls(Object, Options) :-
		setof(
			Predicate,
			Predicate0^Properties^NonTerminal^(
				object_property(Object, calls(Other::Predicate0, Properties)),
				nonvar(Other),
				(	member(non_terminal(NonTerminal), Properties) ->
					Predicate = NonTerminal
				;	Predicate = Predicate0
				)
			),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(object, Other, OtherName),
		^^save_edge(ObjectName, OtherName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Object, Other),
		fail.
	output_object_xref_calls(Object, Options) :-
		setof(
			Predicate,
			Properties^(object_property(Object, calls(':'(Module,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(ObjectName, Module, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Object, Module),
		fail.
	output_object_xref_calls(_, _).

	output_category_relations(Category, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_category_inheritance_relations(Category, Options)
		;	true
		),
		(	member(provide_relations(true), Options) ->
			output_category_provide_relations(Category, Options)
		;	true
		),
		(	member(xref_relations(true), Options) ->
			output_category_xref_relations(Category, Options)
		;	member(xref_calls(true), Options) ->
			output_category_xref_calls(Category, Options)
		;	true
		).

	output_category_inheritance_relations(Category, Options) :-
		extends_category(Category, ExtendedCategory, Scope),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(category, ExtendedCategory, ExtendedCategoryName),
		scope_relation_label(Scope, extends, Label),
		^^save_edge(CategoryName, ExtendedCategoryName, [Label], extends_category, [tooltip(Label)| Options]),
		remember_referenced_entity(Category, ExtendedCategory),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		implements_protocol(Category, Protocol, Scope),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		scope_relation_label(Scope, implements, Label),
		^^save_edge(CategoryName, ProtocolName, [Label], implements_protocol, [tooltip(Label)| Options]),
		remember_referenced_entity(Category, Protocol),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		complements_object(Category, Object),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(CategoryName, ObjectName, [complements], complements_object, [tooltip(complements)| Options]),
		remember_referenced_entity(Category, Object),
		fail.
	output_category_inheritance_relations(_, _).

	output_category_provide_relations(Category, Options) :-
		setof(
			Predicate,
			Properties^category_property(Category, provides(Predicate, To, Properties)),
			_
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		(	current_object(To) ->
			^^ground_entity_identifier(object, To, ToName)
		;	current_category(To) ->
			^^ground_entity_identifier(category, To, ToName)
		;	% unknown entity type (entity not loaded)
			^^ground_entity_identifier(unknown, To, ToName)
		),
		^^save_edge(CategoryName, ToName, [provides], provides_clauses, [tooltip(provides)| Options]),
		remember_referenced_entity(Category, To),
		fail.
	output_category_provide_relations(_, _).

	output_category_xref_relations(Category, Options) :-
		category_property(Category, calls(Object::_, _)),
		nonvar(Object),
		\+ referenced_entity_(Category, Object),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		\+ ^^edge(CategoryName, ObjectName, [uses], calls_predicate, _),
		^^save_edge(CategoryName, ObjectName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Category, Object),
		fail.
	output_category_xref_relations(Category, Options) :-
		category_property(Category, calls(':'(Module,_), _)),
		nonvar(Module),
		\+ referenced_module_(Category, Module),
		^^ground_entity_identifier(category, Category, CategoryName),
		\+ ^^edge(CategoryName, Module, [use_module], calls_predicate, _),
		^^save_edge(CategoryName, Module, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Category, Module),
		fail.
	output_category_xref_relations(_, _).

	output_category_xref_calls(Category, Options) :-
		setof(
			Predicate,
			Predicate0^Properties^NonTerminal^(
				category_property(Category, calls(Object::Predicate0, Properties)),
				nonvar(Object),
				(	member(non_terminal(NonTerminal), Properties) ->
					Predicate = NonTerminal
				;	Predicate = Predicate0
				)
			),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(CategoryName, ObjectName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Category, Object),
		fail.
	output_category_xref_calls(Category, Options) :-
		setof(
			Predicate,
			Properties^(category_property(Category, calls(':'(Module,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^save_edge(CategoryName, Module, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Category, Module),
		fail.
	output_category_xref_calls(_, _).

	output_module_relations(Module, Options) :-
		(	member(provide_relations(true), Options) ->
			output_module_provide_relations(Module, Options)
		;	true
		),
		(	member(xref_relations(true), Options) ->
			output_module_xref_relations(Module, Options)
		;	member(xref_calls(true), Options) ->
			output_module_xref_calls(Module, Options)
		;	true
		).

	output_module_provide_relations(Module, Options) :-
		setof(
			Predicate,
			Properties^(modules_diagram_support::module_property(Module, provides(Predicate, To, Properties))),
			_
		),
		^^save_edge(Module, To, [provides], provides_clauses, [tooltip(provides)| Options]),
		remember_referenced_module(Module, To),
		fail.
	output_module_provide_relations(_, _).

	output_module_xref_relations(Module, Options) :-
		modules_diagram_support::module_property(Module, calls(Object::_, _)),
		nonvar(Object),
		\+ referenced_entity_(Module, Object),
		^^ground_entity_identifier(object, Object, ObjectName),
		\+ ^^edge(Module, ObjectName, [uses], calls_predicate, _),
		^^save_edge(Module, ObjectName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Module, Object),
		fail.
	output_module_xref_relations(Module, Options) :-
		modules_diagram_support::module_property(Module, calls(':'(FromModule,_), _)),
		nonvar(FromModule),
		\+ referenced_module_(Module, FromModule),
		\+ ^^edge(Module, FromModule, [use_module], calls_predicate, _),
		^^save_edge(Module, FromModule, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Module, FromModule),
		fail.
	output_module_xref_relations(_, _).

	output_module_xref_calls(Module, Options) :-
		setof(
			Predicate,
			Properties^(modules_diagram_support::module_property(Module, calls(Object::Predicate, Properties)), nonvar(Object)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(Module, ObjectName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Module, Object),
		fail.
	output_module_xref_calls(Module, Options) :-
		setof(
			Predicate,
			Properties^(modules_diagram_support::module_property(Module, calls(':'(FromModule,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^save_edge(Module, FromModule, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Module, FromModule),
		fail.
	output_module_xref_calls(_, _).

	scope_relation_label(public, Relation, Relation).
	scope_relation_label(protected, Relation, Label) :-
		atom_concat(Relation, ' (protected)', Label).
	scope_relation_label(private, Relation, Label) :-
		atom_concat(Relation, ' (private)', Label).

	entity_name_kind_caption(Location, Entity, Name, Kind, Caption) :-
		(	current_object(Entity) ->
			object_name_kind_caption(Entity, Name, Kind0, Caption)
		;	current_category(Entity) ->
			category_name_kind_caption(Entity, Name, Kind0, Caption)
		;	atom(Entity),
			current_protocol(Entity),
			protocol_name_kind_caption(Entity, Name, Kind0, Caption)
		),
		(	Location == external ->
			atom_concat(external_, Kind0, Kind)
		;	Kind = Kind0
		).

	object_name_kind_caption(Entity, Name, Kind, Caption) :-
		^^ground_entity_identifier(object, Entity, Name),
		(	instantiates_class(Entity, Entity) ->
			Kind = instance_and_class,
			Caption0 = 'instance/class'
		;	specializes_class(Entity, _), instantiates_class(Entity, _) ->
			Kind = instance_and_class,
			Caption0 = 'instance/class'
		;	specializes_class(Entity, _) ->
			Kind = class,
			Caption0 = class
		;	instantiates_class(Entity, _) ->
			Kind = instance,
			Caption0 = instance
		;	Kind = prototype,
			Caption0 = prototype
		),
		(	object_property(Entity, built_in) ->
			atom_concat('built-in ', Caption0, Caption)
		;	object_property(Entity, (dynamic)) ->
			atom_concat('dynamic ', Caption0, Caption)
		;	Caption = Caption0
		).

	category_name_kind_caption(Entity, Name, category, Caption) :-
		^^ground_entity_identifier(category, Entity, Name),
		(	category_property(Entity, built_in) ->
			Caption = 'built-in category'
		;	category_property(Entity, (dynamic)) ->
			Caption = 'dynamic category'
		;	Caption = category
		).

	protocol_name_kind_caption(Entity, Name, protocol, Caption) :-
		^^ground_entity_identifier(protocol, Entity, Name),
		(	protocol_property(Entity, built_in) ->
			Caption = 'built-in protocol'
		;	protocol_property(Entity, (dynamic)) ->
			Caption = 'dynamic protocol'
		;	Caption = protocol
		).

	% by default, diagram layout is bottom to top:
	default_option(layout(bottom_to_top)).
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
	% by default, write inheritance links:
	default_option(inheritance_relations(true)).
	% by default, write provide links:
	default_option(provide_relations(true)).
	% by default, write cross-referencing links:
	default_option(xref_relations(true)).
	% by default, print entity relation labels:
	default_option(relation_labels(true)).
	% by default, write cross-referencing calls:
	default_option(xref_calls(false)).
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
	% by default, don't omit any prefix when printing paths:
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

	diagram_description('Entity diagram').

	diagram_name_suffix('_entity_diagram').

	message_diagram_description('entity').

:- end_object.



:- object(entity_diagram,
	extends(entity_diagram(dot))).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2014-01-01,
		comment is 'Predicates for generating entity diagrams in DOT format with both inheritance and cross-referencing relation edges.',
		see_also is [inheritance_diagram, uses_diagram, xref_diagram]
	]).

:- end_object.
