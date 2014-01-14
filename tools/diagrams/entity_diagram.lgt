%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(entity_diagram(Format),
	imports(diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/14,
		comment is 'Predicates for generating entity diagrams.',
		parnames is ['Format']
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

	:- private(referenced_entity_/1).
	:- dynamic(referenced_entity_/1).

	:- private(referenced_module_/1).
	:- dynamic(referenced_module_/1).

	file(Source, UserOptions) :-
		^^format_object(Format),
		^^locate_file(Source, Basename, Extension, Directory, Path),
		atom_concat(Name, Extension, Basename),
		^^merge_options(UserOptions, Options),
		reset,
		^^output_file_path(Name, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::file_header(output_file, Basename, Options),
		atom_concat(file_, Path, Identifier),
		^^linking_options(Path, Options, GraphOptions),
		Format::graph_header(output_file, Identifier, Basename, file, GraphOptions),
		process(Basename, Directory, Options),
		output_externals(Options),
		^^output_edges(Options),
		Format::graph_footer(output_file, Identifier, Basename, file, GraphOptions),
		Format::file_footer(output_file, Basename, Options),
		close(Stream).

	file(Source) :-
		file(Source, []).

	output_file(File, Basename, Directory, Options) :-
		(	member(file_labels(true), Options) ->
			^^format_object(Format),
			% use the full path for the cluster identifier as we
			% can have more than file with the same basename
			atom_concat(file_, File, Identifier),
			^^linking_options(File, Options, GraphOptions),
			Format::graph_header(output_file, Identifier, Basename, file, GraphOptions),
			process(Basename, Directory, Options),
			Format::graph_footer(output_file, Identifier, Basename, file, GraphOptions)
		;	process(Basename, Directory, Options)
		).

	remember_referenced_entity(Entity) :-
		(	referenced_entity_(Entity) ->
			true
		;	assertz(referenced_entity_(Entity))
		).

	remember_referenced_module(Module) :-
		(	referenced_module_(Module) ->
			true
		;	assertz(referenced_module_(Module))
		).

	reset :-
		^^reset,
		retractall(included_entity_(_)),
		retractall(referenced_entity_(_)),
		retractall(referenced_module_(_)).

	output_externals(_Options) :-
		retract(included_entity_(Entity)),
		retractall(referenced_entity_(Entity)),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_header(output_file, other, '(external entities)', external, [tooltip('(external entities)')| Options]),
		retract(referenced_entity_(Entity)),
		(	current_object(Entity) ->
			^^ground_entity_identifier(object, Entity, Name),
			(	\+ instantiates_class(Entity, _),
				\+ specializes_class(Entity, _) ->
				^^output_node(Name, Name, [], external_prototype, [tooltip(prototype)| Options])
			;	^^output_node(Name, Name, [], external_instance_or_class, [tooltip('instance/class')| Options])
			)
		;	current_category(Entity) ->
			^^ground_entity_identifier(category, Entity, Name),
			^^output_node(Name, Name, [], external_category, [tooltip(category)| Options])
		;	% current_protocol(Entity),
			^^ground_entity_identifier(protocol, Entity, Name),
			^^output_node(Name, Name, [], external_protocol, [tooltip(protocol)| Options])
		),
		fail.
	output_externals(Options) :-
		retract(referenced_module_(Module)),
		^^ground_entity_identifier(module, Module, Name),
		^^output_node(Name, Name, [], external_module, [tooltip(module)| Options]),
		fail.
	output_externals(Options) :-
		^^format_object(Format),
		Format::graph_footer(output_file, other, '(external entities)', external, [tooltip('(external entities)')| Options]).

	process(Basename, Directory, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		protocol_property(Protocol, file(Basename, Directory)),
		\+ member(Protocol, ExcludedEntities),
		output_protocol(Protocol, Options),
		assertz(included_entity_(Protocol)),
		fail.
	process(Basename, Directory, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		object_property(Object, file(Basename, Directory)),
		\+ member(Object, ExcludedEntities),
		output_object(Object, Options),
		assertz(included_entity_(Object)),
		fail.
	process(Basename, Directory, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		category_property(Category, file(Basename, Directory)),
		\+ member(Category, ExcludedEntities),
		output_category(Category, Options),
		assertz(included_entity_(Category)),
		fail.
	process(Basename, Directory, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		atom_concat(Directory, Basename, Path),
		prolog_modules_diagram_support::module_property(Module, file(Path)),
		\+ member(Module, ExcludedEntities),
		output_module(Module, Options),
		assertz(included_entity_(Module)),
		fail.
	process(_, _, _).

	output_protocol(Protocol, Options) :-
		^^ground_entity_identifier(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Resources))
		;	Resources = []
		),
		^^output_node(Name, Name, Resources, protocol, [tooltip(protocol)| Options]),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		^^ground_entity_identifier(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(PublicPredicates)),
			findall(
				To::Predicate,
				object_property(Object, provides(Predicate, To, _)),
				MultifilePredicates
			),
			append(PublicPredicates, MultifilePredicates, Resources)
		;	Resources = []
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			^^output_node(Name, Name, Resources, prototype, [tooltip(prototype)| Options])
		;	^^output_node(Name, Name, Resources, instance_or_class, [tooltip('instance/class')| Options])
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		^^ground_entity_identifier(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(PublicPredicates)),
			findall(
				To::Predicate,
				category_property(Category, provides(Predicate, To, _)),
				MultifilePredicates
			),
			append(PublicPredicates, MultifilePredicates, Resources)
		;	Resources = []
		),
		^^output_node(Name, Name, Resources, category, [tooltip(category)| Options]),
		output_category_relations(Category, Options).

	output_module(Module, Options) :-
		^^ground_entity_identifier(module, Module, Name),
		(	member(interface(true), Options) ->
			prolog_modules_diagram_support::module_property(Module, exports(Resources))
		;	Resources = []
		),
		^^output_node(Name, Name, Resources, module, [tooltip(module)| Options]).

	output_protocol_relations(Protocol, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_protocol_inheritance_relations(Protocol, Options)
		;	true
		),
		(	member(xref_relations(true), Options) ->
			output_protocol_xref_relations(Protocol, Options)
		;	member(xref_calls(true), Options) ->
			output_protocol_xref_calls(Protocol, Options)
		;	true
		).

	output_protocol_inheritance_relations(Protocol, Options) :-
		extends_protocol(Protocol, ExtendedProtocol, Scope),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(protocol, ExtendedProtocol, ExtendedProtocolName),
		scope_relation_label(Scope, extends, Label),
		^^save_edge(ProtocolName, ExtendedProtocolName, [Label], extends_protocol, [tooltip(Label)| Options]),
		remember_referenced_entity(ExtendedProtocol),
		fail.
	output_protocol_inheritance_relations(_, _).

	output_protocol_xref_relations(Protocol, Options) :-
		protocol_property(Protocol, calls(Other::_, _)),
		nonvar(Other),
		\+ referenced_entity_(Other),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(object, Other, OtherName),
		\+ ^^edge(ProtocolName, OtherName, [uses], calls_predicate, _),
		^^save_edge(ProtocolName, OtherName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Other),
		fail.
	output_protocol_xref_relations(Protocol, Options) :-
		protocol_property(Protocol, calls(':'(Module,_), _)),
		nonvar(Module),
		\+ referenced_module_(Module),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(module, Module, ModuleName),
		\+ ^^edge(ProtocolName, ModuleName, [use_module], calls_predicate, _),
		^^save_edge(ProtocolName, ModuleName, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Module),
		fail.
	output_protocol_xref_relations(_, _).

	output_protocol_xref_calls(Protocol, Options) :-
		setof(
			Predicate,
			Properties^(protocol_property(Protocol, calls(Other::Predicate, Properties)), nonvar(Other)),
			Predicates
		),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(object, Other, OtherName),
		^^save_edge(ProtocolName, OtherName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Other),
		fail.
	output_protocol_xref_calls(Protocol, Options) :-
		setof(
			Predicate,
			Properties^(protocol_property(Protocol, calls(':'(Module,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^save_edge(ProtocolName, ModuleName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Module),
		fail.
	output_protocol_xref_calls(_, _).

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
		remember_referenced_entity(Protocol),
		fail.
	output_object_inheritance_relations(Instance, Options) :-
		instantiates_class(Instance, Class, Scope),
		^^ground_entity_identifier(object, Instance, InstanceName),
		^^ground_entity_identifier(object, Class, ClassName),
		scope_relation_label(Scope, instantiates, Label),
		^^save_edge(InstanceName, ClassName, [Label], instantiates_class, [tooltip(Label)| Options]),
		remember_referenced_entity(Class),
		fail.
	output_object_inheritance_relations(Class, Options) :-
		specializes_class(Class, SuperClass, Scope),
		^^ground_entity_identifier(object, Class, ClassName),
		^^ground_entity_identifier(object, SuperClass, SuperClassName),
		scope_relation_label(Scope, specializes, Label),
		^^save_edge(ClassName, SuperClassName, [Label], specializes_class, [tooltip(Label)| Options]),
		remember_referenced_entity(SuperClass),
		fail.
	output_object_inheritance_relations(Prototype, Options) :-
		extends_object(Prototype, Parent, Scope),
		^^ground_entity_identifier(object, Prototype, PrototypeName),
		^^ground_entity_identifier(object, Parent, ParentName),
		scope_relation_label(Scope, extends, Label),
		^^save_edge(PrototypeName, ParentName, [Label], extends_object, [tooltip(Label)| Options]),
		remember_referenced_entity(Parent),
		fail.
	output_object_inheritance_relations(Object, Options) :-
		imports_category(Object, Category, Scope),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(category, Category, CategoryName),
		scope_relation_label(Scope, imports, Label),
		^^save_edge(ObjectName, CategoryName, [Label], imports_category, [tooltip(Label)| Options]),
		remember_referenced_entity(Category),
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
		remember_referenced_entity(To),
		fail.
	output_object_provide_relations(_, _).

	output_object_xref_relations(Object, Options) :-
		object_property(Object, calls(Other::_, _)),
		nonvar(Other),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(object, Other, OtherName),
		\+ ^^edge(ObjectName, OtherName, [uses], calls_predicate, _),
		^^save_edge(ObjectName, OtherName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Other),
		fail.
	output_object_xref_relations(Object, Options) :-
		object_property(Object, calls(':'(Module,_), _)),
		nonvar(Module),
		\+ referenced_module_(Module),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(module, Module, ModuleName),
		\+ ^^edge(ObjectName, ModuleName, [use_module], calls_predicate, _),
		^^save_edge(ObjectName, ModuleName, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Module),
		fail.
	output_object_xref_relations(_, _).

	output_object_xref_calls(Object, Options) :-
		setof(
			Predicate,
			Properties^(object_property(Object, calls(Other::Predicate, Properties)), nonvar(Other)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(object, Other, OtherName),
		^^save_edge(ObjectName, OtherName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Other),
		fail.
	output_object_xref_calls(Object, Options) :-
		setof(
			Predicate,
			Properties^(object_property(Object, calls(':'(Module,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^save_edge(ObjectName, ModuleName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Module),
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
		remember_referenced_entity(ExtendedCategory),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		implements_protocol(Category, Protocol, Scope),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		scope_relation_label(Scope, implements, Label),
		^^save_edge(CategoryName, ProtocolName, [Label], implements_protocol, [tooltip(Label)| Options]),
		remember_referenced_entity(Protocol),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		complements_object(Category, Object),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(ObjectName, CategoryName, [complements], complements_object, [tooltip(complements)| Options]),
		remember_referenced_entity(Object),
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
		remember_referenced_entity(To),
		fail.
	output_category_provide_relations(_, _).

	output_category_xref_relations(Category, Options) :-
		category_property(Category, calls(Object::_, _)),
		nonvar(Object),
		\+ referenced_entity_(Object),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		\+ ^^edge(CategoryName, ObjectName, [uses], calls_predicate, _),
		^^save_edge(CategoryName, ObjectName, [uses], calls_predicate, [tooltip(uses)| Options]),
		remember_referenced_entity(Object),
		fail.
	output_category_xref_relations(Category, Options) :-
		category_property(Category, calls(':'(Module,_), _)),
		nonvar(Module),
		\+ referenced_module_(Module),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(module, Module, ModuleName),
		\+ ^^edge(CategoryName, ModuleName, [use_module], calls_predicate, _),
		^^save_edge(CategoryName, ModuleName, [use_module], calls_predicate, [tooltip(use_module)| Options]),
		remember_referenced_module(Module),
		fail.
	output_category_xref_relations(_, _).

	output_category_xref_calls(Category, Options) :-
		setof(
			Predicate,
			Properties^(category_property(Category, calls(Object::Predicate, Properties)), nonvar(Object)),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^save_edge(CategoryName, ObjectName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_entity(Object),
		fail.
	output_category_xref_calls(Category, Options) :-
		setof(
			Predicate,
			Properties^(category_property(Category, calls(':'(Module,Predicate), Properties)), nonvar(Module)),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^save_edge(CategoryName, ModuleName, Predicates, calls_predicate, [tooltip(calls)| Options]),
		remember_referenced_module(Module),
		fail.
	output_category_xref_calls(_, _).

	scope_relation_label(public, Relation, Relation).
	scope_relation_label(protected, Relation, Label) :-
		atom_concat(Relation, ' (protected)', Label).
	scope_relation_label(private, Relation, Label) :-
		atom_concat(Relation, ' (private)', Label).

	% by default, diagram title is empty:
	default_option(title('')).
	% by default, print current date:
	default_option(date(true)).
	% by default, print entity public predicates:
	default_option(interface(true)).
	% by default, print file labels:
	default_option(file_labels(true)).
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
	% by default, write diagram to the current directory:
	default_option(output_directory('./')).
	% by default, don't exclude any source files:
	default_option(exclude_files([])).
	% by default, don't exclude any library sub-directories:
	default_option(exclude_libraries([])).
	% by default, don't exclude any entities:
	default_option(exclude_entities([])).
	% by default, don't generate cluster URLs:
	default_option(url_protocol('')).
	% by default, don't omit a path prefix when printing paths:
	default_option(omit_path_prefix('')).

	diagram_name_suffix('_entity_diagram').

	% auxiliary predicates; we could use the Logtalk standard library but we
	% prefer to make this object self-contained given its documenting purpose

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.



:- object(entity_diagram,
	extends(entity_diagram(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Predicates for generating entity diagrams in DOT format.'
	]).

:- end_object.
