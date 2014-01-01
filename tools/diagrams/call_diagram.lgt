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


:- object(call_diagram(Format),
	imports(diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/01,
		comment is 'Predicates for generating predicate call diagrams.',
		argnames is ['Format']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), one).
	:- info(file/2, [
		comment is 'Creates a predicate call diagram for all entities in a loaded source file using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a predicate call diagram for all entities in a loaded source file using default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File']
	]).

	:- private(included_entity_/1).
	:- dynamic(included_entity_/1).

	:- private(referenced_entity_/1).
	:- dynamic(referenced_entity_/1).

	:- private(referenced_module_/1).
	:- dynamic(referenced_module_/1).

	rlibrary(Library, UserOptions) :-
		reset_external_entities,
		^^rlibrary(Library, UserOptions).

	output_rlibrary(Library, Path, Options) :-
		^^output_rlibrary(Library, Path, Options),
		output_external_entities(Options).

	library(Library, UserOptions) :-
		reset_external_entities,
		^^library(Library, UserOptions).

	output_library(Library, Path, Options) :-
		^^output_library(Library, Path, Options),
		output_external_entities(Options).

	files(Project, Files, UserOptions) :-
		reset_external_entities,
		^^files(Project, Files, UserOptions).

	file(Source, UserOptions) :-
		^^format_object(Format),
		^^locate_file(Source, Basename, Extension, Directory, Path),
		atom_concat(Name, Extension, Basename),
		merge_options(UserOptions, Options),
		output_file_path(Name, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		reset_external_entities,
		output_file(Path, Basename, Directory, Options),
		output_external_entities(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	file(Source) :-
		file(Source, []).

	output_file(File, Basename, Directory, Options) :-
		^^format_object(Format),
		% use the full path for the cluster identifier as we
		% can have more than file with the same basename
		Format::graph_header(output_file, File, Basename, file, Options),
		process(Basename, Directory, Options),
		Format::graph_footer(output_file, File, Basename, file, Options).

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

	reset_external_entities :-
		retractall(included_entity_(_)),
		retractall(referenced_entity_(_)),
		retractall(referenced_module_(_)).

	output_external_entities(_Options) :-
		retract(included_entity_(Entity)),
		retractall(referenced_entity_(Entity)),
		fail.
	output_external_entities(Options) :-
		^^format_object(Format),
		Format::graph_header(output_file, other, '(other referenced entities)', external, Options),
		retract(referenced_entity_(Entity)),
		(	current_object(Entity) ->
			^^ground_entity_identifier(object, Entity, Name),
			(	\+ instantiates_class(Object, _),
				\+ specializes_class(Object, _) ->
				^^output_node(Name, Name, [], external_prototype, Options)
			;	^^output_node(Name, Name, [], external_instance_or_class, Options)
			)
		;	current_category(Entity) ->
			^^ground_entity_identifier(category, Entity, Name),
			^^output_node(Name, Name, [], external_category, Options)
		;	% current_protocol(Entity),
			^^ground_entity_identifier(protocol, Entity, Name),
			^^output_node(Name, Name, [], external_protocol, Options)
		),
		fail.
	output_external_entities(Options) :-
		retract(referenced_module_(Module)),
		^^ground_entity_identifier(module, Module, Name),
		^^output_node(Name, Name, [], external_module, Options),
		fail.
	output_external_entities(Options) :-
		^^format_object(Format),
		Format::graph_footer(output_file, other, '(other referenced entities)', external, Options).

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
	process(_, _, _).

	output_protocol(Protocol, Options) :-
		^^ground_entity_identifier(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Resources))
		;	Resources = []
		),
		^^output_node(Name, Name, Resources, protocol, Options),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		^^ground_entity_identifier(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(Resources))
		;	Resources = []
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			^^output_node(Name, Name, Resources, prototype, Options)
		;	^^output_node(Name, Name, Resources, instance_or_class, Options)
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		^^ground_entity_identifier(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(Resources))
		;	Resources = []
		),
		^^output_node(Name, Name, Resources, category, Options),
		output_category_relations(Category, Options).

	output_protocol_relations(Protocol, Options) :-
		setof(
			Predicate,
			Properties^(protocol_property(Protocol, calls(Other::Predicate, Properties)), nonvar(Other), \+ referenced_entity_(Other)),
			Predicates
		),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(object, Other, OtherName),
		^^output_edge(ProtocolName, OtherName, Predicates, calls_predicate, Options),
		remember_referenced_entity(Other),
		fail.
	output_protocol_relations(Protocol, Options) :-
		setof(
			Predicate,
			Properties^(protocol_property(Protocol, calls(':'(Module,Predicate), Properties)), nonvar(Module), \+ referenced_module_(Module)),
			Predicates
		),
		^^ground_entity_identifier(protocol, Protocol, ProtocolName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^output_edge(ProtocolName, ModuleName, Predicates, calls_predicate, Options),
		remember_referenced_module(Module),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Object, Options) :-
		setof(
			Predicate,
			Properties^(object_property(Object, calls(Other::Predicate, Properties)), nonvar(Other), \+ referenced_entity_(Other)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(object, Other, OtherName),
		^^output_edge(ObjectName, OtherName, Predicates, calls_predicate, Options),
		remember_referenced_entity(Other),
		fail.
	output_object_relations(Object, Options) :-
		setof(
			Predicate,
			Properties^(object_property(Object, calls(':'(Module,Predicate), Properties)), nonvar(Module), \+ referenced_module_(Module)),
			Predicates
		),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^output_edge(ObjectName, ModuleName, Predicates, calls_predicate, Options),
		remember_referenced_module(Module),
		fail.
	output_object_relations(_, _).

	output_category_relations(Category, Options) :-
		setof(
			Predicate,
			Properties^(category_property(Category, calls(Object::Predicate, Properties)), nonvar(Object), \+ referenced_entity_(Object)),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(object, Object, ObjectName),
		^^output_edge(CategoryName, ObjectName, Predicates, calls_predicate, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(Category, Options) :-
		setof(
			Predicate,
			Properties^(category_property(Category, calls(':'(Module,Predicate), Properties)), nonvar(Module), \+ referenced_module_(Module)),
			Predicates
		),
		^^ground_entity_identifier(category, Category, CategoryName),
		^^ground_entity_identifier(module, Module, ModuleName),
		^^output_edge(CategoryName, ModuleName, Predicates, calls_predicate, Options),
		remember_referenced_module(Module),
		fail.
	output_category_relations(_, _).

	merge_options(UserOptions, Options) :-
		% by default, print current date:
		(member(date(Date), UserOptions) -> true; Date = true),
		% by default, print entity public predicates:
		(member(interface(Interface), UserOptions) -> true; Interface = false),
		% by default, write diagram to the current directory:
		(member(relation_labels(Relations), UserOptions) -> true; Relations = true),
		% by default, write diagram to the current directory:
		(member(output_path(OutputPath), UserOptions) -> true; OutputPath = './'),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; ExcludedFiles = []),
		% by default, don't exclude any library sub-directories:
		(member(exclude_libraries(ExcludedLibraries), UserOptions) -> true; ExcludedLibraries = []),
		% by default, don't exclude any entities:
		(member(exclude_entities(ExcludedEntities), UserOptions) -> true; ExcludedEntities = []),
		Options = [
			date(Date), interface(Interface), relation_labels(Relations),
			output_path(OutputPath),
			exclude_files(ExcludedFiles), exclude_libraries(ExcludedLibraries), exclude_entities(ExcludedEntities)].

	output_file_path(Name0, Options, Format, OutputPath) :-
		atom_concat(Name0, '_call_diagram', Name),
		^^output_file_path(Name, Options, Format, OutputPath).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
