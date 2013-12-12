%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
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
	extends(diagram(Format))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/12,
		comment is 'Predicates for generating entity diagrams.',
		argnames is ['Format']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list(compound)), one).
	:- info(file/2, [
		comment is 'Creates a diagram for all entities in a loaded source file using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File', 'Options']
	]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a loaded source file using default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['File']
	]).

	:- private(included_entity_/1).
	:- dynamic(included_entity_/1).

	:- private(referenced_entity_/1).
	:- dynamic(referenced_entity_/1).

	rlibrary(Library, UserOptions) :-
		reset_external_entities,
		^^rlibrary(Library, UserOptions).

	output_rlibrary(TopDirectory, Options) :-
		^^output_rlibrary(TopDirectory, Options),
		output_external_entities(Options).

	library(Library, UserOptions) :-
		reset_external_entities,
		^^library(Library, UserOptions).

	output_library(RelativePath, Path, Options) :-
		^^output_library(RelativePath, Path, Options),
		output_external_entities(Options).

	files(Project, Files, UserOptions) :-
		reset_external_entities,
		^^files(Project, Files, UserOptions).

	file(Source, UserOptions) :-
		parameter(1, Format),
		::locate_file(Source, Basename, Directory, Path),
		merge_options(UserOptions, Options),
		output_file_path(all_files, Options, Format, OutputPath),
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
		parameter(1, Format),
		(	member(file_names(true), Options) ->
			% use the full path for the cluster identifier as we
			% can have more than file with the same basename
			Format::graph_header(output_file, File, Basename, [bgcolor(snow2)| Options]),
			process(Basename, Directory, Options),
			Format::graph_footer(output_file, File, Basename, [bgcolor(snow2)| Options])
		;	process(Basename, Directory, Options)
		).

	remember_referenced_entity(Entity) :-
		(	referenced_entity_(Entity) ->
			true
		;	assertz(referenced_entity_(Entity))
		).

	reset_external_entities :-
		retractall(included_entity_(_)),
		retractall(referenced_entity_(_)).

	output_external_entities(_Options) :-
		retract(included_entity_(Entity)),
		retractall(referenced_entity_(Entity)),
		fail.
	output_external_entities(Options) :-
		parameter(1, Format),
		Format::graph_header(output_file, other, '(other referenced entities)', [bgcolor(white)| Options]),
		retract(referenced_entity_(Entity)),
		(	current_object(Entity) ->
			print_name(object, Entity, Name),
			(	\+ instantiates_class(Object, _),
				\+ specializes_class(Object, _) ->
				Format::node(output_file, Name, Name, [], external_prototype, Options)
			;	Format::node(output_file, Name, Name, [], external_instance_or_class, Options)
			)
		;	current_protocol(Entity) ->
			print_name(protocol, Entity, Name),
			Format::node(output_file, Name, Name, [], external_protocol, Options)
		;	print_name(category, Entity, Name),
			Format::node(output_file, Name, Name, [], external_category, Options)
		),
		fail.
	output_external_entities(Options) :-
		parameter(1, Format),
		Format::graph_footer(output_file, other, '(other referenced entities)', [bgcolor(white)| Options]).

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
		parameter(1, Format),
		print_name(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Predicates)),
			entity_resources_to_atoms(Predicates, Atoms)
		;	Atoms = []
		),
		Format::node(output_file, Name, Name, Atoms, protocol, Options),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		parameter(1, Format),
		print_name(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(Predicates)),
			entity_resources_to_atoms(Predicates, Atoms)
		;	Atoms = []
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			Format::node(output_file, Name, Name, Atoms, prototype, Options)
		;	Format::node(output_file, Name, Name, Atoms, instance_or_class, Options)
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		parameter(1, Format),
		print_name(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(Predicates)),
			entity_resources_to_atoms(Predicates, Atoms)
		;	Atoms = []
		),
		Format::node(output_file, Name, Name, Atoms, category, Options),
		output_category_relations(Category, Options).

	output_protocol_relations(Protocol, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_protocol_inheritance_relations(Protocol, Options)
		;	true
		),
		(	member(cross_reference_relations(true), Options) ->
			output_protocol_cross_reference_relations(Protocol, Options)
		;	true
		).

	output_protocol_inheritance_relations(Protocol, Options) :-
		parameter(1, Format),
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		Format::edge(output_file, ProtocolName, ExtendedProtocolName, extends, Options),
		remember_referenced_entity(ExtendedProtocol),
		fail.
	output_protocol_inheritance_relations(_, _).

	output_protocol_cross_reference_relations(Protocol, Options) :-
		parameter(1, Format),
		protocol_property(Protocol, calls(Other::_, _)),
		\+ referenced_entity_(Other),
		print_name(protocol, Protocol, ProtocolName),
		print_name(object, Other, OtherName),
		Format::edge(output_file, ProtocolName, OtherName, uses, Options),
		remember_referenced_entity(Other),
		fail.
	output_protocol_cross_reference_relations(Protocol, Options) :-
		parameter(1, Format),
		protocol_property(Protocol, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(protocol, Protocol, ProtocolName),
		print_name(module, Module, ModuleName),
		Format::edge(output_file, ProtocolName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_protocol_cross_reference_relations(_, _).

	output_object_relations(Object, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_object_inheritance_relations(Object, Options)
		;	true
		),
		(	member(cross_reference_relations(true), Options) ->
			output_object_cross_reference_relations(Object, Options)
		;	true
		).

	output_object_inheritance_relations(Object, Options) :-
		parameter(1, Format),
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		Format::edge(output_file, ObjectName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_object_inheritance_relations(Instance, Options) :-
		parameter(1, Format),
		instantiates_class(Instance, Class),
		print_name(object, Instance, InstanceName),
		print_name(object, Class, ClassName),
		Format::edge(output_file, InstanceName, ClassName, instantiates, Options),
		remember_referenced_entity(Class),
		fail.
	output_object_inheritance_relations(Class, Options) :-
		parameter(1, Format),
		specializes_class(Class, SuperClass),
		print_name(object, Class, ClassName),
		print_name(object, SuperClass, SuperClassName),
		Format::edge(output_file, ClassName, SuperClassName, specializes, Options),
		remember_referenced_entity(SuperClass),
		fail.
	output_object_inheritance_relations(Prototype, Options) :-
		parameter(1, Format),
		extends_object(Prototype, Parent),
		print_name(object, Prototype, PrototypeName),
		print_name(object, Parent, ParentName),
		Format::edge(output_file, PrototypeName, ParentName, extends, Options),
		remember_referenced_entity(Parent),
		fail.
	output_object_inheritance_relations(Object, Options) :-
		parameter(1, Format),
		imports_category(Object, Category),
		print_name(object, Object, ObjectName),
		print_name(category, Category, CategoryName),
		Format::edge(output_file, ObjectName, CategoryName, imports, Options),
		remember_referenced_entity(Category),
		fail.
	output_object_inheritance_relations(_, _).

	output_object_cross_reference_relations(Object, Options) :-
		parameter(1, Format),
		object_property(Object, calls(Other::_, _)),
		\+ referenced_entity_(Other),
		print_name(object, Object, ObjectName),
		print_name(object, Other, OtherName),
		Format::edge(output_file, ObjectName, OtherName, uses, Options),
		remember_referenced_entity(Other),
		fail.
	output_object_cross_reference_relations(Object, Options) :-
		parameter(1, Format),
		object_property(Object, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(object, Object, ObjectName),
		print_name(module, Module, ModuleName),
		Format::edge(output_file, ObjectName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_object_cross_reference_relations(_, _).

	output_category_relations(Category, Options) :-
		(	member(inheritance_relations(true), Options) ->
			output_category_inheritance_relations(Category, Options)
		;	true
		),
		(	member(cross_reference_relations(true), Options) ->
			output_category_cross_reference_relations(Category, Options)
		;	true
		).

	output_category_inheritance_relations(Category, Options) :-
		parameter(1, Format),
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		Format::edge(output_file, CategoryName, ExtendedCategoryName, extends, Options),
		remember_referenced_entity(ExtendedCategory),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		parameter(1, Format),
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		Format::edge(output_file, CategoryName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_category_inheritance_relations(Category, Options) :-
		parameter(1, Format),
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		Format::edge(output_file, ObjectName, CategoryName, complements, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_inheritance_relations(_, _).

	output_category_cross_reference_relations(Category, Options) :-
		parameter(1, Format),
		category_property(Category, calls(Object::_, _)),
		\+ referenced_entity_(Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		Format::edge(output_file, CategoryName, ObjectName, uses, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_cross_reference_relations(Category, Options) :-
		parameter(1, Format),
		category_property(Category, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(category, Category, CategoryName),
		print_name(module, Module, ModuleName),
		Format::edge(output_file, CategoryName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_category_cross_reference_relations(_, _).

	print_name(object, Object, ObjectName) :-
		(	atom(Object) ->
			ObjectName = Object
		;	(	object_property(Object, info(Info)) ->
				parameter_names(Object, Info, Names)
			;	parameter_names(Object, [], Names)
			),
			Object =.. [Functor| _],
			ObjectName =.. [Functor| Names]
		).
	print_name(protocol, Protocol, Protocol).
	print_name(category, Category, CategoryName) :-
		(	atom(Category) ->
			CategoryName = Category
		;	(	category_property(Category, info(Info)) ->
				parameter_names(Category, Info, Names)
			;	parameter_names(Category, [], Names)
			),
			Category =.. [Functor| _],
			CategoryName =.. [Functor| Names]
		).

	parameter_names(Entity, Info, Names) :-
		(	member(parnames(Names), Info) ->
			true
		;	member(parameters(Parameters), Info) ->
			pairs::keys(Parameters, Names)
		;	Entity =.. [_| Names],
			variables_to_underscore(Names)
		).

	entity_resources_to_atoms([], []).
	entity_resources_to_atoms([Predicate| Predicates], [Atom| Atoms]) :-
		entity_resource_to_atom(Predicate, Atom),
		entity_resources_to_atoms(Predicates, Atoms).

	entity_resource_to_atom(Functor/Arity, Atom) :-
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Functor, '/', Atom0),
		atom_concat(Atom0, ArityAtom, Atom).
	entity_resource_to_atom(Functor//Arity, Atom) :-
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Functor, '//', Atom0),
		atom_concat(Atom0, ArityAtom, Atom).
	entity_resource_to_atom(op(Priority,Type,Operator), Atom) :-
		number_codes(Priority, PriorityCodes),
		atom_codes(PriorityAtom, PriorityCodes),
		atom_concat('op(', PriorityAtom, Atom0),
		atom_concat(Atom0, ',', Atom1),
		atom_concat(Atom1, Type, Atom2),
		atom_concat(Atom2, ',', Atom3),
		atom_concat(Atom3, Operator, Atom4),
		atom_concat(Atom4, ')', Atom).

	variables_to_underscore([]).
	variables_to_underscore([Arg| Args]) :-
		(	var(Arg) ->
			Arg = '_'
		;	true
		),
		variables_to_underscore(Args).

	merge_options(UserOptions, Options) :-
		% by default, print library paths:
		(member(library_paths(LibraryPaths), UserOptions) -> true; LibraryPaths = true),
		% by default, print file names:
		(member(file_names(FileNames), UserOptions) -> true; FileNames = true),
		% by default, print current date:
		(member(date(Date), UserOptions) -> true; Date = true),
		% by default, print entity public predicates:
		(member(interface(Interface), UserOptions) -> true; Interface = true),
		% by default, write inheritance links:
		(member(inheritance_relations(Inheritance), UserOptions) -> true; Inheritance = true),
		% by default, write cross-referenceing links:
		(member(cross_reference_relations(CrossReference), UserOptions) -> true; CrossReference = true),
		% by default, write diagram to the current directory:
		(member(relation_labels(Relations), UserOptions) -> true; Relations = false),
		% by default, write diagram to the current directory:
		(member(output_path(OutputPath), UserOptions) -> true; OutputPath = './'),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; ExcludedFiles = []),
		% by default, don't exclude any library sub-directories:
		(member(exclude_paths(ExcludedPaths), UserOptions) -> true; ExcludedPaths = []),
		% by default, don't exclude any entities:
		(member(exclude_entities(ExcludedEntities), UserOptions) -> true; ExcludedEntities = []),
		Options = [
			library_paths(LibraryPaths), file_names(FileNames), date(Date), interface(Interface), relation_labels(Relations),
			inheritance_relations(Inheritance), cross_reference_relations(CrossReference),
			output_path(OutputPath),
			exclude_files(ExcludedFiles), exclude_paths(ExcludedPaths), exclude_entities(ExcludedEntities)].

	output_file_path(Name0, Options, Format, OutputPath) :-
		atom_concat(Name0, '_entity_diagram', Name),
		^^output_file_path(Name, Options, Format, OutputPath).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
