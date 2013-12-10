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


:- object(entity_diagram(_Format),
	extends(diagram)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/09,
		comment is 'Predicates for generating entity diagrams.',
		argnames is ['Format']
	]).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list), one).
	:- info(files/3, [
		comment is 'Creates a diagram for all entities in a list of loaded source files using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files', 'Options']
	]).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates a diagram for all entities in a list of loaded source files using the default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files']
	]).

	:- public(file/2).
	:- mode(file(+atom, +list), one).
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

	all(UserOptions) :-
		findall(File, logtalk::loaded_file(File), Files),
		files(all, Files, UserOptions).

	rlibrary(Library, UserOptions) :-
		parameter(1, Format),
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopDirectory),
		Format::output_file_name(Library, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		reset_external_entities,
		output_rlibrary(TopDirectory, Options),
		output_external_entities(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	output_rlibrary(TopDirectory, Options) :-
		parameter(1, Format),
		Format::graph_header(output_file, TopDirectory, TopDirectory, [bgcolor(snow3)| Options]),
		member(exclude_paths(ExcludedPaths), Options),
		forall(
			sub_library(TopDirectory, ExcludedPaths, RelativePath, Path),
			output_library(RelativePath, Path, Options)),
		Format::graph_footer(output_file, TopDirectory, TopDirectory, [bgcolor(snow3)| Options]).

	sub_library(TopDirectory, ExcludedPaths, RelativePath, Path) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopDirectory, RelativePath, Path),
		\+ member(RelativePath, ExcludedPaths).

	library(Library, UserOptions) :-
		parameter(1, Format),
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		Format::output_file_name(Library, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		reset_external_entities,
		output_library(Path, Path, Options),
		output_external_entities(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	output_library(RelativePath, Path, Options) :-
		parameter(1, Format),
		(	member(library_paths(true), Options) ->
			Format::graph_header(output_file, RelativePath, RelativePath, [bgcolor(snow2)| Options]),
			output_library_files(Path, Options),
			Format::graph_footer(output_file, RelativePath, RelativePath, Options)
		;	output_library_files(Path, Options)
		).

	output_library_files(Directory, Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		output_file(Basename, Directory, Options),
		fail.
	output_library_files(_, _).

	files(Project, Paths) :-
		files(Project, Paths, []).

	files(Project, Paths, UserOptions) :-
		parameter(1, Format),
		merge_options(UserOptions, Options),
		Format::output_file_name(Project, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		reset_external_entities,
		output_files(Paths, Options),
		Format::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	output_files([], _Options).
	output_files([Path| Paths], Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		process(Basename, Directory, Options),
		output_files(Paths, Options).

	file(Source, UserOptions) :-
		parameter(1, Format),
		locate_file(Source, Basename, Directory),
		merge_options(UserOptions, Options),
		(	atom_concat(Source, '.lgt', Basename) ->
			true
		;	atom_concat(Source, '.logtalk', Basename)
		),
		Format::output_file_name(Source, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		reset_external_entities,
		output_file(Basename, Directory, Options),
		output_external_entities(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	file(Source) :-
		file(Source, []).

	% file given in library notation
	locate_file(LibraryNotation, Basename, Directory) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Basename, Directory).
	% file given using its name or basename
	locate_file(Source, Basename, Directory) :-
		add_extension(Source, Basename),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			logtalk::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% file given using a full path
	locate_file(Source, Basename, Directory) :-
		add_extension(Source, SourceWithExtension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		atom_concat(Directory, Basename, SourceWithExtension),
		!.

	add_extension(Source, SourceWithExtension) :-
		atom(Source),
		(	sub_atom(Source, _, 4, 0, '.lgt') ->
			SourceWithExtension = Source
		;	sub_atom(Source, _, 8, 0, '.logtalk') ->
			SourceWithExtension = Source
		;	(	atom_concat(Source, '.lgt', SourceWithExtension)
			;	atom_concat(Source, '.logtalk', SourceWithExtension)
			)
		).

	output_file(Basename, Directory, Options) :-
		parameter(1, Format),
		(	member(file_names(true), Options) ->
			% use the full path for the cluster identifier as we
			% can have more than file with the same basename
			atom_concat(Directory, Basename, File),
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
		parameter(1, Format),
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		Format::edge(output_file, ProtocolName, ExtendedProtocolName, extends, Options),
		remember_referenced_entity(ExtendedProtocol),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Object, Options) :-
		parameter(1, Format),
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		Format::edge(output_file, ObjectName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_object_relations(Instance, Options) :-
		parameter(1, Format),
		instantiates_class(Instance, Class),
		print_name(object, Instance, InstanceName),
		print_name(object, Class, ClassName),
		Format::edge(output_file, InstanceName, ClassName, instantiates, Options),
		remember_referenced_entity(Class),
		fail.
	output_object_relations(Class, Options) :-
		parameter(1, Format),
		specializes_class(Class, SuperClass),
		print_name(object, Class, ClassName),
		print_name(object, SuperClass, SuperClassName),
		Format::edge(output_file, ClassName, SuperClassName, specializes, Options),
		remember_referenced_entity(SuperClass),
		fail.
	output_object_relations(Prototype, Options) :-
		parameter(1, Format),
		extends_object(Prototype, Parent),
		print_name(object, Prototype, PrototypeName),
		print_name(object, Parent, ParentName),
		Format::edge(output_file, PrototypeName, ParentName, extends, Options),
		remember_referenced_entity(Parent),
		fail.
	output_object_relations(Object, Options) :-
		parameter(1, Format),
		imports_category(Object, Category),
		print_name(object, Object, ObjectName),
		print_name(category, Category, CategoryName),
		Format::edge(output_file, ObjectName, CategoryName, imports, Options),
		remember_referenced_entity(Category),
		fail.
	output_object_relations(Object, Options) :-
		parameter(1, Format),
		object_property(Object, calls(Other::_, _)),
		\+ referenced_entity_(Other),
		print_name(object, Object, ObjectName),
		print_name(object, Other, OtherName),
		Format::edge(output_file, ObjectName, OtherName, uses, Options),
		remember_referenced_entity(Other),
		fail.
	output_object_relations(Object, Options) :-
		parameter(1, Format),
		object_property(Object, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(object, Object, ObjectName),
		print_name(module, Module, ModuleName),
		Format::edge(output_file, ObjectName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_object_relations(_, _).

	output_category_relations(Category, Options) :-
		parameter(1, Format),
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		Format::edge(output_file, CategoryName, ExtendedCategoryName, extends, Options),
		remember_referenced_entity(ExtendedCategory),
		fail.
	output_category_relations(Category, Options) :-
		parameter(1, Format),
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		Format::edge(output_file, CategoryName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_category_relations(Category, Options) :-
		parameter(1, Format),
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		Format::edge(output_file, ObjectName, CategoryName, complements, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(Category, Options) :-
		parameter(1, Format),
		category_property(Category, calls(Object::_, _)),
		\+ referenced_entity_(Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		Format::edge(output_file, CategoryName, ObjectName, uses, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(Category, Options) :-
		parameter(1, Format),
		category_property(Category, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(category, Category, CategoryName),
		print_name(module, Module, ModuleName),
		Format::edge(output_file, CategoryName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_category_relations(_, _).

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
		% by default, don't print entity relation labels:
		(member(relation_labels(Relations), UserOptions) -> true; Relations = false),
		% by default, write diagram to the current directory:
		(member(output_path(OutputPath), UserOptions) -> true; os::working_directory(OutputPath)),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; ExcludedFiles = []),
		% by default, don't exclude any library sub-directories:
		(member(exclude_paths(ExcludedPaths), UserOptions) -> true; ExcludedPaths = []),
		% by default, don't exclude any entities:
		(member(exclude_entities(ExcludedEntities), UserOptions) -> true; ExcludedEntities = []),
		Options = [
			library_paths(LibraryPaths), file_names(FileNames), date(Date), interface(Interface), relation_labels(Relations),
			output_path(OutputPath),
			exclude_files(ExcludedFiles), exclude_paths(ExcludedPaths), exclude_entities(ExcludedEntities)].

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
