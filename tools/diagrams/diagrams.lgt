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


:- category(diagram).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/05,
		comment is 'Predicates for generating entity diagram files for source files and libraries.'
	]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for all entities in a library its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for all entities in a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates a diagram for all entities in a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for all entities in a library using default options.',
		argnames is ['Library']
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

	:- public(default_options/1).
	:- mode(default_options(-list), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']
	]).

	:- protected(output_file_name/2).
	:- mode(output_file_name(+atom, -atom), one).
	:- info(output_file_name/2, [
		comment is 'Constructs the the diagram file name.',
		argnames is ['Name', 'File']
	]).

	:- protected(output_file_header/2).
	:- mode(output_file_header(+stream_or_alias, +list), one).
	:- info(output_file_header/2, [
		comment is 'Writes the output file header using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(output_file_footer/2).
	:- mode(output_file_footer(+stream_or_alias, +list), one).
	:- info(output_file_footer/2, [
		comment is 'Writes the output file footer using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(graph_header/4).
	:- mode(graph_header(+stream_or_alias, +atom, +atom, +list), one).
	:- info(graph_header/4, [
		comment is 'Writes a graph header using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- protected(graph_footer/4).
	:- mode(graph_footer(+stream_or_alias, +atom, +atom, +list), one).
	:- info(graph_footer/4, [
		comment is 'Writes a graph footer using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- protected(subgraph_header/4).
	:- mode(subgraph_header(+stream_or_alias, +atom, +atom, +list), one).
	:- info(subgraph_header/4, [
		comment is 'Writes a subgraph header using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- protected(subgraph_footer/4).
	:- mode(subgraph_footer(+stream_or_alias, +atom, +atom, +list), one).
	:- info(subgraph_footer/4, [
		comment is 'Writes a subgraph footer using the specified options.',
		argnames is ['Stream', 'Identifier', 'Label', 'Options']
	]).

	:- protected(externals_subgraph_header/2).
	:- mode(externals_subgraph_header(+stream_or_alias, +list), one).
	:- info(externals_subgraph_header/2, [
		comment is 'Writes a subgraph header using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(externals_subgraph_footer/2).
	:- mode(externals_subgraph_footer(+stream_or_alias, +list), one).
	:- info(externals_subgraph_footer/2, [
		comment is 'Writes a subgraph footer using the specified options.',
		argnames is ['Stream', 'Options']
	]).

	:- protected(node/4).
	:- mode(node(+stream_or_alias, +atom, +list(predicate_indicator), +atom), one).
	:- info(node/4, [
		comment is 'Writes an arrow between two nodes using the specified options.',
		argnames is ['Stream', 'Label', 'Predicates', 'Kind']
	]).

	:- protected(arrow/5).
	:- mode(arrow(+stream_or_alias, +atom, +atom, +atom, +list), one).
	:- info(arrow/5, [
		comment is 'Writes an arrow between two nodes using the specified options.',
		argnames is ['Stream', 'Start', 'End', 'Label', 'Options']
	]).

	:- private(included_entity_/1).
	:- dynamic(included_entity_/1).

	:- private(referenced_entity_/1).
	:- dynamic(referenced_entity_/1).

	rlibrary(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopDirectory),
		::output_file_name(Library, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		::output_file_header(output_file, Options),
		reset_external_entities,
		output_rlibrary(TopDirectory, Options),
		output_external_entities(Options),
		::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopDirectory, Options) :-
		::graph_header(output_file, TopDirectory, TopDirectory, Options),
		member(exclude_paths(ExcludedPaths), Options),
		forall(
			sub_library(TopDirectory, ExcludedPaths, RelativePath, Path),
			output_library(RelativePath, Path, Options)),
		::graph_footer(output_file, TopDirectory, TopDirectory, Options).

	sub_library(TopDirectory, ExcludedPaths, RelativePath, Path) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopDirectory, RelativePath, Path),
		\+ member(RelativePath, ExcludedPaths).

	library(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		::output_file_name(Library, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		::output_file_header(output_file, Options),
		reset_external_entities,
		output_library(Path, Path, Options),
		output_external_entities(Options),
		::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	library(Library) :-
		library(Library, []).

	output_library(RelativePath, Path, Options) :-
		(	member(library_paths(true), Options) ->
			::subgraph_header(output_file, RelativePath, RelativePath, Options),
			output_library_files(Path, Options),
			::subgraph_footer(output_file, RelativePath, RelativePath, Options)
		;	output_library_files(Path, Options)
		).

	output_library_files(Directory, Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		output_file(Basename, Directory, Options),
		fail.
	output_library_files(_, _).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		atom_concat(Source1, '.lgt', Path),
		\+ member(Source1, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source2, '.logtalk', Path),
		\+ member(Source2, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source3, '.lgt', Basename),
		\+ member(Source3, [ExcludedFile| ExcludedFiles]),
		atom_concat(Source4, '.logtalk', Basename),
		\+ member(Source4, [ExcludedFile| ExcludedFiles]).

	files(Project, Paths) :-
		files(Project, Paths, []).

	files(Project, Paths, UserOptions) :-
		merge_options(UserOptions, Options),
		::output_file_name(Project, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		::output_file_header(output_file, Options),
		reset_external_entities,
		output_files(Paths, Options),
		::output_file_footer(output_file, Options),
		close(Stream),
		os::change_directory(Current).

	output_files([], _Options).
	output_files([Path| Paths], Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		not_excluded_file(ExcludedFiles, Path, Basename),
		process(Basename, Directory, Options),
		output_files(Paths, Options).

	file(Source, UserOptions) :-
		locate_file(Source, Basename, Directory),
		merge_options(UserOptions, Options),
		(	atom_concat(Source, '.lgt', Basename) ->
			true
		;	atom_concat(Source, '.logtalk', Basename)
		),
		::output_file_name(Source, OutputFile),
		member(output_path(OutputPath), Options),
		os::working_directory(Current),
		os::change_directory(OutputPath),
		open(OutputFile, write, Stream, [alias(output_file)]),
		::output_file_header(output_file, Options),
		reset_external_entities,
		output_file(Basename, Directory, Options),
		output_external_entities(Options),
		::output_file_footer(output_file, Options),
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
		(	member(file_names(true), Options) ->
			% use the full path for the cluster identifier as we
			% can have more than file with the same basename
			atom_concat(Directory, Basename, File),
			::subgraph_header(output_file, File, Basename, Options),
			process(Basename, Directory, Options),
			::subgraph_footer(output_file, File, Basename, Options)
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
		::externals_subgraph_header(output_file, Options),
		retract(referenced_entity_(Entity)),
		(	current_object(Entity) ->
			print_name(object, Entity, Name),
			(	\+ instantiates_class(Object, _),
				\+ specializes_class(Object, _) ->
				::node(output_file, Name, [], external_prototype)
			;	::node(output_file, Name, [], external_instance_or_class)
			)
		;	current_protocol(Entity) ->
			print_name(protocol, Entity, Name),
			::node(output_file, Name, [], external_protocol)
		;	print_name(category, Entity, Name),
			::node(output_file, Name, [], external_category)
		),
		fail.
	output_external_entities(Options) :-
		::externals_subgraph_footer(output_file, Options).

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
		print_name(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Predicates))
		;	true
		),
		::node(output_file, Name, Predicates, protocol),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		print_name(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(Predicates))
		;	true
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			::node(output_file, Name, Predicates, prototype)
		;	::node(output_file, Name, Predicates, instance_or_class)
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		print_name(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(Predicates))
		;	true
		),
		::node(output_file, Name, Predicates, category),
		output_category_relations(Category, Options).

	output_protocol_relations(Protocol, Options) :-
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		::arrow(output_file, ProtocolName, ExtendedProtocolName, extends, Options),
		remember_referenced_entity(ExtendedProtocol),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Object, Options) :-
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		::arrow(output_file, ObjectName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_object_relations(Instance, Options) :-
		instantiates_class(Instance, Class),
		print_name(object, Instance, InstanceName),
		print_name(object, Class, ClassName),
		::arrow(output_file, InstanceName, ClassName, instantiates, Options),
		remember_referenced_entity(Class),
		fail.
	output_object_relations(Class, Options) :-
		specializes_class(Class, SuperClass),
		print_name(object, Class, ClassName),
		print_name(object, SuperClass, SuperClassName),
		::arrow(output_file, ClassName, SuperClassName, specializes, Options),
		remember_referenced_entity(SuperClass),
		fail.
	output_object_relations(Prototype, Options) :-
		extends_object(Prototype, Parent),
		print_name(object, Prototype, PrototypeName),
		print_name(object, Parent, ParentName),
		::arrow(output_file, PrototypeName, ParentName, extends, Options),
		remember_referenced_entity(Parent),
		fail.
	output_object_relations(Object, Options) :-
		imports_category(Object, Category),
		print_name(object, Object, ObjectName),
		print_name(category, Category, CategoryName),
		::arrow(output_file, ObjectName, CategoryName, imports, Options),
		remember_referenced_entity(Category),
		fail.
	output_object_relations(Object, Options) :-
		object_property(Object, calls(Other::_, _)),
		\+ referenced_entity_(Other),
		print_name(object, Object, ObjectName),
		print_name(object, Other, OtherName),
		writeq(::arrow(output_file, ObjectName, OtherName, uses, Options)), nl,
		::arrow(output_file, ObjectName, OtherName, uses, Options),
		remember_referenced_entity(Other),
		fail.
	output_object_relations(Object, Options) :-
		object_property(Object, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(object, Object, ObjectName),
		print_name(module, Module, ModuleName),
		::arrow(output_file, ObjectName, ModuleName, use_module, Options),
		remember_referenced_entity(Module),
		fail.
	output_object_relations(_, _).

	output_category_relations(Category, Options) :-
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		::arrow(output_file, CategoryName, ExtendedCategoryName, extends, Options),
		remember_referenced_entity(ExtendedCategory),
		fail.
	output_category_relations(Category, Options) :-
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		::arrow(output_file, CategoryName, ProtocolName, implements, Options),
		remember_referenced_entity(Protocol),
		fail.
	output_category_relations(Category, Options) :-
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		::arrow(output_file, ObjectName, CategoryName, complements, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(Category, Options) :-
		category_property(Category, calls(Object::_, _)),
		\+ referenced_entity_(Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		::arrow(output_file, CategoryName, ObjectName, uses, Options),
		remember_referenced_entity(Object),
		fail.
	output_category_relations(Category, Options) :-
		category_property(Category, calls(':'(Module,_), _)),
		\+ referenced_entity_(Module),
		print_name(category, Category, CategoryName),
		print_name(module, Module, ModuleName),
		::arrow(output_file, CategoryName, ModuleName, use_module, Options),
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

	default_options(DefaultOptions) :-
		merge_options([], DefaultOptions).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_category.



:- object(diagram(_Format),
	implements(forwarding)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/05,
		comment is 'Generates entity diagram files for source files and libraries in the specified format.',
		argnames is ['Format']
	]).

	:- public(format_object/2).
	:- multifile(format_object/2).

	forward(Message) :-
		parameter(1, Format),
		format_object(Format, Object),
		[Object::Message].

:- end_object.
