%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(diagram).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2011/05/24,
		comment is 'Generates entity diagram DOT files for source files and libraries.']).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for all entities in a library its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for all entities in a library and its sub-libraries using default options.',
		argnames is ['Library']]).

	rlibrary(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopPath),
		atom_concat(Library, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_rlibrary(TopPath, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		write(dot_file, 'subgraph "cluster_'),
		write(dot_file, TopPath),
		write(dot_file, '" {\n'),
		write(dot_file, 'bgcolor=snow3\nlabel="'),
		write(dot_file, TopPath),
		write(dot_file, '"'),
		nl(dot_file),
		member(exclude_paths(ExcludedPaths), Options),
		forall(
			sub_library(TopPath, ExcludedPaths, RelativePath, Path),
			output_library(RelativePath, Path, Options)),
		write(dot_file, '}\n'),
		nl(dot_file).

	sub_library(TopPath, ExcludedPaths, RelativePath, Path) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopPath, RelativePath, Path),
		\+ member(RelativePath, ExcludedPaths).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates a diagram for all entities in a library using the specified options.',
		argnames is ['Library', 'Options']]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for all entities in a library using default options.',
		argnames is ['Library']]).

	library(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		atom_concat(Library, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_library(Path, Path, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	library(Library) :-
		library(Library, []).

	output_library(RelativePath, Path, Options) :-
		(	member(library_paths(true), Options) ->
			write(dot_file, 'subgraph "cluster_'),
			write(dot_file, RelativePath),
			write(dot_file, '" {\n'),
			write(dot_file, 'bgcolor=snow2\nlabel="'),
			write(dot_file, RelativePath),
			write(dot_file, '"'),
			nl(dot_file),
			output_library_files(Path, Options),
			write(dot_file, '}\n'),
			nl(dot_file)
		;	output_library_files(Path, Options)
		).

	output_library_files(Path, Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file(File, Path),
		atom_concat(Source, '.lgt', File),
		\+ member(Source, ExcludedFiles),
		output_file(File, Path, Options),
		fail.
	output_library_files(_, _).

	:- public(file/2).
	:- mode(file(+atom, +list), one).
	:- info(file/2, [
		comment is 'Creates a diagram for all entities in a loaded source file using the specified options. Supports library notation.',
		argnames is ['File', 'Options']]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a loaded source file using default options. Supports library notation.',
		argnames is ['File']]).

	file(Spec, UserOptions) :-
		merge_options(UserOptions, Options),
		compound(Spec),
		Spec =.. [Library, Source],
		logtalk::expand_library_path(Library, Path),
		atom_concat(Source, '.lgt', File),
		atom_concat(Source, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_file(File, Path, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	file(Source, UserOptions) :-
		merge_options(UserOptions, Options),
		atom(Source),
		atom_concat(Source, '.lgt', File),
		atom_concat(Source, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_file(File, _, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	file(Source) :-
		file(Source, []).

	output_file(File, Path, Options) :-
		(	member(file_names(true), Options) ->
			write(dot_file, 'subgraph "cluster_'),
			write(dot_file, File),
			write(dot_file, '" {\n'),
			write(dot_file, 'bgcolor=snow'),
			write(dot_file, '\nlabel="'),
			write(dot_file, File),
			write(dot_file, '"'),
			nl(dot_file),
			process(File, Path, Options),
			write(dot_file, '}\n'),
			nl(dot_file)
		;	process(File, Path, Options)
		).

	dot_header(Options) :-
		write(dot_file, 'digraph G {'),
		write(dot_file, '\nrankdir=BT'),
		write(dot_file, '\nranksep=1.25'),
		write(dot_file, '\ncompound=true'),
		write(dot_file, '\nclusterrank=local'),
		write(dot_file, '\nlabeljust=l'),
		write(dot_file, '\nmargin=1.0'),
		write(dot_file, '\nfontname="Courier"'),
		write(dot_file, '\nfontsize=10'),
		write(dot_file, '\nfontcolor=snow4'),
		write(dot_file, '\npencolor=snow4'),
		write(dot_file, '\nnode [style=filled,fillcolor=white,fontname="Courier",fontsize=9]'),
		write(dot_file, '\nedge [fontname="Courier",fontsize=9]'),
		output_date(Options),
		nl(dot_file).

	output_date(Options) :-
		(	member(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Mins, Secs, _), _, fail) ->
			write(dot_file, '\nlabel="Generated on '),
			write(dot_file, Year), write(dot_file, '/'),
			write(dot_file, Month), write(dot_file, '/'),
			write(dot_file, Day),
			write(dot_file, ', '),
			write(dot_file, Hours), write(dot_file, ':'),
			write(dot_file, Mins), write(dot_file, ':'),
			write(dot_file, Secs),
			write(dot_file, '"')
		;	true
		).

	dot_footer(_) :-
		write(dot_file, '}'),
		nl(dot_file).

	process(File, Path, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		protocol_property(Protocol, file(File, Path)),
		\+ member(Protocol, ExcludedEntities),
		output_protocol(Protocol, Options),
		fail.
	process(File, Path, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		object_property(Object, file(File, Path)),
		\+ member(Object, ExcludedEntities),
		output_object(Object, Options),
		fail.
	process(File, Path, Options) :-
		member(exclude_entities(ExcludedEntities), Options),
		category_property(Category, file(File, Path)),
		\+ member(Category, ExcludedEntities),
		output_category(Category, Options),
		fail.
	process(_, _, _).

	output_protocol(Protocol, Options) :-
		print_name(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		box(Name, PredicateText, protocol),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		print_name(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			box(Name, PredicateText, prototype)
		;	box(Name, PredicateText, instance_or_class)	
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		print_name(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		box(Name, PredicateText, category),
		output_category_relations(Category, Options).

	output_protocol_relations(Protocol, _) :-
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		arrow(ProtocolName, ExtendedProtocolName, extends),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Object, _) :-
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(ObjectName, ProtocolName, implements),
		fail.
	output_object_relations(Instance, _) :-
		instantiates_class(Instance, Class),
		print_name(object, Instance, InstanceName),
		print_name(object, Class, ClassName),
		arrow(InstanceName, ClassName, instantiates),
		fail.
	output_object_relations(Class, _) :-
		specializes_class(Class, SuperClass),
		print_name(object, Class, ClassName),
		print_name(object, SuperClass, SuperClassName),
		arrow(ClassName, SuperClassName, specializes),
		fail.
	output_object_relations(Prototype, _) :-
		extends_object(Prototype, Parent),
		print_name(object, Prototype, PrototypeName),
		print_name(object, Parent, ParentName),
		arrow(PrototypeName, ParentName, extends),
		fail.
	output_object_relations(Object, _) :-
		imports_category(Object, Category),
		print_name(object, Object, ObjectName),
		print_name(category, Category, CategoryName),
		arrow(ObjectName, CategoryName, imports),
		fail.
	output_object_relations(_, _).

	output_category_relations(Category, _) :-
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		arrow(CategoryName, ExtendedCategoryName, extends),
		fail.
	output_category_relations(Category, _) :-
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(CategoryName, ProtocolName, implements),
		fail.
	output_category_relations(Category, _) :-
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		arrow(ObjectName, CategoryName, complements),
		fail.
	output_category_relations(_, _).

	box(Name, PredicateText, Entity) :-
		entity_shape(Entity, Shape),
		write(dot_file, '"'),
		write(dot_file, Name),
		write(dot_file, '" [shape='),
		write(dot_file, Shape),
		write(dot_file, ',label="'),
		write(dot_file, Name),
		write(dot_file, '\\n'),
		write(dot_file, PredicateText),
		write(dot_file, '"]'),
		nl(dot_file).

	entity_shape(prototype, box).
	entity_shape(instance_or_class, box).
	entity_shape(protocol, note).
	entity_shape(category, component).

	arrow(Start, End, Label) :-
		label_arrowhead(Label, ArrowHead),
		write(dot_file, '"'),
		write(dot_file, Start),
		write(dot_file, '" -> "'),
		write(dot_file, End),
		write(dot_file, '" [arrowhead='),
		write(dot_file, ArrowHead),
		write(dot_file, ',label="'),
		write(dot_file, Label),
		write(dot_file, '"]'),
		nl(dot_file).

	label_arrowhead(extends, vee).
	label_arrowhead(instantiates, normal).
	label_arrowhead(specializes, onormal).
	label_arrowhead(implements, dot).
	label_arrowhead(imports, box).
	label_arrowhead(complements, obox).

	predicate_list_to_atom(List, Atom) :-
		predicate_list_to_atom(List, '', Atom).

	predicate_list_to_atom([], Atom, Atom).
	predicate_list_to_atom([Functor/Arity| Predicates], Atom0, Atom) :-
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Atom0, '\\n', Atom1),
		atom_concat(Atom1, Functor, Atom2),
		atom_concat(Atom2, '/', Atom3),
		atom_concat(Atom3, ArityAtom, Atom4),
		predicate_list_to_atom(Predicates, Atom4, Atom).

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
		(	list::member(parnames(Names), Info) ->
			true
		;	list::member(parameters(Parameters), Info) ->
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
		% by default, write diagram to the current directory:
		(member(output_path(OutputPath), UserOptions) -> true; os::working_directory(OutputPath)),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; ExcludedFiles = []),
		% by default, don't exclude any library sub-directories:
		(member(exclude_paths(ExcludedPaths), UserOptions) -> true; ExcludedPaths = []),
		% by default, don't exclude any entities:
		(member(exclude_entities(ExcludedEntities), UserOptions) -> true; ExcludedEntities = []),
		Options = [
			library_paths(LibraryPaths), file_names(FileNames), date(Date), interface(Interface),
			output_path(OutputPath),
			exclude_files(ExcludedFiles), exclude_paths(ExcludedPaths), exclude_entities(ExcludedEntities)].

	:- public(default_options/1).
	:- mode(default_options(-list), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']]).

	default_options(DefaultOptions) :-
		merge_options([], DefaultOptions).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
