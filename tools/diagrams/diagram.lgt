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


:- category(diagram(_Format)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/01/02,
		comment is 'Predicates for generating diagrams.',
		argnames is ['Format']
	]).

	:- public(libraries/1).
	:- mode(libraries(+list(compound)), one).
	:- info(libraries/1, [
		comment is 'Creates a diagram for all loaded libraries using the specified options.',
		argnames is ['Options']
	]).

	libraries(UserOptions) :-
		format_object(Format),
		merge_options(UserOptions, Options),
		::output_file_path(all_libraries, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		output_all_libraries(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_all_libraries(Options) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Directory),
		\+ \+ logtalk::loaded_file_property(_, directory(Directory)),
		::output_rlibrary(Library, Directory, Options),
		fail.
	output_all_libraries(_).

	:- public(libraries/0).
	:- mode(libraries, one).
	:- info(libraries/0, [
		comment is 'Creates a diagram for all loaded libraries using default options.'
	]).

	libraries :-
		::libraries([]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list(compound)), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	rlibrary(Library, UserOptions) :-
		format_object(Format),
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		::output_file_path(Library, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		::reset_externals,
		::output_rlibrary(Library, Path, Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_rlibrary(TopLibrary, TopPath, Options) :-
		format_object(Format),
		Format::graph_header(output_file, TopLibrary, TopLibrary, rlibrary, Options),
		output_library(TopLibrary, TopPath, Options),
		member(exclude_libraries(ExcludedLibraries), Options),
		forall(
			sub_library(TopLibrary, TopPath, ExcludedLibraries, Library, Path),
			output_library(Library, Path, Options)),
		Format::graph_footer(output_file, TopLibrary, TopLibrary, rlibrary, Options),
		::output_externals(Options).

	sub_library(TopLibrary, TopPath, ExcludedLibraries, Library, Path) :-
		logtalk_library_path(Library, _),
		Library \== TopLibrary,
		\+ member(Library, ExcludedLibraries),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopPath, _RelativePath, Path).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	rlibrary(Library) :-
		::rlibrary(Library, []).

	:- public(library/2).
	:- mode(library(+atom, +list(compound)), one).
	:- info(library/2, [
		comment is 'Creates a diagram for a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	library(Library, UserOptions) :-
		format_object(Format),
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		::output_file_path(Library, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		::reset_externals,
		::output_library(Library, Path, Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_library(Library, Path, Options) :-
		format_object(Format),
		Format::graph_header(output_file, Library, Library, library, Options),
		output_library_files(Path, Options),
		Format::graph_footer(output_file, Library, Library, library, Options),
		::output_externals(Options).

	output_library_files(Directory, Options) :-
		member(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		::output_file(Path, Basename, Directory, Options),
		fail.
	output_library_files(Directory, Options) :-
		member(exclude_files(ExcludedFiles), Options),
		prolog_modules_diagram_support::source_file_property(Path, directory(Directory)),
		prolog_modules_diagram_support::source_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		::output_file(Path, Basename, Directory, Options),
		fail.
	output_library_files(_, _).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(files/1).
	:- mode(files(+list(compound)), one).
	:- info(files/1, [
		comment is 'Creates a diagram for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	files(UserOptions) :-
		format_object(Format),
		merge_options(UserOptions, Options),
		::output_file_path(all_files, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		output_all_files(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_all_files(Options) :-
		logtalk::loaded_file(Path),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		::output_file(Path, Basename, Directory, Options),
		fail.
	output_all_files(_).

	:- public(files/0).
	:- mode(files, one).
	:- info(files/0, [
		comment is 'Creates a diagram for all loaded files using default options.'
	]).

	files :-
		::files([]).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list(compound)), one).
	:- info(files/3, [
		comment is 'Creates a diagram for a set of files using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files', 'Options']
	]).

	files(Project, Files, UserOptions) :-
		format_object(Format),
		merge_options(UserOptions, Options),
		::output_file_path(Project, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		::reset_externals,
		::output_files(Files, Options),
		::output_externals(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_files([], _Options).
	output_files([File| Files], Options) :-
		locate_file(File, Basename, _, Directory, Path),
		::output_file(Path, Basename, Directory, Options),
		output_files(Files, Options).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates a diagram for a set of files using the default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files']
	]).

	files(Project, Files) :-
		::files(Project, Files, []).

	:- public(format_object/2).
	:- multifile(format_object/2).
	:- mode(format_object(?atom, ?object_identifier), zero_or_more).
	:- info(format_object/2, [
		comment is 'Table of graph language formats and their implementation objects.',
		argnames is ['Format', 'Object']
	]).

	:- public(format_object/1).
	:- multifile(format_object/1).
	:- mode(format_object(-object_identifier), zero_or_one).
	:- info(format_object/1, [
		comment is 'Returns the identifier of the object implementing the graph language format currently being used. Fails of not format is specified.',
		argnames is ['Object']
	]).

	format_object(Object) :-
		parameter(1, Format),
		nonvar(Format),
		format_object(Format, Object).

	:- public(default_option/1).
	:- mode(default_option(?compound), zero_or_more).
	:- info(default_option/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOption']
	]).

	:- public(default_options/1).
	:- mode(default_options(-list(compound)), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']
	]).

	default_options(DefaultOptions) :-
		findall(DefaultOption, ::default_option(DefaultOption), DefaultOptions).

	:- protected(merge_options/2).
	:- mode(merge_options(+list(compound), -list(compound)), one).
	:- info(merge_options/2, [
		comment is 'Merges the user options with the default options, returning the list of options used when generating a diagram.',
		argnames is ['UserOptions', 'Options']
	]).

	merge_options(UserOptions, Options) :-
		findall(
			DefaultOption,
			(	::default_option(DefaultOption),
				functor(DefaultOption, OptionName, Arity),
				functor(UserOption, OptionName, Arity),
				\+ member(UserOption, UserOptions)
			),
			DefaultOptions
		),
		append(UserOptions, DefaultOptions, Options).

	:- protected(output_rlibrary/3).
	:- mode(output_rlibrary(+atom, +atom, +list(compound)), one).
	:- info(output_rlibrary/3, [
		comment is 'Generates diagram output for all sub-libraries of a library.',
		argnames is ['Library', 'Path', 'Options']
	]).

	:- protected(output_library/3).
	:- mode(output_library(+atom, +atom, +list(compound)), one).
	:- info(output_library/3, [
		comment is 'Generates diagram output for a library.',
		argnames is ['Library', 'Path', 'Options']
	]).

	:- protected(output_files/2).
	:- mode(output_files(+list, +list(compound)), one).
	:- info(output_files/2, [
		comment is 'Generates diagram output for a list of files.',
		argnames is ['Files', 'Options']
	]).

	:- protected(output_file/4).
	:- mode(output_file(+atom, +atom, +atom, +list(compound)), one).
	:- info(output_file/4, [
		comment is 'Generates diagram output for a file.',
		argnames is ['Path', 'Basename', 'Directory', 'Options']
	]).

	:- protected(output_externals/1).
	:- mode(output_externals(+list(compound)), one).
	:- info(output_externals/1, [
		comment is 'Outputs external entities using the specified options.',
		argnames is ['Options']
	]).

	:- protected(reset_externals/0).
	:- mode(reset_externals, one).
	:- info(reset_externals/0, [
		comment is 'Resets information on external entities.'
	]).

	:- protected(output_node/5).
	:- mode(output_node(+nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(output_node/5, [
		comment is 'Outputs a graph node.',
		argnames is ['Identifier', 'Label', 'Contents', 'Kind', 'Options']
	]).

	output_node(Identifier, Label, Lines, Kind, Options) :-
		format_object(Format),
		Format::node(output_file, Identifier, Label, Lines, Kind, Options).

	:- protected(output_edge/5).
	:- mode(output_edge(+nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(output_edge/5, [
		comment is 'Outputs a graph edge.',
		argnames is ['From', 'To', 'Labels', 'Kind', 'Options']
	]).

	output_edge(From, To, Labels, Kind, Options) :-
		format_object(Format),
		(	member(relation_labels(true), Options) ->
			Format::edge(output_file, From, To, Labels, Kind, Options)
		;	Format::edge(output_file, From, To, [], Kind, Options)
		).

	:- protected(not_excluded_file/3).
	:- mode(not_excluded_file(+list(atom), +atom, +atom), zero_or_one).
	:- info(not_excluded_file/3, [
		comment is 'True when the given file is not excluded from the generated output.',
		argnames is ['ExcludedFiles', 'Path', 'Basename']
	]).

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

	:- protected(diagram_name_suffix/1).
	:- mode(diagram_name_suffix(-atom), one).
	:- info(diagram_name_suffix/1, [
		comment is 'Returns the diagram name suffix.',
		argnames is ['Suffix']
	]).

	% default value
	diagram_name_suffix('_diagram').

	:- protected(output_file_path/4).
	:- mode(output_file_path(+atom, +list(atom), +object_identifier, -atom), one).
	:- info(output_file_path/4, [
		comment is 'Returns the output file path.',
		argnames is ['Name', 'Options', 'Format', 'Path']
	]).

	output_file_path(Name0, Options, Format, Path) :-
		::diagram_name_suffix(Suffix),
		atom_concat(Name0, Suffix, Name),
		Format::output_file_name(Name, Basename),
		member(output_directory(Directory0), Options),
		(	sub_atom(Directory0, _, _, 0, '/') ->
			Directory = Directory0
		;	atom_concat(Directory0, '/', Directory)
		),
		atom_concat(Directory, Basename, Path).

	:- protected(locate_file/5).
	:- mode(locate_file(+atom, +atom, +atom, +atom, -atom), one).
	:- info(locate_file/5, [
		comment is 'Locates a file given its name, basename, full path, or library notation representation.',
		argnames is ['File', 'Basename', 'Extension', 'Directory', 'Path']
	]).

	% file given in library notation
	locate_file(LibraryNotation, Basename, Extension, Directory, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Basename, Extension, Directory, Path).
	% file given using its name or basename
	locate_file(Source, Basename, Extension, Directory, Path) :-
		add_extension(Source, Basename, Extension),
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
	locate_file(Source, Basename, Extension, Directory, Path) :-
		add_extension(Source, Path, Extension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		!.

	add_extension(Source, SourceWithExtension, Extension) :-
		atom(Source),
		(	sub_atom(Source, _, 4, 0, '.lgt') ->
			SourceWithExtension = Source,
			Extension = '.lgt'
		;	sub_atom(Source, _, 8, 0, '.logtalk') ->
			SourceWithExtension = Source,
			Extension = '.logtalk'
		;	(	atom_concat(Source, '.lgt', SourceWithExtension),
				Extension = '.lgt'
			;	atom_concat(Source, '.logtalk', SourceWithExtension),
				Extension = '.logtalk'
			)
		).

	:- protected(ground_entity_identifier/3).
	:- mode(ground_entity_identifier(+atom, +callable, -callable), one).
	:- info(ground_entity_identifier/3, [
		comment is 'Converts an entity identifier to a ground term.',
		argnames is ['Kind', 'Identifier', 'GroundIdentifier']
	]).

	ground_entity_identifier(object, Object, ObjectName) :-
		(	atom(Object) ->
			ObjectName = Object
		;	(	object_property(Object, info(Info)) ->
				parameter_names(Object, Info, Names)
			;	parameter_names(Object, [], Names)
			),
			Object =.. [Functor| _],
			ObjectName =.. [Functor| Names]
		).
	ground_entity_identifier(protocol, Protocol, Protocol).
	ground_entity_identifier(category, Category, CategoryName) :-
		(	atom(Category) ->
			CategoryName = Category
		;	(	category_property(Category, info(Info)) ->
				parameter_names(Category, Info, Names)
			;	parameter_names(Category, [], Names)
			),
			Category =.. [Functor| _],
			CategoryName =.. [Functor| Names]
		).
	ground_entity_identifier(module, Module, Module).

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

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_category.
