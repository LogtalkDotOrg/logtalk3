%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- category(diagram(_Format),
	extends(options)).

	:- info([
		version is 2:47:0,
		author is 'Paulo Moura',
		date is 2021-01-31,
		comment is 'Common predicates for generating diagrams.',
		parameters is ['Format' - 'Graph language file format']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2
	]).
	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		% workaround gplc limitation when dealing with multifile predicates
		% that are called from a file but not defined in that file
		:- multifile(user::logtalk_library_path/2).
		:- dynamic(user::logtalk_library_path/2).
		:- multifile(graph_language_registry::language_object/2).
	:- endif.

	:- public(libraries/3).
	:- mode(libraries(+atom, +list(atom), +list(compound)), one).
	:- info(libraries/3, [
		comment is 'Creates a diagram for a set of libraries using the specified options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Libraries', 'Options']
	]).

	libraries(Project, Libraries, UserOptions) :-
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Project, Options, Format, OutputPath),
		::diagram_description(Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Project, [description(Description)| Options]),
			atom_concat(libraries_, Project, Identifier),
			Format::graph_header(diagram_output_file, Identifier, '', libraries, Options),
			output_libraries(Libraries, Format, Options),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, '', libraries, Options),
			Format::file_footer(diagram_output_file, Project, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::libraries(Project, Libraries, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	output_libraries([], _Format, _Options).
	output_libraries([Library| Libraries], Format, Options) :-
		self(Self),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, library, Library)),
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Directory),
		atom_concat(library_, Library, Identifier),
		add_link_options(Directory, Options, GraphOptions),
		omit_path_prefix(Directory, Options, Relative),
		Format::graph_header(diagram_output_file, Identifier, Relative, library, GraphOptions),
		::output_library(Library, Directory, GraphOptions),
		Format::graph_footer(diagram_output_file, Identifier, Relative, library, GraphOptions),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, library, Library)),
		output_libraries(Libraries, Format, Options).

	:- public(libraries/2).
	:- mode(libraries(+atom, +list(atom)), one).
	:- info(libraries/2, [
		comment is 'Creates a diagram for a set of libraries using the default options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Libraries']
	]).

	libraries(Project, Libraries) :-
		::libraries(Project, Libraries, []).

	:- public(libraries/1).
	:- mode(libraries(+list(atom)), one).
	:- info(libraries/1, [
		comment is 'Creates a diagram for a set of libraries using the default options. The prefix ``libraries`` is used for the diagram file name.',
		argnames is ['Libraries']
	]).

	libraries(Libraries) :-
		::libraries(libraries, Libraries, []).

	:- public(all_libraries/1).
	:- mode(all_libraries(+list(compound)), one).
	:- info(all_libraries/1, [
		comment is 'Creates a diagram for all loaded libraries using the specified options.',
		argnames is ['Options']
	]).

	all_libraries(UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		::reset,
		format_object(Format),
		::output_file_path(all_libraries, Options, Format, OutputPath),
		::diagram_description(Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, libraries, [description(Description)| Options]),
			output_all_libraries(Options),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::file_footer(diagram_output_file, libraries, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::all_libraries(UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	output_all_libraries(Options) :-
		self(Self),
		format_object(Format),
		memberchk(exclude_libraries(ExcludedLibraries), Options),
		logtalk_library_path(Library, _),
		\+ member(Library, ExcludedLibraries),
		logtalk::expand_library_path(Library, Directory),
		\+ \+ logtalk::loaded_file_property(_, directory(Directory)),
		% loaded library
		logtalk::print_message(comment, diagrams, generating_diagram(Self, library, Library)),
		atom_concat(library_, Library, Identifier),
		add_link_options(Directory, Options, GraphOptions),
		omit_path_prefix(Directory, Options, Relative),
		Format::graph_header(diagram_output_file, Identifier, Relative, library, GraphOptions),
		::output_library(Library, Directory, GraphOptions),
		Format::graph_footer(diagram_output_file, Identifier, Relative, library, GraphOptions),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, library, Library)),
		fail.
	output_all_libraries(_).

	:- public(all_libraries/0).
	:- mode(all_libraries, one).
	:- info(all_libraries/0, [
		comment is 'Creates a diagram for all loaded libraries using default options.'
	]).

	all_libraries :-
		::all_libraries([]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list(compound)), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	rlibrary(Library, UserOptions) :-
		self(Self),
		^^check_options(UserOptions),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, library, Library)),
		locate_library(Library, Path),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Library, Options, Format, OutputPath),
		diagram_caption(library, Library, Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Library, [description(Description)| Options]),
			atom_concat(rlibrary_, Library, Identifier),
			add_link_options(Path, Options, GraphOptions),
			omit_path_prefix(Path, Options, Relative),
			Format::graph_header(diagram_output_file, Identifier, Relative, rlibrary, GraphOptions),
			::output_rlibrary(Library, Path, GraphOptions),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, Relative, rlibrary, GraphOptions),
			Format::file_footer(diagram_output_file, Library, Options) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, library, Library))
		;	% failure is usually caused by errors in the source itself
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::rlibrary(Library, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

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
		self(Self),
		^^check_options(UserOptions),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, library, Library)),
		locate_library(Library, Path),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Library, Options, Format, OutputPath),
		diagram_caption(library, Library, Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Library, [description(Description)| Options]),
			atom_concat(library_, Library, Identifier),
			add_link_options(Path, Options, GraphOptions),
			omit_path_prefix(Path, Options, Relative),
			Format::graph_header(diagram_output_file, Identifier, Relative, library, GraphOptions),
			::output_library(Library, Path, GraphOptions),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, Relative, library, GraphOptions),
			Format::file_footer(diagram_output_file, Library, Options) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, library, Library))
		;	% failure is usually caused by errors in the source itself
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::library(Library, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	output_library(_Library, Directory, Options) :-
		memberchk(exclude_files(ExcludedFiles), Options),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::loaded_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		::output_file(Path, Basename, Directory, Options),
		fail.
	output_library(_Library, Directory, Options) :-
		memberchk(exclude_files(ExcludedFiles), Options),
		modules_diagram_support::loaded_file_property(Path, directory(Directory)),
		% Logtalk source files may also be loaded from Prolog source files but
		% then the file was already enumerated by the previous clause
		\+ logtalk::loaded_file(Path),
		modules_diagram_support::loaded_file_property(Path, basename(Basename)),
		::not_excluded_file(ExcludedFiles, Path, Basename),
		::output_file(Path, Basename, Directory, Options),
		fail.
	output_library(_, _, _).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(directories/3).
	:- mode(directories(+atom, +list(atom), +list(compound)), one).
	:- info(directories/3, [
		comment is 'Creates a diagram for a set of directories using the specified options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directories', 'Options']
	]).

	directories(Project, Directories, UserOptions) :-
		^^check_options(UserOptions),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Project, Options, Format, OutputPath),
		::diagram_description(Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Project, [description(Description)| Options]),
			atom_concat(directories_, Project, Identifier),
			Format::graph_header(diagram_output_file, Identifier, '', directories, Options),
			normalize_directory_paths(Directories, NormalizedDirectories),
			output_directories(NormalizedDirectories, Project, Format, Options),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, '', directories, Options),
			Format::file_footer(diagram_output_file, Project, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::directories(Project, Directories, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	normalize_directory_paths([], []).
	normalize_directory_paths([Directory| Directories], [NormalizedDirectory| NormalizedDirectories]) :-
		os::absolute_file_name(Directory, NormalizedDirectory0),
		(	sub_atom(NormalizedDirectory0, _, _, 0, '/') ->
			NormalizedDirectory = NormalizedDirectory0
		;	atom_concat(NormalizedDirectory0, '/', NormalizedDirectory)
		),
		normalize_directory_paths(Directories, NormalizedDirectories).

	output_directories([], _Project, _Format, _Options).
	output_directories([Directory| Directories], Project, Format, Options) :-
		self(Self),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, directory, Directory)),
		add_link_options(Directory, Options, GraphOptions),
		::output_library(Project, Directory, GraphOptions),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, directory, Directory)),
		output_directories(Directories, Project, Format, Options).

	:- public(directories/2).
	:- mode(directories(+atom, +list(atom)), one).
	:- info(directories/2, [
		comment is 'Creates a diagram for a set of directories using the default options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directories']
	]).

	directories(Project, Directories) :-
		::directories(Project, Directories, []).

	:- public(rdirectory/3).
	:- mode(rdirectory(+atom, +atom, +list(compound)), one).
	:- info(rdirectory/3, [
		comment is 'Creates a diagram for a directory and its sub-directories using the specified options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directory', 'Options']
	]).

	rdirectory(Project, Directory, UserOptions) :-
		self(Self),
		^^check_options(UserOptions),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, directory, Directory)),
		locate_directory(Directory, NormalizedDirectory),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Project, Options, Format, OutputPath),
		omit_path_prefix(NormalizedDirectory, Options, Relative),
		(	Relative == './' ->
			::diagram_description(Description)
		;	diagram_caption(directory, Relative, Description)
		),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Project, [description(Description)| Options]),
			atom_concat(rdirectory_, Project, Identifier),
			add_link_options(NormalizedDirectory, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, Relative, rdirectory, GraphOptions),
			::output_rdirectory(Project, NormalizedDirectory, GraphOptions),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, Relative, rdirectory, GraphOptions),
			Format::file_footer(diagram_output_file, Project, Options) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, directory, NormalizedDirectory))
		;	% failure is usually caused by errors in the source itself
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::rdirectory(Project, NormalizedDirectory, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	:- public(rdirectory/2).
	:- mode(rdirectory(+atom, +atom), one).
	:- info(rdirectory/2, [
		comment is 'Creates a diagram for a directory and its sub-directories using default options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directory']
	]).

	rdirectory(Project, Directory) :-
		::rdirectory(Project, Directory, []).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Creates a diagram for a directory and its sub-directories using default options. The name of the directory is used as a prefix for the diagram file name.',
		argnames is ['Directory']
	]).

	rdirectory(Directory) :-
		os::absolute_file_name(Directory, Path),
		os::decompose_file_name(Path, _, Project, _),
		::rdirectory(Project, Directory, []).

	:- public(directory/3).
	:- mode(directory(+atom, +atom, +list(compound)), one).
	:- info(directory/3, [
		comment is 'Creates a diagram for a directory using the specified options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directory', 'Options']
	]).

	directory(Project, Directory, UserOptions) :-
		self(Self),
		^^check_options(UserOptions),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, directory, Directory)),
		locate_directory(Directory, NormalizedDirectory),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Project, Options, Format, OutputPath),
		omit_path_prefix(NormalizedDirectory, Options, Relative),
		(	Relative == './' ->
			::diagram_description(Description)
		;	diagram_caption(directory, Relative, Description)
		),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Project, [description(Description)| Options]),
			atom_concat(directory_, Project, Identifier),
			add_link_options(NormalizedDirectory, Options, GraphOptions),
			Format::graph_header(diagram_output_file, Identifier, Relative, directory, GraphOptions),
			::output_library(Project, NormalizedDirectory, GraphOptions),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, Relative, directory, GraphOptions),
			Format::file_footer(diagram_output_file, Project, Options) ->
			logtalk::print_message(comment, diagrams, generated_diagram(Self, directory, NormalizedDirectory))
		;	% failure is usually caused by errors in the source itself
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::directory(Project, NormalizedDirectory, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	:- public(directory/2).
	:- mode(directory(+atom, +atom), one).
	:- info(directory/2, [
		comment is 'Creates a diagram for a directory using default options. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Directory']
	]).

	directory(Project, Directory) :-
		::directory(Project, Directory, []).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Creates a diagram for a directory using default options. The name of the directory is used as a prefix for the diagram file name.',
		argnames is ['Directory']
	]).

	directory(Directory) :-
		os::absolute_file_name(Directory, Path),
		os::decompose_file_name(Path, _, Project, _),
		::directory(Project, Directory, []).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list(compound)), one).
	:- info(files/3, [
		comment is 'Creates a diagram for a set of files using the specified options. The file can be specified by name, basename, full path, or using library notation. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Files', 'Options']
	]).

	files(Project, Files, UserOptions) :-
		^^check_options(UserOptions),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(Project, Options, Format, OutputPath),
		::diagram_description(Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, Project, [description(Description)| Options]),
			atom_concat(files_, Project, Identifier),
			Format::graph_header(diagram_output_file, Identifier, '', files, Options),
			::output_files(Files, Options),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::graph_footer(diagram_output_file, Identifier, '', files, Options),
			Format::file_footer(diagram_output_file, Project, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::files(Project, Files, UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	output_files([], _Options).
	output_files([File| Files], Options) :-
		self(Self),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, file, File)),
		locate_file(File, Basename, _, Directory, Path),
		add_link_options(Path, Options, FileOptions),
		::output_file(Path, Basename, Directory, FileOptions),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, file, File)),
		output_files(Files, Options).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates a diagram for a set of files using the default options. The file can be specified by name, basename, full path, or using library notation. The ``Project`` argument is used as a prefix for the diagram file name.',
		argnames is ['Project', 'Files']
	]).

	files(Project, Files) :-
		::files(Project, Files, []).

	:- public(files/1).
	:- mode(files(+list(atom)), one).
	:- info(files/1, [
		comment is 'Creates a diagram for a set of files using the default options. The file can be specified by name, basename, full path, or using library notation. The prefix ``files`` is used for the diagram file name.',
		argnames is ['Files']
	]).

	files(Files) :-
		::files(files, Files, []).

	:- public(all_files/1).
	:- mode(all_files(+list(compound)), one).
	:- info(all_files/1, [
		comment is 'Creates a diagram for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	all_files(UserOptions) :-
		^^check_options(UserOptions),
		format_object(Format),
		^^merge_options(UserOptions, Options),
		::reset,
		::output_file_path(all_files, Options, Format, OutputPath),
		::diagram_description(Description),
		open(OutputPath, write, Stream, [alias(diagram_output_file)]),
		(	Format::file_header(diagram_output_file, files, [description(Description)| Options]),
			output_all_files(Options),
			::output_externals(Options),
			::output_edges(Options),
			::output_missing_externals(Options),
			Format::file_footer(diagram_output_file, files, Options) ->
			true
		;	% failure is usually caused by errors in the source itself
			self(Self),
			logtalk::print_message(warning, diagrams, generating_diagram_failed(Self::all_files(UserOptions)))
		),
		close(Stream),
		::output_sub_diagrams(UserOptions).

	output_all_files(Options) :-
		self(Self),
		logtalk::loaded_file(Path),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, file, Path)),
		::output_file(Path, Basename, Directory, Options),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, file, Path)),
		fail.
	output_all_files(Options) :-
		self(Self),
		modules_diagram_support::loaded_file_property(Path, basename(Basename)),
		% Logtalk source files may also be loaded from Prolog source files but
		% then the file was already enumerated by the previous clause
		\+ logtalk::loaded_file(Path),
		% only output Prolog files that are parents of Logtalk source files
		once((
			modules_diagram_support::loaded_file_property(Other, parent(Path)),
			logtalk::loaded_file(Other)
		)),
		modules_diagram_support::loaded_file_property(Path, directory(Directory)),
		logtalk::print_message(comment, diagrams, generating_diagram(Self, file, Path)),
		::output_file(Path, Basename, Directory, Options),
		logtalk::print_message(comment, diagrams, generated_diagram(Self, file, Path)),
		fail.
	output_all_files(_).

	:- public(all_files/0).
	:- mode(all_files, one).
	:- info(all_files/0, [
		comment is 'Creates a diagram for all loaded files using default options.'
	]).

	all_files :-
		::all_files([]).

	:- public(format_object/1).
	:- mode(format_object(-object_identifier), zero_or_one).
	:- info(format_object/1, [
		comment is 'Returns the identifier of the object implementing the graph language currently being used. Fails if none is specified.',
		argnames is ['Object']
	]).

	format_object(Object) :-
		parameter(1, Format),
		nonvar(Format),
		graph_language_registry::language_object(Format, Object).

	:- public(diagram_description/1).
	:- mode(diagram_description(-atom), one).
	:- info(diagram_description/1, [
		comment is 'Returns the diagram description.',
		argnames is ['Description']
	]).

	% default value
	diagram_description('Diagram').

	:- public(diagram_name_suffix/1).
	:- mode(diagram_name_suffix(-atom), one).
	:- info(diagram_name_suffix/1, [
		comment is 'Returns the diagram name suffix.',
		argnames is ['Suffix']
	]).

	% default value
	diagram_name_suffix('_diagram').

	valid_option(exclude_libraries(Libraries)) :-
		valid(list(atom), Libraries).
	valid_option(exclude_directories(Directories)) :-
		valid(list(atom), Directories).
	valid_option(exclude_files(Files)) :-
		valid(list(atom), Files).
	valid_option(exclude_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(externals(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(file_extensions(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(path_url_prefixes(Directory, CodePrefix, DocPrefix)) :-
		atom(Directory),
		atom(CodePrefix),
		atom(DocPrefix).
	valid_option(url_prefixes(CodePrefix, DocPrefix)) :-
		atom(CodePrefix),
		atom(DocPrefix).
	valid_option(omit_path_prefixes(Prefixes)) :-
		valid(list(atom), Prefixes).
	valid_option(output_directory(Directory)) :-
		atom(Directory).
	valid_option(layout(Layout)) :-
		valid(one_of(atom, [top_to_bottom,bottom_to_top,left_to_right,right_to_left]), Layout).
	valid_option(title(Title)) :-
		atom(Title).
	valid_option(date(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(interface(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(directory_paths(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(file_labels(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(inheritance_relations(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(provide_relations(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(xref_relations(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(relation_labels(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(xref_calls(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(node_type_captions(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(entity_url_suffix_target(Suffix, Target)) :-
		atom(Suffix),
		atom(Target).
	valid_option(zoom(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(zoom_url_suffix(Suffix)) :-
		atom(Suffix).
	valid_option(url_line_references(Provider)) :-
		valid(one_of(atom, [bitbucket,github,gitlab]), Provider).

	fix_option(path_url_prefixes(Directory, CodePrefix, DocPrefix), path_url_prefixes(NormalizedDirectory, CodePrefix, DocPrefix)) :-
		normalize_directory_paths([Directory], [NormalizedDirectory]).
	fix_option(omit_path_prefixes(Prefixes), omit_path_prefixes(NormalizedPrefixes)) :-
		normalize_directory_paths(Prefixes, NormalizedPrefixes).
	fix_option(output_directory(Directory), output_directory(NormalizedDirectory)) :-
		normalize_directory_paths([Directory], [NormalizedDirectory]).

	:- protected(diagram_caption/3).
	:- mode(diagram_caption(+atom, +callable, -atom), one).
	:- info(diagram_caption/3, [
		comment is 'Creates a diagram caption from the diagram description and the subject and its kind.',
		argnames is ['Kind', 'Subject', 'Description']
	]).

	diagram_caption(Kind, Subject, Description) :-
		::diagram_description(Description0),
		atom_concat(Description0, ' for ', Description1),
		atom_concat(Description1, Kind, Description2),
		atom_concat(Description2, ' ', Description3),
		(	atom(Subject) ->
			atom_concat(Description3, Subject, Description)
		;	functor(Subject, Functor, Arity),
			atom_concat(Description3, Functor, Description4),
			atom_concat(Description4, '/', Description5),
			number_codes(Arity, ArityCodes),
			atom_codes(ArityAtom, ArityCodes),
			atom_concat(Description5, ArityAtom, Description)
		).

	:- protected(output_rlibrary/3).
	:- mode(output_rlibrary(+atom, +atom, +list(compound)), one).
	:- info(output_rlibrary/3, [
		comment is 'Generates diagram output for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Path', 'Options']
	]).

	output_rlibrary(TopLibrary, TopPath, Options) :-
		format_object(Format),
		memberchk(exclude_libraries(ExcludedLibraries), Options),
		atom_concat(library_, TopLibrary, TopIdentifier),
		add_link_options(TopPath, Options, TopGraphOptions),
		omit_path_prefix(TopPath, Options, TopRelative),
		Format::graph_header(diagram_output_file, TopIdentifier, TopRelative, library, TopGraphOptions),
		::output_library(TopLibrary, TopPath, TopGraphOptions),
		Format::graph_footer(diagram_output_file, TopIdentifier, TopRelative, library, TopGraphOptions),
		sub_library(TopLibrary, TopPath, ExcludedLibraries, Library, Path),
			atom_concat(library_, Library, Identifier),
			add_link_options(Path, Options, GraphOptions),
			atom_concat(TopPath, Relative, Path),
			Format::graph_header(diagram_output_file, Identifier, Relative, library, GraphOptions),
			::output_library(Library, Path, GraphOptions),
			Format::graph_footer(diagram_output_file, Identifier, Relative, library, GraphOptions),
		fail.
	output_rlibrary(_, _, _).

	sub_library(TopLibrary, TopPath, ExcludedLibraries, Library, Path) :-
		logtalk_library_path(Library, _),
		Library \== TopLibrary,
		\+ member(Library, ExcludedLibraries),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopPath, RelativePath, Path),
		RelativePath \== ''.

	:- protected(output_library/3).
	:- mode(output_library(+atom, +atom, +list(compound)), one).
	:- info(output_library/3, [
		comment is 'Generates diagram output for a library using the specified options.',
		argnames is ['Library', 'Path', 'Options']
	]).

	:- protected(output_rdirectory/3).
	:- mode(output_rdirectory(+atom, +atom, +list(compound)), one).
	:- info(output_rdirectory/3, [
		comment is 'Generates diagram output for a directory and its sub-directories using the specified options.',
		argnames is ['Project', 'Path', 'Options']
	]).

	output_rdirectory(Project, TopPath, Options) :-
		memberchk(exclude_directories(ExcludedDirectories), Options),
		add_link_options(TopPath, Options, TopGraphOptions),
		::output_library(Project, TopPath, TopGraphOptions),
		sub_directory(TopPath, ExcludedDirectories, Directory, Path),
			add_link_options(Path, Options, GraphOptions),
			::output_library(Directory, Path, GraphOptions),
		fail.
	output_rdirectory(_, _, _).

	sub_directory(TopPath, ExcludedDirectories, SubDirectory, SubDirectoryPath) :-
		os::directory_files(TopPath, SubDirectories, [type(directory), dot_files(false), paths(relative)]),
		member(Directory, SubDirectories),
		\+ member(Directory, ExcludedDirectories),
		atom_concat(TopPath, Directory, DirectoryPath0),
		(	sub_atom(DirectoryPath0, _, 1, 0, '/') ->
			DirectoryPath = DirectoryPath0
		;	atom_concat(DirectoryPath0, '/', DirectoryPath)
		),
		(	SubDirectory = Directory,
			SubDirectoryPath = DirectoryPath
		;	sub_directory(DirectoryPath, ExcludedDirectories, SubDirectory, SubDirectoryPath)
		).

	:- protected(output_externals/1).
	:- mode(output_externals(+list(compound)), one).
	:- info(output_externals/1, [
		comment is 'Output external nodes using the specified options depending on the value of the boolean option ``externals/1``.',
		argnames is ['Options']
	]).

	% default definition; expected to be overridden
	output_externals(_).

	:- protected(output_files/2).
	:- mode(output_files(+list, +list(compound)), one).
	:- info(output_files/2, [
		comment is 'Generates diagram output for a list of files using the specified options.',
		argnames is ['Files', 'Options']
	]).

	:- protected(output_file/4).
	:- mode(output_file(+atom, +atom, +atom, +list(compound)), one).
	:- info(output_file/4, [
		comment is 'Generates diagram output for a file using the specified options.',
		argnames is ['Path', 'Basename', 'Directory', 'Options']
	]).

	:- protected(output_sub_diagrams/1).
	:- mode(output_sub_diagrams(+list(compound)), one).
	:- info(output_sub_diagrams/1, [
		comment is 'Outputs sub-diagrams using the specified options.',
		argnames is ['Options']
	]).

	% by default, don't output sub-diagrams
	output_sub_diagrams(_).

	:- protected(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all temporary information used when generating a diagram.'
	]).

	reset :-
		::retractall(node_(_, _, _, _, _, _)),
		::retractall(edge_(_, _, _, _, _)).

	:- protected(output_node/6).
	:- mode(output_node(+nonvar, +nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(output_node/6, [
		comment is 'Outputs a graph node.',
		argnames is ['Identifier', 'Label', 'Caption', 'Contents', 'Kind', 'Options']
	]).

	output_node(Identifier, Label, Caption, Contents, Kind, Options) :-
		% cache the node so that we can later check for missing external nodes
		::assertz(node_(Identifier, Label, Caption, Contents, Kind, Options)),
		format_object(Format),
		Format::node(diagram_output_file, Identifier, Label, Caption, Contents, Kind, Options).

	:- protected(node/6).
	:- mode(node(?nonvar, ?nonvar, ?nonvar, ?list(compound), ?atom, ?list(compound)), zero_or_more).
	:- info(node/6, [
		comment is 'Enumerates, by backtracking, all saved nodes.',
		argnames is ['Identifier', 'Label', 'Caption', 'Contents', 'Kind', 'Options']
	]).

	node(Identifier, Label, Caption, Contents, Kind, Options) :-
		::node_(Identifier, Label, Caption, Contents, Kind, Options).

	:- private(node_/6).
	:- dynamic(node_/6).
	:- mode(node_(?nonvar, ?nonvar, ?nonvar, ?list(compound), ?atom, ?list(compound)), zero_or_more).
	:- info(node_/6, [
		comment is 'Table of saved nodes.',
		argnames is ['Identifier', 'Label', 'Caption', 'Contents', 'Kind', 'Options']
	]).

	:- protected(edge/5).
	:- mode(edge(?nonvar, ?nonvar, ?list(nonvar), ?atom, ?list(compound)), zero_or_more).
	:- info(edge/5, [
		comment is 'Enumerates, by backtracking, all saved edges.',
		argnames is ['From', 'To', 'Labels', 'Kind', 'Options']
	]).

	edge(From, To, Labels, Kind, Options) :-
		::edge_(From, To, Labels, Kind, Options).

	:- private(edge_/5).
	:- dynamic(edge_/5).
	:- mode(edge_(?nonvar, ?nonvar, ?list(nonvar), ?atom, ?list(compound)), zero_or_more).
	:- info(edge_/5, [
		comment is 'Table of saved edges.',
		argnames is ['From', 'To', 'Labels', 'Kind', 'Options']
	]).

	:- protected(output_edges/1).
	:- mode(output_edges(+list(compound)), one).
	:- info(output_edges/1, [
		comment is 'Outputs all edges.',
		argnames is ['Options']
	]).

	output_edges(Options) :-
		memberchk(externals(Externals), Options),
		format_object(Format),
		output_edges(Externals, Format).

	output_edges(true, Format) :-
		::retract(edge_(From, To, Labels, Kind, EdgeOptions)),
		Format::edge(diagram_output_file, From, To, Labels, Kind, EdgeOptions),
		fail.
	output_edges(false, Format) :-
		::retract(edge_(From, To, Labels, Kind, EdgeOptions)),
		::node_(To, _, _, _, ToKind, _),
		\+ external_node_kind(ToKind),
		Format::edge(diagram_output_file, From, To, Labels, Kind, EdgeOptions),
		fail.
	output_edges(_, _).

	external_node_kind(external_prototype).
	external_node_kind(external_class).
	external_node_kind(external_instance).
	external_node_kind(external_instance_and_class).
	external_node_kind(external_protocol).
	external_node_kind(external_category).
	external_node_kind(external_module).
	external_node_kind(external_file).
	external_node_kind(external_directory).
	external_node_kind(external_library).
	external_node_kind(external_predicate).

	:- protected(save_edge/5).
	:- mode(save_edge(+nonvar, +nonvar, +list(nonvar), +atom, +list(compound)), one).
	:- info(save_edge/5, [
		comment is 'Saves a graph edge.',
		argnames is ['From', 'To', 'Labels', 'Kind', 'Options']
	]).

	save_edge(From, To, Labels, Kind, Options) :-
		(	member(relation_labels(true), Options) ->
			::assertz(edge_(From, To, Labels, Kind, Options))
		;	::assertz(edge_(From, To, [], Kind, Options))
		).

	:- protected(output_missing_externals/1).
	:- mode(output_missing_externals(+list(compound)), one).
	:- info(output_missing_externals/1, [
		comment is 'Outputs missing external nodes (usually due to unloaded resources) that are referenced from edges.',
		argnames is ['Options']
	]).

	% do nothing by default
	output_missing_externals(_).

	:- protected(not_excluded_file/3).
	:- mode(not_excluded_file(+list(atom), +atom, +atom), zero_or_one).
	:- info(not_excluded_file/3, [
		comment is 'True when the given file is not excluded from the generated output. Excluded files may be specified by full path or by basename and with or without extension.',
		argnames is ['ExcludedFiles', 'Path', 'Basename']
	]).

	not_excluded_file([], _, _).
	not_excluded_file([ExcludedFile| ExcludedFiles], Path, Basename) :-
		% files in the exclusion list may be given by full path or by basename
		\+ member(Path, [ExcludedFile| ExcludedFiles]),
		\+ member(Basename, [ExcludedFile| ExcludedFiles]),
		% files in the exclusion list may be given with or without extension
		\+ (	logtalk::file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Path),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	logtalk::file_type_extension(logtalk, Extension),
				atom_concat(Source, Extension, Basename),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	modules_diagram_support::source_file_extension(Extension),
				atom_concat(Source, Extension, Path),
				member(Source, [ExcludedFile| ExcludedFiles])
		),
		\+ (	modules_diagram_support::source_file_extension(Extension),
				atom_concat(Source, Extension, Basename),
				member(Source, [ExcludedFile| ExcludedFiles])
		).

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
		memberchk(output_directory(Directory), Options),
		os::make_directory(Directory),
		atom_concat(Directory, Basename, Path).

	:- protected(locate_library/2).
	:- mode(locate_library(+atom, -atom), one).
	:- info(locate_library/2, [
		comment is 'Locates a library given its name.',
		argnames is ['Library', 'Path']
	]).

	locate_library(Library, Path) :-
		logtalk::expand_library_path(Library, Path),
		!.
	% give up
	locate_library(Library, _) :-
		logtalk::print_message(warning, diagrams, unable_to_locate(library, Library)),
		fail.

	:- protected(locate_directory/2).
	:- mode(locate_directory(+atom, -atom), one).
	:- info(locate_directory/2, [
		comment is 'Locates a directory given its name or full path.',
		argnames is ['Directory', 'Path']
	]).

	locate_directory(Directory, Path) :-
		os::absolute_file_name(Directory, Path0),
		os::directory_exists(Path0),
		(	sub_atom(Path0, _, _, 0, '/') ->
			Path = Path0
		;	atom_concat(Path0, '/', Path)
		),
		!.
	% give up
	locate_directory(Directory, _) :-
		logtalk::print_message(warning, diagrams, unable_to_locate(directory, Directory)),
		fail.

	:- protected(locate_file/5).
	:- mode(locate_file(+atom, +atom, +atom, +atom, -atom), one).
	:- info(locate_file/5, [
		comment is 'Locates a file given its name, basename, full path, or library notation representation.',
		argnames is ['File', 'Basename', 'Extension', 'Directory', 'Path']
	]).

	% Logtalk file given in library notation
	locate_file(LibraryNotation, Basename, Extension, Directory, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Basename, Extension, Directory, Path).
	% Logtalk file given using its name or basename
	locate_file(Source, Basename, Extension, Directory, Path) :-
		add_extension(logtalk, Source, Basename, Extension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			logtalk::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% Prolog file given using its name or basename
	locate_file(Source, Basename, Extension, Directory, Path) :-
		add_extension(prolog, Source, Basename, Extension),
		modules_diagram_support::loaded_file_property(Path, basename(Basename)),
		modules_diagram_support::loaded_file_property(Path, directory(Directory)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			modules_diagram_support::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% Logtalk file given using a full path
	locate_file(Source0, Basename, Extension, Directory, Path) :-
		os::absolute_file_name(Source0, Source),
		add_extension(logtalk, Source, Path, Extension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		!.
	% Prolog file given using a full path
	locate_file(Source0, Basename, Extension, Directory, Path) :-
		os::absolute_file_name(Source0, Source),
		add_extension(prolog, Source, Path, Extension),
		modules_diagram_support::loaded_file_property(Source, basename(Basename)),
		modules_diagram_support::loaded_file_property(Source, directory(Directory)),
		!.
	% give up
	locate_file(Source, _, _, _, _) :-
		logtalk::print_message(warning, diagrams, unable_to_locate(file, Source)),
		fail.

	add_extension(logtalk, Source, SourceWithExtension, Extension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		os::decompose_file_name(Source, _, _, SourceExtension),
		\+ modules_diagram_support::source_file_extension(SourceExtension),
		(	logtalk::file_type_extension(logtalk, SourceExtension) ->
			SourceWithExtension = Source,
			Extension = SourceExtension
		;	% no recognized extension in use
			logtalk::file_type_extension(logtalk, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	add_extension(prolog, Source, SourceWithExtension, Extension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		os::decompose_file_name(Source, _, _, SourceExtension),
		\+ logtalk::file_type_extension(logtalk, SourceExtension),
		(	modules_diagram_support::source_file_extension(SourceExtension) ->
			SourceWithExtension = Source,
			Extension = SourceExtension
		;	% no recognized extension in use
			modules_diagram_support::source_file_extension(Extension),
			atom_concat(Source, Extension, SourceWithExtension)
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
		;	% parametric object
			functor(Object, Functor, Arity),
			functor(Template, Functor, Arity),
			(	object_property(Template, info(Info)) ->
				parameter_names(Template, Info, Names)
			;	parameter_names(Template, [], Names)
			),
			ObjectName =.. [Functor| Names]
		).
	ground_entity_identifier(protocol, Protocol, Protocol).
	ground_entity_identifier(category, Category, CategoryName) :-
		(	atom(Category) ->
			CategoryName = Category
		;	% parametric category
			functor(Category, Functor, Arity),
			functor(Template, Functor, Arity),
			(	category_property(Template, info(Info)) ->
				parameter_names(Template, Info, Names)
			;	parameter_names(Template, [], Names)
			),
			CategoryName =.. [Functor| Names]
		).
	ground_entity_identifier(module, Module, Module).
	ground_entity_identifier(unknown, Entity, EntityName) :-
		(	atom(Entity) ->
			EntityName = Entity
		;	functor(Entity, Functor, Arity),
			functor(EntityName, Functor, Arity),
			EntityName =.. [Functor| Arguments],
			variables_to_underscore(Arguments)
		).

	parameter_names(Entity, Info, Names) :-
		(	member(parnames(Names), Info) ->
			true
		;	member(parameters(Parameters), Info) ->
			keys(Parameters, Names)
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

	:- protected(filter_file_extension/3).
	:- mode(filter_file_extension(+atom, +list(compound), -atom), one).
	:- info(filter_file_extension/3, [
		comment is 'Filters the file name extension depending on the ``file_extensions/1`` option.',
		argnames is ['Basename', 'Options', 'Name']
	]).

	filter_file_extension(Basename, Options, Name) :-
		memberchk(file_extensions(Boolean), Options),
		(	Boolean == true ->
			Name = Basename
		;	os::decompose_file_name(Basename, _, Name, _)
		).

	:- protected(add_link_options/3).
	:- mode(add_link_options(+atom, +list(compound), -list(compound)), one).
	:- info(add_link_options/3, [
		comment is 'Adds ``url/1``, ``urls/2``, and ``tooltip/1`` link options (for use by the graph language) based on the specified path to the list of options.',
		argnames is ['Path', 'Options', 'LinkingOptions']
	]).

	add_link_options(Path, Options, LinkingOptions) :-
		(	member(path_url_prefixes(PathPrefix, CodePrefix, DocPrefix), Options),
			atom_concat(PathPrefix, _, Path) ->
			true
		;	memberchk(url_prefixes(CodePrefix, DocPrefix), Options)
		),
		!,
		memberchk(omit_path_prefixes(Prefixes), Options),
		(	member(Path, Prefixes) ->
			(	CodePrefix == '' ->
				CodeURL = './',
				Suffix = './'
			;	CodeURL = CodePrefix,
				Suffix = CodePrefix
			)
		;	member(Prefix, Prefixes),
			atom_concat(Prefix, Suffix, Path) ->
			atom_concat(CodePrefix, Suffix, CodeURL)
		;	CodeURL = Path,
			Suffix = Path
		),
		LinkingOptions = [url(CodeURL), urls(CodeURL,DocPrefix), tooltip(Suffix)| Options].
	add_link_options(Path, Options, LinkingOptions) :-
		memberchk(omit_path_prefixes(Prefixes), Options),
		(	member(Prefix, Prefixes),
			atom_concat(Prefix, Suffix, Path) ->
			true
		;	Suffix = Path
		),
		LinkingOptions = [url(''), urls('',''), tooltip(Suffix)| Options].

	:- protected(omit_path_prefix/3).
	:- mode(omit_path_prefix(+atom, +list(compound), -atom), one).
	:- info(omit_path_prefix/3, [
		comment is 'Removes a prefix from a path, returning the relative path, when using the option ``omit_path_prefixes/1``. Used mainly for constructing directory and file node identifiers and captions.',
		argnames is ['Path', 'Options', 'Relative']
	]).

	omit_path_prefix(Path, Options, Relative) :-
		memberchk(omit_path_prefixes(Prefixes), Options),
		(	member(Path, Prefixes) ->
			Relative = './'
		;	member(Prefix, Prefixes),
			atom_concat(Prefix, Relative, Path) ->
			true
		;	Relative = Path
		).

	:- protected(add_node_zoom_option/4).
	:- mode(add_node_zoom_option(+atom, +atom, +list(compound), -list(compound)), one).
	:- info(add_node_zoom_option/4, [
		comment is 'Adds node zoom options when using the zoom option.',
		argnames is ['Identifier', 'Suffix', 'Options', 'NodeOptions']
	]).

	add_node_zoom_option(Identifier, Suffix, Options, NodeOptions) :-
		(	member(zoom(false), Options) ->
			NodeOptions = Options
		;	member(zoom_url_suffix(Extension), Options) ->
			(	compound(Identifier) ->
				functor(Identifier, Functor, Arity),
				number_codes(Arity, ArityCodes),
				atom_codes(ArityAtom, ArityCodes),
				atom_concat(Functor, '_', Diagram0),
				atom_concat(Diagram0, ArityAtom, Diagram1),
				atom_concat(Diagram1, Suffix, Diagram2),
				atom_concat(Diagram2, Extension, Diagram)
			;	atom_concat(Identifier, Suffix, Diagram0),
				atom_concat(Diagram0, Extension, Diagram)
			),
			NodeOptions = [zoom_url(Diagram)| Options]
		;	% this case should never occur as the zoom_url_suffix/1 should be always defined
			NodeOptions = Options
		).

	% structured message printing predicates;
	% the main reason to not write directly to an output stream is to
	% allow other tools such as IDEs to intercept and handle results

	:- protected(message_diagram_description/1).
	:- mode(message_diagram_description(?atom), one).
	:- info(message_diagram_description/1, [
		comment is 'Diagram description for progress messages.',
		argnames is ['Description']
	]).

	% default definition; should never be required
	message_diagram_description(Description) :-
		self(Self),
		functor(Self, Description, _).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, diagrams, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, diagrams) -->
		message_tokens(Message).

	message_tokens(generating_diagram(Self, Kind, For)) -->
		{copy_term(For, ForCopy),
		 numbervars(ForCopy, 0, _),
		 Self::message_diagram_description(Description)},
		['Generating ~w diagram for ~w ~q ... '-[Description, Kind, ForCopy]].
	message_tokens(generated_diagram(_Self, _Kind, _For)) -->
		[at_same_line, 'done'-[], nl].
	message_tokens(generating_diagram_failed(Message)) -->
		[nl, 'Generating diagram failed: ~q'-[Message], nl].
	message_tokens(entity_not_loaded(Entity)) -->
		[nl, 'Referenced entity not loaded: ~q'-[Entity], nl].
	message_tokens(unable_to_locate(Kind, Specification)) -->
		[at_same_line, nl, 'Unable to locate ~w: ~q'-[Kind, Specification], nl].

:- end_category.
