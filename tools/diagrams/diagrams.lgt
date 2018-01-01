%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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



:- object(diagrams(_Format)).

	:- info([
		version is 2.3,
		author is 'Paulo Moura',
		date is 2016/05/07,
		comment is 'Predicates for generating all supported diagrams for libraries, directories, or files in one step using the specified format.',
		parnames is ['Format'],
		remarks is [
			'Common options' - 'title/1, date/1, output_directory/1, relation_labels/1, node_type_captions/1, exclude_files/1, exclude_libraries/1, url_prefixes/1, omit_path_prefix/1, entity_url_suffix_target/2, and layout/1.',
			'Limitations' - 'Some of the provided predicates only make sense for some types of diagrams. Also, fine tuning may require generating individual diagrams directly instead of as a batch using this utility object.'
		]
	]).

	:- public(libraries/3).
	:- mode(libraries(+atom, +list(atom), +list(compound)), one).
	:- info(libraries/3, [
		comment is 'Creates all supported diagrams for a set of libraries using the specified options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Libraries', 'Options']
	]).

	libraries(Project, Libraries, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(libraries, Format, Diagram),
			Diagram::libraries(Project, Libraries, Options)
		).

	:- public(libraries/2).
	:- mode(libraries(+atom, +list(atom)), one).
	:- info(libraries/2, [
		comment is 'Creates all supported diagrams for a set of libraries using the default options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Libraries']
	]).

	libraries(Project, Libraries) :-
		::libraries(Project, Libraries, []).

	:- public(libraries/1).
	:- mode(libraries(+list(atom)), one).
	:- info(libraries/1, [
		comment is 'Creates  all supported diagrams for a set of libraries using the default options. The prefix "libraries" is used for the diagram file names.',
		argnames is ['Libraries']
	]).

	libraries(Libraries) :-
		::libraries(libraries, Libraries, []).

	:- public(all_libraries/1).
	:- mode(all_libraries(+list(compound)), one).
	:- info(all_libraries/1, [
		comment is 'Creates all supported diagrams for all loaded libraries using the specified options.',
		argnames is ['Options']
	]).

	all_libraries(Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(all_libraries, Format, Diagram),
			Diagram::all_libraries(Options)
		).

	:- public(all_libraries/0).
	:- mode(all_libraries, one).
	:- info(all_libraries/0, [
		comment is 'Creates all supported diagrams for all loaded libraries using default options.'
	]).

	all_libraries :-
		::all_libraries([]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list(compound)), one).
	:- info(rlibrary/2, [
		comment is 'Creates all supported diagrams for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	rlibrary(Library, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(rlibrary, Format, Diagram),
			Diagram::rlibrary(Library, Options)
		).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates all supported diagrams for a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	rlibrary(Library) :-
		::rlibrary(Library, []).

	:- public(library/2).
	:- mode(library(+atom, +list(compound)), one).
	:- info(library/2, [
		comment is 'Creates all supported diagrams for a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	library(Library, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(library, Format, Diagram),
			Diagram::library(Library, Options)
		).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates all supported diagrams for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(directories/3).
	:- mode(directories(+atom, +list(atom), +list(compound)), one).
	:- info(directories/3, [
		comment is 'Creates all supported diagrams for a set of directories using the specified options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Directories', 'Options']
	]).

	directories(Project, Directories, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(directories, Format, Diagram),
			Diagram::directories(Project, Directories, Options)
		).

	:- public(directories/2).
	:- mode(directories(+atom, +list(atom)), one).
	:- info(directories/2, [
		comment is 'Creates a diagram for a directory using default options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Directories']
	]).

	directories(Project, Directories) :-
		::directories(Project, Directories, []).

	:- public(directory/3).
	:- mode(directory(+atom, +atom, +list(compound)), one).
	:- info(directory/3, [
		comment is 'Creates all supported diagrams for a directory using the specified options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Directory', 'Options']
	]).

	directory(Project, Directory, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(directory, Format, Diagram),
			Diagram::directory(Project, Directory, Options)
		).

	:- public(directory/2).
	:- mode(directory(+atom, +atom), one).
	:- info(directory/2, [
		comment is 'Creates all supported diagrams for a directory using default options. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Directory']
	]).

	directory(Project, Directory) :-
		::directory(Project, Directory, []).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Creates all supported diagrams for a directory using default options. The name of the directory is used as a prefix for the diagram file names.',
		argnames is ['Directory']
	]).

	directory(Directory) :-
		os::absolute_file_name(Directory, Path),
		os::decompose_file_name(Path, _, Project, _),
		::directory(Project, Directory, []).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list(compound)), one).
	:- info(files/3, [
		comment is 'Creates all supported diagrams for a set of files using the specified options. The file can be specified by name, basename, full path, or using library notation. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Files', 'Options']
	]).

	files(Project, Files, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(files, Format, Diagram),
			Diagram::files(Project, Files, Options)
		).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates all supported diagrams for a set of files using the default options. The file can be specified by name, basename, full path, or using library notation. The Project argument is used as a prefix for the diagram file names.',
		argnames is ['Project', 'Files']
	]).

	files(Project, Files) :-
		::files(Project, Files, []).

	:- public(files/1).
	:- mode(files(+list(atom)), one).
	:- info(files/1, [
		comment is 'Creates a diagram for a set of files using the default options. The file can be specified by name, basename, full path, or using library notation. The prefix "files" is used for the diagram file names.',
		argnames is ['Files']
	]).

	files(Files) :-
		::files(files, Files, []).

	:- public(all_files/1).
	:- mode(all_files(+list(compound)), one).
	:- info(all_files/1, [
		comment is 'Creates all supported diagrams for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	all_files(Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(all_files, Format, Diagram),
			Diagram::all_files(Options)
		).

	:- public(all_files/0).
	:- mode(all_files, one).
	:- info(all_files/0, [
		comment is 'Creates all supported diagrams for all loaded files using default options.'
	]).

	all_files :-
		::all_files([]).

	% supported_diagram(+atom, -entity_identifier)
	supported_diagram(_,         Format, entity_diagram(Format)).
	supported_diagram(_,         Format, inheritance_diagram(Format)).
	supported_diagram(_,         Format, uses_diagram(Format)).
	supported_diagram(_,         Format, xref_diagram(Format)).
	supported_diagram(_,         Format, file_dependency_diagram(Format)).
	supported_diagram(_,         Format, file_load_diagram(Format)).
	supported_diagram(Predicate, Format, library_dependency_diagram(Format)) :-
		list::memberchk(Predicate, [all_libraries, libraries, rlibrary, library]).
	supported_diagram(Predicate, Format, library_load_diagram(Format)) :-
		list::memberchk(Predicate, [all_libraries, libraries, rlibrary, library]).

:- end_object.



:- object(diagrams,
	extends(diagrams(dot))).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/03/09,
		comment is 'Predicates for generating all supported diagrams for libraries and files in one step using the DOT format.'
	]).

:- end_object.
