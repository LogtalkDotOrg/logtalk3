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



:- object(diagrams(_Format)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2014/06/20,
		comment is 'Predicates for generating all supported diagrams for libraries and files in one step using the specified format.',
		parnames is ['Format'],
		remarks is [
			'Common options:' - 'title/1, date/1, output_directory/1, relation_labels/1, node_type_captions/1, exclude_files/1, exclude_libraries/1, url_prefixes/1, and omit_path_prefix/1.'
		]
	]).

	:- public(libraries/3).
	:- mode(libraries(+atom, +list(atom), +list(compound)), one).
	:- info(libraries/3, [
		comment is 'Creates all supported diagrams for a set of libraries using the specified options.',
		argnames is ['Project', 'Libraries', 'Options']
	]).

	libraries(Project, Libraries, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(Format, Diagram),
			Diagram::libraries(Project, Libraries, Options)
		).

	:- public(libraries/2).
	:- mode(libraries(+atom, +list(atom)), one).
	:- info(libraries/2, [
		comment is 'Creates all supported diagrams for a set of libraries using the default options.',
		argnames is ['Project', 'Libraries']
	]).

	libraries(Project, Libraries) :-
		::libraries(Project, Libraries, []).

	:- public(all_libraries/1).
	:- mode(all_libraries(+list(compound)), one).
	:- info(all_libraries/1, [
		comment is 'Creates all supported diagrams for all loaded libraries using the specified options.',
		argnames is ['Options']
	]).

	all_libraries(Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(Format, Diagram),
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
			supported_diagram(Format, Diagram),
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
			supported_diagram(Format, Diagram),
			Diagram::library(Library, Options)
		).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'all supported diagrams for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list(compound)), one).
	:- info(files/3, [
		comment is 'Creates all supported diagrams for a set of files using the specified options. The file can be specified by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files', 'Options']
	]).

	files(Project, Files, Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(Format, Diagram),
			Diagram::files(Project, Files, Options)
		).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates all supported diagrams for a set of files using the default options. The file can be specified by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files']
	]).

	files(Project, Files) :-
		::files(Project, Files, []).

	:- public(all_files/1).
	:- mode(all_files(+list(compound)), one).
	:- info(all_files/1, [
		comment is 'Creates all supported diagrams for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	all_files(Options) :-
		parameter(1, Format),
		forall(
			supported_diagram(Format, Diagram),
			Diagram::files(Options)
		).

	:- public(all_files/0).
	:- mode(all_files, one).
	:- info(all_files/0, [
		comment is 'Creates all supported diagrams for all loaded files using default options.'
	]).

	all_files :-
		::all_files([]).

	% supported_diagram(+atom, -entity_identifier)
	supported_diagram(Format, entity_diagram(Format)).
	supported_diagram(Format, inheritance_diagram(Format)).
	supported_diagram(Format, uses_diagram(Format)).
	supported_diagram(Format, xref_diagram(Format)).
	supported_diagram(Format, file_dependency_diagram(Format)).
	supported_diagram(Format, file_load_diagram(Format)).

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
