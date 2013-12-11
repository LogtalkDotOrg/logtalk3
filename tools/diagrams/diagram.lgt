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


:- object(diagram).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2013/12/11,
		comment is 'Predicates for generating diagrams.'
	]).

	:- public(all/1).
	:- mode(all(+atom), one).
	:- info(all/1, [
		comment is 'Creates a diagram for all loaded files using the specified options.',
		argnames is ['Options']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Creates a diagram for all loaded files using default options.'
	]).

	all :-
		::all([]).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for a library and its sub-libraries using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for a library and its sub-libraries using default options.',
		argnames is ['Library']
	]).

	rlibrary(Library) :-
		::rlibrary(Library, []).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates a diagram for a library using the specified options.',
		argnames is ['Library', 'Options']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for a library using default options.',
		argnames is ['Library']
	]).

	library(Library) :-
		::library(Library, []).

	:- public(files/3).
	:- mode(files(+atom, +list(atom), +list), one).
	:- info(files/3, [
		comment is 'Creates a diagram for a set of files using the specified options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files', 'Options']
	]).

	:- public(files/2).
	:- mode(files(+atom, +list(atom)), one).
	:- info(files/2, [
		comment is 'Creates a diagram for a set of files using the default options. The file can be given by name, basename, full path, or using library notation.',
		argnames is ['Project', 'Files']
	]).

	files(Project, Files) :-
		::files(Project, Files, []).

	:- public(default_options/1).
	:- mode(default_options(-list), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']
	]).

	default_options(DefaultOptions) :-
		::merge_options([], DefaultOptions).

	:- protected(merge_options/2).
	:- mode(merge_options(+list, -list), one).
	:- info(merge_options/2, [
		comment is 'Returns the list of options used when generating a diagram.',
		argnames is ['UserOptions', 'DefaultOptions']
	]).

	:- protected(not_excluded_file/3).
	:- mode(not_excluded_file(+list(atom), +atom, +atom), zero_or_one).
	:- info(not_excluded_file/3, [
		comment is 'Returns the list of options used when generating a diagram.',
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

	:- protected(output_file_path/4).
	:- mode(output_file_path(+atom, +list(atom), +object_identifier, -atom), one).
	:- info(output_file_path/4, [
		comment is 'Returns the output file path.',
		argnames is ['Name', 'Options', 'Format', 'Path']
	]).

	output_file_path(Name, Options, Format, Path) :-
		Format::output_file_name(Name, Basename),
		member(output_path(Directory0), Options),
		(	sub_atom(Directory0, _, _, 0, '/') ->
			Directory = Directory0
		;	atom_concat(Directory0, '/', Directory)
		),
		atom_concat(Directory, Basename, Path).

	:- protected(locate_file/4).
	:- mode(locate_file(+atom, +list(atom), +object_identifier, -atom), one).
	:- info(locate_file/4, [
		comment is 'Locates a file given its name, basename, full path, or library notation representation.',
		argnames is ['File', 'Basename', 'Directory', 'Path']
	]).

	% file given in library notation
	locate_file(LibraryNotation, Basename, Directory, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Basename, Directory, Path).
	% file given using its name or basename
	locate_file(Source, Basename, Directory, Path) :-
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
	locate_file(Source, Basename, Directory, Path) :-
		add_extension(Source, Path),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
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

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
