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


:- object(file_diagram(_Format),
	extends(diagram)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/12/10,
		comment is 'Predicates for generating file loading dependency diagrams.',
		argnames is ['Format']
	]).

	all(UserOptions) :-
		parameter(1, Format),
		merge_options(UserOptions, Options),
		output_file_path(all_files, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		output_all_files(Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	output_all_files(Options) :-
		logtalk::loaded_file(Path),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		output_file(Path, Basename, Directory, Options),
		fail.
	output_all_files(Options) :-
		parameter(1, Format),
		logtalk::loaded_file(Path),
		logtalk::loaded_file_property(Path, parent(Parent)),
		Format::edge(output_file, Parent, Path, loads, Options),
		fail.
	output_all_files(_).

	rlibrary(Library, UserOptions) :-
		parameter(1, Format),
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopDirectory),
		output_file_path(Library, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		output_rlibrary(TopDirectory, Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	rlibrary(Library) :-
		rlibrary(Library, []).

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
		output_file_path(Library, Options, Format, OutputPath),
		open(OutputPath, write, Stream, [alias(output_file)]),
		Format::output_file_header(output_file, Options),
		output_library(Path, Path, Options),
		Format::output_file_footer(output_file, Options),
		close(Stream).

	library(Library) :-
		library(Library, []).

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
		output_file(Path, Basename, Directory, Options),
		fail.
	output_library_files(_, _).

	output_file(Path, Basename, Directory, Options) :-
		parameter(1, Format),
		(	member(directory_paths(true), Options) ->
			Format::node(output_file, Path, Basename, [Directory], file, Options)
		;	Format::node(output_file, Path, Basename, [], file, Options)
		),
		fail.
	output_file(Path, _, _, Options) :-
		parameter(1, Format),
		logtalk::loaded_file_property(Path, parent(Parent)),
		Format::edge(output_file, Parent, Path, loads, Options),
		fail.
	output_file(_, _, _, _).

	merge_options(UserOptions, Options) :-
		% by default, print library paths:
		(member(library_paths(LibraryPaths), UserOptions) -> true; LibraryPaths = true),
		% by default, print directory paths:
		(member(directory_paths(DirectoryPaths), UserOptions) -> true; DirectoryPaths = true),
		% by default, print current date:
		(member(date(Date), UserOptions) -> true; Date = true),
		% by default, print entity public predicates:
		(member(relation_labels(Relations), UserOptions) -> true; Relations = false),
		% by default, write diagram to the current directory:
		(member(output_path(OutputPath), UserOptions) -> true; OutputPath = './'),
		% by default, don't exclude any source files:
		(member(exclude_files(ExcludedFiles), UserOptions) -> true; ExcludedFiles = []),
		% by default, don't exclude any library sub-directories:
		(member(exclude_paths(ExcludedPaths), UserOptions) -> true; ExcludedPaths = []),
		Options = [
			library_paths(LibraryPaths), directory_paths(DirectoryPaths), date(Date), relation_labels(Relations),
			output_path(OutputPath),
			exclude_files(ExcludedFiles), exclude_paths(ExcludedPaths)].

	output_file_path(Name0, Options, Format, OutputPath) :-
		atom_concat(Name0, '_file_diagram', Name),
		^^output_file_path(Name, Options, Format, OutputPath).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
