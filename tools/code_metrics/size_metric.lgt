%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- object(size_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0:6:0,
		author is 'Paulo Moura',
		date is 2024-03-27,
		comment is 'Source code size metric. Returned scores are upper bounds and based solely in source file sizes (expressed in bytes).'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, loaded_file/1, loaded_file_property/2, print_message/3
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(os, [
		file_size/2
	]).

	entity_score(Entity, Size) :-
		^^entity_property(Entity, file(File)),
		file_size(File, Size).

	process_entity(_, Entity) :-
		entity_score(Entity, Size),
		print_message(information, code_metrics, source_code_size(Size)).

	file_score(File, Size) :-
		file_size(File, Size).

	process_file(File, _) :-
		file_score(File, Size),
		print_message(information, code_metrics, source_code_size(Size)).

	directory_score(Directory, TotalSize, Options) :-
		findall(Size, directory_file_size(Directory, _, Size, Options), Sizes),
		sum(Sizes, TotalSize).

	process_directory(Directory, Options) :-
		directory_score(Directory, TotalSize, Options),
		print_message(information, code_metrics, source_code_size(TotalSize)).

	directory_file_size(Directory, File, Size, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		loaded_file_property(File, directory(DirectorySlash)),
		\+ member(File, ExcludedFiles),
		file_size(File, Size).

	rdirectory_score(Directory, TotalSize, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		directory_score(Directory, DirectorySize, Options),
		(	setof(
				SubDirectory,
				^^sub_directory(Directory, SubDirectory),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		findall(
			SubDirectorySize,
			(	member(SubDirectory, SubDirectories),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(SubDirectory, 0, _, _, ExcludedDirectory)
				),
				directory_file_size(SubDirectory, _, SubDirectorySize, Options)
			),
			SubDirectorySizes
		),
		sum([DirectorySize| SubDirectorySizes], TotalSize).

	process_rdirectory(Directory, Options) :-
		rdirectory_score(Directory, TotalSize, Options),
		print_message(information, code_metrics, source_code_size(TotalSize)).

	process_library(Library, Options) :-
		expand_library_path(Library, Directory),
		process_directory(Directory, Options).

	rlibrary_score(Library, TotalSize, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		library_score(Library, LibrarySize, Options),
		(	setof(
				SubLibrary,
				^^sub_library(Library, SubLibrary),
				SubLibraries
			) ->
			true
		;	SubLibraries = []
		),
		findall(
			SubLibrarySize,
			(	member(SubLibrary, SubLibraries),
				\+ member(SubLibrary, ExcludedLibraries),
				library_score(SubLibrary, SubLibrarySize, Options)
			),
			SubLibrarySizes
		),
		sum([LibrarySize| SubLibrarySizes], TotalSize).

	process_rlibrary(Library, Options) :-
		rlibrary_score(Library, TotalSize, Options),
		print_message(information, code_metrics, source_code_size(TotalSize)).

	library_score(Library, Size, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Size, Options).

	all_score(TotalSize, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		findall(
			Size,
			(	loaded_file(File),
				\+ member(File, ExcludedFiles),
				file_size(File, Size)
			),
			Sizes
		),
		sum(Sizes, TotalSize).

	process_all(Options) :-
		all_score(TotalSize, Options),
		print_message(information, code_metrics, source_code_size(TotalSize)).

	format_entity_score(_Entity, Size) -->
		logtalk::message_tokens(source_code_size(Size), code_metrics).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(source_code_size(Size), code_metrics) -->
		['Source code size (upper bound): ~w bytes'-[Size], nl].

:- end_object.
