%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2019-09-23,
		comment is 'Source code size metric. Returned scores are upper bounds and based solely in source file sizes (expressed in bytes).'
	]).

	entity_score(Entity, Size) :-
		^^entity_property(Entity, file(File)),
		os::file_size(File, Size).

	process_entity(_, Entity) :-
		entity_score(Entity, Size),
		logtalk::print_message(information, code_metrics, source_code_size(Size)).

	file_score(File, Size) :-
		os::file_size(File, Size).

	process_file(File) :-
		file_score(File, Size),
		logtalk::print_message(information, code_metrics, source_code_size(Size)).

	directory_score(Directory, TotalSize) :-
		findall(Size, directory_file_size(Directory, _, Size), Sizes),
		numberlist::sum(Sizes, TotalSize).

	process_directory(Directory) :-
		directory_score(Directory, TotalSize),
		logtalk::print_message(information, code_metrics, source_code_size(TotalSize)).

	directory_file_size(Directory, File, Size) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(File, directory(DirectorySlash)),
		os::file_size(File, Size).

	rdirectory_score(Directory, TotalSize) :-
		directory_score(Directory, DirectorySize),
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
			(	list::member(SubDirectory, SubDirectories),
				directory_file_size(SubDirectory, _, SubDirectorySize)
			),
			SubDirectorySizes
		),
		numberlist::sum([DirectorySize| SubDirectorySizes], TotalSize).

	process_rdirectory(Directory) :-
		rdirectory_score(Directory, TotalSize),
		logtalk::print_message(information, code_metrics, source_code_size(TotalSize)).

	process_library(Library) :-
		logtalk::expand_library_path(Library, Directory),
		process_directory(Directory).

	rlibrary_score(Library, TotalSize) :-
		library_score(Library, LibrarySize),
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
			(	list::member(SubLibrary, SubLibraries),
				library_score(SubLibrary, SubLibrarySize)
			),
			SubLibrarySizes
		),
		numberlist::sum([LibrarySize| SubLibrarySizes], TotalSize).

	process_rlibrary(Library) :-
		rlibrary_score(Library, TotalSize),
		logtalk::print_message(information, code_metrics, source_code_size(TotalSize)).

	library_score(Library, Size) :-
		logtalk::expand_library_path(Library, Directory),
		directory_score(Directory, Size).

	all_score(TotalSize) :-
		findall(
			Size,
			(	logtalk::loaded_file(File),
				os::file_size(File, Size)
			),
			Sizes
		),
		numberlist::sum(Sizes, TotalSize).

	process_all :-
		all_score(TotalSize),
		logtalk::print_message(information, code_metrics, source_code_size(TotalSize)).

	entity_score(_Entity, Size) -->
		logtalk::message_tokens(source_code_size(Size), code_metrics).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(source_code_size(Size), code_metrics) -->
		['Source code size (upper bound): ~w bytes'-[Size], nl].

:- end_object.
