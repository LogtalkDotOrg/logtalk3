%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2022 Paulo Moura <pmoura@logtalk.org>
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


:- category(code_metric).

	:- info([
		version is 0:10:0,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2022-05-05,
		comment is 'Core predicates for computing source code metrics.'
	]).

	% Much of this interface was adapted from `dead_code_scanner` interface code authored
	% by Paulo Moura and Barry Evans. The original code can be found in the Logtalk source
	% @ tools/dead_code_scanner/dead_code_scanner.lgt

	:- public(entity/1).
	:- mode(entity(+term), zero_or_one).
	:- info(entity/1, [
		comment is 'Scans an entity and prints its metric score.',
		argnames is ['Entity']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Prints metric scores for all the entities defined in a loaded source file.',
		argnames is ['File']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans a directory and prints metric scores for all entities defined in its loaded source files.',
		argnames is ['Directory']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Recursive version of the ``directory/1`` predicate.',
		argnames is ['Directory']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Prints metrics scores for all loaded entities from a given library.',
		argnames is ['Library']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Recursive version of the ``library/1`` predicate.',
		argnames is ['Library']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Scans all loaded entities and prints their metric scores.'
	]).

	:- public(entity_score/2).
	:- mode(entity_score(@entity_identifier, -ground), zero_or_one).
	:- info(entity_score/2, [
		comment is 'Score is a term that represents the metric score associated with a loaded entity. Fails if the metric does not apply.',
		argnames is ['Entity', 'Score']
	]).

	:- public(library_score/2).
	:- mode(library_score(@atom, -ground), zero_or_one).
	:- info(library_score/2, [
		comment is 'Score is a term that represents the metric score associated with a loaded library source files. Fails if the metric does not apply.',
		argnames is ['Library', 'Score']
	]).

	:- public(rlibrary_score/2).
	:- mode(rlibrary_score(@atom, -ground), zero_or_one).
	:- info(rlibrary_score/2, [
		comment is 'Score is a term that represents the metric score associated with loaded source files from a library and its sub-libraries. Fails if the metric does not apply.',
		argnames is ['Library', 'Score']
	]).

	:- public(file_score/2).
	:- mode(file_score(@atom, -ground), zero_or_one).
	:- info(file_score/2, [
		comment is 'Score is a term that represents the metric score associated with a loaded source file. Fails if the metric does not apply.',
		argnames is ['File', 'Score']
	]).

	:- public(directory_score/2).
	:- mode(directory_score(@atom, -ground), zero_or_one).
	:- info(directory_score/2, [
		comment is 'Score is a term that represents the metric score associated with loaded source files from a directory. Fails if the metric does not apply.',
		argnames is ['Directory', 'Score']
	]).

	:- public(rdirectory_score/2).
	:- mode(rdirectory_score(@atom, -ground), zero_or_one).
	:- info(rdirectory_score/2, [
		comment is 'Score is a term that represents the metric score associated with loaded source files from a directory and its sub-directories. Fails if the metric does not apply.',
		argnames is ['Directory', 'Score']
	]).

	:- public(all_score/1).
	:- mode(all_score(-ground), zero_or_one).
	:- info(all_score/1, [
		comment is 'Score is a term that represents the metric score associated with all loaded source files. Fails if the metric does not apply.',
		argnames is ['Score']
	]).

	:- public(format_entity_score//2).
	:- mode(format_entity_score(@entity_identifier, +ground), one).
	:- info(format_entity_score//2, [
		comment is 'Formats the entity score for pretty printing.',
		argnames is ['Entity', 'Score']
	]).

	:- protected(process_entity/2).
	:- mode(process_entity(+atom, @entity_identifier), one).
	:- info(process_entity/2, [
		comment is 'Processes an entity of the given kind.',
		argnames is ['Kind', 'Entity']
	]).

	:- protected(process_file/1).
	:- mode(process_file(+atom), one).
	:- info(process_file/1, [
		comment is 'Processes a source file.',
		argnames is ['Path']
	]).

	:- protected(process_directory/1).
	:- mode(process_directory(+atom), one).
	:- info(process_directory/1, [
		comment is 'Processes a directory of source files.',
		argnames is ['Path']
	]).

	:- protected(process_rdirectory/1).
	:- mode(process_rdirectory(+atom), one).
	:- info(process_rdirectory/1, [
		comment is 'Recursively process a directory of source files.',
		argnames is ['Path']
	]).

	:- protected(process_library/1).
	:- mode(process_library(+atom), one).
	:- info(process_library/1, [
		comment is 'Processes a library of source files.',
		argnames is ['Library']
	]).

	:- protected(process_rlibrary/1).
	:- mode(process_rlibrary(+atom), one).
	:- info(process_rlibrary/1, [
		comment is 'Recursively process a library of source files.',
		argnames is ['Library']
	]).

	:- protected(process_all/0).
	:- mode(process_all, one).
	:- info(process_all/0, [
		comment is 'Processes all loaded source code.'
	]).

	:- protected(sub_directory/2).
	:- mode(sub_directory(+atom, -atom), one).
	:- info(sub_directory/2, [
		comment is 'Enumerates, by backtracking, all directory sub-directories containing loaded files.',
		argnames is ['Directory', 'SubDirectory']
	]).

	:- protected(sub_library/2).
	:- mode(sub_library(+atom, -atom), one).
	:- info(sub_library/2, [
		comment is 'Enumerates, by backtracking, all library sub-libraries.',
		argnames is ['Library', 'SubLibrary']
	]).

	:- uses(logtalk, [print_message/3]).
	:- uses(list, [member/2]).

	%%%%%%%%%%%%%%%%%%
	%% Entity scans %%
	%%%%%%%%%%%%%%%%%%

	entity(Entity) :-
		(	current_object(Entity) ->
			Kind = object
		;	current_category(Entity) ->
			Kind = category
		;	current_protocol(Entity) ->
			Kind = protocol
		;	print_message(warning, code_metrics, unknown(entity,Entity)),
			fail
		),
		write_scan_header('Entity'),
		::process_entity(Kind, Entity),
		write_scan_footer('Entity').

	%%%%%%%%%%%%%%%%
	%% File scans %%
	%%%%%%%%%%%%%%%%

	file(Source) :-
		(	locate_file(Source, Path) ->
			write_scan_header('File'),
			::process_file(Path),
			write_scan_footer('File')
		;	print_message(warning, code_metrics, unknown(file,Source)),
			fail
		).

	process_file(Path) :-
		print_message(information, code_metrics, scanning_file(Path)),
		forall(
			file_entity(Path, Kind, Entity),
			::process_entity(Kind, Entity)
		).

	file_entity(Path, object, Entity) :-
		logtalk::loaded_file_property(Path, object(Entity)).
	file_entity(Path, protocol, Entity) :-
		logtalk::loaded_file_property(Path, protocol(Entity)).
	file_entity(Path, category, Entity) :-
		logtalk::loaded_file_property(Path, category(Entity)).

	% file given in library notation
	locate_file(LibraryNotation, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_file(Source, Path).
	% file given using its name or basename
	locate_file(Source, Path) :-
		add_extension(Source, Basename),
		logtalk::loaded_file_property(Path, basename(Basename)),
		% check that there isn't another file with the same basename
		% from a different directory
		\+ (
			logtalk::loaded_file_property(OtherPath, basename(Basename)),
			Path \== OtherPath
		),
		!.
	% file given using a full path
	locate_file(Source, Path) :-
		add_extension(Source, SourceWithExtension),
		logtalk::loaded_file_property(Path, basename(Basename)),
		logtalk::loaded_file_property(Path, directory(Directory)),
		atom_concat(Directory, Basename, SourceWithExtension),
		!.

	add_extension(Source, SourceWithExtension) :-
		% ensure that Source is not specified using library notation
		atom(Source),
		os::decompose_file_name(Source, _, _, SourceExtension),
		(	logtalk::file_type_extension(source, SourceExtension) ->
			% source file extension present
			SourceWithExtension = Source
		;	% try possible source extensions
			logtalk::file_type_extension(source, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	%%%%%%%%%%%%%%%%%%%%%
	%% Directory scans %%
	%%%%%%%%%%%%%%%%%%%%%

	directory(Directory) :-
		(	os::absolute_file_name(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Directory'),
			::process_directory(Path),
			write_scan_footer('Directory')
		;	print_message(warning, code_metrics, unknown(directory,Directory)),
			fail
		).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recursive directory scans %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	rdirectory(Directory) :-
		(	os::absolute_file_name(Directory, Path),
			os::directory_exists(Path) ->
			write_scan_header('Recursive directory'),
			::process_rdirectory(Path),
			write_scan_footer('Recursive directory')
		;	print_message(warning, code_metrics, unknown(directory,Directory)),
			fail
		).

	process_rdirectory(Directory) :-
		(	setof(
				SubDirectory,
				::sub_directory(Directory, SubDirectory),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		forall(
			member(SubDirectory, [Directory| SubDirectories]),
			::process_directory(SubDirectory)
		).

	sub_directory(Directory, SubDirectory) :-
		logtalk::loaded_file(Path),
		os::decompose_file_name(Path, SubDirectory, _),
		Directory \== SubDirectory,
		atom_concat(Directory, _RelativePath, SubDirectory).

	%%%%%%%%%%%%%%%%%%%
	%% Library scans %%
	%%%%%%%%%%%%%%%%%%%

	library(Library) :-
		(	logtalk_library_path(Library, _) ->
			write_scan_header('Library'),
			::process_library(Library),
			write_scan_footer('Library')
		;	print_message(warning, code_metrics, unknown(library,Library)),
			fail
		).

	process_library(Library) :-
		logtalk::expand_library_path(Library, LibraryPath),
		::process_directory(LibraryPath).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recursive library scans %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	rlibrary(Library) :-
		(	logtalk_library_path(Library, _) ->
			write_scan_header('Recursive library'),
			::process_rlibrary(Library),
			write_scan_footer('Recursive library')
		;	print_message(warning, code_metrics, unknown(library,Library)),
			fail
		).

	process_rlibrary(Library) :-
		::process_library(Library),
		forall(
			sub_library(Library, SubLibrary),
			::process_library(SubLibrary)
		).

	sub_library(Library, SubLibrary) :-
		logtalk::expand_library_path(Library, Path),
		logtalk_library_path(SubLibrary, _),
		Library \== SubLibrary,
		logtalk::expand_library_path(SubLibrary, SubPath),
		atom_concat(Path, _RelativePath, SubPath).

	%%%%%%%%%%%%%%%%%%%%%%%
	%% Scan all entities %%
	%%%%%%%%%%%%%%%%%%%%%%%

	all :-
		write_scan_header('All entities'),
		::process_all,
		write_scan_footer('All entities').

	process_all :-
		forall(
			all(Kind, Entity),
			::process_entity(Kind, Entity)
		).

	all(object, Entity) :-
		current_object(Entity).
	all(category, Entity) :-
		current_category(Entity).
	all(protocol, Entity) :-
		current_protocol(Entity).

	% internal/common predicates

	process_directory(Directory) :-
		print_message(information, code_metrics, scanning_directory(Directory)),
		forall(
			directory_file(Directory, Path),
			::process_file(Path)
		).

	directory_file(Directory, Path) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(Path, directory(DirectorySlash)).

	write_scan_header(Type) :-
		print_message(silent, code_metrics, scan_started),
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(
			information,
			code_metrics,
			scan_start_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)
		).

	write_scan_footer(Type) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		print_message(
			information,
			code_metrics,
			scan_end_date_time(Type, Year, Month, Day, Hours, Minutes, Seconds)
		),
		print_message(silent, code_metrics, scan_ended).

	% default definitions

	process_entity(Kind, Entity) :-
		print_message(information, code_metrics, scanning_entity(Kind, Entity)),
		(	::entity_score(Entity, Score) ->
			self(Metric),
			print_message(information, code_metrics, entity_score(Metric, Entity, Score))
		;	true
		).

	format_entity_score(_Entity, Score) -->
		{self(Metric)},
		['~w score: ~w'-[Metric, Score], nl].

:- end_category.


:- if(current_logtalk_flag(prolog_dialect, gnu)).
	% workaround gplc limitation when dealing with multifile predicates
	% that are called from a file but not defined in that file
	:- multifile(logtalk_library_path/2).
	:- dynamic(logtalk_library_path/2).
:- endif.
