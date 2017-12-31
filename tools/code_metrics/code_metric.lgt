%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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
		version is 0.6,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2017/12/31,
		comment is 'Logtalk frontend for analyzing source code via metrics.'
	]).

	%% Much of this interface was adapted from `dead_code_scanner` interface code authored by
	%% Paulo Moura and Barry Evans. The derivative code can be found in the Logtalk source
	%% @ tools/dead_code_scanner/dead_code_scanner.lgt

	:- public(entity/1).
	:- mode(entity(+term), zero_or_one).
	:- info(entity/1, [
		comment is 'Scans an entity and prints a summary based on all applicable metrics.',
		argnames is ['Entity']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Prints a metric summary of all entities defined in a source file.',
		argnames is ['File']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans a directory and prints metrics summary for all its source files.',
		argnames is ['Directory']
	]).

	:- public(rdirectory/1).
	:- mode(rdirectory(+atom), one).
	:- info(rdirectory/1, [
		comment is 'Recursive version of directory/1.',
		argnames is ['Directory']
	]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Prints a metrics summary of all loaded entities from a given library.',
		argnames is ['Library']
	]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Recursive version of library/1.',
		argnames is ['Library']
	]).

	:- public(all/0).
	:- mode(all, one).
	:- info(all/0, [
		comment is 'Scans all loaded entities and prints a report of all applicable metrics.'
	]).

	:- protected(process_entity/2).
	:- mode(process_entity(+atom, +entity_identifier), one).
	:- info(process_entity/2, [
		comment is 'Processes an entity of the given kind.',
		argnames is ['Kind', 'Entity']
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

	process_entity(Kind, Entity) :-
		print_message(information, code_metrics, scanning_entity(Kind, Entity)),
		self(Metric),
		::entity_score(Entity, Score),
		print_message(information, code_metrics, entity_score(Entity, Metric, Score)).

	%%%%%%%%%%%%%%%%
	%% File scans %%
	%%%%%%%%%%%%%%%%

	file(Source) :-
		(	locate_file(Source, Path) ->
			write_scan_header('File'),
			process_file(Path),
			write_scan_footer('File')
		;	print_message(warning, code_metrics, unknown(file,Source)),
			fail
		).

	process_file(Path) :-
		print_message(information, code_metrics, scanning_file(Path)),
		forall(
			process_file_(Path, Kind, Entity),
			::process_entity(Kind, Entity)
		).

	process_file_(Path, Kind, Entity) :-
		(	logtalk::loaded_file_property(Path, object(Entity)),
			Kind = object
		;	logtalk::loaded_file_property(Path, protocol(Entity)),
			Kind = protocol
		;	logtalk::loaded_file_property(Path, category(Entity)),
			Kind = category
		).

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
			output_directory_files(Path),
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
			output_rdirectory(Path),
			write_scan_footer('Recursive directory')
		;	print_message(warning, code_metrics, unknown(directory,Directory)),
			fail
		).

	output_rdirectory(Directory) :-
		setof(
			SubDirectory,
			sub_directory(Directory, SubDirectory),
			SubDirectories
		),
		forall(
			member(SubDirectory, SubDirectories),
			output_directory_files(SubDirectory)
		).

	sub_directory(Directory, SubDirectory) :-
		logtalk::loaded_file(Path),
		os::decompose_file_name(Path, SubDirectory, _),
		atom_concat(Directory, _RelativePath, SubDirectory).

	%%%%%%%%%%%%%%%%%%%
	%% Library scans %%
	%%%%%%%%%%%%%%%%%%%

	library(Library) :-
		(	logtalk::expand_library_path(Library, Path) ->
			write_scan_header('Library'),
			output_directory_files(Path),
			write_scan_footer('Library')
		;	print_message(warning, code_metrics, unknown(library,Library)),
			fail
		).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recursive library scans %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	rlibrary(Library) :-
		(	logtalk::expand_library_path(Library, TopPath) ->
			write_scan_header('Recursive library'),
			output_rlibrary(TopPath),
			write_scan_footer('Recursive library')
		;	print_message(warning, code_metrics, unknown(library,Library)),
			fail
		).

	output_rlibrary(TopPath) :-
		forall(
			sub_library(TopPath, LibraryPath),
			output_directory_files(LibraryPath)
		).

	sub_library(TopPath, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	%%%%%%%%%%%%%%%%%%%%%%%
	%% Scan all entities %%
	%%%%%%%%%%%%%%%%%%%%%%%

	all :-
		write_scan_header('All entities'),
		forall(
			all(Kind, Entity),
			::process_entity(Kind, Entity)
		),
		write_scan_footer('All entities').

	all(Kind, Entity) :-
		(	current_object(Entity),
			Kind = object
		;	current_category(Entity),
			Kind = category
		;	current_protocol(Entity),
			Kind = protocol
		).

	% internal/common predicates

	output_directory_files(Directory) :-
		print_message(information, code_metrics, scanning_directory(Directory)),
		forall(
			output_directory_files_(Directory, Path),
			process_file(Path)
		).

	output_directory_files_(Directory, Path) :-
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

	entity_score(_Entity, Score) -->
		{self(Metric)},
		['~w score: ~w'-[Metric, Score], nl].

:- end_category.
