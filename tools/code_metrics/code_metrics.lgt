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


:- object(code_metrics).

	:- info([
		version is 0.2,
		author is 'Ebrahim Azarisooreh',
		date is 2017/02/014,
		comment is 'Logtalk frontend for analyzing source code via metrics.'
	]).

	%% Much of this interface was adapted from `dead_code_scanner` interface code authored by
	%% Paulo Moura and Barry Evans. The derivative code can be found in the Logtalk source
	%% @ tools/dead_code_scanner/dead_code_scanner.lgt

	:- public(item/1).
	:- mode(item(+term), zero_or_one).
	:- info(item/1, [
		comment is 'Scans Item and prints a summary based on all applicable metrics.',
		argnames is ['Item']
	]).

	:- public(file/1).
	:- mode(file(+atom), zero_or_one).
	:- info(file/1, [
		comment is 'Prints a metric summary of all items present in a file.',
		argnames is ['File']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), one).
	:- info(directory/1, [
		comment is 'Scans Directory and prints metrics summary.',
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
		comment is 'Prints a metrics summary of all loaded items from a given library.',
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
		comment is 'Scans all loaded items and prints a report of all applicable metrics.'
	]).

	:- uses(logtalk, [print_message/3]).
	:- uses(list, [member/2]).

	%%%%%%%%%%%%%%%%
	%% Item scans %%
	%%%%%%%%%%%%%%%%

	item(Item) :-
		(	current_object(Item) ->
			Kind = object
		;	current_category(Item) ->
			Kind = category
		;	current_protocol(Item),
			Kind = protocol
		),
		write_scan_header('Item'),
		process_item(Kind, Item),
		write_scan_footer('Item').

	process_item(Kind, Item) :-
		print_message(information, code_metrics, scanning_item(Kind, Item)),
		forall(
			process_item_(Item, Metric, Score),
			print_message(information, code_metrics, item_score(Item, Metric, Score))
		).

	process_item_(Item, Metric, Score) :-
		imports_category(Metric, code_metrics_utilities),
		Metric::item_score(Item, Score).

	%%%%%%%%%%%%%%%%
	%% File scans %%
	%%%%%%%%%%%%%%%%

	file(Source) :-
		locate_file(Source, Path),
		write_scan_header('File'),
		process_file(Path),
		write_scan_footer('File').

	process_file(Path) :-
		print_message(information, code_metrics, scanning_file(Path)),
		forall(
			process_file_(Path, Kind, Item),
			process_item(Kind, Item)
		).

	process_file_(Path, Kind, Item) :-
		(	logtalk::loaded_file_property(Path, object(Item)),
			Kind = object
		;	logtalk::loaded_file_property(Path, protocol(Item)),
			Kind = protocol
		;	logtalk::loaded_file_property(Path, category(Item)),
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
		atom(Source),
		(	sub_atom(Source, _, 4, 0, '.lgt') ->
			SourceWithExtension = Source
		;	sub_atom(Source, _, 8, 0, '.logtalk') ->
			SourceWithExtension = Source
		;	(	atom_concat(Source, '.lgt', SourceWithExtension)
			;	atom_concat(Source, '.logtalk', SourceWithExtension)
			)
		).

	%%%%%%%%%%%%%%%%%%%%%
	%% Directory scans %%
	%%%%%%%%%%%%%%%%%%%%%

	directory(Directory) :-
		write_scan_header('Directory'),
		os::expand_path(Directory, Path),
		output_directory_files(Path),
		write_scan_footer('Directory').

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recursive directory scans %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	rdirectory(Directory) :-
		write_scan_header('Recursive directory'),
		os::expand_path(Directory, Path),
		output_rdirectory(Path),
		write_scan_footer('Recursive directory').

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
		os::decompose_file_name(Path, SubDirectory, _, _),
		atom_concat(Directory, _RelativePath, SubDirectory).

	%%%%%%%%%%%%%%%%%%%
	%% Library scans %%
	%%%%%%%%%%%%%%%%%%%

	library(Library) :-
		write_scan_header('Library'),
		logtalk::expand_library_path(Library, Path),
		output_directory_files(Path),
		write_scan_footer('Library').

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Recursive library scans %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	rlibrary(Library) :-
		write_scan_header('Recursive library'),
		logtalk::expand_library_path(Library, TopPath),
		output_rlibrary(TopPath),
		write_scan_footer('Recursive library').

	output_rlibrary(TopPath) :-
		forall(
			sub_library(TopPath, LibraryPath),
			output_directory_files(LibraryPath)
		).

	sub_library(TopPath, LibraryPath) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	%%%%%%%%%%%%%%%%%%%%
	%% Scan all items %%
	%%%%%%%%%%%%%%%%%%%%

	all :-
		write_scan_header('All items'),
		forall(
			all(Kind, Item),
			process_item(Kind, Item)
		),
		write_scan_footer('All items').

	all(Kind, Item) :-
		(	current_object(Item),
			Kind = object
		;	current_category(Item),
			Kind = category
		;	current_protocol(Item),
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

:- end_object.
