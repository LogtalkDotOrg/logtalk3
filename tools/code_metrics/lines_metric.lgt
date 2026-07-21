%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2018-2026 Paulo Moura <pmoura@logtalk.org>
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


:- if(
	(	os::operating_system_type(windows) ->
		os::shell('where.exe cloc > NUL 2>&1'),
		os::shell('where.exe sed > NUL 2>&1')
	;	os::shell('command -v cloc >/dev/null 2>&1')
	)
).

:- object(lines_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-07-22,
		comment is 'Computes the number of lines of code, comments, and blanks by calling cloc and parsing its report file output.',
		remarks is [
			'Entity line range' - 'Entity scores are computed by querying entity lines(BeginLine, EndLine) and running cloc on a temporary file containing only that line range.',
			'External dependency' - 'Requires ``cloc`` to be available in ``PATH``.',
			'Entity score' - 'Represented as the compound term ``lines(Code, Comments, Blanks)``.'
		]
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, loaded_file_property/2, print_message/3
	]).

	:- uses(os, [
		delete_file/1, pid/1, shell/1, temporary_directory/1
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	entity_score(Entity, lines(Code, Comments, Blanks)) :-
		(	var(Entity) ->
			^^current_entity(Entity)
		;	true
		),
		^^entity_property(Entity, file(File)),
		^^entity_property(Entity, lines(BeginLine, EndLine)),
		file_range_stats(File, BeginLine, EndLine, Code, Comments, Blanks).

	process_entity(_, Entity) :-
		entity_score(Entity, Score),
		print_message(information, code_metrics, Score).

	file_score(File, lines(Code, Comments, Blanks), _) :-
		file_stats(File, Code, Comments, Blanks).

	process_file(File, Options) :-
		file_score(File, Score, Options),
		print_message(information, code_metrics, Score).

	directory_score(Directory, Score, Options) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore, Options), FileScores),
		sum_scores(FileScores, Score).

	process_directory(Directory, Options) :-
		directory_score(Directory, Score, Options),
		print_message(information, code_metrics, Score).

	directory_file_score(Directory, File, Lines, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		loaded_file_property(File, directory(Directory)),
		loaded_file_property(File, basename(Basename)),
		^^not_excluded_file(ExcludedFiles, File, Basename),
		file_score(File, Lines, Options).

	rdirectory_score(Directory, Score, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		directory_score(Directory, DirectoryScore, Options),
		(	setof(
				SubDirectory,
				^^sub_directory(Directory, SubDirectory),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		findall(
			FileScore,
			(	member(SubDirectory, SubDirectories),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(SubDirectory, 0, _, _, ExcludedDirectory)
				),
				directory_file_score(SubDirectory, _, FileScore, Options)
			),
			FileScores
		),
		sum_scores([DirectoryScore| FileScores], Score).

	process_rdirectory(Directory, Options) :-
		rdirectory_score(Directory, Score, Options),
		print_message(information, code_metrics, Score).

	library_score(Library, Score, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score, Options).

	process_library(Library, Options) :-
		library_score(Library, Score, Options),
		print_message(information, code_metrics, Score).

	rlibrary_score(Library, Score, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		library_score(Library, LibraryScore, Options),
		(	setof(
				SubLibrary,
				^^sub_library(Library, SubLibrary),
				SubLibraries
			) ->
			true
		;	SubLibraries = []
		),
		findall(
			SubLibraryScore,
			(	member(SubLibrary, SubLibraries),
				\+ member(SubLibrary, ExcludedLibraries),
				library_score(SubLibrary, SubLibraryScore, Options)
			),
			SubLibraryScores
		),
		sum_scores([LibraryScore| SubLibraryScores], Score).

	process_rlibrary(Library, Options) :-
		rlibrary_score(Library, Score, Options),
		print_message(information, code_metrics, Score).

	all_score(Score, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		findall(
			FileScore,
			(	loaded_file_property(File, basename(Basename)),
				^^not_excluded_file(ExcludedFiles, File, Basename),
				file_score(File, FileScore, Options)
			),
			FileScores
		),
		sum_scores(FileScores, Score).

	process_all(Options) :-
		all_score(Score, Options),
		print_message(information, code_metrics, Score).

	format_entity_score(_Entity, lines(Code, Comments, Blanks)) -->
		['Number of code lines: ~w'-[Code], nl],
		['Number of comment lines: ~w'-[Comments], nl],
		['Number of blank lines: ~w'-[Blanks], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(lines(Code, Comments, Blanks), code_metrics) -->
		['Number of code lines: ~w'-[Code], nl],
		['Number of comment lines: ~w'-[Comments], nl],
		['Number of blank lines: ~w'-[Blanks], nl].

	file_range_stats(_File, BeginLine, EndLine, 0, 0, 0) :-
		BeginLine > EndLine,
		!.
	file_range_stats(File, BeginLine, EndLine, Code, Comments, Blanks) :-
		os::internal_os_path(File, FileOS),
		temporary_file('stats', BeginLine, EndLine, '.csv', ReportFile),
		os::internal_os_path(ReportFile, ReportFileOS),
		( 	os::operating_system_type(windows) ->
			temporary_file('range', BeginLine, EndLine, '.lgt', RangeFile),
			os::internal_os_path(RangeFile, RangeFileOS),
			atomic_list_concat([
				'sed -n "', BeginLine, ',', EndLine, 'p" "', FileOS, '" > "', RangeFileOS, '"'
			], ExtractCommand),
			shell(ExtractCommand),
			atomic_list_concat([
				'cloc --quiet --csv --by-file --report-file="', ReportFileOS, '" "', RangeFileOS, '"'
			], ClocCommand),
			shell(ClocCommand),
			delete_file(RangeFile)
		;	atomic_list_concat([
				'sed -n ''', BeginLine, ',', EndLine, 'p'' "', File,
				'" | cloc --quiet --csv --by-file --stdin-name=range.lgt --report-file="', ReportFileOS, '" -'
			], Command),
			shell(Command)
		),
		open(ReportFile, read, Stream),
		find_sum_line(Stream, SumLineCodes),
		close(Stream),
		delete_file(ReportFile),
		parse_sum_line(SumLineCodes, Code, Comments, Blanks).

	file_stats(File, Code, Comments, Blanks) :-
		os::internal_os_path(File, FileOS),
		temporary_file('stats', 0, 0, '.csv', ReportFile),
		os::internal_os_path(ReportFile, ReportFileOS),
		atomic_list_concat([
			'cloc --quiet --csv --by-file --report-file="', ReportFileOS, '" "', FileOS, '"'
		], Command),
		shell(Command),
		open(ReportFile, read, Stream),
		find_sum_line(Stream, SumLineCodes),
		close(Stream),
		delete_file(ReportFile),
		parse_sum_line(SumLineCodes, Code, Comments, Blanks).

	temporary_file(Stem, BeginLine, EndLine, Extension, File) :-
		temporary_directory(Directory),
		pid(PID),
		atomic_list_concat([
			Directory, '/logtalk_', Stem, '_', PID, '_', BeginLine, '_', EndLine, Extension
		], File).

	find_sum_line(Stream, SumLineCodes) :-
		read_line_codes(Stream, LineCodes),
		(	LineCodes == end_of_file ->
			fail
		;	atom_codes(Line, LineCodes),
			sub_atom(Line, 0, 4, _, 'SUM,') ->
			SumLineCodes = LineCodes
		;	find_sum_line(Stream, SumLineCodes)
		).

	parse_sum_line(LineCodes, Code, Comments, Blanks) :-
		atom_codes(Line, LineCodes),
		atom::split(Line, ',', [_, _, BlanksAtom, CommentsAtom, CodeAtom]),
		atom_codes(BlanksAtom, BlanksCodes),
		number_codes(Blanks, BlanksCodes),
		atom_codes(CommentsAtom, CommentsCodes),
		number_codes(Comments, CommentsCodes),
		atom_codes(CodeAtom, CodeCodes),
		number_codes(Code, CodeCodes).

	read_line_codes(Stream, Codes) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Codes = end_of_file
		;	read_line_codes(Code, Stream, Codes)
		).

	read_line_codes(-1, _, []) :-
		!.
	read_line_codes(10, _, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		),
		!.
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

	sum_scores([], lines(0, 0, 0)).
	sum_scores([lines(Code0, Comments0, Blanks0)| Scores], lines(Code, Comments, Blanks)) :-
		sum_scores(Scores, Code0, Code, Comments0, Comments, Blanks0, Blanks).

	sum_scores([], Code, Code, Comments, Comments, Blanks, Blanks).
	sum_scores([lines(Code1, Comments1, Blanks1)| Scores], Code0, Code, Comments0, Comments, Blanks0, Blanks) :-
		Code2 is Code0 + Code1,
		Comments2 is Comments0 + Comments1,
		Blanks2 is Blanks0 + Blanks1,
		sum_scores(Scores, Code2, Code, Comments2, Comments, Blanks2, Blanks).

:- end_object.

:- endif.
