%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2021-08-03,
		comment is 'Unit tests for the "diagrams" tool.'
	]).

	cover(diagram(_)).
	cover(diagrams(_)).
	cover(entity_diagram(_)).
	cover(directory_dependency_diagram(_)).
	cover(directory_diagram(_)).
	cover(directory_load_diagram(_)).
	cover(file_dependency_diagram(_)).
	cover(file_diagram(_)).
	cover(file_load_diagram(_)).
	cover(graph_language_registry).
	cover(inheritance_diagram(_)).
	cover(library_dependency_diagram(_)).
	cover(library_diagram(_)).
	cover(library_load_diagram(_)).
	:- if(current_logtalk_flag(modules, supported)).
		cover(modules_diagram_support).
	:- endif.
	cover(uses_diagram(_)).
	cover(xref_diagram(_)).

	:- uses(lgtunit, [
		deterministic/1
	]).

	:- uses(os, [
		directory_files/2, delete_file/1, delete_directory/1
	]).

	:- uses(list, [
		member/2
	]).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	% entity_diagram tests

	test(entity_diagram_libraries_3_01, deterministic) :-
		entity_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(entity_diagram_libraries_2_01, deterministic) :-
		entity_diagram::libraries(test, [code_metrics,lgtunit]).

	test(entity_diagram_libraries_1_01, deterministic) :-
		entity_diagram::libraries([code_metrics,lgtunit]).

	test(entity_diagram_all_libraries_1_01, deterministic) :-
		entity_diagram::all_libraries([]).

	test(entity_diagram_all_libraries_0_01, deterministic) :-
		entity_diagram::all_libraries.

	test(entity_diagram_rlibrary_2_01, deterministic) :-
		entity_diagram::rlibrary(lgtunit, []).

	test(entity_diagram_rlibrary_1_01, deterministic) :-
		entity_diagram::rlibrary(lgtunit).

	test(entity_diagram_library_2_01, deterministic) :-
		entity_diagram::library(lgtunit, []).

	test(entity_diagram_library_1_01, deterministic) :-
		entity_diagram::library(lgtunit).

	test(entity_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		entity_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(entity_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		entity_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(entity_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		entity_diagram::directory(test, Directory, []).

	test(entity_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		entity_diagram::directory(test, Directory).

	test(entity_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		entity_diagram::directory(Directory).

	test(entity_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		entity_diagram::files(test, Files, []).

	test(entity_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		entity_diagram::files(test, Files).

	test(entity_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		entity_diagram::files(Files).

	test(entity_diagram_all_files_1_01, deterministic) :-
		entity_diagram::all_files([]).

	test(entity_diagram_all_files_0_01, deterministic) :-
		entity_diagram::all_files.

	test(entity_diagram_file_2_01, deterministic) :-
		object_property(entity_diagram, file(File)),
		entity_diagram::file(File, []).

	test(entity_diagram_file_1_01, deterministic) :-
		object_property(entity_diagram, file(File)),
		entity_diagram::file(File).

	% inheritance_diagram tests

	test(inheritance_diagram_libraries_3_01, deterministic) :-
		inheritance_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(inheritance_diagram_libraries_2_01, deterministic) :-
		inheritance_diagram::libraries(test, [code_metrics,lgtunit]).

	test(inheritance_diagram_libraries_1_01, deterministic) :-
		inheritance_diagram::libraries([code_metrics,lgtunit]).

	test(inheritance_diagram_all_libraries_1_01, deterministic) :-
		inheritance_diagram::all_libraries([]).

	test(inheritance_diagram_all_libraries_0_01, deterministic) :-
		inheritance_diagram::all_libraries.

	test(inheritance_diagram_rlibrary_2_01, deterministic) :-
		inheritance_diagram::rlibrary(lgtunit, []).

	test(inheritance_diagram_rlibrary_1_01, deterministic) :-
		inheritance_diagram::rlibrary(lgtunit).

	test(inheritance_diagram_library_2_01, deterministic) :-
		inheritance_diagram::library(lgtunit, []).

	test(inheritance_diagram_library_1_01, deterministic) :-
		inheritance_diagram::library(lgtunit).

	test(inheritance_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		inheritance_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(inheritance_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		inheritance_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(inheritance_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		inheritance_diagram::directory(test, Directory, []).

	test(inheritance_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		inheritance_diagram::directory(test, Directory).

	test(inheritance_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		inheritance_diagram::directory(Directory).

	test(inheritance_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		inheritance_diagram::files(test, Files, []).

	test(inheritance_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		inheritance_diagram::files(test, Files).

	test(inheritance_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		inheritance_diagram::files(Files).

	test(inheritance_diagram_all_files_1_01, deterministic) :-
		inheritance_diagram::all_files([]).

	test(inheritance_diagram_all_files_0_01, deterministic) :-
		inheritance_diagram::all_files.

	test(inheritance_diagram_file_2_01, deterministic) :-
		object_property(inheritance_diagram, file(File)),
		inheritance_diagram::file(File, []).

	test(inheritance_diagram_file_1_01, deterministic) :-
		object_property(inheritance_diagram, file(File)),
		inheritance_diagram::file(File).

	% directory_dependency_diagram tests

	test(directory_dependency_diagram_rdirectory_3_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_dependency_diagram::rdirectory(test, LgtunitDirectory, []).

	test(directory_dependency_diagram_rdirectory_2_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_dependency_diagram::rdirectory(test, LgtunitDirectory, []).

	test(directory_dependency_diagram_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_dependency_diagram::rdirectory(LgtunitDirectory).

	test(directory_dependency_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(directory_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(directory_dependency_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_dependency_diagram::directory(test, Directory, []).

	test(directory_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_dependency_diagram::directory(test, Directory).

	test(directory_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_dependency_diagram::directory(Directory).

	test(directory_dependency_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_dependency_diagram::files(test, Files, []).

	test(directory_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_dependency_diagram::files(test, Files).

	test(directory_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_dependency_diagram::files(Files).

	test(directory_dependency_diagram_all_files_1_01, deterministic) :-
		directory_dependency_diagram::all_files([]).

	test(directory_dependency_diagram_all_files_0_01, deterministic) :-
		directory_dependency_diagram::all_files.

	% directory_load_diagram tests

	test(directory_load_diagram_rdirectory_3_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_load_diagram::rdirectory(test, LgtunitDirectory, []).

	test(directory_load_diagram_rdirectory_2_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_load_diagram::rdirectory(test, LgtunitDirectory, []).

	test(directory_load_diagram_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_load_diagram::rdirectory(LgtunitDirectory).

	test(directory_load_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(directory_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		directory_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(directory_load_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_load_diagram::directory(test, Directory, []).

	test(directory_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_load_diagram::directory(test, Directory).

	test(directory_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		directory_load_diagram::directory(Directory).

	test(directory_load_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_load_diagram::files(test, Files, []).

	test(directory_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_load_diagram::files(test, Files).

	test(directory_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		directory_load_diagram::files(Files).

	test(directory_load_diagram_all_files_1_01, deterministic) :-
		directory_load_diagram::all_files([]).

	test(directory_load_diagram_all_files_0_01, deterministic) :-
		directory_load_diagram::all_files.

	% file_dependency_diagram tests

	test(file_dependency_diagram_libraries_3_01, deterministic) :-
		file_dependency_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(file_dependency_diagram_libraries_2_01, deterministic) :-
		file_dependency_diagram::libraries(test, [code_metrics,lgtunit]).

	test(file_dependency_diagram_libraries_1_01, deterministic) :-
		file_dependency_diagram::libraries([code_metrics,lgtunit]).

	test(file_dependency_diagram_all_libraries_1_01, deterministic) :-
		file_dependency_diagram::all_libraries([]).

	test(file_dependency_diagram_all_libraries_0_01, deterministic) :-
		file_dependency_diagram::all_libraries.

	test(file_dependency_diagram_rlibrary_2_01, deterministic) :-
		file_dependency_diagram::rlibrary(lgtunit, []).

	test(file_dependency_diagram_rlibrary_1_01, deterministic) :-
		file_dependency_diagram::rlibrary(lgtunit).

	test(file_dependency_diagram_library_2_01, deterministic) :-
		file_dependency_diagram::library(lgtunit, []).

	test(file_dependency_diagram_library_1_01, deterministic) :-
		file_dependency_diagram::library(lgtunit).

	test(file_dependency_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		file_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(file_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		file_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(file_dependency_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_dependency_diagram::directory(test, Directory, []).

	test(file_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_dependency_diagram::directory(test, Directory).

	test(file_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_dependency_diagram::directory(Directory).

	test(file_dependency_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_dependency_diagram::files(test, Files, []).

	test(file_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_dependency_diagram::files(test, Files).

	test(file_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_dependency_diagram::files(Files).

	test(file_dependency_diagram_all_files_1_01, deterministic) :-
		file_dependency_diagram::all_files([]).

	test(file_dependency_diagram_all_files_0_01, deterministic) :-
		file_dependency_diagram::all_files.

	% file_load_diagram tests

	test(file_load_diagram_libraries_3_01, deterministic) :-
		file_load_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(file_load_diagram_libraries_2_01, deterministic) :-
		file_load_diagram::libraries(test, [code_metrics,lgtunit]).

	test(file_load_diagram_libraries_1_01, deterministic) :-
		file_load_diagram::libraries([code_metrics,lgtunit]).

	test(file_load_diagram_all_libraries_1_01, deterministic) :-
		file_load_diagram::all_libraries([]).

	test(file_load_diagram_all_libraries_0_01, deterministic) :-
		file_load_diagram::all_libraries.

	test(file_load_diagram_rlibrary_2_01, deterministic) :-
		file_load_diagram::rlibrary(lgtunit, []).

	test(file_load_diagram_rlibrary_1_01, deterministic) :-
		file_load_diagram::rlibrary(lgtunit).

	test(file_load_diagram_library_2_01, deterministic) :-
		file_load_diagram::library(lgtunit, []).

	test(file_load_diagram_library_1_01, deterministic) :-
		file_load_diagram::library(lgtunit).

	test(file_load_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		file_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(file_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		file_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(file_load_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_load_diagram::directory(test, Directory, []).

	test(file_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_load_diagram::directory(test, Directory).

	test(file_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		file_load_diagram::directory(Directory).

	test(file_load_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_load_diagram::files(test, Files, []).

	test(file_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_load_diagram::files(test, Files).

	test(file_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		file_load_diagram::files(Files).

	test(file_load_diagram_all_files_1_01, deterministic) :-
		file_load_diagram::all_files([]).

	test(file_load_diagram_all_files_0_01, deterministic) :-
		file_load_diagram::all_files.

	% library_dependency_diagram tests

	test(library_dependency_diagram_libraries_3_01, deterministic) :-
		library_dependency_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(library_dependency_diagram_libraries_2_01, deterministic) :-
		library_dependency_diagram::libraries(test, [code_metrics,lgtunit]).

	test(library_dependency_diagram_libraries_1_01, deterministic) :-
		library_dependency_diagram::libraries([code_metrics,lgtunit]).

	test(library_dependency_diagram_all_libraries_1_01, deterministic) :-
		library_dependency_diagram::all_libraries([]).

	test(library_dependency_diagram_all_libraries_0_01, deterministic) :-
		library_dependency_diagram::all_libraries.

	test(library_dependency_diagram_rlibrary_2_01, deterministic) :-
		library_dependency_diagram::rlibrary(lgtunit, []).

	test(library_dependency_diagram_rlibrary_1_01, deterministic) :-
		library_dependency_diagram::rlibrary(lgtunit).

	test(library_dependency_diagram_library_2_01, deterministic) :-
		library_dependency_diagram::library(lgtunit, []).

	test(library_dependency_diagram_library_1_01, deterministic) :-
		library_dependency_diagram::library(lgtunit).

	test(library_dependency_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		library_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(library_dependency_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		library_dependency_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(library_dependency_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_dependency_diagram::directory(test, Directory, []).

	test(library_dependency_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_dependency_diagram::directory(test, Directory).

	test(library_dependency_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_dependency_diagram::directory(Directory).

	test(library_dependency_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_dependency_diagram::files(test, Files, []).

	test(library_dependency_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_dependency_diagram::files(test, Files).

	test(library_dependency_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_dependency_diagram::files(Files).

	test(library_dependency_diagram_all_files_1_01, deterministic) :-
		library_dependency_diagram::all_files([]).

	test(library_dependency_diagram_all_files_0_01, deterministic) :-
		library_dependency_diagram::all_files.

	% library_load_diagram tests

	test(library_load_diagram_libraries_3_01, deterministic) :-
		library_load_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(library_load_diagram_libraries_2_01, deterministic) :-
		library_load_diagram::libraries(test, [code_metrics,lgtunit]).

	test(library_load_diagram_libraries_1_01, deterministic) :-
		library_load_diagram::libraries([code_metrics,lgtunit]).

	test(library_load_diagram_all_libraries_1_01, deterministic) :-
		library_load_diagram::all_libraries([]).

	test(library_load_diagram_all_libraries_0_01, deterministic) :-
		library_load_diagram::all_libraries.

	test(library_load_diagram_rlibrary_2_01, deterministic) :-
		library_load_diagram::rlibrary(lgtunit, []).

	test(library_load_diagram_rlibrary_1_01, deterministic) :-
		library_load_diagram::rlibrary(lgtunit).

	test(library_load_diagram_library_2_01, deterministic) :-
		library_load_diagram::library(lgtunit, []).

	test(library_load_diagram_library_1_01, deterministic) :-
		library_load_diagram::library(lgtunit).

	test(library_load_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		library_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(library_load_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		library_load_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(library_load_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_load_diagram::directory(test, Directory, []).

	test(library_load_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_load_diagram::directory(test, Directory).

	test(library_load_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		library_load_diagram::directory(Directory).

	test(library_load_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_load_diagram::files(test, Files, []).

	test(library_load_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_load_diagram::files(test, Files).

	test(library_load_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		library_load_diagram::files(Files).

	test(library_load_diagram_all_files_1_01, deterministic) :-
		library_load_diagram::all_files([]).

	test(library_load_diagram_all_files_0_01, deterministic) :-
		library_load_diagram::all_files.

	% uses_diagram tests

	test(uses_diagram_libraries_3_01, deterministic) :-
		uses_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(uses_diagram_libraries_2_01, deterministic) :-
		uses_diagram::libraries(test, [code_metrics,lgtunit]).

	test(uses_diagram_libraries_1_01, deterministic) :-
		uses_diagram::libraries([code_metrics,lgtunit]).

	test(uses_diagram_all_libraries_1_01, deterministic) :-
		uses_diagram::all_libraries([]).

	test(uses_diagram_all_libraries_0_01, deterministic) :-
		uses_diagram::all_libraries.

	test(uses_diagram_rlibrary_2_01, deterministic) :-
		uses_diagram::rlibrary(lgtunit, []).

	test(uses_diagram_rlibrary_1_01, deterministic) :-
		uses_diagram::rlibrary(lgtunit).

	test(uses_diagram_library_2_01, deterministic) :-
		uses_diagram::library(lgtunit, []).

	test(uses_diagram_library_1_01, deterministic) :-
		uses_diagram::library(lgtunit).

	test(uses_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		uses_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(uses_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		uses_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(uses_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		uses_diagram::directory(test, Directory, []).

	test(uses_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		uses_diagram::directory(test, Directory).

	test(uses_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		uses_diagram::directory(Directory).

	test(uses_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		uses_diagram::files(test, Files, []).

	test(uses_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		uses_diagram::files(test, Files).

	test(uses_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		uses_diagram::files(Files).

	test(uses_diagram_all_files_1_01, deterministic) :-
		uses_diagram::all_files([]).

	test(uses_diagram_all_files_0_01, deterministic) :-
		uses_diagram::all_files.

	test(uses_diagram_file_2_01, deterministic) :-
		object_property(uses_diagram, file(File)),
		uses_diagram::file(File, []).

	test(uses_diagram_file_1_01, deterministic) :-
		object_property(uses_diagram, file(File)),
		uses_diagram::file(File).

	% xref_diagram tests

	test(xref_diagram_libraries_3_01, deterministic) :-
		xref_diagram::libraries(test, [code_metrics,lgtunit], []).

	test(xref_diagram_libraries_2_01, deterministic) :-
		xref_diagram::libraries(test, [code_metrics,lgtunit]).

	test(xref_diagram_libraries_1_01, deterministic) :-
		xref_diagram::libraries([code_metrics,lgtunit]).

	test(xref_diagram_all_libraries_1_01, deterministic) :-
		xref_diagram::all_libraries([]).

	test(xref_diagram_all_libraries_0_01, deterministic) :-
		xref_diagram::all_libraries.

	test(xref_diagram_rlibrary_2_01, deterministic) :-
		xref_diagram::rlibrary(lgtunit, []).

	test(xref_diagram_rlibrary_1_01, deterministic) :-
		xref_diagram::rlibrary(lgtunit).

	test(xref_diagram_library_2_01, deterministic) :-
		xref_diagram::library(lgtunit, []).

	test(xref_diagram_library_1_01, deterministic) :-
		xref_diagram::library(lgtunit).

	test(xref_diagram_directories_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		xref_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory], []).

	test(xref_diagram_directories_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, CodeMetricsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		xref_diagram::directories(test, [CodeMetricsDirectory,LgtunitDirectory]).

	test(xref_diagram_directory_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		xref_diagram::directory(test, Directory, []).

	test(xref_diagram_directory_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		xref_diagram::directory(test, Directory).

	test(xref_diagram_directory_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		xref_diagram::directory(Directory).

	test(xref_diagram_files_3_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		xref_diagram::files(test, Files, []).

	test(xref_diagram_files_2_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		xref_diagram::files(test, Files).

	test(xref_diagram_files_1_01, deterministic) :-
		logtalk::expand_library_path(code_metrics, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		xref_diagram::files(Files).

	test(xref_diagram_all_files_1_01, deterministic) :-
		xref_diagram::all_files([]).

	test(xref_diagram_all_files_0_01, deterministic) :-
		xref_diagram::all_files.

	test(xref_diagram_file_2_01, deterministic) :-
		object_property(xref_diagram, file(File)),
		xref_diagram::file(File, []).

	test(xref_diagram_file_1_01, deterministic) :-
		object_property(xref_diagram, file(File)),
		xref_diagram::file(File).

	cleanup :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files, [paths(absolute), extensions(['.dot'])]),
		forall(
			list::member(File, Files),
			os::delete_file(File)
		).

	% suppress all messages from the "diagrams" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, diagrams, _Tokens).

:- end_object.
