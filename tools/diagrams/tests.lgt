%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/10/13,
		comment is 'Unit tests for the "diagrams" tool.'
	]).

	cover(diagram(_)).
	cover(diagrams(_)).
	cover(entity_diagram(_)).
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

	test(entity_diagram_libraries_3_01) :-
		deterministic(entity_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(entity_diagram_libraries_2_01) :-
		deterministic(entity_diagram::libraries(test, [diagrams,lgtunit])).

	test(entity_diagram_libraries_1_01) :-
		deterministic(entity_diagram::libraries([diagrams,lgtunit])).

	test(entity_diagram_all_libraries_1_01) :-
		deterministic(entity_diagram::all_libraries([])).

	test(entity_diagram_all_libraries_0_01) :-
		deterministic(entity_diagram::all_libraries).

	test(entity_diagram_rlibrary_2_01) :-
		deterministic(entity_diagram::rlibrary(lgtunit, [])).

	test(entity_diagram_rlibrary_1_01) :-
		deterministic(entity_diagram::rlibrary(lgtunit)).

	test(entity_diagram_library_2_01) :-
		deterministic(entity_diagram::library(lgtunit, [])).

	test(entity_diagram_library_1_01) :-
		deterministic(entity_diagram::library(lgtunit)).

	test(entity_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(entity_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(entity_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(entity_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(entity_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(entity_diagram::directory(test, DiagramsDirectory, [])).

	test(entity_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(entity_diagram::directory(test, DiagramsDirectory)).

	test(entity_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(entity_diagram::directory(DiagramsDirectory)).

	test(entity_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(entity_diagram::files(test, Files, [])).

	test(entity_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(entity_diagram::files(test, Files)).

	test(entity_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(entity_diagram::files(Files)).

	test(entity_diagram_all_files_1_01) :-
		deterministic(entity_diagram::all_files([])).

	test(entity_diagram_all_files_0_01) :-
		deterministic(entity_diagram::all_files).

	test(entity_diagram_file_2_01) :-
		object_property(entity_diagram, file(File)),
		deterministic(entity_diagram::file(File, [])).

	test(entity_diagram_file_1_01) :-
		object_property(entity_diagram, file(File)),
		deterministic(entity_diagram::file(File)).

	% inheritance_diagram tests

	test(inheritance_diagram_libraries_3_01) :-
		deterministic(inheritance_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(inheritance_diagram_libraries_2_01) :-
		deterministic(inheritance_diagram::libraries(test, [diagrams,lgtunit])).

	test(inheritance_diagram_libraries_1_01) :-
		deterministic(inheritance_diagram::libraries([diagrams,lgtunit])).

	test(inheritance_diagram_all_libraries_1_01) :-
		deterministic(inheritance_diagram::all_libraries([])).

	test(inheritance_diagram_all_libraries_0_01) :-
		deterministic(inheritance_diagram::all_libraries).

	test(inheritance_diagram_rlibrary_2_01) :-
		deterministic(inheritance_diagram::rlibrary(lgtunit, [])).

	test(inheritance_diagram_rlibrary_1_01) :-
		deterministic(inheritance_diagram::rlibrary(lgtunit)).

	test(inheritance_diagram_library_2_01) :-
		deterministic(inheritance_diagram::library(lgtunit, [])).

	test(inheritance_diagram_library_1_01) :-
		deterministic(inheritance_diagram::library(lgtunit)).

	test(inheritance_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(inheritance_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(inheritance_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(inheritance_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(inheritance_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(inheritance_diagram::directory(test, DiagramsDirectory, [])).

	test(inheritance_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(inheritance_diagram::directory(test, DiagramsDirectory)).

	test(inheritance_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(inheritance_diagram::directory(DiagramsDirectory)).

	test(inheritance_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(inheritance_diagram::files(test, Files, [])).

	test(inheritance_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(inheritance_diagram::files(test, Files)).

	test(inheritance_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(inheritance_diagram::files(Files)).

	test(inheritance_diagram_all_files_1_01) :-
		deterministic(inheritance_diagram::all_files([])).

	test(inheritance_diagram_all_files_0_01) :-
		deterministic(inheritance_diagram::all_files).

	test(inheritance_diagram_file_2_01) :-
		object_property(inheritance_diagram, file(File)),
		deterministic(inheritance_diagram::file(File, [])).

	test(inheritance_diagram_file_1_01) :-
		object_property(inheritance_diagram, file(File)),
		deterministic(inheritance_diagram::file(File)).

	% file_dependency_diagram tests

	test(file_dependency_diagram_libraries_3_01) :-
		deterministic(file_dependency_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(file_dependency_diagram_libraries_2_01) :-
		deterministic(file_dependency_diagram::libraries(test, [diagrams,lgtunit])).

	test(file_dependency_diagram_libraries_1_01) :-
		deterministic(file_dependency_diagram::libraries([diagrams,lgtunit])).

	test(file_dependency_diagram_all_libraries_1_01) :-
		deterministic(file_dependency_diagram::all_libraries([])).

	test(file_dependency_diagram_all_libraries_0_01) :-
		deterministic(file_dependency_diagram::all_libraries).

	test(file_dependency_diagram_rlibrary_2_01) :-
		deterministic(file_dependency_diagram::rlibrary(lgtunit, [])).

	test(file_dependency_diagram_rlibrary_1_01) :-
		deterministic(file_dependency_diagram::rlibrary(lgtunit)).

	test(file_dependency_diagram_library_2_01) :-
		deterministic(file_dependency_diagram::library(lgtunit, [])).

	test(file_dependency_diagram_library_1_01) :-
		deterministic(file_dependency_diagram::library(lgtunit)).

	test(file_dependency_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(file_dependency_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(file_dependency_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(file_dependency_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(file_dependency_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_dependency_diagram::directory(test, DiagramsDirectory, [])).

	test(file_dependency_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_dependency_diagram::directory(test, DiagramsDirectory)).

	test(file_dependency_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_dependency_diagram::directory(DiagramsDirectory)).

	test(file_dependency_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_dependency_diagram::files(test, Files, [])).

	test(file_dependency_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_dependency_diagram::files(test, Files)).

	test(file_dependency_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_dependency_diagram::files(Files)).

	test(file_dependency_diagram_all_files_1_01) :-
		deterministic(file_dependency_diagram::all_files([])).

	test(file_dependency_diagram_all_files_0_01) :-
		deterministic(file_dependency_diagram::all_files).

	% file_load_diagram tests

	test(file_load_diagram_libraries_3_01) :-
		deterministic(file_load_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(file_load_diagram_libraries_2_01) :-
		deterministic(file_load_diagram::libraries(test, [diagrams,lgtunit])).

	test(file_load_diagram_libraries_1_01) :-
		deterministic(file_load_diagram::libraries([diagrams,lgtunit])).

	test(file_load_diagram_all_libraries_1_01) :-
		deterministic(file_load_diagram::all_libraries([])).

	test(file_load_diagram_all_libraries_0_01) :-
		deterministic(file_load_diagram::all_libraries).

	test(file_load_diagram_rlibrary_2_01) :-
		deterministic(file_load_diagram::rlibrary(lgtunit, [])).

	test(file_load_diagram_rlibrary_1_01) :-
		deterministic(file_load_diagram::rlibrary(lgtunit)).

	test(file_load_diagram_library_2_01) :-
		deterministic(file_load_diagram::library(lgtunit, [])).

	test(file_load_diagram_library_1_01) :-
		deterministic(file_load_diagram::library(lgtunit)).

	test(file_load_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(file_load_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(file_load_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(file_load_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(file_load_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_load_diagram::directory(test, DiagramsDirectory, [])).

	test(file_load_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_load_diagram::directory(test, DiagramsDirectory)).

	test(file_load_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(file_load_diagram::directory(DiagramsDirectory)).

	test(file_load_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_load_diagram::files(test, Files, [])).

	test(file_load_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_load_diagram::files(test, Files)).

	test(file_load_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(file_load_diagram::files(Files)).

	test(file_load_diagram_all_files_1_01) :-
		deterministic(file_load_diagram::all_files([])).

	test(file_load_diagram_all_files_0_01) :-
		deterministic(file_load_diagram::all_files).

	% library_dependency_diagram tests

	test(library_dependency_diagram_libraries_3_01) :-
		deterministic(library_dependency_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(library_dependency_diagram_libraries_2_01) :-
		deterministic(library_dependency_diagram::libraries(test, [diagrams,lgtunit])).

	test(library_dependency_diagram_libraries_1_01) :-
		deterministic(library_dependency_diagram::libraries([diagrams,lgtunit])).

	test(library_dependency_diagram_all_libraries_1_01) :-
		deterministic(library_dependency_diagram::all_libraries([])).

	test(library_dependency_diagram_all_libraries_0_01) :-
		deterministic(library_dependency_diagram::all_libraries).

	test(library_dependency_diagram_rlibrary_2_01) :-
		deterministic(library_dependency_diagram::rlibrary(lgtunit, [])).

	test(library_dependency_diagram_rlibrary_1_01) :-
		deterministic(library_dependency_diagram::rlibrary(lgtunit)).

	test(library_dependency_diagram_library_2_01) :-
		deterministic(library_dependency_diagram::library(lgtunit, [])).

	test(library_dependency_diagram_library_1_01) :-
		deterministic(library_dependency_diagram::library(lgtunit)).

	test(library_dependency_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(library_dependency_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(library_dependency_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(library_dependency_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(library_dependency_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_dependency_diagram::directory(test, DiagramsDirectory, [])).

	test(library_dependency_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_dependency_diagram::directory(test, DiagramsDirectory)).

	test(library_dependency_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_dependency_diagram::directory(DiagramsDirectory)).

	test(library_dependency_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_dependency_diagram::files(test, Files, [])).

	test(library_dependency_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_dependency_diagram::files(test, Files)).

	test(library_dependency_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_dependency_diagram::files(Files)).

	test(library_dependency_diagram_all_files_1_01) :-
		deterministic(library_dependency_diagram::all_files([])).

	test(library_dependency_diagram_all_files_0_01) :-
		deterministic(library_dependency_diagram::all_files).

	% library_load_diagram tests

	test(library_load_diagram_libraries_3_01) :-
		deterministic(library_load_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(library_load_diagram_libraries_2_01) :-
		deterministic(library_load_diagram::libraries(test, [diagrams,lgtunit])).

	test(library_load_diagram_libraries_1_01) :-
		deterministic(library_load_diagram::libraries([diagrams,lgtunit])).

	test(library_load_diagram_all_libraries_1_01) :-
		deterministic(library_load_diagram::all_libraries([])).

	test(library_load_diagram_all_libraries_0_01) :-
		deterministic(library_load_diagram::all_libraries).

	test(library_load_diagram_rlibrary_2_01) :-
		deterministic(library_load_diagram::rlibrary(lgtunit, [])).

	test(library_load_diagram_rlibrary_1_01) :-
		deterministic(library_load_diagram::rlibrary(lgtunit)).

	test(library_load_diagram_library_2_01) :-
		deterministic(library_load_diagram::library(lgtunit, [])).

	test(library_load_diagram_library_1_01) :-
		deterministic(library_load_diagram::library(lgtunit)).

	test(library_load_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(library_load_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(library_load_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(library_load_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(library_load_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_load_diagram::directory(test, DiagramsDirectory, [])).

	test(library_load_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_load_diagram::directory(test, DiagramsDirectory)).

	test(library_load_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(library_load_diagram::directory(DiagramsDirectory)).

	test(library_load_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_load_diagram::files(test, Files, [])).

	test(library_load_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_load_diagram::files(test, Files)).

	test(library_load_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(library_load_diagram::files(Files)).

	test(library_load_diagram_all_files_1_01) :-
		deterministic(library_load_diagram::all_files([])).

	test(library_load_diagram_all_files_0_01) :-
		deterministic(library_load_diagram::all_files).

	% uses_diagram tests

	test(uses_diagram_libraries_3_01) :-
		deterministic(uses_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(uses_diagram_libraries_2_01) :-
		deterministic(uses_diagram::libraries(test, [diagrams,lgtunit])).

	test(uses_diagram_libraries_1_01) :-
		deterministic(uses_diagram::libraries([diagrams,lgtunit])).

	test(uses_diagram_all_libraries_1_01) :-
		deterministic(uses_diagram::all_libraries([])).

	test(uses_diagram_all_libraries_0_01) :-
		deterministic(uses_diagram::all_libraries).

	test(uses_diagram_rlibrary_2_01) :-
		deterministic(uses_diagram::rlibrary(lgtunit, [])).

	test(uses_diagram_rlibrary_1_01) :-
		deterministic(uses_diagram::rlibrary(lgtunit)).

	test(uses_diagram_library_2_01) :-
		deterministic(uses_diagram::library(lgtunit, [])).

	test(uses_diagram_library_1_01) :-
		deterministic(uses_diagram::library(lgtunit)).

	test(uses_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(uses_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(uses_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(uses_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(uses_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(uses_diagram::directory(test, DiagramsDirectory, [])).

	test(uses_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(uses_diagram::directory(test, DiagramsDirectory)).

	test(uses_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(uses_diagram::directory(DiagramsDirectory)).

	test(uses_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(uses_diagram::files(test, Files, [])).

	test(uses_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(uses_diagram::files(test, Files)).

	test(uses_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(uses_diagram::files(Files)).

	test(uses_diagram_all_files_1_01) :-
		deterministic(uses_diagram::all_files([])).

	test(uses_diagram_all_files_0_01) :-
		deterministic(uses_diagram::all_files).

	test(uses_diagram_file_2_01) :-
		object_property(uses_diagram, file(File)),
		deterministic(uses_diagram::file(File, [])).

	test(uses_diagram_file_1_01) :-
		object_property(uses_diagram, file(File)),
		deterministic(uses_diagram::file(File)).

	% xref_diagram tests

	test(xref_diagram_libraries_3_01) :-
		deterministic(xref_diagram::libraries(test, [diagrams,lgtunit], [])).

	test(xref_diagram_libraries_2_01) :-
		deterministic(xref_diagram::libraries(test, [diagrams,lgtunit])).

	test(xref_diagram_libraries_1_01) :-
		deterministic(xref_diagram::libraries([diagrams,lgtunit])).

	test(xref_diagram_all_libraries_1_01) :-
		deterministic(xref_diagram::all_libraries([])).

	test(xref_diagram_all_libraries_0_01) :-
		deterministic(xref_diagram::all_libraries).

	test(xref_diagram_rlibrary_2_01) :-
		deterministic(xref_diagram::rlibrary(lgtunit, [])).

	test(xref_diagram_rlibrary_1_01) :-
		deterministic(xref_diagram::rlibrary(lgtunit)).

	test(xref_diagram_library_2_01) :-
		deterministic(xref_diagram::library(lgtunit, [])).

	test(xref_diagram_library_1_01) :-
		deterministic(xref_diagram::library(lgtunit)).

	test(xref_diagram_directories_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(xref_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory], [])).

	test(xref_diagram_directories_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		logtalk::expand_library_path(lgtunit, LgtunitDirectory),
		deterministic(xref_diagram::directories(test, [DiagramsDirectory,LgtunitDirectory])).

	test(xref_diagram_directory_3_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(xref_diagram::directory(test, DiagramsDirectory, [])).

	test(xref_diagram_directory_2_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(xref_diagram::directory(test, DiagramsDirectory)).

	test(xref_diagram_directory_1_01) :-
		logtalk::expand_library_path(diagrams, DiagramsDirectory),
		deterministic(xref_diagram::directory(DiagramsDirectory)).

	test(xref_diagram_files_3_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(xref_diagram::files(test, Files, [])).

	test(xref_diagram_files_2_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(xref_diagram::files(test, Files)).

	test(xref_diagram_files_1_01) :-
		logtalk::expand_library_path(diagrams, Directory),
		os::directory_files(Directory, Files0),
		findall(File, (list::member(File,Files0), sub_atom(File,_,12,0,'_diagram.lgt')), Files),
		deterministic(xref_diagram::files(Files)).

	test(xref_diagram_all_files_1_01) :-
		deterministic(xref_diagram::all_files([])).

	test(xref_diagram_all_files_0_01) :-
		deterministic(xref_diagram::all_files).

	test(xref_diagram_file_2_01) :-
		object_property(xref_diagram, file(File)),
		deterministic(xref_diagram::file(File, [])).

	test(xref_diagram_file_1_01) :-
		object_property(xref_diagram, file(File)),
		deterministic(xref_diagram::file(File)).

	cleanup :-
		this(This),
		object_property(This, file(_,Directory)),
		os::directory_files(Directory, Files),
		forall(
			(	list::member(File, Files),
				sub_atom(File, _, 4, 0, '.dot')
			),
			(	atom_concat(Directory, File, FilePath),
				os::delete_file(FilePath)
			)
		).

	% supress all messages from the "lgtdoc" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, diagrams, _Tokens).

:- end_object.
