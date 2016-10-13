%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 2016 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the "lgtdoc" tool.'
	]).

	cover(lgtdoc).

	:- uses(lgtdoc, [
		all/1, all/0,
		rlibrary/2, rlibrary/1,
		library/2, library/1,
		rdirectory/2, rdirectory/1,
		directory/2, directory/1,
		file/2, file/1
	]).

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

	test(lgtdoc_all_1_01) :-
		deterministic(all([])).

	test(lgtdoc_all_0_01) :-
		deterministic(all).

	test(lgtdoc_library_2_01) :-
		deterministic(library(lgtunit, [])).

	test(lgtdoc_library_1_01) :-
		deterministic(library(lgtunit)).

	test(lgtdoc_rlibrary_2_01) :-
		deterministic(rlibrary(lgtunit, [])).

	test(lgtdoc_rlibrary_1_01) :-
		deterministic(rlibrary(lgtunit)).

	test(lgtdoc_file_2_01) :-
		object_property(lgtunit, file(File)),
		deterministic(file(File, [])).

	test(lgtdoc_file_1_01) :-
		object_property(lgtunit, file(File)),
		deterministic(file(File)).

	test(lgtdoc_directory_2_01) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(directory(Directory, [])).

	test(lgtdoc_directory_1_01) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(directory(Directory)).

	test(lgtdoc_rdirectory_2_01) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(rdirectory(Directory, [])).

	test(lgtdoc_rdirectory_1_01) :-
		logtalk::expand_library_path(lgtunit, Directory),
		deterministic(rdirectory(Directory)).

	cleanup :-
		this(This),
		object_property(This, file(_,Directory)),
		atom_concat(Directory, 'xml_docs/', XMLDocsDirectory),
		os::directory_files(XMLDocsDirectory, XMLFiles),
		forall(
			(	list::member(XMLFile, XMLFiles),
				sub_atom(XMLFile, _, 4, 0, '.xml')
			),
			(	atom_concat(XMLDocsDirectory, XMLFile, XMLFilePath),
				os::delete_file(XMLFilePath)
			)
		),
		os::delete_directory(XMLDocsDirectory).

	% supress all messages from the "lgtdoc" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, lgtdoc, _Tokens).

:- end_object.
