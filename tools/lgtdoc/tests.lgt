%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:6:1,
		author is 'Paulo Moura',
		date is 2021-02-03,
		comment is 'Unit tests for the "lgtdoc" tool.'
	]).

	cover(lgtdoc).

	:- private(xml_docs_directory_/1).
	:- dynamic(xml_docs_directory_/1).

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
		directory_files/3, delete_file/1, delete_directory/1,
		working_directory/1, path_concat/3
	]).

	:- uses(list, [
		member/2
	]).

	setup :-
		retractall(xml_docs_directory_(_)),
		working_directory(Directory),
		path_concat(Directory, 'xml_docs/', XMLDocsDirectory),
		assertz(xml_docs_directory_(XMLDocsDirectory)).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(lgtdoc_all_1_01, deterministic) :-
		all([]).

	test(lgtdoc_all_0_01, deterministic) :-
		all.

	test(lgtdoc_library_2_01, deterministic) :-
		library(lgtunit, []).

	test(lgtdoc_library_1_01, deterministic) :-
		library(lgtunit).

	test(lgtdoc_rlibrary_2_01, deterministic) :-
		rlibrary(lgtunit, []).

	test(lgtdoc_rlibrary_1_01, deterministic) :-
		rlibrary(lgtunit).

	test(lgtdoc_file_2_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File, []).

	test(lgtdoc_file_1_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(lgtdoc_directory_2_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory, []).

	test(lgtdoc_directory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(lgtdoc_rdirectory_2_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, []).

	test(lgtdoc_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	cleanup :-
		xml_docs_directory_(XMLDocsDirectory),
		directory_files(XMLDocsDirectory, XMLFiles, [paths(absolute), extensions(['.xml'])]),
		forall(
			list::member(XMLFile, XMLFiles),
			delete_file(XMLFile)
		),
		delete_directory(XMLDocsDirectory).

	% suppress all messages from the "lgtdoc" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, lgtdoc, _Tokens).

:- end_object.
