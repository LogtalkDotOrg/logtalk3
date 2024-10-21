%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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
		version is 0:6:0,
		author is 'Barry Evans and Paulo Moura',
		date is 2024-10-21,
		comment is 'Unit tests for the "dead_code_scanner" tool.'
	]).

	cover(dead_code_scanner).

	:- uses(dead_code_scanner, [
		predicates/2, predicate/2,
		all/0,
		rlibrary/1, library/1,
		rdirectory/2, rdirectory/1, directory/1,
		file/1,
		entity/1
	]).

	% category tests

	test(dcs_stand_alone_category_01, deterministic) :-
		predicates(stand_alone_category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_category_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_01, deterministic) :-
		predicates(category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_02, deterministic) :-
		setof(Predicate, predicate(category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% prototype tests

	test(dcs_stand_alone_prototype_01, deterministic) :-
		predicates(stand_alone_prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_prototype_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_01, deterministic) :-
		predicates(prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_02, deterministic) :-
		setof(Predicate, predicate(prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% class and instance tests

	test(dcs_stand_alone_class_01, deterministic) :-
		predicates(stand_alone_class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_class_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_01, deterministic) :-
		predicates(class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_02, deterministic) :-
		setof(Predicate, predicate(class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_01, deterministic) :-
		predicates(subclass_with_metaclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_02, deterministic) :-
		setof(Predicate, predicate(subclass_with_metaclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_01, deterministic) :-
		predicates(subclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_02, deterministic) :-
		setof(Predicate, predicate(subclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_01, deterministic) :-
		predicates(instance, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_02, deterministic) :-
		setof(Predicate, predicate(instance, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(dcs_predicates_2_01, deterministic) :-
		predicates(lgtunit, _).

	test(dcs_entity_1_01, deterministic) :-
		entity(lgtunit).

	test(dcs_entity_1_02, deterministic) :-
		entity(lgtunit_messages).

	test(dcs_all_0_01, deterministic) :-
		all.

	test(dcs_library_1_01, deterministic) :-
		library(lgtunit).

	test(dcs_rlibrary_1_01, deterministic) :-
		rlibrary(lgtunit).

	test(dcs_file_1_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(dcs_directory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(dcs_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	% tests for non-called predicates listed in uses/2 directives

	test(dcs_uses_directive_01, deterministic) :-
		setof(Predicate, predicate(predicate_directives, Predicate), Predicates),
		^^assertion(Predicates == [list::app/3, list::member/2, logtalk::dbg/1]).

	% tests for option validation

	test(dcs_exclude_directories_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_directories([foo, bar])]).

	test(dcs_exclude_files_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_files([foo, bar])]).

	test(dcs_exclude_libraries_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_libraries([foo, bar])]).

	test(dcs_exclude_entities_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_entities([foo, bar])]).

	% suppress all messages from the "dead_code_scanner"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, dead_code_scanner, _Tokens).

:- end_object.
