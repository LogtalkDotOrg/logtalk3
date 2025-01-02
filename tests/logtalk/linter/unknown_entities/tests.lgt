%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-13,
		comment is 'Unit tests for the ``unknown_entities`` linter flag.'
	]).

	:- private(unknown_object/5).
	:- dynamic(unknown_object/5).

	:- private(unknown_protocol/5).
	:- dynamic(unknown_protocol/5).

	:- private(unknown_category/5).
	:- dynamic(unknown_category/5).

	:- private(unknown_module/5).
	:- dynamic(unknown_module/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [unknown_entities(warning)]).

	cleanup :-
		retractall(unknown_object(_, _, _, _, _)),
		retractall(unknown_protocol(_, _, _, _, _)),
		retractall(unknown_category(_, _, _, _, _)),
		retractall(unknown_module(_, _, _, _, _)).

	test(unknown_entities_linter_flag_01, exists(Entity == some_object)) :-
		unknown_object(_, _, object, unknown_entities, Entity).

	test(unknown_entities_linter_flag_02, exists(Entity == unknown_object)) :-
		unknown_object(_, _, object, unknown_entities, Entity).

	test(unknown_entities_linter_flag_03, exists(Entity == other_object)) :-
		unknown_object(_, _, object, unknown_entities, Entity).

	test(unknown_entities_linter_flag_04, exists(Entity == some_protocol)) :-
		unknown_protocol(_, _, object, unknown_entities, Entity).

	test(unknown_entities_linter_flag_05, exists(Entity == some_category)) :-
		unknown_category(_, _, object, unknown_entities, Entity).

	test(unknown_entities_linter_flag_06, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(reference_to_unknown_object(file, 1-2, object, unknown_entities, foo), core), Tokens).

	test(unknown_entities_linter_flag_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(reference_to_unknown_protocol(file, 1-2, object, unknown_entities, foo), core), Tokens).

	test(unknown_entities_linter_flag_08, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(reference_to_unknown_category(file, 1-2, object, unknown_entities, foo), core), Tokens).

	:- if(current_logtalk_flag(modules, supported)).

		test(unknown_entities_linter_flag_09, exists(Entity == unknown_module)) :-
			unknown_module(_, _, object, unknown_entities, Entity).

		test(unknown_entities_linter_flag_10, exists(Entity == some_module)) :-
			unknown_module(_, _, object, unknown_entities, Entity).

		test(unknown_entities_linter_flag_11, true(type::valid(ground(list), Tokens))) :-
			phrase(logtalk::message_tokens(reference_to_unknown_module(file, 1-2, object, unknown_entities, foo), core), Tokens).

	:- endif.

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(reference_to_unknown_object(File, Lines, Type, Entity, Object), warning(unknown_entities), core, _) :-
		assertz(unknown_object(File, Lines, Type, Entity, Object)).
	logtalk::message_hook(reference_to_unknown_protocol(File, Lines, Type, Entity, Protocol), warning(unknown_entities), core, _) :-
		assertz(unknown_protocol(File, Lines, Type, Entity, Protocol)).
	logtalk::message_hook(reference_to_unknown_category(File, Lines, Type, Entity, Category), warning(unknown_entities), core, _) :-
		assertz(unknown_category(File, Lines, Type, Entity, Category)).
	logtalk::message_hook(reference_to_unknown_module(File, Lines, Type, Entity, Module), warning(unknown_entities), core, _) :-
		assertz(unknown_module(File, Lines, Type, Entity, Module)).

:- end_object.
