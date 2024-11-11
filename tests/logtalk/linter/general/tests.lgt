%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-11-11,
		comment is 'Unit tests for general linter warnings.'
	]).

	:- private(missing_reference_to_built_in_protocol/5).
	:- dynamic(missing_reference_to_built_in_protocol/5).

	:- private(complementing_category_ignored/4).
	:- dynamic(complementing_category_ignored/4).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [general(warning)]).

	cleanup :-
		retractall(missing_reference_to_built_in_protocol(_, _, _, _, _)),
		retractall(complementing_category_ignored(_, _, _, _)).

	test(general_linter_warnings_01, exists(Protocol == expanding)) :-
		missing_reference_to_built_in_protocol(_, _, object, general, Protocol).

	test(general_linter_warnings_03, exists(Category-Object == cat-general)) :-
		complementing_category_ignored(_, _, Category, Object).

	test(general_linter_warnings_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(missing_reference_to_built_in_protocol(file, 1-2, object, general, expanding), core), Tokens).

	test(general_linter_warnings_07, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(complementing_category_ignored(file, 1-2, cat, obj), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(missing_reference_to_built_in_protocol(File, Lines, Type, Entity, Protocol), warning(general), core, _) :-
		assertz(missing_reference_to_built_in_protocol(File, Lines, Type, Entity, Protocol)).
	logtalk::message_hook(complementing_category_ignored(File, Lines, Category, Object), warning(general), core, _) :-
		assertz(complementing_category_ignored(File, Lines, Category, Object)).

:- end_object.
