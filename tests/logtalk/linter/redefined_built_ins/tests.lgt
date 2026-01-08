%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the ``redefined_built_ins`` linter flag.'
	]).

	:- private(redefined_logtalk_built_in/5).
	:- dynamic(redefined_logtalk_built_in/5).

	:- private(redefined_prolog_built_in/5).
	:- dynamic(redefined_prolog_built_in/5).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [redefined_built_ins(warning)]).

	cleanup :-
		retractall(redefined_logtalk_built_in(_, _, _, _, _)),
		retractall(redefined_prolog_built_in(_, _, _, _, _)).

	test(redefined_built_ins_linter_flag_01, exists(Term == current_object/1)) :-
		redefined_logtalk_built_in(_, _, object, redefined_built_ins, Term).

	test(redefined_built_ins_linter_flag_02, exists(Term == write/1)) :-
		redefined_prolog_built_in(_, _, object, redefined_built_ins, Term).

	test(redefined_built_ins_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(redefined_logtalk_built_in_predicate(file, 1-2, object, redefined_built_ins, a/1), core), Tokens).

	test(redefined_built_ins_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(redefined_prolog_built_in_predicate(file, 1-2, object, redefined_built_ins, a/1), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(redefined_logtalk_built_in_predicate(File, Lines, Type, Entity, Term), warning(redefined_built_ins), core, _) :-
		assertz(redefined_logtalk_built_in(File, Lines, Type, Entity, Term)).
	logtalk::message_hook(redefined_prolog_built_in_predicate(File, Lines, Type, Entity, Term), warning(redefined_built_ins), core, _) :-
		assertz(redefined_prolog_built_in(File, Lines, Type, Entity, Term)).

:- end_object.
