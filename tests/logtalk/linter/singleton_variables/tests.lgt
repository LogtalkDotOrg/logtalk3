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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-08-12,
		comment is 'Unit tests for the ``singleton_variables`` linter flag.'
	]).

	:- uses(lgtunit, [
		variant/2
	]).

	:- private(singleton_variables/4).
	:- dynamic(singleton_variables/4).

	:- private(singleton_variables/6).
	:- dynamic(singleton_variables/6).

	setup :-
		cleanup,
		logtalk_compile(test_entities, [singleton_variables(warning)]).

	cleanup :-
		retractall(singleton_variables(_, _, _, _)),
		retractall(singleton_variables(_, _, _, _, _, _)).

	test(singleton_variables_linter_flag_01, exists(variant(Names-Term, ['T']-(:-initialization(write(_)))))) :-
		singleton_variables(_, _, Names, Term).

	test(singleton_variables_linter_flag_02, exists(variant(Names-Term, ['L']-(:-object(singletons(_)))))) :-
		singleton_variables(_, _, Names, Term).

	test(singleton_variables_linter_flag_03, exists(variant(Names-Term, ['W']-(:-initialization(write(_)))))) :-
		singleton_variables(_, _, object, singletons(_), Names, Term).

	test(singleton_variables_linter_flag_04, exists(variant(Names-Term, ['A','C']-(predicate(_):-write(_))))) :-
		singleton_variables(_, _, object, singletons(_), Names, Term).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(singleton_variables(File, Lines, Names, Term), warning(singleton_variables), core, _) :-
		assertz(singleton_variables(File, Lines, Names, Term)).
	logtalk::message_hook(singleton_variables(File, Lines, Type, Entity, Names, Term), warning(singleton_variables), core, _) :-
		assertz(singleton_variables(File, Lines, Type, Entity, Names, Term)).

:- end_object.
