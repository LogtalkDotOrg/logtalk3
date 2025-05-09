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
		date is 2024-08-16,
		comment is 'Unit tests for the ``underscore_variables`` linter flag.'
	]).

	:- private(singleton_variables/6).
	:- dynamic(singleton_variables/6).

	cleanup :-
		retractall(singleton_variables(_, _, _, _, _, _)).

	test(underscore_variables_linter_01, false, [setup(cleanup)]) :-
		logtalk_compile(test_entities, [underscore_variables(dont_care), clean(on)]),
		singleton_variables(_, _, object, underscore_variables, _, _).

	test(underscore_variables_linter_02, true(Names == ['_Bar']), [setup(cleanup)]) :-
		logtalk_compile(test_entities, [underscore_variables(singletons), clean(on)]),
		singleton_variables(_, _, object, underscore_variables, Names, _).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(singleton_variables(File, Lines, Type, Entity, Names, Term), warning(singleton_variables), core, _) :-
		assertz(singleton_variables(File, Lines, Type, Entity, Names, Term)).

:- end_object.
