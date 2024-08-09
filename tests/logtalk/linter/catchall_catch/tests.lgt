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
		date is 2024-08-09,
		comment is 'Unit tests for the ``catchall_catch`` linter flag.'
	]).

	:- private(catchall_catch/5).
	:- dynamic(catchall_catch/5).

	setup :-
		retractall(catchall_catch(_, _, _, _, _)),
		logtalk_compile(test_entities, [catchall_catch(warning)]).

	cleanup :-
		retractall(catchall_catch(_, _, _, _, _)).

	test(catchall_catch_linter_flag_01, variant(Term, catch(bar, _, baz))) :-
		catchall_catch(_, _, object, catchall_catch, Term).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(catchall_catch(File, Lines, Type, Entity, Term), warning(catchall_catch), core, _) :-
		assertz(catchall_catch(File, Lines, Type, Entity, Term)).

:- end_object.
