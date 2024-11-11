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
		date is 2024-11-11,
		comment is 'Unit tests for the ``encodings`` linter flag.'
	]).

	:- private(misplaced_encoding_directive/2).
	:- dynamic(misplaced_encoding_directive/2).

	:- private(ignored_encoding_directive/2).
	:- dynamic(ignored_encoding_directive/2).

	setup :-
		cleanup,
		logtalk_compile(file, [encodings(warning)]).

	cleanup :-
		retractall(misplaced_encoding_directive(_, _)),
		retractall(ignored_encoding_directive(_, _)).

	test(encodings_linter_flag_01, true) :-
		misplaced_encoding_directive(_, _).

	test(encodings_linter_flag_02, true) :-
		ignored_encoding_directive(_, _).

	test(encodings_linter_flag_03, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(misplaced_encoding_directive(file, 1-2), core), Tokens).

	test(encodings_linter_flag_04, true(type::valid(ground(list), Tokens))) :-
		phrase(logtalk::message_tokens(ignored_encoding_directive(file, 1-2), core), Tokens).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).
	logtalk::message_hook(misplaced_encoding_directive(File, Lines), warning(encodings), core, _) :-
		assertz(misplaced_encoding_directive(File, Lines)).
	logtalk::message_hook(ignored_encoding_directive(File, Lines), warning(encodings), core, _) :-
		assertz(ignored_encoding_directive(File, Lines)).

:- end_object.
