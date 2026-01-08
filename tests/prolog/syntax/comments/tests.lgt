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
		date is 2023-04-07,
		comment is 'Unit tests for the ISO Prolog standard comments syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.1

	test(iso_line_comment_01, true(T == end_of_file)) :-
		^^set_text_input('% abc\n'),
		{read(T)}.

	test(iso_line_comment_02, true(T1-T2 == a-end_of_file)) :-
		^^set_text_input('% abc\na.\n% def\n'),
		{read(T1), read(T2)}.

	test(iso_block_comment_01, true(T == end_of_file)) :-
		^^set_text_input('/* abc\ndef\n*/\n'),
		{read(T)}.

	test(iso_block_comment_02, true(T1-T2 == a-end_of_file)) :-
		^^set_text_input('/* abc */\na.\n/* def */\n'),
		{read(T1), read(T2)}.

	test(iso_block_comment_03, true(T1-T2 == a-end_of_file)) :-
		^^set_text_input('/* /* */\na.\n/* /* */\n'),
		{read(T1), read(T2)}.

	test(iso_block_comment_04, error(syntax_error(_))) :-
		^^set_text_input('/* */ */\n'),
		{read(_)}.

	test(iso_block_comment_05, error(syntax_error(_))) :-
		^^set_text_input('/* \n'),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
