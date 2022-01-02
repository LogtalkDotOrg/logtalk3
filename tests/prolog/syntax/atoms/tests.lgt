%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2022-01-02,
		comment is 'Unit tests for the ISO Prolog standard atom syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.1.3

	test(iso_atom_01, true(atom(T))) :-
		^^set_text_input('a. '),
		{read(T)}.

	test(iso_atom_02, true(atom(T))) :-
		^^set_text_input('\'A\'. '),
		{read(T)}.

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.2.1

	test(iso_atom_03, true(atom(T))) :-
		^^set_text_input('\'a''''b\'. '),
		{read(T)}.

	test(iso_atom_04, true(atom(T))) :-
		^^set_text_input('\'a\\\'b\'. '),
		{read(T)}.

	test(iso_atom_05, true(atom(T))) :-
		^^set_text_input('\'a\\\"b\'. '),
		{read(T)}.

	test(iso_atom_06, true(atom(T))) :-
		^^set_text_input('\'a\\\\b\'. '),
		{read(T)}.

	test(iso_atom_07, true(T == 'aSb')) :-
		^^set_text_input('\'a\\123\\b\'. '),
		{read(T)}.

	test(iso_atom_08, true(T == 'aSb')) :-
		^^set_text_input('\'a\\x53\\b\'. '),
		{read(T)}.

	% invalid atoms

	test(lgt_atom_09, error(syntax_error(_))) :-
		^^set_text_input('\'a\\128\\b\'. '),
		{read(_)}.

	test(lgt_atom_10, error(syntax_error(_))) :-
		^^set_text_input('\'a\\x5G\\b\'. '),
		{read(_)}.

	test(lgt_atom_11, error(syntax_error(_))) :-
		^^set_text_input('\'a\\X5G\\b\'. '),
		{read(_)}.

	test(lgt_atom_12, error(syntax_error(_))) :-
		^^set_text_input('\'a\\ b\'. '),
		{read(_)}.

	test(lgt_atom_13, error(syntax_error(_))) :-
		^^set_text_input('\'a\nb\'. '),
		{read(_)}.

	test(lgt_atom_14, error(syntax_error(_))) :-
		^^set_text_input('\'a\tb\'. '),
		{read(_)}.

	% single quote representation in quoted atoms

	test(lgt_atom_15, true(T == '\'')) :-
		^^set_text_input('\'\'\'\'. '),
		{read(T)}.

	test(lgt_atom_16, true(T == '''')) :-
		^^set_text_input('\'\\\'\'. '),
		{read(T)}.

	% atoms with continuation characters

	test(lgt_atom_17, true(T == 'enchanted evening')) :-
		^^set_text_input(['\'', enchanted, '\\', '\n', ' evening\'. ']),
		{read(T)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
