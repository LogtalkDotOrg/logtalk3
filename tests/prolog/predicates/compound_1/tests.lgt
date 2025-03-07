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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-09-30,
		comment is 'Unit tests for the ISO Prolog standard compound/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.6.4

	fails(iso_compound_1_01) :-
		{compound(33.3)}.

	fails(iso_compound_1_02) :-
		{compound(-33.3)}.

	succeeds(iso_compound_1_03) :-
		{compound(-a)}.

	fails(iso_compound_1_04) :-
		{compound(_)}.

	fails(iso_compound_1_05) :-
		{compound(a)}.

	succeeds(iso_compound_1_06) :-
		{compound(a(b))}.

	fails(iso_compound_1_07) :-
		{compound([])}.

	succeeds(iso_compound_1_08) :-
		{compound([a])}.

	% tests from the Logtalk portability work

	succeeds(lgt_compound_1_09) :-
		{compound(-(1))}.

	succeeds(lgt_compound_1_10) :-
		{compound(+(1))}.

	fails(lgt_compound_1_11) :-
		^^set_text_input('-1. '),
		current_input(S),
		read_term(S, T, []),
		{compound(T)}.

	succeeds(lgt_compound_1_12) :-
		^^set_text_input('+1. '),
		current_input(S),
		read_term(S, T, []),
		{compound(T)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
