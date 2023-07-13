%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		date is 2023-07-13,
		comment is 'Unit tests for the ISO Prolog term syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.3.1

	test(iso_term_01, true) :-
		^^set_text_input('f(x,y). '),
		{read(_)}.

	test(iso_term_02, true) :-
		^^set_text_input('f(:-, ;, [:-, :-|:-]). '),
		{read(_)}.

	test(iso_term_03, error(syntax_error(_))) :-
		^^set_text_input('f(,,a). '),
		{read(_)}.

	test(iso_term_04, error(syntax_error(_))) :-
		^^set_text_input('[a,,|v]. '),
		{read(_)}.

	test(iso_term_05, error(syntax_error(_))) :-
		^^set_text_input('[a,b|,]. '),
		{read(_)}.

	test(iso_term_06, true) :-
		^^set_text_input('f(\',\',a). '),
		{read(_)}.

	test(iso_term_07, true) :-
		^^set_text_input('[a,\',\'|v]. '),
		{read(_)}.

	test(iso_term_08, true) :-
		^^set_text_input('[a,b|\',\']. '),
		{read(_)}.

	% tests from the WG17 standardization work

	test(wg17_term_09, error(syntax_error(_))) :-
		^^set_text_input('f(a:-b). '),
		{read(_)}.

	test(wg17_term_10, error(syntax_error(_))) :-
		^^set_text_input('f(:-b). '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
