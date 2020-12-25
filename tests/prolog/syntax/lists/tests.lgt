%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-07-30,
		comment is 'Unit tests for the ISO Prolog standard list syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.5.1

	test(iso_list_01, true(T == .(a, []))) :-
		^^set_text_input('[a]. '),
		{read(T)}.

	test(iso_list_02, true(T == .(a, .(b, [])))) :-
		^^set_text_input('[a,b]. '),
		{read(T)}.

	test(iso_list_03, true(T == .(a, b))) :-
		^^set_text_input('[a|b]. '),
		{read(T)}.

	% tests from the Logtalk portability work

	test(lgt_list_04, true(T == [])) :-
		^^set_text_input('[]. '),
		{read(T)}.

	test(lgt_list_05, true(T == '[]')) :-
		^^set_text_input('[]. '),
		{read(T)}.

	test(lgt_list_06, error(syntax_error(_))) :-
		^^set_text_input('[|]. '),
		{read(_)}.

	test(lgt_list_07, error(syntax_error(_))) :-
		^^set_text_input('[1|]. '),
		{read(_)}.

	test(lgt_list_08, error(syntax_error(_))) :-
		^^set_text_input('[|1]. '),
		{read(_)}.

	test(lgt_list_09, error(syntax_error(_))) :-
		^^set_text_input('[1,2,3,]. '),
		{read(_)}.

	test(lgt_list_10, error(syntax_error(_))) :-
		^^set_text_input('[1|2|3]. '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
