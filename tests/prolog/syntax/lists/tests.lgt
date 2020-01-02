%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2018/06/28,
		comment is 'Unit tests for the ISO Prolog standard list syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.5.1

	succeeds(iso_list_01) :-
		^^set_text_input('[a]. '),
		{read(T)},
		T == .(a, []).

	succeeds(iso_list_02) :-
		^^set_text_input('[a,b]. '),
		{read(T)},
		T == .(a, .(b, [])).

	succeeds(iso_list_03) :-
		^^set_text_input('[a|b]. '),
		{read(T)},
		T == .(a, b).

	% invalid lists

	throws(lgt_list_04, error(syntax_error(_), _)) :-
		^^set_text_input('[|]. '),
		{read(_)}.

	throws(lgt_list_05, error(syntax_error(_), _)) :-
		^^set_text_input('[1|]. '),
		{read(_)}.

	throws(lgt_list_06, error(syntax_error(_), _)) :-
		^^set_text_input('[|1]. '),
		{read(_)}.

	throws(lgt_list_07, error(syntax_error(_), _)) :-
		^^set_text_input('[1,2,3,]. '),
		{read(_)}.

	throws(lgt_list_08, error(syntax_error(_), _)) :-
		^^set_text_input('[1|2|3]. '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
