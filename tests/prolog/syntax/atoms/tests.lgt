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
		date is 2018-06-28,
		comment is 'Unit tests for the ISO Prolog standard atom syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.1.3

	succeeds(iso_atom_01) :-
		^^set_text_input('a. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_02) :-
		^^set_text_input('\'A\'. '),
		{read(T)},
		atom(T).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.4.2.1

	succeeds(iso_atom_03) :-
		^^set_text_input('\'a''''b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_04) :-
		^^set_text_input('\'a\\\'b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_05) :-
		^^set_text_input('\'a\\\"b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_06) :-
		^^set_text_input('\'a\\\\b\'. '),
		{read(T)},
		atom(T).

	succeeds(iso_atom_07) :-
		^^set_text_input('\'a\\123\\b\'. '),
		{read(T)},
		T == 'aSb'.

	succeeds(iso_atom_08) :-
		^^set_text_input('\'a\\x53\\b\'. '),
		{read(T)},
		T == 'aSb'.

	% invalid atoms

	throws(lgt_atom_09, error(syntax_error(_),_)) :-
		^^set_text_input('\'a\\128\\b\'. '),
		{read(_)}.

	throws(lgt_atom_10, error(syntax_error(_),_)) :-
		^^set_text_input('\'a\\x5G\\b\'. '),
		{read(_)}.

	throws(lgt_atom_11, error(syntax_error(_),_)) :-
		^^set_text_input('\'a\\X5G\\b\'. '),
		{read(_)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
