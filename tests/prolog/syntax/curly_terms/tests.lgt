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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2020-09-30,
		comment is 'Unit tests for the ISO Prolog standard curly bracketed term syntax.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 6.3.6.1

	succeeds(iso_curly_bracketed_term_01) :-
		^^set_text_input('{a}. '),
		{read(T)},
		T == '{}'(a).

	succeeds(iso_curly_bracketed_term_02) :-
		^^set_text_input('{a,b}. '),
		{read(T)},
		T == '{}'(','(a,b)).

	% tests from the Logtalk portability work

	succeeds(lgt_curly_bracketed_term_03) :-
		^^set_text_input('{}. '),
		{read(T)},
		T == '{}'.

	succeeds(lgt_curly_bracketed_term_04) :-
		^^set_text_input('{(a,b)}. '),
		{read(T)},
		T == '{}'(','(a,b)).

	succeeds(lgt_curly_bracketed_term_05) :-
		^^set_text_input('{a}. {}(a). '),
		{read(T1), read(T2)},
		T1 == T2.

	succeeds(lgt_curly_bracketed_term_06) :-
		^^set_text_input('{a,b}. {}((a,b)). '),
		{read(T1), read(T2)},
		T1 == T2.

	cleanup :-
		^^clean_text_input.

:- end_object.
