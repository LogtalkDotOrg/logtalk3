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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2020-09-30,
		comment is 'Unit tests for the ISO Prolog standard number/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.8.4

	succeeds(iso_number_1_01) :-
		{number(3)}.

	succeeds(iso_number_1_02) :-
		{number(3.3)}.

	succeeds(iso_number_1_03) :-
		{number(-3)}.

	fails(iso_number_1_04) :-
		{number(a)}.

	fails(iso_number_1_05) :-
		{number(_X)}.

	% tests from the Logtalk portability work

	fails(iso_number_1_06) :-
		{number(-(1))}.

	fails(iso_number_1_07) :-
		{number(+(1))}.

	succeeds(lgt_number_1_08) :-
		^^set_text_input('-1. '),
		current_input(S),
		read_term(S, T, []),
		{number(T)}.

	fails(lgt_number_1_09) :-
		^^set_text_input('+1. '),
		current_input(S),
		read_term(S, T, []),
		{number(T)}.

:- end_object.
