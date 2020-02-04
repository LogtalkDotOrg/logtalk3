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
		date is 2018-02-10,
		comment is 'Unit tests for the ISO Prolog standard integer/1 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.3.3.4

	succeeds(iso_integer_1_01) :-
		{integer(3)}.

	succeeds(iso_integer_1_02) :-
		{integer(-3)}.

	fails(iso_integer_1_03) :-
		{integer(3.3)}.

	fails(iso_integer_1_04) :-
		{integer(_X)}.

	fails(iso_integer_1_05) :-
		{integer(atom)}.

	% tests from the Logtalk portability work

	fails(lgt_integer_1_06) :-
		{integer(1.0)}.

:- end_object.
