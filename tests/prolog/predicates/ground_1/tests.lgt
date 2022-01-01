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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-12-09,
		comment is 'Unit tests for the ISO Prolog standard ground/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1
	]).

	% tests from the ISO/IEC 13211-1:1995/Cor.2:2012(en) standard, section 8.3.10.4

	succeeds(iso_ground_1_01) :-
		{ground(3)}.

	fails(iso_ground_1_02) :-
		{ground(a(1, _))}.

	% tests from the ECLiPSe test suite

	succeeds(eclipse_ground_1_03) :-
		{ground(a)}.

	succeeds(eclipse_ground_1_04) :-
		{ground(f(3))}.

	fails(eclipse_ground_1_05) :-
		{ground(_)}.

	fails(eclipse_ground_1_06) :-
		{ground(f(_))}.

	% tests from the Logtalk portability work

	succeeds(lgt_ground_1_07) :-
		{ground('$VAR'(0))}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		succeeds(lgt_ground_1_08) :-
			X = f(X),
			{ground(X)}.

	:- else.

		- succeeds(lgt_ground_1_08) :-
			% STO; Undefined.
			X = f(X),
			{ground(X)}.

	:- endif.

:- end_object.
