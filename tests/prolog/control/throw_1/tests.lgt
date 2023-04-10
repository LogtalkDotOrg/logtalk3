%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the ISO Prolog standard throw/1 control construct.'
	]).

	% tests from the Logtalk portability work

	test(lgt_throw_1_01, error(instantiation_error)) :-
		{throw(_)}.

	test(lgt_throw_1_02, ball(my_error)) :-
		{throw(my_error)}.

	:- if((
		current_logtalk_flag(coinduction, supported),
		\+ current_logtalk_flag(prolog_dialect, cx),
		\+ current_logtalk_flag(prolog_dialect, eclipse)
	)).

		test(lgt_throw_1_03, ball(f(_))) :-
			X = f(X),
			{throw(X)}.

	:- else.

		- test(lgt_throw_1_03, ball(f(_)), [note('STO')]) :-
			% STO; Undefined.
			X = f(X),
			{throw(X)}.

	:- endif.

	% tests from the ECLiPSe test suite

	test(eclipse_throw_1_04, ball(a)) :-
		{throw(a)}.

	test(eclipse_throw_1_05, ball(1)) :-
		{throw(1)}.

	test(eclipse_throw_1_06, ball(1.0)) :-
		{throw(1.0)}.

	test(eclipse_throw_1_07, ball(f(a))) :-
		{throw(f(a))}.

	test(eclipse_throw_1_08, variant(T, f(_))) :-
		{catch(throw(f(_)), T, true)}.

:- end_object.
