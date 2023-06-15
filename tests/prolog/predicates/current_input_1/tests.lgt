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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2021-09-25,
		comment is 'Unit tests for the ISO Prolog standard current_input/1 built-in predicate.'
	]).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(sics_current_input_1_01, true) :-
		{current_input(_S)}.

	test(sics_current_input_1_02, error(domain_error(stream,foo))) :-
		{current_input(foo)}.

	test(sics_current_input_1_03, false) :-
		{current_output(S),
		 current_input(S)}.

	test(sics_current_input_1_04, error(domain_error(stream,S))) :-
		^^closed_input_stream(S, []),
		{current_input(S)}.

	test(sics_current_input_1_05, true) :-
		{	current_input(S),
			current_input(S)
		}.

	% tests from the Logtalk portability work

	test(lgt_current_input_1_06, true) :-
		{current_input(S)},
		ground(S).

:- end_object.
