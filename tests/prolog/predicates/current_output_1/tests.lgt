%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2020-08-10,
		comment is 'Unit tests for the ISO Prolog standard current_output/1 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	succeeds(sics_current_output_1_01) :-
		{current_output(_S)}.

	throws(sics_current_output_1_02, error(domain_error(stream,foo),_)) :-
		{current_output(foo)}.

	fails(sics_current_output_1_03) :-
		{current_input(S),
		 current_output(S)}.

	throws(sics_current_output_1_04, error(domain_error(stream,S),_)) :-
		^^closed_output_stream(S, []),
		{current_output(S)}.

	succeeds(sics_current_output_1_05) :-
		{current_output(S),
		 current_output(S)}.

	% tests from the Logtalk portability work

	succeeds(sics_current_output_1_06) :-
		{current_output(S)},
		ground(S).

:- end_object.
