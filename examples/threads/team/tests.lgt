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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2024-02-05,
		comment is 'Unit tests for the "threads/team" example.'
	]).

	cover(team).

	:- if((current_logtalk_flag(prolog_dialect, lvm);current_logtalk_flag(prolog_dialect, arriba))).

		test(team_01, true) :-
			team::start.

	:- else.

		test(team_01, true(Assertion)) :-
			^^set_text_output(''),
			team::start,
			^^text_output_assertion('a(0)\na(1)\na(2)\na(3)\na(4)\na(5)\na(6)\na(7)\na(8)\na(9)\nNumber of lines: 10\n', Assertion).

	:- endif.

:- end_object.
