%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		comment is 'Unit tests for the "threads/ping_pong" example.'
	]).

	cover(ping_pong).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		test(ping_pong_01, true) :-
			ping_pong::play(1).

		test(ping_pong_02, true) :-
			ping_pong::play(5).

	:- else.

		test(ping_pong_01, true(Assertion)) :-
			^^set_text_output(''),
			ping_pong::play(1),
			^^text_output_assertion('Ping ...\n.... Pong\nGame over!\n', Assertion).

		test(ping_pong_02, true(Assertion)) :-
			^^set_text_output(''),
			ping_pong::play(5),
			^^text_output_assertion('Ping ...\n.... Pong\nPing ...\n.... Pong\nPing ...\n.... Pong\nPing ...\n.... Pong\nPing ...\n.... Pong\nGame over!\n', Assertion).

	:- endif.

:- end_object.
