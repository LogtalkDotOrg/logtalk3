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
		version is 1:0:2,
		author is 'Paulo Moura',
		date is 2021-07-26,
		comment is 'Unit tests for the "around_methods" example.'
	]).

	cover(bird).
	cover(patch).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, b),
		\+ current_logtalk_flag(prolog_dialect, gnu),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi),
		\+ current_logtalk_flag(prolog_dialect, xsb)
	)).

	test(around_methods_01, true(Assertion)) :-
		^^set_text_output(''),
		bird::make_sound,
		^^text_output_assertion(
			'Started making sound...\r\nChirp, chirp!\r\n... finished making sound.\r\n',
			Assertion
		).

	:- else.

	test(around_methods_01, true(Assertion)) :-
		^^set_text_output(''),
		bird::make_sound,
		^^text_output_assertion(
			'Started making sound...\nChirp, chirp!\n... finished making sound.\n',
			Assertion
		).

	:- endif.

:- end_object.
