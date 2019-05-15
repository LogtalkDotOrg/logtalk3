%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/01/21,
		comment is 'Unit tests for the "books" example.'
	]).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi)
	)).

		test(optionals_1, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_extra,
			^^text_output_assertion('The Philosopher''s Stone (with extra quidditch_set)\r\nThe Chamber of Secrets (with extra map)\r\nThe Half-Blood Prince (with extra audio_cd)\r\nThe Deathly Hallows (with extra horcrux_set)\r\n', Assertion).

	:- else.

		test(optionals_1, true(Assertion)) :-
			^^set_text_output(''),
			data_processing::print_extra,
			^^text_output_assertion('The Philosopher''s Stone (with extra quidditch_set)\nThe Chamber of Secrets (with extra map)\nThe Half-Blood Prince (with extra audio_cd)\nThe Deathly Hallows (with extra horcrux_set)\n', Assertion).

	:- endif.

:- end_object.
