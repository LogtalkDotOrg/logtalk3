%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2018-02-13,
		comment is 'Unit tests for the "patches" example.'
	]).

	:- if((
		os::operating_system_type(windows),
		\+ current_logtalk_flag(prolog_dialect, ji),
		\+ current_logtalk_flag(prolog_dialect, sicstus),
		\+ current_logtalk_flag(prolog_dialect, swi)
	)).

	succeeds(patches_1) :-
		^^set_text_output(''),
		proto::init,
		^^check_text_output('parent init\r\nproto init\r\n').

	:- else.

	succeeds(patches_1) :-
		^^set_text_output(''),
		proto::init,
		^^check_text_output('parent init\nproto init\n').

	:- endif.

	succeeds(patches_2) :-
		proto::pet(Pet),
		Pet == cat.

:- end_object.
