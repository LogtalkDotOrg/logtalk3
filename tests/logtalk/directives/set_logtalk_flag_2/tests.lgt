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


% source file level set_logtalk_flag/2 directives are local to the file
:- set_logtalk_flag(complements, deny).
:- set_logtalk_flag(context_switching_calls, allow).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-08-18,
		comment is 'Unit tests for the set_logtalk_flag/2 built-in directive.'
	]).

	% entity level set_logtalk_flag/2 directives are local to the entity
	:- set_logtalk_flag(complements, allow).
	:- set_logtalk_flag(context_switching_calls, deny).

	test(set_logtalk_flag_2_directive_01, true) :-
		this(This),
		object_property(This, complements).

	test(set_logtalk_flag_2_directive_02, false) :-
		this(This),
		object_property(This, context_switching_calls).

	test(set_logtalk_flag_2_directive_03, true(X == top)) :-
		bottom::p(X).

	test(set_logtalk_flag_2_directive_04, true(X == patch)) :-
		logtalk_load(patch),
		bottom::p(X).

	% tests for the "optimize" flag

	test(set_logtalk_flag_2_directive_05, true) :-
		optimize_off::p.

	test(set_logtalk_flag_2_directive_06, true) :-
		optimize_on_1::p.

	test(set_logtalk_flag_2_directive_07, true) :-
		optimize_on_2::p.

:- end_object.
