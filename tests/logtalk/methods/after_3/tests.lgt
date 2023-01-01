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
	implements(monitoring),
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2017-06-19,
		comment is 'Unit tests for the after/3 built-in method.'
	]).

	:- initialization(init).

	init :-
		this(This),
		define_events(after, logtalk, _, user, This).

	:- private(message/1).
	:- dynamic(message/1).

	after(_, Message, _) :-
		assertz(message(Message)).

	test(after_3_1) :-
		current_event(Event, Object, _, Sender, Monitor),
		this(This),
		Event == after, Object == logtalk, Sender == user, Monitor == This.

	test(after_3_2) :-
		{logtalk::entity_prefix(logtalk, Prefix)},
		message(entity_prefix(logtalk, Prefix)).

:- end_object.
