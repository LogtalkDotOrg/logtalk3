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
		date is 2020-12-24,
		comment is 'Unit tests for the "msglog" example.'
	]).

	cover(msglog).

	test(msglog_01) :-
		msglog::erase,
		\+ msglog::log(_, _).

	test(msglog_02) :-
		msglog::erase,
		msglog::record,
		% the "msglog" object records messages sent from "user"
		{character::is_alpha(p)},
		msglog::log(Object, Message),
		^^assertion(Object == character),
		^^assertion(Message == is_alpha(p)).

	test(msglog_03) :-
		msglog::erase,
		msglog::record,
		% the "msglog" object records messages sent from "user"
		forall({list::member(_, [1, 2, 3])}, true),
		forall({integer::between(1, 4, _)}, true),
		findall(
			Object::Message,
			msglog::log(Object, Message),
			LogEntries
		),
		LogEntries = [Entry1, Entry2| Tail],
		^^variant(Entry1, list::member(_, [1, 2, 3])),
		^^variant(Entry2, integer::between(1, 4, _)),
		^^assertion(Tail == []).

	test(msglog_04) :-
		msglog::erase,
		msglog::record,
		% the "msglog" object records messages sent from "user"
		{character::is_alpha(p)},
		msglog::stop,
		forall({list::member(_, [1, 2, 3])}, true),
		forall({integer::between(1, 4, _)}, true),
		findall(
			Object::Message,
			msglog::log(Object, Message),
			LogEntries
		),
		LogEntries = [Entry| Tail],
		^^assertion(Entry == character::is_alpha(p)),
		^^assertion(Tail == []).

:- end_object.
