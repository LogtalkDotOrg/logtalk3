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


% first, we define a simple metaclass with a generic new/3 predicate
% for creating a twin instance with a given state (expressed as a list
% of compound terms)

:- object(metaclass,
	instantiates(metaclass)).

	:- public(new/3).
	new(Object, Twin, State) :-
		self(Self),
		create_object(
			Object,
			[implements(forwarding),instantiates(Self)],
			[],
			[(forward(Message):-[Twin::Message])| State]
		).

:- end_object.


% second, we define two simples classes, one representing time data
% and the other representing date data

:- object(time,
	instantiates(metaclass)).

	:- public([
		hour/1, minutes/1, seconds/1
	]).

:- end_object.


:- object(date,
	instantiates(metaclass)).

	:- public([
		year/1, month/1, day/1
	]).

:- end_object.


% finally, we define two static instances for illustrating the
% twin design pattern

:- object(a_time,
	implements(forwarding),
	instantiates(time)).

	hour(11).
	minutes(27).
	seconds(48).

	% forward other messages to the twin object
	forward(Message) :-
		[a_date::Message].

:- end_object.


:- object(a_date,
	implements(forwarding),
	instantiates(date)).

	year(2018).
	month(11).
	day(13).

	% forward other messages to the twin object
	forward(Message) :-
		[a_time::Message].

:- end_object.
