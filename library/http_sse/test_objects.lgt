%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(sse_serve_once_handler,
	implements(http_sse_service_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Helper server session handler used by http_sse wrapper tests.'
	]).

	next([], [sent(greeting)], [event(greeting, hello, none)], continue).
	next([sent(greeting)], done, [data(bye)], stop).

:- end_object.


:- object(sse_open_session_handler,
	implements(http_sse_service_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Helper client session handler used by http_sse wrapper tests.'
	]).

	:- private(events_/1).
	:- dynamic(events_/1).

	handle(Event, stop) :-
		retractall(events_(_)),
		assertz(events_(Event)).

	:- public(last_event/1).
	last_event(Event) :-
		events_(Event).

:- end_object.
