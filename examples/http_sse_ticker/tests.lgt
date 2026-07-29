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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Unit tests for the "http_sse_ticker" example.'
	]).

	:- uses(http_core, [
		header/3, status/2
	]).

	cover(http_sse_ticker_server).
	cover(http_sse_ticker_client).
	cover(http_sse_ticker_demo).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_sse_ticker_demo_01, deterministic) :-
			http_sse_ticker_demo::run(result(ServerSession, ClientSession)),
			ServerSession = session(ServerResponse, ServerEvents),
			ClientSession = session(ClientResponse, ClientEvents),
			status(ServerResponse, status(200, 'OK')),
			header(ServerResponse, content_type, media_type('text/event-stream', _)),
			status(ClientResponse, status(200, 'OK')),
			header(ClientResponse, content_type, media_type('text/event-stream', _)),
			ServerEvents == [event(tick, 'tick 1', none), event(tick, 'tick 2', none), event(tick, 'tick 3', none)],
			ClientEvents == ServerEvents.

	:- endif.

:- end_object.
