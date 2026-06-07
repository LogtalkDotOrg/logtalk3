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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Unit tests for the "http_websocket_echo" example.'
	]).

	:- uses(http_core, [
		property/2, status/2
	]).

	cover(websocket_echo_server).
	cover(websocket_echo_client).
	cover(http_websocket_echo_demo).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_websocket_echo_demo_01, deterministic) :-
			http_websocket_echo_demo::run(result(ServerSession, ClientSession)),
			ServerSession = session(ServerHandshake, ServerReceived, ServerReply),
			ClientSession = session(ClientHandshake, ClientSent, ClientReply),
			status(ServerHandshake, status(101, 'Switching Protocols')),
			property(ServerHandshake, websocket_protocol([chat])),
			status(ClientHandshake, status(101, 'Switching Protocols')),
			property(ClientHandshake, websocket_protocol([chat])),
			ServerReceived == message(text, hello),
			ServerReply == message(text, 'Echo: hello'),
			ClientSent == message(text, hello),
			ClientReply == message(text, 'Echo: hello').

	:- endif.

:- end_object.
