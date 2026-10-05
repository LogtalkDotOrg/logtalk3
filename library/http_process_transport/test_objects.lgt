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


:- object(echo_http_process_transport_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-09-10,
		comment is 'Echo handler used by the "http_process_transport" library server-side tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(websocket_http_process_transport_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-09-13,
		comment is 'WebSocket handshake handler used by the "http_process_transport" library server-side tests.'
	]).

	handle(Request, Response) :-
		http_server_core::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(failing_response_http_process_transport_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-07,
		comment is 'Handler used by the "http_process_transport" library tests to trigger a response-stream error.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::target(Request, origin('/error')),
		!,
		http_core::response(Version, status(200, 'OK'), [], content('application/octet-stream', file('missing_http_process_transport_test_file.tmp', 0, 1)), [], Response).
	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(ok)), [], Response).

:- end_object.
