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


:- object(websocket_http_socket_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'WebSocket handshake handler used by the http_socket tests.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(echo_http_socket_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Echo handler used by the http_socket tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(closing_http_socket_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Handler used by the http_socket tests to force connection-close responses.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], empty, [connection([close])], Response).

:- end_object.


:- object(keep_alive_close_http_socket_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Handler used by the http_socket tests to keep the first HTTP/1.0 response persistent and require an explicit close on the second request.'
	]).

	handle(Request, Response) :-
		Request = request(get, origin('/one'), Version, _Headers, empty, _Properties),
		http_core::response(Version, status(200, 'OK'), [], empty, [connection(['keep-alive'])], Response).
	handle(Request, Response) :-
		Request = request(get, origin('/two'), Version, _Headers, empty, _Properties),
		http_core::property(Request, connection([close])),
		http_core::response(Version, status(200, 'OK'), [], empty, [], Response).

:- end_object.
