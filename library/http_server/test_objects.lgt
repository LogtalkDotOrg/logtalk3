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


:- object(websocket_http_server_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'WebSocket handshake handler used by the http_server tests.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(websocket_no_protocol_http_server_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-24,
		comment is 'WebSocket handshake handler used by the http_server tests to exercise declining all offered subprotocols.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, []).

:- end_object.


:- object(websocket_extensions_http_server_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-24,
		comment is 'WebSocket handshake handler used by the http_server tests to exercise unsupported extension negotiation.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::property(Request, websocket_key(Key)),
		http_websocket_handshake::websocket_accept(Key, Accept),
		http_core::response(
			Version,
			status(101, 'Switching Protocols'),
			[sec_websocket_extensions-'permessage-deflate'],
			empty,
			[connection([upgrade]), upgrade([websocket]), websocket_accept(Accept)],
			Response
		).

:- end_object.


:- object(echo_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Echo handler used by the http_server tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(fixed_body_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Handler used by the http_server tests to return a fixed non-empty response body.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(ready)), [], Response).

:- end_object.


:- object(closing_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Handler used by the http_server tests to force connection closure.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [connection([close])], Response).

:- end_object.


:- object(failing_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Failing handler used by the http_server tests.'
	]).

	handle(_Request, _Response) :-
		fail.

:- end_object.


:- object(multipart_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Multipart summary handler used by the http_server tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_multipart::fields(Body, [field(title, Title, _FieldParameters)]),
		http_multipart::files(Body, [file(upload, Filename, 'text/plain', text(hello), _FileParameters)]),
		atom_concat('title=', Title, Prefix),
		atom_concat(Prefix, '; upload=', Prefix0),
		atom_concat(Prefix0, Filename, Summary),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Summary)), [], Response).

:- end_object.
