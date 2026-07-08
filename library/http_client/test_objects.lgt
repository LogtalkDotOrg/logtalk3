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


:- object(echo_http_client_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Echo handler used by the http_client tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(target_http_client_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Target echo handler used by the http_client tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::target(Request, Target),
		target_text(Target, Text),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Text)), [], Response).

	target_text(origin(Path), Path).
	target_text(origin(Path, Query), Text) :-
		atom_concat(Path, '?', Prefix),
		atom_concat(Prefix, Query, Text).

:- end_object.


:- object(multipart_http_client_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Multipart summary handler used by the http_client tests.'
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


:- object(probe_http_client_socket).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Probe socket object used by the http_client tests to verify scheme-derived defaults.'
	]).

	:- uses(http_websocket_handshake, [
		websocket_accept/2
	]).

	:- public(supported_request_scheme/1).
	:- public(supported_websocket_scheme/1).
	:- public(exchange/5).
	:- public(open_connection/4).
	:- public(close_connection/1).
	:- public(exchange/3).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	exchange(Host, Port, _Request, Response, Options) :-
		http_core::response(http(1, 1), status(200, 'OK'), [], empty, [probe(Host, Port, Options)], Response).

	open_connection(Host, Port, probe_connection(Host, Port, Options), Options).

	close_connection(_Connection).

	exchange(probe_connection(Host, Port, Options), Request, Response) :-
		http_core::property(Request, websocket_key(Key)),
		websocket_accept(Key, Accept),
		http_core::response(
			http(1, 1),
			status(101, 'Switching Protocols'),
			[],
			empty,
			[connection([upgrade]), upgrade([websocket]), websocket_accept(Accept), probe(Host, Port, Options)],
			Response
		).

:- end_object.


:- object(websocket_http_client_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'WebSocket handshake handler used by the http_client tests.'
	]).

	handle(Request, Response) :-
		http_server_core::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.
