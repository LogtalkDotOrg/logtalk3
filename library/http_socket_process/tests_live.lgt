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


:- object(echo_http_socket_process_live_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-18,
		comment is 'Echo handler used by the http_socket_process live smoke tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(websocket_http_socket_process_live_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-18,
		comment is 'WebSocket handshake handler used by the http_socket_process live smoke tests.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(tests_live,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-18,
		comment is 'Opt-in live smoke tests for the "http_socket_process" library covering external HTTPS client paths and local plain/TLS/WSS server paths.'
	]).

	:- private(certificate_file/1).
	:- dynamic(certificate_file/1).

	:- private(key_file/1).
	:- dynamic(key_file/1).

	:- uses(http_core, [
		is_response/1, request/7, status/2
	]).

	:- uses(os, [
		environment_variable/2
	]).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	:- meta_predicate(with_temporary_tls_credentials(2)).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	cover(http_socket_process).

	setup :-
		cleanup,
		^^file_path('http_socket_process_live_cert.pem', CertificateFile),
		^^file_path('http_socket_process_live_key.pem', KeyFile),
		os::resolve_command_path(openssl, Path),
		process::create(Path, ['req', '-quiet', '-x509', '-newkey', 'rsa:2048', '-nodes', '-keyout', KeyFile, '-out', CertificateFile, '-subj', '/CN=127.0.0.1', '-days', '1'], [process(Process)]),
		process::wait(Process, Status),
		once((Status == 0; Status == exit(0))),
		assertz(certificate_file(CertificateFile)),
		assertz(key_file(KeyFile)).

	cleanup :-
		^^clean_file('http_socket_process_live_cert.pem'),
		^^clean_file('http_socket_process_live_key.pem'),
		retractall(certificate_file(_)),
		retractall(key_file(_)).

	test(http_socket_process_exchange_4_live_01, deterministic) :-
		live_endpoint(Host, Port, Path),
		live_request(Host, Path, Request),
		http_socket_process::exchange(Host, Port, Request, Response),
		valid_live_client_response(Response).

	test(http_socket_process_open_connection_4_live_01, deterministic) :-
		live_endpoint(Host, Port, Path),
		live_request(Host, Path, Request),
		http_socket_process::open_connection(Host, Port, Connection, [type(binary)]),
		setup_call_cleanup(
			true,
			(	http_socket_process::exchange(Connection, Request, Response),
				valid_live_client_response(Response)
			),
			http_socket_process::close_connection(Connection)
		).

	test(http_socket_process_exchange_connection_3_live_01, deterministic) :-
		live_endpoint(Host, Port, Path),
		live_request(Host, Path, Request),
		http_socket_process::open_connection(Host, Port, Connection, [type(binary)]),
		setup_call_cleanup(
			true,
			(	http_socket_process::exchange_connection(Connection, [Request, Request], [Response1, Response2]),
				valid_live_client_response(Response1),
				valid_live_client_response(Response2)
			),
			http_socket_process::close_connection(Connection)
		).

	test(http_socket_process_open_connection_pool_4_live_01, deterministic) :-
		live_endpoint(Host, Port, Path),
		live_request(Host, Path, Request),
		http_socket_process::open_connection_pool(Host, Port, Pool, [min_size(1), max_size(1), connection_options([type(binary)])]),
		setup_call_cleanup(
			true,
			(	http_socket_process::connection_pool_stats(Pool, stats(1, 0, 1, 1, 1)),
				http_socket_process::exchange(Pool, Request, Response1),
				valid_live_client_response(Response1),
				http_socket_process::connection_pool_stats(Pool, stats(1, 0, 1, 1, 1)),
				http_socket_process::exchange(Pool, Request, Response2),
				valid_live_client_response(Response2),
				http_socket_process::connection_pool_stats(Pool, stats(1, 0, 1, 1, 1))
			),
			http_socket_process::close_connection_pool(Pool)
		).

	:- if(current_logtalk_flag(threads, supported)).

		test(http_socket_process_serve_once_3_live_01, deterministic, [condition(executable_available(ncat))]) :-
			Host = '127.0.0.1',
			Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, [connection([close])]),
			http_socket_process::open_listener(Host, Port, Listener, []),
			threaded_once(local_plain_exchange(Host, Port, Request, Response), Tag),
			http_socket_process::serve_once(Listener, echo_http_socket_process_live_handler, ClientInfo),
			threaded_exit(local_plain_exchange(Host, Port, Request, Response), Tag),
			http_socket_process::close_listener(Listener),
			compound(ClientInfo),
			status(Response, status(200, 'OK')).

		test(http_socket_process_serve_once_3_live_02, deterministic, [condition(executable_available(ncat))]) :-
			with_temporary_tls_credentials(run_tls_serve_once_smoke_test).

		test(http_socket_process_serve_websocket_once_5_live_01, deterministic, [condition(executable_available(ncat))]) :-
			with_temporary_tls_credentials(run_wss_serve_websocket_smoke_test).

		% auxiliary predicates

		run_tls_serve_once_smoke_test(CertificateFile, KeyFile) :-
			Host = '127.0.0.1',
			Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, [connection([close])]),
			http_socket_process::open_listener(Host, Port, Listener, [listener_transport(tls), tls_certificate_file(CertificateFile), tls_key_file(KeyFile)]),
			threaded_once(local_tls_exchange(Host, Port, CertificateFile, Request, Response), Tag),
			http_socket_process::serve_once(Listener, echo_http_socket_process_live_handler, ClientInfo),
			threaded_exit(local_tls_exchange(Host, Port, CertificateFile, Request, Response), Tag),
			http_socket_process::close_listener(Listener),
			compound(ClientInfo),
			status(Response, status(200, 'OK')).

		run_wss_serve_websocket_smoke_test(CertificateFile, KeyFile) :-
			Host = '127.0.0.1',
			Path = '/socket',
			Key = 'dGhlIHNhbXBsZSBub25jZQ==',
			http_socket_process::open_listener(Host, Port, Listener, [listener_transport(tls), tls_certificate_file(CertificateFile), tls_key_file(KeyFile)]),
			Control = websocket_tls_client_sync(Host, Port, Path, Key),
			start_websocket_tls_client_exchange(Control, Host, Port, Path, [chat], Key, CertificateFile, ClientTag),
			http_socket_process::serve_websocket_once(Listener, websocket_http_socket_process_live_handler, ServerConnection, ServerResponse, ClientInfo),
			await_websocket_tls_client_exchange(Control, ClientConnection, ClientResponse),
			http_socket_process::connection_streams(ServerConnection, ServerInput, ServerOutput),
			http_socket_process::connection_streams(ClientConnection, ClientInput, ClientOutput),
			http_websocket_client_session::initial_state(ClientState0),
			http_websocket_server_session::initial_state(ServerState0),
			ClientMessage = message(text, hello),
			ServerReplyMessage = message(pong, [0'p, 0'o, 0'n, 0'g]),
			http_websocket_client_session::write_message(ClientOutput, ClientMessage),
			http_websocket_server_session::read_message(ServerInput, ServerOutput, ServerState0, _ServerState, ServerMessage),
			http_websocket_server_session::write_message(ServerOutput, ServerReplyMessage),
			http_websocket_client_session::read_message(ClientInput, ClientOutput, ClientState0, _ClientState, ClientReplyMessage),
			finish_websocket_tls_client_exchange(Control, Host, Port, Path, [chat], Key, CertificateFile, ClientConnection, ClientResponse, ClientTag),
			http_socket_process::close_connection(ServerConnection),
			http_socket_process::close_connection(ClientConnection),
			http_socket_process::close_listener(Listener),
			compound(ClientInfo),
			status(ServerResponse, status(101, 'Switching Protocols')),
			http_core::property(ServerResponse, websocket_protocol([chat])),
			status(ClientResponse, status(101, 'Switching Protocols')),
			http_core::property(ClientResponse, websocket_protocol([chat])),
			ServerMessage == ClientMessage,
			ClientReplyMessage == ServerReplyMessage.

		local_plain_exchange(Host, Port, Request, Response) :-
			setup_call_cleanup(
				socket::client_open(Host, Port, Input, Output, [type(binary)]),
				http_client_core::exchange(Input, Output, Request, Response),
				socket::close(Input, Output)
			).

		local_tls_exchange(Host, Port, CertificateFile, Request, Response) :-
			setup_call_cleanup(
				http_socket_process::open_connection(Host, Port, Connection, [type(binary), server_name(none), openssl_arguments(['-CAfile', CertificateFile, '-verify_return_error'])]),
				http_socket_process::exchange(Connection, Request, Response),
				http_socket_process::close_connection(Connection)
			).

		websocket_tls_client_exchange(Host, Port, Path, Protocols, Key, CertificateFile, Connection, Response) :-
			request(
				get,
				origin(Path),
				http(1, 1),
				[],
				empty,
				[
					host(Host, Port),
					connection([upgrade]),
					upgrade([websocket]),
					websocket_key(Key),
					websocket_version(13),
					websocket_protocol(Protocols)
				],
				Request
			),
			http_socket_process::open_connection(Host, Port, Connection, [type(binary), server_name(none), openssl_arguments(['-CAfile', CertificateFile, '-verify_return_error'])]),
			catch(
				http_socket_process::exchange(Connection, Request, Response),
				Error,
				(	catch(http_socket_process::close_connection(Connection), _, true),
					throw(Error)
				)
			).

		start_websocket_tls_client_exchange(Control, Host, Port, Path, Protocols, Key, CertificateFile, Tag) :-
			threaded_call(websocket_tls_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, CertificateFile, _Connection, _Response), Tag).

		await_websocket_tls_client_exchange(Control, Connection, Response) :-
			threaded_wait(websocket_tls_client_ready(Control, Connection, Response)).

		finish_websocket_tls_client_exchange(Control, Host, Port, Path, Protocols, Key, CertificateFile, Connection, Response, Tag) :-
			threaded_notify(websocket_tls_client_release(Control)),
			once(threaded_exit(websocket_tls_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, CertificateFile, Connection, Response), Tag)).

		websocket_tls_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, CertificateFile, Connection, Response) :-
			websocket_tls_client_exchange(Host, Port, Path, Protocols, Key, CertificateFile, Connection, Response),
			threaded_notify(websocket_tls_client_ready(Control, Connection, Response)),
			threaded_wait(websocket_tls_client_release(Control)).

	:- endif.

	live_endpoint(Host, Port, Path) :-
		live_host(Host),
		live_port(Port),
		live_path(Path).

	live_request(Host, Path, Request) :-
		request(get, origin(Path), http(1, 1), [host-host(Host)], empty, [], Request).

	valid_live_client_response(Response) :-
		is_response(Response),
		status(Response, status(Code, _ReasonPhrase)),
		integer(Code),
		Code >= 200,
		Code =< 599.

	live_host(Host) :-
		(	environment_variable('LOGTALK_HTTP_SOCKET_PROCESS_LIVE_HOST', Host0) ->
			Host = Host0
		;	Host = 'httpbin.org'
		).

	live_port(Port) :-
		(	environment_variable('LOGTALK_HTTP_SOCKET_PROCESS_LIVE_PORT', PortAtom) ->
			atom_codes(PortAtom, PortCodes),
			number_codes(Port, PortCodes)
		;	Port = 443
		).

	live_path(Path) :-
		(	environment_variable('LOGTALK_HTTP_SOCKET_PROCESS_LIVE_PATH', Path0) ->
			Path = Path0
		;	Path = '/'
		).

	with_temporary_tls_credentials(Goal) :-
		certificate_file(CertificateFile),
		key_file(KeyFile),
		call(Goal, CertificateFile, KeyFile).

	executable_available(Executable) :-
		os::resolve_command_path(Executable, _).

:- end_object.
