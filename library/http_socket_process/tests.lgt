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


:- object(echo_http_socket_process_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-20,
		comment is 'Echo handler used by the "http_socket_process" library server-side tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::body(Request, Body),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

:- end_object.


:- object(websocket_http_socket_process_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-19,
		comment is 'WebSocket handshake handler used by the "http_socket_process" library server-side tests.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-06-19,
		comment is 'Unit tests for the "http_socket_process" library.'
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	:- uses(user, [
		setup_call_cleanup/3
	]).

	cover(http_socket_process).

	test(http_socket_process_open_connection_4_01, deterministic) :-
		http_socket_process::valid_options([type(text)]).

	test(http_socket_process_open_connection_pool_4_02, deterministic) :-
		http_socket_process::open_connection_pool('example.com', 443, Pool, [min_size(0), max_size(2), connection_options([type(text)])]),
		http_socket_process::close_connection_pool(Pool),
		true.

	test(http_socket_process_connection_pool_stats_2_01, true(Stats == stats(0, 0, 0, 0, 2))) :-
		http_socket_process::open_connection_pool('example.com', 443, Pool, [min_size(0), max_size(2), connection_options([type(text)])]),
		setup_call_cleanup(
			true,
			http_socket_process::connection_pool_stats(Pool, Stats),
			http_socket_process::close_connection_pool(Pool)
		).

	test(http_socket_process_exchange_connection_pool_3_01, true((Responses == [], StatsBefore == stats(0, 0, 0, 0, 2), StatsAfter == stats(0, 0, 0, 0, 2)))) :-
		http_socket_process::open_connection_pool('example.com', 443, Pool, [min_size(0), max_size(2), connection_options([type(text)])]),
		setup_call_cleanup(
			true,
			(	http_socket_process::connection_pool_stats(Pool, StatsBefore),
				http_socket_process::exchange_connection(Pool, [], Responses),
				http_socket_process::connection_pool_stats(Pool, StatsAfter)
			),
			http_socket_process::close_connection_pool(Pool)
		).

	test(http_socket_process_connection_pool_stats_2_02, deterministic) :-
		http_socket_process::open_connection_pool('example.com', 443, Pool, [min_size(0), max_size(2), connection_options([type(text)])]),
		http_socket_process::close_connection_pool(Pool),
		catch(
			http_socket_process::connection_pool_stats(Pool, _Stats),
			error(existence_error(http_socket_connection_pool, Pool), _),
			true
		).

	test(http_socket_process_open_connection_pool_4_01, error(domain_error(http_socket_process_connection_pool_options, [min_size(2), max_size(1)]))) :-
		http_socket_process::open_connection_pool('example.com', 443, _Pool, [min_size(2), max_size(1)]).

	test(http_socket_process_exchange_connection_4_01, true(Responses == [])) :-
		http_socket_process::exchange_connection('example.com', 443, [], Responses).

	test(http_socket_process_connection_streams_3_01, error(domain_error(http_socket_connection, invalid_connection))) :-
		http_socket_process::connection_streams(invalid_connection, _Input, _Output).

	test(http_socket_process_open_listener_4_01, deterministic, [condition(executable_available(ncat))]) :-
		http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
		integer(Port),
		Port > 0,
		http_socket_process::close_listener(Listener).

	test(http_socket_process_open_listener_4_02, deterministic, [condition(executable_available(ncat))]) :-
		http_socket_process::open_listener('127.0.0.1', Port, Listener, [backlog(1)]),
		integer(Port),
		Port > 0,
		http_socket_process::close_listener(Listener).

	test(http_socket_process_open_listener_4_03, deterministic, [condition(executable_available(ncat))]) :-
		http_socket_process::open_listener('127.0.0.1', Port, Listener, [type(text)]),
		integer(Port),
		Port > 0,
		http_socket_process::close_listener(Listener).

	test(http_socket_process_open_listener_4_04, error(domain_error(http_socket_process_listener_tls_options, [tls_certificate_file(none), tls_key_file(none)])), [condition(executable_available(ncat))]) :-
		http_socket_process::open_listener('127.0.0.1', _Port, _Listener, [listener_transport(tls)]).

	test(http_socket_process_open_listener_4_05, deterministic, [condition(executable_available(ncat))]) :-
		Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, [connection([close])]),
		ResponseAtom = 'HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n',
		setup_call_cleanup(
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
			setup_call_cleanup(
				open_pending_raw_connection('127.0.0.1', Port, Request, Input, Output),
				(	raw_server_once(Listener, ResponseAtom, ClientInfo),
					http_client_core::read_response(Input, Response),
					compound(ClientInfo),
					http_core::status(Response, status(200, 'OK'))
				),
				close_stream_pair_silently(Input, Output)
			),
			http_socket_process::close_listener(Listener)
		).

	test(http_socket_process_serve_once_3_01, deterministic, [condition(executable_available(ncat))]) :-
		Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, [connection([close])]),
		setup_call_cleanup(
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
			setup_call_cleanup(
				open_pending_raw_connection('127.0.0.1', Port, Request, Input, Output),
				(	http_socket_process::serve_once(Listener, echo_http_socket_process_handler, ClientInfo),
					http_client_core::read_response(Input, Response),
					compound(ClientInfo),
					http_core::status(Response, status(200, 'OK'))
				),
				close_stream_pair_silently(Input, Output)
			),
			http_socket_process::close_listener(Listener)
		).

	test(http_socket_process_serve_websocket_once_5_01, deterministic, [condition(executable_available(ncat))]) :-
		Host = '127.0.0.1',
		Path = '/socket',
		Key = 'dGhlIHNhbXBsZSBub25jZQ==',
		setup_call_cleanup(
			http_socket_process::open_listener(Host, Port, Listener, []),
			(	http_core::request(
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
						websocket_protocol([chat, superchat])
					],
					Request
				),
				setup_call_cleanup(
					open_pending_raw_connection(Host, Port, Request, Input, Output),
					(	http_socket_process::serve_websocket_once(Listener, websocket_http_socket_process_handler, ServerConnection, ServerResponse, ClientInfo),
						setup_call_cleanup(
							true,
							(	http_client_core::read_response(Input, ClientResponse),
								http_socket_process::connection_streams(ServerConnection, ServerInput, ServerOutput),
								once(stream_property(ServerInput, _)),
								once(stream_property(ServerOutput, _)),
								compound(ClientInfo),
								http_core::status(ServerResponse, status(101, 'Switching Protocols')),
								http_core::property(ServerResponse, websocket_protocol([chat])),
								http_core::status(ClientResponse, status(101, 'Switching Protocols')),
								http_core::property(ClientResponse, websocket_protocol([chat]))
							),
							http_socket_process::close_connection(ServerConnection)
						)
					),
					close_stream_pair_silently(Input, Output)
				)
			),
			http_socket_process::close_listener(Listener)
		).

	:- if(current_logtalk_flag(threads, supported)).

		test(http_socket_process_exchange_4_02, deterministic, [condition(executable_available(ncat))]) :-
			Host = '127.0.0.1',
			Request = request(get, origin('/ping'), http(1, 1), [host-host(Host)], empty, []),
			http_socket_process::open_listener(Host, Port, Listener, []),
			threaded_once(http_socket_process::exchange(Host, Port, Request, Response), Tag),
			http_socket_process::serve_once(Listener, echo_http_socket_process_handler, ClientInfo),
			threaded_exit(http_socket_process::exchange(Host, Port, Request, Response), Tag),
			http_socket_process::close_listener(Listener),
			compound(ClientInfo),
			http_core::status(Response, status(200, 'OK')).

		test(http_socket_process_open_listener_4_06, deterministic, [condition(executable_available(ncat))]) :-
			setup_call_cleanup(
				http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
				(	threaded_once(server_accept_and_close_once(Listener), Tag),
					setup_call_cleanup(
						http_socket_process::open_connection('127.0.0.1', Port, Connection, []),
						threaded_exit(server_accept_and_close_once(Listener), Tag),
						http_socket_process::close_connection(Connection)
					)
				),
				http_socket_process::close_listener(Listener)
			).

		test(http_socket_process_request_shutdown_1_01, deterministic, [condition(executable_available(ncat))]) :-
			Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, [connection([close])]),
			http_socket_process::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket_process::serve_until_shutdown(Listener, echo_http_socket_process_handler, control, []), Tag),
			client_exchange_ignore('127.0.0.1', Port, Request),
			http_socket_process::request_shutdown(control),
			threaded_exit(http_socket_process::serve_until_shutdown(Listener, echo_http_socket_process_handler, control, []), Tag).

			test(http_socket_process_serve_until_shutdown_5_01, deterministic, [condition(executable_available(ncat))]) :-
			Control = control_ready,
			http_socket_process::open_listener('127.0.0.1', _Port, Listener, []),
			http_socket_process::serve_until_shutdown(Listener, echo_http_socket_process_handler, Control, [], request_ready_shutdown(Control)).

	:- endif.

	% auxiliary predicates

	client_exchange_ignore(Host, Port, Request) :-
		setup_call_cleanup(
			open_raw_connection(Host, Port, Input, Output),
			http_client_core::exchange(Input, Output, Request, _Response),
			close_stream_pair_silently(Input, Output)
		).

	request_ready_shutdown(Control) :-
		http_socket_process::request_shutdown(Control).

	open_raw_connection(Host, Port, Input, Output) :-
		socket::client_open(Host, Port, Input, Output, [type(binary)]).

	open_pending_raw_connection(Host, Port, Request, Input, Output) :-
		open_raw_connection(Host, Port, Input, Output),
		http_client_core::write_request(Output, Request).

	raw_server_once(Listener, ResponseAtom, ClientInfo) :-
		socket::server_accept(Listener, Input, Output, ClientInfo),
		( 	catch(
				(	http_server::read_request(Input, _Request),
					atom_codes(ResponseAtom, Bytes),
					write_bytes(Bytes, Output),
					flush_output(Output)
				),
				Error,
				(	catch(close_stream_pair(Input, Output), _, true),
					throw(Error)
				)
			) ->
			close_stream_pair(Input, Output)
		;	close_stream_pair(Input, Output),
			fail
		).

	server_accept_and_close_once(Listener) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		close_stream_pair(Input, Output).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	close_stream_pair_silently(Input, Output) :-
		catch(close_stream_pair(Input, Output), _, true).

	close_stream_pair(Input, Output) :-
		(	Input == Output ->
			close(Input)
		;	close(Input),
			close(Output)
		).

	executable_available(Executable) :-
		os::resolve_command_path(Executable, _).

:- end_object.
