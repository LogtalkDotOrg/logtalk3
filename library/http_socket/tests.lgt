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
		date is 2026-06-10,
		comment is 'Unit tests for the "http_socket" library.'
	]).

	:- uses(http_core, [body/2, property/2, request/7, status/2]).

	cover(http_socket).

	:- if(current_logtalk_flag(threads, supported)).

	:- threaded.

	test(http_socket_open_close_listener_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		integer(Port),
		Port > 0,
		http_socket::close_listener(Listener).

	test(http_socket_open_close_connection_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_accept_and_close_once(Listener), Tag),
		http_socket::open_connection('127.0.0.1', Port, Connection, []),
		http_socket::close_connection(Connection),
		threaded_exit(server_accept_and_close_once(Listener), Tag),
		http_socket::close_listener(Listener).

	test(http_socket_open_close_connection_pool_4_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_accept_and_close_once(Listener), Tag),
		http_socket::open_connection_pool('127.0.0.1', Port, Pool, [min_size(1), max_size(2)]),
		http_socket::connection_pool_stats(Pool, stats(1, 0, 1, 1, 2)),
		http_socket::close_connection_pool(Pool),
		threaded_exit(server_accept_and_close_once(Listener), Tag),
		http_socket::close_listener(Listener).

	test(http_socket_serve_once_3_01, true(compound(ClientInfo))) :-
		Request = request(get, origin('/ping'), http(1, 1), [host-host('example.com')], empty, []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(client_exchange_ignore('127.0.0.1', Port, Request)),
		http_socket::serve_once(Listener, echo_http_socket_handler, ClientInfo),
		http_socket::close_listener(Listener).

	test(http_socket_serve_websocket_once_5_01, deterministic) :-
		Host = '127.0.0.1',
		Path = '/socket',
		Key = 'dGhlIHNhbXBsZSBub25jZQ==',
		http_socket::open_listener(Host, Port, Listener, []),
		Control = websocket_client_sync(Host, Port, Path, Key),
		start_websocket_client_exchange(Control, Host, Port, Path, [chat, superchat], Key, ClientTag),
		http_socket::serve_websocket_once(Listener, websocket_http_socket_handler, ServerConnection, ServerResponse, ClientInfo),
		await_websocket_client_exchange(Control, ClientConnection, ClientResponse),
		http_socket::connection_streams(ServerConnection, ServerInput, ServerOutput),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		finish_websocket_client_exchange(Control, Host, Port, Path, [chat, superchat], Key, ClientConnection, ClientResponse, ClientTag),
		once(stream_property(ServerInput, _)),
		once(stream_property(ServerOutput, _)),
		once(stream_property(ClientInput, _)),
		once(stream_property(ClientOutput, _)),
		http_socket::close_connection(ServerConnection),
		http_socket::close_connection(ClientConnection),
		http_socket::close_listener(Listener),
		compound(ClientInfo),
		status(ServerResponse, status(101, 'Switching Protocols')),
		property(ServerResponse, websocket_protocol([chat])),
		status(ClientResponse, status(101, 'Switching Protocols')),
		property(ClientResponse, websocket_protocol([chat])).

	test(http_socket_serve_websocket_once_5_02, error(domain_error(http_socket_websocket_response, _))) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		Request = request(get, origin('/socket'), http(1, 1), [host-host('example.com')], empty, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request, _Response), ClientTag),
		catch(http_socket::serve_websocket_once(Listener, echo_http_socket_handler, _Connection, _HandshakeResponse, _ClientInfo), Error, (threaded_exit(client_exchange_response('127.0.0.1', Port, Request, _Response), ClientTag), http_socket::close_listener(Listener), throw(Error))),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request, _Response), ClientTag),
		http_socket::close_listener(Listener).

	test(http_socket_exchange_4_01, deterministic) :-
		Request = request(post, origin('/echo'), http(1, 1), [host-host('example.com')], content('text/plain', text(hello)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(server_serve_once(Listener, echo_http_socket_handler)),
		http_socket::exchange('127.0.0.1', Port, Request, Response),
		http_socket::close_listener(Listener),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(hello))).

	test(http_socket_exchange_3_01, deterministic) :-
		Request1 = request(post, origin('/one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
		Request2 = request(post, origin('/two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::open_connection('127.0.0.1', Port, Connection, []),
		http_socket::exchange(Connection, Request1, Response1),
		http_socket::exchange(Connection, Request2, Response2),
		http_socket::close_connection(Connection),
		threaded_exit(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_exchange_pool_3_01, deterministic) :-
		Request1 = request(post, origin('/pool-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
		Request2 = request(post, origin('/pool-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::open_connection_pool('127.0.0.1', Port, Pool, [max_size(1)]),
		http_socket::exchange(Pool, Request1, Response1),
		http_socket::exchange(Pool, Request2, Response2),
		http_socket::connection_pool_stats(Pool, stats(1, 0, 1, 0, 1)),
		http_socket::close_connection_pool(Pool),
		threaded_exit(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_exchange_connection_4_01, deterministic) :-
		Requests = [
			request(post, origin('/one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
			request(post, origin('/two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), [])
		],
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_once(Listener, echo_http_socket_handler), Tag),
		http_socket::exchange_connection('127.0.0.1', Port, Requests, [Response1, Response2]),
		threaded_exit(server_serve_once(Listener, echo_http_socket_handler), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_exchange_connection_4_02, deterministic) :-
		Requests = [
			request(get, origin('/one'), http(1, 0), [host-host('example.com'), connection-['keep-alive', 'keep-alive']], empty, []),
			request(get, origin('/two'), http(1, 0), [host-host('example.com'), connection-['keep-alive', 'keep-alive']], empty, [])
		],
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_once(Listener, echo_http_socket_handler), Tag),
		http_socket::exchange_connection('127.0.0.1', Port, Requests, [Response1, Response2]),
		threaded_exit(server_serve_once(Listener, echo_http_socket_handler), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, empty),
		property(Response1, connection(['keep-alive'])),
		status(Response2, status(200, 'OK')),
		body(Response2, empty),
		\+ property(Response2, connection(_)).

	test(http_socket_exchange_connection_3_01, deterministic) :-
		Requests = [
			request(post, origin('/one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
			request(post, origin('/two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), [])
		],
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::open_connection('127.0.0.1', Port, Connection, []),
		http_socket::exchange_connection(Connection, Requests, [Response1, Response2]),
		http_socket::close_connection(Connection),
		threaded_exit(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_exchange_connection_pool_3_01, deterministic) :-
		Requests = [
			request(post, origin('/pool-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
			request(post, origin('/pool-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), [])
		],
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::open_connection_pool('127.0.0.1', Port, Pool, [max_size(1)]),
		http_socket::exchange_connection(Pool, Requests, [Response1, Response2]),
		http_socket::connection_pool_stats(Pool, stats(1, 0, 1, 0, 1)),
		http_socket::close_connection_pool(Pool),
		threaded_exit(server_serve_requests_once(Listener, echo_http_socket_handler, 2), Tag),
		http_socket::close_listener(Listener),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_exchange_pool_3_02, deterministic) :-
		Control = closing_exchange_pool_control,
		Request = request(get, origin('/pool-close'), http(1, 1), [host-host('example.com')], empty, []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_until_shutdown(Listener, closing_http_socket_handler, Control, [workers(per_connection)]), ServeTag),
		http_socket::open_connection_pool('127.0.0.1', Port, Pool, [max_size(1)]),
		http_socket::exchange(Pool, Request, Response),
		http_socket::connection_pool_stats(Pool, stats(0, 0, 0, 0, 1)),
		http_socket::close_connection_pool(Pool),
		http_socket::request_shutdown(Control),
		threaded_exit(http_socket::serve_until_shutdown(Listener, closing_http_socket_handler, Control, [workers(per_connection)]), ServeTag),
		status(Response, status(200, 'OK')),
		body(Response, empty),
		client_connection_state('127.0.0.1', Port, closed).

	test(http_socket_serve_listener_4_01, deterministic) :-
		Request1 = request(get, origin('/one'), http(1, 1), [host-host('example.com')], empty, []),
		Request2 = request(get, origin('/two'), http(1, 1), [host-host('example.com')], empty, []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_ignore(client_exchange_ignore('127.0.0.1', Port, Request1)),
		threaded_ignore(client_exchange_ignore('127.0.0.1', Port, Request2)),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 2, [ClientInfo1, ClientInfo2]),
		http_socket::close_listener(Listener),
		compound(ClientInfo1),
		compound(ClientInfo2).

	test(http_socket_serve_listener_5_01, deterministic) :-
		Request1 = request(post, origin('/one'), http(1, 1), [host-host('example.com')], content('text/plain', text(one)), []),
		Request2 = request(post, origin('/two'), http(1, 1), [host-host('example.com')], content('text/plain', text(two)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 1, [ClientInfo1], [shutdown(keep_open)]),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		http_socket::serve_once(Listener, echo_http_socket_handler, ClientInfo2),
		http_socket::close_listener(Listener),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		compound(ClientInfo1),
		compound(ClientInfo2),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(one))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(two))).

	test(http_socket_serve_listener_5_02, deterministic) :-
		Request = request(get, origin('/shutdown'), http(1, 1), [host-host('example.com')], empty, []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request, Response), Tag),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 1, [ClientInfo], [shutdown(close)]),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request, Response), Tag),
		compound(ClientInfo),
		status(Response, status(200, 'OK')),
		body(Response, empty),
		client_connection_state('127.0.0.1', Port, closed).

	test(http_socket_serve_listener_5_03, deterministic) :-
		Request1 = request(post, origin('/parallel-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/parallel-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 2, [ClientInfo1, ClientInfo2], [workers(per_connection), shutdown(close)]),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		compound(ClientInfo1),
		compound(ClientInfo2),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))).

	test(http_socket_serve_listener_5_04, deterministic) :-
		Request1 = request(post, origin('/pool-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/pool-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 2, [ClientInfo1, ClientInfo2], [workers(pool(1)), shutdown(close)]),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		compound(ClientInfo1),
		compound(ClientInfo2),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))).

	test(http_socket_serve_listener_5_05, deterministic) :-
		Request1 = request(post, origin('/rolling-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/rolling-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		Request3 = request(post, origin('/rolling-three'), http(1, 1), [host-host('example.com')], content('text/plain', text(gamma)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request3, Response3), Tag3),
		http_socket::serve_listener(Listener, echo_http_socket_handler, 3, [ClientInfo1, ClientInfo2, ClientInfo3], [workers(pool(2, rolling)), shutdown(close)]),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), Tag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), Tag2),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request3, Response3), Tag3),
		compound(ClientInfo1),
		compound(ClientInfo2),
		compound(ClientInfo3),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		status(Response3, status(200, 'OK')),
		body(Response3, content('text/plain', text(gamma))).

	test(http_socket_serve_until_shutdown_4_01, deterministic) :-
		Control = serial_listener_control,
		Request = request(post, origin('/serial-shutdown'), http(1, 1), [host-host('example.com')], content('text/plain', text(serial)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, []), ServeTag),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request, Response), ClientTag),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request, Response), ClientTag),
		http_socket::request_shutdown(Control),
		threaded_exit(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, []), ServeTag),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(serial))),
		client_connection_state('127.0.0.1', Port, closed).

	test(http_socket_serve_until_shutdown_4_02, deterministic) :-
		Control = per_connection_listener_control,
		Request1 = request(post, origin('/parallel-shutdown-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/parallel-shutdown-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(per_connection)]), ServeTag),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		http_socket::request_shutdown(Control),
		threaded_exit(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(per_connection)]), ServeTag),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		client_connection_state('127.0.0.1', Port, closed).

	test(http_socket_serve_until_shutdown_4_03, deterministic) :-
		Control = pool_listener_control,
		Request1 = request(post, origin('/pool-shutdown-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/pool-shutdown-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(pool(1))]), ServeTag),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		http_socket::request_shutdown(Control),
		threaded_exit(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(pool(1))]), ServeTag),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		client_connection_state('127.0.0.1', Port, closed).

	test(http_socket_serve_until_shutdown_4_04, deterministic) :-
		Control = rolling_pool_listener_control,
		Request1 = request(post, origin('/rolling-pool-one'), http(1, 1), [host-host('example.com')], content('text/plain', text(alpha)), []),
		Request2 = request(post, origin('/rolling-pool-two'), http(1, 1), [host-host('example.com')], content('text/plain', text(beta)), []),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(pool(2, rolling))]), ServeTag),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_once(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request1, Response1), ClientTag1),
		threaded_exit(client_exchange_response('127.0.0.1', Port, Request2, Response2), ClientTag2),
		http_socket::request_shutdown(Control),
		threaded_exit(http_socket::serve_until_shutdown(Listener, echo_http_socket_handler, Control, [workers(pool(2, rolling))]), ServeTag),
		status(Response1, status(200, 'OK')),
		body(Response1, content('text/plain', text(alpha))),
		status(Response2, status(200, 'OK')),
		body(Response2, content('text/plain', text(beta))),
		client_connection_state('127.0.0.1', Port, closed).

	client_exchange_ignore(Host, Port, Request) :-
		catch(http_socket::exchange(Host, Port, Request, _Response), _, true).

	client_exchange_response(Host, Port, Request, Response) :-
		once(http_socket::exchange(Host, Port, Request, Response)).

	client_connection_state(Host, Port, State) :-
		(	catch(
				once((
					socket::client_open(Host, Port, Input, Output),
					socket::close(Input, Output)
				)),
				error(socket_error(_), _),
				fail
			) ->
				State = open
		;	State = closed
		).

	server_serve_once(Listener, Handler) :-
		catch(http_socket::serve_once(Listener, Handler, _ClientInfo), _, true).

	server_accept_and_close_once(Listener) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		socket::close(Input, Output).

	server_serve_requests_once(Listener, Handler, Count) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		(	catch(
				serve_requests(Count, Input, Output, Handler),
				Error,
				(	catch(socket::close(Input, Output), _, true),
					throw(Error)
				)
			) ->
			socket::close(Input, Output)
		;		socket::close(Input, Output),
			fail
		).

	serve_requests(0, _Input, _Output, _Handler) :-
		!.
	serve_requests(Count, Input, Output, Handler) :-
		Count > 0,
		http_server::serve(Input, Output, Handler),
		NextCount is Count - 1,
		serve_requests(NextCount, Input, Output, Handler).

	websocket_client_exchange(Host, Port, Path, Protocols, Key, Connection, Response) :-
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
		http_socket::open_connection(Host, Port, Connection, []),
		catch(
			http_socket::exchange(Connection, Request, Response),
			Error,
			(	catch(http_socket::close_connection(Connection), _, true),
				throw(Error)
			)
		).

	start_websocket_client_exchange(Control, Host, Port, Path, Protocols, Key, Tag) :-
		threaded_call(websocket_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, _Connection, _Response), Tag).

	await_websocket_client_exchange(Control, Connection, Response) :-
		threaded_wait(websocket_client_ready(Control, Connection, Response)).

	finish_websocket_client_exchange(Control, Host, Port, Path, Protocols, Key, Connection, Response, Tag) :-
		threaded_notify(websocket_client_release(Control)),
		once(threaded_exit(websocket_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, Connection, Response), Tag)).

	websocket_client_exchange_worker(Control, Host, Port, Path, Protocols, Key, Connection, Response) :-
		websocket_client_exchange(Host, Port, Path, Protocols, Key, Connection, Response),
		threaded_notify(websocket_client_ready(Control, Connection, Response)),
		threaded_wait(websocket_client_release(Control)).

	:- endif.

:- end_object.
