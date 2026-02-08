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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2026-02-07,
		comment is 'Unit tests for the "sockets" library.'
	]).

	cover(socket).

	% test current_host/1

	test(sockets_current_host_01, true(atom(Host))) :-
		socket::current_host(Host).

	test(sockets_current_host_02, true(Host \== '')) :-
		socket::current_host(Host).

	% test server_open/3 and server_close/1

	test(sockets_server_open_close_01, true) :-
		% test opening and closing a server socket with explicit port
		socket::server_open(0, ServerSocket),
		socket::server_close(ServerSocket).

	test(sockets_server_open_close_02, true(integer(Port))) :-
		% test that port 0 binds to an available port
		socket::server_open(Port, ServerSocket),
		socket::server_close(ServerSocket).

	test(sockets_server_open_close_03, true(Port > 0)) :-
		% test that the assigned port is a valid port number
		socket::server_open(Port, ServerSocket),
		socket::server_close(ServerSocket).

	% test client-server communication using threads (when available)
	% these tests verify client_open/4, server_accept/4, and close/1

	:- if(current_logtalk_flag(threads, supported)).

	:- threaded.

	test(sockets_client_server_01, true) :-
		% test basic client-server connection
		socket::server_open(Port, ServerSocket),
		threaded_ignore(
			client_connect_and_close(Port)
		),
		socket::server_accept(ServerSocket, ClientInput, ClientOutput, _ClientInfo),
		socket::close(ClientInput, ClientOutput),
		socket::server_close(ServerSocket).

	test(sockets_client_server_02, true(compound(ClientInfo))) :-
		% test that client info is returned
		socket::server_open(Port, ServerSocket),
		threaded_ignore(
			client_connect_and_close(Port)
		),
		socket::server_accept(ServerSocket, ClientInput, ClientOutput, ClientInfo),
		socket::close(ClientInput, ClientOutput),
		socket::server_close(ServerSocket).

	test(sockets_client_server_03, true(Byte == 42)) :-
		% test sending and receiving data
		socket::server_open(Port, ServerSocket),
		threaded_ignore(
			client_send_byte(Port, 42)
		),
		socket::server_accept(ServerSocket, ClientInput, ClientOutput, _ClientInfo),
		get_byte(ClientInput, Byte),
		socket::close(ClientInput, ClientOutput),
		socket::server_close(ServerSocket).

	test(sockets_client_server_04, true(Byte == 123)) :-
		% test bidirectional communication
		socket::server_open(Port, ServerSocket),
		threaded_ignore(
			client_echo(Port, 123)
		),
		socket::server_accept(ServerSocket, ClientInput, ClientOutput, _ClientInfo),
		% read the byte from client
		get_byte(ClientInput, Byte),
		% echo it back
		put_byte(ClientOutput, Byte),
		flush_output(ClientOutput),
		socket::close(ClientInput, ClientOutput),
		socket::server_close(ServerSocket).

	test(sockets_client_server_05, true) :-
		% test multiple sequential connections
		socket::server_open(Port, ServerSocket),
		% First connection
		threaded_ignore(client_connect_and_close(Port)),
		socket::server_accept(ServerSocket, ClientInput1, ClientOutput1, _),
		socket::close(ClientInput1, ClientOutput1),
		% Second connection
		threaded_ignore(client_connect_and_close(Port)),
		socket::server_accept(ServerSocket, ClientInput2, ClientOutput2, _),
		socket::close(ClientInput2, ClientOutput2),
		socket::server_close(ServerSocket).

	% Helper predicates for threaded tests

	client_connect_and_close(Port) :-
		catch(
			(	socket::client_open('127.0.0.1', Port, Input, Output),
				socket::close(Input, Output)
			),
			_,
			true
		).

	client_send_byte(Port, Byte) :-
		catch(
			(	socket::client_open('127.0.0.1', Port, Input, Output),  write('client_open\n'),
				put_byte(Output, Byte),
				flush_output(Output),
				socket::close(Input, Output)
			),
			_,
			true
		).

	client_echo(Port, Byte) :-
		catch(
			(	socket::client_open('127.0.0.1', Port, Input, Output),
				put_byte(Output, Byte),
				flush_output(Output),
				% read the echoed byte (but don't check it here)
				get_byte(Input, _),
				socket::close(Input, Output)
			),
			_,
			true
		).

	:- endif.

:- end_object.
