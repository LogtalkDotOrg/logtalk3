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
		date is 2026-02-06,
		comment is 'Unit tests for the "stomp" library.'
	]).

	cover(stomp).

	% ==========================================================================
	% Frame Inspection Tests
	% ==========================================================================

	test(stomp_frame_command_01, true(Command == 'MESSAGE')) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test'], 'Hello'),
		stomp::frame_command(Frame, Command).

	test(stomp_frame_command_02, true(Command == 'CONNECT')) :-
		Frame = frame('CONNECT', ['accept-version'-'1.2'], ''),
		stomp::frame_command(Frame, Command).

	test(stomp_frame_header_01, true(Value == '/queue/test')) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test', 'message-id'-'123'], 'Hello'),
		stomp::frame_header(Frame, 'destination', Value).

	test(stomp_frame_header_02, true(Value == '123')) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test', 'message-id'-'123'], 'Hello'),
		stomp::frame_header(Frame, 'message-id', Value).

	test(stomp_frame_header_03, fail) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test'], 'Hello'),
		stomp::frame_header(Frame, 'nonexistent', _).

	test(stomp_frame_headers_01, true(Headers == ['destination'-'/queue/test', 'message-id'-'123'])) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test', 'message-id'-'123'], 'Hello'),
		stomp::frame_headers(Frame, Headers).

	test(stomp_frame_headers_02, true(Headers == [])) :-
		Frame = frame('DISCONNECT', [], ''),
		stomp::frame_headers(Frame, Headers).

	test(stomp_frame_body_01, true(Body == 'Hello, World!')) :-
		Frame = frame('MESSAGE', ['destination'-'/queue/test'], 'Hello, World!'),
		stomp::frame_body(Frame, Body).

	test(stomp_frame_body_02, true(Body == '')) :-
		Frame = frame('DISCONNECT', ['receipt'-'77'], ''),
		stomp::frame_body(Frame, Body).

	test(stomp_frame_body_03, true(Body == '{"key":"value"}')) :-
		Frame = frame('MESSAGE', ['content-type'-'application/json'], '{"key":"value"}'),
		stomp::frame_body(Frame, Body).

	% ==========================================================================
	% Connection Structure Tests
	% ==========================================================================

	test(stomp_connection_alive_01, fail) :-
		% A fake connection should fail the alive test
		FakeConnection = connection(invalid_stream, localhost, 61613, '', '1.2', 0, 0),
		stomp::connection_alive(FakeConnection).

	% ==========================================================================
	% Integration Tests (require running STOMP server and threads support)
	% These tests are skipped by default since they need an external server.
	% To run them, set up a STOMP server on localhost:61613.
	% ==========================================================================

	test(stomp_connect_disconnect_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]), write(user_output, 'Connected\n'),
		stomp::connection_alive(Connection), write(user_output, 'Alive\n'),
		stomp::disconnect(Connection, []).

	test(stomp_connect_with_host_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::disconnect(Connection, []).

	test(stomp_connect_with_heartbeat_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/'), heartbeat(10000, 10000)]),
		stomp::disconnect(Connection, []).

	test(stomp_send_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::send(Connection, '/queue/test', 'Hello, World!', []),
		stomp::disconnect(Connection, []).

	test(stomp_send_with_content_type_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::send(Connection, '/queue/test', '{"msg":"test"}', [
			content_type('application/json')
		]),
		stomp::disconnect(Connection, []).

	test(stomp_subscribe_unsubscribe_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::subscribe(Connection, '/queue/test', 'sub-0', []),
		stomp::unsubscribe(Connection, 'sub-0', []),
		stomp::disconnect(Connection, []).

	test(stomp_subscribe_with_ack_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::subscribe(Connection, '/queue/test', 'sub-0', [ack(client)]),
		stomp::unsubscribe(Connection, 'sub-0', []),
		stomp::disconnect(Connection, []).

	test(stomp_multiple_subscriptions_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::subscribe(Connection, '/queue/test1', 'sub-0', []),
		stomp::subscribe(Connection, '/queue/test2', 'sub-1', []),
		stomp::subscribe(Connection, '/topic/events', 'sub-2', []),
		stomp::unsubscribe(Connection, 'sub-0', []),
		stomp::unsubscribe(Connection, 'sub-1', []),
		stomp::unsubscribe(Connection, 'sub-2', []),
		stomp::disconnect(Connection, []).

	test(stomp_transaction_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::begin_transaction(Connection, 'tx-001', []),
		stomp::send(Connection, '/queue/test', 'Message 1', [transaction('tx-001')]),
		stomp::send(Connection, '/queue/test', 'Message 2', [transaction('tx-001')]),
		stomp::commit_transaction(Connection, 'tx-001', []),
		stomp::disconnect(Connection, []).

	test(stomp_transaction_abort_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		stomp::begin_transaction(Connection, 'tx-002', []),
		stomp::send(Connection, '/queue/test', 'Message to abort', [transaction('tx-002')]),
		stomp::abort_transaction(Connection, 'tx-002', []),
		stomp::disconnect(Connection, []).

	test(stomp_send_receive_01, true(Body == 'Test message'), [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		% Subscribe first
		stomp::subscribe(Connection, '/queue/test-recv', 'sub-recv', [ack(auto)]),
		% Send a message
		stomp::send(Connection, '/queue/test-recv', 'Test message', []),
		% Receive the message
		stomp::receive(Connection, Frame, [timeout(5000)]),
		stomp::frame_command(Frame, 'MESSAGE'),
		stomp::frame_body(Frame, Body),
		stomp::unsubscribe(Connection, 'sub-recv', []),
		stomp::disconnect(Connection, []).

	test(stomp_send_receive_with_ack_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/')]),
		% Subscribe with client acknowledgment
		stomp::subscribe(Connection, '/queue/test-ack', 'sub-ack', [ack(client)]),
		% Send a message
		stomp::send(Connection, '/queue/test-ack', 'Message to ack', []),
		% Receive the message
		stomp::receive(Connection, Frame, [timeout(5000)]),
		stomp::frame_command(Frame, 'MESSAGE'),
		stomp::frame_header(Frame, 'ack', AckId),
		% Acknowledge the message
		stomp::ack(Connection, AckId, []),
		stomp::unsubscribe(Connection, 'sub-ack', []),
		stomp::disconnect(Connection, []).

	test(stomp_heartbeat_01, true, [condition(stomp_server_available)]) :-
		stomp::connect(localhost, 61613, Connection, [host('/'), heartbeat(1000, 1000)]),
		% Send a heartbeat manually
		stomp::send_heartbeat(Connection),
		stomp::disconnect(Connection, []).

	% auxiliary predicates

	stomp_server_available :-
		catch(
			(	socket::client_open(localhost, 61613, Stream, []),
				socket::close(Stream)
			),
			_,
			fail
		).

:- end_object.
