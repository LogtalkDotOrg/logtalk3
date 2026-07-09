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


:- object(tests(_HTTPTransport_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Unit tests for the "http_websocket_service" library.'
	]).

	:- uses(http_core, [
		request/7, status/2
	]).

	:- uses(_HTTPTransport_, [
		close_connection/1, close_listener/1, connection_streams/3, exchange/3, open_connection/4,
		open_listener/4, serve_websocket_once/5
	]).

	:- uses(http_websocket_client_service(_HTTPTransport_), [
		open/5
	]).

	:- uses(http_websocket_server_service(_HTTPTransport_), [
		request_shutdown/1, run_session/4, serve_once/7, serve_until_shutdown/6
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_websocket_service(_, _, _)).
	cover(http_websocket_client_service(_)).
	cover(http_websocket_server_service(_)).
	cover(http_websocket_service_registry).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	test(http_websocket_service_registry_broadcast_except_3_01, deterministic, [cleanup(catch(http_websocket_service_registry::close(Registry), _, true))]) :-
		http_websocket_service_registry::open(Registry),
		http_websocket_service_registry::register(Registry, Session1),
		http_websocket_service_registry::register(Registry, Session2),
		http_websocket_service_registry::send(Registry, Session1, message(text, direct)),
		http_websocket_service_registry::broadcast_except(Registry, Session1, message(text, broadcast)),
		http_websocket_service_registry::take_pending(Registry, Session1, Messages1),
		http_websocket_service_registry::take_pending(Registry, Session2, Messages2),
		http_websocket_service_registry::session_count(Registry, Count),
		Messages1 == [message(text, direct)],
		Messages2 == [message(text, broadcast)],
		Count == 2.

	test(http_websocket_service_registry_send_3_01, deterministic, [cleanup(catch(http_websocket_service_registry::close(Registry), _, true))]) :-
		http_websocket_service_registry::open(Registry),
		http_websocket_service_registry::register(Registry, Session),
		http_websocket_service_registry::send(Registry, Session, message(text, first)),
		http_websocket_service_registry::send(Registry, Session, message(text, second)),
		http_websocket_service_registry::send(Registry, Session, message(text, third)),
		http_websocket_service_registry::take_pending(Registry, Session, Messages),
		http_websocket_service_registry::take_pending(Registry, Session, MessagesAfterDrain),
		http_websocket_service_registry::session_count(Registry, Count),
		Messages == [message(text, first), message(text, second), message(text, third)],
		MessagesAfterDrain == [],
		Count == 1.

	:- if(current_logtalk_flag(threads, supported)).

	test(http_websocket_service_run_session_4_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [auto_pong(on)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		% exchange the frames incrementally to avoid stressing buffered
		% socket writes on backends that stall on burst frame writes
		write_client_frame(final, ping, [0'!], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, PongFrame),
		write_client_frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [5, 6, 7, 8], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, ReplyFrame),
		write_client_frame(final, close, [0x03, 0xE8], [9, 10, 11, 12], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		threaded_exit(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [auto_pong(on)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		http_websocket_frames::opcode(PongFrame, pong),
		http_websocket_frames::payload(PongFrame, [0'!]),
		\+ http_websocket_frames::property(PongFrame, masking_key(_)),
		http_websocket_frames::opcode(ReplyFrame, text),
		http_websocket_frames::payload(ReplyFrame, [0'o, 0'k]),
		\+ http_websocket_frames::property(ReplyFrame, masking_key(_)),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_service_run_session_4_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_client_frame(final, continuation, [], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_session_sequence_error(ServerError),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_service_run_session_4_03, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_client_frame(final, text, [0xC3, 0x28], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_message_text_error(ServerError),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xEF]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_service_run_session_4_04, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_bytes([0x83, 0x80, 0x01, 0x02, 0x03, 0x04], ClientOutput),
		flush_output(ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_websocket_opcode_error(ServerError),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_service_serve_once_7_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once_with_client_info(Listener, websocket_service_loop_handler, ServerResponse, ServerState, ClientInfo, [auto_pong(on)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_client_frame(final, ping, [0'!], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, PongFrame),
		write_client_frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [5, 6, 7, 8], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, ReplyFrame),
		write_client_frame(final, close, [0x03, 0xE8], [9, 10, 11, 12], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		threaded_exit(server_session_once_with_client_info(Listener, websocket_service_loop_handler, ServerResponse, ServerState, ClientInfo, [auto_pong(on)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		compound(ClientInfo),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		http_websocket_frames::opcode(PongFrame, pong),
		http_websocket_frames::payload(PongFrame, [0'!]),
		\+ http_websocket_frames::property(PongFrame, masking_key(_)),
		http_websocket_frames::opcode(ReplyFrame, text),
		http_websocket_frames::payload(ReplyFrame, [0'o, 0'k]),
		\+ http_websocket_frames::property(ReplyFrame, masking_key(_)),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_client_service_open_5_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(serve_once(Listener, websocket_handshake_handler, websocket_service_loop_handler, ServerResponse, ServerState, _ClientInfo, [auto_pong(on)]), ServerTag),
		local_ws_url(Port, '/socket', URL),
		open(
			URL,
			websocket_client_service_loop_handler,
			ClientResponse,
			ClientState,
			[
				protocols([chat]),
				key('dGhlIHNhbXBsZSBub25jZQ=='),
				initial_messages([message(text, hello)]),
				auto_pong(on),
				keepalive_interval(5),
				idle_timeout(5),
				max_payload_length(5)
			]
		),
		threaded_exit(serve_once(Listener, websocket_handshake_handler, websocket_service_loop_handler, ServerResponse, ServerState, _ClientInfo, [auto_pong(on)]), ServerTag),
		close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		ClientState == session_state(idle, closed(status(1000), status(1000))).

	test(http_websocket_server_service_serve_once_7_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [keepalive_interval(0.2)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, PingFrame),
		write_client_close(ClientOutput, status(1000)),
		read_reply_frames_until_close(2.0, ClientInput, [CloseFrame]),
		threaded_exit(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [keepalive_interval(0.2)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		http_websocket_frames::opcode(PingFrame, ping),
		http_websocket_frames::payload(PingFrame, []),
		\+ http_websocket_frames::property(PingFrame, masking_key(_)),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_service_serve_once_7_03, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [idle_timeout(0.2)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		write_client_close(ClientOutput, status(1001, idle_timeout)),
		threaded_exit(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, ServerState, [idle_timeout(0.2)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1001, idle_timeout), status(1001, idle_timeout))),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xE9, 0'i, 0'd, 0'l, 0'e, 0'_, 0't, 0'i, 0'm, 0'e, 0'o, 0'u, 0't]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_service_serve_once_7_04, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, [keepalive_interval(5)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_client_frame(final, continuation, [], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, [keepalive_interval(5)]), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_session_sequence_error(ServerError),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_service_serve_once_7_05, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, [max_payload_length(0)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_client_frame(final, text, [0'h], [1, 2, 3, 4], ClientOutput),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_session_once(Listener, websocket_service_loop_handler, ServerResponse, _ServerState, [max_payload_length(0)]), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_payload_length_limit_error(ServerError),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xF1]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	test(http_websocket_client_service_open_5_02, deterministic) :-
		websocket_keepalive_close_handler::reset,
		open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_keepalive_close_handler, ServerResponse, ServerState, []), ServerTag),
		local_ws_url(Port, '/socket', URL),
		open(
			URL,
			websocket_client_service_loop_handler,
			ClientResponse,
			ClientState,
			[
				protocols([chat]),
				key('dGhlIHNhbXBsZSBub25jZQ=='),
				keepalive_interval(0.2)
			]
		),
		threaded_exit(server_session_once(Listener, websocket_keepalive_close_handler, ServerResponse, ServerState, []), ServerTag),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		ClientState == session_state(idle, closed(status(1000), status(1000))),
		websocket_keepalive_close_handler::captured(Message),
		Message == message(ping, []).

	test(http_websocket_client_service_open_5_03, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 401 Unauthorized\r\nwww-authenticate: Basic realm="socket"\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(open(URL, websocket_client_service_loop_handler, _Response, _State, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_authentication_rejection, Response), _),
		status(Response, status(401, 'Unauthorized')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		safe_close_listener(Listener).

	test(http_websocket_client_service_open_5_04, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 302 Found\r\nlocation: /other-socket\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(open(URL, websocket_client_service_loop_handler, _Response, _State, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_redirection_rejection, Response), _),
		status(Response, status(302, 'Found')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		safe_close_listener(Listener).

	test(http_websocket_server_service_serve_until_shutdown_6_01, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_service_registry::open(Registry),
		Control = websocket_broadcast_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_broadcast_service_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection1, ClientResponse1),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection2, ClientResponse2),
		wait_for_registry_session_count(Registry, 2),
		connection_streams(ClientConnection1, ClientInput1, ClientOutput1),
		connection_streams(ClientConnection2, ClientInput2, ClientOutput2),
		http_websocket_client_session::message(text, hello, BroadcastMessage),
		http_websocket_client_session::write_message(ClientOutput1, BroadcastMessage),
		read_frame_with_timeout(2.0, ClientInput2, BroadcastFrame),
		write_client_close(ClientOutput1, status(1000)),
		read_reply_frames_until_close(2.0, ClientInput1, [CloseFrame1]),
		write_client_close(ClientOutput2, status(1000)),
		read_reply_frames_until_close(2.0, ClientInput2, [CloseFrame2]),
		safe_close_connection(ClientConnection1),
		safe_close_connection(ClientConnection2),
		request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_broadcast_service_handler, Registry, Control, []), ServerTag),
		http_websocket_service_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse1, status(101, 'Switching Protocols')),
		status(ClientResponse2, status(101, 'Switching Protocols')),
		http_websocket_frames::opcode(BroadcastFrame, text),
		http_websocket_frames::payload(BroadcastFrame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		\+ http_websocket_frames::property(BroadcastFrame, masking_key(_)),
		http_websocket_frames::opcode(CloseFrame1, close),
		http_websocket_frames::payload(CloseFrame1, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame1, masking_key(_)),
		http_websocket_frames::opcode(CloseFrame2, close),
		http_websocket_frames::payload(CloseFrame2, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame2, masking_key(_)).

	test(http_websocket_server_service_serve_until_shutdown_6_02, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_service_registry::open(Registry),
		Control = websocket_close_broadcast_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_close_broadcast_service_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection1, ClientResponse1),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection2, ClientResponse2),
		wait_for_registry_session_count(Registry, 2),
		connection_streams(ClientConnection1, ClientInput1, ClientOutput1),
		connection_streams(ClientConnection2, ClientInput2, ClientOutput2),
		write_client_close(ClientOutput1, status(1000)),
		read_frame_with_timeout(2.0, ClientInput1, CloseFrame1),
		write_client_close(ClientOutput2, status(1000)),
		read_reply_frames_until_close(2.0, ClientInput2, Client2Frames),
		safe_close_connection(ClientConnection1),
		safe_close_connection(ClientConnection2),
		request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_close_broadcast_service_handler, Registry, Control, []), ServerTag),
		http_websocket_service_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse1, status(101, 'Switching Protocols')),
		status(ClientResponse2, status(101, 'Switching Protocols')),
		http_websocket_frames::opcode(CloseFrame1, close),
		http_websocket_frames::payload(CloseFrame1, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame1, masking_key(_)),
		Client2Frames = [CloseFrame2],
		http_websocket_frames::opcode(CloseFrame2, close),
		http_websocket_frames::payload(CloseFrame2, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame2, masking_key(_)).

	test(http_websocket_server_service_serve_until_shutdown_6_03, deterministic) :-
		open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_service_registry::open(Registry),
		Control = websocket_close_flush_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_close_then_broadcast_service_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		wait_for_registry_session_count(Registry, 1),
		connection_streams(ClientConnection, ClientInput, ClientOutput),
		http_websocket_client_session::message(text, hello, Message),
		http_websocket_client_session::write_message(ClientOutput, Message),
		read_frame_with_timeout(2.0, ClientInput, CloseFrame),
		http_websocket_service_registry::sessions(Registry, [Session]),
		http_websocket_service_registry::take_pending(Registry, Session, PendingMessages),
		PendingMessages == [message(text, should_not_flush)],
		http_websocket_service_registry::send(Registry, Session, message(text, should_not_flush)),
		write_client_close(ClientOutput, status(1000)),
		wait_for_registry_session_count(Registry, 0),
		safe_close_connection(ClientConnection),
		request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_close_then_broadcast_service_handler, Registry, Control, []), ServerTag),
		http_websocket_service_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse, status(101, 'Switching Protocols')),
		http_websocket_frames::opcode(CloseFrame, close),
		http_websocket_frames::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket_frames::property(CloseFrame, masking_key(_)).

	:- endif.

	% auxiliary predicates

	:- if(current_logtalk_flag(threads, supported)).

	write_client_frame(Final, Opcode, PayloadBytes, MaskingKey, Output) :-
		http_websocket_frames::frame(Final, Opcode, PayloadBytes, [masking_key(MaskingKey)], Frame),
		http_websocket_frames::write_frame(Output, Frame),
		flush_output(Output).

	raw_server_once(Listener, ResponseAtom) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		(	catch(
				(	http_server_core::read_request(Input, _Request),
					atom_codes(ResponseAtom, Bytes),
					write_bytes(Bytes, Output),
					flush_output(Output)
				),
				Error,
				(	catch(socket::close(Input, Output), _, true),
					throw(Error)
				)
			) ->
			socket::close(Input, Output)
		;	socket::close(Input, Output),
			fail
		).

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
		open_connection(Host, Port, Connection, []),
		catch(
			exchange(Connection, Request, Response),
			Error,
			(	catch(close_connection(Connection), _, true),
				throw(Error)
			)
		).

	local_ws_url(Port, Path, URL) :-
		atomic_list_concat(['ws://127.0.0.1:', Port, Path], URL).

	server_session_once(Listener, SessionHandler, Response, State, Options) :-
		serve_once(Listener, websocket_handshake_handler, SessionHandler, Response, State, _ClientInfo, Options).

	server_session_once_with_client_info(Listener, SessionHandler, Response, State, ClientInfo, Options) :-
		serve_once(Listener, websocket_handshake_handler, SessionHandler, Response, State, ClientInfo, Options).

	server_run_session(Listener, SessionHandler, Response, State, Options) :-
		serve_websocket_once(Listener, websocket_handshake_handler, Connection, Response, _ClientInfo),
		run_session(Connection, SessionHandler, State, Options).

	server_session_until_shutdown(Listener, SessionHandler, Registry, Control, Options) :-
		serve_until_shutdown(Listener, websocket_handshake_handler, SessionHandler, Registry, Control, Options).

	wait_for_registry_session_count(Registry, ExpectedCount) :-
		wait_for_registry_session_count(Registry, ExpectedCount, 100).

	wait_for_registry_session_count(Registry, ExpectedCount, Retries) :-
		http_websocket_service_registry::session_count(Registry, Count),
		(	Count == ExpectedCount ->
			true
		;	Retries > 0 ->
			os::sleep(0.01),
			NextRetries is Retries - 1,
			wait_for_registry_session_count(Registry, ExpectedCount, NextRetries)
		;	throw(timeout_error(wait_for_registry_session_count, ExpectedCount))
		).

	read_reply_frames_until_close(Seconds, Input, Frames) :-
		read_frame_result_with_timeout(Seconds, Input, Result),
		read_reply_frames_until_close_result(Result, Seconds, Input, Frames).

	read_reply_frames_until_close_result(frame(Frame), Seconds, Input, [Frame| Frames]) :-
		(	Frame == end_of_file ->
			Frames = []
		;	http_websocket_frames::opcode(Frame, close) ->
			Frames = []
		;	read_reply_frames_until_close(Seconds, Input, Frames)
		).
	read_reply_frames_until_close_result(timeout, _Seconds, _Input, []).
	read_reply_frames_until_close_result(error(Error), _Seconds, _Input, [error(Error)]).
	read_reply_frames_until_close_result(fail, _Seconds, _Input, [fail]).

	read_frame_result_with_timeout(Seconds, Input, Result) :-
		timeout::call_with_timeout(http_websocket_frames::read_frame(Input, Frame), Seconds, TimeoutResult),
		(	TimeoutResult == true ->
			Result = frame(Frame)
		;	TimeoutResult == timeout ->
			Result = timeout
		;	TimeoutResult = error(Error) ->
			Result = error(Error)
		;	Result = fail
		).

	read_frame_with_timeout(Seconds, Input, Frame) :-
		timeout::call_with_timeout(http_websocket_frames::read_frame(Input, Frame0), Seconds, Result),
		(	Result == true ->
			Frame = Frame0
		;	Result == timeout ->
			throw(timeout_error(read_frame, Seconds))
		;	Result = error(Error) ->
			throw(Error)
		;	throw(error(unexpected_timeout_result(Result), read_frame_with_timeout/3))
		).

	expected_session_sequence_error(error(domain_error(http_websocket_session_sequence, _Frame), _Context)).

	expected_message_text_error(error(domain_error(http_websocket_message_text, _Payload), _Context)).

	expected_websocket_opcode_error(error(domain_error(http_websocket_opcode, _Opcode), _Context)).

	expected_payload_length_limit_error(error(domain_error(http_websocket_payload_length_limit, 1), _Context)).

	write_client_close(Output, Payload) :-
		http_websocket_client_session::message(close, Payload, Message),
		http_websocket_client_session::write_message(Output, Message),
		flush_output(Output).

	safe_close_connection(Connection) :-
		catch(close_connection(Connection), _, true).

	safe_close_listener(Listener) :-
		catch(close_listener(Listener), _, true).

	safe_close_registry(Registry) :-
		catch(http_websocket_service_registry::close(Registry), _, true).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	:- endif.

:- end_object.
