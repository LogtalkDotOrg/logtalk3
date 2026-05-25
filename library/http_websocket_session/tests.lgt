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
		date is 2026-05-23,
		comment is 'Unit tests for the "http_websocket_session" library.'
	]).

	:- uses(http, [request/7, status/2]).
	:- uses(user, [atomic_list_concat/2]).

	cover(http_websocket_session(_, _)).
	cover(http_websocket_client_session).
	cover(http_websocket_server_session).
	cover(http_websocket_session_registry).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.
	:- endif.

	cleanup :-
		^^clean_file('test_http_websocket_session.tmp'),
		^^clean_file('test_http_websocket_session_reply.tmp').

	test(http_websocket_session_initial_state_1_01, deterministic(State == session_state(idle))) :-
		http_websocket_server_session::initial_state(State).

	test(http_websocket_session_is_state_1_01, deterministic) :-
		http_websocket_server_session::is_state(session_state(fragment(text, [[0'h, 0'e]]), close_received(status(1000)))).

	test(http_websocket_session_read_message_4_01, deterministic) :-
		http_websocket::frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, State, Message),
		State == session_state(idle),
		Message == message(text, hello).

	test(http_websocket_session_read_message_4_02, deterministic) :-
		http_websocket::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket::frame(final, ping, [0'!], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
		write_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_websocket_server_session::initial_state(State0),
		http_websocket_server_session::read_message(Input, State0, State1, Message1),
		http_websocket_server_session::read_message(Input, State1, State2, Message2),
		close(Input),
		State0 == session_state(idle),
		State1 == session_state(fragment(text, [[0'h, 0'e]])),
		State2 == session_state(idle),
		Message1 == message(ping, [0'!]),
		Message2 == message(text, hello).

	test(http_websocket_session_read_message_4_03, deterministic) :-
		http_websocket::frame(more, binary, [1, 2], [], Frame1),
		http_websocket::frame(final, close, [0x03, 0xE8], [], Frame2),
		http_websocket::frame(final, continuation, [3, 4], [], Frame3),
		write_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_websocket_client_session::initial_state(State0),
		http_websocket_client_session::read_message(Input, State0, State1, Message1),
		catch(http_websocket_client_session::read_message(Input, State1, _State2, _Message2), Error, true),
		close(Input),
		State1 == session_state(idle, close_received(status(1000))),
		Message1 == message(close, status(1000)),
		Error = error(domain_error(http_websocket_session_state, session_state(idle, close_received(status(1000)))), _).

	test(http_websocket_session_read_message_4_06, deterministic) :-
		http_websocket_client_session::message(close, status(1001), SentMessage),
		^^file_path('test_http_websocket_session_reply.tmp', ReplyFile),
		open(ReplyFile, write, ReplyOutput, [type(binary)]),
		http_websocket_client_session::initial_state(State0),
		http_websocket_client_session::write_message(ReplyOutput, State0, State1, SentMessage),
		close(ReplyOutput),
		http_websocket::frame(final, close, [0x03, 0xE8], [], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_websocket_client_session::read_message(Input, State1, State2, Message),
		close(Input),
		State1 == session_state(idle, close_sent(status(1001))),
		State2 == session_state(idle, closed(status(1001), status(1000))),
		Message == message(close, status(1000)).

	test(http_websocket_session_read_message_4_04, error(domain_error(http_websocket_session_masking, _))) :-
		http_websocket::frame(final, text, [0'h], [], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_05, error(domain_error(http_websocket_session_masking, _))) :-
		http_websocket::frame(final, text, [0'h], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_client_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_client_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_07, error(domain_error(http_websocket_session_extensions, _))) :-
		http_websocket::frame(final, text, [0'h], [masking_key([1, 2, 3, 4]), reserved_bits([rsv1])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_08, error(domain_error(http_websocket_session_extensions, _))) :-
		http_websocket::frame(final, text, [0'h], [reserved_bits([rsv2])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_client_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_client_session, State0, _State, _Message).

	test(http_websocket_session_write_message_2_01, deterministic) :-
		http_websocket_client_session::message(text, hello, Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket::opcode(Frame, text),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		http_websocket::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_2_02, deterministic) :-
		fixed_masking_key_http_websocket_client_session::message(text, hello, Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		fixed_masking_key_http_websocket_client_session::write_message(Output, Message),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket::opcode(Frame, text),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		http_websocket::property(Frame, masking_key([1, 2, 3, 4])).

	test(http_websocket_session_write_message_3_01, deterministic) :-
		http_websocket_server_session::message(text, hello, Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_server_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		http_websocket::opcode(Frame1, text),
		http_websocket::final(Frame1, more),
		http_websocket::payload(Frame1, [0'h, 0'e]),
		\+ http_websocket::property(Frame1, masking_key(_)),
		http_websocket::opcode(Frame2, continuation),
		http_websocket::final(Frame2, more),
		http_websocket::payload(Frame2, [0'l, 0'l]),
		\+ http_websocket::property(Frame2, masking_key(_)),
		http_websocket::opcode(Frame3, continuation),
		http_websocket::final(Frame3, final),
		http_websocket::payload(Frame3, [0'o]),
		\+ http_websocket::property(Frame3, masking_key(_)).

	test(http_websocket_session_write_message_3_02, deterministic) :-
		http_websocket_client_session::message(binary, [1, 2, 3, 4], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2]),
		http_websocket::opcode(Frame1, binary),
		http_websocket::final(Frame1, more),
		http_websocket::payload(Frame1, [1, 2]),
		http_websocket::property(Frame1, masking_key(_)),
		http_websocket::opcode(Frame2, continuation),
		http_websocket::final(Frame2, final),
		http_websocket::payload(Frame2, [3, 4]),
		http_websocket::property(Frame2, masking_key(_)).

	test(http_websocket_session_write_message_3_03, deterministic) :-
		http_websocket_client_session::message(ping, [0'!], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(1)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket::opcode(Frame, ping),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, [0'!]),
		http_websocket::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_3_04, deterministic) :-
		http_websocket_server_session::message(text, '', Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_server_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket::opcode(Frame, text),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, []),
		\+ http_websocket::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_3_05, deterministic) :-
		http_websocket_client_session::message(binary, [], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket::opcode(Frame, binary),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, []),
		http_websocket::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_4_01, deterministic) :-
		http_websocket_client_session::message(close, status(1000, bye), Message),
		http_websocket_client_session::initial_state(State0),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, State0, State, Message),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		State == session_state(idle, close_sent(status(1000, bye))),
		http_websocket::opcode(Frame, close),
		http_websocket::final(Frame, final),
		http_websocket::payload(Frame, [0x03, 0xE8, 0'b, 0'y, 0'e]),
		http_websocket::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_4_02, deterministic) :-
		http_websocket_client_session::message(text, hello, Message),
		State0 = session_state(idle, close_received(status(1000))),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		catch(http_websocket_client_session::write_message(Output, State0, _State, Message), Error, true),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', Frames),
		Error = error(domain_error(http_websocket_session_state, session_state(idle, close_received(status(1000)))), _),
		Frames == [].

	test(http_websocket_session_write_message_4_03, deterministic) :-
		http_websocket_client_session::message(text, hello, Message),
		State0 = session_state(idle, close_sent(status(1000))),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		catch(http_websocket_client_session::write_message(Output, State0, _State, Message), Error, true),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', Frames),
		Error = error(domain_error(http_websocket_session_state, session_state(idle, close_sent(status(1000)))), _),
		Frames == [].

	test(http_websocket_session_read_message_5_01, deterministic) :-
		http_websocket::frame(final, close, [0x03, 0xE8, 0'b, 0'y, 0'e], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		^^file_path('test_http_websocket_session.tmp', File),
		^^file_path('test_http_websocket_session_reply.tmp', ReplyFile),
		open(File, read, Input, [type(binary)]),
		open(ReplyFile, write, Output, [type(binary)]),
		http_websocket_server_session::initial_state(State0),
		http_websocket_server_session::read_message(Input, Output, State0, State, Message),
		close(Input),
		close(Output),
		read_frames_file('test_http_websocket_session_reply.tmp', [ReplyFrame]),
		State == session_state(idle, closed(status(1000, bye), status(1000, bye))),
		Message == message(close, status(1000, bye)),
		http_websocket::opcode(ReplyFrame, close),
		http_websocket::final(ReplyFrame, final),
		http_websocket::payload(ReplyFrame, [0x03, 0xE8, 0'b, 0'y, 0'e]),
		\+ http_websocket::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_5_02, deterministic) :-
		http_websocket::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket::frame(final, close, [0x03, 0xE8], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
		write_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		^^file_path('test_http_websocket_session.tmp', File),
		^^file_path('test_http_websocket_session_reply.tmp', ReplyFile),
		open(File, read, Input, [type(binary)]),
		open(ReplyFile, write, Output, [type(binary)]),
		http_websocket_server_session::initial_state(State0),
		http_websocket_server_session::read_message(Input, Output, State0, State, Message),
		close(Input),
		close(Output),
		read_frames_file('test_http_websocket_session_reply.tmp', [ReplyFrame]),
		State == session_state(idle, closed(status(1000), status(1000))),
		Message == message(close, status(1000)),
		http_websocket::opcode(ReplyFrame, close),
		http_websocket::final(ReplyFrame, final),
		http_websocket::payload(ReplyFrame, [0x03, 0xE8]),
		\+ http_websocket::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_6_01, deterministic) :-
		http_websocket::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket::frame(final, ping, [0'!], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
		write_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		^^file_path('test_http_websocket_session.tmp', File),
		^^file_path('test_http_websocket_session_reply.tmp', ReplyFile),
		open(File, read, Input, [type(binary)]),
		open(ReplyFile, write, Output, [type(binary)]),
		http_websocket_server_session::initial_state(State0),
		http_websocket_server_session::read_message(Input, Output, State0, State1, Message1, [auto_pong(on)]),
		http_websocket_server_session::read_message(Input, Output, State1, State2, Message2, [auto_pong(on)]),
		close(Input),
		close(Output),
		read_frames_file('test_http_websocket_session_reply.tmp', [ReplyFrame]),
		State1 == session_state(fragment(text, [[0'h, 0'e]])),
		State2 == session_state(idle),
		Message1 == message(ping, [0'!]),
		Message2 == message(text, hello),
		http_websocket::opcode(ReplyFrame, pong),
		http_websocket::final(ReplyFrame, final),
		http_websocket::payload(ReplyFrame, [0'!]),
		\+ http_websocket::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_6_02, error(domain_error(http_websocket_payload_length_limit, 1))) :-
		http_websocket::frame(final, text, [0'h], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		^^file_path('test_http_websocket_session.tmp', File),
		^^file_path('test_http_websocket_session_reply.tmp', ReplyFile),
		open(File, read, Input, [type(binary)]),
		open(ReplyFile, write, Output, [type(binary)]),
		http_websocket_server_session::initial_state(State0),
		catch(http_websocket_server_session::read_message(Input, Output, State0, _State, _Message, [max_payload_length(0)]), Error, true),
		close(Input),
		close(Output),
		throw(Error).

	test(http_websocket_session_registry_broadcast_except_3_01, deterministic, [cleanup(catch(http_websocket_session_registry::close(Registry), _, true))]) :-
		http_websocket_session_registry::open(Registry),
		http_websocket_session_registry::register(Registry, Session1),
		http_websocket_session_registry::register(Registry, Session2),
		http_websocket_session_registry::send(Registry, Session1, message(text, direct)),
		http_websocket_session_registry::broadcast_except(Registry, Session1, message(text, broadcast)),
		http_websocket_session_registry::take_pending(Registry, Session1, Messages1),
		http_websocket_session_registry::take_pending(Registry, Session2, Messages2),
		http_websocket_session_registry::session_count(Registry, Count),
		Messages1 == [message(text, direct)],
		Messages2 == [message(text, broadcast)],
		Count == 2.

	test(http_websocket_session_registry_send_3_01, deterministic, [cleanup(catch(http_websocket_session_registry::close(Registry), _, true))]) :-
		http_websocket_session_registry::open(Registry),
		http_websocket_session_registry::register(Registry, Session),
		http_websocket_session_registry::send(Registry, Session, message(text, first)),
		http_websocket_session_registry::send(Registry, Session, message(text, second)),
		http_websocket_session_registry::send(Registry, Session, message(text, third)),
		http_websocket_session_registry::take_pending(Registry, Session, Messages),
		http_websocket_session_registry::take_pending(Registry, Session, MessagesAfterDrain),
		http_websocket_session_registry::session_count(Registry, Count),
		Messages == [message(text, first), message(text, second), message(text, third)],
		MessagesAfterDrain == [],
		Count == 1.

	:- if(current_logtalk_flag(threads, supported)).

	test(http_websocket_session_run_session_4_01, deterministic) :-
		http_websocket::frame(final, ping, [0'!], [masking_key([1, 2, 3, 4])], ClientFrame1),
		http_websocket::frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [masking_key([5, 6, 7, 8])], ClientFrame2),
		http_websocket::frame(final, close, [0x03, 0xE8], [masking_key([9, 10, 11, 12])], ClientFrame3),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [auto_pong(on)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame1, ClientFrame2, ClientFrame3], ClientOutput),
		read_reply_frames_until_close(1.0, ClientInput, ClientReplyFrames),
		threaded_exit(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [auto_pong(on)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		ClientReplyFrames = [PongFrame, ReplyFrame, CloseFrame],
		http_websocket::opcode(PongFrame, pong),
		http_websocket::payload(PongFrame, [0'!]),
		\+ http_websocket::property(PongFrame, masking_key(_)),
		http_websocket::opcode(ReplyFrame, text),
		http_websocket::payload(ReplyFrame, [0'o, 0'k]),
		\+ http_websocket::property(ReplyFrame, masking_key(_)),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_session_run_session_4_02, deterministic) :-
		http_websocket::frame(final, continuation, [], [masking_key([1, 2, 3, 4])], ClientFrame),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame], ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_session_sequence_error(ServerError),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_session_run_session_4_03, deterministic) :-
		http_websocket::frame(final, text, [0xC3, 0x28], [masking_key([1, 2, 3, 4])], ClientFrame),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame], ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_message_text_error(ServerError),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xEF]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_session_run_session_4_04, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_bytes([0x83, 0x80, 0x01, 0x02, 0x03, 0x04], ClientOutput),
		flush_output(ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_run_session(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, []), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_websocket_opcode_error(ServerError),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_session_serve_once_7_01, deterministic) :-
		http_websocket::frame(final, ping, [0'!], [masking_key([1, 2, 3, 4])], ClientFrame1),
		http_websocket::frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [masking_key([5, 6, 7, 8])], ClientFrame2),
		http_websocket::frame(final, close, [0x03, 0xE8], [masking_key([9, 10, 11, 12])], ClientFrame3),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once_with_client_info(Listener, websocket_session_loop_handler, ServerResponse, ServerState, ClientInfo, [auto_pong(on)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame1, ClientFrame2, ClientFrame3], ClientOutput),
		read_reply_frames_until_close(1.0, ClientInput, ClientReplyFrames),
		threaded_exit(server_session_once_with_client_info(Listener, websocket_session_loop_handler, ServerResponse, ServerState, ClientInfo, [auto_pong(on)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		compound(ClientInfo),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		ClientReplyFrames = [PongFrame, ReplyFrame, CloseFrame],
		http_websocket::opcode(PongFrame, pong),
		http_websocket::payload(PongFrame, [0'!]),
		\+ http_websocket::property(PongFrame, masking_key(_)),
		http_websocket::opcode(ReplyFrame, text),
		http_websocket::payload(ReplyFrame, [0'o, 0'k]),
		\+ http_websocket::property(ReplyFrame, masking_key(_)),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_client_session_open_5_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(http_websocket_server_session::serve_once(Listener, websocket_http_websocket_session_handler, websocket_session_loop_handler, ServerResponse, ServerState, _ClientInfo, [auto_pong(on)]), ServerTag),
		local_ws_url(Port, '/socket', URL),
		http_websocket_client_session::open(
			URL,
			websocket_client_session_loop_handler,
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
		threaded_exit(http_websocket_server_session::serve_once(Listener, websocket_http_websocket_session_handler, websocket_session_loop_handler, ServerResponse, ServerState, _ClientInfo, [auto_pong(on)]), ServerTag),
		http_socket::close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		ClientState == session_state(idle, closed(status(1000), status(1000))).

	test(http_websocket_server_session_serve_once_7_02, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [keepalive_interval(0.2)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, PingFrame),
		write_client_close(ClientOutput, status(1000)),
		read_reply_frames_until_close(1.0, ClientInput, [CloseFrame]),
		threaded_exit(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [keepalive_interval(0.2)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1000), status(1000))),
		http_websocket::opcode(PingFrame, ping),
		http_websocket::payload(PingFrame, []),
		\+ http_websocket::property(PingFrame, masking_key(_)),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_session_serve_once_7_03, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [idle_timeout(0.2)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		write_client_close(ClientOutput, status(1001, idle_timeout)),
		threaded_exit(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, ServerState, [idle_timeout(0.2)]), ServerTag),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		ServerState == session_state(idle, closed(status(1001, idle_timeout), status(1001, idle_timeout))),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xE9, 0'i, 0'd, 0'l, 0'e, 0'_, 0't, 0'i, 0'm, 0'e, 0'o, 0'u, 0't]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_session_serve_once_7_04, deterministic) :-
		http_websocket::frame(final, continuation, [], [masking_key([1, 2, 3, 4])], ClientFrame),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, [keepalive_interval(5)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame], ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, [keepalive_interval(5)]), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_session_sequence_error(ServerError),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xEA]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_server_session_serve_once_7_05, deterministic) :-
		http_websocket::frame(final, text, [0'h], [masking_key([1, 2, 3, 4])], ClientFrame),
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, [max_payload_length(0)]), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		write_frames([ClientFrame], ClientOutput),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		catch(threaded_exit(server_session_once(Listener, websocket_session_loop_handler, ServerResponse, _ServerState, [max_payload_length(0)]), ServerTag), ServerError, true),
		safe_close_connection(ClientConnection),
		safe_close_listener(Listener),
		status(ServerResponse, status(101, 'Switching Protocols')),
		status(ClientResponse, status(101, 'Switching Protocols')),
		expected_payload_length_limit_error(ServerError),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xF1]),
		\+ http_websocket::property(CloseFrame, masking_key(_)).

	test(http_websocket_client_session_open_5_02, deterministic) :-
		websocket_keepalive_close_handler::reset,
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		threaded_once(server_session_once(Listener, websocket_keepalive_close_handler, ServerResponse, ServerState, []), ServerTag),
		local_ws_url(Port, '/socket', URL),
		http_websocket_client_session::open(
			URL,
			websocket_client_session_loop_handler,
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

	test(http_websocket_client_session_open_5_03, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 401 Unauthorized\r\nwww-authenticate: Basic realm="socket"\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_websocket_client_session::open(URL, websocket_client_session_loop_handler, _Response, _State, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_authentication_rejection, Response), _),
		status(Response, status(401, 'Unauthorized')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		safe_close_listener(Listener).

	test(http_websocket_client_session_open_5_04, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		HandshakeResponse = 'HTTP/1.1 302 Found\r\nlocation: /other-socket\r\ncontent-length: 0\r\n\r\n',
		threaded_once(raw_server_once(Listener, HandshakeResponse), Tag),
		local_ws_url(Port, '/socket', URL),
		catch(http_websocket_client_session::open(URL, websocket_client_session_loop_handler, _Response, _State, [key('dGhlIHNhbXBsZSBub25jZQ==')]), Error, true),
		Error = error(domain_error(http_client_websocket_redirection_rejection, Response), _),
		status(Response, status(302, 'Found')),
		threaded_exit(raw_server_once(Listener, HandshakeResponse), Tag),
		safe_close_listener(Listener).

	test(http_websocket_server_session_serve_until_shutdown_6_01, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_session_registry::open(Registry),
		Control = websocket_broadcast_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_broadcast_session_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection1, ClientResponse1),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection2, ClientResponse2),
		wait_for_registry_session_count(Registry, 2),
		http_socket::connection_streams(ClientConnection1, ClientInput1, ClientOutput1),
		http_socket::connection_streams(ClientConnection2, ClientInput2, ClientOutput2),
		http_websocket_client_session::message(text, hello, BroadcastMessage),
		http_websocket_client_session::write_message(ClientOutput1, BroadcastMessage),
		read_frame_with_timeout(1.0, ClientInput2, BroadcastFrame),
		write_client_close(ClientOutput1, status(1000)),
		write_client_close(ClientOutput2, status(1000)),
		read_reply_frames_until_close(1.0, ClientInput1, [CloseFrame1]),
		read_reply_frames_until_close(1.0, ClientInput2, [CloseFrame2]),
		safe_close_connection(ClientConnection1),
		safe_close_connection(ClientConnection2),
		http_websocket_server_session::request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_broadcast_session_handler, Registry, Control, []), ServerTag),
		http_websocket_session_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse1, status(101, 'Switching Protocols')),
		status(ClientResponse2, status(101, 'Switching Protocols')),
		http_websocket::opcode(BroadcastFrame, text),
		http_websocket::payload(BroadcastFrame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		\+ http_websocket::property(BroadcastFrame, masking_key(_)),
		http_websocket::opcode(CloseFrame1, close),
		http_websocket::payload(CloseFrame1, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame1, masking_key(_)),
		http_websocket::opcode(CloseFrame2, close),
		http_websocket::payload(CloseFrame2, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame2, masking_key(_)).

	test(http_websocket_server_session_serve_until_shutdown_6_02, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_session_registry::open(Registry),
		Control = websocket_close_broadcast_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_close_broadcast_session_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection1, ClientResponse1),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection2, ClientResponse2),
		wait_for_registry_session_count(Registry, 2),
		http_socket::connection_streams(ClientConnection1, ClientInput1, ClientOutput1),
		http_socket::connection_streams(ClientConnection2, ClientInput2, ClientOutput2),
		write_client_close(ClientOutput1, status(1000)),
		read_frame_with_timeout(1.0, ClientInput1, CloseFrame1),
		write_client_close(ClientOutput2, status(1000)),
		read_reply_frames_until_close(1.0, ClientInput2, Client2Frames),
		safe_close_connection(ClientConnection1),
		safe_close_connection(ClientConnection2),
		http_websocket_server_session::request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_close_broadcast_session_handler, Registry, Control, []), ServerTag),
		http_websocket_session_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse1, status(101, 'Switching Protocols')),
		status(ClientResponse2, status(101, 'Switching Protocols')),
		http_websocket::opcode(CloseFrame1, close),
		http_websocket::payload(CloseFrame1, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame1, masking_key(_)),
		Client2Frames = [CloseFrame2],
		http_websocket::opcode(CloseFrame2, close),
		http_websocket::payload(CloseFrame2, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame2, masking_key(_)).

	test(http_websocket_server_session_serve_until_shutdown_6_03, deterministic) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		http_websocket_session_registry::open(Registry),
		Control = websocket_close_flush_control(Port),
		threaded_once(server_session_until_shutdown(Listener, websocket_close_then_broadcast_session_handler, Registry, Control, []), ServerTag),
		websocket_client_exchange('127.0.0.1', Port, '/socket', [chat], 'dGhlIHNhbXBsZSBub25jZQ==', ClientConnection, ClientResponse),
		wait_for_registry_session_count(Registry, 1),
		http_socket::connection_streams(ClientConnection, ClientInput, ClientOutput),
		http_websocket_client_session::message(text, hello, Message),
		http_websocket_client_session::write_message(ClientOutput, Message),
		read_frame_with_timeout(1.0, ClientInput, CloseFrame),
		write_client_close(ClientOutput, status(1000)),
		read_frame_with_timeout(1.0, ClientInput, NextFrame),
		safe_close_connection(ClientConnection),
		http_websocket_server_session::request_shutdown(Control),
		threaded_exit(server_session_until_shutdown(Listener, websocket_close_then_broadcast_session_handler, Registry, Control, []), ServerTag),
		http_websocket_session_registry::session_count(Registry, Count),
		safe_close_listener(Listener),
		safe_close_registry(Registry),
		Count == 0,
		status(ClientResponse, status(101, 'Switching Protocols')),
		http_websocket::opcode(CloseFrame, close),
		http_websocket::payload(CloseFrame, [0x03, 0xE8]),
		\+ http_websocket::property(CloseFrame, masking_key(_)),
		NextFrame == end_of_file.

	:- endif.

	% auxiliary predicates

	write_frames_file(Path, Frames) :-
		^^file_path(Path, File),
		open(File, write, Output, [type(binary)]),
		write_frames(Frames, Output),
		close(Output).

	write_frames([], _Output).
	write_frames([Frame| Frames], Output) :-
		http_websocket::write_frame(Output, Frame),
		write_frames(Frames, Output).

	raw_server_once(Listener, ResponseAtom) :-
		socket::server_accept(Listener, Input, Output, _ClientInfo),
		( 	catch(
				( 	http_server::read_request(Input, _Request),
					atom_codes(ResponseAtom, Bytes),
					write_bytes(Bytes, Output),
					flush_output(Output)
				),
				Error,
				( 	catch(socket::close(Input, Output), _, true),
					throw(Error)
				)
			) ->
			socket::close(Input, Output)
		; 	socket::close(Input, Output),
			fail
		).

	read_frames_file(Path, Frames) :-
		^^file_path(Path, File),
		open(File, read, Input, [type(binary)]),
		read_frames(Input, Frames),
		close(Input).

	read_frames(Input, Frames) :-
		http_websocket::read_frame(Input, Frame),
		( 	Frame == end_of_file ->
			Frames = []
		; 	Frames = [Frame| Rest],
			read_frames(Input, Rest)
		).

	open_file_read_stateful_message(Path, Session, State0, State, Message) :-
		^^file_path(Path, File),
		open(File, read, Input, [type(binary)]),
		Session::read_message(Input, State0, State, Message),
		close(Input).

	:- if(current_logtalk_flag(threads, supported)).

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
			( 	catch(http_socket::close_connection(Connection), _, true),
				throw(Error)
			)
		).

	websocket_client_session_exchange(Host, Port, Path, Protocols, Key, Frames, ReplyFrameCount, Connection, Response, ReplyFrames) :-
		websocket_client_exchange(Host, Port, Path, Protocols, Key, Connection, Response),
		http_socket::connection_streams(Connection, Input, Output),
		write_frames(Frames, Output),
		read_reply_frames(ReplyFrameCount, Input, ReplyFrames).

	read_reply_frames(0, _Input, []) :-
		!.
	read_reply_frames(Count, Input, [Frame| Frames]) :-
		http_websocket::read_frame(Input, Frame),
		NextCount is Count - 1,
		read_reply_frames(NextCount, Input, Frames).

	stream_closed(Stream) :-
		\+ catch(once(stream_property(Stream, _)), _, fail).

	local_ws_url(Port, Path, URL) :-
		atomic_list_concat(['ws://127.0.0.1:', Port, Path], URL).

	server_session_once(Listener, SessionHandler, Response, State, Options) :-
		http_websocket_server_session::serve_once(Listener, websocket_http_websocket_session_handler, SessionHandler, Response, State, _ClientInfo, Options).

	server_session_once_with_client_info(Listener, SessionHandler, Response, State, ClientInfo, Options) :-
		http_websocket_server_session::serve_once(Listener, websocket_http_websocket_session_handler, SessionHandler, Response, State, ClientInfo, Options).

	server_run_session(Listener, SessionHandler, Response, State, Options) :-
		http_socket::serve_websocket_once(Listener, websocket_http_websocket_session_handler, Connection, Response, _ClientInfo),
		http_websocket_server_session::run_session(Connection, SessionHandler, State, Options).

	server_session_until_shutdown(Listener, SessionHandler, Registry, Control, Options) :-
		http_websocket_server_session::serve_until_shutdown(Listener, websocket_http_websocket_session_handler, SessionHandler, Registry, Control, Options).

	wait_for_registry_session_count(Registry, ExpectedCount) :-
		wait_for_registry_session_count(Registry, ExpectedCount, 100).

	wait_for_registry_session_count(Registry, ExpectedCount, Retries) :-
		http_websocket_session_registry::session_count(Registry, Count),
		( 	Count == ExpectedCount ->
			true
		; 	Retries > 0 ->
			os::sleep(0.01),
			NextRetries is Retries - 1,
			wait_for_registry_session_count(Registry, ExpectedCount, NextRetries)
		; 	throw(timeout_error(wait_for_registry_session_count, ExpectedCount))
		).

	read_reply_frames_until_close(Seconds, Input, Frames) :-
		read_frame_result_with_timeout(Seconds, Input, Result),
		read_reply_frames_until_close_result(Result, Seconds, Input, Frames).

	read_reply_frames_until_close_result(frame(Frame), Seconds, Input, [Frame| Frames]) :-
		( 	Frame == end_of_file ->
			Frames = []
		; 	http_websocket::opcode(Frame, close) ->
			Frames = []
		; 	read_reply_frames_until_close(Seconds, Input, Frames)
		).
	read_reply_frames_until_close_result(timeout, _Seconds, _Input, []).
	read_reply_frames_until_close_result(error(Error), _Seconds, _Input, [error(Error)]).
	read_reply_frames_until_close_result(fail, _Seconds, _Input, [fail]).

	read_frame_result_with_timeout(Seconds, Input, Result) :-
		timeout::call_with_timeout(http_websocket::read_frame(Input, Frame), Seconds, TimeoutResult),
		( 	TimeoutResult == true ->
			Result = frame(Frame)
		; 	TimeoutResult == timeout ->
			Result = timeout
		; 	TimeoutResult = error(Error) ->
			Result = error(Error)
		; 	Result = fail
		).

	read_frame_with_timeout(Seconds, Input, Frame) :-
		timeout::call_with_timeout(http_websocket::read_frame(Input, Frame0), Seconds, Result),
		( 	Result == true ->
			Frame = Frame0
		; 	Result == timeout ->
			throw(timeout_error(read_frame, Seconds))
		; 	Result = error(Error) ->
			throw(Error)
		; 	throw(error(unexpected_timeout_result(Result), read_frame_with_timeout/3))
		).

	expected_session_sequence_error(error(domain_error(http_websocket_session_sequence, _Frame), _Context)).

	expected_message_text_error(error(domain_error(http_websocket_message_text, _Payload), _Context)).

	expected_websocket_opcode_error(error(domain_error(http_websocket_opcode, _Opcode), _Context)).

	expected_payload_length_limit_error(error(domain_error(http_websocket_payload_length_limit, 1), _Context)).

	write_client_close(Output, Payload) :-
		http_websocket_client_session::message(close, Payload, Message),
		http_websocket_client_session::write_message(Output, Message).

	safe_close_connection(Connection) :-
		catch(http_socket::close_connection(Connection), _, true).

	safe_close_listener(Listener) :-
		catch(http_socket::close_listener(Listener), _, true).

	safe_close_registry(Registry) :-
		catch(http_websocket_session_registry::close(Registry), _, true).

	:- endif.

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

:- end_object.


:- object(fixed_masking_key_http_websocket_client_session,
	extends(http_websocket_session(client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-24,
		comment is 'Test helper object that overrides client masking-key generation with a fixed key.'
	]).

	:- protected(generate_masking_key/1).
	:- mode(generate_masking_key(-list(byte)), one).
	:- info(generate_masking_key/1, [
		comment is 'Returns the fixed masking key used by the tests.',
		argnames is ['Key']
	]).

	generate_masking_key([1, 2, 3, 4]).

:- end_object.
