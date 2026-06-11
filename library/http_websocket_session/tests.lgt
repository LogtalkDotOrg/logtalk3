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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-06-11,
		comment is 'Unit tests for the "http_websocket_session" library.'
	]).

	cover(http_websocket_session(_, _)).
	cover(http_websocket_client_session).
	cover(http_websocket_server_session).

	setup :-
		cleanup.

	cleanup :-
		^^clean_file('test_http_websocket_session.tmp'),
		^^clean_file('test_http_websocket_session_reply.tmp').

	test(http_websocket_session_initial_state_1_01, deterministic(State == session_state(idle))) :-
		http_websocket_server_session::initial_state(State).

	test(http_websocket_session_is_state_1_01, deterministic) :-
		http_websocket_server_session::is_state(session_state(fragment(text, [[0'h, 0'e]]), close_received(status(1000)))).

	test(http_websocket_session_read_message_4_01, deterministic) :-
		http_websocket_frames::frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, State, Message),
		State == session_state(idle),
		Message == message(text, hello).

	test(http_websocket_session_read_message_4_02, deterministic) :-
		http_websocket_frames::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket_frames::frame(final, ping, [0'!], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket_frames::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
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
		http_websocket_frames::frame(more, binary, [1, 2], [], Frame1),
		http_websocket_frames::frame(final, close, [0x03, 0xE8], [], Frame2),
		http_websocket_frames::frame(final, continuation, [3, 4], [], Frame3),
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
		http_websocket_frames::frame(final, close, [0x03, 0xE8], [], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, read, Input, [type(binary)]),
		http_websocket_client_session::read_message(Input, State1, State2, Message),
		close(Input),
		State1 == session_state(idle, close_sent(status(1001))),
		State2 == session_state(idle, closed(status(1001), status(1000))),
		Message == message(close, status(1000)).

	test(http_websocket_session_read_message_4_04, error(domain_error(http_websocket_session_masking, _))) :-
		http_websocket_frames::frame(final, text, [0'h], [], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_05, error(domain_error(http_websocket_session_masking, _))) :-
		http_websocket_frames::frame(final, text, [0'h], [masking_key([1, 2, 3, 4])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_client_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_client_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_07, error(domain_error(http_websocket_session_extensions, _))) :-
		http_websocket_frames::frame(final, text, [0'h], [masking_key([1, 2, 3, 4]), reserved_bits([rsv1])], Frame),
		write_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_server_session::initial_state(State0),
		open_file_read_stateful_message('test_http_websocket_session.tmp', http_websocket_server_session, State0, _State, _Message).

	test(http_websocket_session_read_message_4_08, error(domain_error(http_websocket_session_extensions, _))) :-
		http_websocket_frames::frame(final, text, [0'h], [reserved_bits([rsv2])], Frame),
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
		http_websocket_frames::opcode(Frame, text),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		http_websocket_frames::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_2_02, deterministic) :-
		fixed_masking_key_http_websocket_client_session::message(text, hello, Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		fixed_masking_key_http_websocket_client_session::write_message(Output, Message),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_frames::opcode(Frame, text),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, [0'h, 0'e, 0'l, 0'l, 0'o]),
		http_websocket_frames::property(Frame, masking_key([1, 2, 3, 4])).

	test(http_websocket_session_write_message_3_01, deterministic) :-
		http_websocket_server_session::message(text, hello, Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_server_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2, Frame3]),
		http_websocket_frames::opcode(Frame1, text),
		http_websocket_frames::final(Frame1, more),
		http_websocket_frames::payload(Frame1, [0'h, 0'e]),
		\+ http_websocket_frames::property(Frame1, masking_key(_)),
		http_websocket_frames::opcode(Frame2, continuation),
		http_websocket_frames::final(Frame2, more),
		http_websocket_frames::payload(Frame2, [0'l, 0'l]),
		\+ http_websocket_frames::property(Frame2, masking_key(_)),
		http_websocket_frames::opcode(Frame3, continuation),
		http_websocket_frames::final(Frame3, final),
		http_websocket_frames::payload(Frame3, [0'o]),
		\+ http_websocket_frames::property(Frame3, masking_key(_)).

	test(http_websocket_session_write_message_3_02, deterministic) :-
		http_websocket_client_session::message(binary, [1, 2, 3, 4], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame1, Frame2]),
		http_websocket_frames::opcode(Frame1, binary),
		http_websocket_frames::final(Frame1, more),
		http_websocket_frames::payload(Frame1, [1, 2]),
		http_websocket_frames::property(Frame1, masking_key(_)),
		http_websocket_frames::opcode(Frame2, continuation),
		http_websocket_frames::final(Frame2, final),
		http_websocket_frames::payload(Frame2, [3, 4]),
		http_websocket_frames::property(Frame2, masking_key(_)).

	test(http_websocket_session_write_message_3_03, deterministic) :-
		http_websocket_client_session::message(ping, [0'!], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(1)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_frames::opcode(Frame, ping),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, [0'!]),
		http_websocket_frames::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_3_04, deterministic) :-
		http_websocket_server_session::message(text, '', Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_server_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_frames::opcode(Frame, text),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, []),
		\+ http_websocket_frames::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_3_05, deterministic) :-
		http_websocket_client_session::message(binary, [], Message),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, Message, [fragment_size(2)]),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		http_websocket_frames::opcode(Frame, binary),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, []),
		http_websocket_frames::property(Frame, masking_key(_)).

	test(http_websocket_session_write_message_4_01, deterministic) :-
		http_websocket_client_session::message(close, status(1000, bye), Message),
		http_websocket_client_session::initial_state(State0),
		^^file_path('test_http_websocket_session.tmp', File),
		open(File, write, Output, [type(binary)]),
		http_websocket_client_session::write_message(Output, State0, State, Message),
		close(Output),
		read_frames_file('test_http_websocket_session.tmp', [Frame]),
		State == session_state(idle, close_sent(status(1000, bye))),
		http_websocket_frames::opcode(Frame, close),
		http_websocket_frames::final(Frame, final),
		http_websocket_frames::payload(Frame, [0x03, 0xE8, 0'b, 0'y, 0'e]),
		http_websocket_frames::property(Frame, masking_key(_)).

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
		http_websocket_frames::frame(final, close, [0x03, 0xE8, 0'b, 0'y, 0'e], [masking_key([1, 2, 3, 4])], Frame),
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
		http_websocket_frames::opcode(ReplyFrame, close),
		http_websocket_frames::final(ReplyFrame, final),
		http_websocket_frames::payload(ReplyFrame, [0x03, 0xE8, 0'b, 0'y, 0'e]),
		\+ http_websocket_frames::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_5_02, deterministic) :-
		http_websocket_frames::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket_frames::frame(final, close, [0x03, 0xE8], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket_frames::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
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
		http_websocket_frames::opcode(ReplyFrame, close),
		http_websocket_frames::final(ReplyFrame, final),
		http_websocket_frames::payload(ReplyFrame, [0x03, 0xE8]),
		\+ http_websocket_frames::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_6_01, deterministic) :-
		http_websocket_frames::frame(more, text, [0'h, 0'e], [masking_key([1, 2, 3, 4])], Frame1),
		http_websocket_frames::frame(final, ping, [0'!], [masking_key([5, 6, 7, 8])], Frame2),
		http_websocket_frames::frame(final, continuation, [0'l, 0'l, 0'o], [masking_key([9, 10, 11, 12])], Frame3),
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
		http_websocket_frames::opcode(ReplyFrame, pong),
		http_websocket_frames::final(ReplyFrame, final),
		http_websocket_frames::payload(ReplyFrame, [0'!]),
		\+ http_websocket_frames::property(ReplyFrame, masking_key(_)).

	test(http_websocket_session_read_message_6_02, error(domain_error(http_websocket_payload_length_limit, 1))) :-
		http_websocket_frames::frame(final, text, [0'h], [masking_key([1, 2, 3, 4])], Frame),
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

	% auxiliary predicates

	write_frames_file(Path, Frames) :-
		^^file_path(Path, File),
		open(File, write, Output, [type(binary)]),
		write_frames(Frames, Output),
		close(Output).

	write_frames([], _Output).
	write_frames([Frame| Frames], Output) :-
		http_websocket_frames::write_frame(Output, Frame),
		write_frames(Frames, Output).

	read_frames_file(Path, Frames) :-
		^^file_path(Path, File),
		open(File, read, Input, [type(binary)]),
		read_frames(Input, Frames),
		close(Input).

	read_frames(Input, Frames) :-
		http_websocket_frames::read_frame(Input, Frame),
		(	Frame == end_of_file ->
			Frames = []
		;	Frames = [Frame| Rest],
			read_frames(Input, Rest)
		).

	open_file_read_stateful_message(Path, Session, State0, State, Message) :-
		^^file_path(Path, File),
		open(File, read, Input, [type(binary)]),
		Session::read_message(Input, State0, State, Message),
		close(Input).

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
