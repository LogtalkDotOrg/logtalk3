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
		date is 2026-06-11,
		comment is 'Unit tests for the "http_websocket_messages" library.'
	]).

	:- uses(http_websocket_messages, [
		message/3, is_message/1, read_message/2, write_message/2, type/2, payload/2
	]).

	cover(http_websocket_messages(_)).
	cover(http_websocket_messages).

	cleanup :-
		^^clean_file('test_http_websocket_messages.tmp').

	test(http_websocket_messages_message_3_01, true(Message == message(text, hello))) :-
		message(text, hello, Message).

	test(http_websocket_messages_message_3_02, true(Message == message(text, codes([0'H, 0'i])))) :-
		http_websocket_messages(codes)::message(text, codes([0'H, 0'i]), Message).

	test(http_websocket_messages_is_message_1_01, fail) :-
		is_message(message(close, status(1005))).

	test(http_websocket_messages_read_message_2_01, deterministic) :-
		http_websocket_frames::frame(final, text, [0'h, 0'e, 0'l, 0'l, 0'o], [], Frame),
		write_frames_file('test_http_websocket_messages.tmp', [Frame]),
		open_file_read_message('test_http_websocket_messages.tmp', Message),
		type(Message, text),
		payload(Message, hello).

	test(http_websocket_messages_read_message_2_02, true(Message == message(text, hello))) :-
		http_websocket_frames::frame(more, text, [0'h, 0'e], [], Frame1),
		http_websocket_frames::frame(final, continuation, [0'l, 0'l, 0'o], [], Frame2),
		write_frames_file('test_http_websocket_messages.tmp', [Frame1, Frame2]),
		open_file_read_message('test_http_websocket_messages.tmp', Message).

	test(http_websocket_messages_read_message_2_03, true(Message == message(binary, [1, 2, 3, 4]))) :-
		http_websocket_frames::frame(more, binary, [1, 2], [], Frame1),
		http_websocket_frames::frame(final, continuation, [3, 4], [], Frame2),
		write_frames_file('test_http_websocket_messages.tmp', [Frame1, Frame2]),
		open_file_read_message('test_http_websocket_messages.tmp', Message).

	test(http_websocket_messages_read_message_2_04, true(Message == message(close, status(1000, bye)))) :-
		http_websocket_frames::frame(final, close, [0x03, 0xE8, 0'b, 0'y, 0'e], [], Frame),
		write_frames_file('test_http_websocket_messages.tmp', [Frame]),
		open_file_read_message('test_http_websocket_messages.tmp', Message).

	test(http_websocket_messages_read_message_2_05, error(domain_error(http_websocket_message_sequence, _))) :-
		http_websocket_frames::frame(more, text, [0'H], [], Frame1),
		http_websocket_frames::frame(final, ping, [0'!], [], Frame2),
		write_frames_file('test_http_websocket_messages.tmp', [Frame1, Frame2]),
		open_file_read_message('test_http_websocket_messages.tmp', _Message).

	test(http_websocket_messages_read_message_2_06, error(domain_error(http_websocket_message_text, [0xC3, 0x28]))) :-
		http_websocket_frames::frame(final, text, [0xC3, 0x28], [], Frame),
		write_frames_file('test_http_websocket_messages.tmp', [Frame]),
		open_file_read_message('test_http_websocket_messages.tmp', _Message).

	test(http_websocket_messages_write_message_2_01, deterministic) :-
		message(text, hello, Message),
		^^file_path('test_http_websocket_messages.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_message(Output, Message),
		close(Output),
		open(File, read, Input, [type(binary)]),
		http_websocket_frames::read_frame(Input, Frame),
		close(Input),
		http_websocket_frames::opcode(Frame, text),
		http_websocket_frames::payload(Frame, [0'h, 0'e, 0'l, 0'l, 0'o]).

	test(http_websocket_messages_write_message_2_02, true(PayloadBytes == [0x03, 0xE8, 0'b, 0'y, 0'e])) :-
		message(close, status(1000, bye), Message),
		^^file_path('test_http_websocket_messages.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_message(Output, Message),
		close(Output),
		open(File, read, Input, [type(binary)]),
		http_websocket_frames::read_frame(Input, Frame),
		close(Input),
		http_websocket_frames::opcode(Frame, close),
		http_websocket_frames::payload(Frame, PayloadBytes).

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

	open_file_read_message(Path, Message) :-
		^^file_path(Path, File),
		open(File, read, Input, [type(binary)]),
		read_message(Input, Message),
		close(Input).

:- end_object.
