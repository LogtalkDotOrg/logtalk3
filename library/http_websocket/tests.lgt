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
		comment is 'Unit tests for the "http_websocket" library.'
	]).

	:- uses(http_websocket, [
		frame/5,
		is_frame/1,
		parse/2,
		generate/2,
		read_frame/2,
		write_frame/2,
		final/2,
		opcode/2,
		payload/2,
		properties/2,
		property/2
	]).

	cover(http_websocket).

	cleanup :-
		^^clean_file('test_http_websocket_frame.tmp').

	test(http_websocket_frame_5_01, deterministic(Properties == [reserved_bits([rsv1, rsv3]), masking_key([1, 2, 3, 4])])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [masking_key([1, 2, 3, 4]), reserved_bits([rsv3, rsv1])], Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		properties(Frame, Properties),
		property(Frame, reserved_bits([rsv1, rsv3])),
		property(Frame, masking_key([1, 2, 3, 4])).

	test(http_websocket_is_frame_1_01, fail) :-
		is_frame(frame(more, ping, [], [])).

	test(http_websocket_parse_2_01, true(Frame == end_of_file)) :-
		parse(bytes([]), Frame).

	test(http_websocket_parse_2_02, deterministic) :-
		parse(bytes([0x81, 0x05, 0'H, 0'e, 0'l, 0'l, 0'o]), Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		properties(Frame, []).

	test(http_websocket_parse_2_03, deterministic) :-
		parse(bytes([0x81, 0x85, 0x37, 0xFA, 0x21, 0x3D, 0x7F, 0x9F, 0x4D, 0x51, 0x58]), Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		property(Frame, masking_key([0x37, 0xFA, 0x21, 0x3D])).

	test(http_websocket_generate_2_01, true(Bytes == [0x81, 0x05, 0'H, 0'e, 0'l, 0'l, 0'o])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [], Frame),
		generate(bytes(Bytes), Frame).

	test(http_websocket_generate_2_02, true(Bytes == [0x81, 0x85, 0x37, 0xFA, 0x21, 0x3D, 0x7F, 0x9F, 0x4D, 0x51, 0x58])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [masking_key([0x37, 0xFA, 0x21, 0x3D])], Frame),
		generate(bytes(Bytes), Frame).

	test(http_websocket_roundtrip_2_01, deterministic) :-
		repeated_byte_list(126, 0xAA, PayloadBytes),
		frame(final, binary, PayloadBytes, [reserved_bits([rsv1])], Frame),
		generate(bytes(Bytes), Frame),
		Bytes = [0xC2, 0x7E, 0x00, 0x7E| _],
		parse(bytes(Bytes), ParsedFrame),
		opcode(ParsedFrame, binary),
		payload(ParsedFrame, PayloadBytes),
		property(ParsedFrame, reserved_bits([rsv1])).

	test(http_websocket_roundtrip_2_02, deterministic) :-
		repeated_byte_list(65536, 0x55, PayloadBytes),
		frame(final, binary, PayloadBytes, [], Frame),
		generate(bytes(Bytes), Frame),
		Bytes = [0x82, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00| _],
		parse(bytes(Bytes), ParsedFrame),
		opcode(ParsedFrame, binary),
		payload(ParsedFrame, PayloadBytes).

	test(http_websocket_write_read_frame_2_01, deterministic) :-
		frame(final, pong, [0'p, 0'o, 0'n, 0'g], [], Frame),
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_frame(Output, Frame),
		close(Output),
		open(File, read, Input, [type(binary)]),
		read_frame(Input, ParsedFrame),
		close(Input),
		ParsedFrame == Frame.

	test(http_websocket_frame_5_02, error(domain_error(http_websocket_frame, _))) :-
		frame(final, close, [0x03], [], _Frame).

	test(http_websocket_frame_5_03, error(domain_error(http_websocket_frame, _))) :-
		frame(more, ping, [], [], _Frame).

	test(http_websocket_parse_2_04, error(domain_error(http_websocket_frame, _))) :-
		parse(bytes([0x88, 0x01, 0x03]), _Frame).

	repeated_byte_list(Length, Byte, Bytes) :-
		repeated_byte_list(Length, Byte, Bytes, []).

	repeated_byte_list(0, _Byte, Bytes, Bytes) :-
		!.
	repeated_byte_list(Length, Byte, [Byte| Bytes0], Bytes) :-
		Length > 0,
		NextLength is Length - 1,
		repeated_byte_list(NextLength, Byte, Bytes0, Bytes).

:- end_object.
