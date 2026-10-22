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
		date is 2026-10-22,
		comment is 'Unit tests for the "http_websocket_frames" library.'
	]).

	:- uses(http_websocket_frames, [
		frame/5, is_frame/1, parse/2, generate/2, read_frame/2, read_frame/3, write_frame/2, final/2,
		opcode/2, payload/2, properties/2, property/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		append/3, member/2
	]).

	cover(http_websocket_frames).

	cleanup :-
		^^clean_file('test_http_websocket_frame.tmp').

	test(http_websocket_frames_frame_5_01, deterministic(Properties == [reserved_bits([rsv1, rsv3]), masking_key([1, 2, 3, 4])])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [masking_key([1, 2, 3, 4]), reserved_bits([rsv3, rsv1])], Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		properties(Frame, Properties),
		property(Frame, reserved_bits([rsv1, rsv3])),
		property(Frame, masking_key([1, 2, 3, 4])).

	test(http_websocket_frames_is_frame_1_01, fail) :-
		is_frame(frame(more, ping, [], [])).

	test(http_websocket_frames_parse_2_01, true(Frame == end_of_file)) :-
		parse(bytes([]), Frame).

	test(http_websocket_frames_parse_2_02, deterministic) :-
		parse(bytes([0x81, 0x05, 0'H, 0'e, 0'l, 0'l, 0'o]), Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		properties(Frame, []).

	test(http_websocket_frames_parse_2_03, deterministic) :-
		parse(bytes([0x81, 0x85, 0x37, 0xFA, 0x21, 0x3D, 0x7F, 0x9F, 0x4D, 0x51, 0x58]), Frame),
		final(Frame, final),
		opcode(Frame, text),
		payload(Frame, [0'H, 0'e, 0'l, 0'l, 0'o]),
		property(Frame, masking_key([0x37, 0xFA, 0x21, 0x3D])).

	test(http_websocket_frames_generate_2_01, true(Bytes == [0x81, 0x05, 0'H, 0'e, 0'l, 0'l, 0'o])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [], Frame),
		generate(bytes(Bytes), Frame).

	test(http_websocket_frames_generate_2_02, true(Bytes == [0x81, 0x85, 0x37, 0xFA, 0x21, 0x3D, 0x7F, 0x9F, 0x4D, 0x51, 0x58])) :-
		frame(final, text, [0'H, 0'e, 0'l, 0'l, 0'o], [masking_key([0x37, 0xFA, 0x21, 0x3D])], Frame),
		generate(bytes(Bytes), Frame).

	test(http_websocket_frames_generate_raw_frame, deterministic(Bytes == [0xD2, 0x83, 1, 2, 3, 4, 1, 125, 252])) :-
		generate(bytes(Bytes), frame(final, binary, [0, 127, 255], [masking_key([1, 2, 3, 4]), reserved_bits([rsv3, rsv1])])).

	test(http_websocket_frames_generate_payload_before_sink, error(domain_error(http_websocket_payload, [0, 255, 256]))) :-
		generate(invalid, frame(final, binary, [0, 255, 256], [])).

	test(http_websocket_frames_generate_payload_before_properties, error(domain_error(http_websocket_payload, [0| invalid]))) :-
		generate(bytes(_Bytes), frame(final, binary, [0| invalid], [invalid])).

	test(http_websocket_frames_generate_properties_before_semantics, error(domain_error(http_websocket_property, invalid))) :-
		generate(bytes(_Bytes), frame(more, ping, [], [invalid])).

	test(http_websocket_frames_generate_variable_payload, error(instantiation_error)) :-
		generate(bytes(_Bytes), frame(final, binary, _Payload, [])).

	test(http_websocket_frames_write_variable_stream_before_frame, error(instantiation_error)) :-
		write_frame(_Stream, invalid).

	test(http_websocket_frames_generate_empty_payload, deterministic(Bytes == [0x82, 0])) :-
		generate(bytes(Bytes), frame(final, binary, [], [])).

	test(http_websocket_frames_control_payload_125, all(ParsedFrame == Frame)) :-
		repeated_byte_list(125, 85, Payload),
		member(Opcode, [ping, pong]),
		frame(final, Opcode, Payload, [], Frame),
		generate(bytes(Bytes), Frame),
		parse(bytes(Bytes), ParsedFrame).

	test(http_websocket_frames_construct_control_payload_126, error(domain_error(http_websocket_frame, _))) :-
		repeated_byte_list(126, 85, Payload),
		frame(final, ping, Payload, [], _Frame).

	test(http_websocket_frames_generate_control_payload_126, error(domain_error(http_websocket_frame, _))) :-
		repeated_byte_list(126, 85, Payload),
		generate(bytes(_Bytes), frame(final, pong, Payload, [])).

	test(http_websocket_frames_generate_close_payloads, all(ParsedFrame == frame(final, close, Payload, []))) :-
		member(Payload, [[], [3, 232]]),
		generate(bytes(Bytes), frame(final, close, Payload, [])),
		parse(bytes(Bytes), ParsedFrame).

	test(http_websocket_frames_generate_close_payload_125, deterministic(Bytes == [0x88, 125, 3, 232| Reason])) :-
		repeated_byte_list(123, 85, Reason),
		generate(bytes(Bytes), frame(final, close, [3, 232| Reason], [])).

	test(http_websocket_frames_generate_close_payload_1, error(domain_error(http_websocket_frame, _))) :-
		generate(bytes(_Bytes), frame(final, close, [3], [])).

	test(http_websocket_frames_roundtrip_2_01, deterministic) :-
		repeated_byte_list(126, 0xAA, PayloadBytes),
		frame(final, binary, PayloadBytes, [reserved_bits([rsv1])], Frame),
		generate(bytes(Bytes), Frame),
		Bytes = [0xC2, 0x7E, 0x00, 0x7E| _],
		parse(bytes(Bytes), ParsedFrame),
		opcode(ParsedFrame, binary),
		payload(ParsedFrame, PayloadBytes),
		property(ParsedFrame, reserved_bits([rsv1])).

	test(http_websocket_frames_roundtrip_2_02, deterministic) :-
		repeated_byte_list(65536, 0x55, PayloadBytes),
		frame(final, binary, PayloadBytes, [], Frame),
		generate(bytes(Bytes), Frame),
		Bytes = [0x82, 0x7F, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00| _],
		parse(bytes(Bytes), ParsedFrame),
		opcode(ParsedFrame, binary),
		payload(ParsedFrame, PayloadBytes).

	test(http_websocket_frames_payload_length_boundaries, all(check_payload_boundary(Length, Header))) :-
		member(Length-Header, [
			0-[0x82, 0],
			1-[0x82, 1],
			125-[0x82, 125],
			126-[0x82, 126, 0, 126],
			127-[0x82, 126, 0, 127],
			65535-[0x82, 126, 255, 255]
		]).

	test(http_websocket_frames_masked_empty_payload, deterministic) :-
		Frame = frame(final, binary, [], [masking_key([1, 2, 3, 4])]),
		generate(bytes(Bytes), Frame),
		assertion(Bytes == [0x82, 0x80, 1, 2, 3, 4]),
		parse(bytes([0x82, 0x80, 1, 2, 3, 4]), ParsedFrame),
		assertion(ParsedFrame == Frame).

	test(http_websocket_frames_masked_payload_key_cycle, deterministic) :-
		Frame = frame(final, binary, [0, 127, 128, 255, 1, 2, 3], [masking_key([1, 2, 3, 4])]),
		ExpectedBytes = [0x82, 0x87, 1, 2, 3, 4, 1, 125, 131, 251, 0, 0, 0],
		generate(bytes(Bytes), Frame),
		assertion(Bytes == ExpectedBytes),
		parse(bytes(ExpectedBytes), ParsedFrame),
		assertion(ParsedFrame == Frame).

	test(http_websocket_frames_reject_malformed_byte_sequences, all(reject_byte_sequence(Bytes))) :-
		member(Bytes, [
			[0x82],
			[0x82, 126],
			[0x82, 126, 0],
			[0x82, 127, 0, 0, 0, 0, 0, 1, 0],
			[0x82, 0x80, 1, 2, 3],
			[0x82, 2, 85],
			[0x82, 126, 0, 126, 85],
			[0x82, 127, 0, 0, 0, 0, 0, 1, 0, 0],
			[0x82, 0, 85],
			[0x82, 126, 0, 125],
			[0x82, 127, 0, 0, 0, 0, 0, 0, 0, 126]
		]).

	test(http_websocket_frames_semantics_before_trailing_bytes, error(domain_error(http_websocket_frame, _))) :-
		parse(bytes([0x88, 1, 3, 85]), _Frame).

	test(http_websocket_frames_write_read_frame_2_01, deterministic(ParsedFrame == Frame)) :-
		frame(final, pong, [0'p, 0'o, 0'n, 0'g], [], Frame),
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_frame(Output, Frame),
		close(Output),
		open(File, read, Input, [type(binary)]),
		read_frame(Input, ParsedFrame),
		close(Input).

	test(http_websocket_frames_write_raw_frame, deterministic(ParsedFrame == frame(final, binary, [0, 127, 255], [reserved_bits([rsv1, rsv3]), masking_key([1, 2, 3, 4])]))) :-
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		catch(
			write_frame(Output, frame(final, binary, [0, 127, 255], [masking_key([1, 2, 3, 4]), reserved_bits([rsv3, rsv1])])),
			Error,
			( 	close(Output),
				throw(Error)
			)
		),
		close(Output),
		open(File, read, Input, [type(binary)]),
		catch(
			read_frame(Input, ParsedFrame),
			ReadError,
			( 	close(Input),
				throw(ReadError)
			)
		),
		close(Input).

	test(http_websocket_frames_read_frame_3_01, error(domain_error(http_websocket_payload_length_limit, 1))) :-
		frame(final, text, [0'h], [], Frame),
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_frame(Output, Frame),
		close(Output),
		open(File, read, Input, [type(binary)]),
		catch(read_frame(Input, _ParsedFrame, [max_payload_length(0)]), Error, true),
		close(Input),
		throw(Error).

	test(http_websocket_frames_read_frame_3_02, error(domain_error(option, max_payload_length(_)))) :-
		frame(final, text, [0'h], [], Frame),
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		write_frame(Output, Frame),
		close(Output),
		open(File, read, Input, [type(binary)]),
		catch(read_frame(Input, _ParsedFrame, [max_payload_length(_)]), Error, true),
		close(Input),
		throw(Error).

	test(http_websocket_frames_frame_5_02, error(domain_error(http_websocket_frame, _))) :-
		frame(final, close, [0x03], [], _Frame).

	test(http_websocket_frames_frame_5_03, error(domain_error(http_websocket_frame, _))) :-
		frame(more, ping, [], [], _Frame).

	test(http_websocket_frames_parse_2_04, error(domain_error(http_websocket_frame, _))) :-
		parse(bytes([0x88, 0x01, 0x03]), _Frame).

	test(http_websocket_frames_parse_2_05, error(domain_error(http_websocket_opcode, 3))) :-
		parse(bytes([0x83, 0x00]), _Frame).

	test(http_websocket_frames_read_frame_2_02, error(domain_error(http_websocket_opcode, 3))) :-
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		put_byte(Output, 0x83),
		put_byte(Output, 0x00),
		close(Output),
		open(File, read, Input, [type(binary)]),
		catch(read_frame(Input, _ParsedFrame), Error, true),
		close(Input),
		throw(Error).

	test(http_websocket_frames_read_consecutive_frames, deterministic) :-
		First = frame(final, binary, [0, 127, 255], [masking_key([1, 2, 3, 4])]),
		Second = frame(final, pong, [], []),
		^^file_path('test_http_websocket_frame.tmp', File),
		open(File, write, Output, [type(binary)]),
		catch(
			( 	write_frame(Output, First),
				write_frame(Output, Second)
			),
			WriteError,
			( 	close(Output),
				throw(WriteError)
			)
		),
		close(Output),
		open(File, read, Input, [type(binary)]),
		catch(
			( 	read_frame(Input, ParsedFirst),
				read_frame(Input, ParsedSecond),
				read_frame(Input, End)
			),
			ReadError,
			( 	close(Input),
				throw(ReadError)
			)
		),
		close(Input),
		assertion(ParsedFirst == First),
		assertion(ParsedSecond == Second),
		assertion(End == end_of_file).

	% auxiliary predicates

	check_payload_boundary(Length, Header) :-
		repeated_byte_list(Length, 85, Payload),
		append(Header, Payload, ExpectedBytes),
		generate(bytes(Bytes), frame(final, binary, Payload, [])),
		assertion(Bytes == ExpectedBytes),
		parse(bytes(ExpectedBytes), ParsedFrame),
		assertion(ParsedFrame == frame(final, binary, Payload, [])).

	reject_byte_sequence(Bytes) :-
		catch(
			( 	parse(bytes(Bytes), _Frame),
				fail
			),
			error(domain_error(http_websocket_byte_sequence, Bytes), _Context),
			true
		).

	repeated_byte_list(0, _Byte, []) :-
		!.
	repeated_byte_list(Length, Byte, [Byte| Bytes]) :-
		Length > 0,
		NextLength is Length - 1,
		repeated_byte_list(NextLength, Byte, Bytes).

:- end_object.
