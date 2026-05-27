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


:- object(http_websocket,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Transport-neutral WebSocket frame predicates for constructing, parsing, generating, reading, and writing normalized frame terms.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(reader, [
		file_to_bytes/2
	]).

	:- public(frame/5).
	:- mode(frame(+atom, +atom, +list(byte), +list(compound), -compound), one_or_error).
	:- info(frame/5, [
		comment is 'Constructs a validated normalized WebSocket frame term from the fragmentation state atom, opcode atom, payload bytes, and frame properties.',
		argnames is ['Final', 'Opcode', 'Payload', 'Properties', 'Frame'],
		remarks is [
			'Final atoms' - 'Use ``final`` for FIN=1 and ``more`` for FIN=0.',
			'Opcode atoms' - 'Supported opcodes are ``continuation``, ``text``, ``binary``, ``close``, ``ping``, and ``pong``.',
			'Property ``masking_key(Key)``' - 'Requests masking on the wire using a list of four mask bytes.',
			'Property ``reserved_bits(Bits)``' - 'Preserves reserved bits as an ordered list containing any of ``rsv1``, ``rsv2``, and ``rsv3``.'
		]
	]).

	:- public(is_frame/1).
	:- mode(is_frame(@term), zero_or_one).
	:- info(is_frame/1, [
		comment is 'True when the argument is a valid normalized WebSocket frame term.',
		argnames is ['Frame']
	]).

	:- public(parse/2).
	:- mode(parse(++compound, -term), one_or_error).
	:- info(parse/2, [
		comment is 'Parses one WebSocket frame from a source term. Supported sources are ``bytes(Bytes)``, ``codes(Bytes)``, ``file(File)``, and ``stream(Stream)``. Empty sources return ``end_of_file``.',
		argnames is ['Source', 'Frame']
	]).

	:- public(generate/2).
	:- mode(generate(++compound, +compound), one_or_error).
	:- info(generate/2, [
		comment is 'Generates one validated WebSocket frame to a sink term. Supported sinks are ``bytes(Bytes)``, ``codes(Bytes)``, ``file(File)``, and ``stream(Stream)``.',
		argnames is ['Sink', 'Frame']
	]).

	:- public(read_frame/2).
	:- mode(read_frame(+stream_or_alias, -term), one_or_error).
	:- info(read_frame/2, [
		comment is 'Reads one WebSocket frame from a binary stream without requiring end-of-file. Returns ``end_of_file`` when the stream is already exhausted.',
		argnames is ['Stream', 'Frame']
	]).

	:- public(read_frame/3).
	:- mode(read_frame(+stream_or_alias, -term, +list), one_or_error).
	:- info(read_frame/3, [
		comment is 'Reads one WebSocket frame from a binary stream using the given read options. Returns ``end_of_file`` when the stream is already exhausted.',
		argnames is ['Stream', 'Frame', 'Options'],
		remarks is [
			'Repeated options' - 'When the same read option is given multiple times, the first occurrence is used.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Use a non-negative integer.'
		]
	]).

	:- public(write_frame/2).
	:- mode(write_frame(+stream_or_alias, +compound), one_or_error).
	:- info(write_frame/2, [
		comment is 'Writes one validated WebSocket frame to a binary stream and flushes the stream.',
		argnames is ['Stream', 'Frame']
	]).

	:- public(final/2).
	:- mode(final(+compound, -atom), one_or_error).
	:- info(final/2, [
		comment is 'Returns the fragmentation state atom of a validated WebSocket frame term.',
		argnames is ['Frame', 'Final']
	]).

	:- public(opcode/2).
	:- mode(opcode(+compound, -atom), one_or_error).
	:- info(opcode/2, [
		comment is 'Returns the opcode atom of a validated WebSocket frame term.',
		argnames is ['Frame', 'Opcode']
	]).

	:- public(payload/2).
	:- mode(payload(+compound, -list(byte)), one_or_error).
	:- info(payload/2, [
		comment is 'Returns the payload byte list of a validated WebSocket frame term.',
		argnames is ['Frame', 'Payload']
	]).

	:- public(properties/2).
	:- mode(properties(+compound, -list(compound)), one_or_error).
	:- info(properties/2, [
		comment is 'Returns the normalized property list of a validated WebSocket frame term.',
		argnames is ['Frame', 'Properties']
	]).

	:- public(property/2).
	:- mode(property(+compound, ?compound), zero_or_one_or_error).
	:- info(property/2, [
		comment is 'True when the validated WebSocket frame term contains the given property. When the property argument is a variable, properties can be enumerated on backtracking.',
		argnames is ['Frame', 'Property']
	]).

	valid_option(max_payload_length(MaxPayloadLength)) :-
		nonvar(MaxPayloadLength),
		(	MaxPayloadLength == none ->
			true
		;	integer(MaxPayloadLength),
			MaxPayloadLength >= 0
		).

	default_option(max_payload_length(none)).

	frame(Final, Opcode, Payload, Properties0, Frame) :-
		normalize_frame(frame(Final, Opcode, Payload, Properties0), Frame, _MaskingKey, _ReservedBits).

	is_frame(Frame) :-
		catch(normalize_frame(Frame, _NormalizedFrame, _MaskingKey, _ReservedBits), _, fail).

	parse(Source, Frame) :-
		(	var(Source) ->
			instantiation_error
		;	Source = file(File) ->
			file_to_bytes(File, Bytes),
			parse_bytes(Bytes, Frame)
		;	Source = stream(Stream) ->
			read_frame(Stream, Frame)
		;	Source = bytes(Bytes) ->
			parse_bytes(Bytes, Frame)
		;	Source = codes(Bytes) ->
			parse_bytes(Bytes, Frame)
		;	domain_error(http_websocket_source, Source)
		).

	generate(Sink, Frame) :-
		normalize_frame(Frame, NormalizedFrame, _MaskingKey, _ReservedBits),
		phrase(encode_frame(NormalizedFrame), Bytes),
		bytes_to_sink(Sink, Bytes).

	read_frame(Stream, Frame) :-
		read_frame(Stream, Frame, []).

	read_frame(Stream, Frame, Options) :-
		(	var(Stream) ->
			instantiation_error
		;	parse_read_frame_options(Options, MaxPayloadLength),
			get_byte(Stream, Byte0),
			(	Byte0 =:= -1 ->
				Frame = end_of_file
			;	read_required_byte(Stream, Byte1),
				decode_first_byte(Byte0, Final, Opcode, ReservedBits),
				MaskFlag is (Byte1 >> 7) /\ 0x01,
				LengthCode is Byte1 /\ 0x7F,
				read_payload_length(Stream, LengthCode, PayloadLength),
				validate_payload_length_limit(MaxPayloadLength, PayloadLength),
				read_masking_key(Stream, MaskFlag, MaskingKey),
				read_required_bytes(PayloadLength, Stream, PayloadBytes0),
				mask_or_unmask_payload(PayloadBytes0, MaskingKey, PayloadBytes),
				properties_from_components(ReservedBits, MaskingKey, Properties),
				frame(Final, Opcode, PayloadBytes, Properties, Frame)
			)
		).

	parse_read_frame_options(Options, MaxPayloadLength) :-
		^^check_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(max_payload_length(MaxPayloadLength), MergedOptions).

	validate_payload_length_limit(none, _PayloadLength) :-
		!.
	validate_payload_length_limit(MaxPayloadLength, PayloadLength) :-
		PayloadLength =< MaxPayloadLength,
		!.
	validate_payload_length_limit(_MaxPayloadLength, PayloadLength) :-
		domain_error(http_websocket_payload_length_limit, PayloadLength).

	write_frame(Stream, Frame) :-
		(	var(Stream) ->
			instantiation_error
		;	normalize_frame(Frame, NormalizedFrame, _MaskingKey, _ReservedBits),
			phrase(encode_frame(NormalizedFrame), Bytes),
			write_bytes(Bytes, Stream),
			flush_output(Stream)
		).

	final(Frame, Final) :-
		normalize_frame(Frame, frame(Final, _Opcode, _Payload, _Properties), _MaskingKey, _ReservedBits).

	opcode(Frame, Opcode) :-
		normalize_frame(Frame, frame(_Final, Opcode, _Payload, _Properties), _MaskingKey, _ReservedBits).

	payload(Frame, Payload) :-
		normalize_frame(Frame, frame(_Final, _Opcode, Payload, _Properties), _MaskingKey, _ReservedBits).

	properties(Frame, Properties) :-
		normalize_frame(Frame, frame(_Final, _Opcode, _Payload, Properties), _MaskingKey, _ReservedBits).

	property(Frame, Property) :-
		normalize_frame(Frame, frame(_Final, _Opcode, _Payload, Properties), _MaskingKey, _ReservedBits),
		(	var(Property) ->
			member(Property, Properties)
		;	memberchk(Property, Properties)
		).

	parse_bytes(Bytes, Frame) :-
		(	valid_byte_list(Bytes) ->
			parse_valid_bytes(Bytes, Frame)
		;	domain_error(http_websocket_byte_sequence, Bytes)
		).

	parse_valid_bytes([], end_of_file) :-
		!.
	parse_valid_bytes(Bytes, Frame) :-
		phrase(decode_frame(Frame), Bytes),
		!.
	parse_valid_bytes(Bytes, _Frame) :-
		domain_error(http_websocket_byte_sequence, Bytes).

	normalize_frame(Frame0, Frame, MaskingKey, ReservedBits) :-
		(	var(Frame0) ->
			instantiation_error
		;	Frame0 = frame(Final, Opcode, Payload, Properties0) ->
			validate_final(Final),
			validate_opcode(Opcode),
			validate_payload(Payload),
			normalize_properties(Properties0, Properties, MaskingKey, ReservedBits),
			validate_frame_semantics(Final, Opcode, Payload),
			Frame = frame(Final, Opcode, Payload, Properties)
		;	domain_error(http_websocket_frame, Frame0)
		).

	validate_final(final) :-
		!.
	validate_final(more) :-
		!.
	validate_final(Final) :-
		domain_error(http_websocket_final, Final).

	validate_opcode(continuation) :-
		!.
	validate_opcode(text) :-
		!.
	validate_opcode(binary) :-
		!.
	validate_opcode(close) :-
		!.
	validate_opcode(ping) :-
		!.
	validate_opcode(pong) :-
		!.
	validate_opcode(Opcode) :-
		domain_error(http_websocket_opcode, Opcode).

	validate_payload(Payload) :-
		(	var(Payload) ->
			instantiation_error
		;	valid_byte_list(Payload) ->
			true
		;	domain_error(http_websocket_payload, Payload)
		).

	normalize_properties(Properties0, Properties, MaskingKey, ReservedBits) :-
		(	var(Properties0) ->
			instantiation_error
		;	extract_properties(Properties0, no, [], MaskingKey, ReservedBits),
			properties_from_components(ReservedBits, MaskingKey, Properties)
		).

	extract_properties([], MaskingKey, ReservedBits, MaskingKey, ReservedBits) :-
		!.
	extract_properties([Property| Properties], MaskingKey0, ReservedBits0, MaskingKey, ReservedBits) :-
		!,
		parse_property(Property, MaskingKey0, ReservedBits0, MaskingKey1, ReservedBits1),
				extract_properties(Properties, MaskingKey1, ReservedBits1, MaskingKey, ReservedBits).
	extract_properties(Properties, _MaskingKey0, _ReservedBits0, _MaskingKey, _ReservedBits) :-
		domain_error(http_websocket_properties, Properties).

	parse_property(masking_key(Key), no, ReservedBits, Key, ReservedBits) :-
		!,
		validate_masking_key(Key).
	parse_property(masking_key(_Key), _MaskingKey, _ReservedBits, _NewMaskingKey, _NewReservedBits) :-
		!,
		domain_error(http_websocket_property, masking_key).
	parse_property(reserved_bits(Bits0), MaskingKey, [], MaskingKey, Bits) :-
		!,
		validate_reserved_bits(Bits0, Bits).
	parse_property(reserved_bits(_Bits), _MaskingKey, _ReservedBits, _NewMaskingKey, _NewReservedBits) :-
		!,
		domain_error(http_websocket_property, reserved_bits).
	parse_property(Property, _MaskingKey, _ReservedBits, _NewMaskingKey, _NewReservedBits) :-
		domain_error(http_websocket_property, Property).

	validate_masking_key(Key) :-
		(	Key = [Byte0, Byte1, Byte2, Byte3],
			valid_byte(Byte0),
			valid_byte(Byte1),
			valid_byte(Byte2),
			valid_byte(Byte3) ->
			true
		;	domain_error(http_websocket_masking_key, Key)
		).

	validate_reserved_bits(Bits0, Bits) :-
		(	var(Bits0) ->
			instantiation_error
		;	validate_reserved_bits_list(Bits0, []),
			canonical_reserved_bits(Bits0, Bits)
		).

	validate_reserved_bits_list([], _Seen) :-
		!.
	validate_reserved_bits_list([Bit| Bits], Seen) :-
		!,
		valid_reserved_bit(Bit),
		(	memberchk(Bit, Seen) ->
			domain_error(http_websocket_reserved_bits, [Bit| Bits])
		;	validate_reserved_bits_list(Bits, [Bit| Seen])
		).
	validate_reserved_bits_list(Bits, _Seen) :-
		domain_error(http_websocket_reserved_bits, Bits).

	valid_reserved_bit(rsv1) :-
		!.
	valid_reserved_bit(rsv2) :-
		!.
	valid_reserved_bit(rsv3).

	canonical_reserved_bits(Bits0, Bits) :-
		(	memberchk(rsv1, Bits0) ->
			Bits = [rsv1| Bits1]
		;	Bits = Bits1
		),
		(	memberchk(rsv2, Bits0) ->
			Bits1 = [rsv2| Bits2]
		;	Bits1 = Bits2
		),
		(	memberchk(rsv3, Bits0) ->
			Bits2 = [rsv3| Bits3]
		;	Bits2 = Bits3
		),
		Bits3 = [].

	validate_frame_semantics(Final, Opcode, Payload) :-
		(	control_opcode(Opcode) ->
			Final == final,
			length(Payload, PayloadLength),
			PayloadLength =< 125,
			validate_close_payload(Opcode, Payload)
		;	true
		),
		!.
	validate_frame_semantics(Final, Opcode, Payload) :-
		domain_error(http_websocket_frame, frame(Final, Opcode, Payload, _)).

	control_opcode(close) :-
		!.
	control_opcode(ping) :-
		!.
	control_opcode(pong).

	validate_close_payload(close, Payload) :-
		!,
		(	Payload == [] ->
			true
		;	Payload = [Byte0, Byte1| _],
			Code is (Byte0 << 8) \/ Byte1,
			valid_close_code(Code)
		).
	validate_close_payload(_Opcode, _Payload).

	valid_close_code(Code) :-
		Code >= 1000,
		Code =< 4999,
		Code =\= 1004,
		Code =\= 1005,
		Code =\= 1006,
		Code =\= 1015,
		(	Code =< 1014 ->
			true
		;	Code >= 3000
		),
		!.

	properties_from_components(ReservedBits, MaskingKey, Properties) :-
		(	ReservedBits == [] ->
			Properties = Properties0
		;	Properties = [reserved_bits(ReservedBits)| Properties0]
		),
		(	MaskingKey == no ->
			Properties0 = []
		;	Properties0 = [masking_key(MaskingKey)]
		).

	decode_frame(Frame) -->
		[Byte0, Byte1],
		{ 	decode_first_byte(Byte0, Final, Opcode, ReservedBits),
			MaskFlag is (Byte1 >> 7) /\ 0x01,
			LengthCode is Byte1 /\ 0x7F
		},
		decode_payload_length(LengthCode, PayloadLength),
		decode_masking_key(MaskFlag, MaskingKey),
		{ 	length(PayloadBytes0, PayloadLength)
		},
		bytes(PayloadBytes0),
		{ 	mask_or_unmask_payload(PayloadBytes0, MaskingKey, PayloadBytes),
			properties_from_components(ReservedBits, MaskingKey, Properties),
			frame(Final, Opcode, PayloadBytes, Properties, Frame)
		}.

	encode_frame(Frame) -->
		{ 	normalize_frame(Frame, frame(Final, Opcode, PayloadBytes, _Properties), MaskingKey, ReservedBits),
			length(PayloadBytes, PayloadLength),
			payload_length_encoding(PayloadLength, LengthCode, ExtendedLengthBytes),
			first_byte(Final, Opcode, ReservedBits, Byte0),
			masking_encoding(MaskingKey, PayloadBytes, MaskBytes, EncodedPayloadBytes),
			Byte1 is MaskBytes /\ 0x80 \/ LengthCode
		},
		[Byte0, Byte1],
		bytes(ExtendedLengthBytes),
		masking_key_bytes(MaskingKey),
		bytes(EncodedPayloadBytes).

	decode_payload_length(LengthCode, PayloadLength) -->
		{ 	LengthCode =< 125,
			PayloadLength = LengthCode
		},
		!.
	decode_payload_length(126, PayloadLength) -->
		[Byte0, Byte1],
		{ 	PayloadLength is (Byte0 << 8) \/ Byte1,
			PayloadLength >= 126
		}.
	decode_payload_length(127, PayloadLength) -->
		[Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7],
		{ 	PayloadLength is (Byte0 << 56) \/ (Byte1 << 48) \/ (Byte2 << 40) \/ (Byte3 << 32) \/ (Byte4 << 24) \/ (Byte5 << 16) \/ (Byte6 << 8) \/ Byte7,
			PayloadLength >= 65536,
			PayloadLength =< 0x7FFFFFFFFFFFFFFF
		}.

	decode_masking_key(0, no) -->
		!.
	decode_masking_key(1, [Byte0, Byte1, Byte2, Byte3]) -->
		[Byte0, Byte1, Byte2, Byte3].

	bytes([]) -->
		[].
	bytes([Byte| Bytes]) -->
		[Byte],
		bytes(Bytes).

	masking_key_bytes(no) -->
		[].
	masking_key_bytes([Byte0, Byte1, Byte2, Byte3]) -->
		[Byte0, Byte1, Byte2, Byte3].

	decode_first_byte(Byte0, Final, Opcode, ReservedBits) :-
		(	Byte0 /\ 0x80 =:= 0x80 ->
			Final = final
		;	Final = more
		),
		OpcodeCode is Byte0 /\ 0x0F,
		opcode_atom(OpcodeCode, Opcode),
		byte_reserved_bits(Byte0, ReservedBits).

	byte_reserved_bits(Byte0, ReservedBits) :-
		(	Byte0 /\ 0x40 =:= 0x40 ->
			ReservedBits = [rsv1| ReservedBits1]
		;	ReservedBits = ReservedBits1
		),
		(	Byte0 /\ 0x20 =:= 0x20 ->
			ReservedBits1 = [rsv2| ReservedBits2]
		;	ReservedBits1 = ReservedBits2
		),
		(	Byte0 /\ 0x10 =:= 0x10 ->
			ReservedBits2 = [rsv3]
		;	ReservedBits2 = []
		).

	first_byte(Final, Opcode, ReservedBits, Byte0) :-
		final_bit(Final, FinalBit),
		reserved_bits_mask(ReservedBits, ReservedBitsMask),
		opcode_code(Opcode, OpcodeCode),
		Byte0 is FinalBit \/ ReservedBitsMask \/ OpcodeCode.

	final_bit(final, 0x80) :-
		!.
	final_bit(more, 0x00).

	reserved_bits_mask(ReservedBits, Mask) :-
		(	memberchk(rsv1, ReservedBits) ->
			Mask0 = 0x40
		;	Mask0 = 0x00
		),
		(	memberchk(rsv2, ReservedBits) ->
			Mask1 is Mask0 \/ 0x20
		;	Mask1 = Mask0
		),
		(	memberchk(rsv3, ReservedBits) ->
			Mask is Mask1 \/ 0x10
		;	Mask = Mask1
		).

	opcode_atom(0x0, continuation) :-
		!.
	opcode_atom(0x1, text) :-
		!.
	opcode_atom(0x2, binary) :-
		!.
	opcode_atom(0x8, close) :-
		!.
	opcode_atom(0x9, ping) :-
		!.
	opcode_atom(0xA, pong) :-
		!.
	opcode_atom(OpcodeCode, _Opcode) :-
		domain_error(http_websocket_opcode, OpcodeCode).

	opcode_code(continuation, 0x0) :-
		!.
	opcode_code(text, 0x1) :-
		!.
	opcode_code(binary, 0x2) :-
		!.
	opcode_code(close, 0x8) :-
		!.
	opcode_code(ping, 0x9) :-
		!.
	opcode_code(pong, 0xA).

	payload_length_encoding(PayloadLength, LengthCode, ExtendedLengthBytes) :-
		(	PayloadLength =< 125 ->
			LengthCode = PayloadLength,
			ExtendedLengthBytes = []
		;	PayloadLength =< 0xFFFF ->
			LengthCode = 126,
			Byte0 is (PayloadLength >> 8) /\ 0xFF,
			Byte1 is PayloadLength /\ 0xFF,
			ExtendedLengthBytes = [Byte0, Byte1]
		;	PayloadLength =< 0x7FFFFFFFFFFFFFFF ->
			LengthCode = 127,
			Byte0 is (PayloadLength >> 56) /\ 0xFF,
			Byte1 is (PayloadLength >> 48) /\ 0xFF,
			Byte2 is (PayloadLength >> 40) /\ 0xFF,
			Byte3 is (PayloadLength >> 32) /\ 0xFF,
			Byte4 is (PayloadLength >> 24) /\ 0xFF,
			Byte5 is (PayloadLength >> 16) /\ 0xFF,
			Byte6 is (PayloadLength >> 8) /\ 0xFF,
			Byte7 is PayloadLength /\ 0xFF,
			ExtendedLengthBytes = [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7]
		;	domain_error(http_websocket_frame_length, PayloadLength)
		).

	masking_encoding(no, PayloadBytes, 0x00, PayloadBytes) :-
		!.
	masking_encoding([Byte0, Byte1, Byte2, Byte3], PayloadBytes, 0x80, EncodedPayloadBytes) :-
		mask_or_unmask_payload(PayloadBytes, [Byte0, Byte1, Byte2, Byte3], EncodedPayloadBytes).

	mask_or_unmask_payload(PayloadBytes, no, PayloadBytes) :-
		!.
	mask_or_unmask_payload(PayloadBytes, [Byte0, Byte1, Byte2, Byte3], ResultBytes) :-
		mask_bytes(PayloadBytes, [Byte0, Byte1, Byte2, Byte3], 0, [], ReversedResultBytes),
		reverse(ReversedResultBytes, ResultBytes).

	mask_bytes([], _MaskingKey, _Index, Bytes, Bytes) :-
		!.
	mask_bytes([Byte| Bytes], [Key0, Key1, Key2, Key3], Index, Acc0, Acc) :-
		masking_byte(Index, Key0, Key1, Key2, Key3, MaskByte),
		MaskedByte is xor(Byte, MaskByte),
		NextIndex is (Index + 1) mod 4,
		mask_bytes(Bytes, [Key0, Key1, Key2, Key3], NextIndex, [MaskedByte| Acc0], Acc).

	masking_byte(0, Key0, _Key1, _Key2, _Key3, Key0).
	masking_byte(1, _Key0, Key1, _Key2, _Key3, Key1).
	masking_byte(2, _Key0, _Key1, Key2, _Key3, Key2).
	masking_byte(3, _Key0, _Key1, _Key2, Key3, Key3).

	read_payload_length(_Stream, LengthCode, PayloadLength) :-
		LengthCode =< 125,
		!,
		PayloadLength = LengthCode.
	read_payload_length(Stream, 126, PayloadLength) :-
		!,
		read_required_bytes(2, Stream, [Byte0, Byte1]),
		PayloadLength is (Byte0 << 8) \/ Byte1,
		PayloadLength >= 126,
		!.
	read_payload_length(Stream, 127, PayloadLength) :-
		read_required_bytes(8, Stream, [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7]),
		PayloadLength is (Byte0 << 56) \/ (Byte1 << 48) \/ (Byte2 << 40) \/ (Byte3 << 32) \/ (Byte4 << 24) \/ (Byte5 << 16) \/ (Byte6 << 8) \/ Byte7,
		PayloadLength >= 65536,
		PayloadLength =< 0x7FFFFFFFFFFFFFFF,
		!.
	read_payload_length(_Stream, _LengthCode, _PayloadLength) :-
		domain_error(http_websocket_byte_sequence, end_of_file).

	read_masking_key(_Stream, 0, no) :-
		!.
	read_masking_key(Stream, 1, MaskingKey) :-
		read_required_bytes(4, Stream, MaskingKey).

	read_required_byte(Stream, Byte) :-
		get_byte(Stream, Byte),
		(	Byte =:= -1 ->
			domain_error(http_websocket_byte_sequence, end_of_file)
		;	true
		).

	read_required_bytes(0, _Stream, []) :-
		!.
	read_required_bytes(Count, Stream, [Byte| Bytes]) :-
		Count > 0,
		get_byte(Stream, Byte),
		(	Byte =:= -1 ->
			domain_error(http_websocket_byte_sequence, end_of_file)
		;	NextCount is Count - 1,
			read_required_bytes(NextCount, Stream, Bytes)
		).

	bytes_to_sink(Sink, Bytes) :-
		(	var(Sink) ->
			instantiation_error
		;	Sink = file(File) ->
			open(File, write, Stream, [type(binary)]),
			write_bytes(Bytes, Stream),
			close(Stream)
		;	Sink = stream(Stream) ->
			write_bytes(Bytes, Stream),
			flush_output(Stream)
		;	Sink = bytes(Bytes) ->
			true
		;	Sink = codes(Bytes) ->
			true
		;	domain_error(http_websocket_sink, Sink)
		).

	write_bytes([], _Stream).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	valid_byte(Byte) :-
		integer(Byte),
		Byte >= 0,
		Byte =< 255.

	valid_byte_list([]).
	valid_byte_list([Byte| Bytes]) :-
		valid_byte(Byte),
		valid_byte_list(Bytes).

:- end_object.
