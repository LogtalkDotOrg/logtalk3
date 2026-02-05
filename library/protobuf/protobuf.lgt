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


:- object(protobuf).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-04,
		comment is 'Google Protocol Buffers binary format parser and generator.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --pair), one_or_error).
	:- info(parse/2, [
		comment is 'Parses Protocol Buffers binary data from the given source (``bytes(List)``, ``stream(Stream)``, or ``file(Path)``) returning a ``Schema-Data`` pair. When the schema is not present in the file, ``Schema`` is unified with ``false``.',
		argnames is ['Source', 'Schema-Data']
	]).

	:- public(parse/3).
	:- mode(parse(++compound, ++term, --term), one_or_error).
	:- info(parse/3, [
		comment is 'Parses Protocol Buffers binary data from the given source using the provided schema, returning the decoded data.',
		argnames is ['Source', 'Schema', 'Data']
	]).

	:- public(generate/3).
	:- mode(generate(++compound, ++term, ++term), one_or_error).
	:- info(generate/3, [
		comment is 'Generates Protocol Buffers binary data to the given sink (``bytes(List)``, ``stream(Stream)``, or ``file(Path)``) from the given schema and data. The schema is not included in the output.',
		argnames is ['Sink', 'Schema', 'Data']
	]).

	:- public(generate/4).
	:- mode(generate(++compound, ++boolean, ++term, ++term), one_or_error).
	:- info(generate/4, [
		comment is 'Generates Protocol Buffers binary data to the given sink from the given schema and data. When ``IncludeSchema`` is ``true``, the schema is embedded in a wrapper message.',
		argnames is ['Sink', 'IncludeSchema', 'Schema', 'Data']
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(reader, [
		file_to_bytes/2, stream_to_bytes/2
	]).

	:- uses(json(curly, dash, atom), [
		parse/2 as json_parse/2, generate/2 as json_generate/2
	]).

	% parse/2 - returns Schema-Data pair
	parse(Source, Schema-Data) :-
		parse_source(Source, Bytes),
		(	phrase(decode_with_schema(Schema0, DataBytes), Bytes) ->
			Schema = Schema0
		;	Schema = false,
			DataBytes = Bytes
		),
		(	Schema \== false ->
			(	phrase(decode_message(Schema, Data), DataBytes) ->
				true
			;	domain_error(protobuf_schema, Schema)
			)
		;	Data = DataBytes
		).

	% parse/3 - parse with provided schema
	parse(Source, Schema, Data) :-
		parse_source(Source, Bytes),
		(	phrase(decode_message(Schema, Data), Bytes) ->
			true
		;	domain_error(protobuf_schema, Schema)
		).

	parse_source(Source, _) :-
		var(Source),
		instantiation_error.
	parse_source(bytes(Bytes), Bytes) :-
		!.
	parse_source(stream(Stream), Bytes) :-
		!,
		stream_to_bytes(Stream, Bytes).
	parse_source(file(File), Bytes) :-
		!,
		file_to_bytes(File, Bytes).
	parse_source(Source, _) :-
		domain_error(protobuf_source, Source).

	% Decode wrapper message with embedded schema (custom format)
	% Field 1 = schema (string), Field 2 = data (bytes)
	decode_with_schema(Schema, DataBytes) -->
		decode_field_raw(1, 2, SchemaBytes),
		decode_field_raw(2, 2, DataBytes),
		{	atom_codes(SchemaAtom, SchemaBytes),
			json_parse(atom(SchemaAtom), Schema)
		}.

	% generate/3 - generate without schema in output
	generate(Sink, Schema, Data) :-
		generate(Sink, false, Schema, Data).

	% generate/4 - generate with optional schema inclusion
	generate(Sink, IncludeSchema, Schema, Data) :-
		(	phrase(encode_message(Schema, Data), DataBytes) ->
			true
		;	domain_error(protobuf_schema, Schema)
		),
		(	IncludeSchema == true ->
			phrase(encode_with_schema(Schema, DataBytes), Bytes)
		;	Bytes = DataBytes
		),
		write_sink(Sink, Bytes).

	write_sink(Sink, _) :-
		var(Sink),
		instantiation_error.
	write_sink(bytes(Bytes), Bytes) :-
		!.
	write_sink(stream(Stream), Bytes) :-
		!,
		write_bytes(Bytes, Stream).
	write_sink(file(File), Bytes) :-
		!,
		open(File, write, Stream, [type(binary)]),
		catch(
			write_bytes(Bytes, Stream),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).
	write_sink(Sink, _) :-
		domain_error(protobuf_sink, Sink).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	% Encode wrapper message with schema embedded
	encode_with_schema(Schema, DataBytes) -->
		{	json_generate(atom(SchemaAtom), Schema),
			atom_codes(SchemaAtom, SchemaCodes)
		},
		% Field 1: schema (string) - wire type 2 (length-delimited)
		encode_field(1, 2, SchemaCodes),
		% Field 2: data (bytes) - wire type 2 (length-delimited)
		encode_field(2, 2, DataBytes).

	% =========================================================================
	% Field encoding/decoding DCGs
	% =========================================================================

	% Encode a field: field_number, wire_type, value (already encoded bytes)
	encode_field(FieldNum, WireType, Value) -->
		{Tag is (FieldNum << 3) \/ WireType},
		encode_varint(Tag),
		(	{WireType =:= 2} ->
			% Length-delimited: encode length then value
			{length(Value, Len)},
			encode_varint(Len),
			bytes(Value)
		;	% Varint, 64-bit, or 32-bit: value bytes directly
			bytes(Value)
		).

	% Decode a field with expected field number and wire type
	decode_field_raw(ExpectedFieldNum, ExpectedWireType, Value) -->
		decode_varint(Tag),
		{	FieldNum is Tag >> 3,
			WireType is Tag /\ 0x07
		},
		(	{FieldNum =:= ExpectedFieldNum, WireType =:= ExpectedWireType} ->
			decode_field_value_raw(WireType, Value)
		;	% Skip this field and try to find the expected one
			skip_field(WireType),
			decode_field_raw(ExpectedFieldNum, ExpectedWireType, Value)
		).

	% Decode field value based on wire type (returns raw bytes or value)
	decode_field_value_raw(0, Value) -->
		% Varint
		decode_varint(Value).
	decode_field_value_raw(1, Value) -->
		% 64-bit: 8 bytes
		bytes_n(8, Value).
	decode_field_value_raw(2, Value) -->
		% Length-delimited
		decode_varint(Len),
		bytes_n(Len, Value).
	decode_field_value_raw(5, Value) -->
		% 32-bit: 4 bytes
		bytes_n(4, Value).

	% Skip a field based on wire type
	skip_field(0) -->
		% Varint: consume until MSB is 0
		decode_varint(_).
	skip_field(1) -->
		% 64-bit: skip 8 bytes
		bytes_n(8, _).
	skip_field(2) -->
		% Length-delimited: read length, skip that many bytes
		decode_varint(Len),
		bytes_n(Len, _).
	skip_field(5) -->
		% 32-bit: skip 4 bytes
		bytes_n(4, _).

	% =========================================================================
	% Message encoding/decoding DCGs
	% =========================================================================

	% Encode message based on schema
	encode_message({message-_MessageName, fields-Fields}, Data) -->
		encode_fields(Fields, Data).
	encode_message(int32, Data) -->
		encode_int32(Data).
	encode_message(int64, Data) -->
		encode_int64(Data).
	encode_message(uint32, Data) -->
		encode_uint32(Data).
	encode_message(uint64, Data) -->
		encode_uint64(Data).
	encode_message(sint32, Data) -->
		encode_sint32(Data).
	encode_message(sint64, Data) -->
		encode_sint64(Data).
	encode_message(bool, Data) -->
		encode_bool(Data).
	encode_message(fixed32, Data) -->
		encode_fixed32(Data).
	encode_message(fixed64, Data) -->
		encode_fixed64(Data).
	encode_message(sfixed32, Data) -->
		encode_sfixed32(Data).
	encode_message(sfixed64, Data) -->
		encode_sfixed64(Data).
	encode_message(float, Data) -->
		encode_float(Data).
	encode_message(double, Data) -->
		encode_double(Data).
	encode_message(string, Data) -->
		encode_string(Data).
	encode_message(bytes, Data) -->
		encode_bytes(Data).

	% Encode fields of a message
	encode_fields([], _) -->
		[].
	encode_fields([Field| Fields], Data) -->
		encode_field_if_present(Field, Data),
		encode_fields(Fields, Data).

	encode_field_if_present({number-Num, name-Name, type-Type}, Data) -->
		(	{memberchk(Name-Value, Data)} ->
			{get_wire_type(Type, WireType)},
			encode_typed_field(Num, WireType, Type, Value)
		;	% Field not present, skip (optional fields)
			[]
		).

	% Encode a typed field (encodes value first, then wraps as field)
	encode_typed_field(Num, WireType, Type, Value) -->
		{phrase(encode_field_value(Type, Value), ValueBytes)},
		encode_field(Num, WireType, ValueBytes).

	% Get wire type for a given schema type
	get_wire_type(int32, 0).
	get_wire_type(int64, 0).
	get_wire_type(uint32, 0).
	get_wire_type(uint64, 0).
	get_wire_type(sint32, 0).
	get_wire_type(sint64, 0).
	get_wire_type(bool, 0).
	get_wire_type(fixed64, 1).
	get_wire_type(sfixed64, 1).
	get_wire_type(double, 1).
	get_wire_type(string, 2).
	get_wire_type(bytes, 2).
	get_wire_type({message-_, fields-_}, 2).
	get_wire_type(fixed32, 5).
	get_wire_type(sfixed32, 5).
	get_wire_type(float, 5).

	% Encode field value based on type
	encode_field_value(int32, Value) -->
		encode_int32(Value).
	encode_field_value(int64, Value) -->
		encode_int64(Value).
	encode_field_value(uint32, Value) -->
		encode_uint32(Value).
	encode_field_value(uint64, Value) -->
		encode_uint64(Value).
	encode_field_value(sint32, Value) -->
		encode_sint32(Value).
	encode_field_value(sint64, Value) -->
		encode_sint64(Value).
	encode_field_value(bool, Value) -->
		encode_bool(Value).
	encode_field_value(fixed32, Value) -->
		encode_fixed32(Value).
	encode_field_value(fixed64, Value) -->
		encode_fixed64(Value).
	encode_field_value(sfixed32, Value) -->
		encode_sfixed32(Value).
	encode_field_value(sfixed64, Value) -->
		encode_sfixed64(Value).
	encode_field_value(float, Value) -->
		encode_float(Value).
	encode_field_value(double, Value) -->
		encode_double(Value).
	encode_field_value(string, Value) -->
		encode_string(Value).
	encode_field_value(bytes, Value) -->
		encode_bytes(Value).
	encode_field_value({message-MessageName, fields-Fields}, Value) -->
		{Schema = {message-MessageName, fields-Fields}},
		(	encode_message(Schema, Value) ->
			{true}
		;	{domain_error(protobuf_schema, Schema)}
		).

	% Decode message based on schema
	decode_message({message-_MessageName, fields-Fields}, Data) -->
		decode_fields(Fields, [], Data).
	decode_message(int32, Data) -->
		decode_int32(Data).
	decode_message(int64, Data) -->
		decode_int64(Data).
	decode_message(uint32, Data) -->
		decode_uint32(Data).
	decode_message(uint64, Data) -->
		decode_uint64(Data).
	decode_message(sint32, Data) -->
		decode_sint32(Data).
	decode_message(sint64, Data) -->
		decode_sint64(Data).
	decode_message(bool, Data) -->
		decode_bool(Data).
	decode_message(fixed32, Data) -->
		decode_fixed32(Data).
	decode_message(fixed64, Data) -->
		decode_fixed64(Data).
	decode_message(sfixed32, Data) -->
		decode_sfixed32(Data).
	decode_message(sfixed64, Data) -->
		decode_sfixed64(Data).
	decode_message(float, Data) -->
		decode_float(Data).
	decode_message(double, Data) -->
		decode_double(Data).
	decode_message(string, Data) -->
		decode_string(Data).
	decode_message(bytes, Data) -->
		decode_bytes(Data).

	% Decode fields of a message - stores decoded values, then reorders per schema
	decode_fields(Fields, Acc, Result) -->
		(	decode_next_field(FieldNum, _WireType, ValueBytes) ->
			(	{find_field_by_number(Fields, FieldNum, Name, Type)} ->
				{decode_field_value(Type, ValueBytes, Value)},
				decode_fields(Fields, [Name-Value| Acc], Result)
			;	% Unknown field, skip it
				decode_fields(Fields, Acc, Result)
			)
		;	% No more parseable fields - reorder to match schema definition order
			{reorder_by_schema(Fields, Acc, Result)}
		).

	% Reorder decoded fields to match schema definition order
	reorder_by_schema([], _, []).
	reorder_by_schema([{number-_, name-Name, type-_}| SchemaFields], Decoded, Result) :-
		(	memberchk(Name-Value, Decoded) ->
			Result = [Name-Value| Rest],
			reorder_by_schema(SchemaFields, Decoded, Rest)
		;	% Field not present in decoded data
			reorder_by_schema(SchemaFields, Decoded, Result)
		).

	% Parse next field from bytes (returns field number, wire type, and value bytes)
	decode_next_field(FieldNum, WireType, ValueBytes) -->
		decode_varint(Tag),
		{	FieldNum is Tag >> 3,
			WireType is Tag /\ 0x07
		},
		extract_field_value(WireType, ValueBytes).

	% Extract field value based on wire type
	extract_field_value(0, ValueBytes) -->
		% Varint: capture raw varint bytes
		decode_varint(Value),
		{phrase(encode_varint(Value), ValueBytes)}.
	extract_field_value(1, ValueBytes) -->
		% 64-bit: 8 bytes
		bytes_n(8, ValueBytes).
	extract_field_value(2, ValueBytes) -->
		% Length-delimited
		decode_varint(Len),
		bytes_n(Len, ValueBytes).
	extract_field_value(5, ValueBytes) -->
		% 32-bit: 4 bytes
		bytes_n(4, ValueBytes).

	% Find field definition by field number
	find_field_by_number([{number-Num, name-Name, type-Type}| _], Num, Name, Type) :-
		!.
	find_field_by_number([_| Fields], Num, Name, Type) :-
		find_field_by_number(Fields, Num, Name, Type).

	% Decode field value based on type
	decode_field_value(int32, Bytes, Value) :-
		phrase(decode_int32(Value), Bytes).
	decode_field_value(int64, Bytes, Value) :-
		phrase(decode_int64(Value), Bytes).
	decode_field_value(uint32, Bytes, Value) :-
		phrase(decode_uint32(Value), Bytes).
	decode_field_value(uint64, Bytes, Value) :-
		phrase(decode_uint64(Value), Bytes).
	decode_field_value(sint32, Bytes, Value) :-
		phrase(decode_sint32(Value), Bytes).
	decode_field_value(sint64, Bytes, Value) :-
		phrase(decode_sint64(Value), Bytes).
	decode_field_value(bool, Bytes, Value) :-
		phrase(decode_bool(Value), Bytes).
	decode_field_value(fixed32, Bytes, Value) :-
		phrase(decode_fixed32(Value), Bytes).
	decode_field_value(fixed64, Bytes, Value) :-
		phrase(decode_fixed64(Value), Bytes).
	decode_field_value(sfixed32, Bytes, Value) :-
		phrase(decode_sfixed32(Value), Bytes).
	decode_field_value(sfixed64, Bytes, Value) :-
		phrase(decode_sfixed64(Value), Bytes).
	decode_field_value(float, Bytes, Value) :-
		phrase(decode_float(Value), Bytes).
	decode_field_value(double, Bytes, Value) :-
		phrase(decode_double(Value), Bytes).
	decode_field_value(string, Bytes, Value) :-
		phrase(decode_string(Value), Bytes).
	decode_field_value(bytes, Bytes, Bytes).
	decode_field_value({message-MessageName, fields-Fields}, Bytes, Value) :-
		Schema = {message-MessageName, fields-Fields},
		(	phrase(decode_message(Schema, Value), Bytes) ->
			true
		;	domain_error(protobuf_schema, Schema)
		).

	% =========================================================================
	% Varint encoding/decoding DCGs (base-128 encoding)
	% =========================================================================

	encode_varint(N) -->
		{N < 128},
		!,
		[N].
	encode_varint(N) -->
		{	Byte is (N /\ 0x7f) \/ 0x80,
			N1 is N >> 7
		},
		[Byte],
		encode_varint(N1).

	decode_varint(Value) -->
		[Byte],
		{Byte < 128},
		!,
		{Value is Byte}.
	decode_varint(Value) -->
		[Byte],
		{Byte1 is Byte /\ 0x7f},
		decode_varint(Value1),
		{Value is Byte1 \/ (Value1 << 7)}.

	% =========================================================================
	% Integer encoding/decoding DCGs
	% =========================================================================

	% int32 (varint)
	encode_int32(Value) -->
		encode_varint(Value).

	decode_int32(Value) -->
		decode_varint(Value).

	% int64 (varint)
	encode_int64(Value) -->
		encode_varint(Value).

	decode_int64(Value) -->
		decode_varint(Value).

	% uint32 (varint)
	encode_uint32(Value) -->
		encode_varint(Value).

	decode_uint32(Value) -->
		decode_varint(Value).

	% uint64 (varint)
	encode_uint64(Value) -->
		encode_varint(Value).

	decode_uint64(Value) -->
		decode_varint(Value).

	% sint32 (zigzag + varint)
	encode_sint32(Value) -->
		{ZigZag is xor((Value << 1), (Value >> 31))},
		encode_varint(ZigZag).

	decode_sint32(Value) -->
		decode_varint(ZigZag),
		{Value is xor((ZigZag >> 1), (-(ZigZag /\ 1)))}.

	% sint64 (zigzag + varint)
	encode_sint64(Value) -->
		{ZigZag is xor((Value << 1), (Value >> 63))},
		encode_varint(ZigZag).

	decode_sint64(Value) -->
		decode_varint(ZigZag),
		{Value is xor((ZigZag >> 1), (-(ZigZag /\ 1)))}.

	% =========================================================================
	% Boolean encoding/decoding DCGs
	% =========================================================================

	encode_bool(false) -->
		[0].
	encode_bool(true) -->
		[1].

	decode_bool(false) -->
		[0].
	decode_bool(true) -->
		[1].

	% =========================================================================
	% Fixed-width integer encoding/decoding DCGs (little-endian)
	% =========================================================================

	% fixed32 (little-endian 32-bit)
	encode_fixed32(Value) -->
		{	B0 is Value /\ 0xff,
			B1 is (Value >> 8) /\ 0xff,
			B2 is (Value >> 16) /\ 0xff,
			B3 is (Value >> 24) /\ 0xff
		},
		[B0, B1, B2, B3].

	decode_fixed32(Value) -->
		[B0, B1, B2, B3],
		{Value is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24)}.

	% fixed64 (little-endian 64-bit)
	encode_fixed64(Value) -->
		{	B0 is Value /\ 0xff,
			B1 is (Value >> 8) /\ 0xff,
			B2 is (Value >> 16) /\ 0xff,
			B3 is (Value >> 24) /\ 0xff,
			B4 is (Value >> 32) /\ 0xff,
			B5 is (Value >> 40) /\ 0xff,
			B6 is (Value >> 48) /\ 0xff,
			B7 is (Value >> 56) /\ 0xff
		},
		[B0, B1, B2, B3, B4, B5, B6, B7].

	decode_fixed64(Value) -->
		[B0, B1, B2, B3, B4, B5, B6, B7],
		{Value is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24) \/
		         (B4 << 32) \/ (B5 << 40) \/ (B6 << 48) \/ (B7 << 56)}.

	% sfixed32 (signed fixed32)
	encode_sfixed32(Value) -->
		encode_fixed32(Value).

	decode_sfixed32(Value) -->
		decode_fixed32(UValue),
		{	UValue >= 2147483648 ->
			Value is UValue - 4294967296
		;	Value = UValue
		}.

	% sfixed64 (signed fixed64)
	encode_sfixed64(Value) -->
		encode_fixed64(Value).

	decode_sfixed64(Value) -->
		decode_fixed64(UValue),
		{	UValue >= 9223372036854775808 ->
			Value is UValue - 18446744073709551616
		;	Value = UValue
		}.

	% =========================================================================
	% Float/Double encoding/decoding DCGs (IEEE 754, little-endian)
	% =========================================================================

	% float (32-bit IEEE 754)
	encode_float(Float) -->
		{	float_to_ieee754_32(Float, Bits),
			B0 is Bits /\ 0xff,
			B1 is (Bits >> 8) /\ 0xff,
			B2 is (Bits >> 16) /\ 0xff,
			B3 is (Bits >> 24) /\ 0xff
		},
		[B0, B1, B2, B3].

	decode_float(Float) -->
		[B0, B1, B2, B3],
		{	Bits is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24),
			ieee754_32_to_float(Bits, Float)
		}.

	% double (64-bit IEEE 754)
	encode_double(Double) -->
		{	float_to_ieee754_64(Double, Bits),
			B0 is Bits /\ 0xff,
			B1 is (Bits >> 8) /\ 0xff,
			B2 is (Bits >> 16) /\ 0xff,
			B3 is (Bits >> 24) /\ 0xff,
			B4 is (Bits >> 32) /\ 0xff,
			B5 is (Bits >> 40) /\ 0xff,
			B6 is (Bits >> 48) /\ 0xff,
			B7 is (Bits >> 56) /\ 0xff
		},
		[B0, B1, B2, B3, B4, B5, B6, B7].

	decode_double(Double) -->
		[B0, B1, B2, B3, B4, B5, B6, B7],
		{	Bits is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24) \/
			        (B4 << 32) \/ (B5 << 40) \/ (B6 << 48) \/ (B7 << 56),
			ieee754_64_to_float(Bits, Double)
		}.

	% =========================================================================
	% String/Bytes encoding/decoding DCGs
	% =========================================================================

	% string (UTF-8 bytes, no length prefix - caller handles it)
	encode_string(Atom) -->
		{atom_codes(Atom, Codes)},
		bytes(Codes).

	decode_string(Atom) -->
		bytes_rest(Codes),
		{atom_codes(Atom, Codes)}.

	% bytes (raw bytes, no length prefix - caller handles it)
	encode_bytes(Bytes) -->
		bytes(Bytes).

	decode_bytes(Bytes) -->
		bytes_rest(Bytes).

	% =========================================================================
	% Helper DCGs for byte sequences
	% =========================================================================

	% Emit/consume a list of bytes
	bytes([]) -->
		[].
	bytes([Byte| Bytes]) -->
		[Byte], bytes(Bytes).

	% Consume exactly N bytes
	bytes_n(0, []) -->
		!.
	bytes_n(N, [Byte| Bytes]) -->
		{N > 0},
		[Byte],
		{N1 is N - 1},
		bytes_n(N1, Bytes).

	% Consume all remaining bytes
	bytes_rest([]) -->
		[].
	bytes_rest([Byte| Bytes]) -->
		[Byte], bytes_rest(Bytes).

	% =========================================================================
	% IEEE 754 conversions (auxiliary predicates)
	% =========================================================================

	float_to_ieee754_32(Float, Bits) :-
		(	Float =:= 0 ->
			Bits = 0
		;	Float < 0 ->
			AbsFloat is -Float,
			float_to_ieee754_32_unsigned(AbsFloat, UBits),
			Bits is UBits \/ 0x80000000
		;	float_to_ieee754_32_unsigned(Float, Bits)
		).

	float_to_ieee754_32_unsigned(Float, Bits) :-
		LogFloat is log(Float) / log(2),
		Exponent is floor(LogFloat),
		BiasedExp is Exponent + 127,
		Mantissa is Float / (2 ** Exponent) - 1,
		MantissaBits is round(Mantissa * 8388608), % 2^23
		Bits is (BiasedExp << 23) \/ MantissaBits.

	ieee754_32_to_float(0, 0.0) :-
		!.
	ieee754_32_to_float(Bits, Float) :-
		Sign is (Bits >> 31) /\ 1,
		Exponent is ((Bits >> 23) /\ 0xff) - 127,
		Mantissa is (Bits /\ 0x7fffff) / 8388608 + 1,
		UFloat is Mantissa * (2 ** Exponent),
		(Sign =:= 1 -> Float is -UFloat ; Float = UFloat).

	float_to_ieee754_64(Float, Bits) :-
		(	Float =:= 0 ->
			Bits = 0
		;	Float < 0 ->
			AbsFloat is -Float,
			float_to_ieee754_64_unsigned(AbsFloat, UBits),
			Bits is UBits \/ 0x8000000000000000
		;	float_to_ieee754_64_unsigned(Float, Bits)
		).

	float_to_ieee754_64_unsigned(Float, Bits) :-
		LogFloat is log(Float) / log(2),
		Exponent is floor(LogFloat),
		BiasedExp is Exponent + 1023,
		Mantissa is Float / (2 ** Exponent) - 1,
		MantissaBits is round(Mantissa * 4503599627370496), % 2^52
		Bits is (BiasedExp << 52) \/ MantissaBits.

	ieee754_64_to_float(0, 0.0) :-
		!.
	ieee754_64_to_float(Bits, Float) :-
		Sign is (Bits >> 63) /\ 1,
		Exponent is ((Bits >> 52) /\ 0x7ff) - 1023,
		Mantissa is (Bits /\ 0xfffffffffffff) / 4503599627370496 + 1,
		UFloat is Mantissa * (2 ** Exponent),
		(Sign =:= 1 -> Float is -UFloat ; Float = UFloat).

:- end_object.
