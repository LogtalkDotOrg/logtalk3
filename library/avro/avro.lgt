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


:- object(avro).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-05,
		comment is 'Apache Avro binary format parser and generator.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --pair), one_or_error).
	:- info(parse/2, [
		comment is 'Parses Avro binary data from the given source (``bytes(List)``, ``stream(Stream)``, or ``file(Path)``) returning a ``Schema-Data`` pair. When the schema is not present in the file, ``Schema`` is unified with ``false``.',
		argnames is ['Source', 'Schema-Data']
	]).

	:- public(parse/3).
	:- mode(parse(++compound, ++term, --term), one_or_error).
	:- info(parse/3, [
		comment is 'Parses Avro binary data from the given source using the provided schema, returning the decoded data.',
		argnames is ['Source', 'Schema', 'Data']
	]).

	:- public(generate/3).
	:- mode(generate(++compound, ++term, ++term), one_or_error).
	:- info(generate/3, [
		comment is 'Generates Avro binary data to the given sink (``bytes(List)``, ``stream(Stream)``, or ``file(Path)``) from the given schema and data. The schema is not included in the output.',
		argnames is ['Sink', 'Schema', 'Data']
	]).

	:- public(generate/4).
	:- mode(generate(++compound, ++boolean, ++term, ++term), one_or_error).
	:- info(generate/4, [
		comment is 'Generates Avro binary data to the given sink from the given schema and data. When ``IncludeSchema`` is ``true``, generates an Avro Object Container File with the schema embedded.',
		argnames is ['Sink', 'IncludeSchema', 'Schema', 'Data']
	]).

	:- uses(list, [
		append/2, append/3, length/2, memberchk/2, nth0/3
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
		(	phrase(avro_container_file(Schema0, DataBytes), Bytes) ->
			Schema = Schema0
		;	Schema = false,
			DataBytes = Bytes
		),
		(	Schema \== false ->
			phrase(decode_data(Schema, Data), DataBytes)
		;	Data = DataBytes
		).

	% parse/3 - parse with provided schema
	parse(Source, Schema, Data) :-
		parse_source(Source, Bytes),
		phrase(decode_data(Schema, Data), Bytes).

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
		domain_error(avro_source, Source).

	% DCG rule for Avro Object Container File magic bytes: Obj1
	avro_container_file(Schema, DataBytes) -->
		[0x4f, 0x62, 0x6a, 0x01],
		% Parse file header metadata (map)
		decode_map(Metadata),
		{	% Extract schema from metadata
			memberchk('avro.schema'-SchemaBytes, Metadata),
			atom_codes(SchemaAtom, SchemaBytes),
			json_parse(atom(SchemaAtom), Schema)
		},
		% Skip sync marker (16 bytes)
		skip_bytes(16),
		% Parse data blocks
		parse_data_blocks(DataBytes).

	skip_bytes(0) -->
		!.
	skip_bytes(N) -->
		[_],
		{ N1 is N - 1 },
		skip_bytes(N1).

	take_bytes(0, []) -->
		!.
	take_bytes(N, [Byte| Bytes]) -->
		[Byte],
		{ N1 is N - 1 },
		take_bytes(N1, Bytes).

	parse_data_blocks([]) -->
		[].
	parse_data_blocks(Data) -->
		decode_long(Count),
		(	{ Count =:= 0 } ->
			{ Data = [] }
		;	decode_long(BlockSize),
			take_bytes(BlockSize, BlockData),
			% Skip sync marker (16 bytes)
			skip_bytes(16),
			parse_data_blocks(MoreData),
			{ append(BlockData, MoreData, Data) }
		).

	% generate/3 - generate without schema in output
	generate(Sink, Schema, Data) :-
		generate(Sink, false, Schema, Data).

	% generate/4 - generate with optional schema inclusion
	generate(Sink, IncludeSchema, Schema, Data) :-
		phrase(encode_data(Schema, Data), DataBytes),
		(	IncludeSchema == true ->
			generate_container_file(Schema, DataBytes, Bytes)
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
		domain_error(avro_sink, Sink).

	write_bytes([], _).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	% Generate Avro Object Container File
	generate_container_file(Schema, DataBytes, Bytes) :-
		% Magic bytes: Obj1
		Magic = [0x4f, 0x62, 0x6a, 0x01],
		% Generate schema JSON
		json_generate(atom(SchemaAtom), Schema),
		atom_codes(SchemaAtom, SchemaCodes),
		% Create metadata map with avro.schema and avro.codec
		% encode_map expects Key-RawValueBytes format where Key is an atom
		Metadata = ['avro.schema'-SchemaCodes, 'avro.codec'-[0x6e, 0x75, 0x6c, 0x6c]], % "null"
		phrase(encode_map(Metadata), MetaBytes),
		% Generate sync marker (16 bytes of simple deterministic data)
		SyncMarker = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
		              0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
		% Create data block
		length(DataBytes, DataLength),
		phrase(encode_long(1), CountBytes), % 1 object in block
		phrase(encode_long(DataLength), SizeBytes),
		append([Magic, MetaBytes, SyncMarker, CountBytes, SizeBytes, DataBytes, SyncMarker], Bytes).

	% DCG rule for varint encoding for long (zig-zag encoding)
	encode_long(N) -->
		{ ZigZag is xor((N << 1), (N >> 63)) },
		encode_varint(ZigZag).

	encode_varint(N) -->
		{ N < 128, ! },
		{ Byte is N /\ 0x7f },
		[Byte].
	encode_varint(N) -->
		{ Byte is (N /\ 0x7f) \/ 0x80 },
		[Byte],
		{ N1 is N >> 7 },
		encode_varint(N1).

	% DCG rule for varint decoding for long
	decode_long(Value) -->
		decode_varint(ZigZag),
		{ Value is xor((ZigZag >> 1), (-(ZigZag /\ 1))) }.

	decode_varint(Value) -->
		[Byte],
		{ Byte < 128, ! },
		{ Value is Byte }.
	decode_varint(Value) -->
		[Byte],
		{ Byte1 is Byte /\ 0x7f },
		decode_varint(Value1),
		{ Value is Byte1 \/ (Value1 << 7) }.

	% Encode int (same as long but 32-bit)
	encode_int(N) -->
		encode_long(N).

	decode_int(Value) -->
		decode_long(Value).

	% DCG rules for encode/decode boolean
	encode_boolean(false) -->
		[0].
	encode_boolean(true) -->
		[1].

	decode_boolean(false) -->
		[0].
	decode_boolean(true) -->
		[1].

	% DCG rules for encode/decode float (4 bytes, little-endian IEEE 754)
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

	% DCG rules for encode/decode double (8 bytes, little-endian IEEE 754)
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

	% IEEE 754 conversions (simplified - using Prolog float representation)
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

	% DCG rules for encode/decode bytes
	encode_bytes(Bytes) -->
		{ length(Bytes, Length) },
		encode_long(Length),
		Bytes.

	decode_bytes(Bytes) -->
		decode_long(Length),
		take_bytes(Length, Bytes).

	% DCG rules for encode/decode string (UTF-8 bytes)
	encode_string(Atom) -->
		{ atom(Atom), ! },
		{ atom_codes(Atom, Codes) },
		encode_bytes(Codes).
	encode_string(codes(Codes)) -->
		encode_bytes(Codes).

	decode_string(Atom) -->
		decode_bytes(Codes),
		{ atom_codes(Atom, Codes) }.

	% DCG rule for decode fixed
	decode_fixed(Size, Bytes) -->
		take_bytes(Size, Bytes).

	% DCG rules for encode/decode array
	encode_array([], _Schema) -->
		[0].
	encode_array([Item| Items], Schema) -->
		{ length([Item| Items], Length) },
		encode_long(Length),
		encode_array_items([Item| Items], Schema),
		[0].

	encode_array_items([], _) -->
		[].
	encode_array_items([Item| Items], Schema) -->
		encode_data(Schema, Item),
		encode_array_items(Items, Schema).

	decode_array(Schema, Items) -->
		decode_array_blocks(Schema, Items).

	decode_array_blocks(Schema, Items) -->
		decode_long(Count),
		(	{ Count =:= 0 } ->
			{ Items = [] }
		;	{ Count < 0 } ->
			{ AbsCount is -Count },
			decode_long(_BlockSize),
			decode_array_items(AbsCount, Schema, Items1),
			decode_array_blocks(Schema, Items2),
			{ append(Items1, Items2, Items) }
		;	decode_array_items(Count, Schema, Items1),
			decode_array_blocks(Schema, Items2),
			{ append(Items1, Items2, Items) }
		).

	decode_array_items(0, _, []) -->
		!.
	decode_array_items(N, Schema, [Item| Items]) -->
		{ N > 0 },
		decode_data(Schema, Item),
		{ N1 is N - 1 },
		decode_array_items(N1, Schema, Items).

	% DCG rules for encode/decode map
	encode_map([]) -->
		[0].
	encode_map([Pair| Pairs]) -->
		{ length([Pair| Pairs], Length) },
		encode_long(Length),
		encode_map_pairs([Pair| Pairs]),
		[0].

	encode_map_pairs([]) -->
		[].
	encode_map_pairs([Key-Value| Pairs]) -->
		encode_string(Key),
		encode_bytes(Value),
		encode_map_pairs(Pairs).

	decode_map(Pairs) -->
		decode_map_blocks(Pairs).

	decode_map_blocks(Pairs) -->
		decode_long(Count),
		(	{ Count =:= 0 } ->
			{ Pairs = [] }
		;	{ Count < 0 } ->
			{ AbsCount is -Count },
			decode_long(_BlockSize),
			decode_map_pairs(AbsCount, Pairs1),
			decode_map_blocks(Pairs2),
			{ append(Pairs1, Pairs2, Pairs) }
		;	decode_map_pairs(Count, Pairs1),
			decode_map_blocks(Pairs2),
			{ append(Pairs1, Pairs2, Pairs) }
		).

	decode_map_pairs(0, []) -->
		!.
	decode_map_pairs(N, [Key-Value| Pairs]) -->
		{ N > 0 },
		decode_string(Key),
		decode_bytes(Value),
		{ N1 is N - 1 },
		decode_map_pairs(N1, Pairs).

	% DCG rules for encode/decode union (index + value)
	encode_union(Index, Schema, Value) -->
		encode_long(Index),
		encode_data(Schema, Value).

	decode_union(Schemas, Value) -->
		decode_long(Index),
		{ nth0(Index, Schemas, Schema) },
		decode_data(Schema, Value).

	% DCG rules for encode/decode enum
	encode_enum(Symbol, Symbols) -->
		{ enum_index(Symbol, Symbols, 0, Index) },
		encode_int(Index).

	enum_index(Symbol, [Symbol| _], Index, Index) :-
		!.
	enum_index(Symbol, [_| Symbols], N, Index) :-
		N1 is N + 1,
		enum_index(Symbol, Symbols, N1, Index).

	decode_enum(Symbols, Symbol) -->
		decode_int(Index),
		{ nth0(Index, Symbols, Symbol) }.

	% Main DCG rule for encode_data - dispatches based on schema type
	encode_data(null, @null) -->
		[].
	encode_data(boolean, Bool) -->
		encode_boolean(Bool).
	encode_data(int, Int) -->
		encode_int(Int).
	encode_data(long, Long) -->
		encode_long(Long).
	encode_data(float, Float) -->
		encode_float(Float).
	encode_data(double, Double) -->
		encode_double(Double).
	encode_data(bytes, Bytes) -->
		encode_bytes(Bytes).
	encode_data(string, String) -->
		encode_string(String).
	% Complex types are represented as curly terms {Pairs}
	encode_data({Props}, Data) -->
		encode_complex_type(Props, Data).
	% Arrays in schema (union types)
	encode_data([Type| Types], Data) -->
		{ find_union_match([Type| Types], Data, 0, Index, Schema) },
		encode_union(Index, Schema, Data).

	encode_complex_type(Props, Data) -->
		{ curly_member(type-Type, Props) },
		encode_by_type(Type, Props, Data).

	encode_by_type(record, Props, Data) -->
		{ curly_member(fields-Fields, Props) },
		encode_record_fields(Fields, Data).
	encode_by_type(enum, Props, Symbol) -->
		{ curly_member(symbols-Symbols, Props) },
		encode_enum(Symbol, Symbols).
	encode_by_type(array, Props, Items) -->
		{ curly_member(items-ItemSchema, Props) },
		encode_array(Items, ItemSchema).
	encode_by_type(map, Props, Pairs) -->
		{ curly_member(values-ValueSchema, Props) },
		encode_map_with_schema(Pairs, ValueSchema).
	encode_by_type(fixed, Props, Data) -->
		{ curly_member(size-Size, Props) },
		{ length(Data, Size) },
		Data.

	encode_record_fields([], _) -->
		[].
	encode_record_fields([{FieldProps}| Fields], Data) -->
		{	curly_member(name-FieldName, FieldProps),
			curly_member(type-FieldType, FieldProps),
			get_field_value(Data, FieldName, Value)
		},
		encode_data(FieldType, Value),
		encode_record_fields(Fields, Data).

	get_field_value({Pairs}, FieldName, Value) :-
		curly_member(FieldName-Value, Pairs).

	encode_map_with_schema([], _) -->
		[0].
	encode_map_with_schema([Pair| Pairs], ValueSchema) -->
		{ length([Pair| Pairs], Length) },
		encode_long(Length),
		encode_map_pairs_with_schema([Pair| Pairs], ValueSchema),
		[0].

	encode_map_pairs_with_schema([], _) -->
		[].
	encode_map_pairs_with_schema([Key-Value| Pairs], ValueSchema) -->
		encode_string(Key),
		encode_data(ValueSchema, Value),
		encode_map_pairs_with_schema(Pairs, ValueSchema).

	find_union_match([Schema| _], Data, Index, Index, Schema) :-
		data_matches_schema(Schema, Data), !.
	find_union_match([_| Schemas], Data, N, Index, Schema) :-
		N1 is N + 1,
		find_union_match(Schemas, Data, N1, Index, Schema).

	data_matches_schema(null, @null).
	data_matches_schema(boolean, Bool) :-
		once((Bool == true ; Bool == false)).
	data_matches_schema(int, Int) :-
		integer(Int).
	data_matches_schema(long, Long) :-
		integer(Long).
	data_matches_schema(float, Float) :-
		float(Float).
	data_matches_schema(double, Double) :-
		float(Double).
	data_matches_schema(string, String) :-
		atom(String).

	% Main DCG rule for decode_data - dispatches based on schema type
	decode_data(null, @null) -->
		[].
	decode_data(boolean, Bool) -->
		decode_boolean(Bool).
	decode_data(int, Int) -->
		decode_int(Int).
	decode_data(long, Long) -->
		decode_long(Long).
	decode_data(float, Float) -->
		decode_float(Float).
	decode_data(double, Double) -->
		decode_double(Double).
	decode_data(bytes, Bytes) -->
		decode_bytes(Bytes).
	decode_data(string, String) -->
		decode_string(String).
	% Complex types
	decode_data({Props}, Data) -->
		decode_complex_type(Props, Data).
	% Union types
	decode_data([Type| Types], Data) -->
		decode_union([Type| Types], Data).

	decode_complex_type(Props, Data) -->
		{ curly_member(type-Type, Props) },
		decode_by_type(Type, Props, Data).

	decode_by_type(record, Props, {FieldPairs}) -->
		{ curly_member(fields-Fields, Props) },
		decode_record_fields(Fields, FieldPairsList),
		{ list_to_curly(FieldPairsList, FieldPairs) }.
	decode_by_type(enum, Props, Symbol) -->
		{ curly_member(symbols-Symbols, Props) },
		decode_enum(Symbols, Symbol).
	decode_by_type(array, Props, Items) -->
		{ curly_member(items-ItemSchema, Props) },
		decode_array(ItemSchema, Items).
	decode_by_type(map, Props, Pairs) -->
		{ curly_member(values-ValueSchema, Props) },
		decode_map_with_schema(ValueSchema, Pairs).
	decode_by_type(fixed, Props, Data) -->
		{ curly_member(size-Size, Props) },
		decode_fixed(Size, Data).

	decode_record_fields([], []) -->
		[].
	decode_record_fields([{FieldProps}| Fields], [FieldName-Value| Pairs]) -->
		{	curly_member(name-FieldName, FieldProps),
			curly_member(type-FieldType, FieldProps)
		},
		decode_data(FieldType, Value),
		decode_record_fields(Fields, Pairs).

	decode_map_with_schema(ValueSchema, Pairs) -->
		decode_map_blocks_with_schema(ValueSchema, Pairs).

	decode_map_blocks_with_schema(ValueSchema, Pairs) -->
		decode_long(Count),
		(	{ Count =:= 0 } ->
			{ Pairs = [] }
		;	{ Count < 0 } ->
			{ AbsCount is -Count },
			decode_long(_BlockSize),
			decode_map_pairs_with_schema(AbsCount, ValueSchema, Pairs1),
			decode_map_blocks_with_schema(ValueSchema, Pairs2),
			{ append(Pairs1, Pairs2, Pairs) }
		;	decode_map_pairs_with_schema(Count, ValueSchema, Pairs1),
			decode_map_blocks_with_schema(ValueSchema, Pairs2),
			{ append(Pairs1, Pairs2, Pairs) }
		).

	decode_map_pairs_with_schema(0, _, []) -->
		!.
	decode_map_pairs_with_schema(N, ValueSchema, [Key-Value| Pairs]) -->
		{ N > 0 },
		decode_string(Key),
		decode_data(ValueSchema, Value),
		{ N1 is N - 1 },
		decode_map_pairs_with_schema(N1, ValueSchema, Pairs).

	% find a pair in a curly term's comma-separated pairs
	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

	% convert list of pairs to comma-separated pairs for curly terms
	list_to_curly([Pair], Pair) :-
		!.
	list_to_curly([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly(Pairs, Rest).

:- end_object.
