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
		date is 2026-01-23,
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
		append/2, append/3, length/2, nth0/3, reverse/2
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
		(	avro_container_file(Bytes, Schema0, DataBytes) ->
			Schema = Schema0
		;	Schema = false,
			DataBytes = Bytes
		),
		(	Schema \== false ->
			decode_data(Schema, DataBytes, Data)
		;	Data = DataBytes
		).

	% parse/3 - parse with provided schema
	parse(Source, Schema, Data) :-
		parse_source(Source, Bytes),
		decode_data(Schema, Bytes, Data).

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

	% Check for Avro Object Container File magic bytes: Obj1
	avro_container_file([0x4f, 0x62, 0x6a, 0x01| Rest], Schema, DataBytes) :-
		% Parse file header metadata (map)
		decode_map(Rest, Metadata, AfterMeta),
		% Extract schema from metadata
		member('avro.schema'-SchemaBytes, Metadata),
		atom_codes(SchemaAtom, SchemaBytes),
		json_parse(atom(SchemaAtom), Schema),
		% Skip sync marker (16 bytes)
		skip_bytes(16, AfterMeta, AfterSync),
		% Parse data blocks
		parse_data_blocks(AfterSync, DataBytes).

	skip_bytes(0, Bytes, Bytes) :-
		!.
	skip_bytes(N, [_|Rest], Result) :-
		N > 0,
		N1 is N - 1,
		skip_bytes(N1, Rest, Result).

	parse_data_blocks([], []) :-
		!.
	parse_data_blocks(Bytes, Data) :-
		decode_long(Bytes, Count, Rest1),
		(	Count =:= 0 ->
			Data = [],
			Rest1 = []
		;	decode_long(Rest1, BlockSize, Rest2),
			take_bytes(BlockSize, Rest2, BlockData, Rest3),
			% Skip sync marker
			skip_bytes(16, Rest3, AfterSync),
			parse_data_blocks(AfterSync, MoreData),
			append(BlockData, MoreData, Data)
		).

	take_bytes(0, Bytes, [], Bytes) :-
		!.
	take_bytes(N, [B|Rest], [B|Taken], Remaining) :-
		N > 0,
		N1 is N - 1,
		take_bytes(N1, Rest, Taken, Remaining).

	member(X, [X|_]).
	member(X, [_|T]) :- member(X, T).

	% generate/3 - generate without schema in output
	generate(Sink, Schema, Data) :-
		generate(Sink, false, Schema, Data).

	% generate/4 - generate with optional schema inclusion
	generate(Sink, IncludeSchema, Schema, Data) :-
		encode_data(Schema, Data, DataBytes),
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
		encode_map(Metadata, MetaBytes),
		% Generate sync marker (16 bytes of simple deterministic data)
		SyncMarker = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
		              0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10],
		% Create data block
		length(DataBytes, DataLen),
		encode_long(1, CountBytes), % 1 object in block
		encode_long(DataLen, SizeBytes),
		append([Magic, MetaBytes, SyncMarker, CountBytes, SizeBytes, DataBytes, SyncMarker], Bytes).

	% Varint encoding for long (zig-zag encoding)
	encode_long(N, Bytes) :-
		ZigZag is xor((N << 1), (N >> 63)),
		encode_varint(ZigZag, Bytes).

	encode_varint(N, [Byte]) :-
		N < 128,
		!,
		Byte is N /\ 0x7f.
	encode_varint(N, [Byte| Bytes]) :-
		Byte is (N /\ 0x7f) \/ 0x80,
		N1 is N >> 7,
		encode_varint(N1, Bytes).

	% Varint decoding for long
	decode_long(Bytes, Value, Rest) :-
		decode_varint(Bytes, ZigZag, Rest),
		Value is xor((ZigZag >> 1), (-(ZigZag /\ 1))).

	decode_varint([Byte| Rest], Value, Rest) :-
		Byte < 128,
		!,
		Value is Byte.
	decode_varint([Byte| Bytes], Value, Rest) :-
		Byte1 is Byte /\ 0x7f,
		decode_varint(Bytes, Value1, Rest),
		Value is Byte1 \/ (Value1 << 7).

	% Encode int (same as long but 32-bit)
	encode_int(N, Bytes) :-
		encode_long(N, Bytes).

	decode_int(Bytes, Value, Rest) :-
		decode_long(Bytes, Value, Rest).

	% Encode/decode boolean
	encode_boolean(false, [0]).
	encode_boolean(true, [1]).

	decode_boolean([0| Rest], false, Rest).
	decode_boolean([1| Rest], true, Rest).

	% Encode/decode null
	encode_null([]).
	decode_null(Bytes, @null, Bytes).

	% Encode/decode float (4 bytes, little-endian IEEE 754)
	encode_float(Float, [B0, B1, B2, B3]) :-
		float_to_ieee754_32(Float, Bits),
		B0 is Bits /\ 0xff,
		B1 is (Bits >> 8) /\ 0xff,
		B2 is (Bits >> 16) /\ 0xff,
		B3 is (Bits >> 24) /\ 0xff.

	decode_float([B0, B1, B2, B3| Rest], Float, Rest) :-
		Bits is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24),
		ieee754_32_to_float(Bits, Float).

	% Encode/decode double (8 bytes, little-endian IEEE 754)
	encode_double(Double, [B0, B1, B2, B3, B4, B5, B6, B7]) :-
		float_to_ieee754_64(Double, Bits),
		B0 is Bits /\ 0xff,
		B1 is (Bits >> 8) /\ 0xff,
		B2 is (Bits >> 16) /\ 0xff,
		B3 is (Bits >> 24) /\ 0xff,
		B4 is (Bits >> 32) /\ 0xff,
		B5 is (Bits >> 40) /\ 0xff,
		B6 is (Bits >> 48) /\ 0xff,
		B7 is (Bits >> 56) /\ 0xff.

	decode_double([B0, B1, B2, B3, B4, B5, B6, B7| Rest], Double, Rest) :-
		Bits is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24) \/
		        (B4 << 32) \/ (B5 << 40) \/ (B6 << 48) \/ (B7 << 56),
		ieee754_64_to_float(Bits, Double).

	% IEEE 754 conversions (simplified - using Prolog float representation)
	float_to_ieee754_32(Float, Bits) :-
		(	Float =:= 0 ->
			Bits = 0
		;	Float < 0 ->
			Sign = 1,
			AbsFloat is -Float,
			float_to_ieee754_32_unsigned(AbsFloat, UBits),
			Bits is UBits \/ 0x80000000
		;	Sign = 0,
			float_to_ieee754_32_unsigned(Float, Bits)
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

	% Encode/decode bytes
	encode_bytes(Bytes, Encoded) :-
		length(Bytes, Len),
		encode_long(Len, LenBytes),
		append(LenBytes, Bytes, Encoded).

	decode_bytes(Input, Bytes, Rest) :-
		decode_long(Input, Len, Input1),
		take_bytes(Len, Input1, Bytes, Rest).

	% Encode/decode string (UTF-8 bytes)
	encode_string(Atom, Encoded) :-
		atom(Atom), !,
		atom_codes(Atom, Codes),
		encode_bytes(Codes, Encoded).
	encode_string(codes(Codes), Encoded) :-
		encode_bytes(Codes, Encoded).

	decode_string(Input, Atom, Rest) :-
		decode_bytes(Input, Codes, Rest),
		atom_codes(Atom, Codes).

	% Encode/decode fixed
	encode_fixed(Bytes, _, Bytes).

	decode_fixed(Input, Size, Bytes, Rest) :-
		take_bytes(Size, Input, Bytes, Rest).

	% Encode/decode array
	encode_array([], _Schema, [0]).
	encode_array(Items, Schema, Encoded) :-
		Items \== [],
		length(Items, Len),
		encode_long(Len, LenBytes),
		encode_array_items(Items, Schema, ItemBytes),
		append(LenBytes, ItemBytes, Encoded1),
		append(Encoded1, [0], Encoded).

	encode_array_items([], _, []).
	encode_array_items([Item| Items], Schema, Encoded) :-
		encode_data(Schema, Item, ItemBytes),
		encode_array_items(Items, Schema, RestBytes),
		append(ItemBytes, RestBytes, Encoded).

	decode_array(Input, Schema, Items, Rest) :-
		decode_array_blocks(Input, Schema, Items, Rest).

	decode_array_blocks(Input, Schema, Items, Rest) :-
		decode_long(Input, Count, Input1),
		(	Count =:= 0 ->
			Items = [],
			Rest = Input1
		;	Count < 0 ->
			AbsCount is -Count,
			decode_long(Input1, _BlockSize, Input2),
			decode_array_items(AbsCount, Schema, Input2, Items1, Input3),
			decode_array_blocks(Input3, Schema, Items2, Rest),
			append(Items1, Items2, Items)
		;	decode_array_items(Count, Schema, Input1, Items1, Input2),
			decode_array_blocks(Input2, Schema, Items2, Rest),
			append(Items1, Items2, Items)
		).

	decode_array_items(0, _, Input, [], Input) :-
		!.
	decode_array_items(N, Schema, Input, [Item| Items], Rest) :-
		N > 0,
		decode_data(Schema, Input, Item, Input1),
		N1 is N - 1,
		decode_array_items(N1, Schema, Input1, Items, Rest).

	% Encode/decode map
	encode_map([], [0]).
	encode_map(Pairs, Encoded) :-
		Pairs \== [],
		length(Pairs, Len),
		encode_long(Len, LenBytes),
		encode_map_pairs(Pairs, PairBytes),
		append(LenBytes, PairBytes, Encoded1),
		append(Encoded1, [0], Encoded).

	encode_map_pairs([], []).
	encode_map_pairs([Key-Value| Pairs], Encoded) :-
		encode_string(Key, KeyBytes),
		encode_bytes(Value, ValueBytes),
		encode_map_pairs(Pairs, RestBytes),
		append([KeyBytes, ValueBytes, RestBytes], Encoded).

	decode_map(Input, Pairs, Rest) :-
		decode_map_blocks(Input, Pairs, Rest).

	decode_map_blocks(Input, Pairs, Rest) :-
		decode_long(Input, Count, Input1),
		(	Count =:= 0 ->
			Pairs = [],
			Rest = Input1
		;	Count < 0 ->
			AbsCount is -Count,
			decode_long(Input1, _BlockSize, Input2),
			decode_map_pairs(AbsCount, Input2, Pairs1, Input3),
			decode_map_blocks(Input3, Pairs2, Rest),
			append(Pairs1, Pairs2, Pairs)
		;	decode_map_pairs(Count, Input1, Pairs1, Input2),
			decode_map_blocks(Input2, Pairs2, Rest),
			append(Pairs1, Pairs2, Pairs)
		).

	decode_map_pairs(0, Input, [], Input) :-
		!.
	decode_map_pairs(N, Input, [Key-Value| Pairs], Rest) :-
		N > 0,
		decode_string(Input, Key, Input1),
		decode_bytes(Input1, Value, Input2),
		N1 is N - 1,
		decode_map_pairs(N1, Input2, Pairs, Rest).

	% Encode/decode union (index + value)
	encode_union(Index, Schema, Value, Encoded) :-
		encode_long(Index, IndexBytes),
		encode_data(Schema, Value, ValueBytes),
		append(IndexBytes, ValueBytes, Encoded).

	decode_union(Input, Schemas, Value, Rest) :-
		decode_long(Input, Index, Input1),
		nth0(Index, Schemas, Schema),
		decode_data(Schema, Input1, Value, Rest).

	% Encode/decode enum
	encode_enum(Symbol, Symbols, Encoded) :-
		enum_index(Symbol, Symbols, 0, Index),
		encode_int(Index, Encoded).

	enum_index(Symbol, [Symbol|_], Index, Index) :-
		!.
	enum_index(Symbol, [_|Symbols], N, Index) :-
		N1 is N + 1,
		enum_index(Symbol, Symbols, N1, Index).

	decode_enum(Input, Symbols, Symbol, Rest) :-
		decode_int(Input, Index, Rest),
		nth0(Index, Symbols, Symbol).

	% Main encode_data predicate - dispatches based on schema type
	encode_data(null, @null, []).
	encode_data(boolean, Bool, Bytes) :-
		encode_boolean(Bool, Bytes).
	encode_data(int, Int, Bytes) :-
		encode_int(Int, Bytes).
	encode_data(long, Long, Bytes) :-
		encode_long(Long, Bytes).
	encode_data(float, Float, Bytes) :-
		encode_float(Float, Bytes).
	encode_data(double, Double, Bytes) :-
		encode_double(Double, Bytes).
	encode_data(bytes, Bytes, Encoded) :-
		encode_bytes(Bytes, Encoded).
	encode_data(string, String, Bytes) :-
		encode_string(String, Bytes).
	% Complex types are represented as curly terms {Pairs}
	encode_data({Props}, Data, Bytes) :-
		encode_complex_type(Props, Data, Bytes).
	% Arrays in schema (union types)
	encode_data([Type| Types], Data, Bytes) :-
		find_union_match([Type| Types], Data, 0, Index, Schema),
		encode_union(Index, Schema, Data, Bytes).

	encode_complex_type(Props, Data, Bytes) :-
		curly_member(type-Type, Props),
		encode_by_type(Type, Props, Data, Bytes).

	encode_by_type(record, Props, Data, Bytes) :-
		curly_member(fields-Fields, Props),
		encode_record_fields(Fields, Data, Bytes).
	encode_by_type(enum, Props, Symbol, Bytes) :-
		curly_member(symbols-Symbols, Props),
		encode_enum(Symbol, Symbols, Bytes).
	encode_by_type(array, Props, Items, Bytes) :-
		curly_member(items-ItemSchema, Props),
		encode_array(Items, ItemSchema, Bytes).
	encode_by_type(map, Props, Pairs, Bytes) :-
		curly_member(values-ValueSchema, Props),
		encode_map_with_schema(Pairs, ValueSchema, Bytes).
	encode_by_type(fixed, Props, Data, Data) :-
		curly_member(size-Size, Props),
		length(Data, Size).

	encode_record_fields([], _, []).
	encode_record_fields([{FieldProps}| Fields], Data, Bytes) :-
		curly_member(name-FieldName, FieldProps),
		curly_member(type-FieldType, FieldProps),
		get_field_value(Data, FieldName, Value),
		encode_data(FieldType, Value, FieldBytes),
		encode_record_fields(Fields, Data, RestBytes),
		append(FieldBytes, RestBytes, Bytes).

	get_field_value({Pairs}, FieldName, Value) :-
		curly_member(FieldName-Value, Pairs).

	encode_map_with_schema([], _, [0]).
	encode_map_with_schema(Pairs, ValueSchema, Encoded) :-
		Pairs \== [],
		length(Pairs, Len),
		encode_long(Len, LenBytes),
		encode_map_pairs_with_schema(Pairs, ValueSchema, PairBytes),
		append(LenBytes, PairBytes, Encoded1),
		append(Encoded1, [0], Encoded).

	encode_map_pairs_with_schema([], _, []).
	encode_map_pairs_with_schema([Key-Value| Pairs], ValueSchema, Encoded) :-
		encode_string(Key, KeyBytes),
		encode_data(ValueSchema, Value, ValueBytes),
		encode_map_pairs_with_schema(Pairs, ValueSchema, RestBytes),
		append([KeyBytes, ValueBytes, RestBytes], Encoded).

	find_union_match([Schema|_], Data, Index, Index, Schema) :-
		data_matches_schema(Schema, Data), !.
	find_union_match([_|Schemas], Data, N, Index, Schema) :-
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

	% decode_data/3 - decode with schema, consuming all bytes
	decode_data(Schema, Bytes, Data) :-
		decode_data(Schema, Bytes, Data, []).

	% decode_data/4 - decode with schema, returning remaining bytes
	decode_data(null, Input, @null, Input).
	decode_data(boolean, Input, Bool, Rest) :-
		decode_boolean(Input, Bool, Rest).
	decode_data(int, Input, Int, Rest) :-
		decode_int(Input, Int, Rest).
	decode_data(long, Input, Long, Rest) :-
		decode_long(Input, Long, Rest).
	decode_data(float, Input, Float, Rest) :-
		decode_float(Input, Float, Rest).
	decode_data(double, Input, Double, Rest) :-
		decode_double(Input, Double, Rest).
	decode_data(bytes, Input, Bytes, Rest) :-
		decode_bytes(Input, Bytes, Rest).
	decode_data(string, Input, String, Rest) :-
		decode_string(Input, String, Rest).
	% Complex types
	decode_data({Props}, Input, Data, Rest) :-
		decode_complex_type(Props, Input, Data, Rest).
	% Union types
	decode_data([Type| Types], Input, Data, Rest) :-
		decode_union(Input, [Type| Types], Data, Rest).

	decode_complex_type(Props, Input, Data, Rest) :-
		curly_member(type-Type, Props),
		decode_by_type(Type, Props, Input, Data, Rest).

	decode_by_type(record, Props, Input, {FieldPairs}, Rest) :-
		curly_member(fields-Fields, Props),
		decode_record_fields(Fields, Input, FieldPairsList, Rest),
		list_to_curly(FieldPairsList, FieldPairs).
	decode_by_type(enum, Props, Input, Symbol, Rest) :-
		curly_member(symbols-Symbols, Props),
		decode_enum(Input, Symbols, Symbol, Rest).
	decode_by_type(array, Props, Input, Items, Rest) :-
		curly_member(items-ItemSchema, Props),
		decode_array(Input, ItemSchema, Items, Rest).
	decode_by_type(map, Props, Input, Pairs, Rest) :-
		curly_member(values-ValueSchema, Props),
		decode_map_with_schema(Input, ValueSchema, Pairs, Rest).
	decode_by_type(fixed, Props, Input, Data, Rest) :-
		curly_member(size-Size, Props),
		decode_fixed(Input, Size, Data, Rest).

	decode_record_fields([], Input, [], Input).
	decode_record_fields([{FieldProps}| Fields], Input, [FieldName-Value| Pairs], Rest) :-
		curly_member(name-FieldName, FieldProps),
		curly_member(type-FieldType, FieldProps),
		decode_data(FieldType, Input, Value, Input1),
		decode_record_fields(Fields, Input1, Pairs, Rest).

	decode_map_with_schema(Input, ValueSchema, Pairs, Rest) :-
		decode_map_blocks_with_schema(Input, ValueSchema, Pairs, Rest).

	decode_map_blocks_with_schema(Input, ValueSchema, Pairs, Rest) :-
		decode_long(Input, Count, Input1),
		(	Count =:= 0 ->
			Pairs = [],
			Rest = Input1
		;	Count < 0 ->
			AbsCount is -Count,
			decode_long(Input1, _BlockSize, Input2),
			decode_map_pairs_with_schema(AbsCount, ValueSchema, Input2, Pairs1, Input3),
			decode_map_blocks_with_schema(Input3, ValueSchema, Pairs2, Rest),
			append(Pairs1, Pairs2, Pairs)
		;	decode_map_pairs_with_schema(Count, ValueSchema, Input1, Pairs1, Input2),
			decode_map_blocks_with_schema(Input2, ValueSchema, Pairs2, Rest),
			append(Pairs1, Pairs2, Pairs)
		).

	decode_map_pairs_with_schema(0, _, Input, [], Input) :-
		!.
	decode_map_pairs_with_schema(N, ValueSchema, Input, [Key-Value| Pairs], Rest) :-
		N > 0,
		decode_string(Input, Key, Input1),
		decode_data(ValueSchema, Input1, Value, Input2),
		N1 is N - 1,
		decode_map_pairs_with_schema(N1, ValueSchema, Input2, Pairs, Rest).

	% Helper to find a pair in a curly term's comma-separated pairs
	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

	% Helper to convert list of pairs to comma-separated pairs for curly terms
	list_to_curly([Pair], Pair) :-
		!.
	list_to_curly([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly(Pairs, Rest).

:- end_object.
