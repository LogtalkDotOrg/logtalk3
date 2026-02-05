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
		date is 2026-02-04,
		comment is 'Unit tests for the "protobuf" library.'
	]).

	:- uses(protobuf, [
		parse/2, parse/3, generate/3, generate/4
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(json(curly, dash, atom), [
		parse/2 as json_parse/2
	]).

	:- uses(list, [
		append/3
	]).

	cover(protobuf).

	condition :-
		current_prolog_flag(bounded, false).

	% Varint encoding tests

	test(protobuf_varint_zero, true(Data == 0)) :-
		generate(bytes(Bytes), int32, 0),
		parse(bytes(Bytes), int32, Data).

	test(protobuf_varint_small, true(Data == 150)) :-
		generate(bytes(Bytes), int32, 150),
		parse(bytes(Bytes), int32, Data).

	test(protobuf_varint_large, true(Data == 300)) :-
		generate(bytes(Bytes), int32, 300),
		parse(bytes(Bytes), int32, Data).

	% int32 encoding/decoding tests

	test(protobuf_int32_zero, true(Data == 0)) :-
		generate(bytes(Bytes), int32, 0),
		parse(bytes(Bytes), int32, Data).

	test(protobuf_int32_positive, true(Data == 42)) :-
		generate(bytes(Bytes), int32, 42),
		parse(bytes(Bytes), int32, Data).

	test(protobuf_int32_large, true(Data == 1000000)) :-
		generate(bytes(Bytes), int32, 1000000),
		parse(bytes(Bytes), int32, Data).

	% int64 encoding/decoding tests

	test(protobuf_int64_zero, true(Data == 0)) :-
		generate(bytes(Bytes), int64, 0),
		parse(bytes(Bytes), int64, Data).

	test(protobuf_int64_positive, true(Data == 123456789)) :-
		generate(bytes(Bytes), int64, 123456789),
		parse(bytes(Bytes), int64, Data).

	% uint32 encoding/decoding tests

	test(protobuf_uint32_zero, true(Data == 0)) :-
		generate(bytes(Bytes), uint32, 0),
		parse(bytes(Bytes), uint32, Data).

	test(protobuf_uint32_positive, true(Data == 999)) :-
		generate(bytes(Bytes), uint32, 999),
		parse(bytes(Bytes), uint32, Data).

	% sint32 encoding/decoding tests (ZigZag)

	test(protobuf_sint32_zero, true(Data == 0)) :-
		generate(bytes(Bytes), sint32, 0),
		parse(bytes(Bytes), sint32, Data).

	test(protobuf_sint32_positive, true(Data == 42)) :-
		generate(bytes(Bytes), sint32, 42),
		parse(bytes(Bytes), sint32, Data).

	test(protobuf_sint32_negative, true(Data == -42)) :-
		generate(bytes(Bytes), sint32, -42),
		parse(bytes(Bytes), sint32, Data).

	% sint64 encoding/decoding tests (ZigZag)

	test(protobuf_sint64_positive, true(Data == 1000)) :-
		generate(bytes(Bytes), sint64, 1000),
		parse(bytes(Bytes), sint64, Data).

	test(protobuf_sint64_negative, true(Data == -1000)) :-
		generate(bytes(Bytes), sint64, -1000),
		parse(bytes(Bytes), sint64, Data).

	% Boolean encoding/decoding tests

	test(protobuf_bool_true, true(Data == true)) :-
		generate(bytes(Bytes), bool, true),
		parse(bytes(Bytes), bool, Data).

	test(protobuf_bool_false, true(Data == false)) :-
		generate(bytes(Bytes), bool, false),
		parse(bytes(Bytes), bool, Data).

	% Fixed32 encoding/decoding tests

	test(protobuf_fixed32_zero, true(Data == 0)) :-
		generate(bytes(Bytes), fixed32, 0),
		parse(bytes(Bytes), fixed32, Data).

	test(protobuf_fixed32_value, true(Data == 123456)) :-
		generate(bytes(Bytes), fixed32, 123456),
		parse(bytes(Bytes), fixed32, Data).

	% Fixed64 encoding/decoding tests

	test(protobuf_fixed64_zero, true(Data == 0)) :-
		generate(bytes(Bytes), fixed64, 0),
		parse(bytes(Bytes), fixed64, Data).

	test(protobuf_fixed64_value, true(Data == 9876543210)) :-
		generate(bytes(Bytes), fixed64, 9876543210),
		parse(bytes(Bytes), fixed64, Data).

	% Float encoding/decoding tests

	test(protobuf_float_zero, true(Data =~= 0.0)) :-
		generate(bytes(Bytes), float, 0.0),
		parse(bytes(Bytes), float, Data).

	test(protobuf_float_positive, true(Data =~= 3.14)) :-
		generate(bytes(Bytes), float, 3.14),
		parse(bytes(Bytes), float, Data).

	% Double encoding/decoding tests

	test(protobuf_double_zero, true(Data =~= 0.0)) :-
		generate(bytes(Bytes), double, 0.0),
		parse(bytes(Bytes), double, Data).

	test(protobuf_double_positive, true(Data =~= 3.14159265359)) :-
		generate(bytes(Bytes), double, 3.14159265359),
		parse(bytes(Bytes), double, Data).

	test(protobuf_double_negative, true(Data =~= -273.15)) :-
		generate(bytes(Bytes), double, -273.15),
		parse(bytes(Bytes), double, Data).

	% String encoding/decoding tests

	test(protobuf_string_empty, true(Data == '')) :-
		generate(bytes(Bytes), string, ''),
		parse(bytes(Bytes), string, Data).

	test(protobuf_string_hello, true(Data == testing)) :-
		generate(bytes(Bytes), string, testing),
		parse(bytes(Bytes), string, Data).

	test(protobuf_string_with_spaces, true(Data == 'Hello World')) :-
		generate(bytes(Bytes), string, 'Hello World'),
		parse(bytes(Bytes), string, Data).

	% Bytes encoding/decoding tests

	test(protobuf_bytes_empty, true(Data == [])) :-
		generate(bytes(Bytes), bytes, []),
		parse(bytes(Bytes), bytes, Data).

	test(protobuf_bytes_data, true(Data == [0x48, 0x65, 0x6c, 0x6c, 0x6f])) :-
		generate(bytes(Bytes), bytes, [0x48, 0x65, 0x6c, 0x6c, 0x6f]),
		parse(bytes(Bytes), bytes, Data).

	% Message encoding/decoding tests

	test(protobuf_message_simple, true(Data == [name-'John', id-42])) :-
		Schema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		generate(bytes(Bytes), Schema, [name-'John', id-42]),
		parse(bytes(Bytes), Schema, Data).

	test(protobuf_message_optional_field, true(Data == [name-'Alice'])) :-
		Schema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		generate(bytes(Bytes), Schema, [name-'Alice']),
		parse(bytes(Bytes), Schema, Data).

	% Round-trip tests - generate then parse should return original data

	test(protobuf_roundtrip_int32, true(Data2 == Data1)) :-
		Data1 = 12345,
		Schema = int32,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_sint32_negative, true(Data2 == Data1)) :-
		Data1 = -9999,
		Schema = sint32,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_string, true(Data2 == Data1)) :-
		Data1 = 'Protocol Buffers',
		Schema = string,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_bool, true(Data2 == Data1)) :-
		Data1 = true,
		Schema = bool,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_float, true(Data2 =~= Data1)) :-
		Data1 = 2.71828,
		Schema = float,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_double, true(Data2 =~= Data1)) :-
		Data1 = 1.41421356237,
		Schema = double,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_bytes, true(Data2 == Data1)) :-
		Data1 = [0x01, 0x02, 0x03, 0x04, 0x05],
		Schema = bytes,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Message round-trip tests

	test(protobuf_roundtrip_message, true(Data2 == Data1)) :-
		Schema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32},
			{number-3, name-email, type-string}
		]},
		Data1 = [name-'Bob', id-99, email-'bob@example.com'],
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(protobuf_roundtrip_message_mixed_types, true(Data2 == Data1)) :-
		Schema = {message-'Data', fields-[
			{number-1, name-flag, type-bool},
			{number-2, name-count, type-sint32},
			{number-3, name-label, type-string},
			{number-4, name-value, type-double}
		]},
		Data1 = [flag-true, count-(-50), label-test, value-98.6],
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Round-trip test with schema included in generated data

	test(protobuf_roundtrip_with_schema, true(Schema2-Data2 == Schema1-Data1)) :-
		Schema1 = {message-'User', fields-[
			{number-1, name-username, type-string},
			{number-2, name-age, type-uint32}
		]},
		Data1 = [username-'alice', age-30],
		generate(bytes(Bytes), true, Schema1, Data1),
		parse(bytes(Bytes), Schema2-Data2).

	% Round-trip test without schema in generated data

	test(protobuf_roundtrip_without_schema, true(Data2 == Data1)) :-
		Schema = {message-'User', fields-[
			{number-1, name-username, type-string},
			{number-2, name-age, type-uint32}
		]},
		Data1 = [username-'bob', age-25],
		generate(bytes(Bytes), false, Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Tests using person schema from test files

	test(protobuf_person_schema, true(Data2 == Data1)) :-
		person_schema(Schema),
		Data1 = [name-'Charlie', id-777, email-'charlie@test.org'],
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Tests using addressbook schema from test files

	test(protobuf_addressbook_simple, true(Data2 == Data1)) :-
		addressbook_schema(Schema),
		Data1 = [owner-'TestUser'],
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% uint64 encoding/decoding tests

	test(protobuf_uint64_zero, true(Data == 0)) :-
		generate(bytes(Bytes), uint64, 0),
		parse(bytes(Bytes), uint64, Data).

	test(protobuf_uint64_large, true(Data == 9876543210)) :-
		generate(bytes(Bytes), uint64, 9876543210),
		parse(bytes(Bytes), uint64, Data).

	% sfixed32 encoding/decoding tests (signed fixed-width 32-bit)

	test(protobuf_sfixed32_zero, true(Data == 0)) :-
		generate(bytes(Bytes), sfixed32, 0),
		parse(bytes(Bytes), sfixed32, Data).

	test(protobuf_sfixed32_positive, true(Data == 12345)) :-
		generate(bytes(Bytes), sfixed32, 12345),
		parse(bytes(Bytes), sfixed32, Data).

	test(protobuf_sfixed32_negative, true(Data == -54321)) :-
		generate(bytes(Bytes), sfixed32, -54321),
		parse(bytes(Bytes), sfixed32, Data).

	% sfixed64 encoding/decoding tests (signed fixed-width 64-bit)

	test(protobuf_sfixed64_zero, true(Data == 0)) :-
		generate(bytes(Bytes), sfixed64, 0),
		parse(bytes(Bytes), sfixed64, Data).

	test(protobuf_sfixed64_positive, true(Data == 123456789012)) :-
		generate(bytes(Bytes), sfixed64, 123456789012),
		parse(bytes(Bytes), sfixed64, Data).

	test(protobuf_sfixed64_negative, true(Data == -987654321098)) :-
		generate(bytes(Bytes), sfixed64, -987654321098),
		parse(bytes(Bytes), sfixed64, Data).

	% Message with fixed32 field (wire type 5)

	test(protobuf_message_fixed32_field, true(Data == [value-42])) :-
		Schema = {message-'FixedMsg', fields-[
			{number-1, name-value, type-fixed32}
		]},
		generate(bytes(Bytes), Schema, [value-42]),
		parse(bytes(Bytes), Schema, Data).

	% Message with fixed64 field (wire type 1)

	test(protobuf_message_fixed64_field, true(Data == [value-9876543210])) :-
		Schema = {message-'Fixed64Msg', fields-[
			{number-1, name-value, type-fixed64}
		]},
		generate(bytes(Bytes), Schema, [value-9876543210]),
		parse(bytes(Bytes), Schema, Data).

	% Message with double field (wire type 1)

	test(protobuf_message_double_field, true(Value =~= 3.14159)) :-
		Schema = {message-'DoubleMsg', fields-[
			{number-1, name-value, type-double}
		]},
		generate(bytes(Bytes), Schema, [value-3.14159]),
		parse(bytes(Bytes), Schema, Data),
		Data = [value-Value].

	% Message with float field (wire type 5)

	test(protobuf_message_float_field, true(Value =~= 2.5)) :-
		Schema = {message-'FloatMsg', fields-[
			{number-1, name-value, type-float}
		]},
		generate(bytes(Bytes), Schema, [value-2.5]),
		parse(bytes(Bytes), Schema, Data),
		Data = [value-Value].

	% Message with sfixed32 field

	test(protobuf_message_sfixed32_field, true(Data == [value- -100])) :-
		Schema = {message-'SFixed32Msg', fields-[
			{number-1, name-value, type-sfixed32}
		]},
		generate(bytes(Bytes), Schema, [value- -100]),
		parse(bytes(Bytes), Schema, Data).

	% Message with sfixed64 field

	test(protobuf_message_sfixed64_field, true(Data == [value- -123456789])) :-
		Schema = {message-'SFixed64Msg', fields-[
			{number-1, name-value, type-sfixed64}
		]},
		generate(bytes(Bytes), Schema, [value- -123456789]),
		parse(bytes(Bytes), Schema, Data).

	% Message with uint64 field

	test(protobuf_message_uint64_field, true(Data == [count-9999999999])) :-
		Schema = {message-'Uint64Msg', fields-[
			{number-1, name-count, type-uint64}
		]},
		generate(bytes(Bytes), Schema, [count-9999999999]),
		parse(bytes(Bytes), Schema, Data).

	% Message with bytes field

	test(protobuf_message_bytes_field, true(Data == [data-[1,2,3,4,5]])) :-
		Schema = {message-'BytesMsg', fields-[
			{number-1, name-data, type-bytes}
		]},
		generate(bytes(Bytes), Schema, [data-[1,2,3,4,5]]),
		parse(bytes(Bytes), Schema, Data).

	% File I/O tests

	test(protobuf_file_roundtrip, true(Data2 == Data1)) :-
		^^file_path('test_output.bin', Path),
		Schema = {message-'TestMsg', fields-[
			{number-1, name-name, type-string},
			{number-2, name-value, type-int32}
		]},
		Data1 = [name-'FileTest', value-42],
		generate(file(Path), Schema, Data1),
		parse(file(Path), Schema, Data2).

	% Stream I/O tests

	test(protobuf_stream_write_read, true(Data2 == Data1)) :-
		^^file_path('test_stream.bin', Path),
		Schema = {message-'StreamMsg', fields-[
			{number-1, name-label, type-string}
		]},
		Data1 = [label-'StreamTest'],
		open(Path, write, WriteStream, [type(binary)]),
		generate(stream(WriteStream), Schema, Data1),
		close(WriteStream),
		open(Path, read, ReadStream, [type(binary)]),
		parse(stream(ReadStream), Schema, Data2),
		close(ReadStream).

	% Unknown field skip tests (tests skip_field predicate)

	test(protobuf_skip_unknown_varint_field, true(Data == [name-'Test'])) :-
		% Manually craft bytes with an unknown field (field 99, wire type 0 varint)
		% Field 1 string "Test": tag=0x0A (field 1, wire type 2), len=4, "Test"
		% Field 99 varint 123: tag=0x318 (99<<3|0), but varint so: 0xF8 0x06, value 123
		% [10, 4, 84, 101, 115, 116] for field 1
		% [248, 6, 123] for unknown field 99 (varint)
		KnownBytes = [10, 4, 84, 101, 115, 116],
		UnknownField = [248, 6, 123],
		append(UnknownField, KnownBytes, Bytes),
		Schema = {message-'SkipTest', fields-[
			{number-1, name-name, type-string}
		]},
		parse(bytes(Bytes), Schema, Data).

	test(protobuf_skip_unknown_64bit_field, true(Data == [id-42])) :-
		% Field 2 int32 42: tag=0x10, value=42
		% Unknown field 50 (wire type 1, 64-bit): tag=0x91 0x03, 8 bytes of data
		KnownBytes = [16, 42],
		UnknownTag = [145, 3],  % field 50, wire type 1
		UnknownData = [1, 2, 3, 4, 5, 6, 7, 8],
		append(UnknownTag, UnknownData, UnknownField),
		append(UnknownField, KnownBytes, Bytes),
		Schema = {message-'Skip64Test', fields-[
			{number-2, name-id, type-int32}
		]},
		parse(bytes(Bytes), Schema, Data).

	test(protobuf_skip_unknown_length_delimited_field, true(Data == [count-100])) :-
		% Field 3 int32 100: tag=0x18, value=100
		% Unknown field 77 (wire type 2, length-delimited): tag=0xEA 0x04, len=3, data
		KnownBytes = [24, 100],
		UnknownTag = [234, 4],  % field 77, wire type 2
		UnknownLen = [3],
		UnknownData = [65, 66, 67],
		append(UnknownTag, UnknownLen, T1),
		append(T1, UnknownData, UnknownField),
		append(UnknownField, KnownBytes, Bytes),
		Schema = {message-'SkipLenTest', fields-[
			{number-3, name-count, type-int32}
		]},
		parse(bytes(Bytes), Schema, Data).

	test(protobuf_skip_unknown_32bit_field, true(Data == [flag-true])) :-
		% Field 4 bool true: tag=0x20, value=1
		% Unknown field 88 (wire type 5, 32-bit): tag=0xC5 0x05, 4 bytes
		KnownBytes = [32, 1],
		UnknownTag = [197, 5],  % field 88, wire type 5
		UnknownData = [10, 20, 30, 40],
		append(UnknownTag, UnknownData, UnknownField),
		append(UnknownField, KnownBytes, Bytes),
		Schema = {message-'Skip32Test', fields-[
			{number-4, name-flag, type-bool}
		]},
		parse(bytes(Bytes), Schema, Data).

	% Negative float test

	test(protobuf_float_negative, true(Value =~= -1.5)) :-
		generate(bytes(Bytes), float, -1.5),
		parse(bytes(Bytes), float, Value).

	% Error handling tests

	test(protobuf_parse_invalid_source, error(domain_error(protobuf_source,invalid))) :-
		parse(invalid, int32, _).

	test(protobuf_generate_invalid_sink, error(domain_error(protobuf_sink,invalid))) :-
		generate(invalid, int32, 42).

	test(protobuf_parse_instantiation_error, error(instantiation_error)) :-
		parse(_, int32, _).

	test(protobuf_generate_instantiation_error, error(instantiation_error)) :-
		generate(_, int32, 42).

	% Nested message tests

	test(protobuf_nested_message_simple, true(Data == [person-[name-'Alice', id-25]])) :-
		InnerSchema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		OuterSchema = {message-'Wrapper', fields-[
			{number-1, name-person, type-InnerSchema}
		]},
		generate(bytes(Bytes), OuterSchema, [person-[name-'Alice', id-25]]),
		parse(bytes(Bytes), OuterSchema, Data).

	test(protobuf_nested_message_multiple_fields, true(Data == [label-'test', person-[name-'Bob', id-42], value-100])) :-
		InnerSchema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		OuterSchema = {message-'Outer', fields-[
			{number-1, name-label, type-string},
			{number-2, name-person, type-InnerSchema},
			{number-3, name-value, type-int32}
		]},
		generate(bytes(Bytes), OuterSchema, [label-'test', person-[name-'Bob', id-42], value-100]),
		parse(bytes(Bytes), OuterSchema, Data).

	test(protobuf_deeply_nested_message, true(Data == [wrapper-[person-[name-'Deep', id-99]]])) :-
		PersonSchema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		WrapperSchema = {message-'Wrapper', fields-[
			{number-1, name-person, type-PersonSchema}
		]},
		OuterSchema = {message-'Outer', fields-[
			{number-1, name-wrapper, type-WrapperSchema}
		]},
		generate(bytes(Bytes), OuterSchema, [wrapper-[person-[name-'Deep', id-99]]]),
		parse(bytes(Bytes), OuterSchema, Data).

	test(protobuf_nested_with_optional_field, true(Data == [person-[name-'Charlie']])) :-
		InnerSchema = {message-'Person', fields-[
			{number-1, name-name, type-string},
			{number-2, name-id, type-int32}
		]},
		OuterSchema = {message-'Wrapper', fields-[
			{number-1, name-person, type-InnerSchema}
		]},
		% Nested message with optional field omitted
		generate(bytes(Bytes), OuterSchema, [person-[name-'Charlie']]),
		parse(bytes(Bytes), OuterSchema, Data).

	% Helper predicates

	% Helper to get the person schema
	person_schema(Schema) :-
		^^file_path('person.json', Path),
		json_parse(file(Path), Schema).

	% Helper to get the addressbook schema
	addressbook_schema(Schema) :-
		^^file_path('addressbook.json', Path),
		json_parse(file(Path), Schema).

:- end_object.
