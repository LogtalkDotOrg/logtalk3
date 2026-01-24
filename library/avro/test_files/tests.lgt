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
		date is 2026-01-23,
		comment is 'Unit tests for the "avro" library.'
	]).

	:- uses(avro, [
		parse/2, parse/3, generate/3, generate/4
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(json(curly, dash, atom), [
		parse/2 as json_parse/2
	]).

	:- uses(json_lines(curly, dash, atom), [
		parse/2 as json_lines_parse/2
	]).

	cover(avro).

	condition :-
		current_prolog_flag(bounded, false).

	% Integer encoding/decoding tests (varint with zig-zag encoding)

	test(avro_int_zero, true(Data == 0)) :-
		generate(bytes(Bytes), int, 0),
		parse(bytes(Bytes), int, Data).

	test(avro_int_positive, true(Data == 42)) :-
		generate(bytes(Bytes), int, 42),
		parse(bytes(Bytes), int, Data).

	test(avro_int_negative, true(Data == -42)) :-
		generate(bytes(Bytes), int, -42),
		parse(bytes(Bytes), int, Data).

	test(avro_long_large, true(Data == 1000000)) :-
		generate(bytes(Bytes), long, 1000000),
		parse(bytes(Bytes), long, Data).

	test(avro_long_negative_large, true(Data == -1000000)) :-
		generate(bytes(Bytes), long, -1000000),
		parse(bytes(Bytes), long, Data).

	% Boolean encoding/decoding tests

	test(avro_boolean_true, true(Data == true)) :-
		generate(bytes(Bytes), boolean, true),
		parse(bytes(Bytes), boolean, Data).

	test(avro_boolean_false, true(Data == false)) :-
		generate(bytes(Bytes), boolean, false),
		parse(bytes(Bytes), boolean, Data).

	% Null encoding/decoding tests

	test(avro_null, true(Data == @null)) :-
		generate(bytes(Bytes), null, @null),
		parse(bytes(Bytes), null, Data).

	% String encoding/decoding tests

	test(avro_string_empty, true(Data == '')) :-
		generate(bytes(Bytes), string, ''),
		parse(bytes(Bytes), string, Data).

	test(avro_string_hello, true(Data == hello)) :-
		generate(bytes(Bytes), string, hello),
		parse(bytes(Bytes), string, Data).

	% Bytes encoding/decoding tests

	test(avro_bytes_empty, true(Data == [])) :-
		generate(bytes(Bytes), bytes, []),
		parse(bytes(Bytes), bytes, Data).

	test(avro_bytes_data, true(Data == [0x01, 0x02, 0x03, 0x04])) :-
		generate(bytes(Bytes), bytes, [0x01, 0x02, 0x03, 0x04]),
		parse(bytes(Bytes), bytes, Data).

	% Float encoding/decoding tests

	test(avro_float_zero, true(Data =~= 0.0)) :-
		generate(bytes(Bytes), float, 0.0),
		parse(bytes(Bytes), float, Data).

	test(avro_float_positive, true(Data =~= 3.14)) :-
		generate(bytes(Bytes), float, 3.14),
		parse(bytes(Bytes), float, Data).

	% Double encoding/decoding tests

	test(avro_double_zero, true(Data =~= 0.0)) :-
		generate(bytes(Bytes), double, 0.0),
		parse(bytes(Bytes), double, Data).

	test(avro_double_positive, true(Data =~= 3.14159265359)) :-
		generate(bytes(Bytes), double, 3.14159265359),
		parse(bytes(Bytes), double, Data).

	test(avro_double_negative, true(Data =~= -273.15)) :-
		generate(bytes(Bytes), double, -273.15),
		parse(bytes(Bytes), double, Data).

	% Array encoding/decoding tests

	test(avro_array_empty, true(Data == [])) :-
		Schema = {type-array, items-int},
		generate(bytes(Bytes), Schema, []),
		parse(bytes(Bytes), Schema, Data).

	test(avro_array_ints, true(Data == [1, 2, 3])) :-
		Schema = {type-array, items-int},
		generate(bytes(Bytes), Schema, [1, 2, 3]),
		parse(bytes(Bytes), Schema, Data).

	% Round-trip tests - generate then parse should return original data

	test(avro_roundtrip_int, true(Data2 == Data1)) :-
		Data1 = 12345,
		Schema = int,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(avro_roundtrip_string, true(Data2 == Data1)) :-
		Data1 = 'Hello World',
		Schema = string,
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	test(avro_roundtrip_array, true(Data2 == Data1)) :-
		Data1 = [10, 20, 30, 40, 50],
		Schema = {type-array, items-long},
		generate(bytes(Bytes), Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Round-trip tests using user.avsc schema and user.jsonl data

	% Round-trip test with schema included in generated file
	test(avro_roundtrip_user_with_schema, true(Schema2-Data2 == Schema1-Data1)) :-
		user_schema(Schema1),
		user_data(Data1),
		generate(bytes(Bytes), true, Schema1, Data1),
		parse(bytes(Bytes), Schema2-Data2).

	% Round-trip test without schema in generated file
	test(avro_roundtrip_user_without_schema, true(Data2 == Data1)) :-
		user_schema(Schema),
		user_data(Data1),
		generate(bytes(Bytes), false, Schema, Data1),
		parse(bytes(Bytes), Schema, Data2).

	% Helper predicates

	% Helper to get the user schema by reading from user.avsc file
	user_schema(Schema) :-
		^^file_path('user.avsc', Path),
		json_parse(file(Path), Schema).

	% Helper to get user data by reading from user.jsonl file (returns each record on backtracking)
	user_data(Data) :-
		^^file_path('user.jsonl', Path),
		json_lines_parse(file(Path), Records),
		list::member(Data, Records).

:- end_object.
