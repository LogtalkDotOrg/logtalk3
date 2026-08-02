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
		date is 2026-08-02,
		comment is 'Unit tests for the "bson" library.'
	]).

	cover(bson(_)).
	cover(bson).

	test(bson_empty_document, deterministic(Bytes == [5,0,0,0,0])) :-
		bson::generate({}, Bytes).

	test(bson_empty_document_parse, deterministic(Document == {})) :-
		bson::parse([5,0,0,0,0], Document).

	test(bson_scalar_document, deterministic(Document == {hello-world, answer-int32(42), ok - @true})) :-
		bson::generate({hello-world, answer-int32(42), ok - @true}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_integer_widths, deterministic(Document == {small-int32(1), large-int64(1)})) :-
		bson::generate({small-int32(1), large-int64(1)}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_array_round_trip, deterministic(Document == {values-[int32(1),a,@null]})) :-
		bson::generate({values-[int32(1),a,@null]}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_codes_representation, deterministic(Document == {codes([107])-codes([118])})) :-
		bson(codes)::generate({codes([107])-codes([118])}, Bytes),
		bson(codes)::parse(Bytes, Document).

	test(bson_invalid_document_length, error(domain_error(bson_byte_sequence, [6,0,0,0,0]))) :-
		bson::parse([6,0,0,0,0], _).

	test(bson_trailing_bytes, error(domain_error(bson_byte_sequence, [5,0,0,0,0,0]))) :-
		bson::parse([5,0,0,0,0,0], _).

	test(bson_plain_integer_uses_int32, deterministic(Bytes == [12,0,0,0,16,110,0,42,0,0,0,0])) :-
		bson::generate({n-42}, Bytes).

	test(bson_plain_integer_uses_int64, deterministic(Bytes == [16,0,0,0,18,110,0,0,0,0,128,0,0,0,0,0])) :-
		bson::generate({n-2147483648}, Bytes).

	test(bson_binary_generic, deterministic(Document == {data-binary(0, bytes([1,2,3]))})) :-
		bson::generate({data-binary(0, bytes([1,2,3]))}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_binary_old, deterministic(Bytes == [20,0,0,0,5,98,0,7,0,0,0,2,3,0,0,0,1,2,3,0])) :-
		bson::generate({b-binary(2, bytes([1,2,3]))}, Bytes).

	test(bson_binary_old_parse, deterministic(Document == {b-binary(2, bytes([1,2,3]))})) :-
		bson::parse([20,0,0,0,5,98,0,7,0,0,0,2,3,0,0,0,1,2,3,0], Document).

	test(bson_binary_user_defined, deterministic(Document == {data-binary(255, bytes([42]))})) :-
		bson::generate({data-binary(255, bytes([42]))}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_object_id, deterministic(Document == {id-object_id(bytes([0,1,2,3,4,5,6,7,8,9,10,11]))})) :-
		bson::generate({id-object_id(bytes([0,1,2,3,4,5,6,7,8,9,10,11]))}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_date_time, deterministic(Document == {created-date_time(-1)})) :-
		bson::generate({created-date_time(-1)}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_regular_expression, deterministic(Document == {pattern-regular_expression('^a', imsux)})) :-
		bson::generate({pattern-regular_expression('^a', imsux)}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_db_pointer, deterministic(Document == {ref-db_pointer(collection, object_id(bytes([0,1,2,3,4,5,6,7,8,9,10,11])))})) :-
		bson::generate({ref-db_pointer(collection, object_id(bytes([0,1,2,3,4,5,6,7,8,9,10,11])))}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_javascript, deterministic(Document == {code-javascript('return 1')})) :-
		bson::generate({code-javascript('return 1')}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_symbol, deterministic(Document == {name-symbol(foo)})) :-
		bson::generate({name-symbol(foo)}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_javascript_scope, deterministic(Document == {code-javascript('return x', {x-int32(1)})})) :-
		bson::generate({code-javascript('return x', {x-int32(1)})}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_timestamp_order, deterministic(Bytes == [16,0,0,0,17,116,0,4,3,2,1,8,7,6,5,0])) :-
		bson::generate({t-timestamp(0x01020304, 0x05060708)}, Bytes).

	test(bson_timestamp_parse, deterministic(Document == {t-timestamp(0x01020304, 0x05060708)})) :-
		bson::parse([16,0,0,0,17,116,0,4,3,2,1,8,7,6,5,0], Document).

	test(bson_decimal128, deterministic(Document == {decimal-decimal128(bytes([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]))})) :-
		bson::generate({decimal-decimal128(bytes([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]))}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_deprecated_and_key_values, deterministic(Document == {u - @undefined, minimum - @min_key, maximum - @max_key})) :-
		bson::generate({u - @undefined, minimum - @min_key, maximum - @max_key}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_double_special_values, deterministic(Document == {positive - @infinity, negative - @negative_infinity, nan - @not_a_number})) :-
		bson::generate({positive - @infinity, negative - @negative_infinity, nan - @not_a_number}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_double_nan_payload, deterministic(Bytes == Bytes0)) :-
		Bytes0 = [16,0,0,0,1,110,0,1,0,0,0,0,0,248,127,0],
		bson::parse(Bytes0, Document),
		bson::generate(Document, Bytes).

	test(bson_embedded_nul_string, deterministic(Document == {codes([118,97,108,117,101])-codes([97,0,98])})) :-
		bson(codes)::generate({value-codes([97,0,98])}, Bytes),
		bson(codes)::parse(Bytes, Document).

	test(bson_duplicate_keys_preserved, deterministic(Document == {a-int32(1), a-int32(2)})) :-
		bson::generate({a-int32(1), a-int32(2)}, Bytes),
		bson::parse(Bytes, Document).

	test(bson_invalid_boolean, error(domain_error(bson_byte_sequence, [9,0,0,0,8,98,0,2,0]))) :-
		bson::parse([9,0,0,0,8,98,0,2,0], _).

	test(bson_invalid_binary_subtype, error(domain_error(bson_byte_sequence, [14,0,0,0,5,98,0,1,0,0,0,10,42,0]))) :-
		bson::parse([14,0,0,0,5,98,0,1,0,0,0,10,42,0], _).

	test(bson_invalid_old_binary_length, error(domain_error(bson_byte_sequence, [20,0,0,0,5,98,0,7,0,0,0,2,2,0,0,0,1,2,3,0]))) :-
		bson::parse([20,0,0,0,5,98,0,7,0,0,0,2,2,0,0,0,1,2,3,0], _).

	test(bson_invalid_array_key, error(domain_error(bson_byte_sequence, [20,0,0,0,4,97,0,12,0,0,0,16,49,0,1,0,0,0,0,0]))) :-
		bson::parse([20,0,0,0,4,97,0,12,0,0,0,16,49,0,1,0,0,0,0,0], _).

	test(bson_invalid_regex_options_order, error(domain_error(bson_term, {pattern-regular_expression(a, xi)}))) :-
		bson::generate({pattern-regular_expression(a, xi)}, _).

	test(bson_invalid_regex_option, error(domain_error(bson_term, {pattern-regular_expression(a, z)}))) :-
		bson::generate({pattern-regular_expression(a, z)}, _).

	test(bson_invalid_object_id_length, error(domain_error(bson_term, {id-object_id(bytes([1]))}))) :-
		bson::generate({id-object_id(bytes([1]))}, _).

	test(bson_invalid_decimal128_length, error(domain_error(bson_term, {decimal-decimal128(bytes([1]))}))) :-
		bson::generate({decimal-decimal128(bytes([1]))}, _).

	test(bson_integer_overflow, error(domain_error(bson_term, {n-9223372036854775808}))) :-
		bson::generate({n-9223372036854775808}, _).

:- end_object.
