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
		date is 2026-08-04,
		comment is 'Unit tests for the "byte_order" library.'
	]).

	cover(byte_order).

	test(byte_order_integer_to_bytes_01, deterministic(Bytes == [0x12])) :-
		byte_order::integer_to_bytes(big, 1, 0x12, Bytes).

	test(byte_order_integer_to_bytes_02, deterministic(Bytes == [0x12, 0x34])) :-
		byte_order::integer_to_bytes(big, 2, 0x1234, Bytes).

	test(byte_order_integer_to_bytes_03, deterministic(Bytes == [0x78, 0x56, 0x34, 0x12])) :-
		byte_order::integer_to_bytes(little, 4, 0x12345678, Bytes).

	test(byte_order_integer_to_bytes_04, deterministic(Bytes == [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef])) :-
		byte_order::integer_to_bytes(big, 8, 0x0123456789abcdef, Bytes).

	test(byte_order_integer_to_bytes_05, deterministic(Bytes == [0x34, 0x56])) :-
		byte_order::integer_to_bytes(big, 2, 0x123456, Bytes).

	test(byte_order_integer_to_bytes_06, deterministic(Bytes == [0x34, 0x12, 0xaa, 0xbb])) :-
		byte_order::integer_to_bytes(little, 2, 0x1234, Bytes, [0xaa, 0xbb]).

	test(byte_order_integer_to_bytes_07, deterministic(Bytes == Expected), [condition(current_prolog_flag(bounded, false))]) :-
		Integer is (1 << 255) + 1,
		byte_order::integer_to_bytes(big, 32, Integer, Bytes),
		Expected = [0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01].

	test(byte_order_bytes_to_integer_01, deterministic(Integer == 0x12345678)) :-
		byte_order::bytes_to_integer(big, [0x12, 0x34, 0x56, 0x78], Integer).

	test(byte_order_bytes_to_integer_02, deterministic(Integer == 0x12345678)) :-
		byte_order::bytes_to_integer(little, [0x78, 0x56, 0x34, 0x12], Integer).

	test(byte_order_bytes_to_integer_03, deterministic([Integer, Rest] == [0x1234, [0xaa, 0xbb]])) :-
		byte_order::bytes_to_integer(big, 2, [0x12, 0x34, 0xaa, 0xbb], Integer, Rest).

	test(byte_order_bytes_to_integer_04, deterministic([Integer, Rest] == [0x1234, [0xaa, 0xbb]])) :-
		byte_order::bytes_to_integer(little, 2, [0x34, 0x12, 0xaa, 0xbb], Integer, Rest).

	test(byte_order_signed_integer_to_bytes_01, deterministic(Bytes == [0xff, 0xff, 0xff, 0xfe])) :-
		byte_order::signed_integer_to_bytes(big, 4, -2, Bytes).

	test(byte_order_signed_integer_to_bytes_02, deterministic(Bytes == [0x00, 0x00, 0x00, 0x80])) :-
		byte_order::signed_integer_to_bytes(little, 4, -2147483648, Bytes).

	test(byte_order_signed_integer_to_bytes_03, deterministic(Bytes == [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f]), [condition(current_prolog_flag(bounded, false))]) :-
		number_chars(Number, ['9','2','2','3','3','7','2','0','3','6','8','5','4','7','7','5','8','0','7']),
		byte_order::signed_integer_to_bytes(little, 8, Number, Bytes).

	test(byte_order_bytes_to_signed_integer_01, deterministic(Integer == -2)) :-
		byte_order::bytes_to_signed_integer(big, [0xff, 0xff, 0xff, 0xfe], Integer).

	test(byte_order_bytes_to_signed_integer_02, deterministic(Integer == Number), [condition(current_prolog_flag(bounded, false))]) :-
		number_chars(Number, ['-','9','2','2','3','3','7','2','0','3','6','8','5','4','7','7','5','8','0','8']),
		byte_order::bytes_to_signed_integer(little, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80], Integer).

	test(byte_order_bytes_to_signed_integer_03, deterministic([Integer, Rest] == [-32768, [0xaa]])) :-
		byte_order::bytes_to_signed_integer(big, 2, [0x80, 0x00, 0xaa], Integer, Rest).

	test(byte_order_bytes_to_signed_integer_04, deterministic([Integer, Rest] == [-32768, [0xaa]])) :-
		byte_order::bytes_to_signed_integer(little, 2, [0x00, 0x80, 0xaa], Integer, Rest).

	test(byte_order_roundtrip_01, deterministic(Decoded == Integer)) :-
		Integer = 0x12345678,
		byte_order::integer_to_bytes(little, 16, Integer, Bytes),
		byte_order::bytes_to_integer(little, Bytes, Decoded).

	test(byte_order_roundtrip_02, deterministic(Decoded == -123456789)) :-
		byte_order::signed_integer_to_bytes(big, 8, -123456789, Bytes),
		byte_order::bytes_to_signed_integer(big, Bytes, Decoded).

:- end_object.
