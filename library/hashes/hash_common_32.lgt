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


:- object(hash_common_32).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'Auxiliary predicates for the hashes library 32-bit algorithms.'
	]).

	:- public(word32_hex/2).
	:- mode(word32_hex(+integer, -atom), one).
	:- info(word32_hex/2, [
		comment is 'Converts a 32-bit word into an 8-digit lowercase hexadecimal atom.',
		argnames is ['Word', 'Hex']
	]).

	:- public(bytes_hex/2).
	:- mode(bytes_hex(+list(integer), -atom), one).
	:- info(bytes_hex/2, [
		comment is 'Converts a list of bytes into a lowercase hexadecimal atom.',
		argnames is ['Bytes', 'Hex']
	]).

	:- public(mask32/1).
	:- mode(mask32(-integer), one).
	:- info(mask32/1, [
		comment is 'Returns the 32-bit mask value.',
		argnames is ['Mask']
	]).

	:- public(add32/3).
	:- mode(add32(+integer, +integer, -integer), one).
	:- info(add32/3, [
		comment is 'Adds two integers modulo 2^32.',
		argnames is ['A', 'B', 'Sum']
	]).

	:- public(add32/4).
	:- mode(add32(+integer, +integer, +integer, -integer), one).
	:- info(add32/4, [
		comment is 'Adds three integers modulo 2^32.',
		argnames is ['A', 'B', 'C', 'Sum']
	]).

	:- public(add32/5).
	:- mode(add32(+integer, +integer, +integer, +integer, -integer), one).
	:- info(add32/5, [
		comment is 'Adds four integers modulo 2^32.',
		argnames is ['A', 'B', 'C', 'D', 'Sum']
	]).

	:- public(mul32/3).
	:- mode(mul32(+integer, +integer, -integer), one).
	:- info(mul32/3, [
		comment is 'Multiplies two integers modulo 2^32.',
		argnames is ['A', 'B', 'Product']
	]).

	:- public(rol32/3).
	:- mode(rol32(+integer, +integer, -integer), one).
	:- info(rol32/3, [
		comment is 'Rotates a 32-bit word left by the given number of bits.',
		argnames is ['Value', 'Shift', 'Rotated']
	]).

	:- public(ror32/3).
	:- mode(ror32(+integer, +integer, -integer), one).
	:- info(ror32/3, [
		comment is 'Rotates a 32-bit word right by the given number of bits.',
		argnames is ['Value', 'Shift', 'Rotated']
	]).

	:- public(little_endian_word32/2).
	:- mode(little_endian_word32(+list(integer), -integer), one).
	:- info(little_endian_word32/2, [
		comment is 'Decodes four bytes in little-endian order into a 32-bit word.',
		argnames is ['Bytes', 'Word']
	]).

	:- public(big_endian_word32/2).
	:- mode(big_endian_word32(+list(integer), -integer), one).
	:- info(big_endian_word32/2, [
		comment is 'Decodes four bytes in big-endian order into a 32-bit word.',
		argnames is ['Bytes', 'Word']
	]).

	:- public(integer_to_little_endian_bytes32/3).
	:- mode(integer_to_little_endian_bytes32(+integer, -list(integer), -variable), one).
	:- info(integer_to_little_endian_bytes32/3, [
		comment is 'Encodes a 32-bit word into four bytes in little-endian order.',
		argnames is ['Integer', 'Bytes', 'Tail']
	]).

	:- public(integer_to_little_endian_bytes32/2).
	:- mode(integer_to_little_endian_bytes32(+integer, -list(integer)), one).
	:- info(integer_to_little_endian_bytes32/2, [
		comment is 'Encodes a 32-bit word into four bytes in little-endian order.',
		argnames is ['Integer', 'Bytes']
	]).

	:- public(integer_to_big_endian_bytes32/3).
	:- mode(integer_to_big_endian_bytes32(+integer, -list(integer), -variable), one).
	:- info(integer_to_big_endian_bytes32/3, [
		comment is 'Encodes a 32-bit word into four bytes in big-endian order.',
		argnames is ['Integer', 'Bytes', 'Tail']
	]).

	:- public(integer_to_big_endian_bytes32/2).
	:- mode(integer_to_big_endian_bytes32(+integer, -list(integer)), one).
	:- info(integer_to_big_endian_bytes32/2, [
		comment is 'Encodes a 32-bit word into four bytes in big-endian order.',
		argnames is ['Integer', 'Bytes']
	]).

	:- public(pad_md/4).
	:- mode(pad_md(+little_big, +list(integer), +integer, -list(integer)), one).
	:- info(pad_md/4, [
		comment is 'Pads a message using MD-style padding with a little-endian or big-endian length field.',
		argnames is ['Endian', 'Bytes', 'LengthFieldBytes', 'PaddedBytes']
	]).

	:- public(pad_md_tail/5).
	:- mode(pad_md_tail(+little_big, +list(integer), +integer, +integer, -list(integer)), one).
	:- info(pad_md_tail/5, [
		comment is 'Pads the final, less-than-one-block tail of a message using MD-style padding given the total message length in bytes. For use when the message has already been consumed block by block during segmented/incremental hashing, so that only the unconsumed tail, and not the whole message, needs to be available.',
		argnames is ['Endian', 'TailBytes', 'TotalLength', 'LengthFieldBytes', 'PaddedBytes']
	]).

	:- uses(list, [
		append/3, length/2, reverse/2
	]).

	word32_hex(Word, Hex) :-
		Mask is 0xFFFFFFFF,
		Value is Word /\ Mask,
		fixed_hex_atom(8, Value, Hex).

	bytes_hex(Bytes, Hex) :-
		bytes_hex_codes(Bytes, Codes),
		atom_codes(Hex, Codes).

	mask32(0xFFFFFFFF).

	add32(A, B, Sum) :-
		Mask is 0xFFFFFFFF,
		Sum is (A + B) /\ Mask.

	add32(A, B, C, Sum) :-
		Mask is 0xFFFFFFFF,
		Sum is (A + B + C) /\ Mask.

	add32(A, B, C, D, Sum) :-
		Mask is 0xFFFFFFFF,
		Sum is (A + B + C + D) /\ Mask.

	mul32(A, B, Product) :-
		Mask is 0xFFFFFFFF,
		Product is (A * B) /\ Mask.

	rol32(Value, Shift, Rotated) :-
		Mask is 0xFFFFFFFF,
		Count is Shift /\ 31,
		(	Count =:= 0 ->
			Rotated is Value /\ Mask
		;	Rotated is ((Value << Count) \/ (Value >> (32 - Count))) /\ Mask
		).

	ror32(Value, Shift, Rotated) :-
		Mask is 0xFFFFFFFF,
		Count is Shift /\ 31,
		(	Count =:= 0 ->
			Rotated is Value /\ Mask
		;	Rotated is ((Value >> Count) \/ (Value << (32 - Count))) /\ Mask
		).

	little_endian_word32([B0, B1, B2, B3], Word) :-
		Word is B0 \/ (B1 << 8) \/ (B2 << 16) \/ (B3 << 24).

	big_endian_word32([B0, B1, B2, B3], Word) :-
		Word is (B0 << 24) \/ (B1 << 16) \/ (B2 << 8) \/ B3.

	integer_to_little_endian_bytes32(Integer, [B0, B1, B2, B3| Tail], Tail) :-
		B0 is Integer /\ 0xFF,
		B1 is (Integer >> 8) /\ 0xFF,
		B2 is (Integer >> 16) /\ 0xFF,
		B3 is (Integer >> 24) /\ 0xFF.

	integer_to_little_endian_bytes32(Integer, Bytes) :-
		integer_to_little_endian_bytes32(Integer, Bytes, []).

	integer_to_big_endian_bytes32(Integer, [B0, B1, B2, B3| Tail], Tail) :-
		B0 is (Integer >> 24) /\ 0xFF,
		B1 is (Integer >> 16) /\ 0xFF,
		B2 is (Integer >> 8) /\ 0xFF,
		B3 is Integer /\ 0xFF.

	integer_to_big_endian_bytes32(Integer, Bytes) :-
		integer_to_big_endian_bytes32(Integer, Bytes, []).

	pad_md(little, Bytes, LengthFieldBytes, PaddedBytes) :-
		length(Bytes, Length),
		BitLength is Length * 8,
		Zeros is (56 - ((Length + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		little_endian_length_bytes(BitLength, LengthFieldBytes, LengthBytes),
		append(Bytes, [0x80| ZeroBytes], PaddedBytes).
	pad_md(big, Bytes, LengthFieldBytes, PaddedBytes) :-
		length(Bytes, Length),
		BitLength is Length * 8,
		Zeros is (56 - ((Length + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		big_endian_length_bytes(BitLength, LengthFieldBytes, LengthBytes),
		append(Bytes, [0x80| ZeroBytes], PaddedBytes).

	pad_md_tail(little, TailBytes, TotalLength, LengthFieldBytes, PaddedBytes) :-
		length(TailBytes, TailLength),
		BitLength is TotalLength * 8,
		Zeros is (56 - ((TailLength + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		little_endian_length_bytes(BitLength, LengthFieldBytes, LengthBytes),
		append(TailBytes, [0x80| ZeroBytes], PaddedBytes).
	pad_md_tail(big, TailBytes, TotalLength, LengthFieldBytes, PaddedBytes) :-
		length(TailBytes, TailLength),
		BitLength is TotalLength * 8,
		Zeros is (56 - ((TailLength + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		big_endian_length_bytes(BitLength, LengthFieldBytes, LengthBytes),
		append(TailBytes, [0x80| ZeroBytes], PaddedBytes).

	zeros(0, Tail, Tail) :-
		!.
	zeros(Count, [0| Zeros], Tail) :-
		NextCount is Count - 1,
		zeros(NextCount, Zeros, Tail).

	little_endian_length_bytes(_, 0, []) :-
		!.
	little_endian_length_bytes(BitLength, Count, [Byte| Bytes]) :-
		Byte is BitLength /\ 0xFF,
		NextBitLength is BitLength >> 8,
		NextCount is Count - 1,
		little_endian_length_bytes(NextBitLength, NextCount, Bytes).

	big_endian_length_bytes(BitLength, Count, Bytes) :-
		little_endian_length_bytes(BitLength, Count, ReversedBytes),
		reverse(ReversedBytes, Bytes).

	bytes_hex_codes([], []).
	bytes_hex_codes([Byte| Bytes], [HighCode, LowCode| Codes]) :-
		High is (Byte >> 4) /\ 0x0F,
		Low is Byte /\ 0x0F,
		hex_digit_code(High, HighCode),
		hex_digit_code(Low, LowCode),
		bytes_hex_codes(Bytes, Codes).

	fixed_hex_atom(Digits, Integer, Hex) :-
		fixed_hex_codes(Digits, Integer, Codes),
		atom_codes(Hex, Codes).

	fixed_hex_codes(0, _, []) :-
		!.
	fixed_hex_codes(Digits, Integer, [Code| Codes]) :-
		Shift is (Digits - 1) * 4,
		Digit is (Integer >> Shift) /\ 0x0F,
		hex_digit_code(Digit, Code),
		NextDigits is Digits - 1,
		fixed_hex_codes(NextDigits, Integer, Codes).

	hex_digit_code( 0, 0'0).
	hex_digit_code( 1, 0'1).
	hex_digit_code( 2, 0'2).
	hex_digit_code( 3, 0'3).
	hex_digit_code( 4, 0'4).
	hex_digit_code( 5, 0'5).
	hex_digit_code( 6, 0'6).
	hex_digit_code( 7, 0'7).
	hex_digit_code( 8, 0'8).
	hex_digit_code( 9, 0'9).
	hex_digit_code(10, 0'a).
	hex_digit_code(11, 0'b).
	hex_digit_code(12, 0'c).
	hex_digit_code(13, 0'd).
	hex_digit_code(14, 0'e).
	hex_digit_code(15, 0'f).

:- end_object.
