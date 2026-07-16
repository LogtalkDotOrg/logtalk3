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


:- object(hash_common_64).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'Auxiliary predicates for the hashes library 64-bit algorithms.'
	]).

	:- public(word64_hex/2).
	:- mode(word64_hex(+integer, -atom), one).
	:- info(word64_hex/2, [
		comment is 'Converts a 64-bit word into a 16-digit lowercase hexadecimal atom.',
		argnames is ['Word', 'Hex']
	]).

	:- public(mask64/1).
	:- mode(mask64(-integer), one).
	:- info(mask64/1, [
		comment is 'Returns the 64-bit mask value.',
		argnames is ['Mask']
	]).

	:- public(add64/3).
	:- mode(add64(+integer, +integer, -integer), one).
	:- info(add64/3, [
		comment is 'Adds two integers modulo 2^64.',
		argnames is ['A', 'B', 'Sum']
	]).

	:- public(mul64/3).
	:- mode(mul64(+integer, +integer, -integer), one).
	:- info(mul64/3, [
		comment is 'Multiplies two integers modulo 2^64.',
		argnames is ['A', 'B', 'Product']
	]).

	:- public(rol64/3).
	:- mode(rol64(+integer, +integer, -integer), one).
	:- info(rol64/3, [
		comment is 'Rotates a 64-bit word left by the given number of bits.',
		argnames is ['Value', 'Shift', 'Rotated']
	]).

	:- public(xor64/3).
	:- mode(xor64(+integer, +integer, -integer), one).
	:- info(xor64/3, [
		comment is 'Computes the bitwise exclusive-or of two integers modulo 2^64.',
		argnames is ['A', 'B', 'Xor']
	]).

	:- public(or64/3).
	:- mode(or64(+integer, +integer, -integer), one).
	:- info(or64/3, [
		comment is 'Computes the bitwise disjunction of two integers modulo 2^64.',
		argnames is ['A', 'B', 'Or']
	]).

	:- public(and64/3).
	:- mode(and64(+integer, +integer, -integer), one).
	:- info(and64/3, [
		comment is 'Computes the bitwise conjunction of two integers modulo 2^64.',
		argnames is ['A', 'B', 'And']
	]).

	:- public(not64/2).
	:- mode(not64(+integer, -integer), one).
	:- info(not64/2, [
		comment is 'Computes the bitwise complement of an integer modulo 2^64.',
		argnames is ['Value', 'Complement']
	]).

	:- public(shl64/3).
	:- mode(shl64(+integer, +integer, -integer), one).
	:- info(shl64/3, [
		comment is 'Shifts a 64-bit word left by the given number of bits and masks the result.',
		argnames is ['Value', 'Shift', 'Shifted']
	]).

	:- public(shr64/3).
	:- mode(shr64(+integer, +integer, -integer), one).
	:- info(shr64/3, [
		comment is 'Shifts a 64-bit word right by the given number of bits after masking the input.',
		argnames is ['Value', 'Shift', 'Shifted']
	]).

	:- public(integer_to_big_endian_bytes64/3).
	:- mode(integer_to_big_endian_bytes64(+integer, -list(integer), -variable), one).
	:- info(integer_to_big_endian_bytes64/3, [
		comment is 'Encodes a 64-bit word into eight bytes in big-endian order.',
		argnames is ['Integer', 'Bytes', 'Tail']
	]).

	:- public(integer_to_big_endian_bytes64/2).
	:- mode(integer_to_big_endian_bytes64(+integer, -list(integer)), one).
	:- info(integer_to_big_endian_bytes64/2, [
		comment is 'Encodes a 64-bit word into eight bytes in big-endian order.',
		argnames is ['Integer', 'Bytes']
	]).

	:- public(pad_md_tail/3).
	:- mode(pad_md_tail(+list(integer), +integer, -list(integer)), one).
	:- info(pad_md_tail/3, [
		comment is 'Pads the final, less-than-one-block tail of a message using SHA-512-style MD padding (128-byte blocks, a big-endian 128-bit length field) given the total message length in bytes. For use when the message has already been consumed block by block during segmented/incremental hashing, so that only the unconsumed tail, and not the whole message, needs to be available.',
		argnames is ['TailBytes', 'TotalLength', 'PaddedBytes']
	]).

	:- uses(list, [
		append/3, length/2
	]).

	word64_hex(Word, Hex) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Value is Word /\ Mask,
		fixed_hex_atom(16, Value, Hex).

	mask64(0xFFFFFFFFFFFFFFFF).

	add64(A, B, Sum) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Sum is (A + B) /\ Mask.

	mul64(A, B, Product) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Product is (A * B) /\ Mask.

	rol64(Value, Shift, Rotated) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Count is Shift /\ 63,
		(	Count =:= 0 ->
			Rotated is Value /\ Mask
		;	Rotated is ((Value << Count) \/ (Value >> (64 - Count))) /\ Mask
		).

	xor64(A, B, Xor) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Xor is xor(A, B) /\ Mask.

	or64(A, B, Or) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Or is (A \/ B) /\ Mask.

	and64(A, B, And) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		And is (A /\ B) /\ Mask.

	not64(Value, Complement) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Complement is (\ Value) /\ Mask.

	shl64(Value, Shift, Shifted) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Shifted is (Value << Shift) /\ Mask.

	shr64(Value, Shift, Shifted) :-
		Mask is 0xFFFFFFFFFFFFFFFF,
		Shifted is (Value /\ Mask) >> Shift.

	integer_to_big_endian_bytes64(Integer, [B0, B1, B2, B3, B4, B5, B6, B7| Tail], Tail) :-
		B0 is (Integer >> 56) /\ 0xFF,
		B1 is (Integer >> 48) /\ 0xFF,
		B2 is (Integer >> 40) /\ 0xFF,
		B3 is (Integer >> 32) /\ 0xFF,
		B4 is (Integer >> 24) /\ 0xFF,
		B5 is (Integer >> 16) /\ 0xFF,
		B6 is (Integer >> 8) /\ 0xFF,
		B7 is Integer /\ 0xFF.

	integer_to_big_endian_bytes64(Integer, Bytes) :-
		integer_to_big_endian_bytes64(Integer, Bytes, []).

	pad_md_tail(TailBytes, TotalLength, PaddedBytes) :-
		length(TailBytes, TailLength),
		BitLength is TotalLength * 8,
		Zeros is (112 - ((TailLength + 1) mod 128) + 128) mod 128,
		zeros(Zeros, ZeroBytes, LengthBytes),
		length_bytes128(BitLength, LengthBytes),
		append(TailBytes, [0x80| ZeroBytes], PaddedBytes).

	zeros(0, Tail, Tail) :-
		!.
	zeros(Count, [0| Zeros], Tail) :-
		NextCount is Count - 1,
		zeros(NextCount, Zeros, Tail).

	length_bytes128(BitLength, LengthBytes) :-
		Two64 is 0x10000000000000000,
		High is (BitLength // Two64) mod Two64,
		Low is BitLength mod Two64,
		integer_to_big_endian_bytes64(High, LengthBytes, LowBytes),
		integer_to_big_endian_bytes64(Low, LowBytes, []).

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
