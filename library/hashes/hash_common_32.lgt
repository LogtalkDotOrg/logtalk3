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
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
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
		comment is 'Multiplies two integers modulo 2^32. Computed via 16-bit limb decomposition so that no intermediate result exceeds about 2**49, avoiding integer overflow errors on backends with bounded integer arithmetic.',
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

	:- uses(byte_order, [
		integer_to_bytes/4
	]).

	:- uses(list, [
		append/3, length/2
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

	% computes (A * B) mod 2^32 without ever forming the full A * B product,
	% which for two near-2^32 operands approaches 2^64 and can trip
	% int_overflow errors on backends with bounded integer arithmetic (e.g.
	% no automatic bignum promotion); splitting each operand into 16-bit
	% halves keeps every intermediate value under about 2^49
	mul32(A, B, Product) :-
		AH is A >> 16,
		AL is A /\ 0xFFFF,
		BH is B >> 16,
		BL is B /\ 0xFFFF,
		Cross is (AH * BL + AL * BH) /\ 0xFFFF,
		Low is AL * BL,
		Product is (Low + (Cross << 16)) /\ 0xFFFFFFFF.

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

	pad_md(little, Bytes, LengthFieldBytes, PaddedBytes) :-
		length(Bytes, Length),
		BitLength is Length * 8,
		Zeros is (56 - ((Length + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		integer_to_bytes(little, LengthFieldBytes, BitLength, LengthBytes),
		append(Bytes, [0x80| ZeroBytes], PaddedBytes).
	pad_md(big, Bytes, LengthFieldBytes, PaddedBytes) :-
		length(Bytes, Length),
		BitLength is Length * 8,
		Zeros is (56 - ((Length + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		integer_to_bytes(big, LengthFieldBytes, BitLength, LengthBytes),
		append(Bytes, [0x80| ZeroBytes], PaddedBytes).

	pad_md_tail(little, TailBytes, TotalLength, LengthFieldBytes, PaddedBytes) :-
		length(TailBytes, TailLength),
		BitLength is TotalLength * 8,
		Zeros is (56 - ((TailLength + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		integer_to_bytes(little, LengthFieldBytes, BitLength, LengthBytes),
		append(TailBytes, [0x80| ZeroBytes], PaddedBytes).
	pad_md_tail(big, TailBytes, TotalLength, LengthFieldBytes, PaddedBytes) :-
		length(TailBytes, TailLength),
		BitLength is TotalLength * 8,
		Zeros is (56 - ((TailLength + 1) mod 64) + 64) mod 64,
		zeros(Zeros, ZeroBytes, LengthBytes),
		integer_to_bytes(big, LengthFieldBytes, BitLength, LengthBytes),
		append(TailBytes, [0x80| ZeroBytes], PaddedBytes).

	zeros(0, Tail, Tail) :-
		!.
	zeros(Count, [0| Zeros], Tail) :-
		NextCount is Count - 1,
		zeros(NextCount, Zeros, Tail).

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
