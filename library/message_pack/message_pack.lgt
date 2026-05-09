%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(message_pack(_StringRepresentation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-09,
		comment is 'MessagePack format exporter and importer.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding MessagePack strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- public(parse/2).
	:- mode(parse(@list(byte), -ground), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a list of bytes in the MessagePack format returning the corresponding term representation. Throws an error when parsing is not possible (usually due to an invalid byte sequence).',
		argnames is ['Bytes', 'Term']
	]).

	:- public(generate/2).
	:- mode(generate(@ground, -list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a list of bytes in the MessagePack format representing the given term. Throws an error when generating is not possible (usually due to a term with no corresponding MessagePack representation).',
		argnames is ['Term', 'Bytes']
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(type, [
		valid/2
	]).

	generate(Term, Bytes) :-
		phrase(encode(Term), Bytes),
		!.
	generate(Term, _) :-
		domain_error(message_pack_term, Term).

	parse(Bytes, Term) :-
		phrase(decode(Term), Bytes),
		!.
	parse(Bytes, _) :-
		domain_error(message_pack_byte_sequence, Bytes).

	encode(Term) --> {var(Term), instantiation_error}.
	encode(@Literal) --> !, encode_literal(Literal).
	encode([]) --> !, encode_array([]).
	encode([Head| Tail]) --> !, encode_array([Head| Tail]).
	encode({}) --> !, encode_map({}).
	encode({Pairs}) --> !, encode_map({Pairs}).
	encode(bytes(Bytes)) --> !, encode_binary_term(Bytes).
	encode(ext(Type, bytes(Bytes))) --> !, encode_extension_term(Type, Bytes).
	encode(chars(Chars)) --> !, encode_utf_8_chars(Chars).
	encode(codes(Codes)) --> !, encode_utf_8_codes(Codes).
	encode(Atom) --> {atom(Atom)}, !, encode_utf_8_atom(Atom).
	encode(Integer) --> {integer(Integer)}, !, encode_integer(Integer).
	encode(Float) --> {float(Float)}, !, encode_float(Float).
	encode(Term) --> {domain_error(message_pack_term, Term)}.

	encode_literal(false) --> [0xc2].
	encode_literal(true) --> [0xc3].
	encode_literal(null) --> [0xc0].
	encode_literal(infinity) --> {ieee_754(single, big, canonical)::generate(@infinity, Bytes, [])}, [0xca], bytes(Bytes).
	encode_literal(negative_infinity) --> {ieee_754(single, big, canonical)::generate(@negative_infinity, Bytes, [])}, [0xca], bytes(Bytes).
	encode_literal(not_a_number) --> {ieee_754(single, big, canonical)::generate(@not_a_number, Bytes, [])}, [0xca], bytes(Bytes).

	encode_integer(Integer) -->
		(   {0 =< Integer} ->
			encode_non_negative_integer(Integer)
		;   encode_negative_integer(Integer)
		).

	encode_non_negative_integer(Integer) -->
		{Integer =< 0x7f},
		!,
		[Integer].
	encode_non_negative_integer(Integer) -->
		{Integer =< 0xff},
		!,
		[0xcc, Integer].
	encode_non_negative_integer(Integer) -->
		{Integer =< 0xffff},
		!,
		{integer_to_bytes(2, Integer, Bytes)},
		[0xcd], bytes(Bytes).
	encode_non_negative_integer(Integer) -->
		{Integer =< 0xffffffff},
		!,
		{integer_to_bytes(4, Integer, Bytes)},
		[0xce], bytes(Bytes).
	encode_non_negative_integer(Integer) -->
		{Integer =< 0xffffffffffffffff},
		!,
		{integer_to_bytes(8, Integer, Bytes)},
		[0xcf], bytes(Bytes).

	encode_negative_integer(Integer) -->
		{Integer >= -32},
		!,
		{Byte is 0x100 + Integer},
		[Byte].
	encode_negative_integer(Integer) -->
		{Integer >= -128},
		!,
		{Byte is 0x100 + Integer},
		[0xd0, Byte].
	encode_negative_integer(Integer) -->
		{Integer >= -32768},
		!,
		{Unsigned is Integer + 0x10000, integer_to_bytes(2, Unsigned, Bytes)},
		[0xd1], bytes(Bytes).
	encode_negative_integer(Integer) -->
		{Integer >= -2147483648},
		!,
		{Unsigned is Integer + 0x100000000, integer_to_bytes(4, Unsigned, Bytes)},
		[0xd2], bytes(Bytes).
	encode_negative_integer(Integer) -->
		{Integer >= -9223372036854775808},
		!,
		{Unsigned is Integer + 0x10000000000000000, integer_to_bytes(8, Unsigned, Bytes)},
		[0xd3], bytes(Bytes).

	encode_float(0.0) -->
		!,
		[0xca, 0x00, 0x00, 0x00, 0x00].
	encode_float(-0.0) -->
		!,
		[0xca, 0x80, 0x00, 0x00, 0x00].
	encode_float(Float) -->
		{encode_float_bytes(Float, Bytes)},
		bytes(Bytes).

	encode_float_bytes(Float, [0xca| Bytes]) :-
		ieee_754(single, big, canonical)::exactly_representable(Float),
		ieee_754(single, big, canonical)::generate(Float, Bytes, []),
		!.
	encode_float_bytes(Float, [0xcb| Bytes]) :-
		ieee_754(double, big, canonical)::generate(Float, Bytes, []).

	encode_binary_term(Bytes) -->
		{valid(list(byte), Bytes), length(Bytes, Length)},
		encode_binary(Length, Bytes).

	encode_binary(Length, Bytes) -->
		{Length =< 0xff},
		!,
		[0xc4, Length], bytes(Bytes).
	encode_binary(Length, Bytes) -->
		{Length =< 0xffff},
		!,
		{integer_to_bytes(2, Length, LengthBytes)},
		[0xc5], bytes(LengthBytes), bytes(Bytes).
	encode_binary(Length, Bytes) -->
		{Length =< 0xffffffff},
		!,
		{integer_to_bytes(4, Length, LengthBytes)},
		[0xc6], bytes(LengthBytes), bytes(Bytes).

	encode_extension_term(Type, Bytes) -->
		{
			integer(Type),
			-128 =< Type, Type =< 127,
			valid(list(byte), Bytes),
			length(Bytes, Length),
			TypeByte is Type /\ 0xff
		},
		encode_extension(Length, TypeByte, Bytes).

	encode_extension(1, TypeByte, [Byte]) --> !, [0xd4, TypeByte, Byte].
	encode_extension(2, TypeByte, Bytes) --> !, [0xd5, TypeByte], bytes(Bytes).
	encode_extension(4, TypeByte, Bytes) --> !, [0xd6, TypeByte], bytes(Bytes).
	encode_extension(8, TypeByte, Bytes) --> !, [0xd7, TypeByte], bytes(Bytes).
	encode_extension(16, TypeByte, Bytes) --> !, [0xd8, TypeByte], bytes(Bytes).
	encode_extension(Length, TypeByte, Bytes) -->
		{Length =< 0xff},
		!,
		[0xc7, Length, TypeByte], bytes(Bytes).
	encode_extension(Length, TypeByte, Bytes) -->
		{Length =< 0xffff},
		!,
		{integer_to_bytes(2, Length, LengthBytes)},
		[0xc8], bytes(LengthBytes), [TypeByte], bytes(Bytes).
	encode_extension(Length, TypeByte, Bytes) -->
		{Length =< 0xffffffff},
		!,
		{integer_to_bytes(4, Length, LengthBytes)},
		[0xc9], bytes(LengthBytes), [TypeByte], bytes(Bytes).

	encode_array(Array) -->
		{length(Array, Length)},
		encode_array_length(Length),
		encode_array_elements(Array).

	encode_array_length(Length) -->
		{Length =< 0x0f},
		!,
		{Byte is 0x90 + Length},
		[Byte].
	encode_array_length(Length) -->
		{Length =< 0xffff},
		!,
		{integer_to_bytes(2, Length, Bytes)},
		[0xdc], bytes(Bytes).
	encode_array_length(Length) -->
		{Length =< 0xffffffff},
		!,
		{integer_to_bytes(4, Length, Bytes)},
		[0xdd], bytes(Bytes).

	encode_array_elements([]) --> [].
	encode_array_elements([Element| Elements]) -->
		encode(Element),
		encode_array_elements(Elements).

	encode_map({}) -->
		!,
		[0x80].
	encode_map({Pairs}) -->
		{count_pairs(Pairs, Length)},
		encode_map_length(Length),
		encode_pairs(Pairs).

	encode_map_length(Length) -->
		{Length =< 0x0f},
		!,
		{Byte is 0x80 + Length},
		[Byte].
	encode_map_length(Length) -->
		{Length =< 0xffff},
		!,
		{integer_to_bytes(2, Length, Bytes)},
		[0xde], bytes(Bytes).
	encode_map_length(Length) -->
		{Length =< 0xffffffff},
		!,
		{integer_to_bytes(4, Length, Bytes)},
		[0xdf], bytes(Bytes).

	encode_pairs((Pair, Pairs)) -->
		!,
		encode_pair(Pair),
		encode_pairs(Pairs).
	encode_pairs(Pair) -->
		encode_pair(Pair).

	encode_pair(Key-Value) -->
		!,
		encode(Key),
		encode(Value).
	encode_pair(_) -->
		{representation_error(pair)}.

	encode_utf_8_atom(Atom) -->
		{atom_codes(Atom, Codes)},
		encode_utf_8_codes(Codes).

	encode_utf_8_chars(chars(Chars)) -->
		!,
		encode_utf_8_chars(Chars).
	encode_utf_8_chars(Chars) -->
		{chars_to_codes(Chars, Codes)},
		encode_utf_8_codes(Codes).

	encode_utf_8_codes(codes(Codes)) -->
		!,
		encode_utf_8_codes(Codes).
	encode_utf_8_codes(Codes) -->
		{utf_8::codes_to_bytes(Codes, Bytes), length(Bytes, Length)},
		encode_utf_8_string(Length, Bytes).

	encode_utf_8_string(Length, Bytes) -->
		{Length =< 0x1f},
		!,
		{Byte is 0xa0 + Length},
		[Byte], bytes(Bytes).
	encode_utf_8_string(Length, Bytes) -->
		{Length =< 0xff},
		!,
		[0xd9, Length], bytes(Bytes).
	encode_utf_8_string(Length, Bytes) -->
		{Length =< 0xffff},
		!,
		{integer_to_bytes(2, Length, LengthBytes)},
		[0xda], bytes(LengthBytes), bytes(Bytes).
	encode_utf_8_string(Length, Bytes) -->
		{Length =< 0xffffffff},
		!,
		{integer_to_bytes(4, Length, LengthBytes)},
		[0xdb], bytes(LengthBytes), bytes(Bytes).

	decode(Term) -->
		[Byte],
		decode(Byte, Term).

	decode(Byte, Integer) -->
		{Byte =< 0x7f},
		!,
		{Integer = Byte}.
	decode(Byte, Map) -->
		{0x80 =< Byte, Byte =< 0x8f},
		!,
		{Length is Byte /\ 0x0f},
		decode_map(Length, Map).
	decode(Byte, Array) -->
		{0x90 =< Byte, Byte =< 0x9f},
		!,
		{Length is Byte /\ 0x0f},
		decode_array(Length, Array).
	decode(Byte, String) -->
		{0xa0 =< Byte, Byte =< 0xbf},
		!,
		{Length is Byte /\ 0x1f},
		decode_utf_8_string(Length, String).
	decode(0xc0, @null) --> !.
	decode(0xc2, @false) --> !.
	decode(0xc3, @true) --> !.
	decode(0xc4, bytes(Bytes)) --> !, [Length], bytes(Length, Bytes).
	decode(0xc5, bytes(Bytes)) --> !, unsigned_integer(2, Length), bytes(Length, Bytes).
	decode(0xc6, bytes(Bytes)) --> !, unsigned_integer(4, Length), bytes(Length, Bytes).
	decode(0xc7, Extension) --> !, [Length], decode_extension(Length, Extension).
	decode(0xc8, Extension) --> !, unsigned_integer(2, Length), decode_extension(Length, Extension).
	decode(0xc9, Extension) --> !, unsigned_integer(4, Length), decode_extension(Length, Extension).
	decode(0xca, Float) --> !, bytes(4, Bytes), {ieee_754(single, big, canonical)::parse(bytes(Bytes), Float)}.
	decode(0xcb, Float) --> !, bytes(8, Bytes), {ieee_754(double, big, canonical)::parse(bytes(Bytes), Float)}.
	decode(0xcc, Integer) --> !, [Integer].
	decode(0xcd, Integer) --> !, unsigned_integer(2, Integer).
	decode(0xce, Integer) --> !, unsigned_integer(4, Integer).
	decode(0xcf, Integer) --> !, unsigned_integer(8, Integer).
	decode(0xd0, Integer) --> !, signed_integer(1, Integer).
	decode(0xd1, Integer) --> !, signed_integer(2, Integer).
	decode(0xd2, Integer) --> !, signed_integer(4, Integer).
	decode(0xd3, Integer) --> !, signed_integer(8, Integer).
	decode(0xd4, Extension) --> !, decode_extension(1, Extension).
	decode(0xd5, Extension) --> !, decode_extension(2, Extension).
	decode(0xd6, Extension) --> !, decode_extension(4, Extension).
	decode(0xd7, Extension) --> !, decode_extension(8, Extension).
	decode(0xd8, Extension) --> !, decode_extension(16, Extension).
	decode(0xd9, String) --> !, [Length], decode_utf_8_string(Length, String).
	decode(0xda, String) --> !, unsigned_integer(2, Length), decode_utf_8_string(Length, String).
	decode(0xdb, String) --> !, unsigned_integer(4, Length), decode_utf_8_string(Length, String).
	decode(0xdc, Array) --> !, unsigned_integer(2, Length), decode_array(Length, Array).
	decode(0xdd, Array) --> !, unsigned_integer(4, Length), decode_array(Length, Array).
	decode(0xde, Map) --> !, unsigned_integer(2, Length), decode_map(Length, Map).
	decode(0xdf, Map) --> !, unsigned_integer(4, Length), decode_map(Length, Map).
	decode(Byte, Integer) -->
		{0xe0 =< Byte, Byte =< 0xff},
		!,
		{Integer is Byte - 0x100}.

	decode_extension(Length, ext(Type, bytes(Bytes))) -->
		[TypeByte],
		bytes(Length, Bytes),
		{byte_to_signed_integer(TypeByte, Type)}.

	decode_utf_8_string(Length, String) -->
		bytes(Length, Bytes),
		{
			utf_8::bytes_to_codes(Bytes, Codes),
			message_pack_utf_8_string_to_term(_StringRepresentation_, Codes, String)
		}.

	decode_array(0, []) -->
		!,
		[].
	decode_array(Length, [Element| Elements]) -->
		{
			Length > 0,
			NextLength is Length - 1
		},
		decode(Element),
		decode_array(NextLength, Elements).

	decode_map(0, {}) -->
		!,
		[].
	decode_map(Length, {Pairs}) -->
		decode_pairs(Length, Pairs).

	decode_pairs(0, {}) -->
		!,
		[].
	decode_pairs(1, Key-Value) -->
		!,
		decode(Key),
		decode(Value).
	decode_pairs(Length, (Key-Value, Pairs)) -->
		{
			Length > 1,
			NextLength is Length - 1
		},
		decode(Key),
		decode(Value),
		decode_pairs(NextLength, Pairs).

	unsigned_integer(Length, Integer) -->
		bytes(Length, Bytes),
		{bytes_to_unsigned_integer(Bytes, Integer)}.

	signed_integer(Length, Integer) -->
		bytes(Length, Bytes),
		{bytes_to_signed_integer(Bytes, Length, Integer)}.

	bytes([]) --> [].
	bytes([Byte| Bytes]) --> [Byte], bytes(Bytes).

	bytes(0, []) -->
		!,
		[].
	bytes(Length, [Byte| Bytes]) -->
		[Byte],
		{NextLength is Length - 1},
		bytes(NextLength, Bytes).

	count_pairs(Pairs, Count) :-
		count_pairs(Pairs, 0, Count).

	count_pairs((_, Pairs), Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		count_pairs(Pairs, Count1, Count).
	count_pairs(_-_, Count0, Count) :-
		Count is Count0 + 1.

	integer_to_bytes(8, Integer, [B7, B6, B5, B4, B3, B2, B1, B0]) :-
		B7 is (Integer >> 56) /\ 0xff,
		B6 is (Integer >> 48) /\ 0xff,
		B5 is (Integer >> 40) /\ 0xff,
		B4 is (Integer >> 32) /\ 0xff,
		B3 is (Integer >> 24) /\ 0xff,
		B2 is (Integer >> 16) /\ 0xff,
		B1 is (Integer >> 8) /\ 0xff,
		B0 is Integer /\ 0xff.

	integer_to_bytes(4, Integer, [B3, B2, B1, B0]) :-
		B3 is (Integer >> 24) /\ 0xff,
		B2 is (Integer >> 16) /\ 0xff,
		B1 is (Integer >> 8) /\ 0xff,
		B0 is Integer /\ 0xff.

	integer_to_bytes(2, Integer, [B1, B0]) :-
		B1 is (Integer >> 8) /\ 0xff,
		B0 is Integer /\ 0xff.

	bytes_to_unsigned_integer(Bytes, Integer) :-
		bytes_to_unsigned_integer(Bytes, 0, Integer).

	bytes_to_unsigned_integer([], Integer, Integer).
	bytes_to_unsigned_integer([Byte| Bytes], Integer0, Integer) :-
		Integer1 is (Integer0 << 8) + Byte,
		bytes_to_unsigned_integer(Bytes, Integer1, Integer).

	bytes_to_signed_integer(Bytes, Length, Integer) :-
		bytes_to_unsigned_integer(Bytes, Unsigned),
		Bits is Length * 8,
		SignMask is 1 << (Bits - 1),
		(   Unsigned /\ SignMask =:= 0 ->
			Integer = Unsigned
		;   Integer is Unsigned - (1 << Bits)
		).

	byte_to_signed_integer(Byte, Integer) :-
		(   Byte >= 0x80 ->
			Integer is Byte - 0x100
		;   Integer = Byte
		).

	message_pack_utf_8_string_to_term(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	message_pack_utf_8_string_to_term(chars, Codes, chars(Chars)) :-
		codes_to_chars(Codes, Chars).
	message_pack_utf_8_string_to_term(codes, Codes, codes(Codes)).

	codes_to_chars(Codes, Chars) :-
		codes_to_chars(Codes, Chars, []).

	codes_to_chars([], Chars, Chars).
	codes_to_chars([Code| Codes], [Char| Chars0], Chars) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars0, Chars).

	chars_to_codes(Chars, Codes) :-
		chars_to_codes(Chars, Codes, []).

	chars_to_codes([], Codes, Codes).
	chars_to_codes([Char| Chars], [Code| Codes0], Codes) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes0, Codes).

:- end_object.
