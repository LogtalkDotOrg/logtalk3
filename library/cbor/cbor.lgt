%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(cbor).

	:- info([
		version is 0:2:0,
		author is 'Paulo Moura',
		date is 2021-03-02,
		comment is 'Concise Binary Object Representation (CBOR) format exporter and importer library.'
	]).

	:- public([parse/2, generate/2]).

	generate(Term, Bytes) :-
		phrase(encode(Term), Bytes).

	encode(false) --> !, [0xf4].
	encode(true) --> !, [0xf5].
	encode(null) --> !, [0xf6].
	encode(undefined) --> !, [0xf7].
	encode(inf) --> !, [0xf9, 0x7c, 0x00].
	encode(neginf) --> !, [0xf9, 0xfc, 0x00].
	encode(nan) --> !, [0xf9, 0x7e, 0x00].
	encode(zero) --> !, [0xf9, 0x00, 0x00].
	encode(negzero) --> !, [0xf9, 0x80, 0x00].
	encode(Integer) --> {integer(Integer)}, !, encode_integer(Integer).
	encode(Float) --> {float(Float)}, !, encode_float(Float).
	encode([]) --> !, [0x80].
	encode([Head| Tail]) --> !, [0x9f], encode_list([Head| Tail]), [0xff].
	encode({}) --> !, [0xa0].
	encode({Pairs}) --> !, [0xbf], encode_pairs(Pairs), [0xff].
	encode(Key-Value) --> !, encode(Key), encode(Value).
	encode(Atom) --> {atom(Atom)}, !, encode_utf_8_string(Atom).
	encode(tag(N, Data)) --> !, encode_tag(N, Data).
	encode(simple(N)) --> !, encode_simple(N).
	encode(Term) --> {throw(domain_error(term, Term))}.

	encode_utf_8_string('') -->
		!, [0x60].
	encode_utf_8_string(Atom) -->
		{atom_codes(Atom, Codes), utf_8_codes_to_bytes(Codes, Bytes)}, [0x7f| Bytes], [0xff].

	encode_list([Head| Tail]) -->
		encode(Head), encode_list(Tail).
	encode_list([]) -->
		[].

	encode_pairs((Pair, Pairs)) -->
		!, encode(Pair), encode_pairs(Pairs).
	encode_pairs(Pair) -->
		encode(Pair).

	encode_float(0.0) -->
		!,
		encode(zero).
	% only some backend Prolog systems support a negative zero
	encode_float(-0.0) -->
		!,
		encode(negzero).
	% other floats are represented using decimal fractions
	encode_float(Float) -->
		[0xc4],
		{float_to_mantissa_and_exponent(Float, Mantissa, Exponent)},
		[0x82], encode_integer(Exponent), encode_integer(Mantissa).

	encode_integer(N) -->
		(	{N >= 0} ->
			encode_positive_integer(N)
		;	encode_negative_integer(N)
		).

	encode_positive_integer(N) -->
		{N > 0xffffffffffffffff}, !, {int_num_bytes(N, Length), int_bytes(N, Bytes)}, [0xc2], encode_byte_string(Length, Bytes).
	encode_positive_integer(N) -->
		{N > 0xffffffff}, !, {int_bytes(8, N, Bytes)}, [0x1b| Bytes].
	encode_positive_integer(N) -->
		{N > 0xffff}, !, {int_bytes(4, N, Bytes)}, [0x1a| Bytes].
	encode_positive_integer(N) -->
		{N > 0xff}, !, {int_bytes(2, N, Bytes)}, [0x19| Bytes].
	encode_positive_integer(N) -->
		{N > 0x17}, !, [0x18, N].
	encode_positive_integer(N) -->
		{N >= 0}, !, [N].

	encode_negative_integer(N) -->
		{N >= -0x18}, !, {Byte is 0x20 - 1 - N}, [Byte].
	encode_negative_integer(N) -->
		{N >= -0xff - 1}, !, {Byte is -1 - N}, [0x38, Byte].
	encode_negative_integer(N) -->
		{N >= -0xffff - 1}, !, {M is -1 - N, int_bytes(2, M, Bytes)}, [0x39| Bytes].
	encode_negative_integer(N) -->
		{N >= -0xffffffff - 1}, !, {M is -1 - N, int_bytes(4, M, Bytes)}, [0x3a| Bytes].
	encode_negative_integer(N) -->
		{N >= -0xffffffffffffffff - 1}, !, {M is -1 - N, int_bytes(8, M, Bytes)}, [0x3b| Bytes].
	encode_negative_integer(N) -->
		{Inv is -1 - N, int_num_bytes(Inv, Length), int_bytes(Inv, Bytes)}, [0xc3], encode_byte_string(Length, Bytes).

	% byte string (0x00..0x17 bytes follow)
	encode_byte_string(Length, Bytes) -->
		{Length =< 0x17}, !, {Size is Length + 0x40}, [Size| Bytes].
	% byte string (one-byte uint8_t for n, and then n bytes follow)
	encode_byte_string(Length, Bytes) -->
		{Length =< 0xff}, !, [0x58, Length| Bytes].
	% byte string (two-byte uint16_t for n, and then n bytes follow)
	encode_byte_string(Length, [Byte| Bytes]) -->
		{Length =< 0xffff}, !, {int_bytes(2, Length, Size)}, [0x59| Size], [Byte| Bytes].
	% byte string (four-byte uint32_t for n, and then n bytes follow)
	encode_byte_string(Length, [Byte| Bytes]) -->
		{Length =< 0xffffffff}, !, {int_bytes(4, Length, Size)}, [0x5a| Size], [Byte| Bytes].
	% byte string (eight-byte uint64_t for n, and then n bytes follow)
	encode_byte_string(Length, [Byte| Bytes]) -->
		{Length =< 0xffffffffffffffff}, !, {int_bytes(8, Length, Size)}, [0x5b| Size], [Byte| Bytes].
	encode_byte_string(_, _) -->
		{throw(representation_error(byte_string_length))}.

	encode_tag(Tag, Data) -->
		{Tag =< 0x17, Byte is Tag + 0xc0}, !, [Byte], encode(Data).
	encode_tag(Tag, Data) -->
		{Tag =< 0xff}, !, [0xd8, Tag], encode(Data).
	encode_tag(Tag, Data) -->
		{Tag =< 0xffff}, !, {int_bytes(2, Tag, Bytes)}, [0xd9| Bytes], encode(Data).
	encode_tag(Tag, Data) -->
		{Tag =< 0xffffff}, !, {int_bytes(4, Tag, Bytes)}, [0xda| Bytes], encode(Data).
	encode_tag(Tag, Data) -->
		{Tag =< 0xffffffff}, !, {int_bytes(8, Tag, Bytes)}, [0xdb| Bytes], encode(Data).

	encode_simple(N) -->
		{N =< 0x13, Byte is N + 0xe0}, !, [Byte].
	encode_simple(N) -->
		{0x20 =< N, N =< 0xff}, !, [0xf8, N].
	encode_simple(_) -->
		{throw(representation_error(simple))}.

	parse(Bytes, Term) :-
		phrase(decode(Term), Bytes).

	decode(Term) -->
		[Byte], decode(Byte, Term).

	decode(0xf4, false) --> !.
	decode(0xf5, true) --> !.
	decode(0xf6, null) --> !.
	decode(0xf7, undefined) --> !.
	decode(0xf9, inf) --> [0x7c, 0x00], !.
	decode(0xf9, neginf) --> [0xfc, 0x00], !.
	decode(0xf9, nan) --> [0x7e, 0x00], !.
	decode(0xf9, zero) --> [0x00, 0x00], !.
	decode(0xf9, negzero) --> [0x80, 0x00], !.

	% unsigned integer 0x00..0x17 (0..23)
	decode(Integer, Integer) -->
		{0x00 =< Integer, Integer =< 0x17}, !.
	% unsigned integer (one-byte uint8_t follows)
	decode(0x18, Integer) -->
		!, [Integer].
	% unsigned integer (two-byte uint16_t follows)
	decode(0x19, Integer) -->
		!, decode_unsigned_integer(2, Integer).
	% unsigned integer (four-byte uint32_t follows)
	decode(0x1a, Integer) -->
		!, decode_unsigned_integer(4, Integer).
	% unsigned integer (eight-byte uint64_t follows)
	decode(0x1b, Integer) -->
		!, decode_unsigned_integer(8, Integer).

	% unsigned bignum (data item "byte string" follows)
	decode(0xc2, Integer) -->
		!, [Byte], decode_unsigned_bignum(Byte, Integer).

	% negative integer -1-0x00..-1-0x17 (-1..-24)
	decode(Integer0, Integer) -->
		{0x20 =< Integer0, Integer0 =< 0x37}, !, {Integer is -1 - Integer0 + 0x20}.
	% negative integer -1-n (one-byte uint8_t for n follows)
	decode(0x38, Integer) -->
		!, decode_negative_integer(1, Integer).
	% negative integer -1-n (two-byte uint16_t for n follows)
	decode(0x39, Integer) -->
		!, decode_negative_integer(2, Integer).
	% negative integer -1-n (four-byte uint32_t for n follows)
	decode(0x3a, Integer) -->
		!, decode_negative_integer(4, Integer).
	% negative integer -1-n (eight-byte uint64_t for n follows)
	decode(0x3b, Integer) -->
		!, decode_negative_integer(8, Integer).

	% negative bignum (data item "byte string" follows)
	decode(0xc3, Integer) -->
		!, [Byte], decode_negative_bignum(Byte, Integer).

	% decimal fraction (data item "array" follows; see Section 3.4.4)
	decode(0xc4, Float) -->
		!, decode([Exponent, Mantissa]), {mantissa_and_exponent_to_float(Mantissa, Exponent, Float)}.

	% bigfloat (data item "array" follows; see Section 3.4.4)
%	decode(0xc5, Float) -->
%		!.

	% half-precision float (two-byte IEEE 754)
	decode(0xf9, Float) -->
		!, decode_halt_precision_float(Float).
	% single-precision float (four-byte IEEE 754)
	decode(0xfa, Float) -->
		!, decode_single_precision_float(Float).
	% double-precision float (eight-byte IEEE 754)
	decode(0xfb, Float) -->
		!, decode_double_precision_float(Float).

	% byte string (0x00..0x17 bytes follow)
	decode(Byte, Bytes) -->
		{0x40 =< Byte, Byte =< 0x57}, !, {Length is Byte - 0x40}, bytes(Length, Bytes).
	% byte string (one-byte uint8_t for n, and then n bytes follow)
	decode(0x58, Bytes) -->
		!, decode_byte_string(1, Bytes).
	% byte string (two-byte uint16_t for n, and then n bytes follow)
	decode(0x59, Bytes) -->
		!, decode_byte_string(2, Bytes).
	% byte string (four-byte uint32_t for n, and then n bytes follow)
	decode(0x5a, Bytes) -->
		!, decode_byte_string(4, Bytes).
	% byte string (eight-byte uint64_t for n, and then n bytes follow)
	decode(0x5b, Bytes) -->
		!, decode_byte_string(8, Bytes).
	% byte string, byte strings follow, terminated by "break"
	decode(0x5f, Atom) -->
		!, bytes_until_break(Bytes), {bytes_to_utf_8_codes(Bytes, Codes), atom_codes(Atom, Codes)}.

	% UTF-8 string (0x00..0x17 bytes follow)
	decode(Byte, Atom) -->
		{0x60 =< Byte, Byte =< 0x77}, !,
		{Length is Byte - 0x60}, bytes(Length, Bytes),
		{bytes_to_utf_8_codes(Bytes, Codes), atom_codes(Atom, Codes)}.
	% UTF-8 string (one-byte uint8_t for n, and then n bytes follow)
	decode(0x78, Atom) -->
		!, decode_utf_8_string(1, Atom).
	% UTF-8 string (two-byte uint16_t for n, and then n bytes follow)
	decode(0x79, Atom) -->
		!, decode_utf_8_string(2, Atom).
	% UTF-8 string (four-byte uint32_t for n, and then n bytes follow)
	decode(0x7a, Atom) -->
		!, decode_utf_8_string(4, Atom).
	% UTF-8 string (eight-byte uint64_t for n, and then n bytes follow)
	decode(0x7b, Atom) -->
		!, decode_utf_8_string(8, Atom).
	% UTF-8 string, UTF-8 strings follow, terminated by "break"
	decode(0x7f, Atom) -->
		!, bytes_until_break(Bytes),
		{bytes_to_utf_8_codes(Bytes, Codes), atom_codes(Atom, Codes)}.

	% (simple value)
	decode(Byte, simple(Simple)) -->
		{0xe0 =< Byte, Byte =< 0xf3}, !, {Simple is Byte - 0xe0}.
	% (simple value, one byte follows)
	decode(0xf8, simple(Simple)) -->
		!, [Simple].

	decode(Byte, tag(Tag, Data)) -->
		{0xc0 =< Byte, Byte =< 0xd7}, !, {Tag is Byte - 0xc0}, decode(Data).
	% (more tags; 1/2/4/8 bytes of tag number and then a data item follow)
	decode(0xd8, tag(Tag, Data)) -->
		!, decode_unsigned_integer(1 ,Tag), decode(Data).
	decode(0xd9, tag(Tag, Data)) -->
		!, decode_unsigned_integer(2 ,Tag), decode(Data).
	decode(0xda, tag(Tag, Data)) -->
		!, decode_unsigned_integer(4 ,Tag), decode(Data).
	decode(0xdb, tag(Tag, Data)) -->
		!, decode_unsigned_integer(8 ,Tag), decode(Data).

	% array (0x00..0x17 data items follow)
	decode(Byte, List) -->
		{0x80 =< Byte, Byte =< 0x97, Length is Byte - 0x80}, !, decode_array_elements(Length, List).
	% array (one-byte uint8_t for n, and then n data items follow)
	decode(0x98, List) -->
		!, [Length], decode_array_elements(Length, List).
	% array (two-byte uint16_t for n, and then n data items follow)
	decode(0x99, List) -->
		!, decode_array(2, List).
	% array (four-byte uint32_t for n, and then n data items follow)
	decode(0x9a, List) -->
		!, decode_array(4, List).
	% array (eight-byte uint64_t for n, and then n data items follow)
	decode(0x9b, List) -->
		!, decode_array(8, List).
	% array, data items follow, terminated by "break"
	decode(0x9f, List) -->
		!, decode_indefinite_length_list(List).

	% map (0x00..0x17 pairs of data items follow)
	decode(0xa0, {}) -->
		!.
	decode(Byte, {Pairs}) -->
		{0xa1 =< Byte, Byte =< 0xb7, Length is Byte - 0xa0}, !, decode_map_elements(Length, Pairs).
	% map (one-byte uint8_t for n, and then n pairs of data items follow)
	decode(0xb8, {Pairs}) -->
		!, [Length], decode_map_elements(Length, Pairs).
	% map (two-byte uint16_t for n, and then n pairs of data items follow)
	decode(0xb9, {Pairs}) -->
		!, decode_map(2, Pairs).
	% map (four-byte uint32_t for n, and then n pairs of data items follow)
	decode(0xba, {Pairs}) -->
		!, decode_map(4, Pairs).
	% map (eight-byte uint64_t for n, and then n pairs of data items follow)
	decode(0xbb, {Pairs}) -->
		!, decode_map(8, Pairs).
	% map, pairs of data items follow, terminated by "break"
	decode(0xbf, {Pairs}) -->
		!, decode_indefinite_length_map(Pairs).

	decode_unsigned_integer(N, Integer) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Integer)}.

	decode_negative_integer(N, Integer) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Integer0), Integer is -1 - Integer0}.

	decode_unsigned_bignum(0x58, Integer) -->
		!, [Length], decode_unsigned_integer(Length, Integer).
	decode_unsigned_bignum(0x59, Integer) -->
		!, decode_unsigned_integer(2, Length), decode_unsigned_integer(Length, Integer).
	decode_unsigned_bignum(0x58, Integer) -->
		!, decode_unsigned_integer(4, Length), decode_unsigned_integer(Length, Integer).
	decode_unsigned_bignum(0x58, Integer) -->
		!, decode_unsigned_integer(8, Length), decode_unsigned_integer(Length, Integer).
	decode_unsigned_bignum(Byte, Integer) -->
		!, {Length is Byte - 0x40}, decode_unsigned_integer(Length, Integer).

	decode_negative_bignum(0x58, Integer) -->
		!, [Length], decode_unsigned_integer(Length, Inv), {Integer is -Inv - 1}.
	decode_negative_bignum(0x59, Integer) -->
		!, decode_unsigned_integer(2, Length), decode_unsigned_integer(Length, Inv), {Integer is -Inv - 1}.
	decode_negative_bignum(0x58, Integer) -->
		!, decode_unsigned_integer(4, Length), decode_unsigned_integer(Length, Inv), {Integer is -Inv - 1}.
	decode_negative_bignum(0x58, Integer) -->
		!, decode_unsigned_integer(8, Length), decode_unsigned_integer(Length, Inv), {Integer is -Inv - 1}.
	decode_negative_bignum(Byte, Integer) -->
		!, {Length is Byte - 0x40}, decode_unsigned_integer(Length, Inv), {Integer is -Inv - 1}.

	decode_byte_string(N, Atom) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Length)},
		bytes(Length, Bytes),
		{atom_codes(Atom, Bytes)}.

	decode_utf_8_string(N, Atom) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Length)},
		bytes(Length, Bytes),
		{bytes_to_utf_8_codes(Bytes, Codes), atom_codes(Atom, Codes)}.

	decode_array(N, List) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Length)},
		decode_array_elements(Length, List).

	decode_array_elements(N, [Head| Tail]) -->
		{N > 0}, !, decode(Head), {M is N - 1}, decode_array_elements(M, Tail).
	decode_array_elements(0, []) -->
		[].

	decode_indefinite_length_list([]) -->
		[0xff], !.
	decode_indefinite_length_list([Head| Tail]) -->
		decode(Head), decode_indefinite_length_list(Tail).

	decode_map(N, Pairs) -->
		bytes_reversed(N, Bytes),
		{bytes_int(Bytes, Length)},
		decode_map_elements(Length, Pairs).

	decode_map_elements(N, (Key-Value, Pairs)) -->
		{N > 1}, !, decode(Key), decode(Value), {M is N - 1}, decode_map_elements(M, Pairs).
	decode_map_elements(1, Key-Value) -->
		decode(Key), decode(Value).

	decode_indefinite_length_map(Key-Value) -->
		decode(Key), decode(Value), [0xff], !.
	decode_indefinite_length_map((Key-Value, Pairs)) -->
		decode(Key), decode(Value), decode_indefinite_length_map(Pairs).

	decode_halt_precision_float(Float) -->
		[Byte1, Byte0],
		{half_precision_to_float(Byte1, Byte0, Float)}.

	decode_single_precision_float(Float) -->
		[Byte3, Byte2, Byte1, Byte0],
		{single_precision_to_float(Byte3, Byte2, Byte1, Byte0, Float)}.

	decode_double_precision_float(Float) -->
		[Byte7, Byte6, Byte5, Byte4, Byte3, Byte2, Byte1, Byte0],
		{double_precision_to_float(Byte7, Byte6, Byte5, Byte4, Byte3, Byte2, Byte1, Byte0, Float)}.

	% auxiliary non-terminals

	bytes_until_break([Byte| Bytes]) -->
		[Byte], {Byte =\= 0xff}, !, bytes_until_break(Bytes).
	bytes_until_break([]) -->
		[0xff].

	bytes(0, []) -->
		!, [].
	bytes(N, [Byte| Bytes]) -->
		[Byte], {M is N - 1}, bytes(M, Bytes).

	bytes_reversed(Length, Bytes) -->
		bytes_reversed(Length, [], Bytes).

	bytes_reversed(0, Bytes, Bytes) -->
		!, [].
	bytes_reversed(N, Bytes0, Bytes) -->
		[Byte], {M is N - 1}, bytes_reversed(M, [Byte| Bytes0], Bytes).

	% auxiliary predicates

	int_bytes(8, Int, [B7, B6, B5, B4, B3, B2, B1, B0]) :-
		B7 is Int >> 56 /\ 0xff,
		B6 is Int >> 48 /\ 0xff,
		B5 is Int >> 40 /\ 0xff,
		B4 is Int >> 32 /\ 0xff,
		B3 is Int >> 24 /\ 0xff,
		B2 is Int >> 16 /\ 0xff,
		B1 is Int >>  8 /\ 0xff,
		B0 is Int >>  0 /\ 0xff.

	int_bytes(4, Int, [B3, B2, B1, B0]) :-
		B3 is Int >> 24 /\ 0xff,
		B2 is Int >> 16 /\ 0xff,
		B1 is Int >>  8 /\ 0xff,
		B0 is Int >>  0 /\ 0xff.

	int_bytes(2, Int, [B1, B0]) :-
		B1 is Int >> 8 /\ 0xff,
		B0 is Int >> 0 /\ 0xff.

	int_bytes(Int, Bytes) :-
		int_num_bytes(Int, N),
		int_bytes_n(N, Int, Bytes).

	int_bytes_n(1, Int, [Byte]) :-
		!,
		Byte is Int >> 0 /\ 0xff.
	int_bytes_n(N, Int, [Byte| Bytes]) :-
		Byte is Int >> ((N-1) * 8) /\ 0xff,
		M is N - 1,
		int_bytes_n(M, Int, Bytes).

	int_num_bytes(Int, N) :-
		int_num_bytes(Int, 1, N).

	int_num_bytes(Int, N0, N) :-
		Int > 255,
		!,
		N1 is N0 + 1,
		int_num_bytes(Int div 256, N1, N).
	int_num_bytes(_, N, N).

	bytes_int(Bytes, Int) :-
		bytes_int(Bytes, 0, 0, Int).

	bytes_int([], _, Int, Int).
	bytes_int([Byte| Bytes], S0, Int0, Int) :-
		Int1 is Int0 + Byte << S0,
		S1 is S0 + 8,
		bytes_int(Bytes, S1, Int1, Int).

	float_to_mantissa_and_exponent(Float, Mantissa, Exponent) :-
		number_codes(Float, Codes),
		float_codes_to_mantissa_and_exponent(Codes, MantissaCodes, Exponent),
		number_codes(Mantissa, MantissaCodes).

	float_codes_to_mantissa_and_exponent([0'.| Codes], Mantissa, Exponent) :-
		!,
		float_codes_to_mantissa_and_exponent(Codes, Mantissa, 0, Exponent).
	float_codes_to_mantissa_and_exponent([Code| Codes], [Code| Mantissa], Exponent) :-
		float_codes_to_mantissa_and_exponent(Codes, Mantissa, Exponent).

	float_codes_to_mantissa_and_exponent([], [], Exponent, Exponent).
	float_codes_to_mantissa_and_exponent([Code, Sign| Codes], [], Exponent0, Exponent) :-
		(Code == 0'e; Code == 0'E),
		!,
		number_codes(Exponent1, Codes),
		(	Sign == 0'- ->
			Exponent is Exponent0 - Exponent1
		;	Exponent is Exponent0 + Exponent1
		).
	float_codes_to_mantissa_and_exponent([Code| Codes], [Code| Mantissa], Exponent0, Exponent) :-
		Exponent1 is Exponent0 - 1,
		float_codes_to_mantissa_and_exponent(Codes, Mantissa, Exponent1, Exponent).

	mantissa_and_exponent_to_float(Mantissa, Exponent, Float) :-
		Float is float(Mantissa * 10**Exponent).

	half_precision_to_float(0x00, 0x00, 0.0) :- !.
	half_precision_to_float(0x80, 0x00, -0.0) :- !.
	half_precision_to_float(0x7c, 0x00, inf) :- !.
	half_precision_to_float(0x7e, 0x00, nan) :- !.
	half_precision_to_float(0xfc, 0x00, neginf) :- !.
	half_precision_to_float(Byte1, Byte0, Float) :-
		Sign is Byte1 >> 7,
		Exponent is Byte1 >> 2 /\ 0x1f,
		Significand is (Byte1 /\ 0x03) << 8 + Byte0,
		(	Exponent =:= 0 ->
			Float is (-1) ** Sign * 2 ** (Exponent - 14) * (Significand / 1024.0)
		;	Float is (-1) ** Sign * 2 ** (Exponent - 15) * (1 + Significand / 1024.0)
		).

	single_precision_to_float(0x00, 0x00, 0x00, 0x00, 0.0) :- !.
	single_precision_to_float(0x10, 0x00, 0x00, 0x00, -0.0) :- !.
	single_precision_to_float(0x7f, 0x80, 0x00, 0x00, inf) :- !.
	single_precision_to_float(0x7f, 0xc0, 0x00, 0x00, nan) :- !.
	single_precision_to_float(0xff, 0x80, 0x00, 0x00, neginf) :- !.
	single_precision_to_float(Byte3, Byte2, Byte1, Byte0, Float) :-
		Sign is Byte3 >> 7,
		Exponent is (Byte3 /\ 0x7f) << 1 + Byte2 >> 7,
		Significand is (Byte2 /\ 0x7f) << 16 + Byte1 << 8 + Byte0,
		(	Exponent =:= 0 ->
			Float is (-1) ** Sign * (Significand * 2 ** -23) * 2 ** (Exponent - 127)
		;	Float is (-1) ** Sign * (1 + Significand * 2 ** -23) * 2 ** (Exponent - 127)
		).

	double_precision_to_float(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0.0) :- !.
	double_precision_to_float(0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, -0.0) :- !.
	double_precision_to_float(0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, inf) :- !.
	double_precision_to_float(0x7f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, nan) :- !.
	double_precision_to_float(0xff, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, neginf) :- !.
	double_precision_to_float(Byte7, Byte6, Byte5, Byte4, Byte3, Byte2, Byte1, Byte0, Float) :-
		Sign is Byte7 >> 7,
		Exponent is (Byte7 /\ 0x7f) << 4 + Byte6 >> 4,
		Significand is (Byte6 /\ 0x0f) << 48 + Byte5 << 40 + Byte4 << 32 + Byte3 << 24 + Byte2 << 16 + Byte1 << 8 + Byte0,
		(	Exponent =:= 0 ->
			Float is (-1) ** Sign * (Significand * 2 ** -52) * 2 ** (Exponent - 1023)
		;	Float is (-1) ** Sign * (1 + Significand * 2 ** -52) * 2 ** (Exponent - 1023)
		).

	utf_8_codes_to_bytes([], []).
	utf_8_codes_to_bytes([Code| Codes], [Code| Bytes]) :-
		Code < 0x80,
		!,
		utf_8_codes_to_bytes(Codes, Bytes).
	utf_8_codes_to_bytes([Code| Codes], [Byte1, Byte2| Bytes]) :-
		Code < 0x800,
		!,
		Byte1 is 0xc0 \/ (Code >> 6),
		Byte2 is 0x80 \/ (Code /\ 0x3f),
		utf_8_codes_to_bytes(Codes, Bytes).
	utf_8_codes_to_bytes([Code| Codes], [Byte1, Byte2, Byte3| Bytes]) :-
		Code < 0x10000,
		!,
		Byte1 is 0xe0 \/ (Code >> 12),
		Byte2 is 0x80 \/ ((Code >> 6) /\ 0x3f),
		Byte3 is 0x80 \/ (Code /\ 0x3f),
		utf_8_codes_to_bytes(Codes, Bytes).
	utf_8_codes_to_bytes([Code| Codes], [Byte1, Byte2, Byte3, Byte4| Bytes]) :-
		Byte1 is 0xF0 \/ ((Code >> 18) /\ 0x07),
		Byte2 is 0x80 \/ ((Code >> 12) /\ 0x3f),
		Byte3 is 0x80 \/ ((Code >>  6) /\ 0x3f),
		Byte4 is 0x80 \/ ((Code >>  0) /\ 0x3f),
		utf_8_codes_to_bytes(Codes, Bytes).

	bytes_to_utf_8_codes([], []).
	bytes_to_utf_8_codes([Byte| Bytes], [Byte| Codes]) :-
		Byte < 0x80,
		!,
		bytes_to_utf_8_codes(Bytes, Codes).
	bytes_to_utf_8_codes([Byte1, Byte2| Bytes], [Code| Codes]) :-
		Byte1 < 0xe0,
		!,
		Code is ((Byte1 /\ 0x1f) << 6) \/ (Byte2 /\ 0x3f),
		bytes_to_utf_8_codes(Bytes, Codes).
	bytes_to_utf_8_codes([Byte1, Byte2, Byte3| Bytes], [Code| Codes]) :-
		Byte1 < 0xf0,
		!,
		Code is ((Byte1 /\ 0xf) << 12) \/ ((Byte2 /\ 0x3f) << 6) \/ (Byte3 /\ 0x3f),
		bytes_to_utf_8_codes(Bytes, Codes).
	bytes_to_utf_8_codes([Byte1, Byte2, Byte3, Byte4| Bytes], [Code| Codes]) :-
		Code is ((Byte1 /\ 0x7) << 18) \/ ((Byte2 /\ 0x3f) << 12) \/ ((Byte3 /\ 0x3f) << 6) \/ (Byte4 /\ 0x3f),
		bytes_to_utf_8_codes(Bytes, Codes).

:- end_object.
