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


:- object(typeid(_Representation_),
	implements(typeid_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-24,
		comment is 'Type-safe, K-sortable, globally unique identifier (TypeID) generator. Implements the TypeID specification (a type prefix plus a version 7 UUID encoded using a strict, lowercase variant of Crockford base32).',
		parameters is [
			'Representation' - 'Text representation for the TypeID, its type prefix, and its UUID suffix. Possible values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [typeid, ids(_,_), cuid2(_,_,_), ksuid(_,_), nanoid(_,_,_), snowflakeid(_,_,_,_,_,_,_), ulid(_), uuid(_)]
	]).

	:- uses(uuid(codes), [
		uuid_v7/1, uuid_v7/2
	]).

	:- uses(list, [
		append/3, last/2, length/2
	]).

	% generation (always uses a version 7 UUID as the suffix)

	generate(TypeID) :-
		generate('', TypeID).

	generate(Prefix, TypeID) :-
		text_codes(Prefix, PrefixCodes),
		valid_prefix_codes(PrefixCodes),
		uuid_v7(UUIDCodes),
		hexadecimal_uuid_bytes(UUIDCodes, Bytes),
		bytes_suffix_codes(Bytes, SuffixCodes),
		compose_codes(PrefixCodes, SuffixCodes, Codes),
		codes_text(_Representation_, Codes, TypeID).

	generate(Prefix, Offset, TypeID) :-
		text_codes(Prefix, PrefixCodes),
		valid_prefix_codes(PrefixCodes),
		uuid_v7(Offset, UUIDCodes),
		hexadecimal_uuid_bytes(UUIDCodes, Bytes),
		bytes_suffix_codes(Bytes, SuffixCodes),
		compose_codes(PrefixCodes, SuffixCodes, Codes),
		codes_text(_Representation_, Codes, TypeID).

	% conversion from/to a UUID (any UUID version is accepted when decoding a TypeID from an user provided UUID)

	from_uuid(Prefix, UUID, TypeID) :-
		text_codes(Prefix, PrefixCodes),
		valid_prefix_codes(PrefixCodes),
		text_codes(UUID, UUIDCodes),
		hexadecimal_uuid_bytes(UUIDCodes, Bytes),
		bytes_suffix_codes(Bytes, SuffixCodes),
		compose_codes(PrefixCodes, SuffixCodes, Codes),
		codes_text(_Representation_, Codes, TypeID).

	to_uuid(TypeID, UUID) :-
		text_codes(TypeID, Codes),
		valid_typeid_codes(Codes, _, SuffixCodes),
		suffix_codes_bytes(SuffixCodes, Bytes),
		bytes_hexadecimal_uuid(Bytes, UUIDCodes),
		codes_text(_Representation_, UUIDCodes, UUID).

	% decomposing and composing

	prefix(TypeID, Prefix) :-
		text_codes(TypeID, Codes),
		valid_typeid_codes(Codes, PrefixCodes, _),
		codes_text(_Representation_, PrefixCodes, Prefix).

	suffix(TypeID, Suffix) :-
		text_codes(TypeID, Codes),
		valid_typeid_codes(Codes, _, SuffixCodes),
		codes_text(_Representation_, SuffixCodes, Suffix).

	decompose(TypeID, Prefix, Suffix) :-
		text_codes(TypeID, Codes),
		valid_typeid_codes(Codes, PrefixCodes, SuffixCodes),
		codes_text(_Representation_, PrefixCodes, Prefix),
		codes_text(_Representation_, SuffixCodes, Suffix).

	compose(Prefix, Suffix, TypeID) :-
		text_codes(Prefix, PrefixCodes),
		valid_prefix_codes(PrefixCodes),
		text_codes(Suffix, SuffixCodes),
		valid_suffix_codes(SuffixCodes),
		compose_codes(PrefixCodes, SuffixCodes, Codes),
		codes_text(_Representation_, Codes, TypeID).

	% validation

	valid(TypeID) :-
		text_codes(TypeID, Codes),
		valid_typeid_codes(Codes, _, _).

	valid_prefix(Prefix) :-
		text_codes(Prefix, PrefixCodes),
		valid_prefix_codes(PrefixCodes).

	valid_suffix(Suffix) :-
		text_codes(Suffix, SuffixCodes),
		valid_suffix_codes(SuffixCodes).

	% auxiliary predicates

	% splits a TypeID into its type prefix and its base32 encoded UUID suffix and
	% checks that both are valid per the TypeID specification; the UUID suffix is
	% always the last 26 characters; when there are extra leading characters, the
	% character immediately before the suffix must be the "_" separator, which in
	% turn requires a non-empty prefix (an empty prefix never takes a separator)

	valid_typeid_codes(Codes, PrefixCodes, SuffixCodes) :-
		decompose_codes(Codes, PrefixCodes, SuffixCodes),
		valid_prefix_codes(PrefixCodes),
		valid_suffix_codes(SuffixCodes).

	decompose_codes(Codes, PrefixCodes, SuffixCodes) :-
		length(Codes, Length),
		Length >= 26,
		(	Length =:= 26 ->
			PrefixCodes = [],
			SuffixCodes = Codes
		;	PrefixLength is Length - 27,
			PrefixLength >= 1,
			length(PrefixCodes, PrefixLength),
			append(PrefixCodes, [0'_| SuffixCodes], Codes)
		).

	compose_codes([], SuffixCodes, SuffixCodes) :-
		!.
	compose_codes(PrefixCodes, SuffixCodes, Codes) :-
		append(PrefixCodes, [0'_| SuffixCodes], Codes).

	% type prefix validation; must match ^([a-z]([a-z_]{0,61}[a-z])?)?$

	valid_prefix_codes([]) :-
		!.
	valid_prefix_codes(Codes) :-
		length(Codes, Length),
		Length =< 63,
		Codes = [First| _],
		lowercase_letter_code(First),
		last(Codes, Last),
		lowercase_letter_code(Last),
		prefix_codes_valid(Codes).

	prefix_codes_valid([]).
	prefix_codes_valid([Code| Codes]) :-
		(	lowercase_letter_code(Code) ->
			true
		;	Code =:= 0'_
		),
		prefix_codes_valid(Codes).

	lowercase_letter_code(Code) :-
		Code >= 0'a,
		Code =< 0'z.

	% UUID suffix validation; must be 26 characters from the base32 alphabet, with
	% the first character decoding to a value no greater than 7 (as required for the
	% suffix to represent no more than the 128 bits of a UUID)

	valid_suffix_codes(Codes) :-
		length(Codes, 26),
		crockford_codes_values(Codes, [FirstValue| _]),
		FirstValue =< 7.

	% base32 (Crockford, strict, lowercase) encoding and decoding of the 16 bytes
	% of a UUID into/from the 26 character suffix of a TypeID; two zero bits are
	% conceptually prepended to the 128 bits of the UUID, resulting in 130 bits
	% that are split, from left (most significant) to right, into 26 groups of
	% 5 bits, each group encoding a single base32 character; this is a fixed
	% bit layout (not a byte oriented, chunked encoding as e.g. the "base32"
	% library implements for RFC 4648 base32) and thus is implemented as a
	% direct bit-level mapping between the 16 bytes and the 26 characters

	bytes_suffix_codes([Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15], Codes) :-
		Value0  is (Byte0 /\ 0xe0) >> 5,
		Value1  is Byte0 /\ 0x1f,
		Value2  is (Byte1 /\ 0xf8) >> 3,
		Value3  is ((Byte1 /\ 0x07) << 2) \/ ((Byte2 /\ 0xc0) >> 6),
		Value4  is (Byte2 /\ 0x3e) >> 1,
		Value5  is ((Byte2 /\ 0x01) << 4) \/ ((Byte3 /\ 0xf0) >> 4),
		Value6  is ((Byte3 /\ 0x0f) << 1) \/ ((Byte4 /\ 0x80) >> 7),
		Value7  is (Byte4 /\ 0x7c) >> 2,
		Value8  is ((Byte4 /\ 0x03) << 3) \/ ((Byte5 /\ 0xe0) >> 5),
		Value9  is Byte5 /\ 0x1f,
		Value10 is (Byte6 /\ 0xf8) >> 3,
		Value11 is ((Byte6 /\ 0x07) << 2) \/ ((Byte7 /\ 0xc0) >> 6),
		Value12 is (Byte7 /\ 0x3e) >> 1,
		Value13 is ((Byte7 /\ 0x01) << 4) \/ ((Byte8 /\ 0xf0) >> 4),
		Value14 is ((Byte8 /\ 0x0f) << 1) \/ ((Byte9 /\ 0x80) >> 7),
		Value15 is (Byte9 /\ 0x7c) >> 2,
		Value16 is ((Byte9 /\ 0x03) << 3) \/ ((Byte10 /\ 0xe0) >> 5),
		Value17 is Byte10 /\ 0x1f,
		Value18 is (Byte11 /\ 0xf8) >> 3,
		Value19 is ((Byte11 /\ 0x07) << 2) \/ ((Byte12 /\ 0xc0) >> 6),
		Value20 is (Byte12 /\ 0x3e) >> 1,
		Value21 is ((Byte12 /\ 0x01) << 4) \/ ((Byte13 /\ 0xf0) >> 4),
		Value22 is ((Byte13 /\ 0x0f) << 1) \/ ((Byte14 /\ 0x80) >> 7),
		Value23 is (Byte14 /\ 0x7c) >> 2,
		Value24 is ((Byte14 /\ 0x03) << 3) \/ ((Byte15 /\ 0xe0) >> 5),
		Value25 is Byte15 /\ 0x1f,
		crockford_values_codes(
			[Value0, Value1, Value2, Value3, Value4, Value5, Value6, Value7, Value8, Value9, Value10, Value11, Value12, Value13, Value14, Value15, Value16, Value17, Value18, Value19, Value20, Value21, Value22, Value23, Value24, Value25],
			Codes
		).

	suffix_codes_bytes(Codes, [Byte0, Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15]) :-
		crockford_codes_values(
			Codes,
			[Value0, Value1, Value2, Value3, Value4, Value5, Value6, Value7, Value8, Value9, Value10, Value11, Value12, Value13, Value14, Value15, Value16, Value17, Value18, Value19, Value20, Value21, Value22, Value23, Value24, Value25]
		),
		Byte0  is ((Value0 << 5) \/ Value1) /\ 0xff,
		Byte1  is ((Value2 << 3) \/ (Value3 >> 2)) /\ 0xff,
		Byte2  is ((Value3 << 6) \/ (Value4 << 1) \/ (Value5 >> 4)) /\ 0xff,
		Byte3  is ((Value5 << 4) \/ (Value6 >> 1)) /\ 0xff,
		Byte4  is ((Value6 << 7) \/ (Value7 << 2) \/ (Value8 >> 3)) /\ 0xff,
		Byte5  is ((Value8 << 5) \/ Value9) /\ 0xff,
		Byte6  is ((Value10 << 3) \/ (Value11 >> 2)) /\ 0xff,
		Byte7  is ((Value11 << 6) \/ (Value12 << 1) \/ (Value13 >> 4)) /\ 0xff,
		Byte8  is ((Value13 << 4) \/ (Value14 >> 1)) /\ 0xff,
		Byte9  is ((Value14 << 7) \/ (Value15 << 2) \/ (Value16 >> 3)) /\ 0xff,
		Byte10 is ((Value16 << 5) \/ Value17) /\ 0xff,
		Byte11 is ((Value18 << 3) \/ (Value19 >> 2)) /\ 0xff,
		Byte12 is ((Value19 << 6) \/ (Value20 << 1) \/ (Value21 >> 4)) /\ 0xff,
		Byte13 is ((Value21 << 4) \/ (Value22 >> 1)) /\ 0xff,
		Byte14 is ((Value22 << 7) \/ (Value23 << 2) \/ (Value24 >> 3)) /\ 0xff,
		Byte15 is ((Value24 << 5) \/ Value25) /\ 0xff.

	crockford_values_codes([], []).
	crockford_values_codes([Value| Values], [Code| Codes]) :-
		crockford_symbol(Value, Code),
		crockford_values_codes(Values, Codes).

	crockford_codes_values([], []).
	crockford_codes_values([Code| Codes], [Value| Values]) :-
		crockford_symbol(Value, Code), !,
		crockford_codes_values(Codes, Values).

	% the alphabet used is 0123456789abcdefghjkmnpqrstvwxyz (Crockford base32,
	% lowercase only; "i", "l", "o", and "u" are not used, avoiding ambiguity
	% with "1", "1", "0", and "v"/"y"); decoding is strict: no case folding and
	% no mapping of ambiguous characters, as mandated by the TypeID specification

	crockford_symbol( 0, 0'0).
	crockford_symbol( 1, 0'1).
	crockford_symbol( 2, 0'2).
	crockford_symbol( 3, 0'3).
	crockford_symbol( 4, 0'4).
	crockford_symbol( 5, 0'5).
	crockford_symbol( 6, 0'6).
	crockford_symbol( 7, 0'7).
	crockford_symbol( 8, 0'8).
	crockford_symbol( 9, 0'9).
	crockford_symbol(10, 0'a).
	crockford_symbol(11, 0'b).
	crockford_symbol(12, 0'c).
	crockford_symbol(13, 0'd).
	crockford_symbol(14, 0'e).
	crockford_symbol(15, 0'f).
	crockford_symbol(16, 0'g).
	crockford_symbol(17, 0'h).
	crockford_symbol(18, 0'j).
	crockford_symbol(19, 0'k).
	crockford_symbol(20, 0'm).
	crockford_symbol(21, 0'n).
	crockford_symbol(22, 0'p).
	crockford_symbol(23, 0'q).
	crockford_symbol(24, 0'r).
	crockford_symbol(25, 0's).
	crockford_symbol(26, 0't).
	crockford_symbol(27, 0'v).
	crockford_symbol(28, 0'w).
	crockford_symbol(29, 0'x).
	crockford_symbol(30, 0'y).
	crockford_symbol(31, 0'z).

	% hexadecimal parsing (case-insensitive) and formatting (lowercase) of the
	% standard 8-4-4-4-12 hyphenated UUID text representation into/from bytes

	hexadecimal_uuid_bytes(Codes, Bytes) :-
		phrase(hexadecimal_uuid_bytes_(Bytes), Codes).

	hexadecimal_uuid_bytes_([Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]) -->
		hexadecimal_byte(Byte1),
		hexadecimal_byte(Byte2),
		hexadecimal_byte(Byte3),
		hexadecimal_byte(Byte4),
		[0'-],
		hexadecimal_byte(Byte5),
		hexadecimal_byte(Byte6),
		[0'-],
		hexadecimal_byte(Byte7),
		hexadecimal_byte(Byte8),
		[0'-],
		hexadecimal_byte(Byte9),
		hexadecimal_byte(Byte10),
		[0'-],
		hexadecimal_byte(Byte11),
		hexadecimal_byte(Byte12),
		hexadecimal_byte(Byte13),
		hexadecimal_byte(Byte14),
		hexadecimal_byte(Byte15),
		hexadecimal_byte(Byte16).

	hexadecimal_byte(Byte) -->
		[Code1, Code2],
		{	hexadecimal_code_decimal(Code1, Decimal1),
			hexadecimal_code_decimal(Code2, Decimal2),
			Byte is Decimal1*16 + Decimal2
		}.

	bytes_hexadecimal_uuid(Bytes, Codes) :-
		phrase(bytes_hexadecimal_uuid_(Bytes), Codes).

	bytes_hexadecimal_uuid_([Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]) -->
		bytes_to_hexadecimal_codes([Byte1, Byte2, Byte3, Byte4]),
		[0'-],
		bytes_to_hexadecimal_codes([Byte5, Byte6]),
		[0'-],
		bytes_to_hexadecimal_codes([Byte7, Byte8]),
		[0'-],
		bytes_to_hexadecimal_codes([Byte9, Byte10]),
		[0'-],
		bytes_to_hexadecimal_codes([Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]).

	bytes_to_hexadecimal_codes([]) -->
		[].
	bytes_to_hexadecimal_codes([Byte| Bytes]) -->
		byte_to_hexadecimal_codes(Byte),
		bytes_to_hexadecimal_codes(Bytes).

	byte_to_hexadecimal_codes(Byte) -->
		{Code1 is Byte >> 4, Code2 is Byte /\ 0xf},
		decimal_hexadecimal_code(Code1),
		decimal_hexadecimal_code(Code2).

	decimal_hexadecimal_code( 0) --> [0'0].
	decimal_hexadecimal_code( 1) --> [0'1].
	decimal_hexadecimal_code( 2) --> [0'2].
	decimal_hexadecimal_code( 3) --> [0'3].
	decimal_hexadecimal_code( 4) --> [0'4].
	decimal_hexadecimal_code( 5) --> [0'5].
	decimal_hexadecimal_code( 6) --> [0'6].
	decimal_hexadecimal_code( 7) --> [0'7].
	decimal_hexadecimal_code( 8) --> [0'8].
	decimal_hexadecimal_code( 9) --> [0'9].
	decimal_hexadecimal_code(10) --> [0'a].
	decimal_hexadecimal_code(11) --> [0'b].
	decimal_hexadecimal_code(12) --> [0'c].
	decimal_hexadecimal_code(13) --> [0'd].
	decimal_hexadecimal_code(14) --> [0'e].
	decimal_hexadecimal_code(15) --> [0'f].

	hexadecimal_code_decimal(0'0,  0).
	hexadecimal_code_decimal(0'1,  1).
	hexadecimal_code_decimal(0'2,  2).
	hexadecimal_code_decimal(0'3,  3).
	hexadecimal_code_decimal(0'4,  4).
	hexadecimal_code_decimal(0'5,  5).
	hexadecimal_code_decimal(0'6,  6).
	hexadecimal_code_decimal(0'7,  7).
	hexadecimal_code_decimal(0'8,  8).
	hexadecimal_code_decimal(0'9,  9).
	hexadecimal_code_decimal(0'a, 10).
	hexadecimal_code_decimal(0'b, 11).
	hexadecimal_code_decimal(0'c, 12).
	hexadecimal_code_decimal(0'd, 13).
	hexadecimal_code_decimal(0'e, 14).
	hexadecimal_code_decimal(0'f, 15).
	hexadecimal_code_decimal(0'A, 10).
	hexadecimal_code_decimal(0'B, 11).
	hexadecimal_code_decimal(0'C, 12).
	hexadecimal_code_decimal(0'D, 13).
	hexadecimal_code_decimal(0'E, 14).
	hexadecimal_code_decimal(0'F, 15).

	% generic text (atom, list of characters, or list of character codes) handling

	text_codes(Text, Codes) :-
		atom(Text),
		!,
		atom_codes(Text, Codes).
	text_codes(Text, Codes) :-
		text_list_codes(Text, Codes).

	text_list_codes([], []).
	text_list_codes([Element| Elements], [Code| Codes]) :-
		integer(Element),
		!,
		Code = Element,
		text_list_codes(Elements, Codes).
	text_list_codes([Element| Elements], [Code| Codes]) :-
		char_code(Element, Code),
		text_list_codes(Elements, Codes).

	codes_text(atom, Codes, Text) :-
		atom_codes(Text, Codes).
	codes_text(chars, Codes, Text) :-
		codes_chars(Codes, Text).
	codes_text(codes, Codes, Codes).

	codes_chars([], []).
	codes_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_chars(Codes, Chars).

:- end_object.


:- object(typeid,
	extends(typeid(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-24,
		comment is 'Type-safe, K-sortable, globally unique identifier (TypeID) generator using an atom representation.',
		see_also is [typeid(_), ids, cuid2, ksuid, nanoid, snowflakeid, ulid, uuid]
	]).

:- end_object.
