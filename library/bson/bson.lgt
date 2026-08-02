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


:- object(bson(_StringRepresentation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'BSON format exporter and importer.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding BSON strings and keys. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- public(parse/2).
	:- mode(parse(@list(byte), -ground), one_or_error).
	:- info(parse/2, [
		comment is 'Parses a list of bytes in the BSON format returning the corresponding document term. Throws an error when parsing is not possible.',
		argnames is ['Bytes', 'Document'],
		exceptions is [
			'``Bytes`` is not ground' - instantiation_error,
			'``Bytes`` is ground but not a valid BSON byte sequence' - domain_error(bson_byte_sequence, 'Bytes')
		]
	]).

	:- public(generate/2).
	:- mode(generate(@ground, -list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a list of bytes in the BSON format representing the given document term. Throws an error when generating is not possible.',
		argnames is ['Document', 'Bytes'],
		exceptions is [
			'``Document`` is a variable' - instantiation_error,
			'``Document`` has no corresponding BSON representation' - domain_error(bson_term, 'Document')
		]
	]).

	:- uses(list, [
		length/2, member/2
	]).

	generate(Document, Bytes) :-
		phrase(encode_document(Document), Bytes),
		!.
	generate(Document, _) :-
		domain_error(bson_term, Document).

	parse(Bytes, Document) :-
		phrase(decode_document(Document), Bytes),
		!.
	parse(Bytes, _) :-
		domain_error(bson_byte_sequence, Bytes).

	encode_document(Document) -->
		{var(Document), instantiation_error}.
	encode_document({}) -->
		!,
		encode_document_bytes([]).
	encode_document({Pairs}) -->
		!,
		{phrase(encode_pairs(Pairs), Body)},
		encode_document_bytes(Body).

	encode_document_bytes(Body) -->
		{
			length(Body, BodyLength),
			Length is BodyLength + 5,
			Length =< 2147483647,
			integer_to_little_endian_bytes(4, Length, LengthBytes)
		},
		bytes(LengthBytes), bytes(Body), [0].

	encode_pairs(Term) -->
		{	var(Term),
			instantiation_error
		}.
	encode_pairs((Pair, Pairs)) -->
		!,
		encode_pair(Pair),
		encode_pairs(Pairs).
	encode_pairs(Pair) -->
		encode_pair(Pair).

	encode_pair(Key-Value) -->
		!,
		encode_value(Value, Type, Bytes),
		[Type], encode_cstring(Key), bytes(Bytes).
	encode_pair(_) -->
		{representation_error(pair)}.

	encode_value(Term, _, _) -->
		{	var(Term),
			instantiation_error
		}.
	encode_value(@false, 0x08, [0]) -->
		!.
	encode_value(@true, 0x08, [1]) -->
		!.
	encode_value(@null, 0x0a, []) -->
		!.
	encode_value(@undefined, 0x06, []) -->
		!.
	encode_value(@min_key, 0xff, []) -->
		!.
	encode_value(@max_key, 0x7f, []) -->
		!.
	encode_value({}, 0x03, Bytes) -->
		!,
		{phrase(encode_document({}), Bytes)}.
	encode_value({Pairs}, 0x03, Bytes) -->
		!,
		{phrase(encode_document({Pairs}), Bytes)}.
	encode_value(List, 0x04, Bytes) -->
		{nonvar(List), List = [_| _]},
		!,
		{phrase(encode_array(List), Bytes)}.
	encode_value([], 0x04, Bytes) -->
		!,
		{phrase(encode_array([]), Bytes)}.
	encode_value(int32(Integer), 0x10, Bytes) -->
		!,
		{	valid_signed_integer(32, Integer),
			integer_to_little_endian_bytes(4, Integer, Bytes)
		}.
	encode_value(int64(Integer), 0x12, Bytes) -->
		!,
		{	valid_signed_integer(64, Integer),
			integer_to_little_endian_bytes(8, Integer, Bytes)
		}.
	encode_value(binary(Subtype, bytes(Payload)), 0x05, Bytes) -->
		!,
		{encode_binary(Subtype, Payload, Bytes)}.
	encode_value(object_id(bytes(Bytes)), 0x07, Bytes) -->
		!,
		{byte_list(Bytes), length(Bytes, 12)}.
	encode_value(date_time(Milliseconds), 0x09, Bytes) -->
		!,
		{	valid_signed_integer(64, Milliseconds),
			integer_to_little_endian_bytes(8, Milliseconds, Bytes)
		}.
	encode_value(regular_expression(Pattern, Options), 0x0b, Bytes) -->
		!,
		{	valid_regular_expression_options(Options),
			phrase((encode_cstring(Pattern), encode_cstring(Options)), Bytes)
		}.
	encode_value(db_pointer(Namespace, object_id(bytes(ObjectId))), 0x0c, Bytes) -->
		!,
		{	byte_list(ObjectId),
			length(ObjectId, 12),
			phrase((encode_string(Namespace), bytes(ObjectId)), Bytes)
		}.
	encode_value(javascript(Code), 0x0d, Bytes) -->
		!,
		{phrase(encode_string(Code), Bytes)}.
	encode_value(symbol(Symbol), 0x0e, Bytes) -->
		!,
		{phrase(encode_string(Symbol), Bytes)}.
	encode_value(javascript(Code, Scope), 0x0f, Bytes) -->
		!,
		{encode_javascript_scope(Code, Scope, Bytes)}.
	encode_value(timestamp(Increment, Seconds), 0x11, Bytes) -->
		!,
		{	valid_unsigned_integer(32, Increment),
			valid_unsigned_integer(32, Seconds),
			integer_to_little_endian_bytes(4, Increment, IncrementBytes),
			integer_to_little_endian_bytes(4, Seconds, SecondsBytes),
			phrase((bytes(IncrementBytes), bytes(SecondsBytes)), Bytes)
		}.
	encode_value(decimal128(bytes(Bytes)), 0x13, Bytes) -->
		!,
		{byte_list(Bytes), length(Bytes, 16)}.
	encode_value(Integer, Type, Bytes) -->
		{integer(Integer)},
		!,
		{(	valid_signed_integer(32, Integer) ->
			Type = 0x10,
			integer_to_little_endian_bytes(4, Integer, Bytes)
		;	valid_signed_integer(64, Integer),
			Type = 0x12,
			integer_to_little_endian_bytes(8, Integer, Bytes)
		)}.
	encode_value(Float, 0x01, Bytes) -->
		{float(Float)},
		!,
		{ieee_754(double, little, payloads)::generate(bytes(Bytes), Float)}.
	encode_value(@Literal, 0x01, Bytes) -->
		{member(Literal, [infinity, negative_infinity, not_a_number])},
		!,
		{ieee_754(double, little, payloads)::generate(bytes(Bytes), @Literal)}.
	encode_value(not_a_number(Payload), 0x01, Bytes) -->
		!,
		{ieee_754(double, little, payloads)::generate(bytes(Bytes), not_a_number(Payload))}.
	encode_value(String, 0x02, Bytes) -->
		{phrase(encode_string(String), Bytes)},
		!,
		[].

	encode_string(String) -->
		{	string_codes(String, Codes),
			utf_8::codes_to_bytes(Codes, StringBytes),
			length(StringBytes, Length0),
			Length is Length0 + 1,
			Length =< 2147483647,
			integer_to_little_endian_bytes(4, Length, LengthBytes)
		},
		bytes(LengthBytes), bytes(StringBytes), [0].

	encode_binary(Subtype, Payload, Bytes) :-
		valid_binary_subtype(Subtype),
		byte_list(Payload),
		length(Payload, PayloadLength),
		(	Subtype =:= 2 ->
			Length is PayloadLength + 4,
			Length =< 2147483647,
			integer_to_little_endian_bytes(4, Length, LengthBytes),
			integer_to_little_endian_bytes(4, PayloadLength, PayloadLengthBytes),
			phrase((bytes(LengthBytes), [Subtype], bytes(PayloadLengthBytes), bytes(Payload)), Bytes)
		;	PayloadLength =< 2147483647,
			integer_to_little_endian_bytes(4, PayloadLength, LengthBytes),
			phrase((bytes(LengthBytes), [Subtype], bytes(Payload)), Bytes)
		).

	encode_javascript_scope(Code, Scope, Bytes) :-
		phrase(encode_string(Code), CodeBytes),
		phrase(encode_document(Scope), ScopeBytes),
		length(CodeBytes, CodeLength),
		length(ScopeBytes, ScopeLength),
		Length is CodeLength + ScopeLength + 4,
		Length =< 2147483647,
		integer_to_little_endian_bytes(4, Length, LengthBytes),
		phrase((bytes(LengthBytes), bytes(CodeBytes), bytes(ScopeBytes)), Bytes).

	encode_array(List) -->
		{phrase(encode_array_elements(List, 0), Body)},
		encode_document_bytes(Body).

	encode_array_elements(Term, _) -->
		{	var(Term),
			instantiation_error
		}.
	encode_array_elements([], _) -->
		[].
	encode_array_elements([Value| Values], Index) -->
		{	number_codes(Index, KeyCodes),
			codes_term(KeyCodes, Key)
		},
		encode_pair(Key-Value),
		{NextIndex is Index + 1},
		encode_array_elements(Values, NextIndex).

	encode_cstring(String) -->
		{
			string_codes(String, Codes),
			utf_8::codes_to_bytes(Codes, Bytes),
			\+ member(0, Bytes)
		},
		bytes(Bytes), [0].

	decode_document(Document) -->
		little_endian_signed_integer(4, Length),
		{Length >= 5, PayloadLength is Length - 4},
		bytes(PayloadLength, Payload),
		{phrase(decode_document_payload(Document), Payload)}.

	decode_document_payload(Document) -->
		decode_elements(Pairs),
		{pairs_document(Pairs, Document)}.

	decode_elements([]) -->
		[0],
		!.
	decode_elements([Key-Value| Pairs]) -->
		[Type],
		decode_cstring(Key),
		decode_value(Type, Value),
		decode_elements(Pairs).

	decode_value(0x01, Float) -->
		bytes(8, Bytes),
		{ieee_754(double, little, payloads)::parse(bytes(Bytes), Float)}.
	decode_value(0x02, String) -->
		little_endian_signed_integer(4, Length),
		{	Length >= 1,
			StringLength is Length - 1
		},
		bytes(StringLength, Bytes), [0],
		{	utf_8::bytes_to_codes(Bytes, Codes),
			decoded_string(_StringRepresentation_, Codes, String)
		}.
	decode_value(0x03, Document) -->
		decode_document(Document).
	decode_value(0x04, Array) -->
		decode_document(Document),
		{document_array(Document, Array)}.
	decode_value(0x05, binary(Subtype, bytes(Payload))) -->
		little_endian_signed_integer(4, Length),
		[Subtype],
		{	Length >= 0,
			valid_binary_subtype(Subtype)
		},
		decode_binary(Subtype, Length, Payload).
	decode_value(0x06, @undefined) --> [].
	decode_value(0x07, object_id(bytes(Bytes))) -->
		bytes(12, Bytes).
	decode_value(0x08, @false) -->
		[0].
	decode_value(0x08, @true) -->
		[1].
	decode_value(0x09, date_time(Milliseconds)) -->
		little_endian_signed_integer(8, Milliseconds).
	decode_value(0x0a, @null) -->
		[].
	decode_value(0x0b, regular_expression(Pattern, Options)) -->
		decode_cstring(Pattern),
		decode_cstring(Options),
		{valid_regular_expression_options(Options)}.
	decode_value(0x0c, db_pointer(Namespace, object_id(bytes(ObjectId)))) -->
		decode_string(Namespace),
		bytes(12, ObjectId).
	decode_value(0x0d, javascript(Code)) -->
		decode_string(Code).
	decode_value(0x0e, symbol(Symbol)) -->
		decode_string(Symbol).
	decode_value(0x0f, javascript(Code, Scope)) -->
		little_endian_signed_integer(4, Length),
		{Length >= 14, PayloadLength is Length - 4},
		bytes(PayloadLength, Payload),
		{phrase((decode_string(Code), decode_document(Scope)), Payload)}.
	decode_value(0x10, int32(Integer)) -->
		little_endian_signed_integer(4, Integer).
	decode_value(0x11, timestamp(Increment, Seconds)) -->
		little_endian_unsigned_integer(4, Increment),
		little_endian_unsigned_integer(4, Seconds).
	decode_value(0x12, int64(Integer)) -->
		little_endian_signed_integer(8, Integer).
	decode_value(0x13, decimal128(bytes(Bytes))) -->
		bytes(16, Bytes).
	decode_value(0x7f, @max_key) -->
		[].
	decode_value(0xff, @min_key) -->
		[].

	decode_binary(2, Length, Payload) -->
		!,
		little_endian_signed_integer(4, PayloadLength),
		{	PayloadLength >= 0,
			Length =:= PayloadLength + 4
		},
		bytes(PayloadLength, Payload).
	decode_binary(_, Length, Payload) -->
		bytes(Length, Payload).

	decode_string(String) -->
		little_endian_signed_integer(4, Length),
		{	Length >= 1,
			StringLength is Length - 1
		},
		bytes(StringLength, Bytes), [0],
		{	utf_8::bytes_to_codes(Bytes, Codes),
			decoded_string(_StringRepresentation_, Codes, String)
		}.

	decode_cstring(String) -->
		decode_cstring_bytes(Bytes),
		{	utf_8::bytes_to_codes(Bytes, Codes),
			decoded_string(_StringRepresentation_, Codes, String)
		}.

	decode_cstring_bytes([]) -->
		[0],
		!.
	decode_cstring_bytes([Byte| Bytes]) -->
		[Byte],
		{Byte =\= 0},
		decode_cstring_bytes(Bytes).

	pairs_document([], {}).
	pairs_document([Pair| Pairs], {PairTerm}) :-
		pairs_term(Pairs, Pair, PairTerm).

	pairs_term([], Pair, Pair).
	pairs_term([Pair| Pairs], Pair0, (Pair0, PairTerm)) :-
		pairs_term(Pairs, Pair, PairTerm).

	document_array({}, []).
	document_array({Pairs}, Array) :-
		pairs_array(Pairs, 0, Array).

	pairs_array((Key-Value, Pairs), Index, [Value| Values]) :-
		!,
		array_key(Key, Index),
		NextIndex is Index + 1,
		pairs_array(Pairs, NextIndex, Values).
	pairs_array(Key-Value, Index, [Value]) :-
		array_key(Key, Index).

	array_key(Key, Index) :-
		decoded_string_codes(Key, Codes),
		number_codes(Index, Codes).

	little_endian_signed_integer(Length, Integer) -->
		bytes(Length, Bytes),
		{	little_endian_bytes_to_unsigned_integer(Bytes, Unsigned),
			Bits is Length * 8,
			SignBit is 1 << (Bits - 1),
			(	Unsigned /\ SignBit =:= 0 ->
				Integer = Unsigned
			;	Integer is Unsigned - (1 << Bits)
			)
		}.

	little_endian_unsigned_integer(Length, Integer) -->
		bytes(Length, Bytes),
		{little_endian_bytes_to_unsigned_integer(Bytes, Integer)}.

	integer_to_little_endian_bytes(Length, Integer, Bytes) :-
		Bits is Length * 8,
		( Integer < 0 -> Unsigned is Integer + (1 << Bits); Unsigned = Integer ),
		integer_to_little_endian_bytes_(Length, Unsigned, Bytes).

	integer_to_little_endian_bytes_(0, _, []) :-
		!.
	integer_to_little_endian_bytes_(Length, Integer, [Byte| Bytes]) :-
		Byte is Integer mod 256,
		NextInteger is Integer // 256,
		NextLength is Length - 1,
		integer_to_little_endian_bytes_(NextLength, NextInteger, Bytes).

	little_endian_bytes_to_unsigned_integer(Bytes, Integer) :-
		little_endian_bytes_to_unsigned_integer(Bytes, 1, 0, Integer).

	little_endian_bytes_to_unsigned_integer([], _, Integer, Integer).
	little_endian_bytes_to_unsigned_integer([Byte| Bytes], Multiplier, Integer0, Integer) :-
		Integer1 is Integer0 + Byte * Multiplier,
		NextMultiplier is Multiplier * 256,
		little_endian_bytes_to_unsigned_integer(Bytes, NextMultiplier, Integer1, Integer).

	valid_signed_integer(32, Integer) :-
		integer(Integer), Integer >= -2147483648, Integer =< 2147483647.
	valid_signed_integer(64, Integer) :-
		integer(Integer), Integer >= -9223372036854775808, Integer =< 9223372036854775807.

	valid_unsigned_integer(32, Integer) :-
		integer(Integer), Integer >= 0, Integer =< 4294967295.

	valid_binary_subtype(Subtype) :-
		integer(Subtype),
		( Subtype >= 0, Subtype =< 9 -> true; Subtype >= 128, Subtype =< 255 ).

	valid_regular_expression_options(Options) :-
		string_codes(Options, Codes),
		valid_regular_expression_option_codes(Codes, -1).

	valid_regular_expression_option_codes([], _).
	valid_regular_expression_option_codes([Code| Codes], Previous) :-
		member(Code, [0'i, 0'm, 0's, 0'u, 0'x]),
		Code > Previous,
		valid_regular_expression_option_codes(Codes, Code).

	byte_list([]).
	byte_list([Byte| Bytes]) :-
		integer(Byte), Byte >= 0, Byte =< 255,
		byte_list(Bytes).

	string_codes(Atom, Codes) :-
		atom(Atom),
		atom_codes(Atom, Codes).
	string_codes(chars(Chars), Codes) :-
		chars_to_codes(Chars, Codes).
	string_codes(codes(Codes), Codes).

	decoded_string(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	decoded_string(chars, Codes, chars(Chars)) :-
		codes_to_chars(Codes, Chars).
	decoded_string(codes, Codes, codes(Codes)).

	decoded_string_codes(Atom, Codes) :-
		atom(Atom),
		!,
		atom_codes(Atom, Codes).
	decoded_string_codes(chars(Chars), Codes) :-
		!,
		chars_to_codes(Chars, Codes).
	decoded_string_codes(codes(Codes), Codes).

	codes_term(Codes, Atom) :-
		atom_codes(Atom, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	bytes([]) -->
		[].
	bytes([Byte| Bytes]) -->
		[Byte], bytes(Bytes).

	bytes(0, []) -->
		!,
		[].
	bytes(Length, [Byte| Bytes]) -->
		{Length > 0, NextLength is Length - 1},
		[Byte],
		bytes(NextLength, Bytes).

:- end_object.


:- object(bson,
	extends(bson(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'BSON format exporter and importer using atoms for decoded strings and keys.'
	]).

:- end_object.
