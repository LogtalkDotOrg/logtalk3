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


:- object(json_pointer(_StringRepresentation_),
	implements(json_pointer_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-04,
		comment is 'JSON Pointer (RFC 6901) and Relative JSON Pointer parser, generator, and evaluator.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used for reference tokens. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- uses(utf_8, [
		codes_to_bytes/2, bytes_to_codes/2
	]).

	:- uses(list, [
		append/3, nth0/3, valid/1 as proper_list/1
	]).

	parse(Source, Pointer) :-
		parse_source_codes(Source, Codes),
		parse_pointer_codes(Codes, Pointer).

	parse_relative(Source, RelativePointer) :-
		parse_relative_source_codes(Source, Codes),
		parse_relative_pointer_codes(Codes, RelativePointer).

	generate(Sink, _) :-
		var(Sink),
		!,
		instantiation_error.
	generate(Sink, Pointer) :-
		pointer_codes(Pointer, Codes),
		generate_sink_codes(Sink, Codes),
		!.
	generate(Sink, _) :-
		domain_error(json_pointer_sink, Sink).

	parse_fragment(Source, Pointer) :-
		parse_fragment_source_codes(Source, Codes),
		fragment_codes_pointer_codes(Codes, PointerCodes),
		parse_pointer_codes(PointerCodes, Pointer).

	generate_fragment(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate_fragment(Sink, Pointer) :-
		pointer_codes(Pointer, PointerCodes),
		codes_to_bytes(PointerCodes, Bytes),
		fragment_bytes_codes(Bytes, Codes, []),
		generate_fragment_sink_codes(Sink, [0'#| Codes]),
		!.
	generate_fragment(Sink, _) :-
		domain_error(json_pointer_fragment_sink, Sink).

	generate_relative(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate_relative(Sink, RelativePointer) :-
		relative_pointer_codes(RelativePointer, Codes),
		generate_sink_codes(Sink, Codes),
		!.
	generate_relative(Sink, _) :-
		domain_error(json_relative_pointer_sink, Sink).

	evaluate(Pointer, _, _) :-
		var(Pointer),
		instantiation_error.
	evaluate(_, JSON, _) :-
		var(JSON),
		instantiation_error.
	evaluate(Pointer, JSON, Value) :-
		evaluate_pointer(Pointer, JSON, Value).

	evaluate_relative(RelativePointer, _, _, _) :-
		var(RelativePointer),
		instantiation_error.
	evaluate_relative(_, Context, _, _) :-
		var(Context),
		instantiation_error.
	evaluate_relative(_, _, JSON, _) :-
		var(JSON),
		instantiation_error.
	evaluate_relative(RelativePointer, Context, JSON, Value) :-
		( 	proper_list(Context) ->
			true
		; 	type_error(list, Context)
		),
		relative_pointer_base(RelativePointer, Context, JSON, BasePointer, Suffix),
		( 	Suffix == '#' ->
			evaluate_relative_hash(BasePointer, JSON, Value)
		; 	append(BasePointer, Suffix, Pointer),
			evaluate_pointer(Pointer, JSON, Value)
		).

	parse_source_codes(Source, _) :-
		var(Source),
		!,
		instantiation_error.
	parse_source_codes(codes(Codes), Codes) :-
		!.
	parse_source_codes(chars(Chars), Codes) :-
		!,
		chars_to_codes(Chars, Codes).
	parse_source_codes(atom(Atom), Codes) :-
		!,
		atom_codes(Atom, Codes).
	parse_source_codes(Source, _) :-
		domain_error(json_pointer_source, Source).

	parse_fragment_source_codes(Source, _) :-
		var(Source),
		!,
		instantiation_error.
	parse_fragment_source_codes(codes(Codes), Codes) :-
		!.
	parse_fragment_source_codes(chars(Chars), Codes) :-
		!,
		chars_to_codes(Chars, Codes).
	parse_fragment_source_codes(atom(Atom), Codes) :-
		!,
		atom_codes(Atom, Codes).
	parse_fragment_source_codes(Source, _) :-
		domain_error(json_pointer_fragment_source, Source).

	parse_relative_source_codes(Source, _) :-
		var(Source),
		instantiation_error.
	parse_relative_source_codes(codes(Codes), Codes) :-
		!.
	parse_relative_source_codes(chars(Chars), Codes) :-
		!,
		chars_to_codes(Chars, Codes).
	parse_relative_source_codes(atom(Atom), Codes) :-
		!,
		atom_codes(Atom, Codes).
	parse_relative_source_codes(Source, _) :-
		domain_error(json_relative_pointer_source, Source).

	generate_sink_codes(codes(Codes), PointerCodes) :-
		!,
		Codes = PointerCodes.
	generate_sink_codes(chars(Chars), PointerCodes) :-
		!,
		codes_to_chars(PointerCodes, Chars).
	generate_sink_codes(atom(Atom), PointerCodes) :-
		!,
		atom_codes(Atom, PointerCodes).

	generate_fragment_sink_codes(codes(Codes), FragmentCodes) :-
		!,
		Codes = FragmentCodes.
	generate_fragment_sink_codes(chars(Chars), FragmentCodes) :-
		!,
		codes_to_chars(FragmentCodes, Chars).
	generate_fragment_sink_codes(atom(Atom), FragmentCodes) :-
		!,
		atom_codes(Atom, FragmentCodes).

	parse_pointer_codes([], []) :-
		!.
	parse_pointer_codes([0'/| Codes], Pointer) :-
		!,
		parse_pointer_tokens(Codes, Pointer).
	parse_pointer_codes(_, _) :-
		fail.

	parse_relative_pointer_codes(Codes, relative(Up, Shift, Suffix)) :-
		parse_non_negative_integer(Codes, Up, RemainingCodes),
		parse_relative_shift(RemainingCodes, Shift, SuffixCodes),
		parse_relative_suffix(SuffixCodes, Suffix).

	parse_non_negative_integer([0'0| Codes], 0, Codes) :-
		!.
	parse_non_negative_integer([Code| Codes], Integer, RemainingCodes) :-
		Code >= 0'1,
		Code =< 0'9,
		parse_decimal_digits(Codes, Digits, RemainingCodes),
		number_codes(Integer, [Code| Digits]).

	parse_decimal_digits([Code| Codes], [Code| Digits], RemainingCodes) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		parse_decimal_digits(Codes, Digits, RemainingCodes).
	parse_decimal_digits(Codes, [], Codes).

	parse_relative_shift([Sign| Codes], Shift, RemainingCodes) :-
		(Sign == 0'+; Sign == 0'-),
		!,
		parse_non_negative_integer(Codes, Magnitude, RemainingCodes),
		( 	Sign == 0'+ ->
			Shift = Magnitude
		; 	Shift is -Magnitude
		).
	parse_relative_shift(Codes, 0, Codes).

	parse_relative_suffix([0'#], '#') :-
		!.
	parse_relative_suffix(Codes, Pointer) :-
		parse_pointer_codes(Codes, Pointer).

	parse_pointer_tokens(Codes, [Token| Tokens]) :-
		split_pointer_segment(Codes, SegmentCodes, RemainingCodes),
		decode_pointer_segment_codes(SegmentCodes, DecodedCodes),
		codes_text(DecodedCodes, Token),
		(	RemainingCodes == [] ->
			Tokens = []
		;	parse_pointer_tokens(RemainingCodes, Tokens)
		).

	split_pointer_segment([], [], []) :-
		!.
	split_pointer_segment([0'/| Codes], [], Codes) :-
		!.
	split_pointer_segment([Code| Codes], [Code| SegmentCodes], RemainingCodes) :-
		split_pointer_segment(Codes, SegmentCodes, RemainingCodes).

	decode_pointer_segment_codes([], []) :-
		!.
	decode_pointer_segment_codes([0'~, 0'0| Codes], [0'~| DecodedCodes]) :-
		!,
		decode_pointer_segment_codes(Codes, DecodedCodes).
	decode_pointer_segment_codes([0'~, 0'1| Codes], [0'/| DecodedCodes]) :-
		!,
		decode_pointer_segment_codes(Codes, DecodedCodes).
	decode_pointer_segment_codes([0'~| _], _) :-
		!,
		fail.
	decode_pointer_segment_codes([Code| Codes], [Code| DecodedCodes]) :-
		decode_pointer_segment_codes(Codes, DecodedCodes).

	pointer_codes([], []) :-
		!.
	pointer_codes(Pointer, Codes) :-
		Pointer = [_| _],
		!,
		pointer_tokens_codes(Pointer, Codes, []).
	pointer_codes(Pointer, _) :-
		type_error(list, Pointer).

	relative_pointer_codes(relative(Up, Shift, '#'), Codes) :-
		valid_non_negative_integer(Up),
		integer(Shift),
		!,
		number_codes(Up, UpCodes),
		relative_shift_codes(Shift, ShiftCodes),
		append(UpCodes, ShiftCodes, PrefixCodes),
		append(PrefixCodes, [0'#], Codes).
	relative_pointer_codes(relative(Up, Shift, Pointer), Codes) :-
		valid_non_negative_integer(Up),
		integer(Shift),
		proper_list(Pointer),
		!,
		number_codes(Up, UpCodes),
		relative_shift_codes(Shift, ShiftCodes),
		pointer_codes(Pointer, PointerCodes),
		append(UpCodes, ShiftCodes, PrefixCodes),
		append(PrefixCodes, PointerCodes, Codes).
	relative_pointer_codes(RelativePointer, _) :-
		domain_error(json_relative_pointer, RelativePointer).

	valid_non_negative_integer(Integer) :-
		integer(Integer),
		Integer >= 0.

	relative_shift_codes(0, []) :-
		!.
	relative_shift_codes(Shift, [0'+| Codes]) :-
		Shift > 0,
		!,
		number_codes(Shift, Codes).
	relative_shift_codes(Shift, [0'-| Codes]) :-
		Magnitude is -Shift,
		number_codes(Magnitude, Codes).

	pointer_tokens_codes([Token| Tokens], [0'/| Codes], Tail) :-
		text_codes(Token, TokenCodes),
		encode_pointer_segment_codes(TokenCodes, Codes, CodesTail),
		pointer_tokens_codes_tail(Tokens, CodesTail, Tail).

	pointer_tokens_codes_tail([], Codes, Codes) :-
		!.
	pointer_tokens_codes_tail([Token| Tokens], [0'/| Codes], Tail) :-
		text_codes(Token, TokenCodes),
		encode_pointer_segment_codes(TokenCodes, Codes, CodesTail),
		!,
		pointer_tokens_codes_tail(Tokens, CodesTail, Tail).
	pointer_tokens_codes_tail(Tokens, _, _) :-
		type_error(list, Tokens).

	encode_pointer_segment_codes([], Codes, Codes).
	encode_pointer_segment_codes([0'~| TokenCodes], [0'~, 0'0| Codes], Tail) :-
		!,
		encode_pointer_segment_codes(TokenCodes, Codes, Tail).
	encode_pointer_segment_codes([0'/| TokenCodes], [0'~, 0'1| Codes], Tail) :-
		!,
		encode_pointer_segment_codes(TokenCodes, Codes, Tail).
	encode_pointer_segment_codes([Code| TokenCodes], [Code| Codes], Tail) :-
		encode_pointer_segment_codes(TokenCodes, Codes, Tail).

	fragment_codes_pointer_codes([0'#| FragmentCodes], PointerCodes) :-
		!,
		fragment_codes_bytes(FragmentCodes, Bytes, []),
		bytes_to_codes(Bytes, PointerCodes).
	fragment_codes_pointer_codes(_, _) :-
		fail.

	fragment_codes_bytes([], Bytes, Bytes).
	fragment_codes_bytes([0'%, High, Low| Codes], [Byte| Bytes], Tail) :-
		!,
		hexadecimal_digit_value(High, HighValue),
		hexadecimal_digit_value(Low, LowValue),
		Byte is HighValue*16 + LowValue,
		fragment_codes_bytes(Codes, Bytes, Tail).
	fragment_codes_bytes([0'%| _], _, _) :-
		!,
		fail.
	fragment_codes_bytes([Code| Codes], Bytes, Tail) :-
		fragment_literal_code_bytes(Code, Bytes, NextBytes),
		fragment_codes_bytes(Codes, NextBytes, Tail).

	fragment_literal_code_bytes(Code, [Code| Bytes], Bytes) :-
		Code =< 0x7F,
		!.
	fragment_literal_code_bytes(Code, Bytes, Tail) :-
		codes_to_bytes([Code], EncodedBytes),
		append(EncodedBytes, Tail, Bytes).

	fragment_bytes_codes([], Codes, Codes).
	fragment_bytes_codes([Byte| Bytes], Codes, Tail) :-
		(	fragment_byte(Byte) ->
			Codes = [Byte| NextCodes]
		;	percent_encoded_byte(Byte, High, Low),
			Codes = [0'%, High, Low| NextCodes]
		),
		fragment_bytes_codes(Bytes, NextCodes, Tail).

	fragment_byte(Byte) :-
		Byte >= 0'a,
		Byte =< 0'z,
		!.
	fragment_byte(Byte) :-
		Byte >= 0'A,
		Byte =< 0'Z,
		!.
	fragment_byte(Byte) :-
		Byte >= 0'0,
		Byte =< 0'9,
		!.
	fragment_byte(0'-).
	fragment_byte(0'.).
	fragment_byte(0'_).
	fragment_byte(0'~).
	fragment_byte(0'!).
	fragment_byte(0'$).
	fragment_byte(0'&).
	fragment_byte(0'\').
	fragment_byte(0'().
	fragment_byte(0')).
	fragment_byte(0'*).
	fragment_byte(0'+).
	fragment_byte(0',).
	fragment_byte(0';).
	fragment_byte(0'=).
	fragment_byte(0':).
	fragment_byte(0'@).
	fragment_byte(0'/).
	fragment_byte(0'?).

	percent_encoded_byte(Byte, High, Low) :-
		Byte >= 0,
		Byte =< 255,
		HighValue is Byte // 16,
		LowValue is Byte mod 16,
		hexadecimal_digit_code(HighValue, High),
		hexadecimal_digit_code(LowValue, Low).

	hexadecimal_digit_code(Value, Code) :-
		Value >= 0,
		Value =< 9,
		!,
		Code is Value + 0'0.
	hexadecimal_digit_code(Value, Code) :-
		Code is Value - 10 + 0'A.

	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		!,
		Value is Code - 0'A + 10.
	hexadecimal_digit_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		Value is Code - 0'a + 10.

	evaluate_pointer([], JSON, Value) :-
		!,
		JSON = Value.
	evaluate_pointer([Token| Tokens], JSON, Value) :-
		!,
		evaluate_token(JSON, Token, Intermediate),
		evaluate_pointer(Tokens, Intermediate, Value).
	evaluate_pointer(Pointer, _, _) :-
		type_error(list, Pointer).

	relative_pointer_base(relative(Up, Shift, Suffix), Context, JSON, BasePointer, Suffix) :-
		valid_non_negative_integer(Up),
		integer(Shift),
		( 	Suffix == ('#') ->
			true
		; 	proper_list(Suffix)
		),
		!,
		ancestor_pointer(Context, Up, AncestorPointer),
		shift_pointer(JSON, AncestorPointer, Shift, BasePointer).
	relative_pointer_base(RelativePointer, _, _, _, _) :-
		domain_error(json_relative_pointer, RelativePointer).

	ancestor_pointer(Pointer, 0, Pointer) :-
		!.
	ancestor_pointer(Pointer, Up, Ancestor) :-
		Up > 0,
		split_last_token(Pointer, Parent, _),
		Up2 is Up - 1,
		ancestor_pointer(Parent, Up2, Ancestor).

	shift_pointer(_, Pointer, 0, Pointer) :-
		!.
	shift_pointer(JSON, Pointer, Shift, ShiftedPointer) :-
		split_last_token(Pointer, Prefix, Token),
		evaluate_pointer(Prefix, JSON, Array),
		proper_list(Array),
		token_index(Token, Index),
		ShiftedIndex is Index + Shift,
		ShiftedIndex >= 0,
		nth0(ShiftedIndex, Array, _),
		number_codes(ShiftedIndex, ShiftedCodes),
		codes_text(ShiftedCodes, ShiftedToken),
		append(Prefix, [ShiftedToken], ShiftedPointer).

	split_last_token([Token], [], Token) :-
		!.
	split_last_token([Head| Tail], [Head| Prefix], Token) :-
		split_last_token(Tail, Prefix, Token).

	evaluate_relative_hash([], _, _) :-
		!,
		fail.
	evaluate_relative_hash(Pointer, JSON, Value) :-
		split_last_token(Pointer, ParentPointer, Token),
		evaluate_pointer(ParentPointer, JSON, Parent),
		relative_hash_value(Parent, Token, Value).

	relative_hash_value(json(Pairs), Token, Token) :-
		!,
		unique_list_pair_value(Pairs, Token, _).
	relative_hash_value({}, _, _) :-
		!,
		fail.
	relative_hash_value({Pairs}, Token, Token) :-
		!,
		unique_curly_pair_value(Pairs, Token, _).
	relative_hash_value(Array, Token, Index) :-
		proper_list(Array),
		!,
		token_index(Token, Index),
		nth0(Index, Array, _).
	relative_hash_value(_, _, _) :-
		fail.

	evaluate_token(json(Pairs), Token, Value) :-
		!,
		unique_list_pair_value(Pairs, Token, Value).
	evaluate_token({}, _, _) :-
		!,
		fail.
	evaluate_token({Pairs}, Token, Value) :-
		!,
		unique_curly_pair_value(Pairs, Token, Value).
	evaluate_token(Array, Token, Value) :-
		proper_list(Array),
		!,
		token_index(Token, Index),
		nth0(Index, Array, Value).
	evaluate_token(_, _, _) :-
		fail.

	unique_curly_pair_value(Pairs, Token, Value) :-
		object_match_count(Pairs, Token, 0, Count, _, Value),
		Count =:= 1.

	object_match_count((Pair, Pairs), Token, Count0, Count, Value0, Value) :-
		!,
		pair_match_count(Pair, Token, Count0, Count1, Value0, Value1),
		object_match_count(Pairs, Token, Count1, Count, Value1, Value).
	object_match_count(Pair, Token, Count0, Count, Value0, Value) :-
		pair_match_count(Pair, Token, Count0, Count, Value0, Value).

	pair_match_count(Pair, Token, Count0, Count, Value0, Value) :-
		pair_key_value(Pair, Key, PairValue),
		(	Key == Token ->
			Count is Count0 + 1,
			(	Count0 =:= 0 ->
				Value = PairValue
			;	Value = Value0
			)
		;	Count = Count0,
			Value = Value0
		).

	unique_list_pair_value(Pairs, Token, Value) :-
		list_match_count(Pairs, Token, 0, Count, _, Value),
		Count =:= 1.

	list_match_count([], _, Count, Count, Value, Value).
	list_match_count([Pair| Pairs], Token, Count0, Count, Value0, Value) :-
		pair_match_count(Pair, Token, Count0, Count1, Value0, Value1),
		list_match_count(Pairs, Token, Count1, Count, Value1, Value).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

	token_index(Token, Index) :-
		text_codes(Token, Codes),
		array_index_codes(Codes, Index).

	array_index_codes([0'0], 0) :-
		!.
	array_index_codes([First| Rest], Index) :-
		First >= 0'1,
		First =< 0'9,
		decimal_digit_codes(Rest),
		number_codes(Index, [First| Rest]).

	decimal_digit_codes([]).
	decimal_digit_codes([Code| Codes]) :-
		Code >= 0'0,
		Code =< 0'9,
		decimal_digit_codes(Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	text_codes(Token, Codes) :-
		(	_StringRepresentation_ == atom ->
			atom(Token),
			atom_codes(Token, Codes)
		;	_StringRepresentation_ == chars ->
			Token = chars(Chars),
			chars_to_codes(Chars, Codes)
		;	_StringRepresentation_ == codes ->
			Token = codes(Codes)
		;	domain_error(json_pointer_token, Token)
		),
		!.
	text_codes(Token, _) :-
		domain_error(json_pointer_token, Token).

	codes_text(Codes, Token) :-
		(	_StringRepresentation_ == atom ->
			atom_codes(Token, Codes)
		;	_StringRepresentation_ == chars ->
			codes_to_chars(Codes, Chars),
			Token = chars(Chars)
		;	_StringRepresentation_ == codes ->
			Token = codes(Codes)
		;	domain_error(json_pointer_string_representation, _StringRepresentation_)
		).

:- end_object.


:- object(json_pointer,
	extends(json_pointer(atom))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-04,
		comment is 'JSON Pointer (RFC 6901) and Relative JSON Pointer parser, generator, and evaluator using atoms for text representation.'
	]).

:- end_object.
