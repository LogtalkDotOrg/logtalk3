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


:- object(cuid2(_Representation_, _Size_, _Alphabet_),
	implements(cuid2_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Cuid2 generator.',
		parameters is [
			'Representation' - 'Text representation for the Cuid2 identifier. Possible values are ``atom``, ``chars``, and ``codes``.',
			'Size' - 'Number of symbols in the Cuid2 identifier.',
			'Alphabet' - 'Alphabet used for generating Cuid2 identifiers represented as an atom, list of characters, or list of character codes.'
		],
		see_also is [cuid2, ids(_,_), ksuid(_,_), nanoid(_,_,_), snowflakeid(_,_,_,_,_,_,_), ulid(_), uuid(_)]
	]).

	:- uses(fast_random(xoshiro128pp), [
		randomize/1, sequence/4
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth0/3, reverse/2
	]).

	:- uses(os, [
		wall_time/1
	]).

	generate(Cuid2) :-
		context(Context),
		check_representation(Context),
		check_size(Context),
		alphabet_codes(Context, _Alphabet_, AlphabetCodes),
		check_alphabet(Context, AlphabetCodes),
		alphabet_letter_codes(AlphabetCodes, LetterCodes),
		check_letter_codes(Context, LetterCodes),
		generate_codes(1, LetterCodes, FirstCodes),
		_Size_ > 1,
		Remaining is _Size_ - 1,
		generate_codes(Remaining, AlphabetCodes, RemainingCodes),
		append(FirstCodes, RemainingCodes, Codes),
		codes_to_cuid2(_Representation_, Codes, Cuid2).
	generate(Cuid2) :-
		context(Context),
		check_representation(Context),
		check_size(Context),
		alphabet_codes(Context, _Alphabet_, AlphabetCodes),
		check_alphabet(Context, AlphabetCodes),
		alphabet_letter_codes(AlphabetCodes, LetterCodes),
		check_letter_codes(Context, LetterCodes),
		generate_codes(1, LetterCodes, Codes),
		codes_to_cuid2(_Representation_, Codes, Cuid2).

	check_representation(Context) :-
		(   var(_Representation_) ->
			throw(error(instantiation_error, Context))
		;   _Representation_ == atom ->
			true
		;   _Representation_ == chars ->
			true
		;   _Representation_ == codes ->
			true
		;   throw(error(domain_error(cuid2_representation, _Representation_), Context))
		).

	check_size(Context) :-
		(   var(_Size_) ->
			throw(error(instantiation_error, Context))
		;   integer(_Size_) ->
			(   _Size_ > 0 ->
				true
			;   throw(error(domain_error(not_less_than_one, _Size_), Context))
			)
		;   throw(error(type_error(integer, _Size_), Context))
		).

	alphabet_codes(Context, Alphabet, Codes) :-
		(   var(Alphabet) ->
			throw(error(instantiation_error, Context))
		;   atom(Alphabet) ->
			atom_codes(Alphabet, Codes)
		;   chars_alphabet_codes(Alphabet, Codes) ->
			true
		;   codes_alphabet_codes(Alphabet, Codes) ->
			true
		;   throw(error(type_error(text, Alphabet), Context))
		).

	chars_alphabet_codes([], []).
	chars_alphabet_codes([Symbol| Symbols], [Code| Codes]) :-
		atom(Symbol),
		atom_length(Symbol, 1),
		char_code(Symbol, Code),
		chars_alphabet_codes(Symbols, Codes).

	codes_alphabet_codes([], []).
	codes_alphabet_codes([Code| Codes], [Code| ConvertedCodes]) :-
		integer(Code),
		codes_alphabet_codes(Codes, ConvertedCodes).

	check_alphabet(Context, Codes) :-
		(   Codes == [] ->
			throw(error(domain_error(cuid2_alphabet, _Alphabet_), Context))
		;   Codes = [_] ->
			throw(error(domain_error(cuid2_alphabet, _Alphabet_), Context))
		;   valid_code_list(Codes) ->
			check_alphabet_unique(Context, Codes)
		;   throw(error(domain_error(cuid2_alphabet, _Alphabet_), Context))
		).

	valid_code_list([]).
	valid_code_list([Code| Codes]) :-
		integer(Code),
		Code >= 0,
		Code =< 0x10ffff,
		valid_code_list(Codes).

	check_alphabet_unique(Context, Codes) :-
		(   repeated_code(Codes) ->
			throw(error(domain_error(cuid2_alphabet, _Alphabet_), Context))
		;   true
		).

	repeated_code([Code| Codes]) :-
		member(Code, Codes),
		!.
	repeated_code([_| Codes]) :-
		repeated_code(Codes).

	alphabet_letter_codes([], []).
	alphabet_letter_codes([Code| Codes], [Code| Letters]) :-
		ascii_letter_code(Code),
		!,
		alphabet_letter_codes(Codes, Letters).
	alphabet_letter_codes([_| Codes], Letters) :-
		alphabet_letter_codes(Codes, Letters).

	ascii_letter_code(Code) :-
		Code >= 0'a,
		Code =< 0'z.
	ascii_letter_code(Code) :-
		Code >= 0'A,
		Code =< 0'Z.

	check_letter_codes(Context, LetterCodes) :-
		(   LetterCodes == [] ->
			throw(error(domain_error(cuid2_alphabet, _Alphabet_), Context))
		;   true
		).

	generate_codes(Size, AlphabetCodes, Codes) :-
		length(AlphabetCodes, AlphabetLength),
		mask(AlphabetLength, Mask),
		step(Size, Mask, AlphabetLength, Step),
		generate_codes(Size, AlphabetCodes, AlphabetLength, Mask, Step, [], ReverseCodes),
		reverse(ReverseCodes, Codes).

	mask(AlphabetLength, Mask) :-
		bits(AlphabetLength, 0, Bits),
		Mask is (1 << Bits) - 1.

	bits(AlphabetLength, Bits0, Bits) :-
		Power is 1 << Bits0,
		(   Power >= AlphabetLength ->
			Bits = Bits0
		;   Bits1 is Bits0 + 1,
			bits(AlphabetLength, Bits1, Bits)
		).

	step(Size, Mask, AlphabetLength, Step) :-
		Step0 is (8*Mask*Size + 5*AlphabetLength - 1) // (5*AlphabetLength),
		(   Step0 >= Size ->
			Step = Step0
		;   Step = Size
		).

	generate_codes(0, _, _, _, _, Codes, Codes) :-
		!.
	generate_codes(Remaining, AlphabetCodes, AlphabetLength, Mask, Step, Acc, Codes) :-
		random_bytes(Step, Bytes),
		consume_bytes(Bytes, Remaining, AlphabetCodes, AlphabetLength, Mask, Acc, NewRemaining, NewAcc),
		generate_codes(NewRemaining, AlphabetCodes, AlphabetLength, Mask, Step, NewAcc, Codes).

	consume_bytes([], Remaining, _, _, _, Acc, Remaining, Acc).
	consume_bytes(_, 0, _, _, _, Acc, 0, Acc) :-
		!.
	consume_bytes([Byte| Bytes], Remaining0, AlphabetCodes, AlphabetLength, Mask, Acc0, Remaining, Acc) :-
		Index is Byte /\ Mask,
		(   Index < AlphabetLength ->
			nth0(Index, AlphabetCodes, Code),
			Remaining1 is Remaining0 - 1,
			Acc1 = [Code| Acc0]
		;   Remaining1 = Remaining0,
			Acc1 = Acc0
		),
		consume_bytes(Bytes, Remaining1, AlphabetCodes, AlphabetLength, Mask, Acc1, Remaining, Acc).

	codes_to_cuid2(atom, Codes, Cuid2) :-
		atom_codes(Cuid2, Codes).
	codes_to_cuid2(chars, Codes, Cuid2) :-
		codes_to_chars(Codes, Cuid2).
	codes_to_cuid2(codes, Cuid2, Cuid2).

	random_bytes(N, Bytes) :-
		catch(open('/dev/urandom', read, Stream, [type(binary)]), _, fail),
		length(Bytes, N),
		read_random_bytes(Bytes, Stream),
		close(Stream),
		!.
	random_bytes(N, Bytes) :-
		wall_time(Time),
		Seed is round(Time),
		randomize(Seed),
		sequence(N, 0, 255, Bytes).

	read_random_bytes([], _).
	read_random_bytes([Byte| Bytes], Stream) :-
		get_byte(Stream, Byte),
		read_random_bytes(Bytes, Stream).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.


:- object(cuid2,
	extends(cuid2(atom, 24, 'abcdefghijklmnopqrstuvwxyz0123456789'))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Cuid2 generator using atom representation, 24 symbols, and a lowercase alphanumeric alphabet.',
		see_also is [cuid2(_, _, _), ids, ksuid, nanoid, snowflakeid, ulid, uuid]
	]).

:- end_object.
