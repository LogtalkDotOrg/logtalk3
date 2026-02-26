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


:- object(nanoid(_Representation_, _Size_, _Alphabet_),
	implements(nanoid_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'NanoID generator.',
		parameters is [
			'Representation' - 'Text representation for the NanoID. Possible values are ``atom``, ``chars``, and ``codes``.',
			'Size' - 'Number of symbols in the NanoID.',
			'Alphabet' - 'Alphabet used for generating NanoIDs represented as an atom, list of characters, or list of character codes.'
		],
		see_also is [nanoid, ids(_, _), ulid(_), uuid(_)]
	]).

	:- uses(fast_random(xoshiro128pp), [
		randomize/1, sequence/4
	]).

	:- uses(list, [
		length/2, member/2, nth0/3, reverse/2
	]).

	:- uses(os, [
		wall_time/1
	]).

	generate(NanoID) :-
		context(Context),
		check_representation(Context),
		check_size(Context),
		alphabet_codes(_Alphabet_, AlphabetCodes),
		check_alphabet(Context, AlphabetCodes),
		generate_codes(_Size_, AlphabetCodes, Codes),
		codes_to_nanoid(_Representation_, Codes, NanoID).

	check_representation(Context) :-
		(   var(_Representation_) ->
			throw(error(instantiation_error, Context))
		;   _Representation_ == atom ->
			true
		;   _Representation_ == chars ->
			true
		;   _Representation_ == codes ->
			true
		;   throw(error(domain_error(nanoid_representation, _Representation_), Context))
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

	alphabet_codes(Alphabet, Codes) :-
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
			throw(error(domain_error(nanoid_alphabet, _Alphabet_), Context))
		;   Codes = [_] ->
			throw(error(domain_error(nanoid_alphabet, _Alphabet_), Context))
		;   valid_code_list(Codes) ->
			check_alphabet_unique(Context, Codes)
		;   throw(error(domain_error(nanoid_alphabet, _Alphabet_), Context))
		).

	valid_code_list([]).
	valid_code_list([Code| Codes]) :-
		integer(Code),
		Code >= 0,
		Code =< 0x10ffff,
		valid_code_list(Codes).

	check_alphabet_unique(Context, Codes) :-
		(   repeated_code(Codes) ->
			throw(error(domain_error(nanoid_alphabet, _Alphabet_), Context))
		;   true
		).

	repeated_code([Code| Codes]) :-
		member(Code, Codes),
		!.
	repeated_code([_| Codes]) :-
		repeated_code(Codes).

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

	codes_to_nanoid(atom, Codes, NanoID) :-
		atom_codes(NanoID, Codes).
	codes_to_nanoid(chars, Codes, NanoID) :-
		codes_to_chars(Codes, NanoID).
	codes_to_nanoid(codes, NanoID, NanoID).

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


:- object(nanoid,
	extends(nanoid(atom, 21, '_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'NanoID generator using atom representation, 21 symbols, and the standard URL alphabet.',
		see_also is [nanoid(_, _, _), ids, ulid, uuid]
	]).

:- end_object.
