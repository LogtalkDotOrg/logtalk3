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


:- object(ksuid(_Representation_, _Alphabet_),
	implements(ksuid_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'KSUID generator.',
		parameters is [
			'Representation' - 'Text representation for the KSUID. Possible values are ``atom``, ``chars``, and ``codes``.',
			'Alphabet' - 'Base62 alphabet used for encoding KSUIDs represented as an atom, list of characters, or list of character codes.'
		],
		see_also is [ksuid, cuid2(_, _, _), nanoid(_, _, _), ids(_, _), ulid(_), uuid(_)]
	]).

	:- uses(fast_random(xoshiro128pp), [
		randomize/1, sequence/4
	]).

	:- uses(iso8601, [
		date/4
	]).

	:- uses(list, [
		append/3, length/2, member/2, nth0/3
	]).

	:- uses(os, [
		date_time/7, wall_time/1
	]).

	generate(KSUID) :-
		context(Context),
		check_representation(Context),
		alphabet_codes(Context, _Alphabet_, AlphabetCodes),
		check_alphabet(Context, AlphabetCodes),
		timestamp_bytes(Context, TimestampBytes),
		random_bytes(16, RandomBytes),
		append(TimestampBytes, RandomBytes, Bytes),
		bytes_integer(Bytes, 0, Integer),
		encode_base62(Context, Integer, AlphabetCodes, 27, Codes),
		codes_to_ksuid(_Representation_, Codes, KSUID).

	check_representation(Context) :-
		(   var(_Representation_) ->
			throw(error(instantiation_error, Context))
		;   _Representation_ == atom ->
			true
		;   _Representation_ == chars ->
			true
		;   _Representation_ == codes ->
			true
		;   throw(error(domain_error(ksuid_representation, _Representation_), Context))
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
		(   valid_code_list(Codes),
			length(Codes, 62),
			\+ repeated_code(Codes) ->
			true
		;   throw(error(domain_error(ksuid_alphabet, _Alphabet_), Context))
		).

	valid_code_list([]).
	valid_code_list([Code| Codes]) :-
		integer(Code),
		Code >= 0,
		Code =< 0x10ffff,
		valid_code_list(Codes).

	repeated_code([Code| Codes]) :-
		member(Code, Codes),
		!.
	repeated_code([_| Codes]) :-
		repeated_code(Codes).

	timestamp_bytes(Context, [Byte1, Byte2, Byte3, Byte4]) :-
		date(UnixEpoch, 1970, 1, 1),
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		date(Current, Year, Month, Day),
		UnixSeconds is (Current - UnixEpoch) * 86400 + Hours * 3600 + Minutes * 60 + Seconds,
		Timestamp is UnixSeconds - 1400000000,
		(   Timestamp >= 0 ->
			Byte1 is (Timestamp >> 24) /\ 0xff,
			Byte2 is (Timestamp >> 16) /\ 0xff,
			Byte3 is (Timestamp >> 8) /\ 0xff,
			Byte4 is Timestamp /\ 0xff
		;   throw(error(domain_error(ksuid_timestamp, Timestamp), Context))
		).

	bytes_integer([], Integer, Integer).
	bytes_integer([Byte| Bytes], Integer0, Integer) :-
		Integer1 is Integer0 * 256 + Byte,
		bytes_integer(Bytes, Integer1, Integer).

	encode_base62(Context, Integer, AlphabetCodes, Size, Codes) :-
		tobase62(Integer, AlphabetCodes, [], Digits),
		length(Digits, DigitsLength),
		(   DigitsLength =< Size ->
			nth0(0, AlphabetCodes, PadCode),
			PaddingLength is Size - DigitsLength,
			padding(PaddingLength, PadCode, PaddingCodes),
			append(PaddingCodes, Digits, Codes)
		;   throw(error(domain_error(ksuid_value, Integer), Context))
		).

	tobase62(0, _, [], [0'0]) :-
		!.
	tobase62(0, _, Digits, Digits) :-
		!.
	tobase62(Integer, AlphabetCodes, Digits0, Digits) :-
		Quotient is Integer // 62,
		Remainder is Integer mod 62,
		nth0(Remainder, AlphabetCodes, Digit),
		tobase62(Quotient, AlphabetCodes, [Digit| Digits0], Digits).

	padding(0, _, []) :-
		!.
	padding(N, Code, [Code| Codes]) :-
		N > 0,
		M is N - 1,
		padding(M, Code, Codes).

	codes_to_ksuid(atom, Codes, KSUID) :-
		atom_codes(KSUID, Codes).
	codes_to_ksuid(chars, Codes, KSUID) :-
		codes_to_chars(Codes, KSUID).
	codes_to_ksuid(codes, KSUID, KSUID).

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


:- object(ksuid,
	extends(ksuid(atom, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'KSUID generator using atom representation and the canonical Base62 alphabet.',
		see_also is [ksuid(_, _), cuid2, nanoid, ids, ulid, uuid]
	]).

:- end_object.
