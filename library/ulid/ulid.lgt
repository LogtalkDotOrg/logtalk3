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


:- object(ulid(_Representation_),
	implements(ulid_protocol)).

	:- info([
		version is 1:1:2,
		author is 'Paulo Moura',
		date is 2026-07-22,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator.',
		parameters is [
			'Representation' - 'Text representation for the ULID. Possible values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [ulid, ulid_types, cuid2(_,_,_), ids(_,_), ksuid(_,_), nanoid(_,_,_), snowflakeid(_,_,_,_,_,_,_), uuid(_)]
	]).

	:- uses(crypto, [
		random_bytes/2
	]).
	:- uses(date, [
		date_time_to_unix/2, unix_to_date_time/2
	]).

	generate(ULID) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), UnixTime),
		TotalMilliseconds is UnixTime * 1000 + Milliseconds,
		generate(TotalMilliseconds, ULID).

	generate(Milliseconds, ULID) :-
		context(Context),
		check_representation(Context),
		check_timestamp(Context, Milliseconds),
		random_bytes(10, Bytes),
		encode_base32(10, Milliseconds, Tail, Codes),
		bytes_to_base32(Bytes, Tail),
		codes_to_ulid(_Representation_, Codes, ULID).

	generate(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds, ULID) :-
		context(Context),
		check_millisecond_component(Context, Milliseconds),
		date_time_to_unix(date_time(Year, Month, Day, Hours, Minutes, Seconds), UnixTime),
		TotalMilliseconds is UnixTime * 1000 + Milliseconds,
		generate(TotalMilliseconds, ULID).

	timestamp(ULID, Milliseconds) :-
		context(Context),
		check_representation(Context),
		type::check(ulid(_Representation_), ULID),
		ulid_to_codes(_Representation_, ULID, Codes),
		decode_time(Codes, Milliseconds).

	timestamp(ULID, Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
		timestamp(ULID, TotalMilliseconds),
		TotalSeconds is TotalMilliseconds // 1000,
		unix_to_date_time(TotalSeconds, date_time(Year, Month, Day, Hours, Minutes, Seconds)),
		Milliseconds is TotalMilliseconds rem 1000.

	check_representation(Context) :-
		(	var(_Representation_) ->
			throw(error(instantiation_error, Context))
		;	_Representation_ == atom ->
			true
		;	_Representation_ == chars ->
			true
		;	_Representation_ == codes ->
			true
		;	throw(error(domain_error(ulid_representation, _Representation_), Context))
		).

	check_timestamp(Context, Milliseconds) :-
		(	var(Milliseconds) ->
			throw(error(instantiation_error, Context))
		;	integer(Milliseconds) ->
			MaxTimestamp is (1 << 48) - 1,
			(	Milliseconds >= 0, Milliseconds =< MaxTimestamp ->
				true
			;	throw(error(domain_error(ulid_timestamp, Milliseconds), Context))
			)
		;	throw(error(type_error(integer, Milliseconds), Context))
		).

	check_millisecond_component(Context, Milliseconds) :-
		(	var(Milliseconds) ->
			throw(error(instantiation_error, Context))
		;	integer(Milliseconds) ->
			(	Milliseconds >= 0, Milliseconds =< 999 ->
				true
			;	throw(error(domain_error(ulid_millisecond, Milliseconds), Context))
			)
		;	throw(error(type_error(integer, Milliseconds), Context))
		).

	% encodes a list of bytes (e.g. the 10 bytes of randomness) directly into
	% Crockford's Base32 codes using a small bit buffer instead of converting
	% the whole byte list into a single (potentially very large) integer first;
	% this avoids int_overflow errors on backends with bounded integer
	% arithmetic as the buffer never holds more than 12 bits
	bytes_to_base32(Bytes, Codes) :-
		bytes_to_base32(Bytes, 0, 0, Codes).

	bytes_to_base32([], 0, 0, []).
	bytes_to_base32([Byte| Bytes], Buffer0, BufferBits0, Codes) :-
		Buffer1 is (Buffer0 << 8) \/ Byte,
		BufferBits1 is BufferBits0 + 8,
		bytes_to_base32_drain(Buffer1, BufferBits1, Bytes, Codes).

	bytes_to_base32_drain(Buffer, BufferBits, Bytes, Codes) :-
		(	BufferBits >= 5 ->
			Shift is BufferBits - 5,
			Index is Buffer >> Shift,
			code(Index, Code),
			Mask is (1 << Shift) - 1,
			Buffer1 is Buffer /\ Mask,
			BufferBits1 is Shift,
			Codes = [Code| Codes1],
			bytes_to_base32_drain(Buffer1, BufferBits1, Bytes, Codes1)
		;	bytes_to_base32(Bytes, Buffer, BufferBits, Codes)
		).

	encode_base32(0, _, Tail, Tail) :-
		!.
	encode_base32(N, Time0, Tail, Codes) :-
		Index is Time0 rem 32,
		code(Index, Code),
		Time1 is Time0 div 32,
		M is N - 1,
		encode_base32(M, Time1, [Code| Tail], Codes).

	decode_time([Code1, Code2, Code3, Code4, Code5, Code6, Code7, Code8, Code9, Code10| _], Milliseconds) :-
		code(Index1, Code1),
		code(Index2, Code2),
		code(Index3, Code3),
		code(Index4, Code4),
		code(Index5, Code5),
		code(Index6, Code6),
		code(Index7, Code7),
		code(Index8, Code8),
		code(Index9, Code9),
		code(Index10, Code10), !,
		Milliseconds is Index1*32^9 + Index2*32^8 + Index3*32^7 + Index4*32^6 + Index5*32^5 + Index6*32^4 + Index7*32^3 + Index8*32^2 + Index9*32 + Index10.

	codes_to_ulid(atom, Codes, ULID) :-
		atom_codes(ULID, Codes).
	codes_to_ulid(chars, Codes, ULID) :-
		codes_to_chars(Codes, ULID).
	codes_to_ulid(codes, ULID, ULID).

	ulid_to_codes(atom, ULID, Codes) :-
		atom_codes(ULID, Codes).
	ulid_to_codes(chars, ULID, Codes) :-
		chars_to_codes(ULID, Codes).
	ulid_to_codes(codes, Codes, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	% Crockford's Base32 encoding
	code( 0, 0'0).
	code( 1, 0'1).
	code( 2, 0'2).
	code( 3, 0'3).
	code( 4, 0'4).
	code( 5, 0'5).
	code( 6, 0'6).
	code( 7, 0'7).
	code( 8, 0'8).
	code( 9, 0'9).
	code(10, 0'A).
	code(11, 0'B).
	code(12, 0'C).
	code(13, 0'D).
	code(14, 0'E).
	code(15, 0'F).
	code(16, 0'G).
	code(17, 0'H).
	code(18, 0'J).
	code(19, 0'K).
	code(20, 0'M).
	code(21, 0'N).
	code(22, 0'P).
	code(23, 0'Q).
	code(24, 0'R).
	code(25, 0'S).
	code(26, 0'T).
	code(27, 0'V).
	code(28, 0'W).
	code(29, 0'X).
	code(30, 0'Y).
	code(31, 0'Z).

:- end_object.


:- object(ulid,
	extends(ulid(atom))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator using an atom representation.',
		see_also is [ulid(_), ulid_types, cuid2, ids, ksuid, nanoid, snowflakeid, uuid]
	]).

:- end_object.
