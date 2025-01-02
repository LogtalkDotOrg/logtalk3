%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-05-19,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator.',
		parameters is [
			'Representation' - 'Text representation for the ULID. Possible values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [ulid, ulid_types, uuid(_), uuid, ids, ids(_, _)]
	]).

	generate(ULID) :-
		iso8601::date(Start, 1970, 1, 1),
		iso8601::date(Current, _, _, _),
		SecondsBetweenEpochs is (Current - Start) * 86400,
		os::date_time(_, _, _, Hours, Minutes, Seconds, Milliseconds),
		TotalMilliseconds is (SecondsBetweenEpochs + Hours*3600 + Minutes*60 + Seconds) * 1000 + Milliseconds,
		generate(TotalMilliseconds, ULID).

	generate(Milliseconds, ULID) :-
		random_bytes(16, Bytes),
		encode_time(10, Milliseconds, Tail, Codes),
		encode_random(Bytes, Tail),
		codes_to_ulid(_Representation_, Codes, ULID).

	generate(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds, ULID) :-
		iso8601::date(Start, 1970, 1, 1),
		iso8601::date(Current, Year, Month, Day),
		SecondsBetweenEpochs is (Current - Start) * 86400,
		TotalMilliseconds is (SecondsBetweenEpochs + Hours*3600 + Minutes*60 + Seconds) * 1000 + Milliseconds,
		generate(TotalMilliseconds, ULID).

	timestamp(ULID, Milliseconds) :-
		ulid_to_codes(_Representation_, ULID, Codes),
		decode_time(Codes, Milliseconds).

	timestamp(ULID, Year, Month, Day, Hours, Minutes, Seconds, Milliseconds) :-
		timestamp(ULID, TotalMilliseconds),
		TotalSeconds is TotalMilliseconds // 1000,
		iso8601::date(Start, 1970, 1, 1),
		Current is Start + TotalSeconds // 86400,
		iso8601::date(Current, Year, Month, Day),
		RemainingSeconds is TotalSeconds - (Current - Start) * 86400,
		Hours is RemainingSeconds // 3600,
		Minutes is (RemainingSeconds - Hours * 3600) // 60,
		Seconds is RemainingSeconds - Hours * 3600 - Minutes * 60,
		Milliseconds is TotalMilliseconds rem 1000.

	encode_time(0, _, Tail, Tail) :-
		!.
	encode_time(N, Time0, Tail, Codes) :-
		Index is Time0 rem 32,
		code(Index, Code),
		Time1 is Time0 div 32,
		M is N - 1,
		encode_time(M, Time1, [Code| Tail], Codes).

	encode_random([], []).
	encode_random([Byte| Bytes], [Code| Codes]) :-
		Index is floor((Byte / 255) * 31),
		code(Index, Code),
		encode_random(Bytes, Codes).

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

	random_bytes(N, Bytes) :-
		catch(open('/dev/urandom', read, Stream, [type(binary)]), _, fail),
		list::length(Bytes, N),
		read_random_bytes(Bytes, Stream),
		close(Stream),
		!.
	random_bytes(N, Bytes) :-
		os::wall_time(Time),
		Seed is round(Time),
		fast_random::randomize(Seed),
		fast_random::sequence(N, 0, 255, Bytes).

	read_random_bytes([], _).
	read_random_bytes([Byte| Bytes], Stream) :-
		get_byte(Stream, Byte),
		read_random_bytes(Bytes, Stream).

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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-05-19,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator using an atom representation.',
		see_also is [ulid(_), ulid_types, uuid, uuid(_), ids, ids(_, _)]
	]).

:- end_object.
