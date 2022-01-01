%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(uuid(_Representation_),
	implements(uuid_protocol)).

	:- info([
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2021-03-13,
		comment is 'Universally unique identifier (UUID) generator.',
		parameters is [
			'Representation' - 'Text representation for the UUID. Possible values are ``atom``, ``chars``, and ``codes``.'
		]
	]).

	uuid_v1([Byte11, Byte12, Byte13, Byte14, Byte15, Byte16], UUID) :-
		iso8601::date(Start, 1582, 10, 15),
		iso8601::date(Current, _, _, _),
		SecondsBetweenEpocs is (Current - Start) * 86400,
		os::date_time(_, _, _, Hours, Minutes, Seconds, Milliseconds),
		HundredsOfNanoseconds is (SecondsBetweenEpocs + Hours*3600 + Minutes*60 + Seconds + Milliseconds//1000) * 10000000,
		TimeLow is HundredsOfNanoseconds /\ 0xffffffff,
		TimeMid is (HundredsOfNanoseconds >> 32) /\ 0xffff,
		TimeHiAndVersion is ((HundredsOfNanoseconds >> 48) /\ 0x0fff) \/ 0b0001000000000000,
		clock_seq(ClockSeq),
		ClockSeqLow is ClockSeq /\ 0xff,
		ClockSeqHiVariant is (((ClockSeq >> 8) /\ 0x3f) /\ 0b10111111) \/ 0b10000000,
		Byte1 is TimeLow >> 24,
		Byte2 is (TimeLow /\ 0x00ffffff) >> 16,
		Byte3 is (TimeLow /\ 0x0000ffff) >> 8,
		Byte4 is TimeLow /\ 0x000000ff,
		Byte5 is TimeMid >> 8,
		Byte6 is TimeMid /\ 0x00ff,
		Byte7 is TimeHiAndVersion >> 8,
		Byte8 is TimeHiAndVersion /\ 0x00ff,
		phrase(
			bytes_to_uuid(
				[Byte1, Byte2, Byte3, Byte4],
				[Byte5, Byte6],
				[Byte7, Byte8],
				[ClockSeqHiVariant, ClockSeqLow],
				[Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]
			),
			Codes
		),
		codes_to_uuid(_Representation_, Codes, UUID).

	clock_seq(ClockSeq) :-
		% get 14 random bits
		random_bytes(2, [Byte1, Byte2]),
		ClockSeq is (Byte1 << 8 + Byte2) /\ 0b0011111111111111.

	uuid_v4(UUID) :-
		random_bytes(16, [Byte1, Byte2, Byte3, Byte4, Byte5, Byte6, Byte7, Byte8, Byte9, Byte10, Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]),
		TimeHiAndVersion is (Byte7 \/ 0b01000000) /\ 0b01001111,
		ClockSeqHi is (Byte9 \/ 0b10000000) /\ 0b10111111,
		phrase(
			bytes_to_uuid(
				[Byte1, Byte2, Byte3, Byte4],
				[Byte5, Byte6],
				[TimeHiAndVersion, Byte8],
				[ClockSeqHi, Byte10],
				[Byte11, Byte12, Byte13, Byte14, Byte15, Byte16]
			),
			Codes
		),
		codes_to_uuid(_Representation_, Codes, UUID).

	bytes_to_uuid(Sequence1, Sequence2, Sequence3, Sequence4, Sequence5) -->
		bytes_to_hexdecimal_codes(Sequence1),
		[0'-],
		bytes_to_hexdecimal_codes(Sequence2),
		[0'-],
		bytes_to_hexdecimal_codes(Sequence3),
		[0'-],
		bytes_to_hexdecimal_codes(Sequence4),
		[0'-],
		bytes_to_hexdecimal_codes(Sequence5).

	codes_to_uuid(atom, Codes, UUID) :-
		atom_codes(UUID, Codes).
	codes_to_uuid(chars, Codes, UUID) :-
		codes_to_chars(Codes, UUID).
	codes_to_uuid(codes, UUID, UUID).

	uuid_null(UUID) :-
		uuid_null(_Representation_, UUID).

	uuid_null(atom,  '00000000-0000-0000-0000-000000000000').
	uuid_null(chars, ['0','0','0','0','0','0','0','0',-,'0','0','0','0',-,'0','0','0','0',-,'0','0','0','0',-,'0','0','0','0','0','0','0','0','0','0','0','0']).
	uuid_null(codes, [48,48,48,48,48,48,48,48,45,48,48,48,48,45,48,48,48,48,45,48,48,48,48,45,48,48,48,48,48,48,48,48,48,48,48,48]).

	random_node(Node) :-
		random_bytes(6, Node).

	% auxiliary predicates

	bytes_to_hexdecimal_codes([]) -->
		[].
	bytes_to_hexdecimal_codes([Byte| Bytes]) -->
		byte_to_hexdecimal_codes(Byte),
		bytes_to_hexdecimal_codes(Bytes).

	byte_to_hexdecimal_codes(Byte) -->
		{Code1 is Byte div 16, Code2 is Byte rem 16},
		decimal_hexdecimal_code(Code1),
		decimal_hexdecimal_code(Code2).

	decimal_hexdecimal_code( 0) --> [0'0].
	decimal_hexdecimal_code( 1) --> [0'1].
	decimal_hexdecimal_code( 2) --> [0'2].
	decimal_hexdecimal_code( 3) --> [0'3].
	decimal_hexdecimal_code( 4) --> [0'4].
	decimal_hexdecimal_code( 5) --> [0'5].
	decimal_hexdecimal_code( 6) --> [0'6].
	decimal_hexdecimal_code( 7) --> [0'7].
	decimal_hexdecimal_code( 8) --> [0'8].
	decimal_hexdecimal_code( 9) --> [0'9].
	decimal_hexdecimal_code(10) --> [0'a].
	decimal_hexdecimal_code(11) --> [0'b].
	decimal_hexdecimal_code(12) --> [0'c].
	decimal_hexdecimal_code(13) --> [0'd].
	decimal_hexdecimal_code(14) --> [0'e].
	decimal_hexdecimal_code(15) --> [0'f].

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

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.


:- object(uuid,
	extends(uuid(atom))).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-03-11,
		comment is 'Universally unique identifier (UUID) generator using an atom representation.'
	]).

:- end_object.
