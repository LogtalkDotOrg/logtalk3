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


:- object(uuid(_Representation_),
	implements(uuid_protocol)).

	:- info([
		version is 0:8:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Universally unique identifier (UUID) generator.',
		parameters is [
			'Representation' - 'Text representation for the UUID. Possible values are ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [uuid, cuid2(_,_,_), ksuid(_,_), ids(_,_), nanoid(_,_,_), snowflakeid(_,_,_,_,_,_,_), ulid(_)]
	]).

	uuid_v1([Byte11, Byte12, Byte13, Byte14, Byte15, Byte16], UUID) :-
		iso8601::date(Start, 1582, 10, 15),
		iso8601::date(Current, _, _, _),
		SecondsBetweenEpochs is (Current - Start) * 86400,
		os::date_time(_, _, _, Hours, Minutes, Seconds, Milliseconds),
		HundredsOfNanoseconds is (SecondsBetweenEpochs + Hours*3600 + Minutes*60 + Seconds + Milliseconds//1000) * 10000000,
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

	uuid_v7(UUID) :-
		% get Unix timestamp in milliseconds
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds),
		% calculate days since Unix epoch (1970-01-01)
		iso8601::date(UnixEpoch, 1970, 1, 1),
		iso8601::date(Current, Year, Month, Day),
		DaysSinceEpoch is Current - UnixEpoch,
		% calculate total milliseconds since Unix epoch
		UnixTimestampMs is DaysSinceEpoch * 86400000 + Hours * 3600000 + Minutes * 60000 + Seconds * 1000 + Milliseconds,
		% extract 6 bytes from the 48-bit timestamp (big-endian)
		Byte1 is (UnixTimestampMs >> 40) /\ 0xff,
		Byte2 is (UnixTimestampMs >> 32) /\ 0xff,
		Byte3 is (UnixTimestampMs >> 24) /\ 0xff,
		Byte4 is (UnixTimestampMs >> 16) /\ 0xff,
		Byte5 is (UnixTimestampMs >> 8) /\ 0xff,
		Byte6 is UnixTimestampMs /\ 0xff,
		% get random bytes for rand_a (12 bits in Byte7-Byte8) and rand_b (62 bits in Byte9-Byte16)
		random_bytes(10, [RandByte7, RandByte8, RandByte9, RandByte10, RandByte11, RandByte12, RandByte13, RandByte14, RandByte15, RandByte16]),
		% set version 7 in the high nibble of Byte7: version = 0b0111 (7)
		Byte7 is 0b01110000 \/ (RandByte7 /\ 0b00001111),
		% Byte8 is all rand_a
		Byte8 is RandByte8,
		% set variant in the high 2 bits of Byte9: variant = 0b10
		Byte9 is 0b10000000 \/ (RandByte9 /\ 0b00111111),
		phrase(
			bytes_to_uuid(
				[Byte1, Byte2, Byte3, Byte4],
				[Byte5, Byte6],
				[Byte7, Byte8],
				[Byte9, RandByte10],
				[RandByte11, RandByte12, RandByte13, RandByte14, RandByte15, RandByte16]
			),
			Codes
		),
		codes_to_uuid(_Representation_, Codes, UUID).

	bytes_to_uuid(Sequence1, Sequence2, Sequence3, Sequence4, Sequence5) -->
		bytes_to_hexadecimal_codes(Sequence1),
		[0'-],
		bytes_to_hexadecimal_codes(Sequence2),
		[0'-],
		bytes_to_hexadecimal_codes(Sequence3),
		[0'-],
		bytes_to_hexadecimal_codes(Sequence4),
		[0'-],
		bytes_to_hexadecimal_codes(Sequence5).

	codes_to_uuid(atom, Codes, UUID) :-
		atom_codes(UUID, Codes).
	codes_to_uuid(chars, Codes, UUID) :-
		codes_to_chars(Codes, UUID).
	codes_to_uuid(codes, UUID, UUID).

	uuid_null(UUID) :-
		uuid_nil(_Representation_, UUID).

	uuid_nil(UUID) :-
		uuid_nil(_Representation_, UUID).

	uuid_nil(atom,  '00000000-0000-0000-0000-000000000000').
	uuid_nil(chars, ['0','0','0','0','0','0','0','0',-,'0','0','0','0',-,'0','0','0','0',-,'0','0','0','0',-,'0','0','0','0','0','0','0','0','0','0','0','0']).
	uuid_nil(codes, [48,48,48,48,48,48,48,48,45,48,48,48,48,45,48,48,48,48,45,48,48,48,48,45,48,48,48,48,48,48,48,48,48,48,48,48]).

	uuid_max(UUID) :-
		uuid_max(_Representation_, UUID).

	uuid_max(atom,  'FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF').
	uuid_max(chars, ['F','F','F','F','F','F','F','F',-,'F','F','F','F',-,'F','F','F','F',-,'F','F','F','F',-,'F','F','F','F','F','F','F','F','F','F','F','F']).
	uuid_max(codes, [70,70,70,70,70,70,70,70,45,70,70,70,70,45,70,70,70,70,45,70,70,70,70,45,70,70,70,70,70,70,70,70,70,70,70,70]).

	random_node(Node) :-
		random_bytes(6, Node).

	% auxiliary predicates

	bytes_to_hexadecimal_codes([]) -->
		[].
	bytes_to_hexadecimal_codes([Byte| Bytes]) -->
		byte_to_hexadecimal_codes(Byte),
		bytes_to_hexadecimal_codes(Bytes).

	byte_to_hexadecimal_codes(Byte) -->
		{Code1 is Byte div 16, Code2 is Byte rem 16},
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Universally unique identifier (UUID) generator using an atom representation.',
		see_also is [uuid(_), cuid2, ksuid, ids, nanoid, snowflakeid, ulid]
	]).

:- end_object.
