%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-05-17,
		comment is 'Unit tests for the "ulid" library.'
	]).

	cover(ulid).
	cover(ulid(_)).

	test(ulid_generate_1_atom, all(atom(ULID))) :-
		integer::between(1, 100, _),
		ulid(atom)::generate(ULID).

	test(ulid_generate_1_atom_length, all(Length == 26)) :-
		integer::between(1, 100, _),
		ulid(atom)::generate(ULID),
		atom_length(ULID, Length).

	test(ulid_generate_1_chars, all(valid_base_32_chars(ULID))) :-
		integer::between(1, 100, _),
		ulid(chars)::generate(ULID).

	test(ulid_generate_1_chars_length, all(Length == 26)) :-
		integer::between(1, 100, _),
		ulid(chars)::generate(ULID),
		list::length(ULID, Length).

	test(ulid_generate_1_codes, all(valid_base_32_codes(ULID))) :-
		integer::between(1, 100, _),
		ulid(codes)::generate(ULID).

	test(ulid_generate_1_codes_length, all(Length == 26)) :-
		integer::between(1, 100, _),
		ulid(codes)::generate(ULID),
		list::length(ULID, Length).

	test(ulid_timestamp_2_integer, true(Timestamp == 1684316883000)) :-
		ulid::generate(1684316883000, ULID),
		ulid::timestamp(ULID, Timestamp).

	test(ulid_timestamp_2_float, true(Timestamp == 1684316883417)) :-
		ulid::generate(1684316883417, ULID),
		ulid::timestamp(ULID, Timestamp).

	test(ulid_generate_8_roundtrip, true(dt(Year,Month,Day,Hours,Minutes,Seconds,Milliseconds) == dt(2023,5,17,16,23,38,591))) :-
		ulid::generate(2023, 5, 17, 16, 23, 38, 591, ULID),
		ulid::timestamp(ULID, Year, Month, Day, Hours, Minutes, Seconds, Milliseconds).

	% auxiliary predicates

	valid_base_32_chars([]).
	valid_base_32_chars([Char| Chars]) :-
		valid_base_32_char(Char),
		valid_base_32_chars(Chars).

	valid_base_32_codes([]).
	valid_base_32_codes([Code| Codes]) :-
		char_code(Char, Code),
		valid_base_32_char(Char),
		valid_base_32_codes(Codes).

	% Crockford's Base32 encoding
	valid_base_32_char('0').
	valid_base_32_char('1').
	valid_base_32_char('2').
	valid_base_32_char('3').
	valid_base_32_char('4').
	valid_base_32_char('5').
	valid_base_32_char('6').
	valid_base_32_char('7').
	valid_base_32_char('8').
	valid_base_32_char('9').
	valid_base_32_char('A').
	valid_base_32_char('B').
	valid_base_32_char('C').
	valid_base_32_char('D').
	valid_base_32_char('E').
	valid_base_32_char('F').
	valid_base_32_char('G').
	valid_base_32_char('H').
	valid_base_32_char('J').
	valid_base_32_char('K').
	valid_base_32_char('M').
	valid_base_32_char('N').
	valid_base_32_char('P').
	valid_base_32_char('Q').
	valid_base_32_char('R').
	valid_base_32_char('S').
	valid_base_32_char('T').
	valid_base_32_char('V').
	valid_base_32_char('W').
	valid_base_32_char('X').
	valid_base_32_char('Y').
	valid_base_32_char('Z').

:- end_object.
