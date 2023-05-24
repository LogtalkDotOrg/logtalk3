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
		date is 2023-05-24,
		comment is 'Unit tests for the "ulid" library.'
	]).

	cover(ulid).
	cover(ulid(_)).

	% generate/1 type tests

	test(ulid_generate_1_atom, true(type::check(ulid(atom), ULID))) :-
		ulid(atom)::generate(ULID).

	test(ulid_generate_1_chars, true(type::check(ulid(chars), ULID))) :-
		ulid(chars)::generate(ULID).

	test(ulid_generate_1_codes, true(type::check(ulid(codes), ULID))) :-
		ulid(codes)::generate(ULID).

	test(ulid_generate_1_monotonic_sort_order, true(ULIDs == SortedULIDs)) :-
		% per spec, within the same millisecond, monotonic sort order is not guaranteed
		findall(
			ULID,
			(integer::between(1, 10, _), ulid(atom)::generate(ULID), os::sleep(1)),
			ULIDs
		),
		sort(ULIDs, SortedULIDs).

	% timestamp/2 type tests

	test(ulid_timestamp_2_integer, true(Timestamp == 1684316883000)) :-
		ulid::generate(1684316883000, ULID),
		ulid::timestamp(ULID, Timestamp).

	test(ulid_timestamp_2_float, true(Timestamp == 1684316883417)) :-
		ulid::generate(1684316883417, ULID),
		ulid::timestamp(ULID, Timestamp).

	% generate/8 and timestamp/8 type tests

	test(ulid_generate_8_roundtrip, true(dt(Year,Month,Day,Hours,Minutes,Seconds,Milliseconds) == dt(2023,5,17,16,23,38,591))) :-
		ulid::generate(2023, 5, 17, 16, 23, 38, 591, ULID),
		ulid::timestamp(ULID, Year, Month, Day, Hours, Minutes, Seconds, Milliseconds).

	% ulid(Representation) type tests

	test(ulid_type_check_atom_instantiation_error, ball(instantiation_error)) :-
		type::check(ulid(atom), _).

	test(ulid_type_check_atom_type_error, ball(type_error(atom, 42))) :-
		type::check(ulid(atom), 42).

	test(ulid_type_check_atom_domain_error_01, ball(domain_error(ulid, 'ABC'))) :-
		type::check(ulid(atom), 'ABC').

	test(ulid_type_check_atom_domain_error_02, ball(domain_error(ulid, '8ZZZZZZZZZZZZZZZZZZZZZZZZZ'))) :-
		type::check(ulid(atom), '8ZZZZZZZZZZZZZZZZZZZZZZZZZ').

	test(ulid_type_check_atom_domain_error_03, ball(domain_error(ulid, '01BX5ZIKBKALTAV9OEVGEMMVRY'))) :-
		type::check(ulid(atom), '01BX5ZIKBKALTAV9OEVGEMMVRY').

	test(ulid_type_check_chars_instantiation_error, ball(instantiation_error)) :-
		type::check(ulid(chars), _).

	test(ulid_type_check_chars_type_error, ball(type_error(chars, 42))) :-
		type::check(ulid(chars), 42).

	test(ulid_type_check_chars_domain_error_01, ball(domain_error(ulid, ['A','B','C']))) :-
		type::check(ulid(chars), ['A','B','C']).

	test(ulid_type_check_chars_domain_error_02, ball(domain_error(ulid, ['8','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z']))) :-
		type::check(ulid(chars), ['8','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z','Z']).

	test(ulid_type_check_chars_domain_error_03, ball(domain_error(ulid, ['0','1','B','X','5','Z','I','K','B','K','A','L','T','A','V','9','O','E','V','G','E','M','M','V','R','Y']))) :-
		type::check(ulid(chars), ['0','1','B','X','5','Z','I','K','B','K','A','L','T','A','V','9','O','E','V','G','E','M','M','V','R','Y']).

	test(ulid_type_check_codes_instantiation_error, ball(instantiation_error)) :-
		type::check(ulid(codes), _).

	test(ulid_type_check_codes_type_error, ball(type_error(codes, 42))) :-
		type::check(ulid(codes), 42).

	test(ulid_type_check_codes_domain_error_01, ball(domain_error(ulid, [0'A,0'B,0'C]))) :-
		type::check(ulid(codes), [0'A,0'B,0'C]).

	test(ulid_type_check_codes_domain_error_02, ball(domain_error(ulid, [0'8,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z]))) :-
		type::check(ulid(codes), [0'8,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z,0'Z]).

	test(ulid_type_check_codes_domain_error_03, ball(domain_error(ulid, [0'0,0'1,0'B,0'X,0'5,0'Z,0'I,0'K,0'B,0'K,0'A,0'L,0'T,0'A,0'V,0'9,0'O,0'E,0'V,0'G,0'E,0'M,0'M,0'V,0'R,0'Y]))) :-
		type::check(ulid(codes), [0'0,0'1,0'B,0'X,0'5,0'Z,0'I,0'K,0'B,0'K,0'A,0'L,0'T,0'A,0'V,0'9,0'O,0'E,0'V,0'G,0'E,0'M,0'M,0'V,0'R,0'Y]).

:- end_object.
