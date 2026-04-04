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


:- object(tests_32,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'Unit tests for the "hashes" library 32-bit algorithms.'
	]).

	cover(djb2_32).
	cover(sdbm_32).
	cover(fnv1a_32).
	cover(crc32).
	cover(murmurhash3_x86_32).
	cover(md5).

	test(djb2_32_empty, deterministic(Hash == '00001505')) :-
		djb2_32::hash([], Hash).

	test(djb2_32_a, deterministic(Hash == '0002b606')) :-
		djb2_32::hash([0'a], Hash).

	test(sdbm_32_empty, deterministic(Hash == '00000000')) :-
		sdbm_32::hash([], Hash).

	test(sdbm_32_a, deterministic(Hash == '00000061')) :-
		sdbm_32::hash([0'a], Hash).

	test(fnv1a_32_empty, deterministic(Hash == '811c9dc5')) :-
		fnv1a_32::hash([], Hash).

	test(fnv1a_32_a, deterministic(Hash == 'e40c292c')) :-
		fnv1a_32::hash([0'a], Hash).

	test(crc32_empty, deterministic(Hash == '00000000')) :-
		crc32::hash([], Hash).

	test(crc32_standard_vector, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		crc32::hash(Bytes, Hash).

	test(murmurhash3_x86_32_empty, deterministic(Hash == '00000000')) :-
		murmurhash3_x86_32::hash([], Hash).

	test(md5_empty, deterministic(Hash == 'd41d8cd98f00b204e9800998ecf8427e')) :-
		md5::hash([], Hash).

	test(md5_quick_brown_fox, deterministic(Hash == '9e107d9d372bb6826bd81d3542a419d6')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		md5::hash(Bytes, Hash).

:- end_object.
