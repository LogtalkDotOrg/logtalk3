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


:- object(tests_64,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'Unit tests for the "hashes" library 64-bit algorithms.'
	]).

	cover(djb2_64).
	cover(sdbm_64).
	cover(fnv1a_64).
	cover(siphash_2_4).
	cover(siphash_2_4(_)).
	cover(murmurhash3_x86_128).
	cover(murmurhash3_x64_128).
	cover(sha1).
	cover(sha256).


	test(djb2_64_empty, deterministic(Hash == '0000000000001505')) :-
		djb2_64::hash([], Hash).

	test(sdbm_64_empty, deterministic(Hash == '0000000000000000')) :-
		sdbm_64::hash([], Hash).

	test(fnv1a_64_empty, deterministic(Hash == 'cbf29ce484222325')) :-
		fnv1a_64::hash([], Hash).

	test(fnv1a_64_a, deterministic(Hash == 'af63dc4c8601ec8c')) :-
		fnv1a_64::hash([0'a], Hash).

	test(siphash_2_4_vector_0, deterministic(Hash == '726fdb47dd0e0e31')) :-
		siphash_2_4::hash([], Hash).

	test(siphash_2_4_vector_1, deterministic(Hash == '74f839c593dc67fd')) :-
		siphash_2_4::hash([0], Hash).

	test(siphash_2_4_vector_2, deterministic(Hash == '0d6c8009d9a94f5a')) :-
		siphash_2_4::hash([0,1], Hash).

	test(siphash_2_4_vector_3, deterministic(Hash == '85676696d7fb7e2d')) :-
		siphash_2_4::hash([0,1,2], Hash).

	test(siphash_2_4_vector_4, deterministic(Hash == 'cf2794e0277187b7')) :-
		siphash_2_4::hash([0,1,2,3], Hash).

	test(siphash_2_4_vector_5, deterministic(Hash == '18765564cd99a68d')) :-
		siphash_2_4::hash([0,1,2,3,4], Hash).

	test(siphash_2_4_vector_6, deterministic(Hash == 'cbc9466e58fee3ce')) :-
		siphash_2_4::hash([0,1,2,3,4,5], Hash).

	test(siphash_2_4_vector_7, deterministic(Hash == 'ab0200f58b01d137')) :-
		siphash_2_4::hash([0,1,2,3,4,5,6], Hash).

	test(siphash_2_4_vector_8, deterministic(Hash == '93f5f5799a932462')) :-
		siphash_2_4::hash([0,1,2,3,4,5,6,7], Hash).

	test(siphash_2_4_custom_key, deterministic(Hash == '726fdb47dd0e0e31')) :-
		siphash_2_4([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])::hash([], Hash).

	test(murmurhash3_x86_128_empty, deterministic(Hash == '00000000000000000000000000000000')) :-
		murmurhash3_x86_128::hash([], Hash).

	test(murmurhash3_x86_128_quick_brown_fox, deterministic(Hash == '2f1583c3ecee2c675d7bf66ce5e91d2c')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x86_128::hash(Bytes, Hash).

	test(murmurhash3_x64_128_empty, deterministic(Hash == '00000000000000000000000000000000')) :-
		murmurhash3_x64_128::hash([], Hash).

	test(murmurhash3_x64_128_quick_brown_fox, deterministic(Hash == 'e34bbc7bbc071b6c7a433ca9c49a9347')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x64_128::hash(Bytes, Hash).

	test(sha1_empty, deterministic(Hash == 'da39a3ee5e6b4b0d3255bfef95601890afd80709')) :-
		sha1::hash([], Hash).

	test(sha1_quick_brown_fox, deterministic(Hash == '2fd4e1c67a2d28fced849ee1bb76e7391b93eb12')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha1::hash(Bytes, Hash).

	test(sha256_empty, deterministic(Hash == 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855')) :-
		sha256::hash([], Hash).

	test(sha256_quick_brown_fox, deterministic(Hash == 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha256::hash(Bytes, Hash).

:- end_object.
