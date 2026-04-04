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
	cover(sha3_224).
	cover(sha3_256).
	cover(sha3_384).
	cover(sha3_512).
	cover(shake128(_)).
	cover(shake256(_)).
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

	test(sha3_224_empty, deterministic(Hash == '6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7')) :-
		sha3_224::hash([], Hash).

	test(sha3_224_quick_brown_fox, deterministic(Hash == 'd15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_224::hash(Bytes, Hash).

	test(sha3_256_empty, deterministic(Hash == 'a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a')) :-
		sha3_256::hash([], Hash).

	test(sha3_256_quick_brown_fox, deterministic(Hash == '69070dda01975c8c120c3aada1b282394e7f032fa9cf32f4cb2259a0897dfc04')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_256::hash(Bytes, Hash).

	test(sha3_384_empty, deterministic(Hash == '0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004')) :-
		sha3_384::hash([], Hash).

	test(sha3_384_quick_brown_fox, deterministic(Hash == '7063465e08a93bce31cd89d2e3ca8f602498696e253592ed26f07bf7e703cf328581e1471a7ba7ab119b1a9ebdf8be41')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_384::hash(Bytes, Hash).

	test(sha3_512_empty, deterministic(Hash == 'a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26')) :-
		sha3_512::hash([], Hash).

	test(sha3_512_quick_brown_fox, deterministic(Hash == '01dedd5de4ef14642445ba5f5b97c15e47b9ad931326e4b0727cd94cefc44fff23f07bf543139939b49128caf436dc1bdee54fcb24023a08d9403f9b4bf0d450')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_512::hash(Bytes, Hash).

	test(shake128_32_empty, deterministic(Hash == '7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef26')) :-
		shake128(32)::hash([], Hash).

	test(shake128_32_quick_brown_fox, deterministic(Hash == 'f4202e3c5852f9182a0430fd8144f0a74b95e7417ecae17db0f8cfeed0e3e66e')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		shake128(32)::hash(Bytes, Hash).

	test(shake256_64_empty, deterministic(Hash == '46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762fd75dc4ddd8c0f200cb05019d67b592f6fc821c49479ab48640292eacb3b7c4be')) :-
		shake256(64)::hash([], Hash).

	test(shake256_64_quick_brown_fox, deterministic(Hash == '2f671343d9b2e1604dc9dcf0753e5fe15c7c64a0d283cbbf722d411a0e36f6ca1d01d1369a23539cd80f7c054b6e5daf9c962cad5b8ed5bd11998b40d5734442')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		shake256(64)::hash(Bytes, Hash).

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
