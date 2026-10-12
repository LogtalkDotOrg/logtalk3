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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Unit tests for the "hashes" library 64-bit algorithms.'
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(list, [
		append/3, length/2, take/4
	]).

	:- uses(integer, [
		sequence/3
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	cover(djb2_64).
	cover(sdbm_64).
	cover(fnv1a_64).
	cover(siphash_2_4).
	cover(siphash_2_4(_)).
	cover(murmurhash3_x86_128).
	cover(murmurhash3_x64_128).
	cover(blake2b).
	cover(blake2b(_, _)).
	cover(sha3_224).
	cover(sha3_256).
	cover(sha3_384).
	cover(sha3_512).
	cover(shake128(_)).
	cover(shake256(_)).
	cover(sha1).
	cover(sha224).
	cover(sha256).
	cover(sha384).
	cover(sha512).
	cover(sha512_256).
	cover(hash_common_64).
	cover(fips202_hash(_, _, _)).
	cover(fips202_fixed_hash(_, _, _)).

	test(hashes_byte_sequence_wraparound, deterministic(Bytes == Expected)) :-
		sequence(0, 255, Prefix),
		append(Prefix, [0, 1, 2, 3], Expected),
		byte_sequence(260, Bytes).

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

	test(blake2b_empty, deterministic(Hash == '786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce')) :-
		blake2b::hash([], Hash).

	test(blake2b_abc, deterministic(Hash == 'ba80a53f981c4d0d6a2797b69f12f6e94c212f14685ac4b74b12bb6fdbffa2d17d87c5392aab792dc252d5de4533cc9518d38aa8dbf1925ab92386edd4009923')) :-
		atom_codes('abc', Bytes),
		blake2b::hash(Bytes, Hash).

	test(blake2b_hash_digest_protocol, deterministic(Info == info('786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce', 64, 128))) :-
		blake2b::digest([], Digest),
		bytes_hex(Digest, Hex),
		blake2b::digest_size(DigestSize),
		blake2b::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(blake2b_parametric_empty_32, deterministic(Hash == '0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8')) :-
		blake2b([], 32)::hash([], Hash).

	test(blake2b_parametric_abc_32, deterministic(Hash == 'bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319')) :-
		atom_codes('abc', Bytes),
		blake2b([], 32)::hash(Bytes, Hash).

	test(blake2b_parametric_digest_size_1, deterministic(Hash == '2e')) :-
		blake2b([], 1)::hash([], Hash).

	test(blake2b_parametric_digest_size_63, deterministic(Hash == '4ded8c5fc8b12f3273f877ca585a44ad6503249a2b345d6d9c0e67d85bcb700db4178c0303e93b8f4ad758b8e2c9fd8b3d0c28e585f1928334bb77d36782e8')) :-
		blake2b([], 63)::hash([], Hash).

	test(blake2b_parametric_keyed_empty_32, deterministic(Hash == '52897a9c27e9781839168d22a76145b07a8a7f8cd85c1d3709e5b09468405099')) :-
		blake2b([0], 32)::hash([], Hash).

	test(blake2b_parametric_keyed_abc_32, deterministic(Hash == '33d411f854d934a7ef279ed88366304b327e57814385e58fd09660fdc6ac6dc9')) :-
		atom_codes('abc', Bytes),
		blake2b([0], 32)::hash(Bytes, Hash).

	test(blake2b_parametric_max_key_empty, deterministic(Hash == '10ebb67700b1868efb4417987acf4690ae9d972fb7a590c2f02871799aaa4786b5e996e8f0f4eb981fc214b005f42d2ff4233499391653df7aefcbc13fc51568')) :-
		sequence(0, 63, Key),
		blake2b(Key, 64)::hash([], Hash).

	test(blake2b_parametric_hash_digest_protocol, deterministic(Info == info('0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8', 32, 128))) :-
		blake2b([], 32)::digest([], Digest),
		bytes_hex(Digest, Hex),
		assertion(Hex \== '786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25'),
		blake2b([], 32)::digest_size(DigestSize),
		blake2b([], 32)::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

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

	test(sha224_empty, deterministic(Hash == 'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f')) :-
		sha224::hash([], Hash).

	test(sha224_abc, deterministic(Hash == '23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7')) :-
		atom_codes('abc', Bytes),
		sha224::hash(Bytes, Hash).

	test(sha512_empty, deterministic(Hash == 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e')) :-
		sha512::hash([], Hash).

	test(sha512_abc, deterministic(Hash == 'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f')) :-
		atom_codes('abc', Bytes),
		sha512::hash(Bytes, Hash).

	test(sha512_256_empty, deterministic(Hash == 'c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a')) :-
		sha512_256::hash([], Hash).

	test(sha512_256_abc, deterministic(Hash == '53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23')) :-
		atom_codes('abc', Bytes),
		sha512_256::hash(Bytes, Hash).

	test(sha384_empty, deterministic(Hash == '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b')) :-
		sha384::hash([], Hash).

	test(sha384_abc, deterministic(Hash == 'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7')) :-
		atom_codes('abc', Bytes),
		sha384::hash(Bytes, Hash).

	test(sha3_224_hash_digest_protocol, deterministic(Info == info('6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7', 28, 144))) :-
		sha3_224::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha3_224::digest_size(DigestSize),
		sha3_224::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha3_256_hash_digest_protocol, deterministic(Info == info('a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a', 32, 136))) :-
		sha3_256::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha3_256::digest_size(DigestSize),
		sha3_256::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha3_384_hash_digest_protocol, deterministic(Info == info('0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004', 48, 104))) :-
		sha3_384::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha3_384::digest_size(DigestSize),
		sha3_384::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha3_512_hash_digest_protocol, deterministic(Info == info('a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26', 64, 72))) :-
		sha3_512::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha3_512::digest_size(DigestSize),
		sha3_512::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha1_hash_digest_protocol, deterministic(Info == info('da39a3ee5e6b4b0d3255bfef95601890afd80709', 20, 64))) :-
		sha1::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha1::digest_size(DigestSize),
		sha1::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha256_hash_digest_protocol, deterministic(Info == info('e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855', 32, 64))) :-
		sha256::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha256::digest_size(DigestSize),
		sha256::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha224_hash_digest_protocol, deterministic(Info == info('d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f', 28, 64))) :-
		sha224::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha224::digest_size(DigestSize),
		sha224::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha512_hash_digest_protocol, deterministic(Info == info('cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e', 64, 128))) :-
		sha512::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha512::digest_size(DigestSize),
		sha512::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha512_256_hash_digest_protocol, deterministic(Info == info('c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a', 32, 128))) :-
		sha512_256::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha512_256::digest_size(DigestSize),
		sha512_256::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(sha384_hash_digest_protocol, deterministic(Info == info('38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b', 48, 128))) :-
		sha384::digest([], Digest),
		bytes_hex(Digest, Hex),
		sha384::digest_size(DigestSize),
		sha384::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	% incremental hashing tests (new_hash_state/1, update_hash_state/3, final_hash_state/2)

	test(djb2_64_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		djb2_64::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(djb2_64, Chunks, Hash2).

	test(sdbm_64_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sdbm_64::hash(Bytes, Hash1),
		chunks(7, Bytes, Chunks),
		run_incremental(sdbm_64, Chunks, Hash2).

	test(fnv1a_64_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		fnv1a_64::hash(Bytes, Hash1),
		chunks(3, Bytes, Chunks),
		run_incremental(fnv1a_64, Chunks, Hash2).

	test(siphash_2_4_incremental_empty, deterministic(Hash == '726fdb47dd0e0e31')) :-
		run_incremental(siphash_2_4, [], Hash).

	test(siphash_2_4_incremental_vector_4, deterministic(Hash == 'cf2794e0277187b7')) :-
		run_incremental(siphash_2_4, [[0,1], [2,3]], Hash).

	% byte-by-byte chunking exercises the worst case for the internal
	% leftover buffer, which never accumulates more than 7 bytes at a time
	test(siphash_2_4_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		siphash_2_4::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(siphash_2_4, Chunks, Hash2).

	test(siphash_2_4_key_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		Key = [15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0],
		siphash_2_4(Key)::hash(Bytes, Hash1),
		chunks(5, Bytes, Chunks),
		run_incremental(siphash_2_4(Key), Chunks, Hash2).

	test(murmurhash3_x86_128_incremental_empty, deterministic(Hash == '00000000000000000000000000000000')) :-
		run_incremental(murmurhash3_x86_128, [], Hash).

	test(murmurhash3_x86_128_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x86_128::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(murmurhash3_x86_128, Chunks, Hash2).

	test(murmurhash3_x64_128_incremental_empty, deterministic(Hash == '00000000000000000000000000000000')) :-
		run_incremental(murmurhash3_x64_128, [], Hash).

	test(murmurhash3_x64_128_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x64_128::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(murmurhash3_x64_128, Chunks, Hash2).

	test(blake2b_incremental_empty, deterministic(Hash == '786a02f742015903c6c6fd852552d272912f4740e15847618a86e217f71f5419d25e1031afee585313896444934eb04b903a685b1448b755d56f701afe9be2ce')) :-
		run_incremental(blake2b, [], Hash).

	test(blake2b_incremental_matches_hash_single_chunk, deterministic(Hash1 == Hash2)) :-
		atom_codes('abc', Bytes),
		blake2b::hash(Bytes, Hash1),
		run_incremental(blake2b, [Bytes], Hash2).

	% exact block boundary: two chunks whose lengths add up to exactly one
	% 128-byte BLAKE2b block; unlike MD5/SHA, this must still be compressed
	% as the final block (Final = true), which blake2b_consume_full_blocks/6
	% only does at final_hash_state/2, never eagerly inside update_hash_state/3
	test(blake2b_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		blake2b::hash(Bytes, Hash1),
		take(50, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	test(blake2b_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		blake2b::hash(Bytes, Hash1),
		take(128, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	test(blake2b_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 127, Bytes),
		blake2b::hash(Bytes, Hash1),
		take(90, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	% unaligned chunking across several blocks, exercising the buffer and
	% block-consuming logic at every offset within a block
	test(blake2b_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		byte_sequence(260, Bytes),
		blake2b::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(blake2b, Chunks, Hash2).

	% BLAKE2-specific: the compression of a block differs depending on
	% whether it is flagged final or not, so a message that is exactly one
	% block long must hash differently from the same bytes followed by more
	% data, even though the first block's raw bytes are identical in both
	% cases; this pins down that update_hash_state/3 never compresses a
	% held-back block until final_hash_state/2 confirms it really is final
	test(blake2b_incremental_final_block_is_not_compressed_early, deterministic(Hash128 \== Hash129)) :-
		sequence(1, 128, Bytes128),
		blake2b::hash(Bytes128, Hash128),
		run_incremental(blake2b, [Bytes128, [129]], Hash129).

	test(blake2b_parametric_keyed_incremental_empty, deterministic(Hash == '52897a9c27e9781839168d22a76145b07a8a7f8cd85c1d3709e5b09468405099')) :-
		run_incremental(blake2b([0], 32), [], Hash).

	test(blake2b_parametric_keyed_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		blake2b([0], 32)::hash(Bytes, Hash1),
		chunks(11, Bytes, Chunks),
		run_incremental(blake2b([0], 32), Chunks, Hash2).

	% SHA-3 / SHAKE family: byte-by-byte equivalence, plus a test with the
	% message length exactly matching the rate (block) size to exercise the
	% absorb_full_blocks/4 and padding_block/2 boundary
	test(sha3_224_incremental_matches_hash_byte_by_byte, deterministic(Hash == 'd15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_224, Chunks, Hash).

	test(sha3_224_incremental_matches_hash_rate_boundary, deterministic(Hash == '5be75e6a08f19913a1d8036c056cc4556b98dc90aeca3f2a0664dedc')) :-
		sequence(0, 143, Bytes),
		take(100, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_224, [Chunk1, Chunk2], Hash).

	test(sha3_256_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_256::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_256, Chunks, Hash2).

	test(sha3_256_incremental_matches_hash_rate_boundary, deterministic(Hash == 'cf3ccff92480a29160c2d38317c430e14749bfee1788106957dfe73f8c4930e5')) :-
		sequence(0, 135, Bytes),
		take(90, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_256, [Chunk1, Chunk2], Hash).

	test(sha3_384_incremental_matches_hash_byte_by_byte, deterministic(Hash == '7063465e08a93bce31cd89d2e3ca8f602498696e253592ed26f07bf7e703cf328581e1471a7ba7ab119b1a9ebdf8be41')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_384, Chunks, Hash).

	test(sha3_384_incremental_matches_hash_rate_boundary, deterministic(Hash == '5b8d0d5cf8b41be507be8fcbfcbdbac3a28eb368d430fed6780aaa78a93a8da4a6c50485949ca344f228be91a96005a3')) :-
		sequence(0, 103, Bytes),
		take(70, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_384, [Chunk1, Chunk2], Hash).

	test(sha3_512_incremental_matches_hash_byte_by_byte, deterministic(Hash == '01dedd5de4ef14642445ba5f5b97c15e47b9ad931326e4b0727cd94cefc44fff23f07bf543139939b49128caf436dc1bdee54fcb24023a08d9403f9b4bf0d450')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_512, Chunks, Hash).

	test(sha3_512_incremental_matches_hash_rate_boundary, deterministic(Hash == '5d63f2bbe971a983ac6847480106e4e1264ee3a0befd79954914e1d86e795b2e18238f12fc5e46cb9cc78efdec610a93647cc04e1c23d8caaa6a58c21dd26c07')) :-
		sequence(0, 71, Bytes),
		take(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_512, [Chunk1, Chunk2], Hash).

	test(shake128_32_incremental_matches_hash_byte_by_byte, deterministic(Hash == 'f4202e3c5852f9182a0430fd8144f0a74b95e7417ecae17db0f8cfeed0e3e66e')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		chunks(1, Bytes, Chunks),
		run_incremental(shake128(32), Chunks, Hash).

	test(shake128_32_incremental_matches_hash_rate_boundary, deterministic(Hash == 'f15277eb61c4908d44a2853f3cde071ae2ed7a23461fbe162a1a98cf6875059c')) :-
		sequence(0, 167, Bytes),
		take(120, Bytes, Chunk1, Chunk2),
		run_incremental(shake128(32), [Chunk1, Chunk2], Hash).

	test(shake256_64_incremental_matches_hash_byte_by_byte, deterministic(Hash == '2f671343d9b2e1604dc9dcf0753e5fe15c7c64a0d283cbbf722d411a0e36f6ca1d01d1369a23539cd80f7c054b6e5daf9c962cad5b8ed5bd11998b40d5734442')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		chunks(1, Bytes, Chunks),
		run_incremental(shake256(64), Chunks, Hash).

	test(shake256_64_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 135, Bytes),
		shake256(64)::hash(Bytes, Hash1),
		take(90, Bytes, Chunk1, Chunk2),
		run_incremental(shake256(64), [Chunk1, Chunk2], Hash2).

	test(sha1_incremental_empty, deterministic(Hash == 'da39a3ee5e6b4b0d3255bfef95601890afd80709')) :-
		run_incremental(sha1, [], Hash).

	test(sha1_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		sha1::hash(Bytes, Hash1),
		take(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha1, [Chunk1, Chunk2], Hash2).

	test(sha1_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		sha1::hash(Bytes, Hash1),
		take(64, Bytes, Chunk1, Chunk2),
		run_incremental(sha1, [Chunk1, Chunk2], Hash2).

	test(sha1_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		sequence(1, 130, Bytes),
		sha1::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha1, Chunks, Hash2).

	test(sha256_incremental_empty, deterministic(Hash == 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855')) :-
		run_incremental(sha256, [], Hash).

	test(sha256_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		sha256::hash(Bytes, Hash1),
		take(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha256, [Chunk1, Chunk2], Hash2).

	test(sha256_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		sha256::hash(Bytes, Hash1),
		take(40, Bytes, Chunk1, Chunk2),
		run_incremental(sha256, [Chunk1, Chunk2], Hash2).

	test(sha256_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		sequence(1, 130, Bytes),
		sha256::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha256, Chunks, Hash2).

	test(sha224_incremental_empty, deterministic(Hash == 'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f')) :-
		run_incremental(sha224, [], Hash).

	test(sha224_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		sha224::hash(Bytes, Hash1),
		take(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha224, [Chunk1, Chunk2], Hash2).

	test(sha224_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		sha224::hash(Bytes, Hash1),
		take(64, Bytes, Chunk1, Chunk2),
		run_incremental(sha224, [Chunk1, Chunk2], Hash2).

	test(sha224_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		sequence(1, 130, Bytes),
		sha224::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha224, Chunks, Hash2).

	test(sha512_incremental_empty, deterministic(Hash == 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e')) :-
		run_incremental(sha512, [], Hash).

	test(sha512_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		sha512::hash(Bytes, Hash1),
		take(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha512, [Chunk1, Chunk2], Hash2).

	test(sha512_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha512::hash(Bytes, Hash1),
		take(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha512, [Chunk1, Chunk2], Hash2).

	test(sha512_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		byte_sequence(260, Bytes),
		sha512::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(sha512, Chunks, Hash2).

	test(sha512_256_incremental_empty, deterministic(Hash == 'c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a')) :-
		run_incremental(sha512_256, [], Hash).

	test(sha512_256_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		sha512_256::hash(Bytes, Hash1),
		take(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha512_256, [Chunk1, Chunk2], Hash2).

	test(sha512_256_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha512_256::hash(Bytes, Hash1),
		take(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha512_256, [Chunk1, Chunk2], Hash2).

	test(sha512_256_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		byte_sequence(260, Bytes),
		sha512_256::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(sha512_256, Chunks, Hash2).

	test(sha384_incremental_empty, deterministic(Hash == '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b')) :-
		run_incremental(sha384, [], Hash).

	test(sha384_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		sha384::hash(Bytes, Hash1),
		take(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha384, [Chunk1, Chunk2], Hash2).

	test(sha384_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha384::hash(Bytes, Hash1),
		take(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha384, [Chunk1, Chunk2], Hash2).

	test(sha384_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		byte_sequence(260, Bytes),
		sha384::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(sha384, Chunks, Hash2).

	% the state must not be tied to a single new_hash_state/1 call: two
	% independent hashing sessions interleaved through the same predicates
	% must not interfere with each other
	test(sha256_incremental_independent_sessions, deterministic(HashA-HashB == ExpectedA-ExpectedB)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes1),
		atom_codes('Pack my box with five dozen liquor jugs', Bytes2),
		sha256::hash(Bytes1, ExpectedA),
		sha256::hash(Bytes2, ExpectedB),
		sha256::new_hash_state(StateA0),
		sha256::new_hash_state(StateB0),
		sha256::update_hash_state(StateA0, Bytes1, StateA1),
		sha256::update_hash_state(StateB0, Bytes2, StateB1),
		sha256::final_hash_state(StateA1, HashA),
		sha256::final_hash_state(StateB1, HashB).

	test(hashes_blake2b_parameter_errors, deterministic) :-
		tests_32<<check_blake2_errors(blake2b, 64, 64).

	test(hashes_murmurhash3_x86_128_fourth_tail_word, deterministic(Hash == '3553d0af909796639097966390979663')) :-
		tests_32<<zero_bytes(15, Bytes),
		murmurhash3_x86_128::hash(Bytes, Hash).

	test(hashes_64_independent_boundaries, all(tests_32<<check_boundary(Object, Count, Expected))) :-
		findall(Object-Count-Expected, boundary_vector(Object, Count, Expected), Cases),
		Cases = [_| _],
		member(Object-Count-Expected, Cases).

	test(hashes_64_chunks_boundaries, deterministic(Results == [[], [[0], [1], [2]], [[0, 1], [2, 3]], [[0, 1], [2]]])) :-
		chunks(7, [], Empty),
		chunks(1, [0, 1, 2], Singletons),
		chunks(2, [0, 1, 2, 3], Exact),
		chunks(2, [0, 1, 2], Partial),
		Results = [Empty, Singletons, Exact, Partial].

	test(hashes_siphash_reference_vectors, all(tests_32<<check_boundary(siphash_2_4, Count, Expected))) :-
		findall(Count-Expected, siphash_vector(Count, Expected), Cases),
		length(Cases, 64),
		member(Count-Expected, Cases).

	test(hashes_siphash_zero_key, deterministic) :-
		tests_32<<zero_bytes(16, Key),
		tests_32<<check_boundary(siphash_2_4(Key), 17, 'e05a24800edfeef2').

	test(hashes_64_interleaved_families, all(tests_32<<check_sessions(Object))) :-
		member(Object, [siphash_2_4, murmurhash3_x86_128, murmurhash3_x64_128, blake2b, sha1, sha256, sha512, sha3_256, shake128(32)]).

	test(hashes_common_64_arithmetic_edges, deterministic) :-
		hash_common_64::mask64(Mask),
		hash_common_64::add64(Mask, 1, Sum),
		assertion(Sum == 0),
		hash_common_64::mul64(Mask, Mask, Product),
		assertion(Product == 1),
		hash_common_64::word64_hex(1, Hex),
		assertion(Hex == '0000000000000001').

	test(hashes_common_64_rotation_counts, all(check_rotation64(Count))) :-
		sequence(0, 64, Counts),
		member(Count, Counts).

	test(hashes_shake_output_rate_boundaries, all(check_shake_output(Variant, Rate, Expected, Extra))) :-
		findall(Variant-Rate-Expected-Extra, shake_output_vector(Variant, Rate, Expected, Extra), Cases),
		length(Cases, 2),
		member(Variant-Rate-Expected-Extra, Cases).

	test(hashes_sha256_large_update, deterministic(Hash == 'a1f259d4365ed4320c377ce26f5c8c56dcdc9a89e7b641bfd8eabfbbeac86654')) :-
		byte_sequence(16384, Bytes),
		run_incremental(sha256, [[], Bytes, []], Hash).

	test(hashes_common_64_length_field, all(Bytes == Expected)) :-
		Two32 is 1 << 32,
		Two64 is Two32 * Two32,
		HighLength is Two64 + 8,
		WrappedLength is Two64 * Two64 + 8,
		LowBytes = [0, 0, 0, 0, 0, 0, 0, 8],
		ZeroHigh = [0, 0, 0, 0, 0, 0, 0, 0| LowBytes],
		OneHigh = [0, 0, 0, 0, 0, 0, 0, 1| LowBytes],
		member(BitLength-Expected, [8-ZeroHigh, HighLength-OneHigh, WrappedLength-ZeroHigh]),
		hash_common_64<<length_bytes128(BitLength, Bytes).

	test(hashes_keccak_lane_packing, deterministic(Output == Bytes)) :-
		fips202_hash(136, 6, 32)<<initial_state(State0),
		Bytes = [0, 1, 2, 3, 4, 5, 6, 7, 8],
		fips202_hash(136, 6, 32)<<xor_block(Bytes, State0, State),
		State = [0x0706050403020100, 8| Rest],
		length(Rest, 23),
		fips202_hash(136, 6, 32)<<state_prefix_bytes(State, 9, Output).

	test(hashes_siphash_key_errors, deterministic) :-
		check_hash_input_error(siphash_2_4(Key), siphash_2_4, instantiation_error),
		assertion(var(Key)),
		check_hash_input_error(siphash_2_4([0| Tail]), siphash_2_4, instantiation_error),
		assertion(var(Tail)),
		tests_32<<zero_bytes(15, ShortKey),
		check_hash_input_error(siphash_2_4([Byte| ShortKey]), siphash_2_4, instantiation_error),
		assertion(var(Byte)),
		check_hash_input_error(siphash_2_4(key(Part)), siphash_2_4, instantiation_error),
		assertion(var(Part)),
		tests_32<<zero_bytes(17, LongKey),
		forall(
			member(BadKey, [invalid, [], [0| invalid], ShortKey, LongKey, [-1| ShortKey], [256| ShortKey], [1.5| ShortKey], [byte| ShortKey]]),
			check_hash_input_error(siphash_2_4(BadKey), siphash_2_4, domain_error(siphash_key, BadKey))
		).

	test(hashes_shake128_output_size_errors, deterministic) :-
		check_shake_errors(shake128).

	test(hashes_shake256_output_size_errors, deterministic) :-
		check_shake_errors(shake256).

	% auxiliary predicates

	check_hash_input_error(Object, Valid, Expected) :-
		Valid::new_hash_state(State),
		tests_32<<check_error(Object, hash([], _), Expected),
		tests_32<<check_error(Object, new_hash_state(_), Expected),
		tests_32<<check_error(Object, final_hash_state(State, _), Expected).

	check_shake_errors(Variant) :-
		Valid =.. [Variant, 0],
		Valid::hash([], EmptyHash),
		assertion(EmptyHash == ''),
		run_incremental(Valid, [[0], []], EmptyIncrementalHash),
		assertion(EmptyIncrementalHash == ''),
		Unbound =.. [Variant, Size],
		check_hash_input_error(Unbound, Valid, instantiation_error),
		assertion(var(Size)),
		Partial =.. [Variant, size(Part)],
		check_hash_input_error(Partial, Valid, instantiation_error),
		assertion(var(Part)),
		forall(
			member(BadSize, [-1, 1.5, invalid, [], size(1)]),
			(	Invalid =.. [Variant, BadSize],
				check_hash_input_error(Invalid, Valid, domain_error(shake_output_size, BadSize))
			)
		).

	check_shake_output(Variant, Rate, Expected, Extra) :-
		Object =.. [Variant, Rate],
		Object::hash([0'a, 0'b, 0'c], Hash),
		assertion(Hash == Expected),
		Size is Rate + 1,
		LargerObject =.. [Variant, Size],
		atom_concat(Expected, Extra, LargerExpected),
		run_incremental(LargerObject, [[0'a], [], [0'b, 0'c]], LargerHash),
		assertion(LargerHash == LargerExpected).

	shake_output_vector(shake128, 168, '5881092dd818bf5cf8a3ddb793fbcba74097d5c526a6d35f97b83351940f2cc844c50af32acd3f2cdd066568706f509bc1bdde58295dae3f891a9a0fca5783789a41f8611214ce612394df286a62d1a2252aa94db9c538956c717dc2bed4f232a0294c857c730aa16067ac1062f1201fb0d377cfb9cde4c63599b27f3462bba4a0ed296c801f9ff7f57302bb3076ee145f97a32ae68e76ab66c48d51675bd49acc29082f5647584e', '6a').
	shake_output_vector(shake256, 136, '483366601360a8771c6863080cc4114d8db44530f8f1e1ee4f94ea37e78b5739d5a15bef186a5386c75744c0527e1faa9f8726e462a12a4feb06bd8801e751e41385141204f329979fd3047a13c5657724ada64d2470157b3cdc288620944d78dbcddbd912993f0913f164fb2ce95131a2d09a3e6d51cbfc622720d7a75c6334e8a2d7ec71a7cc29', 'cf').

	check_rotation64(Count) :-
		Shift is Count mod 64,
		Expected is 1 << Shift,
		hash_common_64::rol64(1, Count, Rotated),
		assertion(Rotated == Expected),
		Inverse is (64 - Shift) mod 64,
		hash_common_64::rol64(Rotated, Inverse, Restored),
		assertion(Restored == 1).

	siphash_vector(0, '726fdb47dd0e0e31').
	siphash_vector(1, '74f839c593dc67fd').
	siphash_vector(2, '0d6c8009d9a94f5a').
	siphash_vector(3, '85676696d7fb7e2d').
	siphash_vector(4, 'cf2794e0277187b7').
	siphash_vector(5, '18765564cd99a68d').
	siphash_vector(6, 'cbc9466e58fee3ce').
	siphash_vector(7, 'ab0200f58b01d137').
	siphash_vector(8, '93f5f5799a932462').
	siphash_vector(9, '9e0082df0ba9e4b0').
	siphash_vector(10, '7a5dbbc594ddb9f3').
	siphash_vector(11, 'f4b32f46226bada7').
	siphash_vector(12, '751e8fbc860ee5fb').
	siphash_vector(13, '14ea5627c0843d90').
	siphash_vector(14, 'f723ca908e7af2ee').
	siphash_vector(15, 'a129ca6149be45e5').
	siphash_vector(16, '3f2acc7f57c29bdb').
	siphash_vector(17, '699ae9f52cbe4794').
	siphash_vector(18, '4bc1b3f0968dd39c').
	siphash_vector(19, 'bb6dc91da77961bd').
	siphash_vector(20, 'bed65cf21aa2ee98').
	siphash_vector(21, 'd0f2cbb02e3b67c7').
	siphash_vector(22, '93536795e3a33e88').
	siphash_vector(23, 'a80c038ccd5ccec8').
	siphash_vector(24, 'b8ad50c6f649af94').
	siphash_vector(25, 'bce192de8a85b8ea').
	siphash_vector(26, '17d835b85bbb15f3').
	siphash_vector(27, '2f2e6163076bcfad').
	siphash_vector(28, 'de4daaaca71dc9a5').
	siphash_vector(29, 'a6a2506687956571').
	siphash_vector(30, 'ad87a3535c49ef28').
	siphash_vector(31, '32d892fad841c342').
	siphash_vector(32, '7127512f72f27cce').
	siphash_vector(33, 'a7f32346f95978e3').
	siphash_vector(34, '12e0b01abb051238').
	siphash_vector(35, '15e034d40fa197ae').
	siphash_vector(36, '314dffbe0815a3b4').
	siphash_vector(37, '027990f029623981').
	siphash_vector(38, 'cadcd4e59ef40c4d').
	siphash_vector(39, '9abfd8766a33735c').
	siphash_vector(40, '0e3ea96b5304a7d0').
	siphash_vector(41, 'ad0c42d6fc585992').
	siphash_vector(42, '187306c89bc215a9').
	siphash_vector(43, 'd4a60abcf3792b95').
	siphash_vector(44, 'f935451de4f21df2').
	siphash_vector(45, 'a9538f0419755787').
	siphash_vector(46, 'db9acddff56ca510').
	siphash_vector(47, 'd06c98cd5c0975eb').
	siphash_vector(48, 'e612a3cb9ecba951').
	siphash_vector(49, 'c766e62cfcadaf96').
	siphash_vector(50, 'ee64435a9752fe72').
	siphash_vector(51, 'a192d576b245165a').
	siphash_vector(52, '0a8787bf8ecb74b2').
	siphash_vector(53, '81b3e73d20b49b6f').
	siphash_vector(54, '7fa8220ba3b2ecea').
	siphash_vector(55, '245731c13ca42499').
	siphash_vector(56, 'b78dbfaf3a8d83bd').
	siphash_vector(57, 'ea1ad565322a1a0b').
	siphash_vector(58, '60e61c23a3795013').
	siphash_vector(59, '6606d7e446282b93').
	siphash_vector(60, '6ca4ecb15c5f91e1').
	siphash_vector(61, '9f626da15c9625f3').
	siphash_vector(62, 'e51b38608ef25f57').
	siphash_vector(63, '958a324ceb064572').

	boundary_vector(sha256, 55, '463eb28e72f82e0a96c0a4cc53690c571281131f672aa229e0d45ae59b598b59').
	boundary_vector(siphash_2_4, 255, 'a9c169fec74db21a').
	boundary_vector(siphash_2_4, 256, '999d0526d2a7bfd7').
	boundary_vector(siphash_2_4, 257, '8a817b8d55b29748').
	boundary_vector(murmurhash3_x86_128, 0, '00000000000000000000000000000000').
	boundary_vector(murmurhash3_x86_128, 1, '88c4adec54d201b954d201b954d201b9').
	boundary_vector(murmurhash3_x86_128, 2, '8acbc432dbd5d393dbd5d393dbd5d393').
	boundary_vector(murmurhash3_x86_128, 3, '37095e0d33c18cef33c18cef33c18cef').
	boundary_vector(murmurhash3_x86_128, 4, 'c72c99e74e343ce24e343ce24e343ce2').
	boundary_vector(murmurhash3_x86_128, 5, '90f84e5e4e7ccbd19275e7659275e765').
	boundary_vector(murmurhash3_x86_128, 6, '0769bbd2d4b11f92ee0d3ffcee0d3ffc').
	boundary_vector(murmurhash3_x86_128, 7, 'ccfa23d7bea74c33c75d01c8c75d01c8').
	boundary_vector(murmurhash3_x86_128, 8, '91f4928ab18cd5bc867244ac867244ac').
	boundary_vector(murmurhash3_x86_128, 9, '68213fd95c93cee4d165defe3737caf6').
	boundary_vector(murmurhash3_x86_128, 10, '8353bd128a66183ef0a8aca00e020a02').
	boundary_vector(murmurhash3_x86_128, 11, '7e3603e73e1d4f94acf8e804c8c3a35b').
	boundary_vector(murmurhash3_x86_128, 12, '3f7e3ab4efbbacdb43f77828566fef0e').
	boundary_vector(murmurhash3_x86_128, 13, '3fcb34d0c1565a55313cb9b2d05851bd').
	boundary_vector(murmurhash3_x86_128, 14, '64319956f6ae1ac844e7f16e5753a929').
	boundary_vector(murmurhash3_x86_128, 15, '757b9c5962a6e4ca61e2db4eca775566').
	boundary_vector(murmurhash3_x86_128, 16, '6c25c825559180eeea53807489a55bef').
	boundary_vector(murmurhash3_x64_128, 0, '00000000000000000000000000000000').
	boundary_vector(murmurhash3_x64_128, 1, '4610abe56eff5cb551622daa78f83583').
	boundary_vector(murmurhash3_x64_128, 2, '7cb3f5c58dab264cf03c01326ec2e044').
	boundary_vector(murmurhash3_x64_128, 3, 'b872a12fef53e6befb6255c252b396b6').
	boundary_vector(murmurhash3_x64_128, 4, 'e1c594ae0ddfaf10d3d605bd13c2fde2').
	boundary_vector(murmurhash3_x64_128, 5, '41ee8cd4a6f94036f8d0155e630c23f6').
	boundary_vector(murmurhash3_x64_128, 6, '66983abba4f5043c57e0240b16512ca0').
	boundary_vector(murmurhash3_x64_128, 7, 'bd4c6987ca4b0d68613addd4bd25c787').
	boundary_vector(murmurhash3_x64_128, 8, '47a7e1bdd68e2fc860e6ee02ec31dcc7').
	boundary_vector(murmurhash3_x64_128, 9, 'fbb4cb0f6e812d3278de751d0200ffb9').
	boundary_vector(murmurhash3_x64_128, 10, 'cfca25e89e58e463254313c472ad2076').
	boundary_vector(murmurhash3_x64_128, 11, 'c57b4f47c7564f886965ea8d20711bb0').
	boundary_vector(murmurhash3_x64_128, 12, 'b35da7e69212a5ca8075f146ecf75346').
	boundary_vector(murmurhash3_x64_128, 13, '4b52d9f2c55f41c284ff869eafa6d8fc').
	boundary_vector(murmurhash3_x64_128, 14, '5fa933ee35906d640d782573380fc9df').
	boundary_vector(murmurhash3_x64_128, 15, '47231598fd4925e9cd846dee88c67de9').
	boundary_vector(murmurhash3_x64_128, 16, '444924b591903f30ab906456762fe845').
	boundary_vector(sha1, 55, '8ae2d46729cfe68ff927af5eec9c7d1b66d65ac2').
	boundary_vector(sha1, 56, '636e2ec698dac903498e648bd2f3af641d3c88cb').
	boundary_vector(sha1, 63, '6d942da0c4392b123528f2905c713a3ce28364bd').
	boundary_vector(sha1, 64, 'c6138d514ffa2135bfce0ed0b8fac65669917ec7').
	boundary_vector(sha1, 65, '69bd728ad6e13cd76ff19751fde427b00e395746').
	boundary_vector(sha224, 55, '8991dfba74284e04dc7581c7c3e4068ff6cb7a63733361429834bb56').
	boundary_vector(sha224, 56, '2b2cd637c16ad7290bb067ad7d8fd04e204fa43a84366afc7130f4ef').
	boundary_vector(sha224, 63, '049e8dd7eab3378ce9f823bfb569e5b270235d4b7f9623606971998f').
	boundary_vector(sha224, 64, 'c37b88a3522dbf7ac30d1c68ea397ac11d4773571aed01ddab73531e').
	boundary_vector(sha224, 65, '114b5fd665736a96585c5d5837d35250aed73c725252cbf7f8b121f6').
	boundary_vector(sha384, 111, 'f5f9fe110d809d34029de262a01b208356caec6e054c7f926b2591f6c9780579d4b59f5578c6f531a84f158a33660cef').
	boundary_vector(sha384, 112, '33ba080ec0ccb378e4e95fed3b26c23aa1a280476e007519ee47f60cd9c5c8a65d627259a9aa2fd33ca06d3c14ee5548').
	boundary_vector(sha384, 127, 'd5fcfe2fcf6b3ef375ede37c8123d9b78065fecc1d55197e2f7721e6e9a93d0ba4d7fd15f9b96dea2744df24141ba2ef').
	boundary_vector(sha384, 128, 'ca2385773319124534111a36d0581fc3f00815e907034b90cff9c3a861e126a741d5dfcff65a417b6d7296863ac0ec17').
	boundary_vector(sha384, 129, 'ef49ae5b9ad51433d00323528d81ea8d2e4d2b507dbd9f1cb84f952b66249a788b1c89fcdb77a0db9f1feb901d47fc73').
	boundary_vector(sha512_256, 111, 'bd209f60b0d04102a09175297fd255367e54b5a5605b928635c606306914363f').
	boundary_vector(sha512_256, 112, '2cafeb0882cc405167e9a255b8581a66dc683212474902dd453dbca20a94e61a').
	boundary_vector(sha512_256, 127, 'c26bc7e9315e62ab0dc6aeb577724d07c09b0c6fdfc0a9f08d8548047c032248').
	boundary_vector(sha512_256, 128, '2ff11194b2aec1f943cb5f130ba647c151334068083194d7281a55d607ae255f').
	boundary_vector(sha512_256, 129, 'c4a3bbf841ed2a289e5109fb392229c80db61c72fd92079b5a4f0441f095a111').
	boundary_vector(sha3_224, 143, '64d0e8a1be3cf30ef6727b30a6e428f7f068d44634c943d277ad8e7f').
	boundary_vector(sha3_224, 144, '5be75e6a08f19913a1d8036c056cc4556b98dc90aeca3f2a0664dedc').
	boundary_vector(sha3_224, 145, '90b861ac1b1598459ad8337afa9933ce2f1a6f972c57daf8fc2737e4').
	boundary_vector(sha3_224, 289, '8cbafc43a0f73ebbbb0b0b93694672c72d32feb0dfe364ebc0e94655').
	boundary_vector(sha3_384, 103, '1f91ee551ad18f268876d1fc262f137fe196580216c5193819a95ec5222537d2a658dd129c3d8080e65ec7460f1f4704').
	boundary_vector(sha3_384, 104, '5b8d0d5cf8b41be507be8fcbfcbdbac3a28eb368d430fed6780aaa78a93a8da4a6c50485949ca344f228be91a96005a3').
	boundary_vector(sha3_384, 105, '4a2f0a8f2f1f4cc4605cc2537e0be28cf8b465c30f0a54b494a7128ec54ee4e85706b5e47a5697344d15cbf85680cd40').
	boundary_vector(sha3_384, 209, '4a40dd56c8a2efb2e3de6f05fc8fe00df8af8869a66aff4fe734f9a6cb0db930d547fc0f3f213f6b8a172db13b15ec5c').
	boundary_vector(sha3_512, 71, '3ccc850d53a1287af7b4560b2ef0d43eb5d9a80d62a0e9cf1dbc040135921104d4395168e90bfc871773ebb34bca1bd67056e1cc7dc7a48ff7c3167d389f117c').
	boundary_vector(sha3_512, 72, '5d63f2bbe971a983ac6847480106e4e1264ee3a0befd79954914e1d86e795b2e18238f12fc5e46cb9cc78efdec610a93647cc04e1c23d8caaa6a58c21dd26c07').
	boundary_vector(sha3_512, 73, '921d9b7b2b0f3066a1646dbb058c979cb3925dec0f8c269faaa7f9648e73465ae55ec527257d5d5e1cfdbf5d6799bea1004b6186f5108c74e3b92fe924166558').
	boundary_vector(sha3_512, 145, '1abec62dce93a6775cd2ec0098d7264676a21e644c7c1b80580c305cfde31b7d5848c63af4d0e7cfeda2e5076a32dbd632665fbb1e7f06651b2ed4d7341ac844').
	boundary_vector(blake2b([0], 16), 127, 'ce61c3a7fe857c659b293a89732cee82').
	boundary_vector(blake2b([0], 16), 128, '7935b60c406dcd79184f9ffab4d4f34a').
	boundary_vector(blake2b([0], 16), 129, '3530ef49891e800f555af1916c7565c9').
	boundary_vector(blake2b([0], 16), 256, 'e84b982f29bb345e1d9e2883eea70fc3').
	boundary_vector(blake2b([0], 16), 257, 'c6049a9b4eef18936e7d5e670cdeb29d').
	boundary_vector(sha256, 56, 'da2ae4d6b36748f2a318f23e7ab1dfdf45acdc9d049bd80e59de82a60895f562').
	boundary_vector(sha256, 63, '29af2686fd53374a36b0846694cc342177e428d1647515f078784d69cdb9e488').
	boundary_vector(sha256, 64, 'fdeab9acf3710362bd2658cdc9a29e8f9c757fcf9811603a8c447cd1d9151108').
	boundary_vector(sha256, 65, '4bfd2c8b6f1eec7a2afeb48b934ee4b2694182027e6d0fc075074f2fabb31781').
	boundary_vector(sha512, 111, 'a1a111449b198d9b1f538bad7f3fc1022b3a5b1a5e90a0bc860de8512746cbc31599e6c834de3a3235327af0b51ff57bf7acf1974a73014d9c3953812edc7c8d').
	boundary_vector(sha512, 112, 'c5fbd731d19d2ae1180f001be72c2c1aaba1d7b094b3748880e24593b8e117a750e11c1bd867cc2f96dace8c8b74abd2d5c4f236be444e77d30d1916174070b9').
	boundary_vector(sha512, 127, 'eab89674feaa34e27aebeeff3c0a4d70070bb872d5e9f186cf1dbbdee517b6e35724d629ff025a5b07185e911ada7e3c8acf830aa0e4f71777bd2d44f504f7f0').
	boundary_vector(sha512, 128, '1dffd5e3adb71d45d2245939665521ae001a317a03720a45732ba1900ca3b8351fc5c9b4ca513eba6f80bc7b1d1fdad4abd13491cb824d61b08d8c0e1561b3f7').
	boundary_vector(sha512, 129, '1d9da57fbbdab09afb3506ab2d223d06109d65c1c8ad197f50138f714bc4c3f2fe5787922639c680acad1c651f955990425954ce2cba0c5cc83f2667d878eb0f').
	boundary_vector(blake2b, 127, 'b6292669ccd38d5f01caae96ba272c76a879a45743afa0725d83b9ebb26665b731f1848c52f11972b6644f554c064fa90780dbbbf3a89d4fc31f67df3e5857ef').
	boundary_vector(blake2b, 128, '2319e3789c47e2daa5fe807f61bec2a1a6537fa03f19ff32e87eecbfd64b7e0e8ccff439ac333b040f19b0c4ddd11a61e24ac1fe0f10a039806c5dcc0da3d115').
	boundary_vector(blake2b, 129, 'f59711d44a031d5f97a9413c065d1e614c417ede998590325f49bad2fd444d3e4418be19aec4e11449ac1a57207898bc57d76a1bcf3566292c20c683a5c4648f').
	boundary_vector(blake2b, 256, '1ecc896f34d3f9cac484c73f75f6a5fb58ee6784be41b35f46067b9c65c63a6794d3d744112c653f73dd7deb6666204c5a9bfa5b46081fc10fdbe7884fa5cbf8').
	boundary_vector(blake2b, 257, 'd8bfe068de0b4f9fa876a3f8024eb9f7b0029fd5dcf251199e065cee89e1a282c8dbf0442f2ade7294ac1c6be19b388dc990c34d8cb79f5f10c54fa813834fda').
	boundary_vector(sha3_256, 135, 'fded8fd9d6551c601eeb3b7c6bc5e5cfd8aad1d015b7e9aaa9c9b9475231d5e2').
	boundary_vector(sha3_256, 136, 'cf3ccff92480a29160c2d38317c430e14749bfee1788106957dfe73f8c4930e5').
	boundary_vector(sha3_256, 137, 'ce9d7dc90913ee5d92745019479a5352c6d6279bef18ed07dc0a83ee8084daca').
	boundary_vector(sha3_256, 273, '6e7f5de2677213044468ef21d3c8c57bb10cc5957e4f99d038db65ac3151e9c1').
	boundary_vector(shake128(32), 167, '1e552791cc4e93a0d4a8dc47ae49228c2faa869e40e628f6ace477aec3f1ca7a').
	boundary_vector(shake128(32), 168, 'f15277eb61c4908d44a2853f3cde071ae2ed7a23461fbe162a1a98cf6875059c').
	boundary_vector(shake128(32), 169, '015be3338c986d9846affa0f94b4afc2a76bc289c709e1a596ec9eccf090a773').
	boundary_vector(shake128(0), 0, '').
	boundary_vector(shake128(337), 0, '7f9c2ba4e88f827d616045507605853ed73b8093f6efbc88eb1a6eacfa66ef263cb1eea988004b93103cfb0aeefd2a686e01fa4a58e8a3639ca8a1e3f9ae57e235b8cc873c23dc62b8d260169afa2f75ab916a58d974918835d25e6a435085b2badfd6dfaac359a5efbb7bcc4b59d538df9a04302e10c8bc1cbf1a0b3a5120ea17cda7cfad765f5623474d368ccca8af0007cd9f5e4c849f167a580b14aabdefaee7eef47cb0fca9767be1fda69419dfb927e9df07348b196691abaeb580b32def58538b8d23f87732ea63b02b4fa0f4873360e2841928cd60dd4cee8cc0d4c922a96188d032675c8ac850933c7aff1533b94c834adbb69c6115bad4692d8619f90b0cdf8a7b9c264029ac185b70b83f2801f2f4b3f70c593ea3aeeb613a7f1b1de33fd75081f592305f2e4526edc09631b10958f464d889f31ba010250fda7f1368ec2967fc84ef2ae9aff268e0b1700a').
	boundary_vector(shake256(32), 135, 'c45dae624ad8a2f5aa7bac9d7557737fd91c96eedb70a6be5574d57a844eade0').
	boundary_vector(shake256(32), 136, 'b7ff4073b3f5a8eabd6e17705ca7f6761a31058f9df781a6a47e3a3063b9d67a').
	boundary_vector(shake256(32), 137, '01d90952c642a5eb2a8fc9d713f843a45d7ac05132dddcb2efc9bebc27e37bcb').
	boundary_vector(shake256(0), 0, '').
	boundary_vector(shake256(273), 0, '46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762fd75dc4ddd8c0f200cb05019d67b592f6fc821c49479ab48640292eacb3b7c4be141e96616fb13957692cc7edd0b45ae3dc07223c8e92937bef84bc0eab862853349ec75546f58fb7c2775c38462c5010d846c185c15111e595522a6bcd16cf86f3d122109e3b1fdd943b6aec468a2d621a7c06c6a957c62b54dafc3be87567d677231395f6147293b68ceab7a9e0c58d864e8efde4e1b9a46cbe854713672f5caaae314ed9083dab4b099f8e300f01b8650f1f4b1d8fcf3f3cb53fb8e9eb2ea203bdc970f50ae55428a91f7f53ac266b28419c3778a15fd248d339ede785fb7f5a1aaa96d313eacc890936c173cdcd0fab').

	byte_sequence(Count, Bytes) :-
		byte_sequence(Count, 0, Bytes).

	byte_sequence(0, _, []) :-
		!.
	byte_sequence(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		NextByte is (Byte + 1) mod 256,
		byte_sequence(NextCount, NextByte, Bytes).

	% splits List into a list of Chunks of (at most) N elements each,
	% used to drive update_hash_state/3 one chunk at a time
	chunks(_, [], []) :-
		!.
	chunks(N, List, [Chunk| Chunks]) :-
		(	take(N, List, Chunk, Rest) ->
			true
		;
			Chunk = List,
			Rest = []
		),
		chunks(N, Rest, Chunks).

	% drives Object's new_hash_state/1, update_hash_state/3, and
	% final_hash_state/2 over a list of chunks, simulating a caller feeding
	% the message to the hash function one chunk at a time
	run_incremental(Object, Chunks, Hash) :-
		Object::new_hash_state(State0),
		fold_update(Object, Chunks, State0, State),
		Object::final_hash_state(State, Hash).

	fold_update(_, [], State, State) :-
		!.
	fold_update(Object, [Chunk| Chunks], State0, State) :-
		Object::update_hash_state(State0, Chunk, State1),
		fold_update(Object, Chunks, State1, State).

:- end_object.
