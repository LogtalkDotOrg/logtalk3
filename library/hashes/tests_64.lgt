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
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-07-17,
		comment is 'Unit tests for the "hashes" library 64-bit algorithms.'
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(list, [
		append/3, length/2
	]).

	:- uses(integer, [
		sequence/3
	]).

	cover(djb2_64).
	cover(sdbm_64).
	cover(fnv1a_64).
	cover(siphash_2_4).
	cover(siphash_2_4(_)).
	cover(murmurhash3_x86_128).
	cover(murmurhash3_x64_128).
	cover(blake2b).
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
		split_at(50, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	test(blake2b_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		blake2b::hash(Bytes, Hash1),
		split_at(128, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	test(blake2b_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 127, Bytes),
		blake2b::hash(Bytes, Hash1),
		split_at(90, Bytes, Chunk1, Chunk2),
		run_incremental(blake2b, [Chunk1, Chunk2], Hash2).

	% unaligned chunking across several blocks, exercising the buffer and
	% block-consuming logic at every offset within a block
	test(blake2b_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		sequence(1, 260, Bytes),
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

	% SHA-3 / SHAKE family: byte-by-byte equivalence, plus a test with the
	% message length exactly matching the rate (block) size to exercise the
	% absorb_full_blocks/4 and padding_block/2 boundary
	test(sha3_224_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_224::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_224, Chunks, Hash2).

	test(sha3_224_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 143, Bytes),
		sha3_224::hash(Bytes, Hash1),
		split_at(100, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_224, [Chunk1, Chunk2], Hash2).

	test(sha3_256_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_256::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_256, Chunks, Hash2).

	test(sha3_256_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 135, Bytes),
		sha3_256::hash(Bytes, Hash1),
		split_at(90, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_256, [Chunk1, Chunk2], Hash2).

	test(sha3_384_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_384::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_384, Chunks, Hash2).

	test(sha3_384_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 103, Bytes),
		sha3_384::hash(Bytes, Hash1),
		split_at(70, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_384, [Chunk1, Chunk2], Hash2).

	test(sha3_512_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sha3_512::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(sha3_512, Chunks, Hash2).

	test(sha3_512_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 71, Bytes),
		sha3_512::hash(Bytes, Hash1),
		split_at(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha3_512, [Chunk1, Chunk2], Hash2).

	test(shake128_32_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		shake128(32)::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(shake128(32), Chunks, Hash2).

	test(shake128_32_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 167, Bytes),
		shake128(32)::hash(Bytes, Hash1),
		split_at(120, Bytes, Chunk1, Chunk2),
		run_incremental(shake128(32), [Chunk1, Chunk2], Hash2).

	test(shake256_64_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		shake256(64)::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(shake256(64), Chunks, Hash2).

	test(shake256_64_incremental_matches_hash_rate_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(0, 135, Bytes),
		shake256(64)::hash(Bytes, Hash1),
		split_at(90, Bytes, Chunk1, Chunk2),
		run_incremental(shake256(64), [Chunk1, Chunk2], Hash2).

	test(sha1_incremental_empty, deterministic(Hash == 'da39a3ee5e6b4b0d3255bfef95601890afd80709')) :-
		run_incremental(sha1, [], Hash).

	test(sha1_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		sha1::hash(Bytes, Hash1),
		split_at(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha1, [Chunk1, Chunk2], Hash2).

	test(sha1_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		sha1::hash(Bytes, Hash1),
		split_at(64, Bytes, Chunk1, Chunk2),
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
		split_at(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha256, [Chunk1, Chunk2], Hash2).

	test(sha256_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		sha256::hash(Bytes, Hash1),
		split_at(40, Bytes, Chunk1, Chunk2),
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
		split_at(30, Bytes, Chunk1, Chunk2),
		run_incremental(sha224, [Chunk1, Chunk2], Hash2).

	test(sha224_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		sha224::hash(Bytes, Hash1),
		split_at(64, Bytes, Chunk1, Chunk2),
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
		split_at(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha512, [Chunk1, Chunk2], Hash2).

	test(sha512_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha512::hash(Bytes, Hash1),
		split_at(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha512, [Chunk1, Chunk2], Hash2).

	test(sha512_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		sequence(1, 260, Bytes),
		sha512::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(sha512, Chunks, Hash2).

	test(sha512_256_incremental_empty, deterministic(Hash == 'c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a')) :-
		run_incremental(sha512_256, [], Hash).

	test(sha512_256_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		sha512_256::hash(Bytes, Hash1),
		split_at(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha512_256, [Chunk1, Chunk2], Hash2).

	test(sha512_256_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha512_256::hash(Bytes, Hash1),
		split_at(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha512_256, [Chunk1, Chunk2], Hash2).

	test(sha512_256_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		sequence(1, 260, Bytes),
		sha512_256::hash(Bytes, Hash1),
		chunks(9, Bytes, Chunks),
		run_incremental(sha512_256, Chunks, Hash2).

	test(sha384_incremental_empty, deterministic(Hash == '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b')) :-
		run_incremental(sha384, [], Hash).

	test(sha384_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 128, Bytes),
		sha384::hash(Bytes, Hash1),
		split_at(50, Bytes, Chunk1, Chunk2),
		run_incremental(sha384, [Chunk1, Chunk2], Hash2).

	test(sha384_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 129, Bytes),
		sha384::hash(Bytes, Hash1),
		split_at(128, Bytes, Chunk1, Chunk2),
		run_incremental(sha384, [Chunk1, Chunk2], Hash2).

	test(sha384_incremental_matches_hash_unaligned_chunks, deterministic(Hash1 == Hash2)) :-
		sequence(1, 260, Bytes),
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

	% auxiliary predicates

	% splits List into Left (the first N elements) and Right (the remainder)
	split_at(N, List, Left, Right) :-
		length(Left, N),
		append(Left, Right, List).

	% splits List into a list of Chunks of (at most) N elements each,
	% used to drive update_hash_state/3 one chunk at a time
	chunks(_, [], []) :-
		!.
	chunks(N, List, [Chunk| Chunks]) :-
		(	length(List, Length),
			Length =< N ->
			Chunk = List,
			Rest = []
		;	split_at(N, List, Chunk, Rest)
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
