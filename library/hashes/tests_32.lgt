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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2026-10-16,
		comment is 'Unit tests for the "hashes" library 32-bit algorithms.'
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, take/4
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(integer, [
		sequence/3
	]).

	cover(djb2_32).
	cover(sdbm_32).
	cover(fnv1a_32).
	cover(crc32_reflected(_)).
	cover(crc32_non_reflected(_,_,_,_)).
	cover(crc32b).
	cover(crc32c).
	cover(crc32posix).
	cover(crc32mpeg2).
	cover(crc32bzip2).
	cover(crc32q).
	cover(murmurhash3_x86_32).
	cover(blake2s).
	cover(blake2s(_, _)).
	cover(md5).
	cover(hash_common_32).
	cover(blake2_core(_, _)).

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

	test(crc32_reflected_parametric_iso_hdlc_empty, deterministic(Hash == '00000000')) :-
		crc32_reflected(0xEDB88320)::hash([], Hash).

	test(crc32_reflected_parametric_iso_hdlc_standard_vector, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		crc32_reflected(0xEDB88320)::hash(Bytes, Hash).

	test(crc32_non_reflected_parametric_mpeg2_empty, deterministic(Hash == 'ffffffff')) :-
		crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0x00000000, false)::hash([], Hash).

	test(crc32_non_reflected_parametric_mpeg2_standard_vector, deterministic(Hash == '0376e6e7')) :-
		atom_codes('123456789', Bytes),
		crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0x00000000, false)::hash(Bytes, Hash).

	test(crc32b_empty, deterministic(Hash == '00000000')) :-
		crc32b::hash([], Hash).

	test(crc32b_standard_vector, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		crc32b::hash(Bytes, Hash).

	test(crc32c_empty, deterministic(Hash == '00000000')) :-
		crc32c::hash([], Hash).

	test(crc32c_standard_vector, deterministic(Hash == 'e3069283')) :-
		atom_codes('123456789', Bytes),
		crc32c::hash(Bytes, Hash).

	test(crc32posix_empty, deterministic(Hash == 'ffffffff')) :-
		crc32posix::hash([], Hash).

	test(crc32posix_standard_vector, deterministic(Hash == '377a6011')) :-
		atom_codes('123456789', Bytes),
		crc32posix::hash(Bytes, Hash).

	test(crc32mpeg2_empty, deterministic(Hash == 'ffffffff')) :-
		crc32mpeg2::hash([], Hash).

	test(crc32mpeg2_standard_vector, deterministic(Hash == '0376e6e7')) :-
		atom_codes('123456789', Bytes),
		crc32mpeg2::hash(Bytes, Hash).

	test(crc32bzip2_empty, deterministic(Hash == '00000000')) :-
		crc32bzip2::hash([], Hash).

	test(crc32bzip2_standard_vector, deterministic(Hash == 'fc891918')) :-
		atom_codes('123456789', Bytes),
		crc32bzip2::hash(Bytes, Hash).

	test(crc32q_empty, deterministic(Hash == '00000000')) :-
		crc32q::hash([], Hash).

	test(crc32q_standard_vector, deterministic(Hash == '3010bf7f')) :-
		atom_codes('123456789', Bytes),
		crc32q::hash(Bytes, Hash).

	test(murmurhash3_x86_32_empty, deterministic(Hash == '00000000')) :-
		murmurhash3_x86_32::hash([], Hash).

	test(blake2s_empty, deterministic(Hash == '69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9')) :-
		blake2s::hash([], Hash).

	test(blake2s_abc, deterministic(Hash == '508c5e8c327c14e2e1a72ba34eeb452f37458b209ed63a294d999b4c86675982')) :-
		atom_codes('abc', Bytes),
		blake2s::hash(Bytes, Hash).

	test(blake2s_hash_digest_protocol, deterministic(Info == info('69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9', 32, 64))) :-
		blake2s::digest([], Digest),
		bytes_hex(Digest, Hex),
		blake2s::digest_size(DigestSize),
		blake2s::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(blake2s_parametric_empty_16, deterministic(Hash == '64550d6ffe2c0a01a14aba1eade0200c')) :-
		blake2s([], 16)::hash([], Hash).

	test(blake2s_parametric_abc_16, deterministic(Hash == 'aa4938119b1dc7b87cbad0ffd200d0ae')) :-
		atom_codes('abc', Bytes),
		blake2s([], 16)::hash(Bytes, Hash).

	test(blake2s_parametric_digest_size_1, deterministic(Hash == 'a1')) :-
		blake2s([], 1)::hash([], Hash).

	test(blake2s_parametric_digest_size_31, deterministic(Hash == '1f57c56334f1ba2d62275430fdc2d2301017ba6be19864dac5a5eca012da4d')) :-
		blake2s([], 31)::hash([], Hash).

	test(blake2s_parametric_keyed_empty_16, deterministic(Hash == '01d7f4cea32638032b156c74aec7e87a')) :-
		blake2s([0], 16)::hash([], Hash).

	test(blake2s_parametric_keyed_abc_16, deterministic(Hash == 'b0b50abcc8035f261e99bc69470f14d3')) :-
		atom_codes('abc', Bytes),
		blake2s([0], 16)::hash(Bytes, Hash).

	test(blake2s_parametric_max_key_empty, deterministic(Hash == '48a8997da407876b3d79c0d92325ad3b89cbb754d86ab71aee047ad345fd2c49')) :-
		sequence(0, 31, Key),
		blake2s(Key, 32)::hash([], Hash).

	test(blake2s_parametric_hash_digest_protocol, deterministic(Info == info('64550d6ffe2c0a01a14aba1eade0200c', 16, 64))) :-
		blake2s([], 16)::digest([], Digest),
		bytes_hex(Digest, Hex),
		assertion(Hex \== '69217a3079908094e11121d042354a7c1f'),
		blake2s([], 16)::digest_size(DigestSize),
		blake2s([], 16)::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	test(md5_empty, deterministic(Hash == 'd41d8cd98f00b204e9800998ecf8427e')) :-
		md5::hash([], Hash).

	test(md5_quick_brown_fox, deterministic(Hash == '9e107d9d372bb6826bd81d3542a419d6')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		md5::hash(Bytes, Hash).

	test(md5_hash_digest_protocol, deterministic(Info == info('d41d8cd98f00b204e9800998ecf8427e', 16, 64))) :-
		md5::digest([], Digest),
		bytes_hex(Digest, Hex),
		md5::digest_size(DigestSize),
		md5::block_size(BlockSize),
		Info = info(Hex, DigestSize, BlockSize).

	% incremental hashing tests (new_hash_state/1, update_hash_state/3, final_hash_state/2)

	test(djb2_32_incremental_empty, deterministic(Hash == '00001505')) :-
		run_incremental(djb2_32, [], Hash).

	test(djb2_32_incremental_single_chunk, deterministic(Hash == '0002b606')) :-
		run_incremental(djb2_32, [[0'a]], Hash).

	test(djb2_32_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		djb2_32::hash(Bytes, Hash1),
		chunks(7, Bytes, Chunks),
		run_incremental(djb2_32, Chunks, Hash2).

	test(sdbm_32_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		sdbm_32::hash(Bytes, Hash1),
		chunks(5, Bytes, Chunks),
		run_incremental(sdbm_32, Chunks, Hash2).

	test(fnv1a_32_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		fnv1a_32::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(fnv1a_32, Chunks, Hash2).

	test(crc32b_incremental_empty, deterministic(Hash == '00000000')) :-
		run_incremental(crc32b, [], Hash).

	test(crc32b_incremental_standard_vector, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		take(4, Bytes, Chunk1, Chunk2),
		run_incremental(crc32b, [Chunk1, Chunk2], Hash).

	test(crc32b_incremental_empty_chunks_are_no_ops, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		take(4, Bytes, Chunk1, Chunk2),
		run_incremental(crc32b, [[], Chunk1, [], Chunk2, []], Hash).

	test(crc32posix_incremental_standard_vector, deterministic(Hash == '377a6011')) :-
		atom_codes('123456789', Bytes),
		chunks(2, Bytes, Chunks),
		run_incremental(crc32posix, Chunks, Hash).

	test(crc32posix_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		crc32posix::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(crc32posix, Chunks, Hash2).

	test(murmurhash3_x86_32_incremental_empty, deterministic(Hash == '00000000')) :-
		run_incremental(murmurhash3_x86_32, [], Hash).

	% byte-by-byte chunking exercises the worst case for the internal
	% leftover buffer, which never accumulates more than 3 bytes at a time
	test(murmurhash3_x86_32_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x86_32::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(murmurhash3_x86_32, Chunks, Hash2).

	% chunks deliberately not aligned on 4-byte boundaries
	test(murmurhash3_x86_32_incremental_matches_hash_unaligned, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		murmurhash3_x86_32::hash(Bytes, Hash1),
		chunks(3, Bytes, Chunks),
		run_incremental(murmurhash3_x86_32, Chunks, Hash2).

	test(blake2s_incremental_empty, deterministic(Hash == '69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9')) :-
		run_incremental(blake2s, [], Hash).

	test(blake2s_incremental_matches_hash_single_chunk, deterministic(Hash1 == Hash2)) :-
		atom_codes('abc', Bytes),
		blake2s::hash(Bytes, Hash1),
		run_incremental(blake2s, [Bytes], Hash2).

	% exact block boundary: two chunks whose lengths add up to exactly one
	% 64-byte BLAKE2s block; unlike MD5/SHA, this must still be compressed
	% as the final block (Final = true), which blake2s_consume_full_blocks/6
	% only does at final_hash_state/2, never eagerly inside update_hash_state/3
	test(blake2s_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		blake2s::hash(Bytes, Hash1),
		take(30, Bytes, Chunk1, Chunk2),
		run_incremental(blake2s, [Chunk1, Chunk2], Hash2).

	test(blake2s_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		blake2s::hash(Bytes, Hash1),
		take(64, Bytes, Chunk1, Chunk2),
		run_incremental(blake2s, [Chunk1, Chunk2], Hash2).

	test(blake2s_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		blake2s::hash(Bytes, Hash1),
		take(40, Bytes, Chunk1, Chunk2),
		run_incremental(blake2s, [Chunk1, Chunk2], Hash2).

	% byte-by-byte chunking across several blocks: at every single-byte step
	% the buffer could hold exactly one full 64-byte block, which must not
	% be eagerly compressed as final, since more bytes keep arriving
	test(blake2s_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		sequence(1, 130, Bytes),
		blake2s::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(blake2s, Chunks, Hash2).

	% BLAKE2-specific: the compression of a block differs depending on
	% whether it is flagged final or not, so a message that is exactly one
	% block long must hash differently from the same bytes followed by more
	% data, even though the first block's raw bytes are identical in both
	% cases; this pins down that update_hash_state/3 never compresses a
	% held-back block until final_hash_state/2 confirms it really is final
	test(blake2s_incremental_final_block_is_not_compressed_early, deterministic(Hash64 \== Hash65)) :-
		sequence(1, 64, Bytes64),
		blake2s::hash(Bytes64, Hash64),
		run_incremental(blake2s, [Bytes64, [65]], Hash65).

	test(blake2s_parametric_keyed_incremental_empty, deterministic(Hash == '01d7f4cea32638032b156c74aec7e87a')) :-
		run_incremental(blake2s([0], 16), [], Hash).

	test(blake2s_parametric_keyed_incremental_matches_hash, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		blake2s([0], 16)::hash(Bytes, Hash1),
		chunks(7, Bytes, Chunks),
		run_incremental(blake2s([0], 16), Chunks, Hash2).

	test(md5_incremental_empty, deterministic(Hash == 'd41d8cd98f00b204e9800998ecf8427e')) :-
		run_incremental(md5, [], Hash).

	test(md5_incremental_matches_hash_single_chunk, deterministic(Hash1 == Hash2)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
		md5::hash(Bytes, Hash1),
		run_incremental(md5, [Bytes], Hash2).

	% exact block boundary: two chunks whose lengths add up to exactly one
	% 64-byte MD5 block, forcing md5_consume_blocks/9 to consume all of the
	% combined buffer and leave an empty leftover tail
	test(md5_incremental_matches_hash_exact_block_boundary, deterministic(Hash1 == Hash2)) :-
		sequence(1, 64, Bytes),
		md5::hash(Bytes, Hash1),
		take(30, Bytes, Chunk1, Chunk2),
		run_incremental(md5, [Chunk1, Chunk2], Hash2).

	% one byte past an exact block boundary: the leftover buffer after the
	% first full block consumed is exactly 1 byte long
	test(md5_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		md5::hash(Bytes, Hash1),
		take(64, Bytes, Chunk1, Chunk2),
		run_incremental(md5, [Chunk1, Chunk2], Hash2).

	% one byte short of an exact block boundary
	test(md5_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		md5::hash(Bytes, Hash1),
		take(40, Bytes, Chunk1, Chunk2),
		run_incremental(md5, [Chunk1, Chunk2], Hash2).

	% byte-by-byte chunking across several blocks, exercising the buffer
	% and block-consuming logic on every possible offset within a block
	test(md5_incremental_matches_hash_byte_by_byte, deterministic(Hash1 == Hash2)) :-
		sequence(1, 130, Bytes),
		md5::hash(Bytes, Hash1),
		chunks(1, Bytes, Chunks),
		run_incremental(md5, Chunks, Hash2).

	% the state must not be tied to a single new_hash_state/1 call: two
	% independent hashing sessions interleaved through the same predicates
	% must not interfere with each other
	test(md5_incremental_independent_sessions, deterministic(HashA-HashB == ExpectedA-ExpectedB)) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes1),
		atom_codes('Pack my box with five dozen liquor jugs', Bytes2),
		md5::hash(Bytes1, ExpectedA),
		md5::hash(Bytes2, ExpectedB),
		md5::new_hash_state(StateA0),
		md5::new_hash_state(StateB0),
		md5::update_hash_state(StateA0, Bytes1, StateA1),
		md5::update_hash_state(StateB0, Bytes2, StateB1),
		md5::final_hash_state(StateA1, HashA),
		md5::final_hash_state(StateB1, HashB).

	test(hashes_blake2s_parameter_errors, deterministic) :-
		check_blake2_errors(blake2s, 32, 32).

	test(hashes_murmurhash3_x86_32_one_byte_tail, deterministic(Hash == '3c2569b2')) :-
		murmurhash3_x86_32::hash([0'a], Hash).

	test(hashes_murmurhash3_x86_32_two_byte_tail, deterministic(Hash == '9bbfd75f')) :-
		murmurhash3_x86_32::hash([0'a, 0'b], Hash).

	test(hashes_common_32_mask, deterministic(Mask == 0xFFFFFFFF)) :-
		hash_common_32::mask32(Mask).

	test(hashes_common_32_three_addends, deterministic(Sum == 2)) :-
		hash_common_32::add32(0xFFFFFFFF, 1, 2, Sum).

	test(hashes_32_independent_boundaries, deterministic) :-
		findall(Object-Count-Expected, boundary_vector(Object, Count, Expected), Cases),
		Cases = [_| _],
		forall(member(Object-Count-Expected, Cases), check_boundary(Object, Count, Expected)).

	test(hashes_crc_posix_length_rollover, all(check_boundary(crc32posix, Count, Expected))) :-
		member(Count-Expected, [0-'ffffffff', 255-'53eb78da', 256-'4e4dc3a1']).

	test(hashes_32_chunks_boundaries, deterministic(Results == [[], [[0], [1], [2]], [[0, 1], [2, 3]], [[0, 1], [2]]])) :-
		chunks(7, [], Empty),
		chunks(1, [0, 1, 2], Singletons),
		chunks(2, [0, 1, 2, 3], Exact),
		chunks(2, [0, 1, 2], Partial),
		Results = [Empty, Singletons, Exact, Partial].

	test(hashes_32_interleaved_families, all(check_sessions(Object))) :-
		member(Object, [md5, blake2s, crc32b, crc32posix, murmurhash3_x86_32]).

	test(hashes_common_32_arithmetic_edges, deterministic) :-
		hash_common_32::bytes_hex([], Empty),
		assertion(Empty == ''),
		hash_common_32::bytes_hex([0, 1, 35, 69, 103, 137, 171, 205, 239], Hex),
		assertion(Hex == '000123456789abcdef'),
		hash_common_32::mul32(0xFFFFFFFF, 0xFFFFFFFF, Product),
		assertion(Product == 1),
		hash_common_32::add32(0xFFFFFFFF, 1, Sum),
		assertion(Sum == 0),
		hash_common_32::add32(0xFFFFFFFF, 1, 0xFFFFFFFF, 1, Sum4),
		assertion(Sum4 == 0).

	test(hashes_common_32_rotation_counts, deterministic) :-
		sequence(0, 32, Counts),
		forall(member(Count, Counts), check_rotation32(Count)).

	test(hashes_common_32_padding_boundaries, all(check_padding32(Count, Blocks))) :-
		member(Count-Blocks, [55-1, 56-2, 63-2, 64-2, 65-2]).

	test(hashes_crc_generic_polynomials, deterministic) :-
		check_boundary(crc32_reflected(0xEB31D82E), 65, '25090a59'),
		check_boundary(crc32_non_reflected(0x1EDC6F41, 0xFFFFFFFF, 0xFFFFFFFF, false), 65, 'e3bdc867').

	test(hashes_crc_reflected_tables, deterministic) :-
		sequence(0, 255, Indices),
		forall(
			member(Polynomial, [0xEDB88320, 0x82F63B78]),
			forall(member(Index, Indices), check_crc_reflected_entry(Polynomial, Index))
		).

	test(hashes_crc_non_reflected_tables, deterministic) :-
		sequence(0, 255, Indices),
		forall(
			member(Polynomial, [0x04C11DB7, 0x814141AB]),
			forall(member(Index, Indices), check_crc_non_reflected_entry(Polynomial, Index))
		).

	% auxiliary predicates

	check_crc_non_reflected_entry(Polynomial, Index) :-
		crc32_non_reflected(Polynomial, 0, 0, false)<<step_byte(0, Index, Actual),
		Start is Index << 24,
		crc32_non_reflected(Polynomial, 0, 0, false)<<step_bits(8, Start, Expected),
		assertion(Actual == Expected).

	check_crc_reflected_entry(Polynomial, Index) :-
		crc32_reflected(Polynomial)<<crc32_table_value(Index, Actual),
		crc32_reflected(Polynomial)<<crc32_table_value(8, Index, Expected),
		assertion(Actual == Expected).

	check_rotation32(Count) :-
		Shift is Count mod 32,
		Expected is 1 << Shift,
		hash_common_32::rol32(1, Count, Rotated),
		assertion(Rotated == Expected),
		hash_common_32::ror32(Rotated, Count, Restored),
		assertion(Restored == 1).

	check_padding32(Count, Blocks) :-
		byte_sequence(Count, 0, Bytes),
		Low is (Count * 8) mod 256,
		High is (Count * 8) // 256,
		ExpectedLength is Blocks * 64,
		forall(
			member(Endian-Field, [little-[Low, High, 0, 0, 0, 0, 0, 0], big-[0, 0, 0, 0, 0, 0, High, Low]]),
			check_padding32(Endian, Bytes, Count, ExpectedLength, Field)
		).

	check_padding32(Endian, Bytes, Count, ExpectedLength, Field) :-
		hash_common_32::pad_md(Endian, Bytes, 8, Padded),
		length(Padded, ExpectedLength),
		append(Bytes, [128| _], Padded),
		append(_, Field, Padded),
		TailLength is Count mod 64,
		PrefixLength is Count - TailLength,
		take(PrefixLength, Bytes, Prefix, Tail),
		hash_common_32::pad_md_tail(Endian, Tail, Count, 8, PaddedTail),
		append(Prefix, PaddedTail, Padded),
		!.

	check_sessions(Object) :-
		Object::hash([0, 1, 2], ExpectedA),
		Object::hash([3, 4, 5], ExpectedB),
		Object::new_hash_state(StateA0),
		Object::new_hash_state(StateB0),
		Object::update_hash_state(StateA0, [0], StateA1),
		Object::update_hash_state(StateB0, [3, 4], StateB1),
		Object::update_hash_state(StateA1, [], StateA2),
		Object::update_hash_state(StateB1, [5], StateB2),
		Object::update_hash_state(StateA2, [1, 2], StateA3),
		Object::final_hash_state(StateB2, HashB),
		Object::final_hash_state(StateA3, HashA),
		assertion(HashA == ExpectedA),
		assertion(HashB == ExpectedB).

	check_boundary(Object, Count, Expected) :-
		byte_sequence(Count, 0, Bytes),
		Object::hash(Bytes, Hash),
		assertion(Hash == Expected),
		chunks(7, Bytes, Chunks),
		run_incremental(Object, [[], []| Chunks], Incremental),
		assertion(Incremental == Expected).

	byte_sequence(0, _, []) :-
		!.
	byte_sequence(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		NextByte is (Byte + 1) mod 256,
		byte_sequence(NextCount, NextByte, Bytes).

	boundary_vector(md5, 55, '6912ee65fff2d9f9ce2508cddf8bcda0').
	boundary_vector(murmurhash3_x86_32, 0, '00000000').
	boundary_vector(murmurhash3_x86_32, 1, '514e28b7').
	boundary_vector(murmurhash3_x86_32, 2, '70e1a2c0').
	boundary_vector(murmurhash3_x86_32, 3, '51d4d0d7').
	boundary_vector(murmurhash3_x86_32, 4, 'f4c0ec39').
	boundary_vector(murmurhash3_x86_32, 5, 'cca4dccb').
	boundary_vector(murmurhash3_x86_32, 6, '901cc302').
	boundary_vector(murmurhash3_x86_32, 7, '8d7e4914').
	boundary_vector(md5, 56, '51fdd1acda72405dfdfa03fcb85896d7').
	boundary_vector(md5, 63, '48a6295221902e8e0938f773a7185e72').
	boundary_vector(md5, 64, 'b2d3f56bc197fd985d5965079b5e7148').
	boundary_vector(md5, 65, '8bd7053801c768420faf816fadba971c').
	boundary_vector(blake2s, 63, 'e57cb79487dd57902432b250733813bd96a84efce59f650fac26e6696aefafc3').
	boundary_vector(blake2s, 64, '56f34e8b96557e90c1f24b52d0c89d51086acf1b00f634cf1dde9233b8eaaa3e').
	boundary_vector(blake2s, 65, '1b53ee94aaf34e4b159d48de352c7f0661d0a40edff95a0b1639b4090e974472').
	boundary_vector(blake2s, 128, '1fa877de67259d19863a2a34bcc6962a2b25fcbf5cbecd7ede8f1fa36688a796').
	boundary_vector(blake2s, 129, '5bd169e67c82c2c2e98ef7008bdf261f2ddf30b1c00f9e7f275bb3e8a28dc9a2').
	boundary_vector(blake2s([0], 16), 63, '0d17d154c1c5854e73326d928a1105ce').
	boundary_vector(blake2s([0], 16), 64, '2ba2b6488a75dba441264fb7e2aec115').
	boundary_vector(blake2s([0], 16), 65, '0fa18e1c3172182230710a4d6bc7b7c4').
	boundary_vector(blake2s([0], 16), 128, '7a98059d4f84767322a894e040e8b284').
	boundary_vector(blake2s([0], 16), 129, 'ff5ac60aeb64b7ff754bb16af1543f07').

	check_blake2_errors(Variant, MaxKey, MaxSize) :-
		check_blake2_error(Variant, [0| Tail], MaxSize, instantiation_error),
		assertion(var(Tail)),
		check_blake2_error(Variant, [Byte], MaxSize, instantiation_error),
		assertion(var(Byte)),
		check_blake2_error(Variant, Key, MaxSize, instantiation_error),
		assertion(var(Key)),
		check_blake2_error(Variant, [], Size, instantiation_error),
		assertion(var(Size)),
		TooMany is MaxKey + 1,
		zero_bytes(TooMany, LongKey),
		forall(
			member(BadKey, [invalid, [0| invalid], [-1], [256], [1.5], [byte], LongKey]),
			check_blake2_error(Variant, BadKey, MaxSize, domain_error(blake2_key, BadKey))
		),
		TooLarge is MaxSize + 1,
		forall(
			member(BadSize, [0, -1, TooLarge, 1.5, invalid]),
			check_blake2_error(Variant, [], BadSize, domain_error(blake2_digest_size, BadSize))
		).

	check_blake2_error(Variant, Key, Size, Expected) :-
		Object =.. [Variant, Key, Size],
		Valid =.. [Variant, [], 1],
		Valid::new_hash_state(State),
		forall(
			member(Goal, [hash([], _), digest([], _), new_hash_state(_), digest_size(_), block_size(_), final_hash_state(State, _)]),
			check_error(Object, Goal, Expected)
		).

	check_error(Object, Goal, Expected) :-
		catch(Object::Goal, Error, true),
		nonvar(Error),
		Error = error(Formal, _),
		assertion(lgtunit::variant(Formal, Expected)).

	zero_bytes(0, []) :-
		!.
	zero_bytes(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zero_bytes(NextCount, Bytes).

	% splits List into a list of Chunks of (at most) N elements each,
	% used to drive update_hash_state/3 one chunk at a time
	chunks(_, [], []) :-
		!.
	chunks(N, List, [Chunk| Chunks]) :-
		(	take(N, List, Chunk, Rest) ->
			true
		;	Chunk = List,
			Rest = []
		),
		chunks(N, Rest, Chunks).

	% drives Object's new_hash_state/1, update_hash_state/3, and
	% final_hash_state/2 over a list of chunks, simulating a caller feeding
	% the message to the hash function one chunk at a time
	run_incremental(Object, Chunks, Hash) :-
		Object::new_hash_state(State0),
		fold_update(Chunks, Object, State0, State),
		Object::final_hash_state(State, Hash).

	fold_update([], _, State, State).
	fold_update([Chunk| Chunks], Object, State0, State) :-
		Object::update_hash_state(State0, Chunk, State1),
		fold_update(Chunks, Object, State1, State).

:- end_object.
