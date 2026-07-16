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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-15,
		comment is 'Unit tests for the "hashes" library 32-bit algorithms.'
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
		split_at(4, Bytes, Chunk1, Chunk2),
		run_incremental(crc32b, [Chunk1, Chunk2], Hash).

	test(crc32b_incremental_empty_chunks_are_no_ops, deterministic(Hash == 'cbf43926')) :-
		atom_codes('123456789', Bytes),
		split_at(4, Bytes, Chunk1, Chunk2),
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
		split_at(30, Bytes, Chunk1, Chunk2),
		run_incremental(blake2s, [Chunk1, Chunk2], Hash2).

	test(blake2s_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		blake2s::hash(Bytes, Hash1),
		split_at(64, Bytes, Chunk1, Chunk2),
		run_incremental(blake2s, [Chunk1, Chunk2], Hash2).

	test(blake2s_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		blake2s::hash(Bytes, Hash1),
		split_at(40, Bytes, Chunk1, Chunk2),
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
		split_at(30, Bytes, Chunk1, Chunk2),
		run_incremental(md5, [Chunk1, Chunk2], Hash2).

	% one byte past an exact block boundary: the leftover buffer after the
	% first full block consumed is exactly 1 byte long
	test(md5_incremental_matches_hash_block_boundary_plus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 65, Bytes),
		md5::hash(Bytes, Hash1),
		split_at(64, Bytes, Chunk1, Chunk2),
		run_incremental(md5, [Chunk1, Chunk2], Hash2).

	% one byte short of an exact block boundary
	test(md5_incremental_matches_hash_block_boundary_minus_one, deterministic(Hash1 == Hash2)) :-
		sequence(1, 63, Bytes),
		md5::hash(Bytes, Hash1),
		split_at(40, Bytes, Chunk1, Chunk2),
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
