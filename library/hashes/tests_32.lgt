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

	test(blake2s_quick_brown_fox, deterministic(Hash == '606beeec743ccbeff6cbcdf5d5302aa855c256c29b88c8ed331ea1a6bf3c8812')) :-
		atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
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

:- end_object.
