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


:- object(djb2_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'DJB2 32-bit hash function.',
		see_also is [djb2_64, sdbm_32, fnv1a_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		djb2_32(Bytes, 5381, Value),
		word32_hex(Value, Hash).

	new_hash_state(5381).

	update_hash_state(Acc0, Bytes, Acc) :-
		djb2_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	djb2_32([], Acc, Acc).
	djb2_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is ((Acc0 << 5) + Acc0 + Byte) /\ 0xFFFFFFFF,
		djb2_32(Bytes, Acc1, Acc).

:- end_object.


:- object(sdbm_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'sdbm 32-bit hash function.',
		see_also is [sdbm_64, djb2_32, fnv1a_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		sdbm_32(Bytes, 0, Value),
		word32_hex(Value, Hash).

	new_hash_state(0).

	update_hash_state(Acc0, Bytes, Acc) :-
		sdbm_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	sdbm_32([], Acc, Acc).
	sdbm_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is (Byte + (Acc0 << 6) + (Acc0 << 16) - Acc0) /\ 0xFFFFFFFF,
		sdbm_32(Bytes, Acc1, Acc).

:- end_object.


:- object(fnv1a_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'FNV-1a 32-bit hash function.',
		see_also is [fnv1a_64, djb2_32, sdbm_32]
	]).

	:- uses(hash_common_32, [
		word32_hex/2, mul32/3
	]).

	hash(Bytes, Hash) :-
		fnv1a_32(Bytes, 0x811C9DC5, Value),
		word32_hex(Value, Hash).

	new_hash_state(0x811C9DC5).

	update_hash_state(Acc0, Bytes, Acc) :-
		fnv1a_32(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		word32_hex(Acc, Hash).

	fnv1a_32([], Acc, Acc).
	fnv1a_32([Byte| Bytes], Acc0, Acc) :-
		Acc1 is xor(Acc0, Byte),
		mul32(Acc1, 0x01000193, Acc2),
		fnv1a_32(Bytes, Acc2, Acc).

:- end_object.


:- object(crc32_reflected(_Polynomial_),
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'Parametric reflected CRC-32 hash function using initial value 0xFFFFFFFF and final xor 0xFFFFFFFF.',
		parameters is [
			'Polynomial' - 'Reflected CRC-32 polynomial.'
		],
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32b, crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q,
			murmurhash3_x86_32, fnv1a_32
		]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	hash(Bytes, Hash) :-
		crc32_reflected(Bytes, CRC32),
		word32_hex(CRC32, Hash).

	new_hash_state(0xFFFFFFFF).

	update_hash_state(Acc0, Bytes, Acc) :-
		crc32_acc(Bytes, Acc0, Acc).

	final_hash_state(Acc, Hash) :-
		CRC32 is xor(Acc, 0xFFFFFFFF) /\ 0xFFFFFFFF,
		word32_hex(CRC32, Hash).

	crc32_reflected(Bytes, CRC32) :-
		crc32_acc(Bytes, 0xFFFFFFFF, Acc),
		CRC32 is xor(Acc, 0xFFFFFFFF) /\ 0xFFFFFFFF.

	crc32_acc([], Acc, Acc).
	crc32_acc([Byte| Bytes], Acc0, Acc) :-
		Index is xor(Acc0, Byte) /\ 0xFF,
		crc32_table_value(Index, TableValue),
		Acc1 is xor((Acc0 >> 8), TableValue) /\ 0xFFFFFFFF,
		crc32_acc(Bytes, Acc1, Acc).

	crc32_table_value(Index, Value) :-
		crc32_table_value(8, Index, Value).

	crc32_table_value(0, Value, Value) :-
		!.
	crc32_table_value(Count, Value0, Value) :-
		(	Value0 /\ 1 =:= 1 ->
			Value1 is xor((Value0 >> 1), _Polynomial_)
		;	Value1 is Value0 >> 1
		),
		NextCount is Count - 1,
		crc32_table_value(NextCount, Value1, Value).

:- end_object.


:- object(crc32b,
	extends(crc32_reflected(0xEDB88320))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/ISO-HDLC hash function using the reflected polynomial 0xEDB88320, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_reflected(_), crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q, murmurhash3_x86_32,
			fnv1a_32
		]
	]).

:- end_object.


:- object(crc32c,
	extends(crc32_reflected(0x82F63B78))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32C/Castagnoli hash function using the reflected polynomial 0x82F63B78, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_reflected(_), crc32b, crc32posix, crc32mpeg2, crc32bzip2, crc32q, murmurhash3_x86_32,
			fnv1a_32
		]
	]).

:- end_object.


:- object(crc32_non_reflected(_Polynomial_, _Initial_, _FinalXor_, _AppendLength_),
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'Parametric non-reflected CRC-32 hash function using a canonical polynomial, configurable initial value and final xor value, and optional appended little-endian length bytes.',
		parameters is [
			'Polynomial' - 'Canonical non-reflected CRC-32 polynomial.',
			'Initial' - 'Initial CRC accumulator value.',
			'FinalXor' - 'Final xor value.',
			'AppendLength' - 'Boolean flag controlling whether the message length is appended as little-endian bytes.'
		],
		see_also is [
			crc32_reflected(_), crc32b, crc32c, crc32posix, crc32mpeg2, crc32bzip2, crc32q,
			murmurhash3_x86_32, fnv1a_32
		]
	]).

	:- uses(hash_common_32, [
		word32_hex/2
	]).

	:- uses(list, [
		length/2
	]).

	hash(Bytes, Hash) :-
		crc32_non_reflected(Bytes, CRC32),
		word32_hex(CRC32, Hash),
		!.

	new_hash_state(state(_Initial_, 0)).

	update_hash_state(state(Acc0, Length0), Bytes, state(Acc, Length)) :-
		crc32_non_reflected_acc(Bytes, Acc0, Acc),
		length(Bytes, BytesLength),
		Length is Length0 + BytesLength.

	final_hash_state(state(Acc0, Length), Hash) :-
		(	_AppendLength_ == true ->
			length_bytes(Length, LengthBytes),
			crc32_non_reflected_acc(LengthBytes, Acc0, Acc1)
		;	Acc1 = Acc0
		),
		CRC32 is xor(Acc1, _FinalXor_) /\ 0xFFFFFFFF,
		word32_hex(CRC32, Hash),
		!.

	crc32_non_reflected(Bytes, CRC32) :-
		crc32_non_reflected_acc(Bytes, _Initial_, Acc0),
		(	_AppendLength_ == true ->
			length_bytes(Bytes, LengthBytes),
			crc32_non_reflected_acc(LengthBytes, Acc0, Acc1)
		;	Acc1 = Acc0
		),
		CRC32 is xor(Acc1, _FinalXor_) /\ 0xFFFFFFFF.

	crc32_non_reflected_acc([], Acc, Acc).
	crc32_non_reflected_acc([Byte| Bytes], Acc0, Acc) :-
		step_byte(Acc0, Byte, Acc1),
		crc32_non_reflected_acc(Bytes, Acc1, Acc).

	step_byte(Acc0, Byte, Acc) :-
		Acc1 is xor(Acc0, (Byte /\ 0xFF) << 24) /\ 0xFFFFFFFF,
		step_bits(8, Acc1, Acc).

	step_bits(0, Acc, Acc) :-
		!.
	step_bits(Count, Acc0, Acc) :-
		(	Acc0 /\ 0x80000000 =:= 0x80000000 ->
			Acc1 is xor((Acc0 << 1), _Polynomial_) /\ 0xFFFFFFFF
		;	Acc1 is (Acc0 << 1) /\ 0xFFFFFFFF
		),
		NextCount is Count - 1,
		step_bits(NextCount, Acc1, Acc).

	length_bytes(Bytes, LengthBytes) :-
		length(Bytes, Length),
		length_bytes(Length, LengthBytes).

	length_bytes(0, []) :-
		!.
	length_bytes(Length, [Byte| Bytes]) :-
		Length > 0,
		Byte is Length /\ 0xFF,
		NextLength is Length >> 8,
		length_bytes(NextLength, Bytes).

:- end_object.


:- object(crc32posix,
	extends(crc32_non_reflected(0x04C11DB7, 0x00000000, 0xFFFFFFFF, true))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/POSIX (cksum) hash function using the canonical polynomial 0x04C11DB7, initial value 0x00000000, appended little-endian length bytes, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32_reflected(_), crc32b, crc32c, crc32mpeg2, crc32bzip2,
			crc32q, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32mpeg2,
	extends(crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0x00000000, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/MPEG-2 hash function using the canonical polynomial 0x04C11DB7, initial value 0xFFFFFFFF, and final xor 0x00000000.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32posix, crc32bzip2, crc32q, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32bzip2,
	extends(crc32_non_reflected(0x04C11DB7, 0xFFFFFFFF, 0xFFFFFFFF, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32/BZIP2 hash function using the canonical polynomial 0x04C11DB7, initial value 0xFFFFFFFF, and final xor 0xFFFFFFFF.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32mpeg2, crc32posix, crc32q, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(crc32q,
	extends(crc32_non_reflected(0x814141AB, 0x00000000, 0x00000000, false))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-05,
		comment is 'CRC-32Q hash function, also used by AIXM-style formats, using the canonical polynomial 0x814141AB, initial value 0x00000000, and final xor 0x00000000.',
		see_also is [
			crc32_non_reflected(_,_,_,_), crc32mpeg2, crc32bzip2, crc32posix, crc32_reflected(_), crc32b,
			crc32c, murmurhash3_x86_32, fnv1a_32
		]
	]).

:- end_object.


:- object(murmurhash3_x86_32,
	implements(hash_state_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-15,
		comment is 'MurmurHash3 x86 32-bit hash function with seed 0.',
		see_also is [murmurhash3_x86_128, murmurhash3_x64_128]
	]).

	:- uses(hash_common_32, [
		word32_hex/2, little_endian_word32/2, mul32/3, rol32/3
	]).

	:- uses(list, [
		append/3, length/2
	]).

	hash(Bytes, Hash) :-
		murmurhash3_x86_32(Bytes, 0, Value),
		word32_hex(Value, Hash).

	% the state buffers the at-most-3 leftover bytes that do not yet form a
	% complete 4-byte block; Length is the total number of bytes seen so
	% far, needed only at finalization
	new_hash_state(state([], 0, 0)).

	update_hash_state(state(Buffer0, Length0, H0), Bytes, state(Buffer1, Length1, H1)) :-
		append(Buffer0, Bytes, Combined),
		length(Bytes, BytesLength),
		Length1 is Length0 + BytesLength,
		body(Combined, H0, _, H1, Buffer1).

	final_hash_state(state(Buffer, Length, H1), Hash) :-
		tail(Buffer, K1),
		H2 is xor(H1, K1),
		H3 is xor(H2, Length),
		fmix(H3, Value),
		word32_hex(Value, Hash).

	murmurhash3_x86_32(Bytes, Seed, Hash) :-
		length(Bytes, Length),
		body(Bytes, Seed, Length, H1, Tail),
		tail(Tail, K1),
		H2 is xor(H1, K1),
		H3 is xor(H2, Length),
		fmix(H3, Hash).

	body([B0, B1, B2, B3| Bytes], H0, Length, H, Tail) :-
		!,
		little_endian_word32([B0, B1, B2, B3], K0),
		block(K0, H0, H1),
		body(Bytes, H1, Length, H, Tail).
	body(Tail, H, _, H, Tail).

	block(K0, H0, H) :-
		mul32(K0, 0xCC9E2D51, K1),
		rol32(K1, 15, K2),
		mul32(K2, 0x1B873593, K3),
		H1 is xor(H0, K3),
		rol32(H1, 13, H2),
		mul32(H2, 5, H3),
		H is (H3 + 0xE6546B64) /\ 0xFFFFFFFF.

	tail([], 0).
	tail([B0], K1) :-
		K0 is B0,
		tail_mix(K0, K1).
	tail([B0, B1], K1) :-
		K0 is B0 \/ (B1 << 8),
		tail_mix(K0, K1).
	tail([B0, B1, B2], K1) :-
		K0 is B0 \/ (B1 << 8) \/ (B2 << 16),
		tail_mix(K0, K1).

	tail_mix(K0, K1) :-
		mul32(K0, 0xCC9E2D51, K2),
		rol32(K2, 15, K3),
		mul32(K3, 0x1B873593, K1).

	fmix(H0, H) :-
		H1 is xor(H0, H0 >> 16),
		mul32(H1, 0x85EBCA6B, H2),
		H3 is xor(H2, H2 >> 13),
		mul32(H3, 0xC2B2AE35, H4),
		H is xor(H4, H4 >> 16) /\ 0xFFFFFFFF.

:- end_object.


:- object(blake2s,
	implements([hash_digest_protocol, hash_state_protocol])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'BLAKE2s hash function.',
		see_also is [blake2b, md5, sha256]
	]).

	:- uses(hash_common_32, [
		add32/3, bytes_hex/2, integer_to_little_endian_bytes32/3, little_endian_word32/2, ror32/3
	]).

	:- uses(list, [
		append/3, length/2, nth0/3
	]).

	digest(Bytes, DigestBytes) :-
		blake2s_initial_state(State0),
		blake2s_blocks(Bytes, 0, State0, State),
		blake2s_state_bytes(State, DigestBytes).

	digest_size(32).

	block_size(64).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	% unlike the MD-style hashes, BLAKE2's compression function is given an
	% explicit flag marking whether the block being compressed is the last
	% one, and that flag changes the digest, not just an appended length
	% field; the state must therefore always hold back a full block (up to
	% 64 bytes) that has not yet been compressed, since a block that
	% currently looks complete may still turn out not to be the last one
	% once more bytes arrive in a later update_hash_state/3 call. Only at
	% final_hash_state/2, when no more bytes can arrive, is the held-back
	% buffer known to be the final block
	new_hash_state(state([], 0, InitialState)) :-
		blake2s_initial_state(InitialState).

	update_hash_state(state(Buffer0, Total0, State0), Bytes, state(Buffer1, Total1, State1)) :-
		append(Buffer0, Bytes, Combined),
		blake2s_consume_full_blocks(Combined, Total0, State0, Buffer1, Total1, State1).

	final_hash_state(state(Buffer, Total0, State0), Hash) :-
		length(Buffer, BlockLength),
		Total is Total0 + BlockLength,
		pad_block(Buffer, 64, Block),
		blake2s_compress(State0, Block, Total, true, State),
		blake2s_state_bytes(State, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	% consumes complete, definitely-not-final 64-byte blocks from Buffer,
	% leaving between 1 and 64 bytes (or 0, only if Buffer started empty
	% and stays empty) as the new held-back buffer
	blake2s_consume_full_blocks(Buffer, Total, State, Buffer, Total, State) :-
		length(Buffer, Length),
		Length =< 64,
		!.
	blake2s_consume_full_blocks(Buffer, Total0, State0, FinalBuffer, Total, State) :-
		take_up_to(64, Buffer, Block, Rest),
		Total1 is Total0 + 64,
		blake2s_compress(State0, Block, Total1, false, State1),
		blake2s_consume_full_blocks(Rest, Total1, State1, FinalBuffer, Total, State).

	blake2s_initial_state([H0, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19]) :-
		H0 is xor(0x6A09E667, 0x01010020) /\ 0xFFFFFFFF.

	blake2s_blocks(Bytes, Total0, State0, State) :-
		take_up_to(64, Bytes, BlockBytes, Rest),
		length(BlockBytes, BlockLength),
		Total is Total0 + BlockLength,
		pad_block(BlockBytes, 64, Block),
		(	Rest == [] ->
			blake2s_compress(State0, Block, Total, true, State)
		;	blake2s_compress(State0, Block, Total, false, State1),
			blake2s_blocks(Rest, Total, State1, State)
		).

	blake2s_compress(State0, Block, Total, Final, State) :-
		blake2s_block_words(Block, MessageWords),
		blake2s_working_vector(State0, Total, Final, Working0),
		blake2s_rounds(0, MessageWords, Working0, Working),
		blake2s_finalize(State0, Working, 0, State).

	blake2s_working_vector([H0, H1, H2, H3, H4, H5, H6, H7], Total, Final, [H0, H1, H2, H3, H4, H5, H6, H7, 0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, V12, V13, V14, 0x5BE0CD19]) :-
		TotalLow is Total /\ 0xFFFFFFFF,
		TotalHigh is (Total >> 32) /\ 0xFFFFFFFF,
		V12 is xor(0x510E527F, TotalLow) /\ 0xFFFFFFFF,
		V13 is xor(0x9B05688C, TotalHigh) /\ 0xFFFFFFFF,
		(	Final == true -> FinalFlag = 0xFFFFFFFF
		;	FinalFlag = 0x00000000
		),
		V14 is xor(0x1F83D9AB, FinalFlag) /\ 0xFFFFFFFF.

	blake2s_rounds(10, _, Working, Working) :-
		!.
	blake2s_rounds(Round, MessageWords, Working0, Working) :-
		blake2s_sigma(Round, S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		nth0(S0, MessageWords, M0),
		nth0(S1, MessageWords, M1),
		nth0(S2, MessageWords, M2),
		nth0(S3, MessageWords, M3),
		nth0(S4, MessageWords, M4),
		nth0(S5, MessageWords, M5),
		nth0(S6, MessageWords, M6),
		nth0(S7, MessageWords, M7),
		nth0(S8, MessageWords, M8),
		nth0(S9, MessageWords, M9),
		nth0(S10, MessageWords, M10),
		nth0(S11, MessageWords, M11),
		nth0(S12, MessageWords, M12),
		nth0(S13, MessageWords, M13),
		nth0(S14, MessageWords, M14),
		nth0(S15, MessageWords, M15),
		blake2s_g(Working0, 0, 4, 8, 12, M0, M1, Working1),
		blake2s_g(Working1, 1, 5, 9, 13, M2, M3, Working2),
		blake2s_g(Working2, 2, 6, 10, 14, M4, M5, Working3),
		blake2s_g(Working3, 3, 7, 11, 15, M6, M7, Working4),
		blake2s_g(Working4, 0, 5, 10, 15, M8, M9, Working5),
		blake2s_g(Working5, 1, 6, 11, 12, M10, M11, Working6),
		blake2s_g(Working6, 2, 7, 8, 13, M12, M13, Working7),
		blake2s_g(Working7, 3, 4, 9, 14, M14, M15, Working8),
		NextRound is Round + 1,
		blake2s_rounds(NextRound, MessageWords, Working8, Working).

	blake2s_g(Working0, AIndex, BIndex, CIndex, DIndex, X, Y, Working) :-
		nth0(AIndex, Working0, A0),
		nth0(BIndex, Working0, B0),
		nth0(CIndex, Working0, C0),
		nth0(DIndex, Working0, D0),
		add32(A0, B0, T0),
		add32(T0, X, A1),
		D1 is xor(D0, A1) /\ 0xFFFFFFFF,
		ror32(D1, 16, D2),
		add32(C0, D2, C1),
		B1 is xor(B0, C1) /\ 0xFFFFFFFF,
		ror32(B1, 12, B2),
		add32(A1, B2, T1),
		add32(T1, Y, A2),
		D3 is xor(D2, A2) /\ 0xFFFFFFFF,
		ror32(D3, 8, D4),
		add32(C1, D4, C2),
		B3 is xor(B2, C2) /\ 0xFFFFFFFF,
		ror32(B3, 7, B4),
		replace_nth0(AIndex, Working0, A2, Working1),
		replace_nth0(BIndex, Working1, B4, Working2),
		replace_nth0(CIndex, Working2, C2, Working3),
		replace_nth0(DIndex, Working3, D4, Working).

	blake2s_finalize([], _, _, []).
	blake2s_finalize([HashWord| HashWords], Working, Index0, [StateWord| StateWords]) :-
		nth0(Index0, Working, Word0),
		Index8 is Index0 + 8,
		nth0(Index8, Working, Word8),
		StateWord is xor(HashWord, xor(Word0, Word8)) /\ 0xFFFFFFFF,
		Index is Index0 + 1,
		blake2s_finalize(HashWords, Working, Index, StateWords).

	blake2s_state_bytes(State, DigestBytes) :-
		blake2s_state_bytes(State, DigestBytes, []).

	blake2s_state_bytes([], Bytes, Bytes).
	blake2s_state_bytes([Word| Words], Bytes, Tail) :-
		integer_to_little_endian_bytes32(Word, Bytes, Rest),
		blake2s_state_bytes(Words, Rest, Tail).

	blake2s_block_words([], []).
	blake2s_block_words([B0, B1, B2, B3| Bytes], [Word| Words]) :-
		little_endian_word32([B0, B1, B2, B3], Word),
		blake2s_block_words(Bytes, Words).

	pad_block(BlockBytes, BlockSize, Block) :-
		length(BlockBytes, Length),
		Padding is BlockSize - Length,
		zeros(Padding, ZeroBytes),
		append(BlockBytes, ZeroBytes, Block).

	zeros(0, []) :-
		!.
	zeros(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zeros(NextCount, Bytes).

	take_up_to(0, Rest, [], Rest) :-
		!.
	take_up_to(_, [], [], []) :-
		!.
	take_up_to(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Bytes, Prefix, Rest).

	replace_nth0(0, [_| Values], Value, [Value| Values]) :-
		!.
	replace_nth0(Index0, [Head| Values0], Value, [Head| Values]) :-
		Index is Index0 - 1,
		replace_nth0(Index, Values0, Value, Values).

	blake2s_sigma(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).
	blake2s_sigma(1, 14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3).
	blake2s_sigma(2, 11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4).
	blake2s_sigma(3, 7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8).
	blake2s_sigma(4, 9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13).
	blake2s_sigma(5, 2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9).
	blake2s_sigma(6, 12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11).
	blake2s_sigma(7, 13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10).
	blake2s_sigma(8, 6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5).
	blake2s_sigma(9, 10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0).

:- end_object.


:- object(md5,
	implements([hash_digest_protocol, hash_state_protocol])).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2026-07-16,
		comment is 'MD5 hash function.',
		see_also is [sha1, sha256]
	]).

	:- uses(hash_common_32, [
		add32/3, add32/5, bytes_hex/2, integer_to_little_endian_bytes32/3, little_endian_word32/2, pad_md/4,
		pad_md_tail/5, rol32/3
	]).

	:- uses(list, [
		append/3, length/2, nth0/3, take/4
	]).

	digest(Bytes, DigestBytes) :-
		pad_md(little, Bytes, 8, PaddedBytes),
		md5_blocks(PaddedBytes, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, A, B, C, D),
		integer_to_little_endian_bytes32(A, DigestBytes, BytesB),
		integer_to_little_endian_bytes32(B, BytesB, BytesC),
		integer_to_little_endian_bytes32(C, BytesC, BytesD),
		integer_to_little_endian_bytes32(D, BytesD, []).

	digest_size(16).

	block_size(64).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	% the state buffers the at-most-63 leftover bytes that do not yet form a
	% complete 64-byte block; Length is the total number of bytes seen so
	% far, needed only for the MD padding appended to the final, partial
	% block at finalization
	new_hash_state(state([], 0, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476)).

	update_hash_state(state(Buffer0, Length0, A0, B0, C0, D0), Bytes, state(Buffer1, Length1, A1, B1, C1, D1)) :-
		append(Buffer0, Bytes, Combined),
		length(Bytes, BytesLength),
		Length1 is Length0 + BytesLength,
		md5_consume_blocks(Combined, A0, B0, C0, D0, A1, B1, C1, D1, Buffer1).

	final_hash_state(state(Buffer, Length, A, B, C, D), Hash) :-
		pad_md_tail(little, Buffer, Length, 8, PaddedTail),
		md5_blocks(PaddedTail, A, B, C, D, FA, FB, FC, FD),
		integer_to_little_endian_bytes32(FA, DigestBytes, BytesB),
		integer_to_little_endian_bytes32(FB, BytesB, BytesC),
		integer_to_little_endian_bytes32(FC, BytesC, BytesD),
		integer_to_little_endian_bytes32(FD, BytesD, []),
		bytes_hex(DigestBytes, Hash).

	md5_consume_blocks(Bytes, A, B, C, D, A, B, C, D, Bytes) :-
		length(Bytes, Length),
		Length < 64,
		!.
	md5_consume_blocks(Bytes, A0, B0, C0, D0, A, B, C, D, Tail) :-
		take(64, Bytes, Block, Rest),
		block_words_le(Block, X),
		md5_rounds(0, X, A0, B0, C0, D0, AA, BB, CC, DD),
		add32(A0, AA, A1),
		add32(B0, BB, B1),
		add32(C0, CC, C1),
		add32(D0, DD, D1),
		md5_consume_blocks(Rest, A1, B1, C1, D1, A, B, C, D, Tail).

	md5_blocks([], A, B, C, D, A, B, C, D).
	md5_blocks([Byte| Bytes], A0, B0, C0, D0, A, B, C, D) :-
		take(64, [Byte| Bytes], Block, Rest),
		block_words_le(Block, X),
		md5_rounds(0, X, A0, B0, C0, D0, AA, BB, CC, DD),
		add32(A0, AA, A1),
		add32(B0, BB, B1),
		add32(C0, CC, C1),
		add32(D0, DD, D1),
		md5_blocks(Rest, A1, B1, C1, D1, A, B, C, D).

	md5_rounds(64, _, A, B, C, D, A, B, C, D) :-
		!.
	md5_rounds(I, X, A0, B0, C0, D0, A, B, C, D) :-
		md5_f(I, B0, C0, D0, F),
		md5_g(I, G),
		nth0(G, X, XG),
		md5_k(I, K),
		add32(A0, F, XG, K, T0),
		md5_s(I, S),
		rol32(T0, S, T1),
		add32(B0, T1, B1),
		NextI is I + 1,
		md5_rounds(NextI, X, D0, B1, B0, C0, A, B, C, D).

	md5_f(I, B, C, D, F) :-
		(	I < 16 ->
			F is ((B /\ C) \/ ((\ B) /\ D)) /\ 0xFFFFFFFF
		;	I < 32 ->
			F is ((D /\ B) \/ ((\ D) /\ C)) /\ 0xFFFFFFFF
		;	I < 48 ->
			F is xor(B, xor(C, D)) /\ 0xFFFFFFFF
		;	F is xor(C, (B \/ (\ D))) /\ 0xFFFFFFFF
		).

	md5_g(I, G) :-
		(	I < 16 -> G is I
		;	I < 32 -> G is (5 * I + 1) mod 16
		;	I < 48 -> G is (3 * I + 5) mod 16
		;	G is (7 * I) mod 16
		).

	md5_s( 0,  7).
	md5_s( 1, 12).
	md5_s( 2, 17).
	md5_s( 3, 22).
	md5_s( 4,  7).
	md5_s( 5, 12).
	md5_s( 6, 17).
	md5_s( 7, 22).
	md5_s( 8,  7).
	md5_s( 9, 12).
	md5_s(10, 17).
	md5_s(11, 22).
	md5_s(12,  7).
	md5_s(13, 12).
	md5_s(14, 17).
	md5_s(15, 22).
	md5_s(16,  5).
	md5_s(17,  9).
	md5_s(18, 14).
	md5_s(19, 20).
	md5_s(20,  5).
	md5_s(21,  9).
	md5_s(22, 14).
	md5_s(23, 20).
	md5_s(24,  5).
	md5_s(25,  9).
	md5_s(26, 14).
	md5_s(27, 20).
	md5_s(28,  5).
	md5_s(29,  9).
	md5_s(30, 14).
	md5_s(31, 20).
	md5_s(32,  4).
	md5_s(33, 11).
	md5_s(34, 16).
	md5_s(35, 23).
	md5_s(36,  4).
	md5_s(37, 11).
	md5_s(38, 16).
	md5_s(39, 23).
	md5_s(40,  4).
	md5_s(41, 11).
	md5_s(42, 16).
	md5_s(43, 23).
	md5_s(44,  4).
	md5_s(45, 11).
	md5_s(46, 16).
	md5_s(47, 23).
	md5_s(48,  6).
	md5_s(49, 10).
	md5_s(50, 15).
	md5_s(51, 21).
	md5_s(52,  6).
	md5_s(53, 10).
	md5_s(54, 15).
	md5_s(55, 21).
	md5_s(56,  6).
	md5_s(57, 10).
	md5_s(58, 15).
	md5_s(59, 21).
	md5_s(60,  6).
	md5_s(61, 10).
	md5_s(62, 15).
	md5_s(63, 21).

	md5_k( 0, 0xD76AA478).
	md5_k( 1, 0xE8C7B756).
	md5_k( 2, 0x242070DB).
	md5_k( 3, 0xC1BDCEEE).
	md5_k( 4, 0xF57C0FAF).
	md5_k( 5, 0x4787C62A).
	md5_k( 6, 0xA8304613).
	md5_k( 7, 0xFD469501).
	md5_k( 8, 0x698098D8).
	md5_k( 9, 0x8B44F7AF).
	md5_k(10, 0xFFFF5BB1).
	md5_k(11, 0x895CD7BE).
	md5_k(12, 0x6B901122).
	md5_k(13, 0xFD987193).
	md5_k(14, 0xA679438E).
	md5_k(15, 0x49B40821).
	md5_k(16, 0xF61E2562).
	md5_k(17, 0xC040B340).
	md5_k(18, 0x265E5A51).
	md5_k(19, 0xE9B6C7AA).
	md5_k(20, 0xD62F105D).
	md5_k(21, 0x02441453).
	md5_k(22, 0xD8A1E681).
	md5_k(23, 0xE7D3FBC8).
	md5_k(24, 0x21E1CDE6).
	md5_k(25, 0xC33707D6).
	md5_k(26, 0xF4D50D87).
	md5_k(27, 0x455A14ED).
	md5_k(28, 0xA9E3E905).
	md5_k(29, 0xFCEFA3F8).
	md5_k(30, 0x676F02D9).
	md5_k(31, 0x8D2A4C8A).
	md5_k(32, 0xFFFA3942).
	md5_k(33, 0x8771F681).
	md5_k(34, 0x6D9D6122).
	md5_k(35, 0xFDE5380C).
	md5_k(36, 0xA4BEEA44).
	md5_k(37, 0x4BDECFA9).
	md5_k(38, 0xF6BB4B60).
	md5_k(39, 0xBEBFBC70).
	md5_k(40, 0x289B7EC6).
	md5_k(41, 0xEAA127FA).
	md5_k(42, 0xD4EF3085).
	md5_k(43, 0x04881D05).
	md5_k(44, 0xD9D4D039).
	md5_k(45, 0xE6DB99E5).
	md5_k(46, 0x1FA27CF8).
	md5_k(47, 0xC4AC5665).
	md5_k(48, 0xF4292244).
	md5_k(49, 0x432AFF97).
	md5_k(50, 0xAB9423A7).
	md5_k(51, 0xFC93A039).
	md5_k(52, 0x655B59C3).
	md5_k(53, 0x8F0CCC92).
	md5_k(54, 0xFFEFF47D).
	md5_k(55, 0x85845DD1).
	md5_k(56, 0x6FA87E4F).
	md5_k(57, 0xFE2CE6E0).
	md5_k(58, 0xA3014314).
	md5_k(59, 0x4E0811A1).
	md5_k(60, 0xF7537E82).
	md5_k(61, 0xBD3AF235).
	md5_k(62, 0x2AD7D2BB).
	md5_k(63, 0xEB86D391).

	block_words_le([], []).
	block_words_le([B0, B1, B2, B3| Bytes], [Word| Words]) :-
		little_endian_word32([B0, B1, B2, B3], Word),
		block_words_le(Bytes, Words).

:- end_object.
