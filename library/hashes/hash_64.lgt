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


:- object(djb2_64,
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'DJB2 64-bit hash function.',
		see_also is [djb2_32, sdbm_64, fnv1a_64]
	]).

	:- uses(hash_common_64, [
		word64_hex/2
	]).

	hash(Bytes, Hash) :-
		djb2_64(Bytes, 5381, Value),
		word64_hex(Value, Hash).

	djb2_64([], Acc, Acc).
	djb2_64([Byte| Bytes], Acc0, Acc) :-
		Acc1 is ((Acc0 << 5) + Acc0 + Byte) /\ 0xFFFFFFFFFFFFFFFF,
		djb2_64(Bytes, Acc1, Acc).

:- end_object.


:- object(sdbm_64,
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'sdbm 64-bit hash function.',
		see_also is [sdbm_32, djb2_64, fnv1a_64]
	]).

	:- uses(hash_common_64, [
		word64_hex/2
	]).

	hash(Bytes, Hash) :-
		sdbm_64(Bytes, 0, Value),
		word64_hex(Value, Hash).

	sdbm_64([], Acc, Acc).
	sdbm_64([Byte| Bytes], Acc0, Acc) :-
		Acc1 is (Byte + (Acc0 << 6) + (Acc0 << 16) - Acc0) /\ 0xFFFFFFFFFFFFFFFF,
		sdbm_64(Bytes, Acc1, Acc).

:- end_object.


:- object(fnv1a_64,
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'FNV-1a 64-bit hash function.',
		see_also is [fnv1a_32, djb2_64, sdbm_64]
	]).

	:- uses(hash_common_64, [
		word64_hex/2, mul64/3
	]).

	hash(Bytes, Hash) :-
		fnv1a_64(Bytes, 0xCBF29CE484222325, Value),
		word64_hex(Value, Hash).

	fnv1a_64([], Acc, Acc).
	fnv1a_64([Byte| Bytes], Acc0, Acc) :-
		Acc1 is xor(Acc0, Byte),
		mul64(Acc1, 0x100000001B3, Acc2),
		fnv1a_64(Bytes, Acc2, Acc).

:- end_object.


:- object(siphash_2_4(_Key_),
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'SipHash-2-4 keyed hash function.',
		parameters is [
			'Key' - 'A list of 16 bytes.'
		],
		see_also is [siphash_2_4, fnv1a_64, crc32b, crc32c]
	]).

	:- uses(hash_common_64, [
		add64/3, or64/3, rol64/3, shl64/3, word64_hex/2, xor64/3
	]).

	hash(Bytes, Hash) :-
		list::length(_Key_, 16),
		siphash(Bytes, Value),
		word64_hex(Value, Hash).

	siphash(Bytes, Hash) :-
		key_words(_Key_, K0, K1),
		xor64(0x736F6D6570736575, K0, V0_0),
		xor64(0x646F72616E646F6D, K1, V1_0),
		xor64(0x6C7967656E657261, K0, V2_0),
		xor64(0x7465646279746573, K1, V3_0),
		process_blocks(Bytes, V0_0, V1_0, V2_0, V3_0, V0_1, V1_1, V2_1, V3_1, Last),
		last_word(Bytes, Last, FinalM),
		xor64(V3_1, FinalM, V3_2),
		sip_rounds(2, V0_1, V1_1, V2_1, V3_2, V0_3, V1_3, V2_3, V3_3),
		xor64(V0_3, FinalM, V0_4),
		xor64(V2_3, 0xFF, V2_4),
		sip_rounds(4, V0_4, V1_3, V2_4, V3_3, V0_5, V1_5, V2_5, V3_5),
		xor64(V0_5, V1_5, T01),
		xor64(V2_5, V3_5, T23),
		xor64(T01, T23, Hash).

	process_blocks(Bytes, V0, V1, V2, V3, FV0, FV1, FV2, FV3, Last) :-
		process_blocks(Bytes, [], V0, V1, V2, V3, FV0, FV1, FV2, FV3, Last).

	process_blocks([], Acc, V0, V1, V2, V3, V0, V1, V2, V3, Acc).
	process_blocks([Byte| Bytes], Acc0, V0, V1, V2, V3, FV0, FV1, FV2, FV3, Last) :-
		list::append(Acc0, [Byte], Acc1),
		(   Acc1 = [B0, B1, B2, B3, B4, B5, B6, B7] ->
			little_endian_word64([B0, B1, B2, B3, B4, B5, B6, B7], M),
			xor64(V3, M, V3_0),
			sip_rounds(2, V0, V1, V2, V3_0, V0_1, V1_1, V2_1, V3_1),
			xor64(V0_1, M, V0_2),
			process_blocks(Bytes, [], V0_2, V1_1, V2_1, V3_1, FV0, FV1, FV2, FV3, Last)
		;   process_blocks(Bytes, Acc1, V0, V1, V2, V3, FV0, FV1, FV2, FV3, Last)
		).

	last_word(Bytes, Acc, Word) :-
		list::length(Bytes, Length),
		shl64(Length /\ 0xFF, 56, LastByte),
		partial_word(Acc, 0, Partial),
		or64(LastByte, Partial, Word).

	little_endian_word64(Bytes, Word) :-
		partial_word(Bytes, 0, Word).

	partial_word([], _, 0).
	partial_word([Byte| Bytes], Shift, Word) :-
		partial_word(Bytes, Shift + 8, Rest),
		shl64(Byte, Shift, ShiftedByte),
		or64(ShiftedByte, Rest, Word).

	sip_rounds(0, V0, V1, V2, V3, V0, V1, V2, V3) :-
		!.
	sip_rounds(Count, V0_0, V1_0, V2_0, V3_0, V0, V1, V2, V3) :-
		add64(V0_0, V1_0, A0),
		rol64(V1_0, 13, A1),
		xor64(A1, A0, B1),
		rol64(A0, 32, B0),
		add64(V2_0, V3_0, C0),
		rol64(V3_0, 16, C1),
		xor64(C1, C0, D1),
		add64(B0, D1, E0),
		rol64(D1, 21, E1),
		xor64(E1, E0, F1),
		add64(C0, B1, G0),
		rol64(B1, 17, G1),
		xor64(G1, G0, H1),
		rol64(G0, 32, H0),
		NextCount is Count - 1,
		sip_rounds(NextCount, E0, H1, H0, F1, V0, V1, V2, V3).

	key_words([K0, K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11, K12, K13, K14, K15], Word0, Word1) :-
		little_endian_word64([K0, K1, K2, K3, K4, K5, K6, K7], Word0),
		little_endian_word64([K8, K9, K10, K11, K12, K13, K14, K15], Word1).

:- end_object.


:- object(siphash_2_4,
	extends(siphash_2_4([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'SipHash-2-4 hash function using the standard reference key 00 01 02 ... 0f.',
		see_also is [siphash_2_4(_), fnv1a_64, crc32b, crc32c]
	]).

:- end_object.


:- object(murmurhash3_x86_128,
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'MurmurHash3 x86 128-bit hash function with seed 0.',
		see_also is [murmurhash3_x86_32, murmurhash3_x64_128]
	]).

	:- uses(hash_common_32, [
		add32/3, add32/5, little_endian_word32/2, mul32/3, rol32/3, word32_hex/2
	]).

	hash(Bytes, Hash) :-
		murmurhash3_x86_128(Bytes, 0, H1, H2, H3, H4),
		word32_hex(H1, Hex1),
		word32_hex(H2, Hex2),
		word32_hex(H3, Hex3),
		word32_hex(H4, Hex4),
		atom_concat(Hex1, Hex2, Hex12),
		atom_concat(Hex12, Hex3, Hex123),
		atom_concat(Hex123, Hex4, Hash),
		!.

	murmurhash3_x86_128(Bytes, Seed, H1, H2, H3, H4) :-
		list::length(Bytes, Length),
		body(Bytes, Seed, Seed, Seed, Seed, T1, T2, T3, T4, Tail),
		tail(Tail, T1, T2, T3, T4, U1, U2, U3, U4),
		F1_0 is xor(U1, Length),
		F2_0 is xor(U2, Length),
		F3_0 is xor(U3, Length),
		F4_0 is xor(U4, Length),
		add32(F1_0, F2_0, F3_0, F4_0, F1_1),
		add32(F2_0, F1_1, F2_1),
		add32(F3_0, F1_1, F3_1),
		add32(F4_0, F1_1, F4_1),
		fmix32(F1_1, M1),
		fmix32(F2_1, M2),
		fmix32(F3_1, M3),
		fmix32(F4_1, M4),
		add32(M1, M2, M3, M4, H1),
		add32(M2, H1, H2),
		add32(M3, H1, H3),
		add32(M4, H1, H4).

	body([B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15| Bytes], H1_0, H2_0, H3_0, H4_0, H1, H2, H3, H4, Tail) :-
		!,
		little_endian_word32([B0, B1, B2, B3], K1_0),
		little_endian_word32([B4, B5, B6, B7], K2_0),
		little_endian_word32([B8, B9, B10, B11], K3_0),
		little_endian_word32([B12, B13, B14, B15], K4_0),
		mul32(K1_0, 0x239B961B, K1_1),
		rol32(K1_1, 15, K1_2),
		mul32(K1_2, 0xAB0E9789, K1),
		H1_X is xor(H1_0, K1),
		rol32(H1_X, 19, H1_R),
		add32(H1_R, H2_0, H1_S),
		mul32(H1_S, 5, H1_M),
		add32(H1_M, 0x561CCD1B, H1_1),
		mul32(K2_0, 0xAB0E9789, K2_1),
		rol32(K2_1, 16, K2_2),
		mul32(K2_2, 0x38B34AE5, K2),
		H2_X is xor(H2_0, K2),
		rol32(H2_X, 17, H2_R),
		add32(H2_R, H3_0, H2_S),
		mul32(H2_S, 5, H2_M),
		add32(H2_M, 0x0BCAA747, H2_1),
		mul32(K3_0, 0x38B34AE5, K3_1),
		rol32(K3_1, 17, K3_2),
		mul32(K3_2, 0xA1E38B93, K3),
		H3_X is xor(H3_0, K3),
		rol32(H3_X, 15, H3_R),
		add32(H3_R, H4_0, H3_S),
		mul32(H3_S, 5, H3_M),
		add32(H3_M, 0x96CD1C35, H3_1),
		mul32(K4_0, 0xA1E38B93, K4_1),
		rol32(K4_1, 18, K4_2),
		mul32(K4_2, 0x239B961B, K4),
		H4_X is xor(H4_0, K4),
		rol32(H4_X, 13, H4_R),
		add32(H4_R, H1_1, H4_S),
		mul32(H4_S, 5, H4_M),
		add32(H4_M, 0x32AC3B17, H4_1),
		body(Bytes, H1_1, H2_1, H3_1, H4_1, H1, H2, H3, H4, Tail).
	body(Tail, H1, H2, H3, H4, H1, H2, H3, H4, Tail).

	tail(Bytes, H1_0, H2_0, H3_0, H4_0, H1, H2, H3, H4) :-
		take_up_to(4, Bytes, K1Bytes, Rest1),
		take_up_to(4, Rest1, K2Bytes, Rest2),
		take_up_to(4, Rest2, K3Bytes, K4Bytes),
		tail_mix_k4(K4Bytes, H4_0, H4),
		tail_mix_k3(K3Bytes, H3_0, H3),
		tail_mix_k2(K2Bytes, H2_0, H2),
		tail_mix_k1(K1Bytes, H1_0, H1).

	tail_mix_k1([], H1, H1).
	tail_mix_k1([Byte| Bytes], H1_0, H1) :-
		partial_word32([Byte| Bytes], 0, K1_0),
		mul32(K1_0, 0x239B961B, K1_1),
		rol32(K1_1, 15, K1_2),
		mul32(K1_2, 0xAB0E9789, K1),
		H1 is xor(H1_0, K1).

	tail_mix_k2([], H2, H2).
	tail_mix_k2([Byte| Bytes], H2_0, H2) :-
		partial_word32([Byte| Bytes], 0, K2_0),
		mul32(K2_0, 0xAB0E9789, K2_1),
		rol32(K2_1, 16, K2_2),
		mul32(K2_2, 0x38B34AE5, K2),
		H2 is xor(H2_0, K2).

	tail_mix_k3([], H3, H3).
	tail_mix_k3([Byte| Bytes], H3_0, H3) :-
		partial_word32([Byte| Bytes], 0, K3_0),
		mul32(K3_0, 0x38B34AE5, K3_1),
		rol32(K3_1, 17, K3_2),
		mul32(K3_2, 0xA1E38B93, K3),
		H3 is xor(H3_0, K3).

	tail_mix_k4([], H4, H4).
	tail_mix_k4([Byte| Bytes], H4_0, H4) :-
		partial_word32([Byte| Bytes], 0, K4_0),
		mul32(K4_0, 0xA1E38B93, K4_1),
		rol32(K4_1, 18, K4_2),
		mul32(K4_2, 0x239B961B, K4),
		H4 is xor(H4_0, K4).

	fmix32(H0, H) :-
		H1 is xor(H0, H0 >> 16),
		mul32(H1, 0x85EBCA6B, H2),
		H3 is xor(H2, H2 >> 13),
		mul32(H3, 0xC2B2AE35, H4),
		H is xor(H4, H4 >> 16) /\ 0xFFFFFFFF.

	take_up_to(0, Rest, [], Rest) :-
		!.
	take_up_to(_, [], [], []).
	take_up_to(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Bytes, Prefix, Rest).

	partial_word32([], _, 0).
	partial_word32([Byte| Bytes], Shift, Word) :-
		partial_word32(Bytes, Shift + 8, Rest),
		Word is (Byte << Shift) \/ Rest.

:- end_object.


:- object(murmurhash3_x64_128,
	implements(hash_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'MurmurHash3 x64 128-bit hash function with seed 0.',
		see_also is [murmurhash3_x86_32, murmurhash3_x86_128]
	]).

	:- uses(hash_common_64, [
		add64/3, mul64/3, or64/3, rol64/3, shl64/3, shr64/3, word64_hex/2, xor64/3
	]).

	hash(Bytes, Hash) :-
		murmurhash3_x64_128(Bytes, 0, H1, H2),
		word64_hex(H1, Hex1),
		word64_hex(H2, Hex2),
		atom_concat(Hex1, Hex2, Hash),
		!.

	murmurhash3_x64_128(Bytes, Seed, H1, H2) :-
		list::length(Bytes, Length),
		body(Bytes, Seed, Seed, T1, T2, Tail),
		tail(Tail, T1, T2, U1, U2),
		xor64(U1, Length, F1_0),
		xor64(U2, Length, F2_0),
		add64(F1_0, F2_0, F1_1),
		add64(F2_0, F1_1, F2_1),
		fmix64(F1_1, M1),
		fmix64(F2_1, M2),
		add64(M1, M2, H1),
		add64(M2, H1, H2).

	body([B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15| Bytes], H1_0, H2_0, H1, H2, Tail) :-
		!,
		partial_word64([B0, B1, B2, B3, B4, B5, B6, B7], 0, K1_0),
		partial_word64([B8, B9, B10, B11, B12, B13, B14, B15], 0, K2_0),
		mul64(K1_0, 0x87C37B91114253D5, K1_1),
		rol64(K1_1, 31, K1_2),
		mul64(K1_2, 0x4CF5AD432745937F, K1),
		xor64(H1_0, K1, H1_X),
		rol64(H1_X, 27, H1_R),
		add64(H1_R, H2_0, H1_S),
		mul64(H1_S, 5, H1_M),
		add64(H1_M, 0x52DCE729, H1_1),
		mul64(K2_0, 0x4CF5AD432745937F, K2_1),
		rol64(K2_1, 33, K2_2),
		mul64(K2_2, 0x87C37B91114253D5, K2),
		xor64(H2_0, K2, H2_X),
		rol64(H2_X, 31, H2_R),
		add64(H2_R, H1_1, H2_S),
		mul64(H2_S, 5, H2_M),
		add64(H2_M, 0x38495AB5, H2_1),
		body(Bytes, H1_1, H2_1, H1, H2, Tail).
	body(Tail, H1, H2, H1, H2, Tail).

	tail(Bytes, H1_0, H2_0, H1, H2) :-
		take_up_to(8, Bytes, K1Bytes, K2Bytes),
		tail_mix_k2(K2Bytes, H2_0, H2),
		tail_mix_k1(K1Bytes, H1_0, H1).

	tail_mix_k1([], H1, H1).
	tail_mix_k1([Byte| Bytes], H1_0, H1) :-
		partial_word64([Byte| Bytes], 0, K1_0),
		mul64(K1_0, 0x87C37B91114253D5, K1_1),
		rol64(K1_1, 31, K1_2),
		mul64(K1_2, 0x4CF5AD432745937F, K1),
		xor64(H1_0, K1, H1).

	tail_mix_k2([], H2, H2).
	tail_mix_k2([Byte| Bytes], H2_0, H2) :-
		partial_word64([Byte| Bytes], 0, K2_0),
		mul64(K2_0, 0x4CF5AD432745937F, K2_1),
		rol64(K2_1, 33, K2_2),
		mul64(K2_2, 0x87C37B91114253D5, K2),
		xor64(H2_0, K2, H2).

	fmix64(H0, H) :-
		shr64(H0, 33, H0Shift33),
		xor64(H0, H0Shift33, H1),
		mul64(H1, 0xFF51AFD7ED558CCD, H2),
		shr64(H2, 33, H2Shift33),
		xor64(H2, H2Shift33, H3),
		mul64(H3, 0xC4CEB9FE1A85EC53, H4),
		shr64(H4, 33, H4Shift33),
		xor64(H4, H4Shift33, H).

	take_up_to(0, Rest, [], Rest) :-
		!.
	take_up_to(_, [], [], []).
	take_up_to(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_up_to(NextCount, Bytes, Prefix, Rest).

	partial_word64([], _, 0).
	partial_word64([Byte| Bytes], Shift, Word) :-
		partial_word64(Bytes, Shift + 8, Rest),
		shl64(Byte, Shift, ShiftedByte),
		or64(ShiftedByte, Rest, Word).

:- end_object.


:- object(fips202_hash(_RateBytes_, _Suffix_, _OutputBytes_),
	implements(hash_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'Common implementation of the standardized FIPS 202 SHA-3 and SHAKE variants using the Keccak-f[1600] permutation.',
		parnames is ['RateBytes', 'Suffix', 'OutputBytes']
	]).

	:- protected(absorb/2).
	:- mode(absorb(+list(byte), --list(integer)), one).
	:- info(absorb/2, [
		comment is 'Absorbs the input bytes into the Keccak state and returns the resulting state after the final padded block permutation.',
		argnames is ['Bytes', 'State']
	]).
	:- protected(squeeze/3).
	:- mode(squeeze(+list(integer), +integer, --list(byte)), one).
	:- info(squeeze/3, [
		comment is 'Squeezes the requested number of output bytes from a Keccak state.',
		argnames is ['State', 'OutputBytes', 'DigestBytes']
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(hash_common_64, [
		and64/3, mask64/1, not64/2, rol64/3, shl64/3, shr64/3, xor64/3
	]).

	hash(Bytes, Hash) :-
		integer(_OutputBytes_),
		_OutputBytes_ >= 0,
		absorb(Bytes, State),
		squeeze(State, _OutputBytes_, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	absorb(Bytes, State) :-
		initial_state(State0),
		absorb_full_blocks(Bytes, State0, State1, Tail),
		padding_block(Tail, Block),
		xor_block(Block, State1, State2),
		keccak_f1600(State2, State).

	absorb_full_blocks(Bytes, State0, State, Tail) :-
		(   take_exact_prefix(_RateBytes_, Bytes, Block, Rest) ->
			xor_block(Block, State0, State1),
			keccak_f1600(State1, State2),
			absorb_full_blocks(Rest, State2, State, Tail)
		;   State = State0,
			Tail = Bytes
		).

	padding_block(Tail, Block) :-
		list::length(Tail, TailLength),
		ZeroCount is _RateBytes_ - TailLength - 1,
		zero_bytes(ZeroCount, Zeros),
		list::append(Tail, [_Suffix_| Zeros], Block0),
		xor_last_byte(Block0, 0x80, Block).

	squeeze(_, 0, []) :-
		!.
	squeeze(State, OutputBytes, DigestBytes) :-
		(   OutputBytes =< _RateBytes_ ->
			state_prefix_bytes(State, OutputBytes, DigestBytes)
		;   state_prefix_bytes(State, _RateBytes_, BlockBytes),
			Remaining is OutputBytes - _RateBytes_,
			keccak_f1600(State, NextState),
			list::append(BlockBytes, RestBytes, DigestBytes),
			squeeze(NextState, Remaining, RestBytes)
		).

	xor_block(Bytes, State0, State) :-
		xor_block(Bytes, 0, State0, State).

	xor_block([], _, State, State).
	xor_block([Byte| Bytes], Offset, State0, State) :-
		LaneIndex is Offset >> 3,
		Shift is (Offset /\ 7) << 3,
		state_lane(State0, LaneIndex, Lane0),
		shl64(Byte, Shift, ShiftedByte),
		xor64(Lane0, ShiftedByte, Lane1),
		set_lane(State0, LaneIndex, Lane1, State1),
		NextOffset is Offset + 1,
		xor_block(Bytes, NextOffset, State1, State).

	state_prefix_bytes(State, Count, Bytes) :-
		state_prefix_bytes(0, Count, State, Bytes).

	state_prefix_bytes(_, 0, _, []) :-
		!.
	state_prefix_bytes(Offset, Count, State, [Byte| Bytes]) :-
		LaneIndex is Offset >> 3,
		Shift is (Offset /\ 7) << 3,
		state_lane(State, LaneIndex, Lane),
		shr64(Lane, Shift, ShiftedLane),
		Byte is ShiftedLane /\ 0xFF,
		NextOffset is Offset + 1,
		NextCount is Count - 1,
		state_prefix_bytes(NextOffset, NextCount, State, Bytes).

	keccak_f1600(State0, State) :-
		round_constants(RoundConstants),
		keccak_rounds(RoundConstants, State0, State).

	keccak_rounds([], State, State).
	keccak_rounds([RoundConstant| RoundConstants], State0, State) :-
		theta(State0, State1),
		rho_pi(State1, State2),
		chi(State2, State3),
		iota(State3, RoundConstant, State4),
		keccak_rounds(RoundConstants, State4, State).

	theta(State0, State) :-
		column_parity(0, State0, C0),
		column_parity(1, State0, C1),
		column_parity(2, State0, C2),
		column_parity(3, State0, C3),
		column_parity(4, State0, C4),
		rol64(C1, 1, RC1),
		rol64(C2, 1, RC2),
		rol64(C3, 1, RC3),
		rol64(C4, 1, RC4),
		rol64(C0, 1, RC0),
		xor64(C4, RC1, D0),
		xor64(C0, RC2, D1),
		xor64(C1, RC3, D2),
		xor64(C2, RC4, D3),
		xor64(C3, RC0, D4),
		apply_theta(0, State0, [D0, D1, D2, D3, D4], State).

	column_parity(X, State, Parity) :-
		X5 is X + 5,
		X10 is X + 10,
		X15 is X + 15,
		X20 is X + 20,
		state_lane(State, X, A0),
		state_lane(State, X5, A1),
		state_lane(State, X10, A2),
		state_lane(State, X15, A3),
		state_lane(State, X20, A4),
		xor64(A3, A4, P34),
		xor64(A2, P34, P234),
		xor64(A1, P234, P1234),
		xor64(A0, P1234, Parity).

	apply_theta(25, _, _, []) :-
		!.
	apply_theta(Index, State0, Deltas, [Lane| State]) :-
		X is Index mod 5,
		state_lane(State0, Index, Lane0),
		state_lane(Deltas, X, Delta),
		xor64(Lane0, Delta, Lane),
		NextIndex is Index + 1,
		apply_theta(NextIndex, State0, Deltas, State).

	rho_pi(State0, State) :-
		rho_offsets(Offsets),
		initial_state(BlankState),
		rho_pi(0, Offsets, State0, BlankState, State).

	rho_pi(25, [], _, State, State) :-
		!.
	rho_pi(Index, [Offset| Offsets], State0, Acc0, State) :-
		state_lane(State0, Index, Lane),
		rol64(Lane, Offset, Rotated),
		X is Index mod 5,
		Y is Index // 5,
		NewX is Y,
		NewY is (2 * X + 3 * Y) mod 5,
		NewIndex is NewX + 5 * NewY,
		set_lane(Acc0, NewIndex, Rotated, Acc1),
		NextIndex is Index + 1,
		rho_pi(NextIndex, Offsets, State0, Acc1, State).

	chi(State0, State) :-
		mask64(Mask),
		chi_rows(0, State0, Mask, State).

	chi_rows(5, _, _, []) :-
		!.
	chi_rows(Y, State0, Mask, [N0, N1, N2, N3, N4| Rest]) :-
		Base is Y * 5,
		I1 is Base + 1,
		I2 is Base + 2,
		I3 is Base + 3,
		I4 is Base + 4,
		state_lane(State0, Base, B0),
		state_lane(State0, I1, B1),
		state_lane(State0, I2, B2),
		state_lane(State0, I3, B3),
		state_lane(State0, I4, B4),
		not64(B1, NB1),
		not64(B2, NB2),
		not64(B3, NB3),
		not64(B4, NB4),
		not64(B0, NB0),
		and64(NB1, B2, T0),
		and64(NB2, B3, T1),
		and64(NB3, B4, T2),
		and64(NB4, B0, T3),
		and64(NB0, B1, T4),
		xor64(B0, T0, N0_0),
		xor64(B1, T1, N1_0),
		xor64(B2, T2, N2_0),
		xor64(B3, T3, N3_0),
		xor64(B4, T4, N4_0),
		N0 is N0_0 /\ Mask,
		N1 is N1_0 /\ Mask,
		N2 is N2_0 /\ Mask,
		N3 is N3_0 /\ Mask,
		N4 is N4_0 /\ Mask,
		NextY is Y + 1,
		chi_rows(NextY, State0, Mask, Rest).

	iota([Lane| Lanes], RoundConstant, [UpdatedLane| Lanes]) :-
		xor64(Lane, RoundConstant, UpdatedLane).

	initial_state([
		0, 0, 0, 0, 0,
		0, 0, 0, 0, 0,
		0, 0, 0, 0, 0,
		0, 0, 0, 0, 0,
		0, 0, 0, 0, 0
	]).

	round_constants([
		0x0000000000000001, 0x0000000000008082,
		0x800000000000808A, 0x8000000080008000,
		0x000000000000808B, 0x0000000080000001,
		0x8000000080008081, 0x8000000000008009,
		0x000000000000008A, 0x0000000000000088,
		0x0000000080008009, 0x000000008000000A,
		0x000000008000808B, 0x800000000000008B,
		0x8000000000008089, 0x8000000000008003,
		0x8000000000008002, 0x8000000000000080,
		0x000000000000800A, 0x800000008000000A,
		0x8000000080008081, 0x8000000000008080,
		0x0000000080000001, 0x8000000080008008
	]).

	rho_offsets([
		0, 1, 62, 28, 27,
		36, 44, 6, 55, 20,
		3, 10, 43, 25, 39,
		41, 45, 15, 21, 8,
		18, 2, 61, 56, 14
	]).

	take_exact_prefix(0, Rest, [], Rest) :-
		!.
	take_exact_prefix(_, [], _, _) :-
		!,
		fail.
	take_exact_prefix(Count, [Byte| Bytes], [Byte| Prefix], Rest) :-
		NextCount is Count - 1,
		take_exact_prefix(NextCount, Bytes, Prefix, Rest).

	zero_bytes(0, []) :-
		!.
	zero_bytes(Count, [0| Bytes]) :-
		NextCount is Count - 1,
		zero_bytes(NextCount, Bytes).

	xor_last_byte([Byte], Value, [UpdatedByte]) :-
		!,
		UpdatedByte is xor(Byte, Value).
	xor_last_byte([Byte| Bytes], Value, [Byte| UpdatedBytes]) :-
		xor_last_byte(Bytes, Value, UpdatedBytes).

	state_lane([Lane| _], 0, Lane) :-
		!.
	state_lane([_| Lanes], Index, Lane) :-
		NextIndex is Index - 1,
		state_lane(Lanes, NextIndex, Lane).

	set_lane([_| Lanes], 0, UpdatedLane, [UpdatedLane| Lanes]) :-
		!.
	set_lane([Lane| Lanes], Index, UpdatedLane, [Lane| UpdatedLanes]) :-
		NextIndex is Index - 1,
		set_lane(Lanes, NextIndex, UpdatedLane, UpdatedLanes).

:- end_object.


:- object(fips202_fixed_hash(_RateBytes_, _Suffix_, _OutputBytes_),
	extends(fips202_hash(_RateBytes_, _Suffix_, _OutputBytes_)),
	implements(hash_digest_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'Common implementation of the fixed-size FIPS 202 SHA-3 variants suitable for use with HMAC.',
		parnames is ['RateBytes', 'Suffix', 'OutputBytes']
	]).

	digest(Bytes, DigestBytes) :-
		integer(_OutputBytes_),
		_OutputBytes_ >= 0,
		^^absorb(Bytes, State),
		^^squeeze(State, _OutputBytes_, DigestBytes).

	digest_size(_OutputBytes_).

	block_size(_RateBytes_).

:- end_object.


:- object(sha3_224,
	extends(fips202_fixed_hash(144, 0x06, 28))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'FIPS 202 SHA3-224 hash function.',
		see_also is [sha3_256, sha3_384, sha3_512, shake128(_), shake256(_)]
	]).

:- end_object.


:- object(sha3_256,
	extends(fips202_fixed_hash(136, 0x06, 32))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'FIPS 202 SHA3-256 hash function.',
		see_also is [sha3_224, sha3_384, sha3_512, sha256, shake128(_), shake256(_)]
	]).

:- end_object.


:- object(sha3_384,
	extends(fips202_fixed_hash(104, 0x06, 48))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'FIPS 202 SHA3-384 hash function.',
		see_also is [sha3_224, sha3_256, sha3_512, shake128(_), shake256(_)]
	]).

:- end_object.


:- object(sha3_512,
	extends(fips202_fixed_hash(72, 0x06, 64))).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-04-15,
		comment is 'FIPS 202 SHA3-512 hash function.',
		see_also is [sha3_224, sha3_256, sha3_384, shake128(_), shake256(_)]
	]).

:- end_object.


:- object(shake128(_OutputBytes_),
	extends(fips202_hash(168, 0x1F, _OutputBytes_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'FIPS 202 SHAKE128 extensible-output function.',
		parameters is [
			'OutputBytes' - 'Number of output bytes to generate.'
		],
		see_also is [shake256(_), sha3_224, sha3_256, sha3_384, sha3_512]
	]).

:- end_object.


:- object(shake256(_OutputBytes_),
	extends(fips202_hash(136, 0x1F, _OutputBytes_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-04,
		comment is 'FIPS 202 SHAKE256 extensible-output function.',
		parameters is [
			'OutputBytes' - 'Number of output bytes to generate.'
		],
		see_also is [shake128(_), sha3_224, sha3_256, sha3_384, sha3_512]
	]).

:- end_object.


:- object(sha1,
	implements(hash_digest_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'SHA-1 hash function.',
		see_also is [md5, sha256]
	]).

	:- uses(hash_common_32, [
		pad_md/4, integer_to_big_endian_bytes32/3, bytes_hex/2, add32/3, rol32/3, big_endian_word32/2
	]).

	digest(Bytes, DigestBytes) :-
		pad_md(big, Bytes, 8, PaddedBytes),
		sha1_blocks(PaddedBytes, 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0, H0, H1, H2, H3, H4),
		integer_to_big_endian_bytes32(H0, DigestBytes, B1),
		integer_to_big_endian_bytes32(H1, B1, B2),
		integer_to_big_endian_bytes32(H2, B2, B3),
		integer_to_big_endian_bytes32(H3, B3, B4),
		integer_to_big_endian_bytes32(H4, B4, []).

	digest_size(20).

	block_size(64).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	sha1_blocks([], H0, H1, H2, H3, H4, H0, H1, H2, H3, H4).
	sha1_blocks([Byte| Bytes], H0_0, H1_0, H2_0, H3_0, H4_0, H0, H1, H2, H3, H4) :-
		list::take(64, [Byte| Bytes], Block, Rest),
		block_words_be(Block, Words0, Tail0),
		extend_sha1_words(16, Words0, Tail0, Words),
		sha1_rounds(0, Words, H0_0, H1_0, H2_0, H3_0, H4_0, A, B, C, D, E),
		add32(H0_0, A, H0_1),
		add32(H1_0, B, H1_1),
		add32(H2_0, C, H2_1),
		add32(H3_0, D, H3_1),
		add32(H4_0, E, H4_1),
		sha1_blocks(Rest, H0_1, H1_1, H2_1, H3_1, H4_1, H0, H1, H2, H3, H4).

	sha1_rounds(80, _, A, B, C, D, E, A, B, C, D, E) :-
		!.
	sha1_rounds(I, W, A0, B0, C0, D0, E0, A, B, C, D, E) :-
		list::nth0(I, W, WI),
		sha1_f_k(I, B0, C0, D0, F, K),
		rol32(A0, 5, RA),
		T is (RA + F + E0 + K + WI) /\ 0xFFFFFFFF,
		rol32(B0, 30, C1),
		NextI is I + 1,
		sha1_rounds(NextI, W, T, A0, C1, C0, D0, A, B, C, D, E).

	sha1_f_k(I, B, C, D, F, K) :-
		(   I < 20 ->
			F is ((B /\ C) \/ ((\ B) /\ D)) /\ 0xFFFFFFFF,
			K = 0x5A827999
		;   I < 40 ->
			F is xor(B, xor(C, D)) /\ 0xFFFFFFFF,
			K = 0x6ED9EBA1
		;   I < 60 ->
			F is ((B /\ C) \/ (B /\ D) \/ (C /\ D)) /\ 0xFFFFFFFF,
			K = 0x8F1BBCDC
		;   F is xor(B, xor(C, D)) /\ 0xFFFFFFFF,
			K = 0xCA62C1D6
		).

	extend_sha1_words(80, Words, [], Words) :-
		!.
	extend_sha1_words(Index, Words0, [Word| Tail1], Words) :-
		I3 is Index - 3,
		I8 is Index - 8,
		I14 is Index - 14,
		I16 is Index - 16,
		list::nth0(I3, Words0, W3),
		list::nth0(I8, Words0, W8),
		list::nth0(I14, Words0, W14),
		list::nth0(I16, Words0, W16),
		Temp is xor(W3, xor(W8, xor(W14, W16))),
		rol32(Temp, 1, Word),
		NextIndex is Index + 1,
		extend_sha1_words(NextIndex, Words0, Tail1, Words).

	block_words_be([], Tail, Tail).
	block_words_be([B0, B1, B2, B3| Bytes], [Word| Words], Tail) :-
		big_endian_word32([B0, B1, B2, B3], Word),
		block_words_be(Bytes, Words, Tail).

:- end_object.


:- object(sha512_256,
	implements(hash_digest_protocol)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'SHA-512/256 hash function.',
		see_also is [sha256, sha1]
	]).

	:- uses(hash_common_32, [
		bytes_hex/2
	]).

	:- uses(hash_common_64, [
		add64/3, and64/3, integer_to_big_endian_bytes64/3, not64/2, or64/3,
		rol64/3, shl64/3, shr64/3, xor64/3
	]).

	digest(Bytes, DigestBytes) :-
		sha512_256_pad(Bytes, PaddedBytes),
		sha512_256_blocks(PaddedBytes, [0x22312194FC2BF72C,0x9F555FA3C84C64C2,0x2393B86B6F53B151,0x963877195940EABD,0x96283EE2A88EFFE3,0xBE5E1E2553863992,0x2B0199FC2C85B8AA,0x0EB72DDC81C52CA2], State),
		sha512_256_state_bytes(State, DigestBytes).

	digest_size(32).

	block_size(128).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	sha512_256_pad(Bytes, PaddedBytes) :-
		list::length(Bytes, Length),
		BitLength is Length * 8,
		Zeros is (112 - ((Length + 1) mod 128) + 128) mod 128,
		sha512_256_zeros(Zeros, ZeroBytes, LengthBytes),
		sha512_256_length_bytes(BitLength, LengthBytes),
		list::append(Bytes, [0x80| ZeroBytes], PaddedBytes).

	sha512_256_zeros(0, Tail, Tail) :-
		!.
	sha512_256_zeros(Count, [0| Zeros], Tail) :-
		NextCount is Count - 1,
		sha512_256_zeros(NextCount, Zeros, Tail).

	sha512_256_length_bytes(BitLength, LengthBytes) :-
		Two64 is 0x10000000000000000,
		High is (BitLength // Two64) mod Two64,
		Low is BitLength mod Two64,
		integer_to_big_endian_bytes64(High, LengthBytes, LowBytes),
		integer_to_big_endian_bytes64(Low, LowBytes, []).

	sha512_256_blocks([], State, State).
	sha512_256_blocks([Byte| Bytes], State0, State) :-
		list::take(128, [Byte| Bytes], Block, Rest),
		sha512_256_block_words(Block, Words0, Tail0),
		extend_sha512_256_words(16, Words0, Tail0, Words),
		sha512_256_compress(Words, State0, State1),
		sha512_256_blocks(Rest, State1, State).

	sha512_256_compress(W, [A0,B0,C0,D0,E0,F0,G0,H0], [A,B,C,D,E,F,G,H]) :-
		sha512_256_rounds(0, W, A0, B0, C0, D0, E0, F0, G0, H0, A1, B1, C1, D1, E1, F1, G1, H1),
		sha512_256_accumulate_state([A0,B0,C0,D0,E0,F0,G0,H0], [A1,B1,C1,D1,E1,F1,G1,H1], [A,B,C,D,E,F,G,H]).

	sha512_256_rounds(80, _, A, B, C, D, E, F, G, H, A, B, C, D, E, F, G, H) :-
		!.
	sha512_256_rounds(I, W, A0, B0, C0, D0, E0, F0, G0, H0, A, B, C, D, E, F, G, H) :-
		list::nth0(I, W, WI),
		sha512_256_k(I, KI),
		sha512_256_sigma1(E0, S1),
		and64(E0, F0, EF),
		not64(E0, NE),
		and64(NE, G0, NG),
		xor64(EF, NG, Ch),
		add64(H0, S1, T1_0),
		add64(T1_0, Ch, T1_1),
		add64(T1_1, KI, T1_2),
		add64(T1_2, WI, T1),
		sha512_256_sigma0(A0, S0),
		and64(A0, B0, AB),
		and64(A0, C0, AC),
		and64(B0, C0, BC),
		or64(AB, AC, ABAC),
		or64(ABAC, BC, Maj),
		add64(S0, Maj, T2),
		add64(T1, T2, A1),
		add64(D0, T1, E1),
		NextI is I + 1,
		sha512_256_rounds(NextI, W, A1, A0, B0, C0, E1, E0, F0, G0, A, B, C, D, E, F, G, H).

	sha512_256_accumulate_state([], [], []).
	sha512_256_accumulate_state([StateWord| StateWords], [WorkingWord| WorkingWords], [Word| Words]) :-
		add64(StateWord, WorkingWord, Word),
		sha512_256_accumulate_state(StateWords, WorkingWords, Words).

	sha512_256_sigma0(X, Sigma) :-
		sha512_256_ror64(X, 28, A),
		sha512_256_ror64(X, 34, B),
		sha512_256_ror64(X, 39, C),
		xor64(B, C, BC),
		xor64(A, BC, Sigma).

	sha512_256_sigma1(X, Sigma) :-
		sha512_256_ror64(X, 14, A),
		sha512_256_ror64(X, 18, B),
		sha512_256_ror64(X, 41, C),
		xor64(B, C, BC),
		xor64(A, BC, Sigma).

	sha512_256_gamma0(X, Gamma) :-
		sha512_256_ror64(X, 1, A),
		sha512_256_ror64(X, 8, B),
		shr64(X, 7, C),
		xor64(B, C, BC),
		xor64(A, BC, Gamma).

	sha512_256_gamma1(X, Gamma) :-
		sha512_256_ror64(X, 19, A),
		sha512_256_ror64(X, 61, B),
		shr64(X, 6, C),
		xor64(B, C, BC),
		xor64(A, BC, Gamma).

	sha512_256_ror64(Value, Shift, Rotated) :-
		LeftShift is 64 - (Shift /\ 63),
		rol64(Value, LeftShift, Rotated).

	extend_sha512_256_words(80, Words, [], Words) :-
		!.
	extend_sha512_256_words(Index, Words0, [Word| Tail1], Words) :-
		I2 is Index - 2,
		I7 is Index - 7,
		I15 is Index - 15,
		I16 is Index - 16,
		list::nth0(I2, Words0, W2),
		list::nth0(I7, Words0, W7),
		list::nth0(I15, Words0, W15),
		list::nth0(I16, Words0, W16),
		sha512_256_gamma1(W2, G1),
		sha512_256_gamma0(W15, G0),
		add64(G1, W7, T0),
		add64(T0, G0, T1),
		add64(T1, W16, Word),
		NextIndex is Index + 1,
		extend_sha512_256_words(NextIndex, Words0, Tail1, Words).

	sha512_256_state_bytes([W0, W1, W2, W3| _], DigestBytes) :-
		integer_to_big_endian_bytes64(W0, DigestBytes, B1),
		integer_to_big_endian_bytes64(W1, B1, B2),
		integer_to_big_endian_bytes64(W2, B2, B3),
		integer_to_big_endian_bytes64(W3, B3, []).

	sha512_256_block_words([], Tail, Tail).
	sha512_256_block_words([B0, B1, B2, B3, B4, B5, B6, B7| Bytes], [Word| Words], Tail) :-
		sha512_256_big_endian_word64([B0, B1, B2, B3, B4, B5, B6, B7], Word),
		sha512_256_block_words(Bytes, Words, Tail).

	sha512_256_big_endian_word64([B0, B1, B2, B3, B4, B5, B6, B7], Word) :-
		shl64(B0, 56, W0),
		shl64(B1, 48, W1),
		shl64(B2, 40, W2),
		shl64(B3, 32, W3),
		shl64(B4, 24, W4),
		shl64(B5, 16, W5),
		shl64(B6, 8, W6),
		or64(W0, W1, T01),
		or64(W2, W3, T23),
		or64(W4, W5, T45),
		or64(W6, B7, T67),
		or64(T01, T23, T0123),
		or64(T45, T67, T4567),
		or64(T0123, T4567, Word).

	sha512_256_k( 0, 0x428A2F98D728AE22).
	sha512_256_k( 1, 0x7137449123EF65CD).
	sha512_256_k( 2, 0xB5C0FBCFEC4D3B2F).
	sha512_256_k( 3, 0xE9B5DBA58189DBBC).
	sha512_256_k( 4, 0x3956C25BF348B538).
	sha512_256_k( 5, 0x59F111F1B605D019).
	sha512_256_k( 6, 0x923F82A4AF194F9B).
	sha512_256_k( 7, 0xAB1C5ED5DA6D8118).
	sha512_256_k( 8, 0xD807AA98A3030242).
	sha512_256_k( 9, 0x12835B0145706FBE).
	sha512_256_k(10, 0x243185BE4EE4B28C).
	sha512_256_k(11, 0x550C7DC3D5FFB4E2).
	sha512_256_k(12, 0x72BE5D74F27B896F).
	sha512_256_k(13, 0x80DEB1FE3B1696B1).
	sha512_256_k(14, 0x9BDC06A725C71235).
	sha512_256_k(15, 0xC19BF174CF692694).
	sha512_256_k(16, 0xE49B69C19EF14AD2).
	sha512_256_k(17, 0xEFBE4786384F25E3).
	sha512_256_k(18, 0x0FC19DC68B8CD5B5).
	sha512_256_k(19, 0x240CA1CC77AC9C65).
	sha512_256_k(20, 0x2DE92C6F592B0275).
	sha512_256_k(21, 0x4A7484AA6EA6E483).
	sha512_256_k(22, 0x5CB0A9DCBD41FBD4).
	sha512_256_k(23, 0x76F988DA831153B5).
	sha512_256_k(24, 0x983E5152EE66DFAB).
	sha512_256_k(25, 0xA831C66D2DB43210).
	sha512_256_k(26, 0xB00327C898FB213F).
	sha512_256_k(27, 0xBF597FC7BEEF0EE4).
	sha512_256_k(28, 0xC6E00BF33DA88FC2).
	sha512_256_k(29, 0xD5A79147930AA725).
	sha512_256_k(30, 0x06CA6351E003826F).
	sha512_256_k(31, 0x142929670A0E6E70).
	sha512_256_k(32, 0x27B70A8546D22FFC).
	sha512_256_k(33, 0x2E1B21385C26C926).
	sha512_256_k(34, 0x4D2C6DFC5AC42AED).
	sha512_256_k(35, 0x53380D139D95B3DF).
	sha512_256_k(36, 0x650A73548BAF63DE).
	sha512_256_k(37, 0x766A0ABB3C77B2A8).
	sha512_256_k(38, 0x81C2C92E47EDAEE6).
	sha512_256_k(39, 0x92722C851482353B).
	sha512_256_k(40, 0xA2BFE8A14CF10364).
	sha512_256_k(41, 0xA81A664BBC423001).
	sha512_256_k(42, 0xC24B8B70D0F89791).
	sha512_256_k(43, 0xC76C51A30654BE30).
	sha512_256_k(44, 0xD192E819D6EF5218).
	sha512_256_k(45, 0xD69906245565A910).
	sha512_256_k(46, 0xF40E35855771202A).
	sha512_256_k(47, 0x106AA07032BBD1B8).
	sha512_256_k(48, 0x19A4C116B8D2D0C8).
	sha512_256_k(49, 0x1E376C085141AB53).
	sha512_256_k(50, 0x2748774CDF8EEB99).
	sha512_256_k(51, 0x34B0BCB5E19B48A8).
	sha512_256_k(52, 0x391C0CB3C5C95A63).
	sha512_256_k(53, 0x4ED8AA4AE3418ACB).
	sha512_256_k(54, 0x5B9CCA4F7763E373).
	sha512_256_k(55, 0x682E6FF3D6B2B8A3).
	sha512_256_k(56, 0x748F82EE5DEFB2FC).
	sha512_256_k(57, 0x78A5636F43172F60).
	sha512_256_k(58, 0x84C87814A1F0AB72).
	sha512_256_k(59, 0x8CC702081A6439EC).
	sha512_256_k(60, 0x90BEFFFA23631E28).
	sha512_256_k(61, 0xA4506CEBDE82BDE9).
	sha512_256_k(62, 0xBEF9A3F7B2C67915).
	sha512_256_k(63, 0xC67178F2E372532B).
	sha512_256_k(64, 0xCA273ECEEA26619C).
	sha512_256_k(65, 0xD186B8C721C0C207).
	sha512_256_k(66, 0xEADA7DD6CDE0EB1E).
	sha512_256_k(67, 0xF57D4F7FEE6ED178).
	sha512_256_k(68, 0x06F067AA72176FBA).
	sha512_256_k(69, 0x0A637DC5A2C898A6).
	sha512_256_k(70, 0x113F9804BEF90DAE).
	sha512_256_k(71, 0x1B710B35131C471B).
	sha512_256_k(72, 0x28DB77F523047D84).
	sha512_256_k(73, 0x32CAAB7B40C72493).
	sha512_256_k(74, 0x3C9EBE0A15C9BEBC).
	sha512_256_k(75, 0x431D67C49C100D4C).
	sha512_256_k(76, 0x4CC5D4BECB3E42B6).
	sha512_256_k(77, 0x597F299CFC657E2A).
	sha512_256_k(78, 0x5FCB6FAB3AD6FAEC).
	sha512_256_k(79, 0x6C44198C4A475817).

:- end_object.


:- object(sha256,
	implements(hash_digest_protocol)).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-06-01,
		comment is 'SHA-256 hash function.',
		see_also is [md5, sha1]
	]).

	:- uses(hash_common_32, [
		pad_md/4, bytes_hex/2, add32/3, ror32/3, integer_to_big_endian_bytes32/3, big_endian_word32/2
	]).

	digest(Bytes, DigestBytes) :-
		pad_md(big, Bytes, 8, PaddedBytes),
		sha256_blocks(PaddedBytes, [0x6A09E667,0xBB67AE85,0x3C6EF372,0xA54FF53A,0x510E527F,0x9B05688C,0x1F83D9AB,0x5BE0CD19], State),
		state_bytes(State, DigestBytes).

	digest_size(32).

	block_size(64).

	hash(Bytes, Hash) :-
		digest(Bytes, DigestBytes),
		bytes_hex(DigestBytes, Hash).

	sha256_blocks([], State, State).
	sha256_blocks([Byte| Bytes], State0, State) :-
		list::take(64, [Byte| Bytes], Block, Rest),
		block_words_be(Block, Words0, Tail0),
		extend_sha256_words(16, Words0, Tail0, Words),
		sha256_compress(Words, State0, State1),
		sha256_blocks(Rest, State1, State).

	sha256_compress(W, [A0,B0,C0,D0,E0,F0,G0,H0], [A,B,C,D,E,F,G,H]) :-
		sha256_rounds(0, W, A0, B0, C0, D0, E0, F0, G0, H0, A1, B1, C1, D1, E1, F1, G1, H1),
		add32(A0, A1, A),
		add32(B0, B1, B),
		add32(C0, C1, C),
		add32(D0, D1, D),
		add32(E0, E1, E),
		add32(F0, F1, F),
		add32(G0, G1, G),
		add32(H0, H1, H).

	sha256_rounds(64, _, A, B, C, D, E, F, G, H, A, B, C, D, E, F, G, H) :-
		!.
	sha256_rounds(I, W, A0, B0, C0, D0, E0, F0, G0, H0, A, B, C, D, E, F, G, H) :-
		list::nth0(I, W, WI),
		sha256_k(I, KI),
		sha256_sigma1(E0, S1),
		Ch is xor((E0 /\ F0), ((\ E0) /\ G0)) /\ 0xFFFFFFFF,
		T1 is (H0 + S1 + Ch + KI + WI) /\ 0xFFFFFFFF,
		sha256_sigma0(A0, S0),
		Maj is ((A0 /\ B0) \/ (A0 /\ C0) \/ (B0 /\ C0)) /\ 0xFFFFFFFF,
		T2 is (S0 + Maj) /\ 0xFFFFFFFF,
		A1 is (T1 + T2) /\ 0xFFFFFFFF,
		E1 is (D0 + T1) /\ 0xFFFFFFFF,
		NextI is I + 1,
		sha256_rounds(NextI, W, A1, A0, B0, C0, E1, E0, F0, G0, A, B, C, D, E, F, G, H).

	sha256_sigma0(X, Sigma) :-
		ror32(X, 2, A),
		ror32(X, 13, B),
		ror32(X, 22, C),
		Sigma is xor(A, xor(B, C)) /\ 0xFFFFFFFF.

	sha256_sigma1(X, Sigma) :-
		ror32(X, 6, A),
		ror32(X, 11, B),
		ror32(X, 25, C),
		Sigma is xor(A, xor(B, C)) /\ 0xFFFFFFFF.

	sha256_gamma0(X, Gamma) :-
		ror32(X, 7, A),
		ror32(X, 18, B),
		C is X >> 3,
		Gamma is xor(A, xor(B, C)) /\ 0xFFFFFFFF.

	sha256_gamma1(X, Gamma) :-
		ror32(X, 17, A),
		ror32(X, 19, B),
		C is X >> 10,
		Gamma is xor(A, xor(B, C)) /\ 0xFFFFFFFF.

	extend_sha256_words(64, Words, [], Words) :-
		!.
	extend_sha256_words(Index, Words0, [Word| Tail1], Words) :-
		I2 is Index - 2,
		I7 is Index - 7,
		I15 is Index - 15,
		I16 is Index - 16,
		list::nth0(I2, Words0, W2),
		list::nth0(I7, Words0, W7),
		list::nth0(I15, Words0, W15),
		list::nth0(I16, Words0, W16),
		sha256_gamma1(W2, G1),
		sha256_gamma0(W15, G0),
		Word is (G1 + W7 + G0 + W16) /\ 0xFFFFFFFF,
		NextIndex is Index + 1,
		extend_sha256_words(NextIndex, Words0, Tail1, Words).

	state_bytes([], []).
	state_bytes([Word| Words], Bytes) :-
		integer_to_big_endian_bytes32(Word, Bytes, RestBytes),
		state_bytes(Words, RestBytes).

	sha256_k( 0, 0x428A2F98).
	sha256_k( 1, 0x71374491).
	sha256_k( 2, 0xB5C0FBCF).
	sha256_k( 3, 0xE9B5DBA5).
	sha256_k( 4, 0x3956C25B).
	sha256_k( 5, 0x59F111F1).
	sha256_k( 6, 0x923F82A4).
	sha256_k( 7, 0xAB1C5ED5).
	sha256_k( 8, 0xD807AA98).
	sha256_k( 9, 0x12835B01).
	sha256_k(10, 0x243185BE).
	sha256_k(11, 0x550C7DC3).
	sha256_k(12, 0x72BE5D74).
	sha256_k(13, 0x80DEB1FE).
	sha256_k(14, 0x9BDC06A7).
	sha256_k(15, 0xC19BF174).
	sha256_k(16, 0xE49B69C1).
	sha256_k(17, 0xEFBE4786).
	sha256_k(18, 0x0FC19DC6).
	sha256_k(19, 0x240CA1CC).
	sha256_k(20, 0x2DE92C6F).
	sha256_k(21, 0x4A7484AA).
	sha256_k(22, 0x5CB0A9DC).
	sha256_k(23, 0x76F988DA).
	sha256_k(24, 0x983E5152).
	sha256_k(25, 0xA831C66D).
	sha256_k(26, 0xB00327C8).
	sha256_k(27, 0xBF597FC7).
	sha256_k(28, 0xC6E00BF3).
	sha256_k(29, 0xD5A79147).
	sha256_k(30, 0x06CA6351).
	sha256_k(31, 0x14292967).
	sha256_k(32, 0x27B70A85).
	sha256_k(33, 0x2E1B2138).
	sha256_k(34, 0x4D2C6DFC).
	sha256_k(35, 0x53380D13).
	sha256_k(36, 0x650A7354).
	sha256_k(37, 0x766A0ABB).
	sha256_k(38, 0x81C2C92E).
	sha256_k(39, 0x92722C85).
	sha256_k(40, 0xA2BFE8A1).
	sha256_k(41, 0xA81A664B).
	sha256_k(42, 0xC24B8B70).
	sha256_k(43, 0xC76C51A3).
	sha256_k(44, 0xD192E819).
	sha256_k(45, 0xD6990624).
	sha256_k(46, 0xF40E3585).
	sha256_k(47, 0x106AA070).
	sha256_k(48, 0x19A4C116).
	sha256_k(49, 0x1E376C08).
	sha256_k(50, 0x2748774C).
	sha256_k(51, 0x34B0BCB5).
	sha256_k(52, 0x391C0CB3).
	sha256_k(53, 0x4ED8AA4A).
	sha256_k(54, 0x5B9CCA4F).
	sha256_k(55, 0x682E6FF3).
	sha256_k(56, 0x748F82EE).
	sha256_k(57, 0x78A5636F).
	sha256_k(58, 0x84C87814).
	sha256_k(59, 0x8CC70208).
	sha256_k(60, 0x90BEFFFA).
	sha256_k(61, 0xA4506CEB).
	sha256_k(62, 0xBEF9A3F7).
	sha256_k(63, 0xC67178F2).

	block_words_be([], Tail, Tail).
	block_words_be([B0, B1, B2, B3| Bytes], [Word| Words], Tail) :-
		big_endian_word32([B0, B1, B2, B3], Word),
		block_words_be(Bytes, Words, Tail).

:- end_object.
