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


:- object(aes_common(_KeySize_, _Nk_, _Nr_),
	implements(block_cipher_prepared_key_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'AES block cipher implementation shared by the AES-128, AES-192, and AES-256 objects.',
		parameters is [
			'KeySize' - 'Key size in bytes.',
			'Nk' - 'Key size in 32-bit words.',
			'Nr' - 'Number of encryption rounds.'
		]
	]).

	:- uses(list, [
		append/3, nth0/3, reverse/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	block_size(16).

	key_size(_KeySize_).

	encrypt_block(Key, PlaintextBlock, CiphertextBlock) :-
		prepare_key(Key, PreparedKey),
		encrypt_prepared_block(PreparedKey, PlaintextBlock, CiphertextBlock).

	decrypt_block(Key, CiphertextBlock, PlaintextBlock) :-
		prepare_key(Key, PreparedKey),
		decrypt_prepared_block(PreparedKey, CiphertextBlock, PlaintextBlock).

	prepare_key(Key, aes_prepared(_KeySize_, _Nr_, RoundKeys)) :-
		context(Context),
		check(list(byte, _KeySize_), Key, Context),
		expand_key(Key, _Nk_, _Nr_, RoundKeys).

	encrypt_prepared_block(PreparedKey, PlaintextBlock, CiphertextBlock) :-
		check_prepared_key(PreparedKey, RoundKeys),
		context(Context),
		check(list(byte, 16), PlaintextBlock, Context),
		RoundKeys = [InitialRoundKey| EncryptionRoundKeys],
		add_round_key(PlaintextBlock, InitialRoundKey, InitialState),
		encrypt_rounds(EncryptionRoundKeys, InitialState, CiphertextBlock).

	decrypt_prepared_block(PreparedKey, CiphertextBlock, PlaintextBlock) :-
		check_prepared_key(PreparedKey, RoundKeys),
		context(Context),
		check(list(byte, 16), CiphertextBlock, Context),
		reverse(RoundKeys, [FinalRoundKey| DecryptionRoundKeys]),
		add_round_key(CiphertextBlock, FinalRoundKey, InitialState),
		decrypt_rounds(DecryptionRoundKeys, InitialState, PlaintextBlock).

	check_prepared_key(PreparedKey, RoundKeys) :-
		(	var(PreparedKey) ->
			instantiation_error
		;	PreparedKey = aes_prepared(KeySize, Nr, RoundKeys),
			KeySize == _KeySize_,
			Nr == _Nr_,
			RoundCount is _Nr_ + 1,
			valid(list(list(byte, 16), RoundCount), RoundKeys) ->
			true
		;	domain_error(aes_prepared_key, PreparedKey)
		).

	expand_key(Key, Nk, Nr, RoundKeys) :-
		key_words(Key, Words0),
		TotalWords is 4 * (Nr + 1),
		expand_words(Nk, TotalWords, Nk, Words0, Words),
		round_keys(Words, RoundKeys).

	key_words([], []).
	key_words([B0, B1, B2, B3| Bytes], [[B0, B1, B2, B3]| Words]) :-
		key_words(Bytes, Words).

	expand_words(Index, TotalWords, _, Words, Words) :-
		Index >= TotalWords,
		!.
	expand_words(Index, TotalWords, Nk, Words0, Words) :-
		PreviousIndex is Index - 1,
		nth0(PreviousIndex, Words0, PreviousWord),
		Remainder is Index mod Nk,
		transform_expansion_word(Remainder, Index, Nk, PreviousWord, ExpansionWord),
		BaseIndex is Index - Nk,
		nth0(BaseIndex, Words0, BaseWord),
		xor_word(BaseWord, ExpansionWord, Word),
		append(Words0, [Word], Words1),
		NextIndex is Index + 1,
		expand_words(NextIndex, TotalWords, Nk, Words1, Words).

	transform_expansion_word(0, Index, Nk, Word0, Word) :-
		!,
		rotate_word(Word0, RotatedWord),
		substitute_word(RotatedWord, [B0, B1, B2, B3]),
		RoundConstantIndex is Index div Nk,
		round_constant(RoundConstantIndex, RoundConstant),
		C0 is xor(B0, RoundConstant),
		Word = [C0, B1, B2, B3].
	transform_expansion_word(4, _, 8, Word0, Word) :-
		!,
		substitute_word(Word0, Word).
	transform_expansion_word(_, _, _, Word, Word).

	rotate_word([B0, B1, B2, B3], [B1, B2, B3, B0]).

	substitute_word([], []).
	substitute_word([Byte| Bytes], [SubstitutedByte| SubstitutedBytes]) :-
		sbox(Byte, SubstitutedByte),
		substitute_word(Bytes, SubstitutedBytes).

	xor_word([], [], []).
	xor_word([A| As], [B| Bs], [C| Cs]) :-
		C is xor(A, B),
		xor_word(As, Bs, Cs).

	round_constant(1, 1) :-
		!.
	round_constant(Index, RoundConstant) :-
		PreviousIndex is Index - 1,
		round_constant(PreviousIndex, PreviousRoundConstant),
		xtime(PreviousRoundConstant, RoundConstant).

	round_keys([], []).
	round_keys(
		[[A0, A1, A2, A3], [B0, B1, B2, B3], [C0, C1, C2, C3], [D0, D1, D2, D3]| Words],
		[[A0, A1, A2, A3, B0, B1, B2, B3, C0, C1, C2, C3, D0, D1, D2, D3]| RoundKeys]
	) :-
		round_keys(Words, RoundKeys).

	encrypt_rounds([FinalRoundKey], State0, CiphertextBlock) :-
		!,
		sub_bytes(State0, State1),
		shift_rows(State1, State2),
		add_round_key(State2, FinalRoundKey, CiphertextBlock).
	encrypt_rounds([RoundKey| RoundKeys], State0, CiphertextBlock) :-
		sub_bytes(State0, State1),
		shift_rows(State1, State2),
		mix_columns(State2, State3),
		add_round_key(State3, RoundKey, State4),
		encrypt_rounds(RoundKeys, State4, CiphertextBlock).

	decrypt_rounds([InitialRoundKey], State0, PlaintextBlock) :-
		!,
		inverse_shift_rows(State0, State1),
		inverse_sub_bytes(State1, State2),
		add_round_key(State2, InitialRoundKey, PlaintextBlock).
	decrypt_rounds([RoundKey| RoundKeys], State0, PlaintextBlock) :-
		inverse_shift_rows(State0, State1),
		inverse_sub_bytes(State1, State2),
		add_round_key(State2, RoundKey, State3),
		inverse_mix_columns(State3, State4),
		decrypt_rounds(RoundKeys, State4, PlaintextBlock).

	sub_bytes([], []).
	sub_bytes([Byte| Bytes], [SubstitutedByte| SubstitutedBytes]) :-
		sbox(Byte, SubstitutedByte),
		sub_bytes(Bytes, SubstitutedBytes).

	shift_rows(
		[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15],
		[S0, S5, S10, S15, S4, S9, S14, S3, S8, S13, S2, S7, S12, S1, S6, S11]
	).

	inverse_sub_bytes([], []).
	inverse_sub_bytes([Byte| Bytes], [SubstitutedByte| SubstitutedBytes]) :-
		inverse_sbox(Byte, SubstitutedByte),
		inverse_sub_bytes(Bytes, SubstitutedBytes).

	inverse_shift_rows(
		[S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15],
		[S0, S13, S10, S7, S4, S1, S14, S11, S8, S5, S2, S15, S12, S9, S6, S3]
	).

	mix_columns([], []).
	mix_columns([A0, A1, A2, A3| Columns], [B0, B1, B2, B3| MixedColumns]) :-
		xtime(A0, A0x2),
		xtime(A1, A1x2),
		xtime(A2, A2x2),
		xtime(A3, A3x2),
		A0x3 is xor(A0x2, A0),
		A1x3 is xor(A1x2, A1),
		A2x3 is xor(A2x2, A2),
		A3x3 is xor(A3x2, A3),
		B0 is xor(A0x2, xor(A1x3, xor(A2, A3))),
		B1 is xor(A0, xor(A1x2, xor(A2x3, A3))),
		B2 is xor(A0, xor(A1, xor(A2x2, A3x3))),
		B3 is xor(A0x3, xor(A1, xor(A2, A3x2))),
		mix_columns(Columns, MixedColumns).

	inverse_mix_columns([], []).
	inverse_mix_columns([A0, A1, A2, A3| Columns], [B0, B1, B2, B3| MixedColumns]) :-
		gf_products(A0, A0x9, A0x11, A0x13, A0x14),
		gf_products(A1, A1x9, A1x11, A1x13, A1x14),
		gf_products(A2, A2x9, A2x11, A2x13, A2x14),
		gf_products(A3, A3x9, A3x11, A3x13, A3x14),
		B0 is xor(A0x14, xor(A1x11, xor(A2x13, A3x9))),
		B1 is xor(A0x9, xor(A1x14, xor(A2x11, A3x13))),
		B2 is xor(A0x13, xor(A1x9, xor(A2x14, A3x11))),
		B3 is xor(A0x11, xor(A1x13, xor(A2x9, A3x14))),
		inverse_mix_columns(Columns, MixedColumns).

	gf_products(Byte, Product9, Product11, Product13, Product14) :-
		xtime(Byte, Product2),
		xtime(Product2, Product4),
		xtime(Product4, Product8),
		Product9 is xor(Product8, Byte),
		Product11 is xor(Product8, xor(Product2, Byte)),
		Product13 is xor(Product8, xor(Product4, Byte)),
		Product14 is xor(Product8, xor(Product4, Product2)).

	add_round_key([], [], []).
	add_round_key([StateByte| StateBytes], [KeyByte| KeyBytes], [Byte| Bytes]) :-
		Byte is xor(StateByte, KeyByte),
		add_round_key(StateBytes, KeyBytes, Bytes).

	xtime(Byte, Product) :-
		Shifted is Byte << 1,
		(	Byte /\ 0x80 =:= 0 ->
			Product is Shifted /\ 0xFF
		;	Product is xor(Shifted, 0x1B) /\ 0xFF
		).

	sbox(Byte, SubstitutedByte) :-
		Row is Byte >> 4,
		Column is Byte /\ 0x0F,
		sbox_row(Row, Values),
		nth0(Column, Values, SubstitutedByte).

	sbox_row( 0, [0x63,0x7C,0x77,0x7B,0xF2,0x6B,0x6F,0xC5,0x30,0x01,0x67,0x2B,0xFE,0xD7,0xAB,0x76]).
	sbox_row( 1, [0xCA,0x82,0xC9,0x7D,0xFA,0x59,0x47,0xF0,0xAD,0xD4,0xA2,0xAF,0x9C,0xA4,0x72,0xC0]).
	sbox_row( 2, [0xB7,0xFD,0x93,0x26,0x36,0x3F,0xF7,0xCC,0x34,0xA5,0xE5,0xF1,0x71,0xD8,0x31,0x15]).
	sbox_row( 3, [0x04,0xC7,0x23,0xC3,0x18,0x96,0x05,0x9A,0x07,0x12,0x80,0xE2,0xEB,0x27,0xB2,0x75]).
	sbox_row( 4, [0x09,0x83,0x2C,0x1A,0x1B,0x6E,0x5A,0xA0,0x52,0x3B,0xD6,0xB3,0x29,0xE3,0x2F,0x84]).
	sbox_row( 5, [0x53,0xD1,0x00,0xED,0x20,0xFC,0xB1,0x5B,0x6A,0xCB,0xBE,0x39,0x4A,0x4C,0x58,0xCF]).
	sbox_row( 6, [0xD0,0xEF,0xAA,0xFB,0x43,0x4D,0x33,0x85,0x45,0xF9,0x02,0x7F,0x50,0x3C,0x9F,0xA8]).
	sbox_row( 7, [0x51,0xA3,0x40,0x8F,0x92,0x9D,0x38,0xF5,0xBC,0xB6,0xDA,0x21,0x10,0xFF,0xF3,0xD2]).
	sbox_row( 8, [0xCD,0x0C,0x13,0xEC,0x5F,0x97,0x44,0x17,0xC4,0xA7,0x7E,0x3D,0x64,0x5D,0x19,0x73]).
	sbox_row( 9, [0x60,0x81,0x4F,0xDC,0x22,0x2A,0x90,0x88,0x46,0xEE,0xB8,0x14,0xDE,0x5E,0x0B,0xDB]).
	sbox_row(10, [0xE0,0x32,0x3A,0x0A,0x49,0x06,0x24,0x5C,0xC2,0xD3,0xAC,0x62,0x91,0x95,0xE4,0x79]).
	sbox_row(11, [0xE7,0xC8,0x37,0x6D,0x8D,0xD5,0x4E,0xA9,0x6C,0x56,0xF4,0xEA,0x65,0x7A,0xAE,0x08]).
	sbox_row(12, [0xBA,0x78,0x25,0x2E,0x1C,0xA6,0xB4,0xC6,0xE8,0xDD,0x74,0x1F,0x4B,0xBD,0x8B,0x8A]).
	sbox_row(13, [0x70,0x3E,0xB5,0x66,0x48,0x03,0xF6,0x0E,0x61,0x35,0x57,0xB9,0x86,0xC1,0x1D,0x9E]).
	sbox_row(14, [0xE1,0xF8,0x98,0x11,0x69,0xD9,0x8E,0x94,0x9B,0x1E,0x87,0xE9,0xCE,0x55,0x28,0xDF]).
	sbox_row(15, [0x8C,0xA1,0x89,0x0D,0xBF,0xE6,0x42,0x68,0x41,0x99,0x2D,0x0F,0xB0,0x54,0xBB,0x16]).

	inverse_sbox(Byte, SubstitutedByte) :-
		Row is Byte >> 4,
		Column is Byte /\ 0x0F,
		inverse_sbox_row(Row, Values),
		nth0(Column, Values, SubstitutedByte).

	inverse_sbox_row( 0, [0x52,0x09,0x6A,0xD5,0x30,0x36,0xA5,0x38,0xBF,0x40,0xA3,0x9E,0x81,0xF3,0xD7,0xFB]).
	inverse_sbox_row( 1, [0x7C,0xE3,0x39,0x82,0x9B,0x2F,0xFF,0x87,0x34,0x8E,0x43,0x44,0xC4,0xDE,0xE9,0xCB]).
	inverse_sbox_row( 2, [0x54,0x7B,0x94,0x32,0xA6,0xC2,0x23,0x3D,0xEE,0x4C,0x95,0x0B,0x42,0xFA,0xC3,0x4E]).
	inverse_sbox_row( 3, [0x08,0x2E,0xA1,0x66,0x28,0xD9,0x24,0xB2,0x76,0x5B,0xA2,0x49,0x6D,0x8B,0xD1,0x25]).
	inverse_sbox_row( 4, [0x72,0xF8,0xF6,0x64,0x86,0x68,0x98,0x16,0xD4,0xA4,0x5C,0xCC,0x5D,0x65,0xB6,0x92]).
	inverse_sbox_row( 5, [0x6C,0x70,0x48,0x50,0xFD,0xED,0xB9,0xDA,0x5E,0x15,0x46,0x57,0xA7,0x8D,0x9D,0x84]).
	inverse_sbox_row( 6, [0x90,0xD8,0xAB,0x00,0x8C,0xBC,0xD3,0x0A,0xF7,0xE4,0x58,0x05,0xB8,0xB3,0x45,0x06]).
	inverse_sbox_row( 7, [0xD0,0x2C,0x1E,0x8F,0xCA,0x3F,0x0F,0x02,0xC1,0xAF,0xBD,0x03,0x01,0x13,0x8A,0x6B]).
	inverse_sbox_row( 8, [0x3A,0x91,0x11,0x41,0x4F,0x67,0xDC,0xEA,0x97,0xF2,0xCF,0xCE,0xF0,0xB4,0xE6,0x73]).
	inverse_sbox_row( 9, [0x96,0xAC,0x74,0x22,0xE7,0xAD,0x35,0x85,0xE2,0xF9,0x37,0xE8,0x1C,0x75,0xDF,0x6E]).
	inverse_sbox_row(10, [0x47,0xF1,0x1A,0x71,0x1D,0x29,0xC5,0x89,0x6F,0xB7,0x62,0x0E,0xAA,0x18,0xBE,0x1B]).
	inverse_sbox_row(11, [0xFC,0x56,0x3E,0x4B,0xC6,0xD2,0x79,0x20,0x9A,0xDB,0xC0,0xFE,0x78,0xCD,0x5A,0xF4]).
	inverse_sbox_row(12, [0x1F,0xDD,0xA8,0x33,0x88,0x07,0xC7,0x31,0xB1,0x12,0x10,0x59,0x27,0x80,0xEC,0x5F]).
	inverse_sbox_row(13, [0x60,0x51,0x7F,0xA9,0x19,0xB5,0x4A,0x0D,0x2D,0xE5,0x7A,0x9F,0x93,0xC9,0x9C,0xEF]).
	inverse_sbox_row(14, [0xA0,0xE0,0x3B,0x4D,0xAE,0x2A,0xF5,0xB0,0xC8,0xEB,0xBB,0x3C,0x83,0x53,0x99,0x61]).
	inverse_sbox_row(15, [0x17,0x2B,0x04,0x7E,0xBA,0x77,0xD6,0x26,0xE1,0x69,0x14,0x63,0x55,0x21,0x0C,0x7D]).

:- end_object.
