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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Unit tests for the "block_ciphers" library.'
	]).

	cover(aes128).
	cover(aes192).
	cover(aes256).
	cover(aes_common(_, _, _)).
	cover(block_cipher_modes_common).
	cover(ecb).
	cover(cbc).
	cover(ctr).

	test(aes128_protocol_conformance, deterministic) :-
		conforms_to_protocol(aes128, block_cipher_protocol).

	test(aes192_protocol_conformance, deterministic) :-
		conforms_to_protocol(aes192, block_cipher_protocol).

	test(aes256_protocol_conformance, deterministic) :-
		conforms_to_protocol(aes256, block_cipher_protocol).

	test(aes_prepared_key_protocol_conformance, deterministic) :-
		conforms_to_protocol(aes128, block_cipher_prepared_key_protocol),
		conforms_to_protocol(aes192, block_cipher_prepared_key_protocol),
		conforms_to_protocol(aes256, block_cipher_prepared_key_protocol).

	test(aes_block_sizes, deterministic(Sizes == [16, 16, 16])) :-
		aes128::block_size(Size128),
		aes192::block_size(Size192),
		aes256::block_size(Size256),
		Sizes = [Size128, Size192, Size256].

	test(aes_key_sizes, deterministic(Sizes == [16, 24, 32])) :-
		aes128::key_size(Size128),
		aes192::key_size(Size192),
		aes256::key_size(Size256),
		Sizes = [Size128, Size192, Size256].

	test(aes128_fips_197, deterministic(Ciphertext == [0x69,0xC4,0xE0,0xD8,0x6A,0x7B,0x04,0x30,0xD8,0xCD,0xB7,0x80,0x70,0xB4,0xC5,0x5A])) :-
		aes128::encrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F],
			[0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF],
			Ciphertext
		).

	test(aes192_fips_197, deterministic(Ciphertext == [0xDD,0xA9,0x7C,0xA4,0x86,0x4C,0xDF,0xE0,0x6E,0xAF,0x70,0xA0,0xEC,0x0D,0x71,0x91])) :-
		aes192::encrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17],
			[0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF],
			Ciphertext
		).

	test(aes256_fips_197, deterministic(Ciphertext == [0x8E,0xA2,0xB7,0xCA,0x51,0x67,0x45,0xBF,0xEA,0xFC,0x49,0x90,0x4B,0x49,0x60,0x89])) :-
		aes256::encrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F],
			[0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF],
			Ciphertext
		).

	test(aes128_fips_197_decrypt, deterministic(Plaintext == [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF])) :-
		aes128::decrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F],
			[0x69,0xC4,0xE0,0xD8,0x6A,0x7B,0x04,0x30,0xD8,0xCD,0xB7,0x80,0x70,0xB4,0xC5,0x5A],
			Plaintext
		).

	test(aes192_fips_197_decrypt, deterministic(Plaintext == [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF])) :-
		aes192::decrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17],
			[0xDD,0xA9,0x7C,0xA4,0x86,0x4C,0xDF,0xE0,0x6E,0xAF,0x70,0xA0,0xEC,0x0D,0x71,0x91],
			Plaintext
		).

	test(aes256_fips_197_decrypt, deterministic(Plaintext == [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF])) :-
		aes256::decrypt_block(
			[0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F],
			[0x8E,0xA2,0xB7,0xCA,0x51,0x67,0x45,0xBF,0xEA,0xFC,0x49,0x90,0x4B,0x49,0x60,0x89],
			Plaintext
		).

	test(aes128_prepared_key_round_trip, deterministic(Decrypted == Plaintext)) :-
		Key = [0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F],
		Plaintext = [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xAA,0xBB,0xCC,0xDD,0xEE,0xFF],
		aes128::prepare_key(Key, PreparedKey),
		aes128::encrypt_prepared_block(PreparedKey, Plaintext, Ciphertext),
		aes128::decrypt_prepared_block(PreparedKey, Ciphertext, Decrypted).

	test(aes_cross_object_prepared_key, error(domain_error(aes_prepared_key, _))) :-
		zero_key(Key),
		zero_key(Block),
		aes128::prepare_key(Key, PreparedKey),
		aes192::encrypt_prepared_block(PreparedKey, Block, _).

	test(aes_partial_prepared_key, error(domain_error(aes_prepared_key, _))) :-
		zero_key(Block),
		aes128::encrypt_prepared_block(aes_prepared(_, _, _), Block, _).

	test(aes128_nist_sp_800_38a, deterministic(Ciphertext == [0x3A,0xD7,0x7B,0xB4,0x0D,0x7A,0x36,0x60,0xA8,0x9E,0xCA,0xF3,0x24,0x66,0xEF,0x97])) :-
		aes128::encrypt_block(
			[0x2B,0x7E,0x15,0x16,0x28,0xAE,0xD2,0xA6,0xAB,0xF7,0x15,0x88,0x09,0xCF,0x4F,0x3C],
			[0x6B,0xC1,0xBE,0xE2,0x2E,0x40,0x9F,0x96,0xE9,0x3D,0x7E,0x11,0x73,0x93,0x17,0x2A],
			Ciphertext
		).

	test(aes192_nist_sp_800_38a, deterministic(Ciphertext == [0xBD,0x33,0x4F,0x1D,0x6E,0x45,0xF2,0x5F,0xF7,0x12,0xA2,0x14,0x57,0x1F,0xA5,0xCC])) :-
		aes192::encrypt_block(
			[0x8E,0x73,0xB0,0xF7,0xDA,0x0E,0x64,0x52,0xC8,0x10,0xF3,0x2B,0x80,0x90,0x79,0xE5,0x62,0xF8,0xEA,0xD2,0x52,0x2C,0x6B,0x7B],
			[0x6B,0xC1,0xBE,0xE2,0x2E,0x40,0x9F,0x96,0xE9,0x3D,0x7E,0x11,0x73,0x93,0x17,0x2A],
			Ciphertext
		).

	test(aes256_nist_sp_800_38a, deterministic(Ciphertext == [0xF3,0xEE,0xD1,0xBD,0xB5,0xD2,0xA0,0x3C,0x06,0x4B,0x5A,0x7E,0x3D,0xB1,0x81,0xF8])) :-
		aes256::encrypt_block(
			[0x60,0x3D,0xEB,0x10,0x15,0xCA,0x71,0xBE,0x2B,0x73,0xAE,0xF0,0x85,0x7D,0x77,0x81,0x1F,0x35,0x2C,0x07,0x3B,0x61,0x08,0xD7,0x2D,0x98,0x10,0xA3,0x09,0x14,0xDF,0xF4],
			[0x6B,0xC1,0xBE,0xE2,0x2E,0x40,0x9F,0x96,0xE9,0x3D,0x7E,0x11,0x73,0x93,0x17,0x2A],
			Ciphertext
		).

	test(aes128_variable_key, error(instantiation_error)) :-
		aes128::encrypt_block(_, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_partial_key, error(instantiation_error)) :-
		aes128::encrypt_block([0| _], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_non_list_key, error(type_error(list(byte,16), foo))) :-
		aes128::encrypt_block(foo, [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_short_key, error(type_error(list(byte,16), [0]))) :-
		aes128::encrypt_block([0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_long_key, error(type_error(list(byte,16), [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes192_short_key, error(type_error(list(byte,24), [0]))) :-
		aes192::encrypt_block([0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes256_short_key, error(type_error(list(byte,32), [0]))) :-
		aes256::encrypt_block([0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_non_integer_key_byte, error(type_error(integer, a))) :-
		aes128::encrypt_block([a,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_invalid_key_byte, error(domain_error(byte, 256))) :-
		aes128::encrypt_block([256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_variable_plaintext, error(instantiation_error)) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _, _).

	test(aes128_partial_plaintext, error(instantiation_error)) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0| _], _).

	test(aes128_non_list_plaintext, error(type_error(list(byte,16), foo))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], foo, _).

	test(aes128_short_plaintext, error(type_error(list(byte,16), [0]))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0], _).

	test(aes128_long_plaintext, error(type_error(list(byte,16), [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_non_integer_plaintext_byte, error(type_error(integer, a))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [a,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	test(aes128_invalid_plaintext_byte, error(domain_error(byte, 256))) :-
		aes128::encrypt_block([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], _).

	% NIST SP 800-38A mode vectors

	test(ecb_aes128_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes128, ecb, Key, _, Plaintext, Ciphertext),
		ecb::encrypt(aes128, Key, Plaintext, Ciphertext),
		ecb::decrypt(aes128, Key, Ciphertext, Decrypted).

	test(ecb_aes192_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes192, ecb, Key, _, Plaintext, Ciphertext),
		ecb::encrypt(aes192, Key, Plaintext, Ciphertext),
		ecb::decrypt(aes192, Key, Ciphertext, Decrypted).

	test(ecb_aes256_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes256, ecb, Key, _, Plaintext, Ciphertext),
		ecb::encrypt(aes256, Key, Plaintext, Ciphertext),
		ecb::decrypt(aes256, Key, Ciphertext, Decrypted).

	test(cbc_aes128_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes128, cbc, Key, IV, Plaintext, Ciphertext),
		cbc::encrypt(aes128, Key, IV, Plaintext, Ciphertext),
		cbc::decrypt(aes128, Key, IV, Ciphertext, Decrypted).

	test(cbc_aes192_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes192, cbc, Key, IV, Plaintext, Ciphertext),
		cbc::encrypt(aes192, Key, IV, Plaintext, Ciphertext),
		cbc::decrypt(aes192, Key, IV, Ciphertext, Decrypted).

	test(cbc_aes256_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes256, cbc, Key, IV, Plaintext, Ciphertext),
		cbc::encrypt(aes256, Key, IV, Plaintext, Ciphertext),
		cbc::decrypt(aes256, Key, IV, Ciphertext, Decrypted).

	test(ctr_aes128_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes128, ctr, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes128, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes128, Key, Counter, Ciphertext, Decrypted).

	test(ctr_aes192_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes192, ctr, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes192, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes192, Key, Counter, Ciphertext, Decrypted).

	test(ctr_aes256_nist_sp_800_38a, deterministic(Decrypted == Plaintext)) :-
		mode_vector(aes256, ctr, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes256, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes256, Key, Counter, Ciphertext, Decrypted).

	test(ecb_pkcs7_empty, deterministic(Plaintext == [])) :-
		zero_key(Key),
		ecb::encrypt_padded(aes128, Key, [], Ciphertext),
		ecb::decrypt_padded(aes128, Key, Ciphertext, Plaintext).

	test(ecb_pkcs7_aligned, deterministic(Decrypted == Plaintext)) :-
		zero_key(Key),
		Plaintext = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		ecb::encrypt_padded(aes128, Key, Plaintext, Ciphertext),
		ecb::decrypt_padded(aes128, Key, Ciphertext, Decrypted).

	test(cbc_pkcs7_partial, deterministic(Decrypted == Plaintext)) :-
		zero_key(Key),
		zero_key(IV),
		Plaintext = [1,2,3,4,5],
		cbc::encrypt_padded(aes128, Key, IV, Plaintext, Ciphertext),
		cbc::decrypt_padded(aes128, Key, IV, Ciphertext, Decrypted).

	test(ecb_raw_empty, deterministic(Ciphertext-Plaintext == []-[])) :-
		zero_key(Key),
		ecb::encrypt(aes128, Key, [], Ciphertext),
		ecb::decrypt(aes128, Key, [], Plaintext).

	test(cbc_raw_empty, deterministic(Ciphertext-Plaintext == []-[])) :-
		zero_key(Key),
		zero_key(IV),
		cbc::encrypt(aes128, Key, IV, [], Ciphertext),
		cbc::decrypt(aes128, Key, IV, [], Plaintext).

	test(ctr_partial_block, deterministic(Decrypted == Plaintext)) :-
		zero_key(Key),
		zero_key(Counter),
		Plaintext = [1,2,3,4,5],
		ctr::crypt(aes128, Key, Counter, Plaintext, Ciphertext),
		ctr::crypt(aes128, Key, Counter, Ciphertext, Decrypted).

	test(ctr_whole_counter_wrap, deterministic(Output == Expected)) :-
		zero_key(Key),
		FullCounter = [255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255],
		zero_key(ZeroCounter),
		zero_blocks(Input),
		aes128::encrypt_block(Key, FullCounter, FirstBlock),
		aes128::encrypt_block(Key, ZeroCounter, SecondBlock),
		append(FirstBlock, SecondBlock, Expected),
		ctr::crypt(aes128, Key, FullCounter, Input, Output).

	test(ecb_raw_non_aligned, error(domain_error(block_aligned_byte_length(16), [0]))) :-
		zero_key(Key),
		ecb::encrypt(aes128, Key, [0], _).

	test(cbc_short_iv, error(type_error(list(byte,16), [0]))) :-
		zero_key(Key),
		cbc::encrypt(aes128, Key, [0], [], _).

	test(ctr_non_cipher, error(domain_error(block_cipher, foo))) :-
		ctr::crypt(foo, [], [], [], _).

	test(ecb_padded_empty_ciphertext, error(domain_error(non_empty_ciphertext, []))) :-
		zero_key(Key),
		ecb::decrypt_padded(aes128, Key, [], _).

	test(ecb_pkcs7_zero_padding, error(domain_error(pkcs7_padding, Ciphertext))) :-
		zero_key(Key),
		zero_key(InvalidPaddedPlaintext),
		ecb::encrypt(aes128, Key, InvalidPaddedPlaintext, Ciphertext),
		ecb::decrypt_padded(aes128, Key, Ciphertext, _).

	test(ecb_pkcs7_inconsistent_padding, error(domain_error(pkcs7_padding, Ciphertext))) :-
		zero_key(Key),
		InvalidPaddedPlaintext = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2],
		ecb::encrypt(aes128, Key, InvalidPaddedPlaintext, Ciphertext),
		ecb::decrypt_padded(aes128, Key, Ciphertext, _).

	test(ctr_variable_cipher, error(instantiation_error)) :-
		ctr::crypt(_, [], [], [], _).

	% auxiliary predicates

	zero_key([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]).

	zero_blocks([
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	]).

	mode_vector(Cipher, Mode, Key, Initial, Plaintext, Ciphertext) :-
		key_hex(Cipher, KeyHex),
		initial_hex(Mode, InitialHex),
		plaintext_hex(PlaintextHex),
		ciphertext_hex(Cipher, Mode, CiphertextHex), !,
		hex_bytes(KeyHex, Key),
		hex_bytes(InitialHex, Initial),
		hex_bytes(PlaintextHex, Plaintext),
		hex_bytes(CiphertextHex, Ciphertext).

	key_hex(aes128, '2b7e151628aed2a6abf7158809cf4f3c').
	key_hex(aes192, '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b').
	key_hex(aes256, '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4').

	initial_hex(ecb, '').
	initial_hex(cbc, '000102030405060708090a0b0c0d0e0f').
	initial_hex(ctr, 'f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff').

	plaintext_hex('6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710').

	ciphertext_hex(aes128, ecb, '3ad77bb40d7a3660a89ecaf32466ef97f5d3d58503b9699de785895a96fdbaaf43b1cd7f598ece23881b00e3ed0306887b0c785e27e8ad3f8223207104725dd4').
	ciphertext_hex(aes192, ecb, 'bd334f1d6e45f25ff712a214571fa5cc974104846d0ad3ad7734ecb3ecee4eefef7afd2270e2e60adce0ba2face6444e9a4b41ba738d6c72fb16691603c18e0e').
	ciphertext_hex(aes256, ecb, 'f3eed1bdb5d2a03c064b5a7e3db181f8591ccb10d410ed26dc5ba74a31362870b6ed21b99ca6f4f9f153e7b1beafed1d23304b7a39f9f3ff067d8d8f9e24ecc7').
	ciphertext_hex(aes128, cbc, '7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7').
	ciphertext_hex(aes192, cbc, '4f021db243bc633d7178183a9fa071e8b4d9ada9ad7dedf4e5e738763f69145a571b242012fb7ae07fa9baac3df102e008b0e27988598881d920a9e64f5615cd').
	ciphertext_hex(aes256, cbc, 'f58c4c04d6e5f1ba779eabfb5f7bfbd69cfc4e967edb808d679f777bc6702c7d39f23369a9d9bacfa530e26304231461b2eb05e2c39be9fcda6c19078c6a9d1b').
	ciphertext_hex(aes128, ctr, '874d6191b620e3261bef6864990db6ce9806f66b7970fdff8617187bb9fffdff5ae4df3edbd5d35e5b4f09020db03eab1e031dda2fbe03d1792170a0f3009cee').
	ciphertext_hex(aes192, ctr, '1abc932417521ca24f2b0459fe7e6e0b090339ec0aa6faefd5ccc2c6f4ce8e941e36b26bd1ebc670d1bd1d665620abf74f78a7f6d29809585a97daec58c6b050').
	ciphertext_hex(aes256, ctr, '601ec313775789a5b7a7f504bbf3d228f443e3ca4d62b59aca84e990cacaf5c52b0930daa23de94ce87017ba2d84988ddfc9c58db67aada613c2dd08457941a6').

	hex_bytes(Hex, Bytes) :-
		atom_codes(Hex, Codes),
		hex_codes(Codes, Bytes).

	hex_codes([], []).
	hex_codes([HighCode, LowCode| Codes], [Byte| Bytes]) :-
		hex_nibble(HighCode, High),
		hex_nibble(LowCode, Low),
		Byte is (High << 4) \/ Low,
		hex_codes(Codes, Bytes).

	hex_nibble(0'0, 0). hex_nibble(0'1, 1). hex_nibble(0'2, 2). hex_nibble(0'3, 3).
	hex_nibble(0'4, 4). hex_nibble(0'5, 5). hex_nibble(0'6, 6). hex_nibble(0'7, 7).
	hex_nibble(0'8, 8). hex_nibble(0'9, 9). hex_nibble(0'a, 10). hex_nibble(0'b, 11).
	hex_nibble(0'c, 12). hex_nibble(0'd, 13). hex_nibble(0'e, 14). hex_nibble(0'f, 15).

	:- uses(list, [append/3]).

:- end_object.
