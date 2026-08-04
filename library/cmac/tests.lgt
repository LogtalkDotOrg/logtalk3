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


:- object(cmac_test_cipher_64,
	implements(block_cipher_prepared_key_protocol)).

	block_size(8).
	key_size(1).

	encrypt_block(Key, Block, EncryptedBlock) :-
		prepare_key(Key, PreparedKey),
		encrypt_prepared_block(PreparedKey, Block, EncryptedBlock).

	decrypt_block(Key, Block, DecryptedBlock) :-
		prepare_key(Key, PreparedKey),
		decrypt_prepared_block(PreparedKey, Block, DecryptedBlock).

	prepare_key([Key], cmac_test_prepared(Key)).

	encrypt_prepared_block(cmac_test_prepared(_), [Byte| Bytes], [EncryptedByte| Bytes]) :-
		EncryptedByte is xor(Byte, 0x80).

	decrypt_prepared_block(PreparedKey, Block, DecryptedBlock) :-
		encrypt_prepared_block(PreparedKey, Block, DecryptedBlock).

:- end_object.


:- object(cmac_test_cipher_unsupported,
	implements(block_cipher_prepared_key_protocol)).

	block_size(12).
	key_size(1).
	encrypt_block(_, _, _).
	decrypt_block(_, _, _).
	prepare_key(_, cmac_test_prepared).
	encrypt_prepared_block(_, _, _).
	decrypt_prepared_block(_, _, _).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
		comment is 'Unit tests for the "cmac" library.'
	]).

	cover(cmac).

	test(cmac_protocol_conformance, deterministic) :-
		conforms_to_protocol(cmac, cmac_protocol).

	test(aes128_rfc4493_example_1, deterministic(HexDigest == 'bb1d6929e95937287fa37d129b756746')) :-
		aes128_rfc4493_key(Key),
		cmac::hex_digest(aes128, Key, [], HexDigest).

	test(aes128_rfc4493_example_2, deterministic(HexDigest == '070a16b46b4d4144f79bdd9dd04a287c')) :-
		aes128_rfc4493_key(Key),
		rfc4493_message(Message),
		prefix(16, Message, Prefix),
		cmac::hex_digest(aes128, Key, Prefix, HexDigest).

	test(aes128_rfc4493_example_3, deterministic(HexDigest == 'dfa66747de9ae63030ca32611497c827')) :-
		aes128_rfc4493_key(Key),
		rfc4493_message(Message),
		prefix(40, Message, Prefix),
		cmac::hex_digest(aes128, Key, Prefix, HexDigest).

	test(aes128_rfc4493_example_4, deterministic(HexDigest == '51f0bebf7e3b9d92fc49741779363cfe')) :-
		aes128_rfc4493_key(Key),
		rfc4493_message(Message),
		cmac::hex_digest(aes128, Key, Message, HexDigest).

	test(aes192_nist_example_1, deterministic(HexDigest == 'd17ddf46adaacde531cac483de7a9367')) :-
		aes192_nist_key(Key),
		cmac::hex_digest(aes192, Key, [], HexDigest).

	test(aes192_nist_example_3, deterministic(HexDigest == '8a1de5be2eb31aad089a82e6ee908b0e')) :-
		aes192_nist_key(Key),
		rfc4493_message(Message),
		prefix(40, Message, Prefix),
		cmac::hex_digest(aes192, Key, Prefix, HexDigest).

	test(aes256_nist_example_1, deterministic(HexDigest == '028962f61b7bf89efc6b551f4667d983')) :-
		aes256_nist_key(Key),
		cmac::hex_digest(aes256, Key, [], HexDigest).

	test(aes256_nist_example_4, deterministic(HexDigest == 'e1992190549f6ed5696a2c056c315410')) :-
		aes256_nist_key(Key),
		rfc4493_message(Message),
		cmac::hex_digest(aes256, Key, Message, HexDigest).

	test(aes128_truncated_raw_digest, deterministic(Digest == [0x07])) :-
		aes128_rfc4493_key(Key),
		rfc4493_message(Message),
		prefix(16, Message, Prefix),
		cmac::digest(aes128, Key, Prefix, 1, Digest).

	test(aes128_truncated_hex_digest, deterministic(HexDigest == '070a16b46b4d4144')) :-
		aes128_rfc4493_key(Key),
		rfc4493_message(Message),
		prefix(16, Message, Prefix),
		cmac::hex_digest(aes128, Key, Prefix, 8, HexDigest).

	test(aes128_full_raw_digest, deterministic(Digest == [0xBB,0x1D,0x69,0x29,0xE9,0x59,0x37,0x28,0x7F,0xA3,0x7D,0x12,0x9B,0x75,0x67,0x46])) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [], Digest).

	test(cmac_64_empty_message, deterministic(Digest == [0,0,0,0,0,0,0,0x36])) :-
		cmac::digest(cmac_test_cipher_64, [0], [], Digest).

	test(cmac_64_complete_block, deterministic(Digest == [0x81,2,3,4,5,6,7,0x13])) :-
		cmac::digest(cmac_test_cipher_64, [0], [1,2,3,4,5,6,7,8], Digest).

	test(cmac_64_partial_block, deterministic(Digest == [0x81,2,0x80,0,0,0,0,0x36])) :-
		cmac::digest(cmac_test_cipher_64, [0], [1,2], Digest).

	test(cmac_variable_cipher, error(instantiation_error)) :-
		cmac::digest(_, [], [], _).

	test(cmac_non_conforming_cipher, error(domain_error(cmac_cipher, crc32b))) :-
		cmac::digest(crc32b, [], [], _).

	test(cmac_unsupported_block_size, error(domain_error(cmac_block_size, 12))) :-
		cmac::digest(cmac_test_cipher_unsupported, [], [], _).

	test(cmac_invalid_aes_key, error(type_error(list(byte,16), [0]))) :-
		cmac::digest(aes128, [0], [], _).

	test(cmac_variable_message, error(instantiation_error)) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, _, _).

	test(cmac_partial_message, error(instantiation_error)) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [0| _], _).

	test(cmac_non_list_message, error(type_error(list(byte), foo))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, foo, _).

	test(cmac_non_integer_message_byte, error(type_error(integer, a))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [a], _).

	test(cmac_invalid_message_byte, error(domain_error(byte, 256))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [256], _).

	test(cmac_variable_length, error(instantiation_error)) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [], _, _).

	test(cmac_non_integer_length, error(type_error(integer, one))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [], one, _).

	test(cmac_zero_length, error(domain_error(cmac_output_length(1,16), 0))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [], 0, _).

	test(cmac_oversized_length, error(domain_error(cmac_output_length(1,16), 17))) :-
		aes128_rfc4493_key(Key),
		cmac::digest(aes128, Key, [], 17, _).

	aes128_rfc4493_key([
		0x2B,0x7E,0x15,0x16,0x28,0xAE,0xD2,0xA6,
		0xAB,0xF7,0x15,0x88,0x09,0xCF,0x4F,0x3C
	]).

	aes192_nist_key([
		0x8E,0x73,0xB0,0xF7,0xDA,0x0E,0x64,0x52,0xC8,0x10,0xF3,0x2B,
		0x80,0x90,0x79,0xE5,0x62,0xF8,0xEA,0xD2,0x52,0x2C,0x6B,0x7B
	]).

	aes256_nist_key([
		0x60,0x3D,0xEB,0x10,0x15,0xCA,0x71,0xBE,0x2B,0x73,0xAE,0xF0,0x85,0x7D,0x77,0x81,
		0x1F,0x35,0x2C,0x07,0x3B,0x61,0x08,0xD7,0x2D,0x98,0x10,0xA3,0x09,0x14,0xDF,0xF4
	]).

	rfc4493_message([
		0x6B,0xC1,0xBE,0xE2,0x2E,0x40,0x9F,0x96,0xE9,0x3D,0x7E,0x11,0x73,0x93,0x17,0x2A,
		0xAE,0x2D,0x8A,0x57,0x1E,0x03,0xAC,0x9C,0x9E,0xB7,0x6F,0xAC,0x45,0xAF,0x8E,0x51,
		0x30,0xC8,0x1C,0x46,0xA3,0x5C,0xE4,0x11,0xE5,0xFB,0xC1,0x19,0x1A,0x0A,0x52,0xEF,
		0xF6,0x9F,0x24,0x45,0xDF,0x4F,0x9B,0x17,0xAD,0x2B,0x41,0x7B,0xE6,0x6C,0x37,0x10
	]).

	prefix(0, _, []) :-
		!.
	prefix(Count, [Byte| Bytes], [Byte| Prefix]) :-
		NextCount is Count - 1,
		prefix(NextCount, Bytes, Prefix).

:- end_object.
