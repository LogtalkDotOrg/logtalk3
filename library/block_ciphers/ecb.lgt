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

:- object(ecb,
	imports(block_cipher_modes_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Generic Electronic Codebook mode encryption and decryption.'
	]).

	:- public(encrypt/4).
	:- mode(encrypt(+object_identifier, +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(encrypt/4, [
		comment is 'Encrypts block-aligned plaintext without padding.',
		argnames is ['Cipher', 'Key', 'Plaintext', 'Ciphertext']
	]).

	:- public(decrypt/4).
	:- mode(decrypt(+object_identifier, +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(decrypt/4, [
		comment is 'Decrypts block-aligned ciphertext without removing padding.',
		argnames is ['Cipher', 'Key', 'Ciphertext', 'Plaintext']
	]).

	:- public(encrypt_padded/4).
	:- mode(encrypt_padded(+object_identifier, +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(encrypt_padded/4, [
		comment is 'Pads plaintext using PKCS#7 and encrypts it.',
		argnames is ['Cipher', 'Key', 'Plaintext', 'Ciphertext']
	]).

	:- public(decrypt_padded/4).
	:- mode(decrypt_padded(+object_identifier, +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(decrypt_padded/4, [
		comment is 'Decrypts ciphertext and validates and removes PKCS#7 padding.',
		argnames is ['Cipher', 'Key', 'Ciphertext', 'Plaintext']
	]).

	:- uses(list, [
		append/3
	]).

	encrypt(Cipher, Key, Plaintext, Ciphertext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_bytes(Plaintext),
		^^check_aligned(BlockSize, Plaintext),
		^^blocks(BlockSize, Plaintext, PlaintextBlocks),
		encrypt_blocks(PlaintextBlocks, Cipher, PreparedKey, Ciphertext).

	decrypt(Cipher, Key, Ciphertext, Plaintext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_bytes(Ciphertext),
		^^check_aligned(BlockSize, Ciphertext),
		^^blocks(BlockSize, Ciphertext, CiphertextBlocks),
		decrypt_blocks(CiphertextBlocks, Cipher, PreparedKey, Plaintext).

	encrypt_padded(Cipher, Key, Plaintext, Ciphertext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_bytes(Plaintext),
		^^pkcs7_pad(BlockSize, Plaintext, PaddedPlaintext),
		^^blocks(BlockSize, PaddedPlaintext, PlaintextBlocks),
		encrypt_blocks(PlaintextBlocks, Cipher, PreparedKey, Ciphertext).

	decrypt_padded(Cipher, Key, Ciphertext, Plaintext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_bytes(Ciphertext),
		(	Ciphertext == [] ->
			domain_error(non_empty_ciphertext, Ciphertext)
		;	true
		),
		^^check_aligned(BlockSize, Ciphertext),
		^^blocks(BlockSize, Ciphertext, CiphertextBlocks),
		decrypt_blocks(CiphertextBlocks, Cipher, PreparedKey, PaddedPlaintext),
		^^pkcs7_unpad(BlockSize, PaddedPlaintext, Ciphertext, Plaintext).

	encrypt_blocks([], _, _, []).
	encrypt_blocks([Block| Blocks], Cipher, PreparedKey, Ciphertext) :-
		Cipher::encrypt_prepared_block(PreparedKey, Block, EncryptedBlock),
		append(EncryptedBlock, RestCiphertext, Ciphertext),
		encrypt_blocks(Blocks, Cipher, PreparedKey, RestCiphertext).

	decrypt_blocks([], _, _, []).
	decrypt_blocks([Block| Blocks], Cipher, PreparedKey, Plaintext) :-
		Cipher::decrypt_prepared_block(PreparedKey, Block, DecryptedBlock),
		append(DecryptedBlock, RestPlaintext, Plaintext),
		decrypt_blocks(Blocks, Cipher, PreparedKey, RestPlaintext).

:- end_object.
