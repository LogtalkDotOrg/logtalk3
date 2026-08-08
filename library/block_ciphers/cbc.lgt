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

:- object(cbc,
	imports(block_cipher_modes_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Generic Cipher Block Chaining mode encryption and decryption.'
	]).

	:- public(encrypt/5).
	:- mode(encrypt(+object_identifier, +list(byte), +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(encrypt/5, [
		comment is 'Encrypts block-aligned plaintext without padding using an explicit initialization vector.',
		argnames is ['Cipher', 'Key', 'IV', 'Plaintext', 'Ciphertext'],
		exceptions is [
			'``Cipher`` is a variable' - instantiation_error,
			'``Cipher`` is neither a variable nor a prepared-key block cipher object' - domain_error(block_cipher, 'Cipher'),
			'``IV`` is not a byte list of exactly one block' - type_error(list(byte, 'BlockSize'), 'IV'),
			'``Plaintext`` is a variable or a partial list' - instantiation_error,
			'``Plaintext`` is neither a partial list nor a list' - type_error(list, 'Plaintext'),
			'An element ``Byte`` of the ``Plaintext`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``Plaintext`` list is an integer but not a valid byte' - domain_error(byte, 'Byte'),
			'``Plaintext`` length is not block aligned' - domain_error(block_aligned_byte_length('BlockSize'), 'Plaintext')
		]
	]).

	:- public(decrypt/5).
	:- mode(decrypt(+object_identifier, +list(byte), +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(decrypt/5, [
		comment is 'Decrypts block-aligned ciphertext without removing padding using an explicit initialization vector.',
		argnames is ['Cipher', 'Key', 'IV', 'Ciphertext', 'Plaintext'],
		exceptions is [
			'``Cipher`` is a variable' - instantiation_error,
			'``Cipher`` is neither a variable nor a prepared-key block cipher object' - domain_error(block_cipher, 'Cipher'),
			'``IV`` is not a byte list of exactly one block' - type_error(list(byte, 'BlockSize'), 'IV'),
			'``Ciphertext`` is a variable or a partial list' - instantiation_error,
			'``Ciphertext`` is neither a partial list nor a list' - type_error(list, 'Ciphertext'),
			'An element ``Byte`` of the ``Ciphertext`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``Ciphertext`` list is an integer but not a valid byte' - domain_error(byte, 'Byte'),
			'``Ciphertext`` length is not block aligned' - domain_error(block_aligned_byte_length('BlockSize'), 'Ciphertext')
		]
	]).

	:- public(encrypt_padded/5).
	:- mode(encrypt_padded(+object_identifier, +list(byte), +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(encrypt_padded/5, [
		comment is 'Pads plaintext using PKCS#7 and encrypts it using an explicit initialization vector.',
		argnames is ['Cipher', 'Key', 'IV', 'Plaintext', 'Ciphertext'],
		exceptions is [
			'``Cipher`` is a variable' - instantiation_error,
			'``Cipher`` is neither a variable nor a prepared-key block cipher object' - domain_error(block_cipher, 'Cipher'),
			'``Cipher`` block size exceeds the PKCS#7 limit' - domain_error(pkcs7_block_size, 'BlockSize'),
			'``IV`` is not a byte list of exactly one block' - type_error(list(byte, 'BlockSize'), 'IV'),
			'``Plaintext`` is a variable or a partial list' - instantiation_error,
			'``Plaintext`` is neither a partial list nor a list' - type_error(list, 'Plaintext'),
			'An element ``Byte`` of the ``Plaintext`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``Plaintext`` list is an integer but not a valid byte' - domain_error(byte, 'Byte')
		]
	]).

	:- public(decrypt_padded/5).
	:- mode(decrypt_padded(+object_identifier, +list(byte), +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(decrypt_padded/5, [
		comment is 'Decrypts ciphertext and validates and removes PKCS#7 padding using an explicit initialization vector.',
		argnames is ['Cipher', 'Key', 'IV', 'Ciphertext', 'Plaintext'],
		exceptions is [
			'``Cipher`` is a variable' - instantiation_error,
			'``Cipher`` is neither a variable nor a prepared-key block cipher object' - domain_error(block_cipher, 'Cipher'),
			'``IV`` is not a byte list of exactly one block' - type_error(list(byte, 'BlockSize'), 'IV'),
			'``Ciphertext`` is a variable or a partial list' - instantiation_error,
			'``Ciphertext`` is neither a partial list nor a list' - type_error(list, 'Ciphertext'),
			'An element ``Byte`` of the ``Ciphertext`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``Ciphertext`` list is an integer but not a valid byte' - domain_error(byte, 'Byte'),
			'``Ciphertext`` is empty' - domain_error(non_empty_ciphertext, 'Ciphertext'),
			'``Ciphertext`` length is not block aligned' - domain_error(block_aligned_byte_length('BlockSize'), 'Ciphertext'),
			'``Ciphertext`` does not contain valid PKCS#7 padding' - domain_error(pkcs7_padding, 'Ciphertext')
		]
	]).

	:- uses(list, [
		append/3
	]).

	encrypt(Cipher, Key, IV, Plaintext, Ciphertext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_block(BlockSize, IV),
		^^check_bytes(Plaintext),
		^^check_aligned(BlockSize, Plaintext),
		^^blocks(BlockSize, Plaintext, PlaintextBlocks),
		encrypt_blocks(PlaintextBlocks, Cipher, PreparedKey, IV, Ciphertext).

	decrypt(Cipher, Key, IV, Ciphertext, Plaintext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_block(BlockSize, IV),
		^^check_bytes(Ciphertext),
		^^check_aligned(BlockSize, Ciphertext),
		^^blocks(BlockSize, Ciphertext, CiphertextBlocks),
		decrypt_blocks(CiphertextBlocks, Cipher, PreparedKey, IV, Plaintext).

	encrypt_padded(Cipher, Key, IV, Plaintext, Ciphertext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_block(BlockSize, IV),
		^^check_bytes(Plaintext),
		^^pkcs7_pad(BlockSize, Plaintext, PaddedPlaintext),
		^^blocks(BlockSize, PaddedPlaintext, PlaintextBlocks),
		encrypt_blocks(PlaintextBlocks, Cipher, PreparedKey, IV, Ciphertext).

	decrypt_padded(Cipher, Key, IV, Ciphertext, Plaintext) :-
		^^prepare_cipher(Cipher, Key, BlockSize, PreparedKey),
		^^check_block(BlockSize, IV),
		^^check_bytes(Ciphertext),
		(	Ciphertext == [] ->
			domain_error(non_empty_ciphertext, Ciphertext)
		;	true
		),
		^^check_aligned(BlockSize, Ciphertext),
		^^blocks(BlockSize, Ciphertext, CiphertextBlocks),
		decrypt_blocks(CiphertextBlocks, Cipher, PreparedKey, IV, PaddedPlaintext),
		^^pkcs7_unpad(BlockSize, PaddedPlaintext, Ciphertext, Plaintext).

	encrypt_blocks([], _, _, _, []).
	encrypt_blocks([Block| Blocks], Cipher, PreparedKey, PreviousBlock, Ciphertext) :-
		^^xor_bytes(Block, PreviousBlock, ChainedBlock),
		Cipher::encrypt_prepared_block(PreparedKey, ChainedBlock, EncryptedBlock),
		append(EncryptedBlock, RestCiphertext, Ciphertext),
		encrypt_blocks(Blocks, Cipher, PreparedKey, EncryptedBlock, RestCiphertext).

	decrypt_blocks([], _, _, _, []).
	decrypt_blocks([Block| Blocks], Cipher, PreparedKey, PreviousBlock, Plaintext) :-
		Cipher::decrypt_prepared_block(PreparedKey, Block, DecryptedBlock),
		^^xor_bytes(DecryptedBlock, PreviousBlock, PlaintextBlock),
		append(PlaintextBlock, RestPlaintext, Plaintext),
		decrypt_blocks(Blocks, Cipher, PreparedKey, Block, RestPlaintext).

:- end_object.
