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


:- protocol(block_cipher_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Protocol for one-shot encryption and decryption using a block cipher.'
	]).

	:- public(encrypt_block/3).
	:- mode(encrypt_block(+list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(encrypt_block/3, [
		comment is 'Encrypts a plaintext block using the given key.',
		argnames is ['Key', 'PlaintextBlock', 'CiphertextBlock'],
		exceptions is [
			'``Key`` or ``PlaintextBlock`` is a variable or a partial list' - instantiation_error,
			'``Key`` is neither a variable nor a list of the required number of bytes' - type_error(list(byte, 'KeySize'), 'Key'),
			'``PlaintextBlock`` is neither a variable nor a list of the required number of bytes' - type_error(list(byte, 'BlockSize'), 'PlaintextBlock'),
			'``Key`` or ``PlaintextBlock`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Key`` or ``PlaintextBlock`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(decrypt_block/3).
	:- mode(decrypt_block(+list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(decrypt_block/3, [
		comment is 'Decrypts a ciphertext block using the given key.',
		argnames is ['Key', 'CiphertextBlock', 'PlaintextBlock'],
		exceptions is [
			'``Key`` or ``CiphertextBlock`` is a variable or a partial list' - instantiation_error,
			'``Key`` is neither a variable nor a list of the required number of bytes' - type_error(list(byte, 'KeySize'), 'Key'),
			'``CiphertextBlock`` is neither a variable nor a list of the required number of bytes' - type_error(list(byte, 'BlockSize'), 'CiphertextBlock'),
			'``Key`` or ``CiphertextBlock`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``Key`` or ``CiphertextBlock`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(block_size/1).
	:- mode(block_size(--integer), one).
	:- info(block_size/1, [
		comment is 'Returns the cipher block size in bytes.',
		argnames is ['BlockSize']
	]).

	:- public(key_size/1).
	:- mode(key_size(--integer), one).
	:- info(key_size/1, [
		comment is 'Returns the cipher key size in bytes.',
		argnames is ['KeySize']
	]).

:- end_protocol.
