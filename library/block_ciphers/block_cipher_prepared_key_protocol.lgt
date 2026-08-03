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


:- protocol(block_cipher_prepared_key_protocol,
	extends(block_cipher_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Protocol for block ciphers supporting opaque prepared keys for repeated block operations.'
	]).

	:- public(prepare_key/2).
	:- mode(prepare_key(+list(byte), --compound), one_or_error).
	:- info(prepare_key/2, [
		comment is 'Validates and prepares a key for repeated block encryption or decryption.',
		argnames is ['Key', 'PreparedKey']
	]).

	:- public(encrypt_prepared_block/3).
	:- mode(encrypt_prepared_block(+compound, +list(byte), --list(byte)), one_or_error).
	:- info(encrypt_prepared_block/3, [
		comment is 'Encrypts a plaintext block using an opaque prepared key created by the same cipher object.',
		argnames is ['PreparedKey', 'PlaintextBlock', 'CiphertextBlock']
	]).

	:- public(decrypt_prepared_block/3).
	:- mode(decrypt_prepared_block(+compound, +list(byte), --list(byte)), one_or_error).
	:- info(decrypt_prepared_block/3, [
		comment is 'Decrypts a ciphertext block using an opaque prepared key created by the same cipher object.',
		argnames is ['PreparedKey', 'CiphertextBlock', 'PlaintextBlock']
	]).

:- end_protocol.