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


:- protocol(cmac_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Protocol for one-shot CMAC digest computation using block cipher objects implementing ``block_cipher_prepared_key_protocol``.',
		see_also is [block_cipher_prepared_key_protocol]
	]).

	:- public(digest/4).
	:- mode(digest(+object_identifier, +list(byte), +list(byte), --list(byte)), one_or_error).
	:- info(digest/4, [
		comment is 'Computes the full CMAC digest for a block cipher object, key bytes, and message bytes.',
		argnames is ['Cipher', 'KeyBytes', 'MessageBytes', 'DigestBytes'],
		exceptions is [
			'``Cipher`` or ``MessageBytes`` is a variable or a partial list' - instantiation_error,
			'``Cipher`` does not implement the required prepared-key block cipher protocol' - domain_error(cmac_cipher, 'Cipher'),
			'``Cipher`` reports a block size other than 8 or 16 bytes' - domain_error(cmac_block_size, 'BlockSize'),
			'``MessageBytes`` is neither a variable nor a list' - type_error(list(byte), 'MessageBytes'),
			'``MessageBytes`` contains a non-integer byte' - type_error(integer, 'Byte'),
			'``MessageBytes`` contains an integer outside the byte range' - domain_error(byte, 'Byte')
		]
	]).

	:- public(hex_digest/4).
	:- mode(hex_digest(+object_identifier, +list(byte), +list(byte), --atom), one_or_error).
	:- info(hex_digest/4, [
		comment is 'Computes the full CMAC digest for a block cipher object, key bytes, and message bytes, returning a lowercase hexadecimal atom.',
		argnames is ['Cipher', 'KeyBytes', 'MessageBytes', 'HexDigest'],
		exceptions is [
			'``Cipher`` or ``MessageBytes`` is a variable or a partial list' - instantiation_error,
			'``Cipher`` does not implement the required prepared-key block cipher protocol' - domain_error(cmac_cipher, 'Cipher'),
			'``Cipher`` reports a block size other than 8 or 16 bytes' - domain_error(cmac_block_size, 'BlockSize'),
			'``MessageBytes`` is neither a variable nor a list' - type_error(list(byte), 'MessageBytes'),
			'An element ``Byte`` of the `MessageBytes`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``MessageBytes`` list is an integer but not a valid byte' - domain_error(byte, 'Byte')
		]
	]).

	:- public(digest/5).
	:- mode(digest(+object_identifier, +list(byte), +list(byte), +integer, --list(byte)), one_or_error).
	:- info(digest/5, [
		comment is 'Computes a truncated CMAC digest containing the requested number of leftmost bytes.',
		argnames is ['Cipher', 'KeyBytes', 'MessageBytes', 'Length', 'DigestBytes'],
		exceptions is [
			'``Length`` is a variable' - instantiation_error,
			'``Length`` is neither a variable nor an integer' - type_error(integer, 'Length'),
			'``Length`` is an integer outside the range from one through the cipher block size' - domain_error(cmac_output_length(1, 'BlockSize'), 'Length')
		]
	]).

	:- public(hex_digest/5).
	:- mode(hex_digest(+object_identifier, +list(byte), +list(byte), +integer, --atom), one_or_error).
	:- info(hex_digest/5, [
		comment is 'Computes a truncated CMAC digest containing the requested number of leftmost bytes and returns it as a lowercase hexadecimal atom.',
		argnames is ['Cipher', 'KeyBytes', 'MessageBytes', 'Length', 'HexDigest'],
		exceptions is [
			'``Cipher``, ``MessageBytes``, or ``Length`` is a variable or a partial list' - instantiation_error,
			'``Cipher`` does not implement the required prepared-key block cipher protocol' - domain_error(cmac_cipher, 'Cipher'),
			'``Cipher`` reports a block size other than 8 or 16 bytes' - domain_error(cmac_block_size, 'BlockSize'),
			'``MessageBytes`` is neither a variable nor a list' - type_error(list(byte), 'MessageBytes'),
			'An element ``Byte`` of the `MessageBytes`` list is neither a variable nor an integer' - type_error(integer, 'Byte'),
			'An element ``Byte`` of the ``MessageBytes`` list is an integer but not a valid byte' - domain_error(byte, 'Byte'),
			'``Length`` is neither a variable nor an integer' - type_error(integer, 'Length'),
			'``Length`` is an integer outside the range from one through the cipher block size' - domain_error(cmac_output_length(1, 'BlockSize'), 'Length')
		]
	]).

:- end_protocol.
