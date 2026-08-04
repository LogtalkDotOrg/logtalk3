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

:- category(block_cipher_modes_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Shared validation, block, XOR, and PKCS#7 predicates for block cipher modes.'
	]).

	:- protected(prepare_cipher/4).
	:- mode(prepare_cipher(+object_identifier, +list(byte), --positive_integer, --compound), one_or_error).
	:- info(prepare_cipher/4, [
		comment is 'Validates a cipher object, queries its block size, and prepares a key.',
		argnames is ['Cipher', 'Key', 'BlockSize', 'PreparedKey']
	]).

	:- protected(check_bytes/1).
	:- mode(check_bytes(+list(byte)), one_or_error).
	:- info(check_bytes/1, [
		comment is 'Checks a list of bytes.',
		argnames is ['Bytes']
	]).

	:- protected(check_block/2).
	:- mode(check_block(+positive_integer, +list(byte)), one_or_error).
	:- info(check_block/2, [
		comment is 'Checks a byte list whose length is exactly one block.',
		argnames is ['BlockSize', 'Block']
	]).

	:- protected(check_aligned/2).
	:- mode(check_aligned(+positive_integer, +list(byte)), one_or_error).
	:- info(check_aligned/2, [
		comment is 'Checks that a byte-list length is a multiple of the block size.',
		argnames is ['BlockSize', 'Bytes']
	]).

	:- protected(blocks/3).
	:- mode(blocks(+positive_integer, +list(byte), --list(list(byte))), one).
	:- info(blocks/3, [
		comment is 'Splits an aligned byte list into blocks.',
		argnames is ['BlockSize', 'Bytes', 'Blocks']
	]).

	:- protected(xor_bytes/3).
	:- mode(xor_bytes(+list(byte), +list(byte), --list(byte)), one).
	:- info(xor_bytes/3, [
		comment is 'XORs equal-length byte lists.',
		argnames is ['Bytes1', 'Bytes2', 'Bytes']
	]).

	:- protected(pkcs7_pad/3).
	:- mode(pkcs7_pad(+positive_integer, +list(byte), --list(byte)), one_or_error).
	:- info(pkcs7_pad/3, [
		comment is 'Pads bytes using PKCS#7.',
		argnames is ['BlockSize', 'Bytes', 'PaddedBytes']
	]).

	:- protected(pkcs7_unpad/4).
	:- mode(pkcs7_unpad(+positive_integer, +list(byte), +term, --list(byte)), one_or_error).
	:- info(pkcs7_unpad/4, [
		comment is 'Validates and removes PKCS#7 padding, reporting the supplied error value when padding is invalid.',
		argnames is ['BlockSize', 'PaddedBytes', 'ErrorValue', 'Bytes']
	]).

	:- uses(list, [
		append/3, last/2, length/2
	]).
	:- uses(type, [
		check/3
	]).

	prepare_cipher(Cipher, Key, BlockSize, PreparedKey) :-
		(	var(Cipher) ->
			instantiation_error
		;	conforms_to_protocol(Cipher, block_cipher_prepared_key_protocol) ->
			Cipher::block_size(BlockSize),
			(	integer(BlockSize), BlockSize > 0 ->
				true
			;	domain_error(block_cipher_block_size, BlockSize)
			),
			Cipher::prepare_key(Key, PreparedKey)
		;	domain_error(block_cipher, Cipher)
		).

	check_bytes(Bytes) :-
		context(Context),
		check(list(byte), Bytes, Context).

	check_block(BlockSize, Block) :-
		context(Context),
		check(list(byte, BlockSize), Block, Context).

	check_aligned(BlockSize, Bytes) :-
		length(Bytes, Length),
		(	Length mod BlockSize =:= 0 ->
			true
		;	domain_error(block_aligned_byte_length(BlockSize), Bytes)
		).

	blocks(_, [], []) :-
		!.
	blocks(BlockSize, Bytes, [Block| Blocks]) :-
		take_block(BlockSize, Bytes, Block, Rest),
		blocks(BlockSize, Rest, Blocks).

	take_block(0, Bytes, [], Bytes) :-
		!.
	take_block(Count, [Byte| Bytes], [Byte| Block], Rest) :-
		NextCount is Count - 1,
		take_block(NextCount, Bytes, Block, Rest).

	xor_bytes([], [], []).
	xor_bytes([Byte1| Bytes1], [Byte2| Bytes2], [Byte| Bytes]) :-
		Byte is xor(Byte1, Byte2),
		xor_bytes(Bytes1, Bytes2, Bytes).

	pkcs7_pad(BlockSize, Bytes, PaddedBytes) :-
		check_pkcs7_block_size(BlockSize),
		length(Bytes, Length),
		PaddingLength is BlockSize - Length mod BlockSize,
		padding_bytes(PaddingLength, PaddingLength, Padding),
		append(Bytes, Padding, PaddedBytes).

	pkcs7_unpad(BlockSize, PaddedBytes, ErrorValue, Bytes) :-
		check_pkcs7_block_size(BlockSize),
		(	PaddedBytes == [] ->
			domain_error(pkcs7_padding, ErrorValue)
		;	last(PaddedBytes, PaddingLength),
			length(PaddedBytes, Length),
			(	integer(PaddingLength),
				PaddingLength >= 1, PaddingLength =< BlockSize,
				PaddingLength =< Length ->
				BytesLength is Length - PaddingLength,
				length(Bytes, BytesLength),
				length(Padding, PaddingLength),
				append(Bytes, Padding, PaddedBytes),
				(	padding_values(Padding, PaddingLength) ->
					true
				;	domain_error(pkcs7_padding, ErrorValue)
				)
			;	domain_error(pkcs7_padding, ErrorValue)
			)
		).

	check_pkcs7_block_size(BlockSize) :-
		(	BlockSize =< 255 ->
			true
		;	domain_error(pkcs7_block_size, BlockSize)
		).

	padding_bytes(0, _, []) :-
		!.
	padding_bytes(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		padding_bytes(NextCount, Byte, Bytes).

	padding_values([], _).
	padding_values([Byte| Bytes], Byte) :-
		padding_values(Bytes, Byte).

:- end_category.
